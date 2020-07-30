;;;; Copyright (C) 2020  Andrea De Michele
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
;;;; USA

(in-package :clim-doors)

(climi::define-event-class window-manager-request-event (window-manager-event)
  ())

(climi::define-event-class window-manager-configuration-request-event (window-manager-request-event)
  ((window :initarg :window :reader window-manager-configuration-request-event-window)
   (x :initarg :x :reader window-manager-configuration-request-event-native-x)
   (y :initarg :y :reader window-manager-configuration-request-event-native-y)
   (width :initarg :width :reader window-manager-configuration-request-event-width)
   (height :initarg :height :reader window-manager-configuration-request-event-height)))

(climi::define-event-class window-manager-map-request-event (window-manager-request-event)
  ((window :initarg :window :reader window-manager-map-request-event-window)))

(defun grant-configure-request (event)
  "grant the configure request"
  (with-slots (window x y width height) event
    (xlib:with-state (window)
      (when x (setf (xlib:drawable-x window) x))
      (when y (setf (xlib:drawable-y window) y))
      (when width (setf (xlib:drawable-width window) width))
      (when height (setf (xlib:drawable-height window) height)))))

(defmethod distribute-event ((port doors-port) (event keyboard-event))
  (let  ((sheet (event-sheet event)))
    (if (member sheet (list (graft sheet) (frame-query-io *wm-application*)))
	(dispatch-event (frame-query-io *wm-application*) event)
	(dispatch-event (port-keyboard-input-focus port) event))))

(defvar *doors-port*)
(defvar *wait-function*)

(defmethod process-next-event ((port doors-port) &key wait-function (timeout nil))
  (let ((*doors-port* port)
        (*wait-function* wait-function))
    (when (maybe-funcall wait-function)
      (return-from process-next-event
        (values nil :wait-function)))
    (let ((event (xlib:process-event (clx-port-display port)
                                     :timeout timeout
                                     :handler #'event-handler
                                     :discard-p t
                                     :force-output-p t)))
      (case event
        ((nil)
         (if (maybe-funcall wait-function)
             (values nil :wait-function)
             (values nil :timeout)))
        ((t)   (values nil :wait-function))
        (otherwise
         (prog1 t (distribute-event port event)))))))

(defun ensure-focus-frame ()
  (if (car (doors::managed-frames *wm-application*))
      (setf (doors::active-frame (port *wm-application*))
            (car (doors::managed-frames *wm-application*)))
      (doors::com-goto-wm-interactor)))

(defun event-handler (&key display event-window window kind event-key code state mode time
                        type width height x y root-x root-y
                        data override-redirect-p send-event-p
                        target property requestor selection
                        request first-keycode count value-mask child
                        &allow-other-keys)
  (declare (ignore first-keycode count))
  (macrolet ((with-sheet-from-window
                 ((sheet) &body body)
		     `(when-let ((,sheet (and window
                                (or (port-lookup-sheet *doors-port* window)
                                    (port-lookup-foreign-sheet *doors-port* window)))))
                ,@body)))
    (case event-key
      ((:focus-out)
       (when  (eq :none (xlib:input-focus display))
         (xlib:set-input-focus (clim-clx::clx-port-display (port *wm-application*))
                               (sheet-mirror (graft *wm-application*))
                               :parent)
         (ensure-focus-frame)
         (return-from event-handler (maybe-funcall *wait-function*))))
      ((:property-notify)
       (when-let ((win (port-aux-xwindow *doors-port*)))
         (when (xlib:window-equal window win)
           (setf (x-server-timestamp *doors-port*) time)))
       (return-from event-handler (maybe-funcall *wait-function*)))
      ((:configure-request)
       ;;; maybe I can use with-sheet-from-window here
       (let ((sheet (and window
                      (or (port-lookup-sheet *doors-port* window)
                          (port-lookup-foreign-sheet *doors-port* window)))))
         (return-from event-handler
           (make-instance 'window-manager-configuration-request-event
                          :sheet (or sheet *wm-application*)
                          :window window
                          :x (and (= 1 (logand value-mask 1)) x)
                          :y (and (= 2 (logand value-mask 2)) y)
                          :width (and (= 4 (logand value-mask 4)) width)
                          :height (and (= 8 (logand value-mask 8)) height)))))
      ((:mapping-notify)
       (xlib:mapping-notify display request 0 0)
       (return-from event-handler (maybe-funcall *wait-function*)))
      ((:map-request)
       (make-instance 'window-manager-map-request-event
                      :sheet *wm-application*
                      :window window))
      ((:key-press :key-release)
       (with-sheet-from-window (sheet)
         (multiple-value-bind (keyname modifier-state keysym-name)
           (clim-clx::x-event-to-key-name-and-modifiers *doors-port*
                                                        event-key code state)
         (make-instance (if (eq event-key :key-press)
                            'key-press-event
                            'key-release-event)
                        :key-name keysym-name
                        :key-character (and (characterp keyname) keyname)
                        :x x :y y
                        :graft-x root-x
                        :graft-y root-y
                        :sheet (or (and (graftp sheet) (frame-query-io *wm-application*))
                                   (frame-properties (pane-frame sheet) 'focus) sheet)
                        :modifier-state modifier-state :timestamp time))))
      ((:button-press :button-release)
       ;; :button-press on a foreign-application change the focus on
       ;; that application and the click is replay on the foreign
       ;; application.
       (with-sheet-from-window (sheet)
         (unless (or (eq *wm-application* (pane-frame sheet))
                   (and (eq (active-frame *doors-port*) (pane-frame sheet))
                        (not (eq (pane-frame sheet) (pane-frame (port-keyboard-input-focus *doors-port*))))))
         (setf (active-frame *doors-port*) (pane-frame sheet)))
       (when (typep sheet 'foreign-application-pane)
         (xlib:allow-events display :replay-pointer time)
         (return-from event-handler (maybe-funcall *wait-function*)))
       (let ((modifier-state (clim-xcommon:x-event-state-modifiers *doors-port* state))
             (button (clim-clx::decode-x-button-code code)))
         (if (member button '(#.+pointer-wheel-up+
                              #.+pointer-wheel-down+
                              #.+pointer-wheel-left+
                              #.+pointer-wheel-right+))
             ;; Pointer scroll generates button press and button
             ;; release event. We ignore the latter. -- jd 2019-09-01
             (when (eq event-key :button-press)
               (make-instance 'climi::pointer-scroll-event
                              :pointer 0
                              :button button :x x :y y
                              :graft-x root-x
                              :graft-y root-y
                              :sheet sheet
                              :modifier-state modifier-state
                              :delta-x (case button
                                         (#.+pointer-wheel-left+ -1)
                                         (#.+pointer-wheel-right+ 1)
                                         (otherwise 0))
                              :delta-y (case button
                                         (#.+pointer-wheel-up+ -1)
                                         (#.+pointer-wheel-down+ 1)
                                         (otherwise 0))
                              :timestamp time))
             (make-instance (if (eq event-key :button-press)
                                'pointer-button-press-event
                                'pointer-button-release-event)
                            :pointer 0
                            :button button :x x :y y
                            :graft-x root-x
                            :graft-y root-y
                            :sheet sheet :modifier-state modifier-state
                            :timestamp time)))))
      ((:leave-notify :enter-notify)
       ;; Ignore :{ENTER,LEAVE}-NOTIFY events of kind :INFERIOR unless
       ;; the mode is :[UN]GRAB.
       ;;
       ;; The :INFERIOR kind corresponds to the pointer moving from a
       ;; parent window to a child window which we do not consider
       ;; leaving the parent.
       ;;
       ;; But we cannot ignore any :[UN]GRAB events since doing so
       ;; would violate the stack-properties of enter/exit event
       ;; sequences.
       ;;
       ;; The event kinds filtered here must be coordinated with the
       ;; processing in the DISTRIBUTE-EVENTS method for BASIC-PORT
       ;; and related methods.
       (with-sheet-from-window (sheet)
         (when (or (not (eq kind :inferior))
                   (member mode '(:grab :ungrab)))
           (make-instance (case event-key
                            (:leave-notify (case mode
                                             (:grab 'pointer-grab-leave-event)
                                             (:ungrab 'pointer-ungrab-leave-event)
                                             (t 'pointer-exit-event)))
                            (:enter-notify (case mode
                                             (:grab 'pointer-grab-enter-event)
                                             (:ungrab 'pointer-ungrab-enter-event)
                                             (t 'pointer-enter-event))))
                          :pointer 0 :button code
                          :x x :y y
                          :graft-x root-x
                          :graft-y root-y
                          :sheet sheet
                          :modifier-state (clim-xcommon:x-event-state-modifiers
                                           *doors-port* state)
                          :timestamp time))))
      (:configure-notify
       (with-sheet-from-window (sheet)
         (make-instance 'window-configuration-event
                        :sheet sheet
                        :x x :y y :width width :height height)))
      (:destroy-notify
       (with-sheet-from-window (sheet)
         (make-instance 'window-destroy-event :sheet sheet)))
      (:motion-notify
       (with-sheet-from-window (sheet)
         (let ((modifier-state (clim-xcommon:x-event-state-modifiers *doors-port*
                                                                     state)))
           (make-instance 'pointer-motion-event
                          :pointer 0 :button code
                          :x x :y y
                          :graft-x root-x
                          :graft-y root-y
                          :sheet sheet
                          :modifier-state modifier-state
                          :timestamp time))))
      ((:exposure :display :graphics-exposure)
       ;; Notes:
       ;; . Do not compare count with 0 here, last rectangle in an
       ;;   :exposure event sequence does not cover the whole region.
       ;;
       ;; . Do not transform the event region here, since
       ;;   WINDOW-EVENT-REGION does it already. And rightfully so.
       ;;   (think about changing a sheet's native transformation).
       ;;--GB
       ;;
       ;; Mike says:
       ;;   One of the lisps is bogusly sending a :display event instead of an
       ;; :exposure event. I don't remember if it's CMUCL or SBCL. So the
       ;; :display event should be left in.
       ;;
       (with-sheet-from-window (sheet)
         (make-instance 'window-repaint-event
                        :timestamp time
                        :sheet sheet
                        :region (make-rectangle* x y (+ x width) (+ y height)))))
      ;; port processes selection events synchronously and there is
      ;; no event passed to the rest of the system.
      (:selection-notify
       (with-sheet-from-window (sheet)
         (clim-clx::process-selection-notify *doors-port* window target property selection time))
       (maybe-funcall *wait-function*))
      (:selection-clear
       (with-sheet-from-window (sheet)
         (clim-clx::process-selection-clear *doors-port* selection))
       (maybe-funcall *wait-function*))
      (:selection-request
       (with-sheet-from-window (sheet)
         (clim-clx::process-selection-request *doors-port* window sheet target property requestor selection time))
       (maybe-funcall *wait-function*))
      (:client-message
       (with-sheet-from-window (sheet)
         (or (port-client-message sheet time type data)
             (maybe-funcall *wait-function*))))
      (t
       (unless (xlib:event-listen (clx-port-display *doors-port*))
         (xlib:display-force-output (clx-port-display *doors-port*)))
       (maybe-funcall *wait-function*)))))

;; copied from clim-clx
(defun port-client-message (sheet time type data)
  (case type
    (:wm_protocols
     (let ((message (xlib:atom-name (slot-value *doors-port* 'clim-clx::display) (aref data 0))))
       (case message
         (:wm_take_focus
          ;; hmm, this message seems to be sent twice.
          (when-let ((mirror (sheet-xmirror sheet)))
            (xlib:set-input-focus (clx-port-display *doors-port*)
                                  mirror :parent (elt data 1)))
          (make-instance 'window-manager-focus-event :sheet sheet :timestamp time))
         (:wm_delete_window
          (make-instance 'window-manager-delete-event :sheet sheet :timestamp time))
         (otherwise
          (warn "Unprocessed WM Protocols message: ~:_message = ~S;~:_ data = ~S;~_ sheet = ~S."
                message data sheet)))))
    (otherwise
     (warn "Unprocessed client message: ~:_type = ~S;~:_ data = ~S;~_ sheet = ~S."
           type data sheet))))

(defmethod queue-event ((client application-frame) event)
  (climi::event-queue-append (climi::frame-event-queue client) event))
