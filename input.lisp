;;;; Copyright (C) 2020, 2021  Andrea De Michele
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

(climi::define-event-class window-manager-number-of-desktops-request-event (window-manager-request-event)
  ((number :initarg :number :reader window-manager-number-of-desktops-request-event-number)))

(climi::define-event-class window-manager-current-desktop-request-event (window-manager-request-event)
  ((number :initarg :number :reader window-manager-current-desktop-request-event-number)))

;; For the moment I ignore source and current active window because I always obey to the request
(climi::define-event-class window-manager-active-window-request-event (window-manager-request-event)
  ((frame :initarg :frame :reader window-manager-active-window-request-event-frame)))

(defun send-configure-notify (window)
  ;; may the parent of window
  (multiple-value-bind (x y)
      (xlib:translate-coordinates window 0 0
                                  (xlib:drawable-root window))
    (xlib:send-event window :configure-notify (xlib:make-event-mask :structure-notify)
        	                :event-window window
        	                :window window
                            :override-redirect-p nil
                            :x x :y y
        	                :width (xlib:drawable-width window)
        	                :height (xlib:drawable-height window)
                            :border-width 0
        	                :propagate-p nil)))

(defun grant-configure-request (event)
  "grant the configure request"
  (with-slots (window x y width height) event
    (xlib:with-state (window)
;      (when x (setf (xlib:drawable-x window) x))
;      (when y (setf (xlib:drawable-y window) y))
      (when width (setf (xlib:drawable-width window) width))
      (when height (setf (xlib:drawable-height window) height)))))

(defmethod queue-event ((client application-frame) event)
  (climi::event-queue-append (climi::frame-event-queue client) event))

;; check this two
(defmethod distribute-event :around ((port doors-port) (event keyboard-event))
  (if (or (eq (graft port) (event-sheet event))
          (loop for x in *grabbed-keystrokes* thereis (event-matches-gesture-name-p event x)))
      (when *wm-application* (climi::dispatch-event-copy *wm-application* event))
      (call-next-method)))

(defmethod dispatch-event ((client doors-graft) event)
  (queue-event *wm-application* event))

(defvar *doors-port*)
(defvar *wait-function*)

(defmethod process-next-event ((port doors-port) &key wait-function (timeout nil))
  (let ((*doors-port* port)
        (clim-clx::*clx-port* port)
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

(defun event-handler (&key display event-window window kind event-key code state mode time
                        type width height x y root-x root-y
                        data override-redirect-p send-event-p
                        target property requestor selection
                        request first-keycode count value-mask child atom
                      &allow-other-keys)
  (declare (ignorable first-keycode count child override-redirect-p
                      send-event-p event-window))
  (when (and time (> time (x-server-timestamp *doors-port*)))
    (setf (x-server-timestamp *doors-port*) time))
  (macrolet ((with-sheet-from-window
                 ((sheet) &body body)
		       `(when-let ((,sheet (and window
                                        (or (getf (xlib:window-plist window) 'sheet)
                                            (port-lookup-foreign-sheet *doors-port* window)
                                            (graft *doors-port*)))))
                  ,@body)))
    (case event-key
      ((:focus-out)
       (log:warn "focus out che fare ?" )
       (return-from event-handler (maybe-funcall *wait-function*)))
      ((:focus-in)
       (log:warn "focus in che fare ?" )
       (return-from event-handler (maybe-funcall *wait-function*)))
      ((:property-notify)
       (when-let ((win (port-aux-xwindow *doors-port*)))
         (when (xlib:window-equal window win)
           (setf (x-server-timestamp *doors-port*) time)))
       (log:warn "property changed" atom window)
       (return-from event-handler (maybe-funcall *wait-function*)))
      ((:configure-request)
       ;;; maybe I can use with-sheet-from-window here
       (let ((sheet (and window 
                         (or (getf (xlib:window-plist window) 'sheet)
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
             (clim-xcommon:x-event-to-key-name-and-modifiers *doors-port*
                                                          event-key code state)
           (make-instance (if (eq event-key :key-press)
                              'key-press-event
                              'key-release-event)
                          :key-name keysym-name
                          :key-character (and (characterp keyname) keyname)
                          :x x :y y
                          :sheet sheet
                          :modifier-state modifier-state :timestamp time))))
      ((:button-press :button-release)
       (with-sheet-from-window (sheet)
         (when (typep sheet 'foreign-application-pane)
           (xlib:allow-events display :replay-pointer time))
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
                                :pointer (port-pointer *doors-port*)
                                :button button :x x :y y
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
                              :pointer (port-pointer *doors-port*)
                              :button button :x x :y y
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
                          :pointer (port-pointer *doors-port*) :button code
                          :x x :y y
                          :sheet sheet
                          :modifier-state (clim-xcommon:x-event-state-modifiers
                                           *doors-port* state)
                          :timestamp time))))
      (:configure-notify
       (with-sheet-from-window (sheet)
         (make-instance 'window-configuration-event
                        :sheet sheet
                        :region (make-bounding-rectangle
                                 x y (+ x width) (+ y height)))))
      (:destroy-notify
       (with-sheet-from-window (sheet)
         (make-instance 'window-destroy-event :sheet sheet)))
      (:motion-notify
       (with-sheet-from-window (sheet)
         (let ((modifier-state (clim-xcommon:x-event-state-modifiers *doors-port*
                                                                     state)))
           (make-instance 'pointer-motion-event
                          :pointer (port-pointer *doors-port*) :button code
                          :x x :y y
                          :sheet sheet
                          :modifier-state modifier-state
                          :timestamp time))))
      ((:exposure :graphics-exposure)
       ;; Notes:
       ;; . Do not compare count with 0 here, last rectangle in an
       ;;   :exposure event sequence does not cover the whole region.
       ;;
       ;; . Do not transform the event region here, since
       ;;   WINDOW-EVENT-REGION does it already. And rightfully so.
       ;;   (think about changing a sheet's native transformation).
       ;;--GB
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
       (clim-clx::process-selection-clear *doors-port* selection)
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


(defun port-client-message (sheet time type data)
  (case type
    (:wm_protocols
     (let ((message (xlib:atom-name (clx-port-display *doors-port*) (aref data 0))))
       (case message
         (:wm_take_focus
          ;; hmm, this message seems to be sent twice.
          (when-let ((mirror (sheet-mirror sheet)))
            (xlib:set-input-focus (clx-port-display *doors-port*)
                                  (clim-clx::window mirror) :parent (elt data 1)))
          (make-instance 'window-manager-focus-event :sheet sheet :timestamp time))
         (:wm_delete_window
          (make-instance 'window-manager-delete-event :sheet sheet :timestamp time))     
         (otherwise
          (warn "Unprocessed WM Protocols message: ~:_message = ~S;~:_ data = ~S;~_ sheet = ~S."
                message data sheet)))))
    (:_net_number_of_desktops
     (make-instance 'window-manager-number-of-desktops-request-event :sheet (or *wm-application* sheet) :number (elt data 0) :timestamp time))
    (:_net_current_desktop
     (make-instance 'window-manager-current-desktop-request-event :sheet (or *wm-application* sheet) :number (elt data 0) :timestamp (elt data 1)))
    (:_net_active_window
     (when (and *wm-application* (panep sheet))
       (make-instance 'window-manager-active-window-request-event :sheet *wm-application* :frame (pane-frame sheet) :timestamp (elt data 1)))) ;;sourceindication??
    (otherwise
     (warn "Unprocessed client message: ~:_type = ~S;~:_ data = ~S;~_ sheet = ~S."
           type data sheet))))
