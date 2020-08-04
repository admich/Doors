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

(defclass foreign-application-pane (mirrored-sheet-mixin ; must be the first                                     
                                        ;extended-input-stream
                                    immediate-sheet-input-mixin
                                    ;; fundamental-character-input-stream
				     ;fundamental-input-stream
				     basic-gadget)
  ())

(defmethod foreign-xwindow ((pane foreign-application-pane))
  (foreign-xwindow (pane-frame pane)))

(defun configure-foreign-application (foreign-pane)
  (let* ((xparent (sheet-mirror foreign-pane))
         (xwindow (foreign-xwindow foreign-pane))
         (w (xlib:drawable-width xparent))
         (h (xlib:drawable-height xparent)))
    (when xwindow
      (setf (xlib:drawable-width xwindow) w)
      (setf (xlib:drawable-height xwindow) h)
      (multiple-value-bind (x y)
          (xlib:translate-coordinates xparent
                                      0
                                      0
                                      (clx-port-window (port foreign-pane)))
        (xlib:send-event xwindow :configure-notify nil
        	       :event-window xwindow
        	       :window xwindow
                   :x x :y y
        	       :width w
        	       :height h
                   :border-width 0
        	       :propagate-p nil)))))

(defmethod handle-event :after ((sheet top-level-sheet-pane)
                                (event window-configuration-event))
  (when (typep (pane-frame sheet) 'foreign-application)
    (let* ((frame (pane-frame sheet))
           (foreign-pane (find-pane-named frame 'main)))
      (configure-foreign-application foreign-pane))))

(defmethod handle-event ((pane foreign-application-pane) (event window-manager-configuration-request-event))
  ()
  (with-slots (window x y width height) event
    (layout-frame *application-frame* width height)))

(defmethod handle-event ((pane foreign-application-pane) (event window-manager-delete-event))
  (frame-exit (pane-frame pane)))

(defmethod handle-event ((pane foreign-application-pane) (event window-destroy-event))
  (frame-exit (pane-frame pane)))

(defmethod note-sheet-region-changed :after ((sheet foreign-application-pane))
  (configure-foreign-application sheet))

(defmethod note-sheet-transformation-changed :after ((sheet foreign-application-pane))
  (configure-foreign-application sheet))

(defmethod compose-space ((pane foreign-application-pane) &key width height)
  (declare (ignore width height))
  (let* ((parent (sheet-mirror pane))
	 (xwin (foreign-xwindow pane))
	 (width (and xwin (xlib:drawable-width xwin)))
	 (height (and xwin (xlib:drawable-height xwin)))
	 (w (and parent (xlib:drawable-width parent)))
	 (h (and parent (xlib:drawable-height parent))))
    (make-space-requirement :width  (or  width 800)
                            :height (or  height 600))))


(define-application-frame foreign-application ()
  ((foreign-xwindow :initarg :foreign-xwindow :initform nil :accessor foreign-xwindow))
  (:panes (main (make-pane 'foreign-application-pane)))
  (:menu-bar nil)
  (:layouts (:default (vertically () main)))
  (:top-level (foreign-application-frame-top-level . nil)))

(defmethod (setf active-frame) :after ((frame foreign-application) (port doors-port))
  (when-let ((window (foreign-xwindow frame)))
    (xlib:set-input-focus  (clim-clx::clx-port-display (port frame))
                           window :parent)))

(defmethod frame-pretty-name ((frame foreign-application))
  (multiple-value-bind (name class)
      (handler-case
          (xlib:get-wm-class (foreign-xwindow frame))
        (xlib:window-error () (values "NoWin" "NoWin"))
        (type-error () (values "NoWin" "NoWin")))
      (or class "NoName")))

(defmethod foreign-application-frame-top-level ((frame application-frame))
  (setf (active-frame (port frame)) frame)
  (clim-extensions:simple-event-loop))

(defmethod frame-exit :around ((frame foreign-application))
  (when-let ((window (foreign-xwindow frame)))
    ;; maybe is necessary also to remap window to root if window is not destroyed
    ;; is necessary to kill the window??
    (port-unregister-foreign-application (port frame) (find-pane-named frame 'main) window)
    (xlib:send-event window
                     :client-message nil
                     :window window
                     :type :WM_PROTOCOLS
                     :format 32
                     :data (list (xlib:intern-atom (clx-port-display (port frame)) :WM_DELETE_WINDOW))))
  (call-next-method))

(defmethod generate-panes :after ((fm doors-frame-manager) (frame foreign-application))
  (when-let ((window (foreign-xwindow frame)))
    (xlib:set-input-focus  (clim-clx::clx-port-display (port frame))
                           window :parent)))

(defmethod disown-frame :before ((frame-manager doors-frame-manager) (frame foreign-application))
  (when-let ((window (foreign-xwindow frame)))
    (xlib:reparent-window window (sheet-mirror (graft frame)) 0 0)))

(defmethod adopt-frame :after ((frame-manager doors-frame-manager) (frame foreign-application))
  (let* ((window (foreign-xwindow frame))
         (pane (find-pane-named frame 'main))
         (parent-window (sheet-mirror pane)))
    (when window
      (port-register-foreign-application (port frame-manager) pane window))
    (xlib:grab-button parent-window 1 '(:button-press)
                             :owner-p nil
                             :sync-pointer-p t
                             :sync-keyboard-p nil)
    (setf (xlib:window-event-mask parent-window) '(:substructure-notify :substructure-redirect))
    (configure-foreign-application pane)
    (when window
      (xlib:with-server-grabbed ((clim-clx::clx-port-display (port frame-manager)))
        (xlib:reparent-window window parent-window 0 0)
        (xlib:map-window window)))))

(defun make-foreign-application (window &key (frame-manager (find-frame-manager)))
  (let ((frame (make-application-frame 'foreign-application
                                       :foreign-xwindow window
                                       :state :disowned
                                       :frame-manager frame-manager)))
    (setf (xlib:window-event-mask window) '(:structure-notify))
    (clim-sys:make-process #'(lambda () (run-frame-top-level frame)) :name "Foreign App")
    ;; usare semafori invece o server grab
    (sleep 0.5)))

(defun foreign-application-unmanage-xwindow (frame)
  (when-let ((window (foreign-xwindow frame))
             (root (sheet-mirror (graft frame)))
             (pane (find-pane-named frame 'main)))
    (setf (xlib:window-event-mask window) 0)
    (multiple-value-bind (x y)
        (xlib:translate-coordinates window 0 0 root)
      (xlib:reparent-window window root x y))
    (port-unregister-foreign-application (port frame) pane window)
    (setf (foreign-xwindow frame) nil)))

