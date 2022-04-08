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

(defparameter *foreign-window-events*  '(:structure-notify
                                         :property-change
                                         :focus-change
                                         :enter-window))

(defclass foreign-application-pane (mirrored-sheet-mixin ; must be the first
                                    fundamental-input-stream
                                    basic-gadget)
  ())

(defmethod foreign-xwindow ((pane foreign-application-pane))
  (foreign-xwindow (pane-frame pane)))

(defmethod handle-event ((pane foreign-application-pane) (event window-manager-configuration-request-event))
  (change-space-requirements pane))

(defmethod handle-event ((pane foreign-application-pane) (event pointer-button-press-event))
  (let* ((frame (pane-frame pane))
         (port (port frame)))
    (setf (active-frame port) frame)))

(defmethod handle-event ((pane foreign-application-pane) (event window-destroy-event))
  (let ((frame (pane-frame pane)))
    (frame-exit frame)))

(defmethod compose-space ((pane foreign-application-pane) &key width height)
  (declare (ignore width height))
  (let* ((xwin (foreign-xwindow pane))
         (hints (and xwin (xlib:wm-normal-hints xwin)))
         (hw (when hints (xlib:wm-size-hints-width hints)))
         (hh (when hints (xlib:wm-size-hints-height hints)))
    	 (width (or hw (ignore-errors (xlib:drawable-width xwin))))
    	 (height (or hh (ignore-errors (xlib:drawable-height xwin)))))
    (make-space-requirement :width  (or  width 800)
                            :height (or  height 600))))

(defmethod allocate-space ((pane foreign-application-pane) width height)
  (call-next-method)
  (let* ((xparent (clim-clx::window (sheet-mirror pane)))
         (xwindow (foreign-xwindow pane))
         (w (xlib:drawable-width xparent))
         (h (xlib:drawable-height xparent)))
    (when xwindow
      (setf (xlib:drawable-width xwindow) w)
      (setf (xlib:drawable-height xwindow) h)
      (send-configure-notify xwindow))))

(define-application-frame foreign-application ()
  ((foreign-xwindow :initarg :foreign-xwindow :initform nil :accessor foreign-xwindow))
  (:panes (main (make-pane 'foreign-application-pane)))
  (:menu-bar nil)
  (:layouts (:default (vertically () main)))
  (:top-level (foreign-application-frame-top-level . nil)))

(defun foreign-application-p (frame)
  (typep frame 'foreign-application))

(defmethod (setf active-frame) :after ((frame foreign-application) (port doors-port))
  (when-let ((window (foreign-xwindow frame)))
    (when (member (frame-properties frame :input-model) '(:passive :locally-active))
      (xlib:set-input-focus  (clim-clx::clx-port-display (port frame))
                             window :parent))
    (when (member (frame-properties frame :input-model) '(:locally-active :globally-active))
      (send-client-message (foreign-xwindow frame) :wm_take_focus (x-server-timestamp port)))))

(defmethod foreign-application-frame-top-level ((frame application-frame))
  (clim-extensions:simple-event-loop))

(defmethod frame-exit :around ((frame foreign-application))
  (when-let ((window (foreign-xwindow frame)))
    ;; maybe is necessary also to remap window to root if window is not destroyed
    ;; is necessary to kill the window??
    (port-unregister-foreign-application (port frame) (find-pane-named frame 'main) window)
    (send-client-message window :WM_DELETE_WINDOW (x-server-timestamp (port frame))))
  (call-next-method))

(defmethod disown-frame :before ((frame-manager doors-frame-manager) (frame foreign-application))
  (when-let ((window (foreign-xwindow frame)))
    (xlib:reparent-window window (clim-clx::window (sheet-mirror (graft (port frame)))) 0 0)))

(defmethod adopt-frame :after ((frame-manager doors-frame-manager) (frame foreign-application))
  (let* ((window (foreign-xwindow frame))
         (pane (find-pane-named frame 'main))
         (parent-window (clim-clx::window (sheet-mirror pane))))
    (when window
      (port-register-foreign-application (port frame-manager) pane window))
    (xlib:grab-button parent-window 1 '(:button-press)
                             :owner-p nil
                             :sync-pointer-p t
                             :sync-keyboard-p nil)
    (setf (xlib:window-event-mask parent-window) '(:substructure-notify :substructure-redirect))
    (when window
      (xlib:with-server-grabbed ((clim-clx::clx-port-display (port frame-manager)))
        (xlib:reparent-window window parent-window 0 0)
        (xlib:map-window window)))))

(defun calculate-initial-position (window)
  ;; for the moment I don't check win-gravity. In this way I assume always north-west
  ;; and for the moment I don't check hints at all
  (let* ((hints (xlib:wm-normal-hints window))
         (hx (when hints (xlib:wm-size-hints-x hints)))
         (hy (when hints (xlib:wm-size-hints-y hints)))
    	 (x (or hx (ignore-errors (xlib:drawable-x window))))
    	 (y (or hy (ignore-errors (xlib:drawable-y window)))))
  (values  x y)))

(defun initial-state (window)
  (let ((hints (xlib:wm-hints window)))
    (if (and hints
             (eql :iconic (xlib:wm-hints-initial-state hints)))
        :shrunk
        :enabled)))

(defun make-foreign-application (window &key (frame-manager (find-frame-manager)))
  (let ((initial-state (initial-state window))
        (input (icccm-input-model window)))
    (log:warn input)
    (multiple-value-bind (x y) (calculate-initial-position window)
      (let* ((frame (make-application-frame 'foreign-application
                                            :foreign-xwindow window
                                            :left x
                                            ;; :state initial-state ; some problem with this
                                            :top y
                                            :frame-manager frame-manager))
             (name (or (ignore-errors (doors::net-wm-name window))
                       (ignore-errors (xlib:wm-name window))
                       "NoWin")))
        (setf (xlib:window-event-mask window) *foreign-window-events*)
        (setf (frame-properties frame :input-model) input)
        (clim-sys:make-process #'(lambda () (run-frame-top-level frame)) :name (format nil  "Foreign App: ~a" name))
        ;; usare semafori invece o server grab
        (sleep 0.5)))))

(defun foreign-application-unmanage-xwindow (frame)
  (when-let ((window (foreign-xwindow frame))
             (root (clim-clx::window (sheet-mirror (graft frame))))
             (pane (find-pane-named frame 'main)))
    (setf (xlib:window-event-mask window) 0)
    (multiple-value-bind (x y)
        (xlib:translate-coordinates window 0 0 root)
      (xlib:reparent-window window root x y))
    (port-unregister-foreign-application (port frame) pane window)
    (setf (foreign-xwindow frame) nil)))

(defun restart-foreign-application (frame)
  (when (foreign-application-p frame)
    (let ((xwindow (foreign-xwindow frame)))
      (foreign-application-unmanage-xwindow frame)
      (destroy-frame frame)
      (make-foreign-application xwindow))))
