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

(in-package :doors)

(defparameter *wm-selection* "WM_S0")

(defun check-for-existing-window (wm)
  (let ((windows (xlib:query-tree (xroot wm))))
    (loop for win in windows
       unless (or (eq (xlib:window-override-redirect win) :on)
                  (eq (xlib:window-map-state win) :unmapped)) do
                    (make-foreign-application win :frame-manager wm))))

(defun start-wm (wm)
  "Initialize the icccm and ewmh protocols"
  (let ((replace (doors-wm-replace-wm wm))
        (port (port wm))
        (dpy (find-display))
        (root (xroot wm))
        timestamp)
    (intern-atoms dpy +icccm-atoms+)
    (intern-atoms dpy +ewmh-atoms+)
    (xlib:intern-atom dpy *wm-selection*)
    (let ((old-wm (xlib:selection-owner dpy *wm-selection*))
          (wm-sn-manager (xlib:create-window :parent root
                                             :override-redirect :on
                                             :width 1 :height 1
                                             :x -10 :y -10
                                             :event-mask '(:property-change))))
      (setf timestamp (update-server-timestamp port))

      (if (and old-wm (not replace))
          (progn (xlib:destroy-window wm-sn-manager)
                 (error "Another WM is running~%"))
          (setf (xlib:selection-owner dpy *wm-selection* timestamp) wm-sn-manager))

      (when old-wm
        (unless
            (dotimes (i 5)
              (handler-case (xlib:drawable-x old-wm)
                (xlib:drawable-error ()
                  (return t)))
              (sleep 1))
          (error "The old WM doesn't release the selection ownership.~%")))

      (unless (and (xlib:selection-owner dpy *wm-selection*)
                   (xlib:window-equal wm-sn-manager (xlib:selection-owner dpy *wm-selection*)))
        (xlib:destroy-window wm-sn-manager)
        (error "Failed to acquire WM selection ownership~%"))

      (handler-case
          (progn
            (setf (xlib:window-event-mask root)
                  '(:substructure-notify :substructure-redirect :focus-change))
            (xlib:display-finish-output dpy))
        (error ()
          (xlib:destroy-window wm-sn-manager)
          (error "A non ICCCM WM this running~%")))

      (setf (wm-selection-manager port) wm-sn-manager)

      (xlib:send-event root :client-message '(:structure-notify)
                       :window root
                       :type :MANAGER
                       :format 32
                       :data (list timestamp
                                   (xlib:find-atom dpy *wm-selection*)
                                   (xlib:window-id wm-sn-manager)))
      (ewmh-startup)
      (check-for-existing-window wm)
      (loop for key in *grabbed-keystrokes* do
        (grab/ungrab-keystroke key :port port))
      (ewmh-update-desktop))))

(defun stop-wm (wm)
  "Stop xwm"
  (let ((port (port wm)))
    (loop for key in *grabbed-keystrokes* do
      (grab/ungrab-keystroke key :port port :ungrab t))
    ;; check this maybe frame-exit doesn't work
    (maphash (lambda (mirror sheet)
               (declare (ignore mirror))
               (clim-doors:foreign-application-unmanage-xwindow (pane-frame sheet))
               (frame-exit (pane-frame sheet)))
             (slot-value port 'foreign-mirror->sheet))
    (xlib:destroy-window (wm-selection-manager port))
    (setf (wm-selection-manager port) nil)
    (setf (xlib:window-event-mask (xroot wm)) (xlib:make-event-mask))))
