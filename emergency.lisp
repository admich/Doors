;;;; Copyright (C) 2021  Andrea De Michele
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

(in-package #:clim-doors)

(defun emergency-loop (&optional display)
  (let ((dpy (if display
                 (xlib:open-display "" :display display)
                 (xlib:open-default-display))))
    (unwind-protect
         (let* ((screen (first (xlib:display-roots dpy)))
                (grab-event-mask (xlib:make-event-mask :key-press))
                (root (xlib:screen-root screen))
                (win (xlib:create-window
                      :parent root
                      :width 1
                      :height 1
                      :override-redirect :on
                      :x -10
                      :y -10
                      :background (xlib:alloc-color (xlib:screen-default-colormap screen) (xlib:make-color :red 1 :green 0 :blue 0))
                      :event-mask grab-event-mask)))
           (setf (xlib:wm-name win) "Emergency")
           (setf (xlib:wm-icon-name win) "Emergency")
           (xlib:map-window win)
           (xlib:display-force-output dpy)
           (unwind-protect
                (progn (xlib:grab-key root :any :modifiers (+ 64 8 4)) ;; super meta ctrl
                       (xlib:event-case (dpy)
                         (:key-press (code window)
                                     (case (xlib:keycode->character dpy code 0)
                                       (#\q t)
                                       (#\t (uiop:launch-program "xterm") nil)
                                       (#\e (uiop:launch-program "emacs") nil)
                                       (#\k (when-let ((wm *wm-application*)) (destroy-port (port wm))) nil)
                                       (#\s (doors:doors :new-process t) nil)
                                       (t nil))))))
           (xlib:ungrab-key root :any :modifiers 0))
      (xlib:close-display dpy))))


