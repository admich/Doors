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

(defparameter *emergency-event-mask* (xlib:make-event-mask :button-press :button-release :pointer-motion :button-motion :enter-window :leave-window :key-press ))

;; (defmacro with-emergency (&body body)
;;   (alexandria:with-gensyms (emergency-continuation)
;;     `(flet ((,emergency-continuation () ,@body))
;;        (invoke-with-emergency #',emergency-continuation))))

;; (defun invoke-with-emergency (continuation)
;;   (let ((dpy (xlib:open-default-display)))
;;     (unwind-protect
;;          (let* ((screen (first (xlib:display-roots dpy)))
;;                 (root (xlib:screen-root screen))
;;                 (win (xlib:create-window
;;                       :parent root
;;                       :width 800
;;                       :height 600
;;                       :x 0
;;                       :y 0
;;                       :override-redirect :on
;;                       :background (xlib:alloc-color (xlib:screen-default-colormap screen) (xlib:make-color :red 0 :green 0 :blue 1))
;;                       :event-mask *emergency-event-mask*))
;;                 (e-code (xlib:keysym->keycodes dpy (car (xlib:character->keysyms #\e dpy))))
;;                 (e-state 12))
;;            (setf (xlib:wm-name win) "Doors Emergency window")
;;            (setf (xlib:wm-icon-name win) "Doors Emergency window")           
;;            (xlib:grab-key root e-code :modifiers e-state)
;;            (clim-sys:make-process continuation)
;;            (xlib:event-case (dpy)
;;              (:key-press (code state window)
;;                          (cond 
;;                            ((and (eq code e-code) (eq state e-state))
;;                             (xlib:map-window win)
;;                             (xlib:display-force-output dpy)
;;                             (xlib:set-input-focus dpy win :parent)
;;                             nil)
;;                            ((eq code (xlib:keysym->keycodes dpy (car (xlib:character->keysyms #\q dpy))))
;;                             (frame-exit *wm-application*)
;;                             (setf *wm-application* nil)
;;                             (xlib:unmap-window win)
;;                             t)
;;                            (t nil)
;;                            )))
;;            (xlib:ungrab-key root (xlib:keysym->keycodes dpy (car (xlib:character->keysyms #\e dpy))) :modifiers 12))
;;       (xlib:close-display dpy))))
(defun emergency-start (dpy)
  (let* ((screen (first (xlib:display-roots dpy)))
         (root (xlib:screen-root screen))
         (win (xlib:create-window
               :parent root
               :width 800
               :height 600
               :x 0
               :y 0
               :override-redirect :on
               :background (xlib:alloc-color (xlib:screen-default-colormap screen) (xlib:make-color :red 0 :green 0 :blue 1))
               :event-mask *emergency-event-mask*)))
    (setf (xlib:wm-name win) "Doors Emergency window")
    (setf (xlib:wm-icon-name win) "Doors Emergency window")
    (xlib:map-window win)
    (xlib:display-force-output dpy)
    (xlib:set-input-focus dpy win :parent)
    (xlib:event-case (dpy)
      (:key-press (code state window)
                  (cond                     
                    ((eq code (xlib:keysym->keycodes dpy (car (xlib:character->keysyms #\q dpy))))
                     (xlib:unmap-window win)
                     (sb-ext:exit)
                     t)
                    ((eq code (xlib:keysym->keycodes dpy (car (xlib:character->keysyms #\c dpy))))
                     (xlib:unmap-window win)
                     t)
                    (t nil)
                    )))))
