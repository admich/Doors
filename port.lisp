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

(defclass doors-pointer (clim-clx::clx-pointer)
  ())

(defclass doors-port (mcclim-truetype::clx-ttf-port)
  ((foreign-mirror->sheet :initform (make-hash-table :test #'equalp))
   (focus :initform nil :accessor %port-keyboard-input-focus)
   (active-frame :initform nil :accessor active-frame)))

(defmethod (setf active-frame) :after (frame (port doors-port))
  (raise-frame frame)
  (when (frame-standard-input frame)
    (stream-set-input-focus (frame-standard-input frame))))

(defmethod port-lookup-sheet :around ((port doors-port) mirror)
  (if (xlib:window-equal mirror (sheet-mirror (graft port)))
      (graft port)
      (call-next-method)))

(defgeneric port-lookup-foreign-sheet (port mirror))
(defgeneric port-register-foreign-application (port sheet mirror))
(defgeneric port-unregister-foreign-application (port sheet mirror))

(defmethod port-lookup-foreign-sheet ((port doors-port) mirror)
  (gethash (xlib:window-id mirror) (slot-value port 'foreign-mirror->sheet)))

(defmethod port-register-foreign-application
    ((port doors-port) sheet mirror)
  (setf (gethash (xlib:window-id mirror) (slot-value port 'foreign-mirror->sheet)) sheet)
  nil)

(defmethod port-unregister-foreign-application
    ((port doors-port) sheet mirror)
  (remhash (xlib:window-id mirror) (slot-value port 'foreign-mirror->sheet))
  nil)


(setf (get :doors :port-type) 'doors-port)
(setf (get :doors :server-path-parser) 'clim-clx::parse-clx-server-path)


(defmethod initialize-instance :after ((port doors-port) &rest args)
  (declare (ignore args))
  (let ((options (cdr (port-server-path port))))
    (pop (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-onroot-frame-manager
		 :port port options)
          (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-tile-frame-manager
		 :port port options)
          (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-fullscreen-frame-manager
		 :port port options)
	  (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-stack-frame-manager
		 :port port options)
	  (slot-value port 'frame-managers))
    (setf (slot-value port 'pointer)
	  (make-instance 'doors-pointer :port port))))

(defmethod make-graft ((port doors-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'doors-graft
		 :port port :mirror (clim-clx::clx-port-window port)
		 :orientation orientation :units units))
        (width (xlib:screen-width (clim-clx::clx-port-screen port)))
        (height (xlib:screen-height (clim-clx::clx-port-screen port))))
    (climi::%%set-sheet-region (make-bounding-rectangle 0 0 width height)
                               graft)
    (push graft (climi::port-grafts port))
    graft))

(defmethod port-keyboard-input-focus ((port doors-port))
  (%port-keyboard-input-focus port))


(defmethod (setf port-keyboard-input-focus) (focus (port doors-port))
  (let ((old-focus (port-keyboard-input-focus port))
        (mirror (sheet-mirror focus)))
    (setf  (%port-keyboard-input-focus port) focus)
    (when (xlib:window-p mirror)
      (xlib:set-input-focus  (clim-clx::clx-port-display port) mirror :parent))
    old-focus))

(defmethod port-set-mirror-transformation :after ((port doors-port) mirror mirror-transformation)
  (xlib:display-force-output (clim-clx::clx-port-display port)))

;;;; Mirrors
(defmethod bury-mirror ((port doors-port) (sheet basic-sheet))
  (let ((mirror (clim-clx::sheet-xmirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      ;; (xlib:circulate-window-down mirror)
      (setf (xlib:window-priority mirror) :below)
      (xlib:display-force-output (clim-clx::clx-port-display port)))))

(defmethod raise-mirror ((port doors-port) (sheet basic-sheet))
  (let ((mirror (clim-clx::sheet-xmirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      ;; (xlib:circulate-window-down mirror)
      (setf (xlib:window-priority mirror) :above)
      (xlib:display-force-output (clim-clx::clx-port-display port)))))

