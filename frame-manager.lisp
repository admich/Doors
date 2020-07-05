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

(defparameter *wm-application* '())

(defclass doors-frame-manager (clim-clx::clx-frame-manager)
  ((class-gensym :initarg :class-gensym
		 :initform (gensym "DOORS-")
		 :reader class-gensym)))

(defclass doors-stack-frame-manager (doors-frame-manager)
  ())

(defclass doors-desktop-frame-manager (doors-stack-frame-manager)
  ())

(defclass doors-tile-frame-manager (doors-frame-manager)
  ())

(defclass doors-fullscreen-frame-manager (doors-frame-manager)
  ())

(defclass doors-onroot-frame-manager (doors-frame-manager)
  ())

(defmethod climi::port-frame-manager-conforms-to-options-p ((port doors-port) frame-manager &rest options)
  (and (call-next-method)
       (let ((fm-type (getf options :fm-type)))
         (if fm-type
             (case fm-type
               (:fullscreen (typep frame-manager 'doors-fullscreen-frame-manager))
               (:stack (typep frame-manager 'doors-stack-frame-manager))
               (:desktop (typep frame-manager 'doors-desktop-frame-manager))
               (:onroot (typep frame-manager 'doors-onroot-frame-manager))
               (:tile (typep frame-manager 'doors-tile-frame-manager)))
             t))))

;;;; wm-ornaments-pane

(defclass wm-ornaments-pane (basic-gadget
                             clim-stream-pane)
  ((managed-frame :initarg :managed-frame :initform nil :accessor managed-frame)))

(defmethod compose-space ((pane wm-ornaments-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width  15
                          :height 15
                          :max-height 15))

(defmethod handle-event ((pane wm-ornaments-pane) (event pointer-enter-event))
  (clime:frame-display-pointer-documentation-string *wm-application* "R: Move  L: Resize"))

(defmethod handle-event ((pane wm-ornaments-pane) (event pointer-exit-event))
  (clime:frame-display-pointer-documentation-string *wm-application* ""))

(defmethod handle-event ((pane wm-ornaments-pane) (event pointer-button-press-event))
  (let ((button (pointer-event-button event))
	(t-l-s (frame-top-level-sheet (managed-frame pane))))
    (cond
      ((eql button +pointer-left-button+)
       (setf (stream-pointer-position pane) (values 0 0))
       (clime:frame-display-pointer-documentation-string *wm-application* "Drag to move")
       (block track
         (tracking-pointer (pane)
           (:pointer-motion (window x y)
           		    (multiple-value-bind (x y)
           			(transform-position (sheet-delta-transformation pane (sheet-parent t-l-s)) x y)
           		      (move-sheet t-l-s x y)))
	   (:pointer-button-release (event x y)
				    (multiple-value-bind (x y)
                        (transform-position (sheet-delta-transformation pane (sheet-parent t-l-s)) x y)
				      (move-sheet t-l-s x y))
                    (clime:frame-display-pointer-documentation-string *wm-application* "")
				    (return-from track)))))
      ((eql button +pointer-right-button+)
       (clime:frame-display-pointer-documentation-string *wm-application* "Drag to resize")
       (multiple-value-bind (w h) (bounding-rectangle-size t-l-s)
         (setf (stream-pointer-position pane) (values w h)))
       (block track
         (tracking-pointer (pane)
           (:pointer-motion (window x y)
           		    (multiple-value-bind (x y)
                                        (transform-position (sheet-delta-transformation pane t-l-s) x y)
                                      (resize-sheet t-l-s x y)))
           (:pointer-button-release (event x y)
                                    (multiple-value-bind (x y)
                                        (transform-position (sheet-delta-transformation pane t-l-s) x y)
                                      (resize-sheet t-l-s x y))
                                    (clime:frame-display-pointer-documentation-string *wm-application* "")
                                    (return-from track))))))))

(define-presentation-type application-frame ())

(define-presentation-method present (object (type application-frame) stream view &key)
  (declare (ignore view))
  (format stream " ~a " (frame-pretty-name object)))

(defmethod generate-panes ((fm doors-stack-frame-manager) (frame application-frame))
  (call-next-method)
  (with-look-and-feel-realization (fm frame)
    (let ((frame-panes (frame-panes frame))
          (ornaments-pane (make-pane 'wm-ornaments-pane
                                     :managed-frame frame
                                     :foreground +white+ :background +blue+ :height 20 :max-height 20 :min-height 20)))
      (setf (frame-panes frame) (vertically () ornaments-pane frame-panes)))))

(defmethod disown-frame ((fm doors-stack-frame-manager) (frame application-frame))
  (alexandria:when-let* ((event-queue (climi::frame-event-queue frame))
                         (calling-frame (climi::frame-calling-frame frame))
                         (calling-queue (climi::frame-event-queue calling-frame))
                         (another-queue-p (not (eql calling-queue event-queue))))
    (setf (climi::event-queue-port event-queue) nil))
  (setf (slot-value fm 'climi::frames) (remove frame (slot-value fm 'climi::frames)))
  (sheet-disown-child (sheet-parent (frame-top-level-sheet frame))
                      (frame-top-level-sheet frame))
  (setf (climi::%frame-manager frame) nil)
  (setf (slot-value frame 'climi::state) :disowned)
  (port-force-output (port fm))
  frame)

(defmethod adopt-frame :after ((fm doors-fullscreen-frame-manager) (frame application-frame))
  (let ((t-l-s (frame-top-level-sheet frame)))
    (move-and-resize-sheet t-l-s 0 0 (graft-width (graft frame)) (graft-height (graft frame))))
  (layout-frame frame (graft-width (graft frame)) (graft-height (graft frame))))

(defun save-frame-geometry (frame)
  "Save the actual geometry of the frame FRAME in the slots of the FRAME"
  (let ((t-l-s (frame-top-level-sheet frame)))
          (multiple-value-bind (x y) (transform-position (sheet-delta-transformation  t-l-s (sheet-parent t-l-s)) 0 0)
            (multiple-value-bind (w h) (bounding-rectangle-size (sheet-region t-l-s))
              (with-slots ((left climi::geometry-left)
                           (top climi::geometry-top)
                           (width climi::geometry-width)
                           (height climi::geometry-height)) frame
                (setf left x
                      top y
                      width w
                      height h))))))
