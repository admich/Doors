;;;; Doors a window manager based on McCLIM.
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

(in-package :clim-doors)
(defparameter *wm-application* '())

;;;; wm-ornaments-pane
(defparameter *ornaments-height* 15)

(defclass wm-ornaments-pane (basic-gadget
                             immediate-sheet-input-mixin)
  ((managed-frame :initarg :managed-frame :initform nil :accessor managed-frame)))

(defmethod compose-space ((pane wm-ornaments-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width *ornaments-height*  
                          :height *ornaments-height*
                          :max-height *ornaments-height*))

(defmethod handle-repaint ((pane wm-ornaments-pane) region)
  (let* ((region (sheet-region pane))
         (title (frame-pretty-name (pane-frame pane))))
    (with-bounding-rectangle* (x1 y1 x2 y2) region
      (draw-text* pane title 5 y2 :align-y :bottom))))

(defmethod handle-event ((pane wm-ornaments-pane) (event pointer-enter-event))
  (clime:frame-display-pointer-documentation-string *wm-application* "L: Move  R: Resize"))

(defmethod handle-event ((pane wm-ornaments-pane) (event pointer-exit-event))
  (clime:frame-display-pointer-documentation-string *wm-application* ""))

(defmethod handle-event ((pane wm-ornaments-pane) (event pointer-button-press-event))
  (let ((button (pointer-event-button event))
        (outer (sheet-parent pane))
        (graft  (graft pane))
        (pointer (port-pointer (port pane))))
    (cond
      ((eql button +pointer-left-button+)
       (multiple-value-bind (x y) (transform-position (sheet-delta-transformation outer graft) 0 0)
         (setf (pointer-position pointer) (values x y)))
       (clime:frame-display-pointer-documentation-string *wm-application* "Drag to move")
       (block track
         (tracking-pointer (outer :multiple-window nil)
           (:pointer-motion (x y)
                            (multiple-value-bind (x y)
                                  (transform-position (sheet-delta-transformation pane (sheet-parent outer)) x y)
                                (move-sheet outer x y)))
           (:pointer-button-release (event x y)
                                    (multiple-value-bind (x y)
                                        (transform-position (sheet-delta-transformation (event-sheet event) (sheet-parent outer)) x y)
                                      (move-sheet outer x y))
                    (clime:frame-display-pointer-documentation-string *wm-application* "")
        		    (return-from track)))))
      ((eql button +pointer-right-button+)
       (clime:frame-display-pointer-documentation-string *wm-application* "Drag to resize")
       (multiple-value-bind (w h) (bounding-rectangle-size outer)
         (multiple-value-bind (x y) (transform-position (sheet-delta-transformation outer graft) w h)
           (setf (pointer-position pointer) (values x y))))
       (block track
         (tracking-pointer (outer :multiple-window nil)
           (:pointer-motion (x y)
                            (resize-sheet outer x y)
                            (allocate-space outer x y))
           (:pointer-button-release (x y)
                                    (clime:frame-display-pointer-documentation-string *wm-application* "")
                                    (return-from track))))))))

;; stack container
(defclass stack-container-pane (vbox-pane)
  ()
  (:documentation "A frame container with ornaments for stack frame manager"))
