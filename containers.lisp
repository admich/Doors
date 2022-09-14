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

(in-package :doors)

;;;; wm-ornaments-pane
(defparameter *ornaments-height* 15)

(defclass wm-ornaments-pane (basic-gadget
                             immediate-sheet-input-mixin)
  ())

(defmethod compose-space ((pane wm-ornaments-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :width *ornaments-height*  
                          :height *ornaments-height*
                          :max-height *ornaments-height*))

(defmethod handle-repaint ((pane wm-ornaments-pane) region)
  (let* ((region (sheet-region pane))
         (title (frame-pretty-name (pane-frame pane)))
         (ink (if (eql (pane-frame pane) (active-frame (port pane))) +blue+ +lightsteelblue+)))
    (with-bounding-rectangle* (x1 y1 x2 y2) region
      (draw-rectangle* pane x1 y1 x2 y2 :filled t :ink ink)
      (draw-text* pane title 5 y2 :align-y :bottom))))

;; (defmethod handle-event ((pane wm-ornaments-pane) (event pointer-enter-event))
;;   (a:when-let ((panel (wm-panel *wm-application*)))
;;     (clime:frame-display-pointer-documentation-string panel "L: Move  R: Resize")))

;; (defmethod handle-event ((pane wm-ornaments-pane) (event pointer-exit-event))
;;   (a:when-let ((panel (wm-panel *wm-application*)))
;;     (clime:frame-display-pointer-documentation-string panel "")))

(defmethod handle-event ((pane wm-ornaments-pane) (event pointer-button-press-event))
  (let* ((button (pointer-event-button event))
         (outer (sheet-parent pane))
         (graft  (graft pane))
         (pointer (port-pointer (port pane)))
         (frame (pane-frame pane))
         (port (port frame)))
    (cond
      ((eql button +pointer-left-button+)
       (multiple-value-bind (x y) (transform-position (sheet-delta-transformation outer graft) 0 0)
         (setf (pointer-position pointer) (values x y)))
       ;; (a:when-let ((panel (wm-panel *wm-application*)))
       ;;   (clime:frame-display-pointer-documentation-string panel "Drag to move"))
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
                                    ;; (a:when-let ((panel (wm-panel *wm-application*)))
                                    ;;   (clime:frame-display-pointer-documentation-string panel ""))
        		                    (return-from track)))))
      ((eql button +pointer-right-button+)
       ;; (a:when-let ((panel (wm-panel *wm-application*)))
       ;;   (clime:frame-display-pointer-documentation-string panel "Drag to resize"))
       (multiple-value-bind (w h) (bounding-rectangle-size outer)
         (multiple-value-bind (x y) (transform-position (sheet-delta-transformation outer graft) w h)
           (setf (pointer-position pointer) (values x y))))
       (block track
         (tracking-pointer (outer :multiple-window nil)
           (:pointer-motion (x y)
                            (resize-sheet outer x y))
           (:pointer-button-release ()
                                    ;; (a:when-let ((panel (wm-panel *wm-application*)))
                                    ;;   (clime:frame-display-pointer-documentation-string panel ""))
                                    (return-from track))))))
    (setf (frame-properties frame :position) nil)
    (setf (active-frame port) frame)))

;; stack container
(defclass stack-top-level-sheet-pane (climi::top-level-sheet-pane climi::vbox-pane)
  ((ornaments :accessor wm-ornaments)
   (border :accessor border :initform 2))
  (:documentation "A frame container with ornaments for stack frame manager")
  (:default-initargs :background +blue+))

(defmethod initialize-instance :after ((pane stack-top-level-sheet-pane) &rest args)
  (declare (ignore args))
  (let* ((frame (pane-frame pane))
         (fm (frame-manager frame)))
    (setf (slot-value pane 'ornaments)
          (make-pane-1 fm frame
                       'wm-ornaments-pane
                       :foreground +white+
                       :background +blue+
                       :height 20 :max-height 20 :min-height 20)))
  (sheet-adopt-child pane (slot-value pane 'ornaments)))

(defun remove-ornaments (tls)
  (sheet-disown-child tls (wm-ornaments tls)))

(defun add-ornaments (tls)
  (sheet-adopt-child tls (wm-ornaments tls))
  (reorder-sheets tls (reverse (sheet-children tls))))

(defun tls-fullscreen-p (tls)
  (= 1 (length (sheet-children tls))))

(defmethod border :around ((pane stack-top-level-sheet-pane))
  (if (tls-fullscreen-p pane)
      0
      (call-next-method)))

(defmethod compose-space :around ((pane stack-top-level-sheet-pane) &key (width 100) (height 100))
  (setf (climi::pane-space-requirement pane) nil)
  (let* ((border (border pane))
         (delta (* 2 border))
         (width (max (- width delta) delta))
         (height (max (- height delta) delta))
         (sr (call-next-method pane :width width :height height)))
      (make-space-requirement
       :width (+ delta (space-requirement-width sr))
       :height (+ delta (space-requirement-height sr))
       :min-width (+ delta (space-requirement-min-width sr))
       :min-height (+ delta (space-requirement-min-height sr))
       :max-width (+ delta (space-requirement-max-width sr))
       :max-height (+ delta (space-requirement-max-height sr)))))

(defmethod allocate-space ((pane stack-top-level-sheet-pane) width height)
  (let* ((border (border pane))
         (border2 (* 2 border))
         (real-width (max (- width border2) border2))
         (real-height (max (- height border2) border2)))
    (multiple-value-bind (heights widths)
        (climi::box-layout-mixin/vertically-allocate-space-aux* pane real-width real-height)
      (loop with spacing = (slot-value pane 'climi::y-spacing)
            with y = 0
            with align-x = (climi::pane-align-x pane)
            with align-y = (climi::pane-align-y pane)
            for child in (climi::box-layout-mixin-clients pane)
            for height in heights
            for width in widths
            do (a:when-let ((pane (climi::box-client-pane child)))
                 (climi::layout-child pane align-x align-y
                                      border y
                                      real-width
                                      height)
                 (incf y spacing))
               (incf y height)))))

(defmethod repaint-sheet :after ((sheet stack-top-level-sheet-pane) (region climi::everywhere-region))
  (a:when-let ((ornaments (wm-ornaments sheet)))
    (repaint-sheet ornaments region)))
