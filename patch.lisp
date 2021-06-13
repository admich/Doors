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

;;;; Some patch that change normal McCLIM behaviour.

(in-package :climi)
;;;; some bug in McCLIM (make PR for this

;;; graft must work on application-frame
(defmethod graft ((frame application-frame))
  (when-let ((tls (frame-top-level-sheet frame)))
    (graft tls)))

;;;;
(defun find-frame-manager (&rest options &key port &allow-other-keys)
  (declare (special *frame-manager*))
  (climi::with-keywords-removed (options (:port))
    (if (and (boundp '*frame-manager*)
             (or (null port)
                 (and (eql port (port *frame-manager*))
                      (apply #'port-frame-manager-conforms-to-options-p port *frame-manager* options))))
        *frame-manager*
        (if (and *default-frame-manager*
                 (frame-manager-p *default-frame-manager*)
                 (or (null port)
                     (and
                      (eql port (port *default-frame-manager*))
                      (apply #'port-frame-manager-conforms-to-options-p port *default-frame-manager* options))))
            *default-frame-manager*
            (progn
              (unless port (setf port (apply #'find-port options)))
              (loop for frame-manager in (frame-managers port)
                 when (apply #'port-frame-manager-conforms-to-options-p port frame-manager options)
                   do (return frame-manager)
                 finally
                   (first (frame-managers port))))))))

(defgeneric port-frame-manager-conforms-to-options-p (port frame-manager &rest options)
  (:documentation "Check if FRAME-MANAGER conforms to OPTIONS")
  (:method (port frame-manager &rest options)
    (declare (ignore options))
    (eql port (port frame-manager))))

(defmethod (setf frame-manager) (fm (frame application-frame))
  (let ((old-manager (frame-manager frame)))
    (when old-manager
      (disown-frame old-manager frame))
    (when fm (adopt-frame fm frame))))

;; compared to stanard mccclim method here (frame-panes frame) can be
;; not a direct child of the top-level-sheet but some panes could be
;; in the middle (e.g. layout container for wm ornament)
(defmethod generate-panes :before (fm  (frame application-frame))
  (declare (ignore fm))
  (when (and (frame-panes frame) (sheet-parent (frame-panes frame)))
    (sheet-disown-child (sheet-parent (frame-panes frame))(frame-panes frame)))
  (loop for (nil . pane) in (frame-panes-for-layout frame)
        for parent = (sheet-parent pane)
        if  parent
     do (sheet-disown-child parent pane)))

;;; compared to standard mccclim method here I ensure that the
;;; top-level-sheet is an ancestor of frame-pane. This is done by
;;; find-pane-for-frame.
(defmethod (setf frame-current-layout) :around (name (frame application-frame))  
  (unless (eql name (frame-current-layout frame))
    (call-next-method)
    (alexandria:when-let ((fm (frame-manager frame)))
      (generate-panes fm frame)
      (setf (slot-value frame 'climi::top-level-sheet)
            (find-pane-for-frame (frame-manager frame) frame))
       (layout-frame frame)
       (signal 'frame-layout-changed :frame frame))))

;;; mcclim version
;; (defmethod (setf frame-current-layout) :around (name (frame application-frame))
;;   (unless (eql name (frame-current-layout frame))
;;     (call-next-method)
;;     (when-let ((fm (frame-manager frame)))
;;       (if-let ((tls (and (frame-resize-frame frame)
;;                          (frame-top-level-sheet frame))))
;;         (multiple-value-bind (width height)
;;             (bounding-rectangle-size tls)
;;           (generate-panes fm frame)
;;           (layout-frame frame width height))
;;         (progn
;;           (generate-panes fm frame)
;;           (layout-frame frame)))
;;       (signal 'frame-layout-changed :frame frame))))


;;; compared to mcclim when the port is the WM we don't need to do nothing here
(defmethod handle-event ((sheet top-level-sheet-pane)
                         (event window-configuration-event))
  (when (and (null (clim-doors::wm-selection-manager (port sheet)))
             (eql (sheet-parent sheet) (graft sheet)))
    (let ((x (window-configuration-event-x event))
          (y (window-configuration-event-y event))
          (width (window-configuration-event-width event))
          (height (window-configuration-event-height event)))
      (let ((*configuration-event-p* sheet))
      (%set-sheet-region-and-transformation
       sheet
       (make-bounding-rectangle 0 0 width height)
       ;; negative offsets are handled by the native transformation?
       (make-translation-transformation x y))))))

;;; some keysym
(in-package :clim-xcommon)
(define-keysym :XF86-Audio-Lower-Volume #x1008FF11)
(define-keysym :XF86-Audio-Mute #x1008FF12)
(define-keysym :XF86-Audio-Raise-Volume #x1008FF13)
