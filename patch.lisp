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

;;;; Some patch that change normal McCLIM behaviour.

(in-package :climi)

;;; find-frame-manager with options
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


;;; top-level-sheet-pane: allow top-level-sheet-pane with multiple child
(defclass top-level-sheet-pane (top-level-sheet-mixin)
  ()
  (:documentation "For the first pane in the architecture"))

(defclass standard-top-level-sheet-pane (top-level-sheet-pane
                                         single-child-composite-pane)
  ())

(defclass unmanaged-top-level-sheet-pane (unmanaged-sheet-mixin standard-top-level-sheet-pane)
  ()
  (:documentation "Top-level sheet without window manager intervention"))

(defmethod allocate-space ((pane standard-top-level-sheet-pane) width height)
  (unless (pane-space-requirement pane)
    (setf (pane-space-requirement pane)
          (compose-space pane)))
  (when-let ((child (sheet-child pane)))
    (allocate-space child
                    (clamp width  (sr-min-width pane)  (sr-max-width pane))
                    (clamp height (sr-min-height pane) (sr-max-height pane)))))

(defmethod allocate-space ((pane top-level-sheet-pane) width height)
 (call-next-method))

(defmethod find-pane-for-frame
    ((fm standard-frame-manager) (frame standard-application-frame))
  (make-pane-1 fm frame 'standard-top-level-sheet-pane
               :name (frame-name frame)
               :pretty-name (frame-pretty-name frame)
               :icon (frame-icon frame)
               ;; sheet is enabled from enable-frame
               :enabled-p nil))

;;; some keysym
(in-package :clim-xcommon)
(define-keysym :XF86-Audio-Lower-Volume #x1008FF11)
(define-keysym :XF86-Audio-Mute #x1008FF12)
(define-keysym :XF86-Audio-Raise-Volume #x1008FF13)
