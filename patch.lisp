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

;; set mirror transformation also for top-level-sheet-pane
(defun %set-mirror-geometry (sheet &key
                                     (MT (make-translation-transformation -5 -5))
                                     (MR (make-rectangle* 0 0 1 1))
                                     (invalidate-transformations nil))
  
  (setf (%sheet-mirror-region sheet) MR)
  (setf (%sheet-mirror-transformation sheet) MT)
  (when (and (sheet-direct-mirror sheet)
             (not (eql *configuration-event-p* sheet)))
    (let ((port (port sheet))
          (mirror (sheet-direct-mirror sheet)))
      (port-set-mirror-region port mirror MR)
      ;; TOP-LEVEL-SHEET-PANE is our window (and it is managed by the window
      ;; manager - decorations and such. We can't pinpoint exact translation. On
      ;; the other hand UNMANAGED-TOP-LEVEL-SHEET-PANE is essential for menus
      ;; and has exact position set (thanks to not being managed by WM).
      ;; (unless (and (typep sheet 'top-level-sheet-pane)
      ;;              (null (typep sheet 'unmanaged-top-level-sheet-pane)))
      ;; 	(port-set-mirror-transformation port mirror MT))

      ;; doors is the window manager
      (port-set-mirror-transformation port mirror MT)
      )
    (when invalidate-transformations
      (with-slots (native-transformation device-transformation) sheet
        (setf native-transformation nil
              device-transformation nil)))))

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

;; compared to stanard mccclim method here I don't do nothing, the
;; top-level-sheet adopt the (frame-panes frame) not here but in the
;; adopt-frame
(defmethod generate-panes :after (fm (frame application-frame))
  (declare (ignore fm)))


(in-package :clim-xcommon)
(define-keysym :XF86-Audio-Lower-Volume #x1008FF11)
(define-keysym :XF86-Audio-Mute #x1008FF12)
(define-keysym :XF86-Audio-Raise-Volume #x1008FF13)
