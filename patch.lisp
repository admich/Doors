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

;;;; I made a PR on McCLIM that was reverted in b6299768
 (defmethod disown-frame :before
     ((fm headless-frame-manager) (frame application-frame))
   (disable-frame frame)
   (alexandria:removef (slot-value fm 'frames) frame))

;;;; Possible PR to make move and resize sheet easier
(defmethod move-and-resize-sheet ((sheet basic-sheet) x y width height)
  (move-sheet sheet x y)
  (resize-sheet sheet width height))

;; in the PR remove also %set-sheet-region-and-transformation and
#+nil
(defmethod handle-event ((sheet top-level-sheet-pane)
                         (event window-configuration-event))

  (let ((x (window-configuration-event-x event))
        (y (window-configuration-event-y event))
        (width (window-configuration-event-width event))
        (height (window-configuration-event-height event)))
    (let ((*configuration-event-p* sheet))
      (move-and-resize-sheet sheet x y width height))))

;;; compared to mcclim we never set *configuration-event-p* because
;;; the actual move and resize is made in application thread
(defmethod handle-event ((sheet top-level-sheet-pane)
                         (event window-configuration-event))
  (let ((x (window-configuration-event-x event))
        (y (window-configuration-event-y event))
        (width (window-configuration-event-width event))
        (height (window-configuration-event-height event)))
    (move-and-resize-sheet sheet x y width height)))

;;; compared to McCLIM the menu take the input focus
(defmethod menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation)
  (declare (ignore cache unique-id
                   id-test cache-value cache-test default-presentation))
  (with-room-for-graphics (menu :first-quadrant nil)
    (funcall drawer menu presentation-type))

  (adjust-menu-size-and-position menu :x-position x-position
                                      :y-position y-position)
  ;; The menu is enabled (make visible) after the size is adjusted.
  (enable-menu menu)
  (let ((*pointer-documentation-output* pointer-documentation)
        (*abort-gestures* (append *menu-choose-abort-gestures*
                                  *abort-gestures*)))
    (with-input-focus (menu)
      (handler-case
          (with-input-context (`(or ,presentation-type blank-area) :override t)
              (object type event)
              (prog1 nil (loop (read-gesture :stream menu)))
            (blank-area nil)
            (t (values object event)))
        (abort-gesture () nil)))))

;;; FARE McCLIM PR aggiungere il caso con presentation-type
(defmethod frame-manager-menu-choose
    (frame-manager items    ; XXX specialize on STANDARD-FRAME-MANAGER
     &rest options
     &key associated-window printer presentation-type
     (default-item nil default-item-p)
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows (n-columns 1) x-spacing y-spacing row-wise
     cell-align-x cell-align-y (scroll-bars :vertical)
     ;; We provide pointer documentation by default.
     (pointer-documentation *pointer-documentation-output*))
  (flet ((drawer (stream type)
           (draw-standard-menu stream type items
                               (if default-item-p
                                   default-item
                                   (first items))
                               :item-printer
                               (cond
                                 (presentation-type
                                  (lambda (menu-item stream)
                                    (present menu-item presentation-type :stream stream)))
                                 (printer printer)
                                 (t #'print-menu-item))
                               :max-width max-width
                               :max-height max-height
                               :n-rows n-rows
                               :n-columns n-columns
                               :x-spacing x-spacing
                               :y-spacing y-spacing
                               :row-wise row-wise
                               :cell-align-x cell-align-x
                               :cell-align-y cell-align-y)))
    (multiple-value-bind (object event)
        (with-menu (menu associated-window
                         :label label
                         :scroll-bars scroll-bars)
          (when text-style
            (setf (medium-text-style menu) text-style))
          (letf (((stream-default-view menu) +textual-menu-view+))
            (menu-choose-from-drawer menu (or presentation-type 'menu-item)
                                     #'drawer
                                     :cache cache
                                     :unique-id unique-id
                                     :id-test id-test
                                     :cache-value cache-value
                                     :cache-test cache-test
                                     :pointer-documentation pointer-documentation)))
      (unless (null event)              ; Event is NIL if user aborted.
        (let ((subitems (menu-item-option object :items 'menu-item-no-items)))
          (if (eq subitems 'menu-item-no-items)
              (values (menu-item-value object) object event)
              (apply #'frame-manager-menu-choose
                     frame-manager subitems
                     options)))))))
;;; some keysym
(in-package :clim-xcommon)
(define-keysym :XF86-Audio-Lower-Volume #x1008FF11)
(define-keysym :XF86-Audio-Mute #x1008FF12)
(define-keysym :XF86-Audio-Raise-Volume #x1008FF13)
