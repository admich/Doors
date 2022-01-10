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

;;;; MENU

;;; compared to McCLIM the menu grab the mouse take the input focus,
;;; and have keyboard navigation. If the drawer return a list of
;;; presentations the keyboard navigation is activated. The drawer can
;;; return as second value the default presentation.
(defmethod menu-choose-from-drawer
    (menu presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation &aux presentations default)
  (declare (ignore cache unique-id
                   id-test cache-value cache-test default-presentation))
  (with-room-for-graphics (menu :first-quadrant nil)
    (multiple-value-setq  (presentations default)
      (funcall drawer menu presentation-type)))
  (when (and presentations (null default))
    (setf default (first presentations)))
  (adjust-menu-size-and-position menu :x-position x-position
                                      :y-position y-position)
  ;; The menu is enabled (make visible) after the size is adjusted.
  (enable-menu menu)
  (when default
    (setf (stream-pointer-position menu)
          (with-bounding-rectangle* (min-x min-y max-x max-y) default
            (values (+ min-x (floor (- max-x min-x) 2))
                    (+ min-y (floor (- max-y min-y) 2))))))
  (with-pointer-grabbed ((port menu) menu)
    (with-input-focus (menu)
      (let ((*pointer-documentation-output* pointer-documentation)
            (*abort-gestures* (append *menu-choose-abort-gestures*
                                      *abort-gestures*))
            (*accelerator-gestures* '(:next :prev :return))
            (ntot (length presentations))
            (n (position default presentations)))
        (with-input-context (`(or ,presentation-type blank-area) :override t)
            (object type event)
            (labels ((next ()
                       (when presentations
                         (highlight-output-record default menu :unhighlight)
                         (setf n (mod (1+ n) ntot)
                               default (nth n presentations))))
                     (prev ()
                       (when presentations
                         (highlight-output-record default menu :unhighlight)
                         (setf n (mod (1- n) ntot)
                               default (nth n presentations))))
                     (ret ()
                       (when presentations
                         (throw-highlighted-presentation
                          default *input-context*
                          (multiple-value-bind (x y) (output-record-position default)
                            (multiple-value-setq (x y)
                              (transform-position (sheet-native-transformation menu) x y))
                            (make-instance 'pointer-button-press-event
                                           :sheet menu
                                           :x x :y y
                                           :modifier-state 0
                                           :button +pointer-left-button+))))))
              (loop (handler-case
                        (prog1 nil
                          (loop
                            (highlight-output-record default menu :highlight)
                            (read-gesture :stream menu)))
                      (abort-gesture () (return-from menu-choose-from-drawer nil))
                      (accelerator-gesture (c)
                        (gesture-case (accelerator-gesture-event c)
                          (:next (next))
                          (:prev (prev))
                          (:return (ret)))))))
          (blank-area nil)
          (t (values object event)))))))

;;; Comapared to McCLIM return the list of presentations and the default presentation
(defun draw-standard-menu
    (stream presentation-type items default-item
     &key item-printer
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y &aux  presentations default-presentation)
  (orf item-printer #'print-menu-item)
  (format-items items
                :stream stream
                :printer
                (lambda (item stream)
                  (ecase (menu-item-option item :type :item)
                    (:item
                     ;; This is a normal item, just output.
                     (push
                      (let ((activep (menu-item-option item :active t)))
                        (with-presentation-type-decoded (name params options)
                            presentation-type
                          (let ((*allow-sensitive-inferiors* activep))
                            (with-text-style
                                (stream (menu-item-option
                                         item :style
                                         '(:sans-serif nil nil)))
                              (with-output-as-presentation
                                  (stream
                                   item
                                   `((,name ,@params)
                                     :description ,(getf (menu-item-options item) :documentation)
                                     ,@options)
                                   :single-box t)
                                (funcall item-printer item stream))))))
                      presentations)
                     (when (eql default-item item)
                       (setf default-presentation (first presentations))))
                    (:label
                     ;; This is a static label, it should not be
                     ;; mouse-sensitive, but not grayed out either.
                     (with-text-style (stream (menu-item-option
                                               item :style
                                               '(:sans-serif nil nil)))
                       (funcall item-printer item stream)))
                    (:divider
                     ;; FIXME: Should draw a line instead.
                     (with-text-style (stream (menu-item-option
                                               item :style
                                               '(:sans-serif :italic nil)))
                       (funcall item-printer item stream)))))
                :presentation-type nil
                :x-spacing x-spacing
                :y-spacing y-spacing
                :n-columns n-columns
                :n-rows n-rows
                :max-width max-width
                :max-height max-height
                :cell-align-x cell-align-x
                :cell-align-y (or cell-align-y :top)
                :row-wise row-wise)
  (values (nreverse presentations) default-presentation))
;;; some keysym
(in-package :clim-xcommon)
(define-keysym :XF86-Audio-Lower-Volume #x1008FF11)
(define-keysym :XF86-Audio-Mute #x1008FF12)
(define-keysym :XF86-Audio-Raise-Volume #x1008FF13)
