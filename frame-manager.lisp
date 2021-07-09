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

(defclass doors-frame-manager (standard-frame-manager)
  ((mirroring :initarg :mirroring
              :initform :full
              :reader mirroring)
   (class-gensym :initarg :class-gensym
                 :initform (gensym "DOORS-")
                 :reader class-gensym)))

;;; We use &ALLOW-OTHER-KEYS since the INITIALIZE-INSTANCE for
;;; CLX-PORT passes various initargs that CLX-FRAME-MANAGER doesn't
;;; necessarily accept.
(defmethod initialize-instance :after ((instance doors-frame-manager)
                                       &key &allow-other-keys))

;; Abstract pane lookup logic copied from clim-clx

(defmethod find-concrete-pane-class ((fm doors-frame-manager)
                                     pane-type &optional errorp)
  ;; This backend doesn't have any specialized pane implementations
  ;; but depending on circumstances it may add optional mirroring to
  ;; the class by defining an ad-hoc subclass. Such automatically
  ;; defined concrete classes use a name that is interned in the
  ;; backend package and derived from the original class name by
  ;; including a gensym prefix, the original symbol package and the
  ;; original symbol name.
  (declare (ignore errorp))
  (maybe-add-mirroring-superclasses
   (call-next-method) (mirroring fm)
   (symbol-name (class-gensym fm)) (find-package '#:clim-doors)
   (lambda (concrete-pane-class)
     `(,(find-class 'mirrored-sheet-mixin)
       ,@(unless (subtypep concrete-pane-class 'sheet-with-medium-mixin)
           `(,(find-class 'permanent-medium-sheet-output-mixin)))
       ,concrete-pane-class))))

;;; Default mirroring predicate
(defun add-mirroring-superclasses-p (class mirroring)
  (cond ((functionp mirroring)
         (funcall mirroring class))
        ((subtypep class 'mirrored-sheet-mixin)
         nil)
        ((and (eq mirroring :single)
              (subtypep class 'top-level-sheet-pane))
         t)
        ((and (eq mirroring :full)
              (subtypep class 'basic-pane))
         t)
        ((and (eq mirroring :random) ; for testing
              (or (subtypep class 'top-level-sheet-pane)
                  (zerop (random 2)))))))

;;; This is an example of how MAKE-PANE-1 might create specialized
;;; instances of the generic pane types based upon the type of the
;;; frame manager. However, in the CLX case, we don't expect there to
;;; be any CLX specific panes. CLX uses the default generic panes
;;; instead.
(defun maybe-add-mirroring-superclasses
    (concrete-pane-class mirroring
     class-name-prefix class-name-package compute-superclasses)
  (flet ((make-class-name (concrete-class-name)
           (intern (format nil "~A-~A:~A"
                           class-name-prefix
                           (alexandria:if-let ((package (symbol-package
                                              concrete-class-name)))
                             (package-name package)
                             "UNINTERNED")
                           (symbol-name concrete-class-name))
                   class-name-package))
         (define-class (metaclass name concrete-class)
           (let* ((superclasses (funcall compute-superclasses concrete-class))
                  (class (make-instance metaclass
                                        :name name
                                        :direct-superclasses superclasses)))
             (setf (find-class name) class))))
    (if (add-mirroring-superclasses-p concrete-pane-class mirroring)
        (multiple-value-bind (concrete-class concrete-class-name)
            (if (typep concrete-pane-class 'class)
                (values concrete-pane-class (class-name concrete-pane-class))
                (values (find-class concrete-pane-class) concrete-pane-class))
          (multiple-value-bind (class-symbol foundp)
              (make-class-name concrete-class-name)
            (if foundp
                (find-class class-symbol)
                (define-class (class-of concrete-class)
                              class-symbol
                              concrete-class))))
        concrete-pane-class)))

(defmethod adopt-frame :before ((fm doors-frame-manager) (frame menu-frame))
  ;; Temporary kludge.
  (when (eq (slot-value frame 'climi::top) nil)
    (multiple-value-bind (x y)
        (xlib:query-pointer (clim-clx::clx-port-window (port fm)))
      (incf x 10)
      (setf (slot-value frame 'climi::left) x
            (slot-value frame 'climi::top) y))))

(defmethod adopt-frame :after ((fm doors-frame-manager) (frame menu-frame))
  (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
    (xlib:map-window
     (clim-clx::window (sheet-direct-mirror (slot-value frame 'top-level-sheet))))))

(defmethod adopt-frame :after
    ((fm doors-frame-manager) (frame standard-application-frame))
  (let ((sheet (slot-value frame 'top-level-sheet)))
    (let* ((top-level-sheet (frame-top-level-sheet frame))
           (mirror (sheet-direct-mirror top-level-sheet))
           (window (clim-clx::window mirror)))
      (case (clime:find-frame-type frame)
        (:override-redirect (setf (xlib:window-override-redirect window) :on))
        (:dialog (xlib:change-property window
                                       :_NET_WM_WINDOW_TYPE
                                       (list (xlib:intern-atom
                                              (xlib:window-display window)
                                              :_NET_WM_WINDOW_TYPE_DIALOG))
                                       :atom 32)))
      (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
        (declare (ignore w h))
        (when (and x y)
          (setf (xlib:drawable-x window) x
                (xlib:drawable-y window) y))
        (clim-clx::tell-window-manager-about-space-requirements top-level-sheet))
      ;; :structure-notify events were not yet turned on, turn them
      ;; on now, so that we get informed about the windows position
      ;; (and possibly size), when the window gets maped.
      (setf (xlib:window-event-mask window)
            (logior (xlib:window-event-mask window)
                    (xlib:make-event-mask :structure-notify)))
      ;; Care for calling-frame, be careful not to trip on missing bits
      (let* ((calling-frame (frame-calling-frame frame))
             (tls (and calling-frame (frame-top-level-sheet calling-frame)))
             (calling-mirror (and tls (sheet-mirror tls))))
        (when calling-mirror
          (setf (xlib:transient-for window) (clim-clx::window calling-mirror))))
      ;;
      (when (sheet-enabled-p sheet)
        (xlib:map-window window)))))

(defclass doors-stack-frame-manager (doors-frame-manager)
  ())

(defclass doors-desktop-frame-manager (doors-stack-frame-manager)
  ())

(defclass doors-tile-frame-manager (doors-frame-manager)
  ())

(defclass doors-onroot-frame-manager (doors-frame-manager)
  ())

(defclass doors-fullscreen-frame-manager (doors-onroot-frame-manager)
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

(defmethod find-frame-container ((fm doors-frame-manager) (frame application-frame))
  (graft frame))

(defmethod find-frame-container ((fm doors-desktop-frame-manager) (frame application-frame))
  (find-pane-named *wm-application* 'doors::desktop))

(defmethod find-pane-for-frame ((fm doors-stack-frame-manager) (frame application-frame))
    (let* ((tls (make-pane-1 fm frame 'top-level-sheet-pane
                            :name (frame-name frame)
                            :pretty-name (frame-pretty-name frame)
                            :icon (clime:frame-icon frame)
                            ;; sheet is enabled from enable-frame
                            :enabled-p nil))
           (ornaments-pane (make-pane-1 fm frame 'wm-ornaments-pane
                                        :managed-frame frame
                                        :foreground +white+
                                        :background +blue+
                                        :height 20 :max-height 20 :min-height 20))
           (outer (vertically () ornaments-pane tls)))
      (sheet-adopt-child (find-frame-container fm frame) outer)
      tls))

(defmethod adopt-frame :after
    ((fm doors-stack-frame-manager) (frame standard-application-frame))
  (let* ((tls (frame-top-level-sheet frame))
         (outer (sheet-parent tls)))
    (resize-sheet outer 1000 500)
    (allocate-space outer 1000 500)
    (layout-frame frame 1000 480)))

(defmethod disown-frame :around
    ((fm doors-stack-frame-manager) (frame standard-application-frame))
  (let* ((tpl-sheet (frame-top-level-sheet frame))
         (outer (sheet-parent tpl-sheet)))
    (sheet-disown-child (sheet-parent outer) outer)
    (call-next-method))
  frame)

(defmethod adopt-frame :after ((fm doors-fullscreen-frame-manager) (frame standard-application-frame))
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
                (setf left (round x)
                      top (round y)
                      width (round w)
                      height (round h)))))))

(defmethod enable-frame :around ((frame application-frame))
  (call-next-method)
  (set-xwindow-state (clim-clx::window (sheet-mirror (frame-top-level-sheet frame))) +normal-state+))

(defmethod disable-frame :around ((frame application-frame))
  (xlib:delete-property (clim-clx::window (sheet-mirror (frame-top-level-sheet frame))) :WM_STATE)
  (call-next-method))

(defmethod shrink-frame :around ((frame application-frame))
  (set-xwindow-state (clim-clx::window (sheet-mirror (frame-top-level-sheet frame))) +iconic-state+)
  (call-next-method))

(defgeneric maximize-frame (frame-manager frame)
  (:documentation "Maximize the FRAME according to the policy of the FRAME-MANAGER")
  (:method ((frame-manager standard-frame-manager) frame)
    t)
  (:method ((frame-manager doors-desktop-frame-manager) frame)
    (let* ((top-sheet (sheet-parent (frame-top-level-sheet frame)))
           (desktop-region (sheet-region (sheet-parent top-sheet)))
           (w (bounding-rectangle-width desktop-region))
           (h (bounding-rectangle-height desktop-region)))
      (move-and-resize-sheet top-sheet 0 0 w h)
      (allocate-space top-sheet w h))))
