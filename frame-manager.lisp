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
                 :initform 'DOORS-
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

(defmethod adopt-frame :after ((fm doors-frame-manager) (frame standard-application-frame))
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
        (move-sheet top-level-sheet (or x 0) (or y 0)))
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


