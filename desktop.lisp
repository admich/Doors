;;;; virtual desktop
(in-package :doors)

(defclass desktop ()
  ((number :initarg :number
           :initform 0
           :accessor desktop-number)
   (active-p :initform nil
             :initarg :active
             :accessor desktop-active-p)))

(defmethod (setf frame-properties) :around (value (frame standard-application-frame) (property (eql :wm-desktop)))
  (declare (ignore property))
  (call-next-method)
  (xlib:change-property (clim-doors::xwindow-for-properties frame) :_NET_WM_DESKTOP
                          (list (position value (doors::desktops *wm-application*)))
                          :cardinal 32)
  (if (eql (current-desktop *wm-application*) value)
      (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)
      (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil)))

(defun desktop-frames (desktop)
  (remove-if-not #'(lambda (frame) (eql desktop (frame-properties frame :wm-desktop))) (managed-frames)))

(define-presentation-method present (object (type desktop) stream view &key)
  (declare (ignore view))
  (if (desktop-active-p object)
      (with-text-face (stream :bold)
        (format stream " ~d " (+ 1 (desktop-number object))))
      (format stream " ~d " (+ 1 (desktop-number object)))))

