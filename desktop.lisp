;;;; virtual desktop
(in-package :doors)

(defclass desktop ()
  ((number :initarg :number
           :initform 0
           :accessor desktop-number)
   (frames :initarg :frames
           :initform '()
           :accessor desktop-frames)
   (active-p :initform nil
             :initarg :active
             :accessor desktop-active-p)))

(defmethod (setf frame-properties) :around (value (frame application-frame) (property (eql :wm-desktop)))
  (declare (ignore property))
  (call-next-method)
  (if (eql (current-desktop *wm-application*) value)
      (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)
      (setf (sheet-enabled-p (frame-top-level-sheet frame)) nil)))

(define-presentation-method present (object (type desktop) stream view &key)
  (declare (ignore view))
  (if (desktop-active-p object)
      (with-text-face (stream :bold)
        (format stream " ~d " (desktop-number object)))
      (format stream " ~d " (desktop-number object))))
