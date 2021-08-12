;;;; virtual desktop
(in-package :doors)

(defclass desktop ()
  ((number :initarg :number
           :initform 0
           :accessor desktop-number)
   (active-p :initform nil
             :initarg :active
             :accessor desktop-active-p)))

(defun frame-visible-in-desktop (frame desktop)
  (member (frame-properties frame :wm-desktop)
          (list :all-desktops desktop)))

(defun renumber-desktops (doors-wm)
  (loop for desk in (desktops doors-wm)
        for i = 0 then (1+ i) do
          (setf (desktop-number desk) i)))

(defmethod (setf desktops) :around (value doors-wm)
  (call-next-method)
  (ewmh-update-desktop))

(defmethod (setf frame-properties) :around (value (frame standard-application-frame) (property (eql :wm-desktop)))
  (declare (ignore property))
  (call-next-method)
  (let ((xvalue (if (eql value :all-desktops)
                    #xFFFFFFFF
                    (position value (desktops *wm-application*)))))
    (xlib:change-property (xwindow-for-properties frame) :_NET_WM_DESKTOP
                          (list xvalue)
                          :cardinal 32))
  (if (frame-visible-in-desktop frame (current-desktop *wm-application*))
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


