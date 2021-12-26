(in-package :doors)

;; KLUDGE to call menu-choose with :associated-window the graft although it isn't a pane
(defmethod pane-frame ((pane doors-graft))
  *wm-application*)

(defmethod make-pane-1 :around (realizer (frame doors-wm)
                                abstract-class-name &rest initargs)
;;  (call-next-method)
  (apply #'make-pane-1 realizer climi::*default-application-frame*
         abstract-class-name initargs)
  )


;;; The doors graft don't call tell-window-manager-about-space-reqiurements
(defmethod note-space-requirements-changed :around ((graft doors-graft) pane)
  (declare (ignore graft))
  nil)
#|
(menu-choose (managed-frames) :associated-window (graft *wm-application*))
|#


