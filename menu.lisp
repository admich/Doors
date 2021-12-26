(in-package :doors)

;; KLUDGE to call menu-choose with :associated-window the graft although it isn't a pane
(defmethod pane-frame ((pane doors-graft))
  *wm-application*)

;;; The doors graft don't call tell-window-manager-about-space-reqiurements
(defmethod note-space-requirements-changed :around ((graft doors-graft) pane)
  (declare (ignore graft))
  nil)

#|
(menu-choose (managed-frames) :associated-window (graft *wm-application*))
|#


