;;;; Doors a window manager based on McCLIM.
;;;; Copyright (C) 2021  Andrea De Michele
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

(in-package :doors)

;;;; Doors panel
(define-application-frame doors-panel ()
  ((tray :initarg :tray
         :reader panel-tray
         :initform nil))
  (:command-table (doors-panel :inherit-from (doors-wm)))
  (:menu-bar nil)
  (:panes
   (info :application
         :incremental-redisplay t
         :display-function 'display-info :max-height 15 :scroll-bars nil)
   (interactor :interactor :height 24)
   (pointer-doc :pointer-documentation :scroll-bars nil)
   (tray (make-pane 'doors-systray:tray-pane :background +white+)))
  (:layouts
   (with-interactor
       (vertically ()
          interactor pointer-doc (horizontally () (:fill info) tray)))
   (without-interactor
       (vertically ()
         pointer-doc (horizontally () (:fill info) tray)))))

(defmethod find-pane-for-frame
    ((fm doors-wm) (frame doors-panel))
  (let ((tls (make-pane-1 fm frame 'climi::standard-top-level-sheet-pane
               :name (frame-name frame)
               :pretty-name (frame-pretty-name frame)
               :icon (clime:frame-icon frame)
               ;; sheet is enabled from enable-frame
               :enabled-p nil)))
    (sheet-adopt-child (find-frame-container fm frame) tls)
    tls))

(defclass info-line-event (window-manager-event) ())

(defmethod handle-event ((frame doors-panel) (event info-line-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame 'info))
  (clime:schedule-event (find-pane-named frame 'info)
                        (make-instance 'info-line-event :sheet frame)
                        1))

(defmethod run-frame-top-level :before ((frame doors-panel) &key &allow-other-keys)
  (queue-event (find-pane-named frame 'info) (make-instance 'info-line-event :sheet frame))
  (when (panel-tray frame)
    (doors-systray:start-tray (find-pane-named frame 'tray))))

(defmethod default-frame-top-level :around ((frame doors-panel)
                                            &key &allow-other-keys)
  (setf (frame-properties frame :wm-desktop) :all-desktops)
  (call-next-method))

(defmethod execute-frame-command ((frame doors-panel) command)
  (if (command-present-in-command-table-p (car command) 'doors-wm)
      (execute-frame-command *wm-application* command)
      (call-next-method)))


;;;; Commands
(define-presentation-to-command-translator
    com-frame-toggle-fullscreen
    (application-frame com-frame-toggle-fullscreen doors-panel 
     :gesture nil
     :documentation "Toggle Fullscreen")
    (object)
  (list object))

(defun programs-in-path ()
  "Return a list of all programs in PATH env variable"
  (loop for dir  in (ppcre:split ":" (uiop:getenv "PATH"))
        appending (map 'list #'pathname-name (uiop:directory-files (uiop:ensure-directory-pathname dir)))))

;;; CHECK THIS
(define-presentation-type program-name () :inherit-from 'string)

(define-presentation-method presentation-typep (object (type program-name))
  (and nil (stringp object)
        (find object  (programs-in-path)
              :test #'string=)))

;; define-presentation-method presentation-subtypep ?

(define-presentation-method accept ((type program-name) stream (view textual-view)
                                    &key)
  (let* ((suggestions (programs-in-path))
         (obj (completing-from-suggestions (stream)
                (dolist (x suggestions)
                 (suggest x x)))))
      obj))

(define-doors-panel-command (com-run :name t)
    ((command 'program-name :prompt "Command")
     (args '(or null (sequence string)) :prompt "Arguments" :default '()))
  (format (frame-query-io *application-frame*) "~s" (cons command args))
  (uiop:launch-program (cons command args)))

(define-doors-wm-command-with-grabbed-keystroke (com-goto-wm-interactor :keystroke (#\i :super))
    ()
  (a:when-let ((panel (wm-panel *application-frame*)))
    (setf (frame-current-layout panel) 'with-interactor)
    (stream-set-input-focus (frame-standard-input panel))))

(define-doors-wm-command-with-grabbed-keystroke (com-toggle-interactor :keystroke (#\I :super))
    ()
  (let ((panel (wm-panel *application-frame*)))
    (execute-frame-command panel '(com-change-layout))))

(define-doors-panel-command(com-change-layout :name t)
    ()
  (let* ((panel *application-frame*)
         (tls (frame-top-level-sheet panel)))
    (unwind-protect 
         (setf (frame-current-layout panel)
               (case (frame-current-layout panel)
                 (with-interactor    'without-interactor)
                 (without-interactor 'with-interactor)))
      (move-sheet tls 0 (- (graft-height (graft tls))
                         (bounding-rectangle-height (sheet-region tls)))))))

(defun start-panel (&key new-process tray)
  (assert *wm-application*)
  (let* ((graft (find-graft))
         (height 150)
         (panel (make-application-frame 'doors-panel :tray tray :top (- (graft-height graft) height) :height height :width (graft-width graft))))
    (setf (wm-panel *wm-application*) panel)
    (if new-process
        (clim-sys:make-process #'(lambda () (run-frame-top-level panel)) :name "Doors panel")
        (run-frame-top-level panel))))

(define-doors-panel-command(com-update-info-line :name t)
    ()
  (redisplay-frame-pane *application-frame* 'info)
  (clime:schedule-event (find-pane-named *application-frame* 'info)
                        (make-instance 'info-line-event :sheet *application-frame*)
                        1))

