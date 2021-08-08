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

(defun find-foreign-application (win-class)
  (let ((table (slot-value (port *wm-application*) 'clim-doors::foreign-mirror->sheet)))
    (loop for pane being the hash-value of table
          when (string= win-class (xlib:get-wm-class (clim-doors::foreign-xwindow pane)))
            collect (pane-frame pane))))

(defmacro define-run-or-raise (name sh-command win-class keystroke)
  `(define-doors-wm-command-with-grabbed-keystroke (,name :name t :keystroke ,keystroke)
       ()
     (alexandria:if-let (frames (find-foreign-application ,win-class))
       (setf (active-frame (port *application-frame*)) (car frames))
       (uiop:launch-program ,sh-command))))

(define-run-or-raise com-emacs (first *emacs*) (second *emacs*) (#\E :super))

(define-run-or-raise com-browser (first *browser*) (second *browser*) (#\b :super))

(define-run-or-raise com-terminal (first *terminal*) (second *terminal*) (#\t :super))

(define-doors-wm-command-with-grabbed-keystroke (com-listener :name t :keystroke (#\l :super))
    ()
  (let ((frame (car (member "Listener" (managed-frames) :key  #'frame-pretty-name  :test #'string=))))
    (if frame
        (setf (active-frame (port *application-frame*)) frame)
        (clim-listener:run-listener :width 1000 :height 600 :new-process t))))

(define-doors-wm-command-with-grabbed-keystroke (com-new-listener :name t :keystroke (#\L :super))
    ()
  (clim-listener:run-listener :width 1000 :height 600 :new-process t))

(define-doors-wm-command-with-grabbed-keystroke (com-editor :name t :keystroke (#\e :super))
    ()
  (find-application-frame 'climacs::climacs))

(define-doors-wm-command-with-grabbed-keystroke (com-next-frame :name t :keystroke (#\n :super))
    ()
  (alexandria:when-let ((frames (desktop-frames (current-desktop *wm-application*))))
    (let* ((old-active (active-frame (port *application-frame*)))
           (old-position (or (position old-active frames) 0))
           (new-active (nth (mod (1+ old-position) (length frames)) frames)))
      (setf (active-frame (port *application-frame*)) new-active))))

(define-doors-wm-command-with-grabbed-keystroke (com-previous-frame :name t :keystroke (#\p :super))
    ()
  (alexandria:when-let ((frames (desktop-frames (current-desktop *wm-application*))))
    (let* ((old-active (active-frame (port *application-frame*)))
           (old-position (or (position old-active frames) 0))
           (new-active (nth (mod (1- old-position) (length frames)) frames)))
      (setf (active-frame (port *application-frame*)) new-active))))

(define-doors-wm-command-with-grabbed-keystroke (com-banish-pointer :name t :keystroke (#\. :super))
    ()
  (setf (pointer-position (port-pointer (port *application-frame*)))
        (values (graft-width (graft *application-frame*))
                (graft-height (graft *application-frame*)))))

(define-command (com-frame-focus :name t :command-table  doors-wm)
    ((frame 'application-frame :gesture :select))
  (setf (active-frame (port *application-frame*)) frame))

(define-command (com-frame-toggle-fullscreen :name t :command-table doors-wm)
    ((frame 'application-frame :default (active-frame (port *application-frame*))))
  (fullscreen-frame (frame-manager frame) frame)
  (setf (active-frame (port frame)) frame))

(define-doors-wm-command-with-grabbed-keystroke (com-fullscreen :name t :keystroke (#\Space :super))
    ()
  (let ((frame  (active-frame (port *application-frame*))))
    (when (member frame (managed-frames))
      (com-frame-toggle-fullscreen frame))))

(define-doors-wm-command-with-grabbed-keystroke (com-maximize :name t :keystroke (#\m :super))
    ()
  (let ((frame  (active-frame (port *application-frame*))))
    (when (member frame (managed-frames *application-frame*))
      (maximize-frame (frame-manager frame) frame))))

;; (define-presentation-to-command-translator
;;     com-frame-toggle-fullscreen
;;     (application-frame com-frame-toggle-fullscreen doors
;;      :gesture nil
;;      :documentation "Toggle Fullscreen")
;;     (object)
;;     (list object))

(define-doors-wm-command-with-grabbed-keystroke (com-dmenu :keystroke (#\Return :super))
    ()
  (uiop:run-program "dmenu_run -i -b -p \"run command:\""))

;; (defun programs-in-path ()
;;   "Return a list of all programs in PATH env variable"
;;   (loop for dir  in (ppcre:split ":" (uiop:getenv "PATH"))
;;         appending (map 'list #'pathname-name (uiop:directory-files (uiop:ensure-directory-pathname dir)))))

;; (define-presentation-type program-name () :inherit-from 'string)

;; (define-presentation-method presentation-typep (object (type program-name))
;;   (and nil (stringp object)
;;         (find object  (programs-in-path)
;;               :test #'string=)))

;; ;; define-presentation-method presentation-subtypep ?

;; (define-presentation-method accept ((type string) stream (view textual-view)
;;                                     &key)
;;   (let* ((suggestions (programs-in-path))
;;          (obj (completing-from-suggestions (stream)
;;                 (dolist (x suggestions)
;;                  (suggest x x)))))
;;       obj))

;; (define-doors-command (com-run :name t)
;;     ((command 'program-name :prompt "Command")
;;      (args '(or null (sequence string)) :prompt "Arguments" :default '()))
;;   (format (frame-query-io *application-frame*) "~s" (cons command args))
;;   (uiop:launch-program (cons command args)))

;; (define-doors-command-with-grabbed-keystroke (com-bury-all :name t :keystroke (#\_ :super))
;;     ()
;;   (let* ((frames (managed-frames)))
;;     (map nil #'bury-frame frames)))

;; (define-doors-command-with-grabbed-keystroke (com-goto-wm-interactor :keystroke (#\i :super))
;;     ()
;;   (setf (frame-current-layout *wm-application*) 'with-interactor)
;;   (stream-set-input-focus (frame-standard-input *wm-application*)))

;; (define-doors-command-with-grabbed-keystroke (com-toggle-interactor
;;                                              :keystroke     (#\I :super))
;;     ()
;;   (let ((frame *application-frame*))
;;     (setf (frame-current-layout frame)
;;           (case (frame-current-layout frame)
;;             (with-interactor    'without-interactor)
;;             (without-interactor 'with-interactor)))))

(define-doors-wm-command-with-grabbed-keystroke (com-quit-doors :name t :keystroke (#\Q :super))
    ()
  (setf *wm-application* nil)
  (frame-exit *application-frame*))

(define-command (com-frame-kill :name t :command-table doors-wm)
    ((frame 'application-frame :gesture :delete))
  (queue-event (frame-top-level-sheet frame)
               (make-instance 'window-manager-delete-event :sheet (frame-top-level-sheet frame))))

(define-doors-wm-command-with-grabbed-keystroke (com-kill :keystroke (#\K :super))
    ()
  (let ((frame  (active-frame (port *application-frame*))))
    (when (member frame (managed-frames))
      (com-frame-kill frame))))

;; ;;;; MULTIMEDIA

(define-doors-wm-command-with-grabbed-keystroke (com-audio-mute :name t :keystroke (:xf86-audio-mute)) 
    ()
  (let* ((out (uiop:run-program "amixer -D default sset Master toggle" :output :string))
         (state (cl-ppcre:scan-to-strings "\\[(on|off)\\]" out)))
    (format (frame-query-io *application-frame*) "Audio: ~a" state)))

(define-doors-wm-command-with-grabbed-keystroke (com-audio-increase-volume :name t :keystroke (:xf86-audio-raise-volume)) 
    ()
  (let* ((out (uiop:run-program "amixer -D default sset Master 1%+" :output :string))
         (state (cl-ppcre:scan-to-strings "\\[([0-9]*%)\\]" out)))
    (format (frame-query-io *application-frame*) "Audio Volume: ~a" state)))

(define-doors-wm-command-with-grabbed-keystroke (com-audio-decrease-volume :name t :keystroke (:xf86-audio-lower-volume)) 
    ()
  (let* ((out (uiop:run-program "amixer -D default sset Master 1%-" :output :string))
         (state (cl-ppcre:scan-to-strings "\\[([0-9]*%)\\]" out)))
    (format (frame-query-io *application-frame*) "Audio Volume: ~a" state)))

(define-command (com-set-current-desktop :name t :command-table doors-wm)
    ((desktop 'desktop :prompt "Select a desktop" :gesture :select))
  (setf (current-desktop *application-frame*) desktop))

(define-command (com-move-frame-to-desktop :name t :command-table doors-wm)
    ((frame 'application-frame :prompt "Select a frame")
     (desktop 'desktop :prompt "Select a desktop"))
  (setf (frame-properties frame :wm-desktop) desktop))

(define-doors-wm-command-with-grabbed-keystroke (com-set-desktop-0 :keystroke (#\1 :super))
    ()
  (setf (current-desktop *application-frame*) (first (desktops *application-frame*))))

(define-doors-wm-command-with-grabbed-keystroke (com-set-desktop-1 :keystroke (#\2 :super))
    ()
  (setf (current-desktop *application-frame*) (second (desktops *application-frame*))))

(define-doors-wm-command-with-grabbed-keystroke (com-move-to-desktop-0 :keystroke (#\1 :super :control))
    ()
  (let ((desktop (elt (desktops *application-frame*) 0)))
    (setf (frame-properties (active-frame  (port *application-frame*)) :wm-desktop)
          desktop)
    (setf (current-desktop *application-frame*) desktop)))

(define-doors-wm-command-with-grabbed-keystroke (com-move-to-desktop-1 :keystroke (#\2 :super :control))
    ()
  (let ((desktop (elt (desktops *application-frame*) 1)))
    (setf (frame-properties (active-frame  (port *application-frame*)) :wm-desktop)
          desktop)
    (setf (current-desktop *application-frame*) desktop)))



