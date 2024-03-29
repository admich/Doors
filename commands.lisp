;;;; Doors a window manager based on McCLIM.
;;;; Copyright (C) 2021-2022  Andrea De Michele
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

(defparameter *terminal* '("xterm" "xterm"))
(defparameter *browser* '("firefox" "Navigator"))
(defparameter *emacs* '("emacs" "emacs"))

;;;; frame management
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

(define-doors-wm-command-with-grabbed-keystroke (com-last-frame :name t :keystroke (#\TAB :super))
    ()
  (let* ((new-active (second (managed-frames-ordered *application-frame*))))
    (when new-active
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

(macrolet ((define-move-frame-command (position &optional keystroke)
             (let ((com-name (a:symbolicate "COM-FRAME-" position))
                   (keystroke (or keystroke `(,position :super))))
               `(define-doors-wm-command-with-grabbed-keystroke (,com-name :name t :keystroke ,keystroke)
                    ()
                  (let ((frame  (active-frame (port *application-frame*))))
                    (when (member frame (managed-frames *application-frame*))
                      (move-frame (frame-manager frame) frame ,position)))))))
  (define-move-frame-command :up)
  (define-move-frame-command :down)
  (define-move-frame-command :left)
  (define-move-frame-command :right)
  (define-move-frame-command :maximized (#\m :super)))

(defun frame-position (frame)
  (let ((tls (frame-top-level-sheet frame)))
    (transform-position (sheet-delta-transformation tls (graft frame)) 0 0)))

(macrolet ((define-select-frame-command (direction coordinate test)
             (let ((com-name (a:symbolicate "COM-SELECT-FRAME-" direction)))
               `(define-doors-wm-command-with-grabbed-keystroke (,com-name :name t :keystroke (,direction :super :control))
                    ()
                  (let* ((aframe (active-frame (port *application-frame*)))
                         (oframes (loop :for frame :in (desktop-frames (current-desktop *wm-application*))
                                        :when (,test (,coordinate frame) (,coordinate aframe))
                                          :collect frame)))
                    (a:when-let ((new-active (loop :for frame :in (managed-frames-ordered *wm-application*)
                                                   :when (member frame oframes)
                                                     :return frame)))
                      (setf (active-frame (port *application-frame*)) new-active)))))))
  (define-select-frame-command :left frame-position <)
  (define-select-frame-command :right frame-position >)
  (define-select-frame-command :up (lambda (x) (nth-value 1 (frame-position x))) <)
  (define-select-frame-command :down (lambda (x) (nth-value 1 (frame-position x))) >))

(define-doors-wm-command-with-grabbed-keystroke (com-dmenu :keystroke (#\Return :super))
    ()
  (uiop:run-program "dmenu_run -i -b -p \"run command:\""))

(define-doors-wm-command-with-grabbed-keystroke (com-rofi-run :keystroke (#\Return :super :shift))
    ()
  (uiop:run-program "rofi -show run"))

(define-doors-wm-command-with-grabbed-keystroke (com-select-windows :keystroke (#\s :super))
    ()
  (a:when-let ((frame (menu-choose (managed-frames)
                                   :presentation-type 'application-frame
                                   :label "Choose a frame:"
                                   :associated-window (main-graft *wm-application*))))
    (setf (active-frame (port *wm-application*)) frame)))

(define-doors-wm-command-with-grabbed-keystroke (com-select-windows2 :keystroke (#\S :super))
    ()
  (uiop:run-program "rofi -show window"))

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

;;;; MULTIMEDIA

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

;;;; Desktops
(define-command (com-set-current-desktop :name t :command-table doors-wm)
    ((desktop 'desktop :prompt "Select a desktop" :gesture :select))
  (setf (current-desktop *application-frame*) desktop))

(define-command (com-move-frame-to-desktop :name t :command-table doors-wm)
    ((frame 'application-frame :prompt "Select a frame")
     (desktop 'desktop :prompt "Select a desktop"))
  (setf (frame-properties frame :wm-desktop) desktop))


(macrolet ((generate-desktop-keybinding (n)
             (let* ((keychar (elt (format nil "~d" n) 0))
                    (set-name (a:symbolicate "COM-SET-DESKTOP-" keychar))
                    (move-name (a:symbolicate "COM-MOVE-TO-DESKTOP-" keychar)))
               `(progn
                  (define-doors-wm-command-with-grabbed-keystroke (,set-name :keystroke (,keychar :super))
                      ()
                    (a:when-let ((desktop (nth (1- ,n) (desktops *application-frame*))))
                      (setf (current-desktop *application-frame*) desktop)))
                  (define-doors-wm-command-with-grabbed-keystroke (,move-name :keystroke (,keychar :super :control))
                      ()
                    (a:when-let ((desktop (nth (1- ,n) (desktops *application-frame*)))
                                 (frame (active-frame  (port *application-frame*))))
                      (setf (frame-properties frame  :wm-desktop) desktop)
                      (setf (current-desktop *application-frame*) desktop)))))))
  (generate-desktop-keybinding 1)
  (generate-desktop-keybinding 2)
  (generate-desktop-keybinding 3)
  (generate-desktop-keybinding 4)
  (generate-desktop-keybinding 5)
  (generate-desktop-keybinding 6)
  (generate-desktop-keybinding 7)
  (generate-desktop-keybinding 8)
  (generate-desktop-keybinding 9))

(define-command (com-create-desktop :name t :command-table doors-wm)
    ()
  (with-accessors ((desktops desktops)) *application-frame*
    (a:appendf desktops
     (list (make-instance 'desktop
                          :number (length desktops))))))

(define-command (com-remove-desktop :name t :command-table doors-wm)
    ((desktop 'desktop :prompt "Select a desktop to remove"))
  (with-accessors ((desktops desktops)) *application-frame*
    (when (= 1 (length desktops))
      (warn "I need at least one desktop")
      (return-from com-remove-desktop nil))
    (let ((frames (desktop-frames desktop))
          (new-desk (nth (mod (1- (desktop-number desktop)) (length desktops)) desktops)))
      (mapc #'(lambda (f) (setf (frame-properties f :wm-desktop) new-desk)) frames)
      (when (eql desktop (current-desktop *application-frame*))
        (setf (current-desktop *application-frame*) new-desk))
      (a:removef desktops desktop)
      (renumber-desktops *application-frame*))))

;;;; keystroke second-map
(define-command-table second-map :inherit-from (doors-wm))

(define-gesture-name abort-second-map :keyboard (#\c :control))
(define-gesture-name abort-second-map :keyboard (#\g :control) :unique nil)
(define-gesture-name abort-second-map :keyboard :escape :unique nil)

(define-command (com-abort-second-map :command-table second-map :name t :keystroke abort-second-map)
       ()
  nil)

(add-keystroke-to-command-table 'second-map '(#\a :super) :command 'com-last-frame :errorp nil)

(define-doors-wm-command-with-grabbed-keystroke (com-second-map :keystroke (#\a :super)) 
    ()
  (let* ((old-table (frame-command-table *application-frame*))
         (table (find-command-table 'second-map))
         (root-window (clim-clx::window (sheet-mirror (find-graft))))
         (port (port *application-frame*))
         (dpy (clim-clx::clx-port-display port))
         (cursor (gethash :busy (clim-clx::clx-port-cursor-table port))))
    (unwind-protect
         (progn
           ;;;; here I use directly xlib to grab pointer and keyboard,
           ;;;; maybe its better to use some McCLIM functions/macro
           (xlib:grab-pointer root-window nil :owner-p nil :cursor cursor)
           (xlib:grab-keyboard root-window :owner-p nil)
           (setf (frame-command-table *application-frame*) table) ;; necessary because the commands must be enabled in frame
           (let ((command (read-frame-command *application-frame*)))
             (execute-frame-command *application-frame* command)))
      (setf (frame-command-table *application-frame*) old-table)
      (xlib:ungrab-pointer dpy)
      (xlib:ungrab-keyboard dpy))))

(defun find-foreign-application (win-class)
  (loop for frame in (managed-frames)
        when (and (typep frame 'clim-doors:foreign-application)
                  (string= win-class (xlib:get-wm-class (clim-doors:foreign-xwindow frame))))
          collect frame))

(defmacro define-run-or-raise (name sh-command win-class keystroke)
  (let ((command-name (a:symbolicate "COM-" name))
        (gesture-name (a:symbolicate "KEYSTROKE-" name)))
    `(progn
       (define-gesture-name ,gesture-name :keyboard ,keystroke)
       (define-gesture-name ,gesture-name :keyboard (,keystroke :super) :unique nil)
       (define-command (,name :name t :command-table doors-wm)
           ()
         (alexandria:if-let (frames (find-foreign-application ,win-class))
           (setf (active-frame (port *application-frame*)) (car frames))
           (uiop:launch-program ,sh-command)))
       (add-keystroke-to-command-table 'second-map ',gesture-name :command ',name))))

(define-run-or-raise com-emacs (first *emacs*) (second *emacs*) #\e)

(define-run-or-raise com-browser (first *browser*) (second *browser*) #\b)

(define-run-or-raise com-terminal (first *terminal*) (second *terminal*) #\t)

(defmacro define-command-with-second-map-keystroke (name keystroke &body body)
  (let ((gesture-name (a:symbolicate "KEYSTROKE-" name)))
    `(progn
       (define-gesture-name ,gesture-name :keyboard ,keystroke)
       (define-gesture-name ,gesture-name :keyboard (,keystroke :super) :unique nil)
       (define-command (,name :name t :command-table doors-wm)
           ()
         ,@body)
       (add-keystroke-to-command-table 'second-map ',gesture-name :command ',name))))

(define-command-with-second-map-keystroke com-listener #\l
  (let ((frame (car (member "Listener" (managed-frames) :key  #'frame-pretty-name  :test #'string=))))
    (if frame
        (setf (active-frame (port *application-frame*)) frame)
        (clim-listener:run-listener :width 1000 :height 600 :new-process t))))

(define-command-with-second-map-keystroke com-new-listener #\L
  (clim-listener:run-listener :width 1000 :height 600 :new-process t))

(define-command-with-second-map-keystroke com-editor #\E
  (find-application-frame 'climacs::climacs))
