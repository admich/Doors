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

(in-package #:doors)


;; Xephyr -br -ac -noreset -screen 1920x1080 :1
;; (setf clim:*default-server-path* (list :doors :mirroring :single))
(setf clim:*default-server-path* (list :doors))

(swank/backend:install-debugger-globally #'clim-debugger:debugger)

(defparameter *config-file* (merge-pathnames "doors/config.lisp" (uiop:xdg-config-home)))
(defparameter *terminal* '("xterm" "xterm"))
(defparameter *browser* '("firefox" "Navigator"))
(defparameter *emacs* '("emacs" "emacs"))

(define-application-frame doors ()
  ((start-wm :initarg :start-wm :initform :off :reader doors-start-wm))
  (:panes
   (desktop (make-pane :bboard-pane :background +gray+))
   (info :application
         :incremental-redisplay t
         :display-function 'display-info :max-height 15 :scroll-bars nil)
   (interactor :interactor :height 24)
   (pointer-doc :pointer-documentation :scroll-bars nil)
   (tray (make-pane 'doors-systray:tray-pane :background +white+)))
  (:layouts
   (with-interactor
       (vertically ()
         (:fill desktop) (make-pane 'clime:box-adjuster-gadget)  interactor pointer-doc (horizontally () (:fill info) tray)))
   (without-interactor
       (vertically ()
         (:fill desktop) pointer-doc (horizontally () (:fill info) tray)))))

(defmethod default-frame-top-level :around ((frame doors) &key &allow-other-keys)
  (with-frame-manager ((find-frame-manager :port (port frame) :fm-type :desktop))
    (call-next-method)))

(defun managed-frames (&optional (wm *wm-application*))
  (loop for fm in (climi::frame-managers (port wm))
     unless (eql fm (frame-manager wm))
     appending (frame-manager-frames fm)))

(defmethod dispatch-event ((client doors) event)
  (queue-event client event))

(defmethod handle-event ((client doors) (event window-manager-configuration-request-event))
  (grant-configure-request event))

(defmethod handle-event ((client doors) (event window-manager-map-request-event))
  (unless (port-lookup-foreign-sheet (port client) (window-manager-map-request-event-window event))
    (make-foreign-application (window-manager-map-request-event-window event) :frame-manager (find-frame-manager :port (port client)))))

(defmethod run-frame-top-level :around ((frame doors)
                                        &key &allow-other-keys)
  (let ((port (port frame)))
    (unwind-protect
         (progn
           (case (doors-start-wm frame)
             (:on (start-wm port))
             (:replace (start-wm port t)))
           (load *config-file*)
           (call-next-method))
      (when (wm-selection-manager port)
        (stop-wm port)))))

(defmethod run-frame-top-level :before ((frame doors) &key &allow-other-keys)
  (queue-event (find-pane-named frame 'info) (make-instance 'info-line-event :sheet frame)))

(defclass info-line-event (window-manager-event) ())

(defmethod handle-event ((frame doors) (event info-line-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame 'info))
  (clime:schedule-event (find-pane-named frame 'info)
                  (make-instance 'info-line-event :sheet frame)
                  1))

(defmacro define-doors-command-with-grabbed-keystroke (name-and-options arguments &rest body)
  (let* ((name (if (listp name-and-options)
                   (first name-and-options)
                   name-and-options))
         (options (if (listp name-and-options)
                      (cdr name-and-options)
                      nil))
         (keystroke (getf options :keystroke)))
    `(progn
       (define-doors-command (,name ,@options)
       ,arguments ,@body)
       (when ',keystroke (pushnew ',keystroke *grabbed-keystrokes*))
       (when *wm-application* (grab/ungrab-keystroke ',keystroke :port (port *wm-application*))))))


(defun find-foreign-application (win-class)
  (let ((table (slot-value (port *wm-application*) 'clim-doors::foreign-mirror->sheet)))
    (loop for pane being the hash-value of table
        when (string= win-class (xlib:get-wm-class (clim-doors::foreign-xwindow pane)))
         collect (pane-frame pane))))

(defmacro define-run-or-raise (name sh-command win-class keystroke)
  `(define-doors-command-with-grabbed-keystroke (,name :name t :keystroke ,keystroke)
       ()
     (alexandria:if-let (frames (find-foreign-application ,win-class))
       (setf (active-frame (port *application-frame*)) (car frames))
       (uiop:launch-program ,sh-command))))

(define-run-or-raise com-emacs (first *emacs*) (second *emacs*) (#\E :super))

(define-run-or-raise com-browser (first *browser*) (second *browser*) (#\b :super))

(define-run-or-raise com-terminal (first *terminal*) (second *terminal*) (#\c :super))

(define-doors-command-with-grabbed-keystroke (com-listener :name t :keystroke (#\l :super))
    ()
  (let ((frame (car (member "Listener" (managed-frames) :key  #'frame-pretty-name  :test #'string=))))
    (if frame
        (setf (active-frame (port *application-frame*)) frame)
        (clim-listener:run-listener :width 1000 :height 600 :new-process t))))

(define-doors-command-with-grabbed-keystroke (com-new-listener :name t :keystroke (#\L :super))
    ()
  (clim-listener:run-listener :width 1000 :height 600 :new-process t))

(define-doors-command-with-grabbed-keystroke (com-editor :name t :keystroke (#\e :super))
    ()
  (find-application-frame 'climacs::climacs))

(define-doors-command-with-grabbed-keystroke (com-next-frame :name t :keystroke (#\n :super))
    ()
  (alexandria:when-let ((frames (managed-frames)))
    (let* ((old-active (active-frame (port *application-frame*)))
           (old-position (or (position old-active frames) 0))
           (new-active (nth (mod (1+ old-position) (length frames)) frames)))
      (setf (active-frame (port *application-frame*)) new-active))))

(define-doors-command-with-grabbed-keystroke (com-previous-frame :name t :keystroke (#\p :super))
    ()
  (alexandria:when-let ((frames (managed-frames)))
    (let* ((old-active (active-frame (port *application-frame*)))
           (old-position (or (position old-active frames) 0))
           (new-active (nth (mod (1- old-position) (length frames)) frames)))
      (setf (active-frame (port *application-frame*)) new-active))))

(define-doors-command-with-grabbed-keystroke (com-banish-pointer :name t :keystroke (#\. :super))
    ()
  (setf (pointer-position (port-pointer (port *application-frame*)))
        (values (graft-width (graft *application-frame*))
                (graft-height (graft *application-frame*)))))

(define-doors-command (com-frame-focus :name t)
    ((frame 'application-frame :gesture :select))
  (setf (active-frame (port *application-frame*)) frame))

(define-doors-command (com-frame-toggle-fullscreen :name t)
    ((frame 'application-frame :default (active-frame (port *application-frame*))))
  (if (typep (frame-manager frame) 'clim-doors::doors-fullscreen-frame-manager)
      (progn
        (setf (frame-manager frame) (find-frame-manager :port (port frame) :fm-type :desktop)))
      (progn
        (save-frame-geometry frame)
        (setf (frame-manager frame) (find-frame-manager :port (port frame) :fm-type :fullscreen))))
  (setf (active-frame (port frame)) frame))

(define-doors-command-with-grabbed-keystroke (com-fullscreen :name t :keystroke (#\Space :super))
    ()
  (let ((frame  (active-frame (port *application-frame*))))
    (when (member frame (managed-frames))
      (com-frame-toggle-fullscreen frame))))

(define-doors-command-with-grabbed-keystroke (com-maximize :name t :keystroke (#\m :super))
    ()
  (let ((frame  (active-frame (port *application-frame*))))
    (when (and (member frame (managed-frames)) (eql (frame-manager frame) (find-frame-manager :port (port frame) :fm-type :desktop)))
      (let* ((tls (frame-top-level-sheet frame))
             (desktop-region (sheet-region (find-pane-named *wm-application* 'desktop)))
             (w (bounding-rectangle-width desktop-region))
             (h (bounding-rectangle-height desktop-region)))
        (move-and-resize-sheet tls 0 0 w h)))))

(define-presentation-to-command-translator
    com-frame-toggle-fullscreen
    (application-frame com-frame-toggle-fullscreen doors
     :documentation "Toggle Fullscreen")
    (object)
    (list object))

(define-doors-command-with-grabbed-keystroke (com-dmenu :keystroke (#\Return :super))
    ()
  (uiop:run-program "dmenu_run -i -b -p \"run command:\""))

(define-doors-command (com-run :name t)
    ((command `(member-sequence
                ,(loop for dir  in (ppcre:split ":" (uiop:getenv "PATH"))
                    appending (map 'list #'pathname-name (uiop:directory-files (uiop:ensure-directory-pathname dir)))))
              :prompt "Command")
     (args '(or null (sequence string)) :prompt "Arguments" :default '()))
  (format (frame-query-io *application-frame*) "~s" (cons command args))
  (uiop:launch-program (cons command args)))

(define-doors-command-with-grabbed-keystroke (com-bury-all :name t :keystroke (#\_ :super))
    ()
  (let* ((frames (managed-frames)))
    (map nil #'bury-frame frames)))

(define-doors-command-with-grabbed-keystroke (com-bury-doors :name t :keystroke (#\h :super))
    ()
  (bury-frame *application-frame*))

(define-doors-command-with-grabbed-keystroke (com-raise-doors :name t :keystroke (#\H :super))
    ()
  (raise-frame *application-frame*))

(define-doors-command-with-grabbed-keystroke (com-goto-wm-interactor :keystroke (#\i :super))
    ()
  (setf (frame-current-layout *wm-application*) 'with-interactor)
  (stream-set-input-focus (frame-standard-input *wm-application*)))

(define-doors-command-with-grabbed-keystroke (com-toggle-interactor
                                             :keystroke     (#\I :super))
    ()
  (let ((frame *application-frame*))
    (setf (frame-current-layout frame)
          (case (frame-current-layout frame)
            (with-interactor    'without-interactor)
            (without-interactor 'with-interactor)))))

(define-doors-command-with-grabbed-keystroke (com-quit :name t :keystroke (#\Q :super))
    ()
  (setf *wm-application* nil)
  (frame-exit *application-frame*))

(define-doors-command (com-frame-kill :name t)
    ((frame 'application-frame :gesture :delete))
  (queue-event (frame-top-level-sheet frame)
               (make-instance 'window-manager-delete-event :sheet (frame-top-level-sheet frame))))

(define-doors-command-with-grabbed-keystroke (com-kill :keystroke (#\K :super))
    ()
  (let ((frame  (active-frame (port *application-frame*))))
    (when (member frame (managed-frames))
      (com-frame-kill frame))))

;;;; MULTIMEDIA

(define-doors-command-with-grabbed-keystroke (com-audio-mute :name t :keystroke (:xf86-audio-mute)) 
    ()
  (let* ((out (uiop:run-program "amixer -D default sset Master toggle" :output :string))
         (state (cl-ppcre:scan-to-strings "\\[(on|off)\\]" out)))
    (format (frame-query-io *application-frame*) "Audio: ~a" state)))

(define-doors-command-with-grabbed-keystroke (com-audio-increase-volume :name t :keystroke (:xf86-audio-raise-volume)) 
    ()
  (let* ((out (uiop:run-program "amixer -D default sset Master 1%+" :output :string))
         (state (cl-ppcre:scan-to-strings "\\[([0-9]*%)\\]" out)))
    (format (frame-query-io *application-frame*) "Audio Volume: ~a" state)))

(define-doors-command-with-grabbed-keystroke (com-audio-decrease-volume :name t :keystroke (:xf86-audio-lower-volume)) 
    ()
  (let* ((out (uiop:run-program "amixer -D default sset Master 1%-" :output :string))
         (state (cl-ppcre:scan-to-strings "\\[([0-9]*%)\\]" out)))
    (format (frame-query-io *application-frame*) "Audio Volume: ~a" state)))

(defun doors (&key new-process (port (find-port :server-path '(:doors))) (start-wm :on))
  ;; maybe is necessary to control if therreis another instance
  (let* ((fm (find-frame-manager :port port :fm-type :onroot))
        (frame (make-application-frame 'doors
                                       :frame-manager fm
                                       :start-wm start-wm
                                       :width (graft-width (find-graft :port port))
                                       :height (graft-height (find-graft :port port)))))
    (setf *wm-application* frame)
    (if new-process
        (clim-sys:make-process #'(lambda () (run-frame-top-level frame)) :name "Doors WM")
        (run-frame-top-level frame))))

(defun start-tray ()
  (doors-systray:start-tray (find-pane-named *wm-application* 'tray)))

