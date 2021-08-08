;;;; Copyright (C) 2020, 2021  Andrea De Michele
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

;; (swank/backend:install-debugger-globally #'clim-debugger:debugger)

(defparameter *config-file* (merge-pathnames "doors/config.lisp" (uiop:xdg-config-home)))
(defparameter *terminal* '("xterm" "xterm"))
(defparameter *browser* '("firefox" "Navigator"))
(defparameter *emacs* '("emacs" "emacs"))


;;;; Doors wm
(defclass doors-wm (application-frame clim-doors::doors-frame-manager)
  ((replace-wm :initarg :replace-wm
               :initform nil
               :reader doors-wm-replace-wm)
   (desktops :initarg :desktops
             :initform (list (make-instance 'desktop :number 0 :active t) (make-instance 'desktop :number 1))
             :accessor desktops)
   (current-desktop :initarg :current-desktop
                    :accessor current-desktop)
   (command-table
    :initarg :command-table
    :initform (find-command-table 'doors-wm)
    :accessor frame-command-table)
   (port :initarg :port
         :initform nil
         :reader port)
   (config-file :initarg :config-file :initform *config-file* :reader config-file)
   (main-graft :initarg :main-graft
               :initform nil
               :accessor main-graft)
   ;;;  useful standard-application-frame slots
   (process
    :accessor climi::frame-process
    :initform nil)
   (command-queue
    :initform (make-instance 'climi::concurrent-event-queue :port nil)
    :reader climi::frame-command-queue)
   (event-queue
    :initarg :frame-event-queue
    :initform (if climi::*multiprocessing-p*
                  (make-instance 'climi::concurrent-event-queue)
                  (make-instance 'climi::simple-event-queue))
    :accessor climi::frame-event-queue)
   (reading-command-p
    :initform nil
    :accessor climi::frame-reading-command-p)
   (disabled-commands
    :accessor climi::disabled-commands
    :accessor climi::frame-disabled-commands
    :initarg :disabled-commands
    :initform nil
    :documentation "A list of command names that have been disabled in this frame.")
   (output-pane
    :initform nil
    :accessor frame-standard-output
    :accessor frame-error-output)
   (input-pane
    :initform nil
    :accessor frame-standard-input)
   (properties
    :accessor climi::%frame-properties
    :initarg :properties
    :initform nil)
   (panel
    :accessor wm-panel
    :initarg :panel
    :initform nil)
   (top-level-lambda
    :initform 'default-frame-top-level
    :initarg :top-level-lambda
    :reader climi::frame-top-level-lambda)))

(defmethod initialize-instance :after ((obj doors-wm) &rest initargs)
  (setf (slot-value obj 'current-desktop) (first (slot-value obj 'desktops))
        (slot-value obj 'main-graft) (graft (port obj))))

(defun managed-frames (&optional (wm *wm-application*))
  (remove-if #'(lambda (x) (or (eq (frame-state x) :disabled)
                               (not (typep x 'standard-application-frame))))
             (frame-manager-frames wm)))

(defmethod dispatch-event ((client doors-wm) event)
  (queue-event client event))

(defmethod handle-event ((client doors-wm) (event window-manager-configuration-request-event))
  (grant-configure-request event))

(defmethod handle-event ((client doors-wm) (event window-manager-map-request-event))
  (unless (port-lookup-foreign-sheet (port client) (window-manager-map-request-event-window event))
    (make-foreign-application (window-manager-map-request-event-window event) :frame-manager (find-frame-manager :port (port client)))))

;;; Desktops
(defmethod number-of-desktops ((frame doors-wm))
  (length (desktops frame)))

(defmethod (setf current-desktop) :around (value (frame doors-wm))
  (setf (desktop-active-p (current-desktop frame)) nil)
  (call-next-method )
  (setf (desktop-active-p (current-desktop frame)) t)
  (xlib:change-property (clim-doors::find-root) :_NET_CURRENT_DESKTOP
                          (list (position (doors::current-desktop frame) (doors::desktops frame)))
                          :cardinal 32)
  (dolist (appf (managed-frames frame))
    (if (frame-visible-in-desktop appf (current-desktop frame))
        (setf (sheet-enabled-p (frame-top-level-sheet appf)) t)
        (setf (sheet-enabled-p (frame-top-level-sheet appf)) nil))))

;;;
(defmethod graft ((frame doors-wm))
  (main-graft frame))

(defmethod frame-pointer-documentation-output ((frame doors-wm))
  nil)

(defmethod (setf frame-manager) (new-manager (frame doors-wm))
  (error "Don't set the frame-manager for DOORS-WM frame ~A" frame))

(defmethod frame-manager ((frame doors-wm))
  frame)

(defmethod frame-query-io ((frame doors-wm))
  nil)

(defmethod frame-parent ((frame doors-wm))
  (port frame))

(defmethod frame-state ((frame doors-wm))
  (if (climi::frame-process frame)
      :enabled
      :disabled))

(defmethod run-frame-top-level :around ((frame doors-wm) &key)
  (let ((*application-frame* frame)
        (*input-context* nil)
        (*input-wait-test* nil)
        (*input-wait-handler* nil)
        (*pointer-button-press-handler* nil))
    (declare (special *input-context* *input-wait-test* *input-wait-handler*
                      *pointer-button-press-handler*))
    (loop named run-frame-loop
          for query-io = (frame-query-io frame)
          for *default-frame-manager* = frame
          do (block run-frame-iter
               (handler-bind
                   ((frame-exit
                      (lambda (condition)
                        (unless (%frame-exit-handled condition)
                          (setf (%frame-exit-handled condition) t)
                          (let ((exiting-frame (frame-exit-frame condition)))
                            (if (eq exiting-frame frame)
                                (return-from run-frame-loop)
                                (disown-frame (frame-manager exiting-frame)
                                              exiting-frame)))))))
                 (return-from run-frame-loop
                   (if query-io
                       (with-input-focus (query-io)
                         (climi::letf (((climi::frame-process frame) (clim-sys:current-process)))
                           (funcall (climi::frame-top-level-lambda frame) frame)))
                       (climi::letf (((climi::frame-process frame) (clim-sys:current-process)))
                         (funcall (climi::frame-top-level-lambda frame) frame)))))))))

(defmethod default-frame-top-level :around ((frame doors-wm) &key &allow-other-keys)
  (let ((port (port frame)))
    (unwind-protect
         (progn
           (push frame (slot-value port 'climi::frame-managers))
           (setf *wm-application* frame)
           (start-wm port (doors-wm-replace-wm frame))
           (alexandria:when-let ((config-file (config-file frame)))
             (load config-file))
           (with-frame-manager (frame)
             (call-next-method)))
      (when (wm-selection-manager port)
        (stop-wm port))
      (alexandria:removef (slot-value port 'climi::frame-managers) frame)
      (setf *wm-application* nil))))

(defmethod default-frame-top-level
    ((frame doors-wm)
     &key (command-parser 'command-line-command-parser)
          (command-unparser 'command-line-command-unparser)
          (partial-command-parser
           'command-line-read-remaining-arguments-for-partial-command)
          (prompt "Command: "))
  ;; Give each pane a fresh start first time through.
  (loop
    ;; The variables are rebound each time through the loop because the
    ;; values of frame-standard-input et al. might be changed by a command.
    ;;
    ;; We rebind *QUERY-IO* ensuring variable is always a stream,
    ;; but we use FRAME-QUERY-IO for our own actions and to decide
    ;; whenever frame has the query IO stream associated with it..
    (let* ((frame-query-io (frame-query-io frame))
           (*standard-input*  (or (frame-standard-input frame)  *standard-input*))
           (*standard-output* (or (frame-standard-output frame) *standard-output*))
           (*query-io* (or frame-query-io *query-io*))
           ;; during development, don't alter *error-output*
           ;; (*error-output* (frame-error-output frame))
           (*pointer-documentation-output* (frame-pointer-documentation-output frame))
           (*command-parser* command-parser)
           (*command-unparser* command-unparser)
           (*partial-command-parser* partial-command-parser))
      (restart-case
          (flet ((execute-command ()
                   (alexandria:when-let ((command (read-frame-command frame)))
                     (execute-frame-command frame command))))
            (execute-command))
        (abort ()
          :report "Return to application command loop.")))))

(defmethod command-enabled (command-name (frame doors-wm))
  (and (command-accessible-in-command-table-p command-name
                                              (frame-command-table frame))
       (not (member command-name (climi::disabled-commands frame)))))

(defmethod (setf command-enabled)
    (enabled command-name (frame doors-wm))
  (unless (command-accessible-in-command-table-p command-name
                                                 (frame-command-table frame))
    (return-from command-enabled nil))
  (with-accessors ((disabled-commands climi::disabled-commands))
      frame
    (if enabled
        (progn
          (setf disabled-commands (delete command-name disabled-commands)))
        (progn
          (pushnew command-name disabled-commands)))
    enabled))

(defmethod read-frame-command ((frame doors-wm)
                               &key (stream *standard-input*))
  (declare (ignore stream))
  (let ((command-table (frame-command-table frame))
        (queue (climi::frame-event-queue frame)))
    (do* ((event (climi::event-queue-read queue) (climi::event-queue-read queue))
          (res (handle-event (event-sheet event) event) (handle-event (event-sheet event) event)))
         ((and (consp res)
               (symbolp (car res))
               (command-accessible-in-command-table-p (car res) command-table))
          res))))

(defmethod handle-event ((client doors-wm) (event keyboard-event))
  (when (eql (class-of event) (find-class 'key-press-event))
    (let ((command (lookup-keystroke-command-item
                    event (frame-command-table client))))
      (when (consp command)
        command))))

(defmethod frame-exit ((frame doors-wm))
  (signal 'frame-exit :frame frame))


;;;; Frame manager

(defmethod find-pane-for-frame
    ((fm doors-wm) (frame standard-application-frame))
  (let ((tls (make-pane-1 fm frame 'clim-doors::stack-top-level-sheet-pane
               :name (frame-name frame)
               :pretty-name (frame-pretty-name frame)
               :icon (clime:frame-icon frame)
               ;; sheet is enabled from enable-frame
               :enabled-p nil)))
    (sheet-adopt-child (clim-doors::find-frame-container fm frame) tls)
    tls))


;;; Command table
(make-command-table 'doors-wm :errorp nil)

;; check for variable capture
(defmacro define-doors-wm-command-with-grabbed-keystroke (name-and-options arguments &rest body)
  (let* ((name (if (listp name-and-options)
                   (first name-and-options)
                   name-and-options))
         (options (if (listp name-and-options)
                      (cdr name-and-options)
                      nil))
         (keystroke (getf options :keystroke)))
    `(progn
       (define-command (,name ,@options :command-table doors-wm)
         ,arguments ,@body)
       (when ',keystroke (pushnew ',keystroke *grabbed-keystrokes*))
       (when *wm-application* (grab/ungrab-keystroke ',keystroke :port (port *wm-application*))))))


;;;; Doors panel
(define-application-frame doors-panel ()
  ()
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
    (sheet-adopt-child (clim-doors::find-frame-container fm frame) tls)
    tls))

(defmethod default-frame-top-level :around ((frame doors-panel)
                                            &key &allow-other-keys)
  (setf (frame-properties frame :wm-desktop) :all-desktops)
  (call-next-method))

(defmethod run-frame-top-level :before ((frame doors-panel) &key &allow-other-keys)
  (queue-event (find-pane-named frame 'info) (make-instance 'info-line-event :sheet frame)))

(defclass info-line-event (window-manager-event) ())

(defmethod handle-event ((frame doors-panel) (event info-line-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame 'info))
  (clime:schedule-event (find-pane-named frame 'info)
                        (make-instance 'info-line-event :sheet frame)
                        1))


;;;; startup functions
(defun start-tray (panel)
  (doors-systray:start-tray (find-pane-named panel 'tray)))

(defun start-panel (&key new-process)
  (assert *wm-application*)
  (let* ((graft (find-graft))
         (height 150)
         (panel (make-application-frame 'doors-panel :top (- (graft-height graft) height) :height height :width (graft-width graft))))
    (setf (wm-panel *wm-application*) panel)
    (if new-process
        (clim-sys:make-process #'(lambda () (run-frame-top-level panel)) :name "Doors panel")
        (run-frame-top-level panel))))

(defun doors (&key new-process replace-wm (port (find-port :server-path '(:doors))) (config-file *config-file*))
  (let* ((doorswm (make-instance 'doors-wm
                                 :port port
                                 :replace-wm replace-wm
                                 :config-file config-file)))
    (when *wm-application* (error "Another instance of Doors WM is running: ~A" *wm-application*))
    (if new-process
        (clim-sys:make-process #'(lambda () (run-frame-top-level doorswm)) :name "Doors WM")
        (run-frame-top-level doorswm))))
