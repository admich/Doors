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


;;;; Doors wm
(defclass doors-wm (application-frame doors-frame-manager)
  ((replace-wm :initarg :replace-wm
               :initform nil
               :reader doors-wm-replace-wm)
   (net-supporting-wm-check :initform nil
                            :accessor net-supporting-wm-check)
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

(defun xroot (frame-or-wm)
  (clim-clx::window (sheet-mirror (graft frame-or-wm))))

(defun xdisplay (frame-or-wm)
  (clim-clx::clx-port-display (port frame-or-wm)))

(defmethod initialize-instance :after ((obj doors-wm) &rest initargs)
  (declare (ignore initargs))
  (let ((port (port obj)))
    (setf (slot-value obj 'current-desktop) (first (slot-value obj 'desktops))
          (slot-value obj 'main-graft) (graft port))))

(defun managed-frames (&optional (wm *wm-application*))
  (remove-if #'(lambda (x) (or (eq (frame-state x) :disabled)
                               (not (typep x 'standard-application-frame))))
             (frame-manager-frames wm)))

(defun managed-frames-ordered (&optional (wm *wm-application*))
  (reverse (xlib:get-property (xroot wm) :_NET_CLIENT_LIST_STACKING :transform #'xwindow-top-level-to-frame )))

(defmethod dispatch-event ((client doors-wm) event)
  (queue-event client event))

(defmethod handle-event ((client doors-wm) (event t))
  (warn "The event ~a is not processed by DOORS WM" event))

(defmethod handle-event ((client doors-wm) (event window-manager-configuration-request-event))
  (grant-configure-request event))

(defmethod handle-event ((client doors-wm) (event window-manager-map-request-event))
  (unless (port-lookup-foreign-sheet (port client) (window-manager-map-request-event-window event))
    (make-foreign-application (window-manager-map-request-event-window event) :frame-manager (find-frame-manager :port (port client)))))

(defmethod handle-event ((client doors-wm) (event climi::execute-command-event))
  (let ((command (climi::execute-command-event-command event))
        (frame (climi::execute-command-event-frame event)))
    (if (eql *application-frame* client)
        command
        (climi::event-queue-append (climi::frame-command-queue frame) command))))

(defmethod handle-event ((client doors-wm) (event window-manager-number-of-desktops-request-event))
  (let ((n (window-manager-number-of-desktops-request-event-number event)))
    (assert (and (integerp n) (> n 0) (< n 21)))
    (when (> n (length (desktops client)))
      (do () ((>= (length (desktops client)) n))
        (com-create-desktop)))
    (when (< n (length (desktops client)))
      (do () ((<= (length (desktops client)) n))
        (com-remove-desktop (a:lastcar (desktops client)))))))

(defmethod handle-event ((client doors-wm) (event window-manager-current-desktop-request-event))
  (let ((n (window-manager-current-desktop-request-event-number event)))
    (when (<= 0 n (1- (length (desktops client))))
      (com-set-current-desktop (nth n (desktops client))))))

(defmethod handle-event ((client doors-wm) (event window-manager-active-window-request-event))
  (let ((frame (window-manager-active-window-request-event-frame event)))
      (setf (active-frame (port client)) frame)))

;;; Desktops
(defmethod number-of-desktops ((frame doors-wm))
  (length (desktops frame)))

(defmethod (setf current-desktop) :around (value (frame doors-wm))
  (setf (desktop-active-p (current-desktop frame)) nil)
  (call-next-method )
  (setf (desktop-active-p (current-desktop frame)) t)
  (xlib:change-property (xroot frame) :_NET_CURRENT_DESKTOP
                          (list (position (current-desktop frame) (desktops frame)))
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
                        (unless (climi::%frame-exit-handled condition)
                          (setf (climi::%frame-exit-handled condition) t)
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
           (start-wm frame)
           (alexandria:when-let ((config-file (config-file frame)))
             (load config-file))
           (with-frame-manager (frame)
             (call-next-method)))
      (when (wm-selection-manager port)
        (stop-wm frame))
      (alexandria:removef (slot-value port 'climi::frame-managers) frame)
      (setf *wm-application* nil))))

(defmethod default-frame-top-level
    ((frame doors-wm)
     &key (command-parser 'command-line-command-parser)
          (command-unparser 'command-line-command-unparser)
          (partial-command-parser
           'command-line-read-remaining-arguments-for-partial-command)
     &allow-other-keys)
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

(defmethod execute-frame-command ((frame doors-wm) command)
  (check-type command cons)
  (if (and (or (null (climi::frame-process frame))
               (eq (climi::frame-process frame) (clim-sys:current-process)))
           (not (climi::frame-reading-command-p frame)))
      (let ((name (command-name command))
            (args (command-arguments command)))
        (restart-case (apply name args)
          (try-again ()
            :report (lambda (stream)
                      (format stream "Try executing the command ~S again." name))
            (execute-frame-command frame command))))
      (dispatch-event frame (make-instance 'climi::execute-command-event
                                           :sheet frame
                                           :frame frame
                                           :command command))))

(defmethod handle-event ((client doors-wm) (event keyboard-event))
  (when (eql (class-of event) (find-class 'key-press-event))
    (let ((command (lookup-keystroke-command-item
                    event (frame-command-table client))))
      (when (consp command)
        command))))

(defmethod frame-exit ((frame doors-wm))
  (signal 'frame-exit :frame frame))


;;;; Frame manager
(defmethod find-frame-container ((fm doors-frame-manager) (frame application-frame))
  (graft (port fm)))

(defmethod find-pane-for-frame
    ((fm doors-wm) (frame standard-application-frame))
  (let ((tls (make-pane-1 fm frame 'stack-top-level-sheet-pane
               :name (frame-name frame)
               :pretty-name (frame-pretty-name frame)
               :icon (clime:frame-icon frame)
               ;; sheet is enabled from enable-frame
               :enabled-p nil)))
    (sheet-adopt-child (find-frame-container fm frame) tls)
    tls))

(defun save-frame-geometry (frame)
  "Save the actual geometry of the frame FRAME in the slots of the FRAME"
  (unless (or (frame-properties frame :fullscreen)
              (frame-properties frame :position))
    (let ((t-l-s (frame-top-level-sheet frame)))
      (multiple-value-bind (x y) (transform-position (sheet-delta-transformation  t-l-s (sheet-parent t-l-s)) 0 0)
        (multiple-value-bind (w h) (bounding-rectangle-size (sheet-region t-l-s))
          (with-slots ((left climi::geometry-left)
                       (top climi::geometry-top)
                       (width climi::geometry-width)
                       (height climi::geometry-height)) frame
            (setf left (round x)
                  top (round y)
                  width (round w)
                  height (round h))))))))

(defun desktop-region (frame)
  (let ((panel-tls (frame-top-level-sheet (wm-panel *wm-application*))))
    (region-difference (sheet-region (graft frame))
                       (transform-region (sheet-transformation panel-tls)
                                         (sheet-region panel-tls)))))

(defgeneric move-frame (frame-manager frame position &optional toggle)
  (:documentation "Move the FRAME according to the policy of the FRAME-MANAGER on position. 
Position can be :UP :DOWN :LEFT :RIGHT :MAXIMIZED")
  (:method ((frame-manager standard-frame-manager) frame position &optional toggle)
    (declare (ignore frame-manager frame position toggle))
    t)
  (:method ((frame-manager doors-wm) frame position &optional (toggle t))
    (flet ((new-geometry (position)
             (with-bounding-rectangle* (x0 y0 x1 y1)
                 (desktop-region frame)
               (case position
                 (:maximized (values x0 y0 (- x1 x0) (- y1 y0)))
                 (:left (values x0 y0 (/ (- x1 x0) 2) (- y1 y0)))
                 (:right (values (+ x0 (/ (- x1 x0) 2)) y0 (/ (- x1 x0) 2) (- y1 y0)))
                 (:up (values x0 y0 (- x1 x0) (/ (- y1 y0) 2)))
                 (:down (values x0 (+ y0 (/ (- y1 y0) 2)) (- x1 x0) (/ (- y1 y0) 2)))))))
      (unless (frame-properties frame :fullscreen)
        (let* ((tls (frame-top-level-sheet frame)))
          (if (and toggle (eq position (frame-properties frame :position)))
              (with-slots ((left climi::geometry-left)
                           (top climi::geometry-top)
                           (width climi::geometry-width)
                           (height climi::geometry-height)) frame
                (move-and-resize-sheet tls left top width height)
                (allocate-space tls width height)
                (setf (frame-properties frame :position) nil))
              (multiple-value-bind (x y w h) (new-geometry position)
                (save-frame-geometry frame)
                (move-and-resize-sheet tls x y w h)
                (allocate-space tls w h)
                (setf (frame-properties frame :position) position))))))))

(defgeneric fullscreen-frame (frame-manager frame)
  (:documentation "Fullscreen the FRAME according to the policy of the FRAME-MANAGER")
  (:method ((frame-manager standard-frame-manager) frame)
    t)
  (:method ((frame-manager doors-wm) frame)
    (let* ((tls (frame-top-level-sheet frame))
           (graft (graft frame)))
      (if (frame-properties frame :fullscreen)
          (progn
            (add-ornaments tls)
            (setf (frame-properties frame :fullscreen) nil)
            (a:if-let ((position (frame-properties frame :position)))
              (move-frame frame-manager frame position nil)
              (with-slots ((left climi::geometry-left)
                           (top climi::geometry-top)
                           (width climi::geometry-width)
                           (height climi::geometry-height)) frame
                (move-and-resize-sheet tls left top width height)
                (allocate-space tls width height))))
          (progn
            (save-frame-geometry frame)
            (remove-ornaments tls)
            (move-and-resize-sheet tls 0 0 (graft-width graft) (graft-height graft))
            (allocate-space tls (graft-width graft) (graft-height graft))
            (setf (frame-properties frame :fullscreen) t))))))

(defmethod note-frame-enabled :after ((fm doors-wm) (frame standard-application-frame))
  (declare (ignore fm))
  (xlib:change-property (xwindow-for-properties frame) :WM_STATE (list +normal-state+) :WM_STATE 32)
  (ewmh-update-client-list))

(defmethod note-frame-disabled :after ((fm doors-wm) (frame standard-application-frame))
  (declare (ignore fm))
  (xlib:change-property (xwindow-for-properties frame) :WM_STATE (list +withdrawn-state+) :WM_STATE 32)
  (ewmh-update-client-list))

(defmethod note-frame-iconified :after ((fm doors-wm) (frame standard-application-frame))
  (declare (ignore fm))
  (xlib:change-property (xwindow-for-properties frame) :WM_STATE (list +iconic-state+) :WM_STATE 32))

(defmethod note-frame-deiconified :after ((fm doors-wm) (frame standard-application-frame))
  (declare (ignore fm))
  (xlib:change-property (xwindow-for-properties frame) :WM_STATE (list +normal-state+) :WM_STATE 32))

(defmethod raise-sheet :after ((sheet climi::top-level-sheet-pane))
  (ewmh-update-client-list-stacking))

(defmethod bury-sheet :after ((sheet climi::top-level-sheet-pane))
  (ewmh-update-client-list-stacking))

(defmethod reorder-sheets :after ((sheet doors-graft) new-ordering)
  (ewmh-update-client-list-stacking))


;;; Command table
(define-command-table doors-wm)

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


;;;; startup functions

(defun doors (&key new-process replace-wm (port (find-port :server-path '(:doors))) (config-file *config-file*))
  (let* ((doorswm (make-instance 'doors-wm
                                 :port port
                                 :replace-wm replace-wm
                                 :config-file config-file)))
    (when *wm-application* (error "Another instance of Doors WM is running: ~A" *wm-application*))
    (if new-process
        (clim-sys:make-process #'(lambda () (run-frame-top-level doorswm)) :name "Doors WM")
        (run-frame-top-level doorswm))))
