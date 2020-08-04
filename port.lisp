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

(in-package :clim-doors)

(defparameter *wm-selection* "WM_S0")

(defparameter *grabbed-keystrokes* nil)

(defclass doors-pointer (clim-clx::clx-pointer)
  ())

(defclass doors-port (mcclim-truetype::clx-ttf-port)
  ((foreign-mirror->sheet :initform (make-hash-table :test #'equalp))
   (focus :initform nil :accessor %port-keyboard-input-focus)
   (active-frame :initform nil :accessor active-frame)
   (wm-selection-manager :accessor wm-selection-manager
                         :initform nil)
   (aux-xwindow :accessor port-aux-xwindow
                         :initform nil)
   (server-timestamp :accessor x-server-timestamp
                     :initform 0)))

(defun update-server-timestamp (port)
  (when-let ((window (port-aux-xwindow port)))
    (xlib:change-property window :wm_name '(0) :string 8)
    (xlib:display-force-output (clx-port-display port))
    (sleep 1)
    (x-server-timestamp port)))

(defmethod (setf active-frame) :after (frame (port doors-port))
  (when (member (frame-state frame) '(:disabled :shrunk))
    (enable-frame frame))
  (when (eql (frame-state frame) :enabled)
      (raise-frame frame)
      (when (frame-standard-input frame)
        (stream-set-input-focus (frame-standard-input frame)))))

(defmethod port-lookup-sheet :around ((port doors-port) mirror)
  (if (xlib:window-equal mirror (sheet-mirror (graft port)))
      (graft port)
      (call-next-method)))

(defgeneric port-lookup-foreign-sheet (port mirror))
(defgeneric port-register-foreign-application (port sheet mirror))
(defgeneric port-unregister-foreign-application (port sheet mirror))

(defmethod port-lookup-foreign-sheet ((port doors-port) mirror)
  (gethash (xlib:window-id mirror) (slot-value port 'foreign-mirror->sheet)))

(defmethod port-register-foreign-application
    ((port doors-port) sheet mirror)
  (setf (gethash (xlib:window-id mirror) (slot-value port 'foreign-mirror->sheet)) sheet)
  nil)

(defmethod port-unregister-foreign-application
    ((port doors-port) sheet mirror)
  (remhash (xlib:window-id mirror) (slot-value port 'foreign-mirror->sheet))
  nil)

(setf (get :doors :port-type) 'doors-port)
(setf (get :doors :server-path-parser) 'clim-clx::parse-clx-server-path)

(defun check-for-existing-window (port)
  (let ((windows (xlib:query-tree (clx-port-window port))))
    (loop for win in windows
       unless (or (xlib:window-equal win (sheet-mirror (frame-top-level-sheet *wm-application*)))
                  (eq (xlib:window-override-redirect win) :on)
                  (eq (xlib:window-map-state win) :unmapped)) do
         (make-foreign-application win :frame-manager (find-frame-manager :port port)))))
;; ;;; The parameter STATE is a bit mask represented as the logical OR
;; ;;; of individual bits.  Each bit corresponds to a modifier or a
;; ;;; pointer button that is active immediately before the key was
;; ;;; pressed or released.  The bits have the following meaning:
;; ;;;
;; ;;;   position  value    meaning
;; ;;;     0         1      shift
;; ;;;     1         2      lock
;; ;;;     2         4      control
;; ;;;     3         8      mod1
;; ;;;     4        16      mod2
;; ;;;     5        32      mod3
;; ;;;     6        64      mod4
;; ;;;     7       128      mod5
;; ;;;     8       256      button1
;; ;;;     9       512      button2
;; ;;;    10      1024      button3
;; ;;;    11      2048      button4
;; ;;;    12      4096      button5

(defun keystroke-to-keycode-and-state (keystroke port)
  "Return x11 keycode and state from a keystroke"
  (let* ((mod-values '((:shift . 1) (:control . 4) (:meta . 8) (:super . 64)))
         (display (clim-clx::clx-port-display port))
         (key (car keystroke))
         (modifiers (cdr keystroke))
         (keysym (if (characterp key)
                     (first (xlib:character->keysyms key display))
                     (clim-xcommon:keysym-name-to-keysym key)))
         (keycode (xlib:keysym->keycodes display keysym))
         (shift? (cond
                   ((= keysym (xlib:keycode->keysym display keycode 0))
                    nil)
                   ((= keysym (xlib:keycode->keysym display keycode 1))
                    t)
                   (t (error "Error in find the keycode of char ~S" char))))
         state)
    (when shift? (pushnew :shift modifiers))
    ;; maybe is better to use logior
    (setf state (loop for i in modifiers sum (alexandria:assoc-value mod-values i)))
    (values keycode state)))

(defun grab/ungrab-keystroke (keystroke &key (port (find-port)) (ungrab nil))
  (let* ((display (clim-clx::clx-port-display port))
         (root (clim-clx::clx-port-window port)))
    (multiple-value-bind (code state) (keystroke-to-keycode-and-state keystroke port)
      (if ungrab
          (xlib:ungrab-key root code :modifiers state)
          (xlib:grab-key root code :modifiers state)))
    (xlib:display-finish-output display)))

(defun start-wm (port &optional replace)
  "Initialize the icccm and ewmh protocols"
  (let ((dpy (clx-port-display port))
        (root (clx-port-window port))
        timestamp)
    (xlib:intern-atom dpy *wm-selection*)
    (let ((old-wm (xlib:selection-owner dpy *wm-selection*))
          (wm-sn-manager (xlib:create-window :parent root
                                             :override-redirect :on
                                             :width 1 :height 1
                                             :x -10 :y -10
                                             :event-mask '(:property-change))))
      (setf timestamp (update-server-timestamp port))

      (if (and old-wm (not replace))
          (progn (xlib:destroy-window wm-sn-manager)
                 (error "Another WM is running~%"))
          (setf (xlib:selection-owner dpy *wm-selection* timestamp) wm-sn-manager))

      (when old-wm
        (unless
            (dotimes (i 5)
              (handler-case (xlib:drawable-x old-wm)
                (xlib:drawable-error ()
                  (return t)))
              (sleep 1))
          (error "The old WM doesn't release the selection ownership.~%")))

      (unless (and (xlib:selection-owner dpy *wm-selection*)
                   (xlib:window-equal wm-sn-manager (xlib:selection-owner dpy *wm-selection*)))
        (xlib:destroy-window wm-sn-manager)
        (error "Failed to acquire WM selection ownership~%"))

      (handler-case
          (progn
            (setf (xlib:window-event-mask root)
                  '(:substructure-notify :substructure-redirect :focus-change))
            (xlib:display-finish-output dpy))
        (error ()
          (xlib:destroy-window wm-sn-manager)
          (error "A non ICCCM WM this running~%")))

      (setf (wm-selection-manager port) wm-sn-manager)

      (xlib:send-event root :client-message '(:structure-notify)
                       :window root
                       :type :MANAGER
                       :format 32
                       :data (list timestamp
                                   (xlib:find-atom dpy *wm-selection*)
                                   (xlib:window-id wm-sn-manager)))
      (check-for-existing-window port)
      (loop for key in *grabbed-keystrokes* do
                (grab/ungrab-keystroke key :port port))
      (when *wm-application*
        (let ((graft (graft *wm-application*)))
          (move-sheet (frame-top-level-sheet *wm-application*) 0 0)
          (layout-frame *wm-application* (bounding-rectangle-width graft) (bounding-rectangle-height graft)))))))

(defun stop-wm (port)
  "Stop xwm"
  (loop for key in *grabbed-keystrokes* do
           (grab/ungrab-keystroke key :port port :ungrab t))
  (maphash (lambda (mirror sheet)
             (declare (ignore mirror))
             (foreign-application-unmanage-xwindow (pane-frame sheet))
             (frame-exit (pane-frame sheet)))
           (slot-value port 'foreign-mirror->sheet))
  (xlib:destroy-window (wm-selection-manager port))
  (setf (wm-selection-manager port) nil)
  (setf (xlib:window-event-mask (clx-port-window port)) (xlib:make-event-mask)))

(defmethod initialize-instance :after ((port doors-port) &rest args)
  (declare (ignore args))
  (let ((options (cdr (port-server-path port))))
    (pop (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-onroot-frame-manager
		 :port port options)
          (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-tile-frame-manager
		 :port port options)
          (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-fullscreen-frame-manager
		 :port port options)
	  (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-stack-frame-manager
		 :port port options)
          (slot-value port 'frame-managers))
    (push (apply #'make-instance 'doors-desktop-frame-manager
		 :port port options)
	  (slot-value port 'frame-managers))
    (setf (slot-value port 'pointer)
          (make-instance 'doors-pointer :port port))
    (setf (port-aux-xwindow port) (xlib:create-window :parent (clx-port-window port)
                                             :override-redirect :on
                                             :width 1 :height 1
                                             :x -10 :y -10
                                             :event-mask '(:property-change)))))

(defmethod make-graft ((port doors-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'doors-graft
		 :port port :mirror (clim-clx::clx-port-window port)
		 :orientation orientation :units units))
        (width (xlib:screen-width (clim-clx::clx-port-screen port)))
        (height (xlib:screen-height (clim-clx::clx-port-screen port))))
    (climi::%%set-sheet-region (make-bounding-rectangle 0 0 width height)
                               graft)
    (push graft (climi::port-grafts port))
    graft))

(defmethod port-keyboard-input-focus ((port doors-port))
  (%port-keyboard-input-focus port))

(defmethod (setf port-keyboard-input-focus) (focus (port doors-port))
  (let ((old-focus (port-keyboard-input-focus port))
        (mirror (sheet-mirror focus)))
    (setf  (%port-keyboard-input-focus port) focus)
    (when (xlib:window-p mirror)
      (xlib:set-input-focus  (clim-clx::clx-port-display port) mirror :parent))
    old-focus))

(defmethod port-set-mirror-transformation :after ((port doors-port) mirror mirror-transformation)
  (xlib:display-force-output (clim-clx::clx-port-display port)))

;;;; Mirrors
(defmethod bury-mirror ((port doors-port) (sheet basic-sheet))
  (let ((mirror (clim-clx::sheet-xmirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      ;; (xlib:circulate-window-down mirror)
      (setf (xlib:window-priority mirror) :below)
      (xlib:display-force-output (clim-clx::clx-port-display port)))))

(defmethod raise-mirror ((port doors-port) (sheet basic-sheet))
  (let ((mirror (clim-clx::sheet-xmirror sheet)))
    (when (and mirror
	       (typep mirror 'xlib:window))
      ;; (xlib:circulate-window-down mirror)
      (setf (xlib:window-priority mirror) :above)
      (xlib:display-force-output (clim-clx::clx-port-display port)))))

