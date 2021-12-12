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

(in-package :clim-doors)

(defparameter *wm-application* '())
(defparameter *grabbed-keystrokes* nil)

(defclass doors-pointer (clim-clx::clx-pointer)
  ())

(defclass doors-port (clim-clx::clx-ttf-port)
  ((foreign-mirror->sheet :initform (make-hash-table :test #'equalp))
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

(defmethod (setf port-keyboard-input-focus) :after
    (sheet (port doors-port))
  (when-let* ((mirror (sheet-mirror sheet))
              (xwin (clim-clx::window mirror)))
    (xlib:set-input-focus (clx-port-display port) xwin :parent)))

(defgeneric port-lookup-foreign-sheet (port window))
(defgeneric port-register-foreign-application (port sheet window))
(defgeneric port-unregister-foreign-application (port sheet window))

(defmethod port-lookup-foreign-sheet ((port doors-port) window)
  (gethash (xlib:window-id window) (slot-value port 'foreign-mirror->sheet)))

(defmethod port-register-foreign-application
    ((port doors-port) sheet window)
  (setf (gethash (xlib:window-id window) (slot-value port 'foreign-mirror->sheet)) sheet)
  nil)

(defmethod port-unregister-foreign-application
    ((port doors-port) sheet window)
  (remhash (xlib:window-id window) (slot-value port 'foreign-mirror->sheet))
  nil)

(setf (get :doors :port-type) 'doors-port)
(setf (get :doors :server-path-parser) 'clim-clx::parse-clx-server-path)

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
                   (t (error "Error in find the keycode of ~S" key))))
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

(defmethod initialize-instance :after ((port doors-port) &rest args)
  (declare (ignore args))
  ;; remove the clx-frame-manager
  (pop (slot-value port 'frame-managers))
  (setf (slot-value port 'pointer)
        (make-instance 'doors-pointer :port port))
  (setf (port-aux-xwindow port) (xlib:create-window :parent (clx-port-window port)
                                                    :override-redirect :on
                                                    :width 1 :height 1
                                                    :x -10 :y -10
                                                    :event-mask '(:property-change))))

(defmethod make-graft ((port doors-port) &key (orientation :default) (units :device))
  (declare (ignore orientation units))
  (change-class (call-next-method) 'doors-graft))

(defmethod port-set-mirror-transformation :after ((port doors-port) mirror mirror-transformation)
  (xlib:display-force-output (clim-clx::clx-port-display port)))

(defmethod (setf active-frame) :around (frame (port doors-port))
  (let ((old-active (active-frame port)))
    (call-next-method)
    (let ((tls (frame-top-level-sheet frame)))
      (repaint-sheet tls +everywhere+))
    (when old-active
      (repaint-sheet (frame-top-level-sheet old-active) +everywhere+))))


(defmethod (setf active-frame) :after (frame (port doors-port))
  (when (member (frame-state frame) '(:disabled :shrunk))
    (enable-frame frame))
  (when (eql (frame-state frame) :enabled)
    (unless (eql (frame-properties frame :wm-desktop) :all-desktops)
      (setf (doors::current-desktop *wm-application*) (frame-properties frame :wm-desktop)))
    (raise-frame frame)
    (when (frame-standard-input frame)
      (stream-set-input-focus (frame-standard-input frame)))
    (xlib:change-property (clim-clx::clx-port-window port) :_NET_ACTIVE_WINDOW
                          (list (if frame
                                    (xlib:window-id (doors::xwindow-for-properties frame))
                                    0))
                          :WINDOW 32)))


