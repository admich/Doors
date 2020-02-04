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

;;; This tray is inspired by stumptray (https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/stumptray)
;;; and by examples in clx-xembed (https://github.com/laynor/clx-xembed)

(defpackage :doors-systray
  (:use  #:xembed #:clim #:clim-lisp #:clim-doors)
  (:export #:tray-pane
           #:start-tray
           #:tray-event-process
           #:tray-win
           #:tray-sowin
           #:tray-fpwin
           #:tray-icon-height
           #:tray-icons
           #:tray-display
           #:tray-screen))

(in-package :doors-systray)
;;; Event masks
;; main window
(defparameter +win-event-mask+
  (xlib:make-event-mask :property-change :key-press :key-release))
;; Focus proxy window
(defparameter +fpwin-event-mask+
  (xlib:make-event-mask :property-change :key-press :key-release))
;; Selection owner window
(defparameter +sowin-event-mask+
  (xlib:make-event-mask))

(defparameter *tray* nil)

(defclass tray-pane (mirrored-sheet-mixin ; must be the first
				     fundamental-character-input-stream
				     basic-gadget)
  ((win :initarg :win :initform nil :accessor tray-win :documentation "Main window")
   (sowin :initarg :sowin :accessor tray-sowin :documentation "Selection Owner Window (required by the tray protocol)")
   (fpwin :initarg :fpwin :accessor tray-fpwin :documentation "Focus proxy window (required by the xembed protocol)")
   (icon-height :initform 16 :initarg :icon-height :accessor tray-icon-height :documentation "Icon height")
   (icons :initarg :vicons :initform nil :accessor tray-icons :documentation "Icon list")
   (event-process :initarg :event-process :initform nil :accessor tray-event-process :documentation "tray events process loop")
   (display :initarg :display :accessor tray-display)
   (screen :initarg :screen :accessor tray-screen)))

(defmethod compose-space ((pane tray-pane) &key width height)
  (make-space-requirement :width  (or (and (tray-win pane)
                                           (xlib:drawable-width (tray-win pane))) 0) 
                          :height (or (and (tray-win pane)
                                           (xlib:drawable-height (tray-win pane)))
                                      (tray-icon-height pane))))

(defun tray-loop (tray)
  (when *tray* (kill-tray *tray*))
  (setf *tray* tray
        (tray-event-process tray) (clim-sys:current-process))
  (create-tray-wins tray)
  (tray-init tray)
  (map-tray tray)
  (let ((dpy (tray-display tray))
        (hnd (make-tray-handler tray)))
    (unwind-protect 
         (do ()
             (nil)
           (xlib:display-finish-output dpy)
           (xlib:process-event dpy :discard-p t :handler hnd))
      (cleanup-tray tray)
      (setf (tray-event-process tray) nil
            *tray* nil))))

(defun cleanup-tray (tray)
  "Cleanup the tray"
  (dolist (socket (tray-icons tray))
    (ignore-errors (xembed:destroy-socket socket)))
  (xlib:destroy-window (tray-win tray))
  (setf (tray-win tray) nil))

(defun unmap-tray (pane)
  (alexandria:when-let ((win (tray-win pane)))
    (xlib:unmap-window win)
    (xlib:display-force-output (xlib:drawable-display win))
    (xlib:reparent-window win (xlib:drawable-root win) 0 0)
    (xlib:display-force-output (xlib:drawable-display win))))

(defmethod note-sheet-degrafted :before ((pane tray-pane))
  (unmap-tray pane))

(defun create-tray-wins (tray)
  (let* ((clim-dpy (clim-clx::clx-port-display (port *wm-application*)))
         (dpy (xlib:open-display (xlib:display-host clim-dpy)
                                 :display (xlib:display-display clim-dpy)))
         (xscreen (xlib:display-default-screen dpy))
         (root (xlib:screen-root xscreen))
         (depth (xlib:drawable-depth root))
         (width (tray-icon-height tray))
         (height width))
    (with-slots (win sowin fpwin icon-height display screen) tray
      (setf win (xlib:create-window :parent root 
                                    :x 0
                                    :y 0
                                    :depth depth
                                    :width width 
                                    :height height
                                    :background (xlib:alloc-color (xlib:screen-default-colormap xscreen)
                                                                  (multiple-value-bind (r g b)
                                                                      (color-rgb (pane-background tray))
                                                                      (xlib:make-color :red r :green g :blue b)))
                                      :event-mask +WIN-EVENT-MASK+)
            sowin (xlib:create-window :parent win
                                      :x -1 :y -1 :width 1 :height 1
                                        :event-mask +sowin-event-mask+)
            fpwin (xlib:create-window :parent win
                                      :x -1 :y -1 :width 1 :height 1
                                      :event-mask +fpwin-event-mask+)
            icon-height width
            display dpy
            screen xscreen))))

(defun map-tray (pane)
  (alexandria:when-let ((window (tray-win pane)))
    (let* ((parent-window (sheet-mirror pane)))
      (xlib:reparent-window window parent-window 0 0)
      (xlib:display-force-output (clim-clx::clx-port-display (port (pane-frame pane))))
      (xlib:map-window window)
      (xlib:display-force-output (xlib:drawable-display window)))))

(defmethod note-sheet-grafted :after ((pane tray-pane))
  (map-tray pane))

(defun start-tray (tray)
  (clim-sys:make-process (lambda () (tray-loop tray)) :name "Tray-loop"))

(defun kill-tray (tray)
  (if (tray-event-process tray)
      (clim-sys:destroy-process (tray-event-process tray))
      (cleanup-tray tray)))

;;;; FDO Tray requirements
(defun fdo-tray-selection-name (tray)
  "Returns the selection atom name for TRAY as specified by the FDO
System Tray protocol."
  (let* ((screen (tray-screen tray))
         (display (tray-display tray)))
    (intern 
     (format nil "_NET_SYSTEM_TRAY_S~a" (xlib::screen-position screen display))
     'keyword)))

(defun fdo-tray-init-properties (tray)
  "Sets the selection owner window property as specified by the FDO
System tray protocol."
  (xlib:change-property (tray-sowin tray)
                        :_NET_SYSTEM_TRAY_ORIENTATION #(0)  ; 0 horizontal 1 vertical 
                        :_NET_SYSTEM_TRAY_ORIENTATION 32))

(defun fdo-tray-set-selection-owner (tray)
  "Sets the selection owner of the manager selection as specified by
the FDO System Tray protocol."
  (setf (xlib:selection-owner (tray-display tray)
			      (fdo-tray-selection-name tray))
	(tray-sowin tray)))

(defun fdo-tray-send-manager-notification (tray)
  (let ((root-window (xlib:drawable-root (tray-win tray)))
        (atom-id (xlib:intern-atom (tray-display tray) (fdo-tray-selection-name tray))))
    (xlib:send-event root-window :client-message (xlib:make-event-mask :structure-notify)
		     :window root-window
		     :type :MANAGER
		     :format 32
		     :data (vector xembed:*timestamp* atom-id (xlib:window-id (tray-sowin tray)) 0 0)
		     :propagate-p nil)))

(defun fdo-tray-init (tray)
  "Initializes the TRAY windows as needed by the FDO System Tray
protocol."
  (fdo-tray-init-properties tray)
  (fdo-tray-set-selection-owner tray)
  (fdo-tray-send-manager-notification tray))

(defparameter +FDO-TRAY-OPCODES-ALIST+
  '((:SYSTEM-TRAY-REQUEST-DOCK . 0)
    (:SYSTEM-TRAY-BEGIN-MESSAGE . 1)
    (:SYSTEM-TRAY-CANCEL-MESSAGE . 2)))

(defun fdo-tray-encode-opcode (type)
  (cdr (assoc type +FDO-TRAY-OPCODES-ALIST+)))

(defun fdo-tray-decode-opcode (type)
  (car (rassoc type +FDO-TRAY-OPCODES-ALIST+)))

;;;; Xembed requirements
(defun xembed-tray-init (tray)
  "Initializes the TRAY windows as needed by the XEMBED protocol."
  (pushnew :WM_TAKE_FOCUS (xlib:wm-protocols (tray-win tray)))
  (pushnew :WM_TAKE_FOCUS (xlib:wm-protocols (tray-fpwin tray))))

;;; Icon tiling
;;; This is necessary as some icons may want to resize themselves, and
;;; sometimes need to be repositioned These functions actually work on
;;; sockets

(defun tray-tile-icons (tray)
  "Repositions the icons embedded in TRAY one next to the other."
  (let ((x 0))
    (dolist (icon (tray-icons tray))
      (setf (xlib:drawable-x icon) x)
      (incf x (xlib:drawable-width icon)))))

;;; Tray geometry updating
(defun tray-height (tray)
  "Returns the TRAY height."
  (tray-icon-height tray))

(defun tray-width (tray)
  "Calculates the total width of the TRAY."
  (max (tray-icon-height tray)
       (reduce #'+ (tray-icons tray) :key #'xlib:drawable-width)))

(defun tray-update-geometry (tray)
  "Updates the TRAY's geometry (main windows and subwindows)."
  (let ((win (tray-win tray)))
    (setf (xlib:drawable-width win)
          (tray-width tray))
    (setf (xlib:drawable-height win)
          (tray-height tray))))

(defun tray-update (tray &optional (map-p t))
  "Sorts the icons embedded in TRAY, tiles them and updates the
geometry of its windows. Maps the tray windows - or unmaps them, based
on the TRAY state - if MAP-P is T."
  (tray-tile-icons tray)
  (tray-update-geometry tray)
  (change-space-requirements tray))

(defun tray-init (tray)
  "Initializes the TRAY object."
  (fdo-tray-init tray)
  (xembed-tray-init tray)
  (tray-update tray nil))

;;; Xembed sockets
(defun make-icon-socket (tray parent)
  "Creates and returns an xembed socket"
  (let ((root (xlib:drawable-root parent))
        (icon-height (tray-icon-height tray)))
    (xembed:create-socket nil :parent parent :depth (xlib:drawable-depth root)
			  :background :PARENT-RELATIVE
			  :x 0 :y 0
			  :width icon-height :height icon-height)))

(defun initialize-icon-socket (icon-socket)
  "Maps and activates the socket."
  (xlib:map-window icon-socket)
  (xlib:map-subwindows icon-socket)
  (xembed:socket-activate icon-socket))

;;; Icon scaling to tray size
(defun scale-icon-width (tray-icon-height-i width height)
  "Scales the icon keeping its aspect ratio so that its height is TRAY-ICON-HEIGHT-I."
  (let ((aspect-ratio (if (or (zerop height) (zerop width)) 
			   1 ; some icons are initially mapped with zero width or height, assume square
			   (/ width height))))
    (ceiling (* tray-icon-height-i (max 1 aspect-ratio))))) ;; assume 1 as minimum aspect ratio

;;; Adding icons
(defun add-icon (tray icon-id)
  "Adds an icon with window id ICON-ID to TRAY, and starts the XEMBED
protocol."
  (let* ((icon (xlib::lookup-window (tray-display tray) icon-id))
         (socket (make-icon-socket tray (tray-win tray))))
    (xembed:embed socket icon t 0 0)
    (initialize-icon-socket socket)
    (push socket (tray-icons tray))))

;;; Icon removal
(defun remove-icon (tray socket)
  "Removes the icon embedded in SOCKET from TRAY."
  (setf (tray-icons tray)
	(remove socket (tray-icons tray) :test #'xlib:window-equal))
  (xembed:destroy-socket socket)
  (tray-update tray))

;;;; Event handlers

;; Implements the fdo systemtray specification
(defun fdo-tray-make-event-handler (tray)
  (xembed:handler-vector
   ((:client-message) (window type data)
    (when (eq type :_NET_SYSTEM_TRAY_OPCODE) ;FIXME check destination window
      (destructuring-bind (timestamp message data1 data2 data3)
	  (coerce data 'list)
	(declare (ignorable data2 data3))
	(xembed:update-timestamp (tray-fpwin tray) timestamp)
	(let ((opcode (fdo-tray-decode-opcode message)))
	  (xembed:dformat 0 "TRAY-MESSAGE[~S](~S)~%" window opcode)
	  (case opcode
	    (:SYSTEM-TRAY-REQUEST-DOCK 
	     (add-icon tray data1)
	     (tray-update tray))
	    (:SYSTEM-TRAY-BEGIN-MESSAGE t)
	    (:SYSTEM-TRAY-CANCEL-MESSAGE t))))))))

;;; Xembed event handler
(defun make-tray-xembed-event-handler (tray)
  (let ((hnd (xembed:socket-list-handler-vector
              (lambda () (tray-icons tray)))))
    (xembed:combine-handlers
     (xembed:handler-vector
      ((:client-message) (type data)
       (when (and (eq type :_XEMBED))
         (let ((opcode (xembed:decode-xembed-message-type (elt data 1))))
           (case opcode
             (:xembed-protocol-finished
              (let ((socket (xlib::lookup-window (tray-display tray)
                                                 (elt data 3))))
                (remove-icon tray socket)))))))
      ((:configure-notify) (event-window window width height)
       (let ((iheight (tray-icon-height tray)))
         (when (and (member event-window (tray-icons tray)
						    :test #'xlib:window-equal)
                    (xlib:window-equal (xembed:client event-window) window))
           (xembed:dformat 2 "CONFIGURE ~S~%" (list width height))
           (xembed:socket-resize event-window
                                 (scale-icon-width iheight width height)
                                 iheight)
           (tray-update tray)))
       t))
     hnd)))

(defun make-tray-handler (tray)
  "Builds and returns an event handler vector for TRAY, that can be
passed to `xlib:process-event'."
  (reduce #'xembed:combine-handlers
          (list (make-tray-xembed-event-handler tray)
                (fdo-tray-make-event-handler tray))))

