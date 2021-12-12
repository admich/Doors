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

(in-package :doors)

(defparameter *wm-selection* "WM_S0")

(defun check-for-existing-window (wm)
  (let ((windows (xlib:query-tree (xroot wm))))
    (loop for win in windows
       unless (or (eq (xlib:window-override-redirect win) :on)
                  (eq (xlib:window-map-state win) :unmapped)) do
                    (make-foreign-application win :frame-manager wm))))

(defun ewmh-startup (port)
  (let ((root (clim-clx:clx-port-window port))
        (dpy (clim-clx:clx-port-display port)))
    (xlib:change-property root :_NET_SUPPORTED
                          (mapcar #'(lambda (x) (xlib:find-atom dpy x)) +ewmh-atoms+)
                               :atom 32)
    (let ((supporting-window (xlib:create-window
                      :parent root
                      :x 0 :y 1 :width 1 :height 1)))
      (setf (xlib:wm-name supporting-window) "doors")
      (xlib:change-property supporting-window :_NET_WM_NAME
                          "doors" :string 32
                          :transform #'xlib:char->card8)
      (xlib:change-property root :_NET_SUPPORTING_WM_CHECK
                            (list supporting-window) :window 32
                            :transform #'xlib:drawable-id)
      (xlib:change-property supporting-window :_NET_SUPPORTING_WM_CHECK
                          (list supporting-window) :window 32
                                      :transform #'xlib:drawable-id)
      (setf (net-supporting-wm-check *wm-application*) supporting-window))))

(defun ewmh-stop ()
  (let ((supporting-window (net-supporting-wm-check *wm-application*)))
    (setf (net-supporting-wm-check *wm-application*) nil)
    (xlib:destroy-window supporting-window)))

(defun ewmh-update-client-list-stacking ()
  (let* ((root (xroot *wm-application*))
         (wins (loop for win in (xlib:query-tree root)
                     for sheet = (getf (xlib:window-plist win) 'sheet)
                     for frame = (and sheet (pane-frame sheet))
                     when (member frame (managed-frames))
                       collect (xwindow-for-properties frame))))
    (xlib:change-property root :_NET_CLIENT_LIST_STACKING wins
                               :window 32
                               :transform #'xlib:drawable-id
                               :mode :replace)))

(defun ewmh-update-client-list ()
  (let ((root (xroot *wm-application*)))
    (xlib:change-property root :_NET_CLIENT_LIST
                          (mapcar #'(lambda (x) (xwindow-for-properties x))
                                  (reverse (managed-frames)))
                               :window 32
                               :transform #'xlib:drawable-id
                               :mode :replace))
  (ewmh-update-client-list-stacking))

(defun ewmh-update-desktop ()
  (let ((root (xroot *wm-application*)))
    (xlib:change-property root :_NET_NUMBER_OF_DESKTOPS
                          (list (length (desktops *wm-application*)))
                               :cardinal 32)
    (xlib:change-property root :_NET_DESKTOP_GEOMETRY
                          (list (xlib:drawable-width root) (xlib:drawable-height root))
                               :cardinal 32)
    (xlib:change-property root :_NET_DESKTOP_VIEWPORT
                          (list 0 0)
                               :cardinal 32)
    (xlib:change-property root :_NET_CURRENT_DESKTOP
                          (list (position (current-desktop *wm-application*) (desktops *wm-application*)))
                          :cardinal 32)))

(defgeneric xwindow-for-properties (frame)
  (:documentation "The x window where set the properties")
  (:method ((frame standard-application-frame))
    (clim-clx::window (sheet-mirror (frame-top-level-sheet frame))))
  (:method ((frame foreign-application))
    (clim-doors:foreign-xwindow frame)))

(defun xwindow-top-level-to-frame (window-or-id &optional (port (find-port)))
  "return the application frame from the xwindow top-level sheet.
   It is the reverse of xwidnow-for-properties"
  (let* ((dpy (clim-clx::clx-port-display port))
         (window (if (integerp window-or-id)
                     (xlib::lookup-window dpy window-or-id)
                     window-or-id))
         (sheet (or (getf (xlib:window-plist window) 'sheet) (port-lookup-foreign-sheet (port *wm-application*) window))))
    (pane-frame sheet)))

(defmethod enable-frame :around ((frame standard-application-frame))
  (call-next-method)
  (unless (frame-properties frame :wm-desktop)
    (setf (frame-properties frame :wm-desktop) (current-desktop *wm-application*)))
  (set-xwindow-state (xwindow-for-properties frame)  +normal-state+)
  (setf (active-frame (port frame)) frame))

(defmethod disable-frame :around ((frame standard-application-frame))
  (xlib:delete-property (xwindow-for-properties frame) :WM_STATE)
  (call-next-method))

(defmethod shrink-frame :around ((frame standard-application-frame))
  (set-xwindow-state (xwindow-for-properties frame) +iconic-state+)
  (call-next-method))


(defmethod frame-pretty-name ((frame foreign-application))
  (let ((window (foreign-xwindow frame)))
    (or (ignore-errors (net-wm-name window))
        (ignore-errors (xlib:wm-name window))
        "NoWin")))

(defmethod frame-short-name ((frame foreign-application))
  (let ((window (foreign-xwindow frame)))
    (or (ignore-errors (net-wm-icon-name window))
        (ignore-errors (xlib:wm-icon-name window))
        (frame-pretty-name frame))))

(defun start-wm (wm)
  "Initialize the icccm and ewmh protocols"
  (let ((replace (doors-wm-replace-wm wm))
        (port (port wm))
        (dpy (xdisplay wm))
        (root (xroot wm))
        timestamp)
    (intern-atoms dpy +icccm-atoms+)
    (intern-atoms dpy +ewmh-atoms+)
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
      (ewmh-startup port)
      (check-for-existing-window wm)
      (loop for key in *grabbed-keystrokes* do
        (grab/ungrab-keystroke key :port port))
      (ewmh-update-desktop))))

(defun stop-wm (wm)
  "Stop xwm"
  (let ((port (port wm)))
    (loop for key in *grabbed-keystrokes* do
      (grab/ungrab-keystroke key :port port :ungrab t))
    ;; check this maybe frame-exit doesn't work
    (maphash (lambda (mirror sheet)
               (declare (ignore mirror))
               (clim-doors:foreign-application-unmanage-xwindow (pane-frame sheet))
               (frame-exit (pane-frame sheet)))
             (slot-value port 'foreign-mirror->sheet))
    (xlib:destroy-window (wm-selection-manager port))
    (setf (wm-selection-manager port) nil)
    (ewmh-stop)
    (setf (xlib:window-event-mask (xroot wm)) (xlib:make-event-mask))))

(dotimes (i 10)
  (let ((wm-sel (alexandria:make-keyword (format nil "WM_S~d" i))))
    (defmethod release-selection (wm (selection (eql wm-sel)) object)
      (stop-wm wm))))

