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

;;;; icccm and ewmh related stuff

(in-package :doors)

(defconstant +normal-state+ 1)
(defconstant +iconic-state+ 3)
(defconstant +withdrawn-state+ 0)

(defconstant +icccm-atoms+
  '(:WM_STATE))

(defconstant  +ewmh-atoms+
  '(;;; Root Window Properties (and Related Messages)
    :_NET_SUPPORTED
    :_NET_CLIENT_LIST
    :_NET_CLIENT_LIST_STACKING
    :_NET_NUMBER_OF_DESKTOPS
    :_NET_DESKTOP_GEOMETRY
    :_NET_DESKTOP_VIEWPORT
    :_NET_CURRENT_DESKTOP
    ;; :_NET_DESKTOP_NAMES
    :_NET_ACTIVE_WINDOW
    ;; :_NET_WORKAREA
    :_NET_SUPPORTING_WM_CHECK
    ;; :_NET_VIRTUAL_ROOTS
    ;; :_NET_DESKTOP_LAYOUT
    ;; :_NET_SHOWING_DESKTOP
    
    ;;; Other Root Window Messages
    ;; :_NET_CLOSE_WINDOW
    ;; :_NET_MOVERESIZE_WINDOW
    ;; :_NET_WM_MOVERESIZE
    ;; :_NET_RESTACK_WINDOW
    ;; :_NET_REQUEST_FRAME_EXTENTS
    
    ;;; Application Window Properties
    :_NET_WM_NAME
    ;; : _NET_WM_VISIBLE_NAME
    :_NET_WM_ICON_NAME
    ;; : _NET_WM_VISIBLE_ICON_NAME
    :_NET_WM_DESKTOP
    ;; : _NET_WM_WINDOW_TYPE
    ;; : _NET_WM_STATE
    ;; : _NET_WM_ALLOWED_ACTIONS
    ;; : _NET_WM_STRUT
    ;; : _NET_WM_STRUT_PARTIAL
    ;; : _NET_WM_ICON_GEOMETRY
    ;; : _NET_WM_ICON
    ;; : _NET_WM_PID
    ;; : _NET_WM_HANDLED_ICONS
    ;; : _NET_WM_USER_TIME
    ;; : _NET_WM_USER_TIME_WINDOW
    ;; : _NET_FRAME_EXTENTS
    ;; : _NET_WM_OPAQUE_REGION
    ;; : _NET_WM_BYPASS_COMPOSITOR
    
    ;;; Window Manager Protocols
    ;; :_NET_WM_PING
    ;; :_NET_WM_SYNC_REQUEST
    ;; :_NET_WM_FULLSCREEN_MONITORS
    
    ;;; Other Properties
    ;; :_NET_WM_FULL_PLACEMENT
    
    ;;; Compositing Managers
    ;; :_NET_WM_CM_Sn Manager Selection
    ))

(defun intern-atoms (dpy atoms)
  (mapcar #'(lambda (atom) (xlib:intern-atom dpy atom)) atoms))

(defun set-xwindow-state (xwindow state)
  (xlib:change-property xwindow
                        :WM_STATE
                        (list state)
                        :WM_STATE
                        32))

(defun get-utf8-property (window atom)
  (a:when-let ((prop (xlib:get-property window atom
                                      :result-type '(vector (unsigned-byte 8)))))
    (babel:octets-to-string prop :encoding :utf-8)))

(defun net-wm-name (window)
  (get-utf8-property window :_NET_WM_NAME))

(defun net-wm-icon-name (window)
  (get-utf8-property window :_NET_WM_ICON_NAME))

(defun all-windows-tree (&optional (root (clim-clx::window (sheet-mirror (find-graft )))))
  (let ((children (xlib:query-tree root)))
    (if (null children)
        root
        (list root (mapcar #'(lambda (x) (all-windows-tree x)) children)))))

(defun all-windows (&optional (root (clim-clx::window (sheet-mirror (find-graft )))))
  (alexandria:flatten (all-windows-tree root)))

(defun find-root ()
  (clim-clx::window (sheet-mirror (find-graft))))

(defun find-display ()
  (clim-clx:clx-port-display (find-port)))

(defun ewmh-startup ()
  (let ((root (find-root))
        (dpy (find-display)))
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
  (let* ((root (find-root))
         (dpy (find-display))
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
  (let ((root (find-root))
        (dpy (find-display)))
    (xlib:change-property root :_NET_CLIENT_LIST
                          (mapcar #'(lambda (x) (xwindow-for-properties x))
                                  (reverse (managed-frames)))
                               :window 32
                               :transform #'xlib:drawable-id
                               :mode :replace))
  (ewmh-update-client-list-stacking))

(defun ewmh-update-desktop ()
  (let ((root (find-root))
        (dpy (find-display)))
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

(defun xwindow-top-level-to-frame (window-or-id)
  "return the application frame from the xwindow top-level sheet.
   It is the reverse of xwidnow-for-properties"
  (let* ((dpy (find-display))
         (window (if (integerp window-or-id)
                     (xlib::lookup-window dpy window-or-id)
                     window-or-id))
         (sheet (or (getf (xlib:window-plist window) 'sheet) (port-lookup-foreign-sheet (port *wm-application*) window))))
    (pane-frame sheet)))

(defmethod enable-frame :around ((frame standard-application-frame))
  (call-next-method)
  (unless (frame-properties frame :wm-desktop)
    (setf (frame-properties frame :wm-desktop) (current-desktop *wm-application*)))
  (set-xwindow-state (xwindow-for-properties frame)  +normal-state+))

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
