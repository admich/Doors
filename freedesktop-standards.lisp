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

(in-package :freedesktop-standards)

(defconstant +normal-state+ 1)
(defconstant +iconic-state+ 3)
(defconstant +withdrawn-state+ 0)

(a:define-constant +icccm-atoms+
    '(:WM_STATE
      :WM_PROTOCOLS
      :WM_TAKE_FOCUS
      :WM_DELETE_WINDOW) :test #'equal)

(a:define-constant  +ewmh-atoms+
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
    ) :test #'equal)

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

(defun icccm-input-model (window)
  "Return the input model (icccm):
Input Model      Input Field WM_TAKE_FOCUS
No Input         False       Absent       
Passive          True        Absent       
Locally Active   True        Present      
Globally Active  False       Present      
"
  (let* ((hints (xlib:wm-hints  window))
         (input-fields (or (null hints) (not (eql :off (xlib:wm-hints-input hints)))))
         (wm-take-focus (member :wm_take_focus (xlib:wm-protocols window))))
    (log:warn input-fields wm-take-focus)
    (cond
      ((and (not input-fields) (not wm-take-focus)) :no-input)
      ((and input-fields (not wm-take-focus)) :passive)
      ((and input-fields wm-take-focus) :locally-active)
      ((and (not input-fields) wm-take-focus) :globally-active))))

;; icccm 4.2.8
(defun send-client-message (window protocol time &rest data)
  (let ((dpy (xlib:drawable-display window)))
   (xlib:send-event window :client-message nil
                           :window window
                           :type :WM_PROTOCOLS
                           :format 32
                           :propagate-p nil
                           :data (cons (xlib:find-atom dpy protocol) (cons (or time 0) data)))))

