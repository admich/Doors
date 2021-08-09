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

(defpackage #:clim-doors
  (:use :clim :clim-lisp :clim-backend :clim-xcommon :clim-clx)
  (:import-from :alexandria
                #:when-let*
                #:when-let)
  (:import-from :climi
                #:port-grafts
                #:top-level-sheet-pane
                #:top-level-sheet
                #:menu-frame
                #:window-destroy-event
                #:frame-managers
                #:maybe-funcall
                #:pointer-grab-enter-event
                #:pointer-grab-leave-event
                #:pointer-ungrab-enter-event
                #:pointer-ungrab-leave-event)
  (:export
   #:*wm-application*
   #:active-frame
   #:window-manager-request-event
   #:window-manager-configuration-request-event
   #:window-manager-configuration-request-event-window
   #:window-manager-configuration-request-event-native-x
   #:window-manager-configuration-request-event-native-y
   #:window-manager-configuration-request-event-width
   #:window-manager-configuration-request-event-height
   #:window-manager-map-request-event
   #:window-manager-map-request-event-window
   #:grant-configure-request
   #:port-lookup-foreign-sheet
   #:foreign-application
   #:foreign-xwindow
   #:make-foreign-application
   #:foreign-application-unmanage-xwindow
   #:wm-selection-manager
   #:*grabbed-keystrokes*
   #:grab/ungrab-keystroke
   #:update-server-timestamp
   #:doors-frame-manager)
  (:local-nicknames (#:a #:alexandria)))

(defpackage #:doors
  (:use #:clim-lisp #:clim #:clim-doors)
  (:export #:doors)
  (:local-nicknames (#:a #:alexandria)))

