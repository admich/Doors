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
                #:when-let*)
  (:import-from :climi
                #:+alt-key+
                ;;
                #:port-text-style-mappings
                #:port-lookup-mirror
                #:port-register-mirror
                #:port-event-process
                #:port-grafts
                #:%%sheet-native-transformation
                #:%%set-sheet-native-transformation
                #:device-transformation
                ;;
                #:clamp
                #:get-environment-variable
                #:pixmap-sheet
                #:port-lookup-sheet
                #:port-unregister-mirror
                #:port-pointer-sheet
                #:map-repeated-sequence
                #:pixmap-mirror
                #:do-sequence
                #:with-double-buffering
                #:with-transformed-position
                #:with-transformed-positions
                #:with-medium-options
                ;;
                #:pixmap
                #:top-level-sheet-mixin
                #:unmanaged-sheet-mixin
                #:top-level-sheet-pane
                #:unmanaged-top-level-sheet-pane
                #:menu-frame
                ;;
                #:frame-managers        ;used as slot
                #:top-level-sheet       ;used as slot
                #:medium-device-region
                #:draw-image
                #:height                ;this seems bogus
                #:width                 ;dito
                #:coordinate=
                #:get-transformation
                ;;
                #:medium-miter-limit
                ;; classes
                #:mirrored-pixmap
                #:window-destroy-event
                #:pointer-grab-enter-event
                #:pointer-grab-leave-event
                #:pointer-ungrab-leave-event
                #:pointer-ungrab-enter-event
                #:device-font-text-style
                ;; utils
                #:dolines
                #:maybe-funcall
                #:when-let
                #:if-let)
  (:import-from #:climi
                #:event-listen-or-wait
                #:%sheet-mirror-region
                #:%sheet-mirror-transformation
                #:standard-port)  
  (:export
   #:*wm-application*
   #:active-frame
   #:save-frame-geometry))

(defpackage #:doors
  (:use #:clim-lisp #:clim #:clim-doors)
  (:export #:doors))

