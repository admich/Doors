;;;; Doors a window manager based on McCLIM.
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

(asdf:defsystem #:doors
		:description "A X11 window manager based on McCLIM"
		:author "Andrea De Michele <andrea.demichele@gmail.com>"
		:license "LGPL-2.1+"
        :version "0.0.1"
		:depends-on (#:alexandria #:uiop
                     #:mcclim #:mcclim-doors
                     #:clim-listener #:climacs #:clim-debugger #:xembed)
		:serial t
		:components ((:file "package")
                     (:file "doors-tray")
                     (:file "desktop")
                     (:file "doors")
                     (:file "info-line")))

