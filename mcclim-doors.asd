;;;; Doors a window manager based on McCLIM.
;;;; Copyright (C) 2021  Andrea De Michele
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

(defsystem #:mcclim-doors
  :depends-on (#:mcclim-clx
               #:log4cl)
  :components((:file "package")
              (:file "freedesktop-standards")
              (:file "patch")
              (:file "graft" :depends-on ("package"))
              (:file "port" :depends-on ("package" "graft"))
              (:file "emergency")
              (:file "input" :depends-on ("port"))
              (:file "frame-manager" :depends-on ( "port" ))
              (:file "foreign-application" :depends-on ( "frame-manager"))))

