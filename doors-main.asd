;;;; Doors a window manager based on McCLIM.
;;;; Copyright (C) 2022  Andrea De Michele
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

(asdf:defsystem "doors-main"
  :description "A X11 window manager based on McCLIM.
This system is to build an executable."
  :author "Andrea De Michele <andrea.demichele@gmail.com>"
  :license "LGPL-2.1+"
  :version "0.0.1"
  :depends-on ("doors" "net.didierverna.clon")
  :serial t
  :components ((:file "main"))
  :build-operation "program-op"
  :build-pathname "doors"
  :entry-point "doors-main:main")

