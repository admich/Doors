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

(in-package :clim-doors)

(defclass doors-graft (clim-clx::clx-graft
                       ; clim:standard-sheet-input-mixin
					   ; permanent-medium-sheet-output-mixin
                       )
  ())

;; (defmethod initialize-instance :after ((graft delorean-graft) &rest args)
;;   (setf (climi::%sheet-medium graft) (make-medium (port graft) graft)))
(defmethod handle-event ((pane doors-graft) event)
  (log:warn "When sheet is unknown the event is sent to the graft and it do nothing" event)
  nil)
