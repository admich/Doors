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

;;;; Some patch that change normal McCLIM behaviour.

(in-package :climi)

;;; find-frame-manager with options
(defun find-frame-manager (&rest options &key port &allow-other-keys)
  (declare (special *frame-manager*))
  (climi::with-keywords-removed (options (:port))
    (if (and (boundp '*frame-manager*)
             (or (null port)
                 (and (eql port (port *frame-manager*))
                      (apply #'port-frame-manager-conforms-to-options-p port *frame-manager* options))))
        *frame-manager*
        (if (and *default-frame-manager*
                 (frame-manager-p *default-frame-manager*)
                 (or (null port)
                     (and
                      (eql port (port *default-frame-manager*))
                      (apply #'port-frame-manager-conforms-to-options-p port *default-frame-manager* options))))
            *default-frame-manager*
            (progn
              (unless port (setf port (apply #'find-port options)))
              (loop for frame-manager in (frame-managers port)
                 when (apply #'port-frame-manager-conforms-to-options-p port frame-manager options)
                   do (return frame-manager)
                 finally
                   (first (frame-managers port))))))))

(defgeneric port-frame-manager-conforms-to-options-p (port frame-manager &rest options)
  (:documentation "Check if FRAME-MANAGER conforms to OPTIONS")
  (:method (port frame-manager &rest options)
    (declare (ignore options))
    (eql port (port frame-manager))))

;;; compared to mcclim when the port is the WM we don't need to do nothing here
(defmethod handle-event ((sheet top-level-sheet-pane)
                         (event window-configuration-event))
  (when (and (null (clim-doors::wm-selection-manager (port sheet)))
             (eql (sheet-parent sheet) (graft sheet)))
    (let ((x (window-configuration-event-x event))
          (y (window-configuration-event-y event))
          (width (window-configuration-event-width event))
          (height (window-configuration-event-height event)))
      (let ((*configuration-event-p* sheet))
      (%set-sheet-region-and-transformation
       sheet
       (make-bounding-rectangle 0 0 width height)
       ;; negative offsets are handled by the native transformation?
       (make-translation-transformation x y))))))


;;; top-level-sheet-pane: allow top-level-sheet-pane with multiple child
(defclass top-level-sheet-pane (top-level-sheet-mixin)
  ()
  (:documentation "For the first pane in the architecture"))

(defclass standard-top-level-sheet-pane (top-level-sheet-pane
                                         single-child-composite-pane)
  ())

(defclass unmanaged-top-level-sheet-pane (unmanaged-sheet-mixin standard-top-level-sheet-pane)
  ()
  (:documentation "Top-level sheet without window manager intervention"))

(defmethod allocate-space ((pane standard-top-level-sheet-pane) width height)
  (unless (pane-space-requirement pane)
    (setf (pane-space-requirement pane)
          (compose-space pane)))
  (when-let ((child (sheet-child pane)))
    (allocate-space child
                    (clamp width  (sr-min-width pane)  (sr-max-width pane))
                    (clamp height (sr-min-height pane) (sr-max-height pane)))))

(defmethod allocate-space ((pane top-level-sheet-pane) width height)
 (call-next-method))

(defmethod find-pane-for-frame
    ((fm standard-frame-manager) (frame standard-application-frame))
  (make-pane-1 fm frame 'standard-top-level-sheet-pane
               :name (frame-name frame)
               :pretty-name (frame-pretty-name frame)
               :icon (frame-icon frame)
               ;; sheet is enabled from enable-frame
               :enabled-p nil))

;; this is a bug in mcclim I ned to make a PR. note-frame-enabled must
;; be called as last things
(defmethod enable-frame ((frame application-frame))
  (let ((old-value (slot-value frame 'state)))
    (setf (slot-value frame 'state) :enabled)
    (ecase old-value
      (:disabled
       (note-frame-enabled (frame-manager frame) frame))
      (:shrunk
       (note-frame-deiconified (frame-manager frame) frame))
      (:enabled))))

;; this is a bug in mcclim I ned to make a PR. Not all sheet have
;; medium so before medium-finish-output is necessary to check
(defun invoke-tracking-pointer (state)
  (let* ((tracked-sheet (tracked-sheet state))
         (pointer (tracked-pointer state))
         (multiple-window (multiple-window state))
         (transformp (transformp state))
         (modifier-state))
    (labels ((track-pointer-event (event)
               (multiple-value-call #'track-event state event
                 (let ((x (pointer-event-x event))
                       (y (pointer-event-y event)))
                   (if (not transformp)
                       (values x y)
                       (with-sheet-medium (medium (event-sheet event))
                         (transform-position (medium-transformation medium) x y))))))
             (do-it ()
               (with-pointer-grabbed ((port tracked-sheet) tracked-sheet
                                      :pointer pointer :multiple-window multiple-window)
                 ;; Synthesize a pointer motion event for the current pointer
                 ;; position so that appropriate handlers are called even if no
                 ;; event immediately follows the INVOKE-TRACKING-POINTER call.
                 ;; This ensures, for example, that feedback and/or pointer
                 ;; documentation are initialized right away in the context of
                 ;; presentation drag and drop.
                 ;;
                 ;; However, to prevent things like drag and drop feedback being
                 ;; drawn to the wrong sheet, discard the synthesized event if
                 ;; its sheet is not a tracked sheet. This can happen if
                 ;; MULTIPLE-WINDOW is false, INVOKE-TRACKING-POINTER is invoked
                 ;; via, say, a keyboard gesture or programmatically and the
                 ;; pointer is not over TRACKED-SHEET.
                 (let ((event (synthesize-pointer-motion-event pointer)))
                   (setf modifier-state (event-modifier-state event))
                   (when (or multiple-window
                             (eql tracked-sheet (event-sheet event)))
                     (track-pointer-event event)))
                 (loop for event = (event-read tracked-sheet)
                       for sheet = (event-sheet event)
                       ;; We let HANDLE-EVENT take care of events that are not
                       ;; for TRACKED-SHEET (unless MULTIPLE-WINDOW is true). On
                       ;; the other hand, we pass events for TRACKED-SHEET (or
                       ;; all events if MULTIPLE-WINDOW is true) to TRACK-EVENT.
                       do (cond ((not (or multiple-window
                                          (eql tracked-sheet sheet)))
                                 ;; Event is not intercepted.
                                 (handle-event sheet event))
                                ((typep event 'pointer-event)
                                 (track-pointer-event event))
                                (t
                                 (track-event state event nil nil)))
                          ;; As a special exception, whenever a device event changes
                          ;; the modifier state, we synthesize an event, so that
                          ;; mouse-only and non-MULTIPLE-WINDOW handling can still
                          ;; react to changed keyboard modifiers.
                       when (typep event 'device-event)
                         do (let ((new-state (event-modifier-state event)))
                              (when (not (eql modifier-state new-state))
                                (track-pointer-event
                                 (synthesize-pointer-motion-event pointer)))
                              (setf modifier-state new-state))
                      when (typep sheet 'sheet-with-medium-mixin)      
                       do (medium-finish-output sheet)))))
      (if (keyboard-handler state)
          (with-input-focus (tracked-sheet)
            (do-it))
          (do-it)))))

;; a frame must be disabled before the disown in this way note-frame-disabled is called.
(defmethod disown-frame :before
    ((fm headless-frame-manager) (frame application-frame))
  (alexandria:removef (slot-value fm 'frames) frame)
  (disable-frame frame))
;;; some keysym
(in-package :clim-xcommon)
(define-keysym :XF86-Audio-Lower-Volume #x1008FF11)
(define-keysym :XF86-Audio-Mute #x1008FF12)
(define-keysym :XF86-Audio-Raise-Volume #x1008FF13)
