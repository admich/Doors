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
(in-package :doors)
;;;; application-frame
(defgeneric frame-short-name (frame)
  (:method ((frame standard-application-frame))
    (frame-pretty-name frame)))

(define-presentation-type application-frame ())

(define-presentation-method present (object (type application-frame) stream view &key)
  (declare (ignore view))
  (format stream " ~a " (frame-short-name object)))

;;;; program-name
(let (programs)
  (defun programs-in-path (&optional update)
    "Return a list of all programs in PATH env variable"
    (if (and programs (null update))
        programs
        (setf programs
              (loop for dir  in (ppcre:split ":" (uiop:getenv "PATH"))
               appending (map 'list #'pathname-name (uiop:directory-files (uiop:ensure-directory-pathname dir))))))))

(define-presentation-type-abbreviation program-name () `(member-sequence ,(programs-in-path)))

;;; new completion ui with doors-view: the possibilities are always
;;; visible and are navigable with the keyboard
(defclass doors-view (textual-view) ())
(defparameter +doors-view+ (make-instance 'doors-view))

(define-presentation-method accept ((type completion)
                                            stream
                                            (view doors-view)
                                            &key)
  (let* ((parameters climi::parameters)
         (options climi::options)
         (type (apply #'make-presentation-type-specifier
                     `(completion ,@parameters)
                     options)))
    (accept-using-completion-1
     type stream (lambda (input-string mode)
                   (complete-from-possibilities
                    input-string sequence partial-completers
                    :action mode :name-key name-key :value-key value-key))
     :partial-completers climi::partial-completers)))

(defun accept-using-completion-1 (type stream func
                                &rest complete-with-input-key-args)
  "A wrapper around complete-with-input that returns the presentation type with
  the completed object."
  (multiple-value-bind (object success input)
      (apply #'complete-input-1 stream func complete-with-input-key-args)
    (if success
        (values object type)
        (simple-parse-error "Error parsing ~S for presentation type ~S"
                            input
                            type))))

(define-gesture-name :throw-default  :keyboard (#\Return :control))

(defun read-completion-gesture-1 (stream
                                    partial-completers
                                    help-displays-possibilities)
    (flet ((possibilitiesp (gesture)
             (or (climi::gesture-match gesture *possibilities-gestures*)
                 (and help-displays-possibilities
                      (climi::gesture-match gesture *help-gestures*)))))
      (let ((climi::*completion-possibilities-continuation*
              #'(lambda ()
                  (return-from read-completion-gesture-1
                    (values nil :possibilities)))))
        (handler-bind ((accelerator-gesture
                         #'(lambda (c)
                             (let ((gesture (accelerator-gesture-event c)))
                               (cond
                                 ((possibilitiesp gesture)
                                  (return-from read-completion-gesture-1
                                    (values nil :possibilities)))
                                 ((climi::gesture-match gesture '(:next))
                                  (return-from read-completion-gesture-1 :next))
                                 ((climi::gesture-match gesture '(:prev))
                                  (return-from read-completion-gesture-1 :prev))
                                 ((climi::gesture-match gesture '(:throw-default))
                                  (return-from read-completion-gesture-1 :throw-default)))))))
          (let ((gesture (read-gesture :stream stream)))
            (values gesture
                    (cond ((possibilitiesp gesture)
                           :possibilities)
                          ((climi::gesture-match gesture partial-completers)
                           :complete-limited)
                          ((climi::gesture-match gesture *completion-gestures*)
                           :complete-maximal)
                          ((climi::complete-gesture-p gesture)
                           :complete)
                          (t nil))))))))

(defun possibility-printer (possibility ptype stream)
  "A default function for printing a possibility. Suitable for
used as value of `:possibility-printer' in calls to
`complete-input'"
  (with-output-as-presentation (stream (second possibility) ptype)
    (write-string (first possibility) stream)))

(defun complete-input-1 (stream func &key
                                       partial-completers allow-any-input
                                       (possibility-printer #'possibility-printer)
                                       (help-displays-possibilities t)
                         &aux x y rec presentations ntot n default)
  (multiple-value-setq (x y) (stream-cursor-position stream))
  (let* ((so-far (make-array 1 :element-type 'character :adjustable t :fill-pointer 0))
         (*accelerator-gestures* (append *help-gestures*
                                         '(:next :prev (#\Return :control))
                                         *possibilities-gestures*
                                         *accelerator-gestures*))
         (orig-stream (encapsulating-stream-stream stream)))
    (unwind-protect
         (labels ((print-possibilities (so-far)
                    (when default
                      (highlight-output-record default orig-stream :unhighlight))
                    (when rec
                      (erase-output-record rec stream))
                    (setf presentations nil default nil)
                    (let* ((possibilities
                             (multiple-value-bind (input success object nmatches possibilities) (funcall func so-far :possibilities)
                               (declare (ignore input success object nmatches))
                               possibilities))
                           (record (with-new-output-record (orig-stream)
                                     (fresh-line)
                                     (surrounding-output-with-border (orig-stream)
                                       (format-items possibilities
                                                     :stream orig-stream
                                                     :printer
                                                     (lambda (item stream)
                                                       (let ((presentation (funcall possibility-printer
                                                                                    item
                                                                                    (input-context-type (first *input-context*))
                                                                                    stream)))
                                                         (when (presentationp presentation) (push presentation presentations))))))
                                     (fresh-line))))
                      (setf (stream-cursor-position orig-stream) (values x y)
                            presentations (nreverse presentations)
                            rec record
                            ntot (length presentations)
                            n 0
                            default (nth n presentations))
                      (replay record orig-stream)
                      (when default
                        (highlight-output-record default orig-stream :highlight))))
                  (next ()
                    (when presentations 
                      (when default
                        (highlight-output-record default orig-stream :unhighlight))
                      (setf n (mod (1+ n) ntot)
                            default (nth n presentations))
                      (when default
                        (highlight-output-record default orig-stream :highlight))))
                  (prev ()
                    (when presentations
                      (when default
                        (highlight-output-record default orig-stream :unhighlight))
                      (setf n (mod (1- n) ntot)
                              default (nth n presentations))
                        (when default
                          (highlight-output-record default orig-stream :highlight))))
                  (ret ()
                         (when presentations
                           (when default
                             (throw-highlighted-presentation
                              default *input-context*
                              (multiple-value-bind (x y) (output-record-position default)
                                (multiple-value-setq (x y)
                                  (transform-position (sheet-native-transformation orig-stream) x y))
                                (make-instance 'pointer-button-press-event
                                               :sheet orig-stream
                                               :x x :y y
                                               :modifier-state 0
                                               :button +pointer-left-button+))))))
                  (insert-input (input)
                                (adjust-array so-far (length input)
                                              :fill-pointer (length input))
                                (replace so-far input)
                                ;; XXX: Relies on non-specified behavior of :rescan.
                                (replace-input stream input :rescan nil)))
           (multiple-value-bind (object success input)
               (climi::complete-input-rescan stream func partial-completers
                                             so-far allow-any-input)
             (when success
               (return-from complete-input-1 (values object success input))))
           (loop
             (print-possibilities so-far)
             (climi::with-input-position (stream)
               (loop
                 (multiple-value-bind (gesture mode)
                     (read-completion-gesture-1 stream
                                                partial-completers
                                                help-displays-possibilities)
                   (cond
                     (mode
                      (multiple-value-bind
                            (input success object nmatches possibilities)
                          (funcall func (subseq so-far 0) mode)
                        (when (and (zerop nmatches)
                                   (eq mode :complete-limited)
                                   (climi::complete-gesture-p gesture))
                          ;; Gesture is both a partial completer and a
                          ;; delimiter e.g., #\space.  If no partial match,
                          ;; try again with a total match.
                          (setf (values input success object nmatches possibilities)
                                (funcall func (subseq so-far 0) :complete))
                          (setf mode :complete))
                        ;; Preserve the delimiter
                        (when (and success (eq mode :complete))
                          (unread-gesture gesture :stream stream))
                        ;; Get completion from menu
                        (when climi::*trace-complete-input*
                          (format *trace-output* "nmatches = ~A, mode = ~A~%"
                                  nmatches mode))
                        (unless (and (eq mode :complete) (not success))
                          (if (> nmatches 0)
                              (insert-input input)
                              (beep)))
                        (cond ((and success (eq mode :complete))
                               (return-from complete-input-1
                                 (values object success input)))
                              ((activation-gesture-p gesture)
                               (if allow-any-input
                                   (return-from complete-input-1
                                     (values nil t (subseq so-far 0)))
                                   (error 'simple-completion-error
                                          :format-control "Input ~S does not match"
                                          :format-arguments (list so-far)
                                          :input-so-far so-far))))))
                     ((null gesture) ; e.g. end-of-input if STREAM is a string stream
                      (return-from complete-input-1 (values nil nil so-far)))
                     ((eq gesture :next) (next))
                     ((eq gesture :prev) (prev))
                     ((eq gesture :throw-default) (ret))
                     (t
                      (vector-push-extend gesture so-far)
                      (print-possibilities so-far))))))))
      (when rec (erase-output-record rec stream nil)))))

