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

(in-package :doors)

(defparameter *batteries* nil)

(defclass battery ()
  ((name :initarg :name :reader battery-name :initform "---")
   (path :initarg :path :reader battery-path)))

(defclass battery-set (battery)
  ((set :initarg :set :accessor battery-set :initform nil))
  (:default-initargs :name "ALL"))

(define-presentation-type battery () :options ((what-present :name)) :inherit-from 'standard-object)

(define-presentation-method present (object (type battery) stream view &key)
  (case what-present
    (:name (format stream "~a" (battery-name object)))
    (:long (format stream "~a: ~a, ~d%"
                   (battery-name object)
                   (battery-status object)
                   (round (battery-% object)))
           (unless (string= "UNKNOWN" (battery-status object))
             (format stream " ~{~2,'0d~^:~} remaining" (battery-remaining-time object))))
    (t (let* ((percent (battery-% object))
         (ink (cond
                ((< percent 20) +red+)
                ((< 20 percent 50) +orange+)
                (t +foreground-ink+))))
         (write-string "[" stream)
         (with-drawing-options (stream :ink ink)
           (format stream "~d%~:[-~;+~]" (round percent) (battery-charging-p object)))
         (write-string "] " stream)))))

(defun read-sysfs (bat file)
  (read-from-string
   (string-trim '(#\Newline)
                (alexandria:read-file-into-string (merge-pathnames file (battery-path bat))))))

(defun init-batteries ()
  (setf *batteries*
        (make-instance 'battery-set
                       :set
                       (loop for path in (directory "/sys/class/power_supply/*")
                          when (string= "Battery" (string-trim '(#\Newline) (alexandria:read-file-into-string (merge-pathnames "type" path))))
                          collect (make-instance 'battery :path path
                                                 :name (alexandria:lastcar (pathname-directory path)))))))

(init-batteries)

(defmethod battery-energy-full ((bat battery))
  (read-sysfs bat "energy_full"))

(defmethod battery-energy-full ((bat battery-set))
  (reduce #'+ (battery-set bat) :key #'battery-energy-full))

(defmethod battery-energy-now ((bat battery))
  (read-sysfs bat "energy_now"))

(defmethod battery-energy-now ((bat battery-set))
  (reduce #'+ (battery-set bat) :key #'battery-energy-now))

(defmethod battery-power-now ((bat battery))
  (read-sysfs bat "power_now"))

(defmethod battery-power-now ((bat battery-set))
  (reduce #'+ (battery-set bat) :key #'battery-power-now))

(defmethod battery-% (bat)
  (* 100 (/ (battery-energy-now bat) (battery-energy-full bat))))

(defmethod battery-charging-p ((bat battery))
  (string= "CHARGING" (symbol-name (read-sysfs bat "status"))))

(defmethod battery-charging-p ((bat battery-set))
  (loop for x in (battery-set bat) thereis (battery-charging-p x)))

(defmethod battery-status ((bat battery))
  (symbol-name (read-sysfs bat "status")))

(defmethod battery-status ((bat battery-set))
  (if (battery-charging-p bat)
      "CHARGING"
      "DISCHARGING"))

(defmethod battery-remaining-time ((bat battery))
  (when (not (zerop (battery-power-now bat)))
    (let ((time (if (battery-charging-p bat)
                    (/ (- (battery-energy-full bat) (battery-energy-now bat)) (battery-power-now bat))
                    (/ (battery-energy-now bat) (battery-power-now bat)))))
      (multiple-value-bind (h rh) (floor time)
        (multiple-value-bind (min rmin) (floor (* 60 rh))
          (list h min (round (* 60 rmin))))))))

(define-presentation-action battery-menu
    (battery nil doors
             :documentation "View batteries"
             :pointer-documentation ((object stream) (present object '((battery) :what-present :long) :stream stream))
             :menu nil
             :gesture :select
             :tester ((object)
                      (typep object 'battery-set)))
    (object)
  (menu-choose (map 'list (lambda (x)
                            (list (present-to-string x '((battery) :what-present :long)) :value t))
                    (battery-set object))
               :scroll-bars nil
               :y-spacing 10
               :label "Batteries"))

;;;; Wireless
(defun wireless ()
  (let* ((iwoutput (uiop:run-program "iwconfig" :output :string))
         (regex-scan (nth-value 1 (cl-ppcre:scan-to-strings
                                   (cl-ppcre:create-scanner
                                    "ESSID:\"(.*)\".*Link Quality=([0-9]*/[0-9]*)"
                                    :single-line-mode t)
                                   iwoutput)))
         (name (if regex-scan
                   (aref regex-scan 0)
                   "No Link"))
         (quality (* 100
                     (if regex-scan
                      (read-from-string
                       (aref regex-scan 1))
                      0))))
    (format nil "(~a: ~d%)" name (round quality))))

(defun display-info (frame pane)
  (format pane " " )
  (multiple-value-bind (sec min h d m y) (decode-universal-time (get-universal-time))
    (format pane "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d " y m d h min sec))
  (present *batteries* '((battery) :what-present :short))
  (format pane "~a" (wireless))
  (loop for frame in (managed-frames)
     when (typep frame 'application-frame)
     do
       (if (eql frame (active-frame (port *wm-application*)))
             (with-text-face (pane :bold)
               (present frame 'application-frame))
             (present frame 'application-frame))))
