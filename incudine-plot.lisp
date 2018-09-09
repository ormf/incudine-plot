;;; 
;;; incudine-plot.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :incudine-plot)

(dsp! play-envelope ((env incudine::envelope) timescale)
  (out (envelope env :time-scale timescale)))

(defmethod plot ((obj envelope) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t))
  "Plot input data given as an incudine envelope."
  (declare (ignore region header options grid))
    (with-gnuplot-instance (out args)
      (let* ((env-dur (envelope-duration obj))
             (env-buffer (make-buffer 100)))
        (incudine::bounce-to-buffer (env-buffer :frames 100 :sample-rate 100)
          (play-envelope obj (/ env-dur)))
        (loop for idx below 100
           do (format out "~a ~a~%"
                      (/ (* env-dur idx) 100)
                      (float (buffer-value env-buffer idx) 1.0)))))
    (values obj))
#|
   Example:

   (plot (make-envelope '(0 1 1 0) '(1 0.5 1) :curve '(:cubic :lin :cubic)))

|#

(defmethod plot ((buffer buffer) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t) (x-axis :samples) (num-values 1000))
  "Plot input data given as an incudine buffer. :x-axis can be
:samples (default) or :seconds"
  (declare (ignore header options grid))
  (let* ((region
          (if region
              (list (max 0 (first region))
                    (min (second region) (1- (buffer-frames buffer))))
              `(0 ,(buffer-frames buffer))))
         (buffer-samplerate (slot-value buffer 'incudine::sample-rate)))
    (setf (getf args :region)
          (case x-axis
            (:samples region)
            (:seconds (mapcar (lambda (x) (/ x incudine::*sample-rate*)) region))))
    (with-gnuplot-instance (out args)
      (let* ((start (first region))
             (dur (- (second region) start)))
        (loop for plot-idx below num-values
           do (let ((idx (round (+ start (* (/ plot-idx (1- num-values)) dur)))))
                (case x-axis
                  (:samples
                   (format out "~,4f ~,4f~%"
                           idx
                           (float (buffer-value buffer idx) 1.0)))
                  (:seconds
                   (format out "~,4f ~,4f~%"
                           (/ idx buffer-samplerate)
                           (float (buffer-value buffer idx) 1.0)))))))))
  (values buffer))

#|
   Example:

   (defparameter *my-buffer* (buffer-load "/tmp/test.wav"))

   (plot *my-buffer* :x-axis :seconds)

|#
