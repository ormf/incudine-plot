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
  (declare (ignore header options grid))
    (let* ((gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command :region region args))
           :input :stream)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (let* ((env-dur (envelope-duration obj))
             (env-buffer (make-buffer 100)))
        (incudine::bounce-to-buffer (env-buffer :frames 100 :sample-rate 100)
          (play-envelope obj (/ env-dur)))
        (loop for idx below 100
           do (format out "~a ~a~%"
                      (/ (* env-dur idx) 100)
                      (float (buffer-value env-buffer idx) 1.0)))))))
#|
   Example:

   (plot (make-envelope '(0 1 1 0) '(1 2 1)))

|#

(defmethod plot ((buffer buffer) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t) (x-axis :samples))
  "Plot input data given as an incudine buffer. :x-axis can be
:samples (default) or :seconds"
  (declare (ignore header options grid))
  (let* ((region
          (if region
              (list (max 0 (first region))
                    (min (1- (buffer-frames buffer)) (second region)))
              `(0 ,(buffer-frames buffer))))
         (gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command
                        :region
                        (case x-axis
                          (:samples region)
                          (:seconds (mapcar
                                     (lambda (x) (/ x incudine::*sample-rate*))
                                     region)))
                        args))
           :input :stream)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (loop for idx from (first region) to (second region)
         do (case x-axis
              (:samples
               (format out "~,4f ~,4f~%"
                       idx
                       (float (buffer-value buffer idx) 1.0)))
              (:seconds
               (format out "~,4f ~,4f~%"
                       (/ idx incudine::*sample-rate*)
                       (float (buffer-value buffer idx) 1.0))))))))

#|
   Example:

   (defparameter *my-buffer* (buffer-load "/tmp/test.wav"))

   (plot *my-buffer* :x-axis :seconds)

|#
