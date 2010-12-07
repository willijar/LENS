;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; statistical logging classes

;;; Code:

(in-package :math)

(defgeneric record(value statistics &key time)
  (:documentation "Record the value and (optionally) time"))

(defgeneric write-log(statistics &key stream header separator)
  (:documentation "Write out the statistics to a stream"))

(defclass time-value()
  ((time-values :initform nil :accessor time-values))
  (:documentation "A time value statistics object"))

(defmethod record((value number) (stats time-value)
                  &key (time (simulation-time)))
  (push (cons time value) (time-values stats)))

(defmethod write-log((stats time-value) &key
                     (stream *standard-output*) header (separator ","))
  (when header (write-line header stream))
  (dolist(tv (reverse (time-values stats)))
    (format stream "~A~A~A~%" (car tv) separator (cdr tv))))

(defmethod reset((stats time-value))
  (setf (time-values stats) nil))

(defclass average-min-max()
  ((min-sample :type number :accessor min-sample)
   (max-sample :type number :accessor max-sample)
   (total :type number :initform 0 :accessor total)
   (no-samples :initform 0 :type integer :accessor no-samples))
  (:documentation "Class to dynamically track the average, minimum,
and maximum values for the measured stsatistics."))

(defmethod record((value number) (stats average-min-max)
                  &key time)
  (declare (ignore time))
  (if (zerop (no-samples stats))
      (setf (min-sample stats) value
            (max-sample stats) value)
      (setf (min-sample stats) (min value (min-sample stats))
            (max-sample stats) (max value (max-sample stats))))
  (incf (no-samples stats))
  (incf (total stats) value))

(defmethod reset((stats average-min-max))
  (setf (no-samples stats) 0
        (total stats) 0))

(defun average(stats)
  (unless (zerop (no-samples stats))
    (/ (total stats) (no-samples stats))))

(defmethod write-log((stats average-min-max)
               &key (stream *standard-output*) header (separator ", "))
  (unless (zerop (no-samples stats))
    (when header (write-line header stream))
    (format stream "Min ~A~AMax ~A~AAverage ~A~%"
            (min-sample stats)
            separator
            (max-sample stats)
            separator
            (average stats))))

(defmethod print-object((stats average-min-max) stream)
  (print-unreadable-object(stats stream :type t :identity t)
    (unless (zerop (no-samples stats))
      (format stream "~A ~A ~A"
              (min-sample stats) (average stats) (max-sample stats)))))

(defclass histogram()
  ((bins :type  (vector integer *) :reader bins)
   (min-value :initform 0 :initarg :min :type number :accessor min-value)
   (max-value :initform 0 :initarg :min :type number :accessor max-value)
   (out-of-range :initform 0 :accessor out-of-range))
  (:documentation "defines a statistics collection
that tracks samplees in fixed size buckets
and counts the number of samples falling in each
bucket interval."))

(defmethod initialize-instance :after ((stats histogram)
                                      &key (no-bins 10) &allow-other-keys)
  (setf (slot-value stats 'bins)
        (make-array no-bins :element-type 'number :initial-element 0)))

(defmethod reset((stats histogram))
  (let ((bins (bins stats)))
    (loop :for i :from 0 :below (length bins) :do (setf (aref bins i) 0)))
  (setf (out-of-range stats) 0))


(defmethod record((v number) (stats histogram) &key time)
  (declare (ignore time))
  (with-slots(min-value max-value out-of-range) stats
    (if (< min-value v max-value)
        (incf (svref (bins stats)
                     (floor
                      (* (- v min-value)
                         (/ (length (bins stats))
                            (- max-value min-value))))))
        (progn
          (incf out-of-range)
          nil))))

(defmethod write-log((stats histogram) &key (stream *standard-output*)
               header (separator ", "))
  (when header (write-line header stream))
  (loop :for c :across (bins stats)
        :with per-bin = (/ (- (max-value stats) (min-value stats))
                           (length (bins stats)))
        :for f = (min-value stats) :then (+ f per-bin)
        :do (format stream "~A~A~A~A~A~%"
                    f separator (+ f per-bin) separator c)))

(defun cdf(stats &key (stream *standard-output*)
           header (separator ", "))
  (let* ((bins (bins stats))
         (tot (+ (out-of-range stats) (reduce #'+ bins))))
    (when header (write-line header stream))
    (format stream "~A~A~A~%" (min-value stats) separator 0)
  (loop :for c :across bins
        :with per-bin = (/ (- (max-value stats) (min-value stats))
                           (length (bins stats)))
        :for f = (+ (min-value stats) per-bin) :then (+ f per-bin)
        :for cum = 0 :then (+ cum c)
        :do (format stream "~A~A~A~%"
                    f separator (/ cum tot) ))
    (format stream "Out of Range~A~A~%"
            separator (out-of-range stats))))

(defclass inter-arrival-histogram()
  ((bins :initform (make-hash-table) :type hash-table :reader bins)
   (units :initform 0.05 :initarg :units :type real :reader units))
  (:documentation "Statistics for recording histograms of interarrival times"))

(defmethod reset((stats inter-arrival-histogram))
  (clrhash (bins stats)))

(defmethod record((value real) (stats inter-arrival-histogram) &key time)
  (declare (ignore time))
  (incf (gethash (round (/ value (units stats))) (bins stats) 0)))

(defmethod write-log((stats inter-arrival-histogram)
                     &key (stream *standard-output*)
               header (separator ", "))
  (when header (write-line header stream))
  (let ((bins (sort
               (loop :for k :being :each :hash-key :of (bins stats)
                     :using (:hash-value v)
                     :collect (cons k v))
               #'<)))
  (loop :for c :across bins
        :do (format stream "~A~A~A~%"
                    (car c) separator (cdr c)))))

;;; round trip time estimation

(defstruct rtt-history
  (seq 0 :type seq)         ; First sequence number in packet sent
  (count 0 :type counter)   ; Number of bytes sent
  (time 0.0 :type time-type)  ; Time this one was sent
  (retx nil :type boolean)) ; True if this has been retransmitted

(defclass rtt-estimator()
  ((seq-next :initform 0 :type seq :accessor seq-next
             :documentation  "Next expected sequence to be sent")
   (history :initform nil :type list :accessor history
            :documentation "List of sent packets")
   (estimate :initarg :initial-estimate :initform 0.0
             :type time-type :accessor estimate
             :documentation "Current estimate")
   (no-samples :initform 0 :type integer :accessor no-samples
               :documentation "Number of samples")
   (multiplier :initform 1.0 :type real :accessor multiplier
               :documentation "RTO multiplier")
   (statistics :initarg :statistics :initform nil :accessor statistics
               :documentation "Private statistics collection object")
   (global-statistics
    :initform nil :accessor global-statistics :allocation :class
    :documentation "Global statistics collection object")
   (initial-estimate :initform 1.0 :type time-type :allocation :class
                     :documentation "Default initial estimate")
   (max-multiplier :initform 64 :type real :allocation :class
                   :documentation "Maximum value of multiplier"))
  (:documentation "A virtual base class which defines the behavior of
a round trip time estimator used by TCP."))

(defgeneric sent-seq(seq no-bytes rtt-estimator)
  (:documentation "Record in rtt-estimator that no-bytes where sent
with particular sequence number")
  (:method (seq no-bytes (rtt rtt-estimator))
    (cond
      ((= seq (seq-next rtt)) ;; next expected one log on end
       (push
        (make-rtt-history :seq seq :count no-bytes :time (simulation-time))
        (history rtt))
       (setf (seq-next rtt) (+ seq no-bytes)))
      (t ;; is a retransmit - find in list and mark as retx
       (let ((h (find-if #'(lambda(h) (and (>= seq (rtt-history-seq h))
                                           (< seq (+ (rtt-history-seq h)
                                                     (rtt-history-count h)))))
                         (history rtt))))
         (when h
           (setf (rtt-history-retx h) t)
           ;; ensure retx doesnt extend seq-next
           (when (> (+ seq no-bytes) (seq-next rtt))
             (setf (seq-next rtt) (+ seq no-bytes))
             (setf (rtt-history-count h)
                   (- (+ seq no-bytes) (rtt-history-seq h))))))))))

(defgeneric sent-clear(rtt-estimator)
  (:documentation "Clear the list of pending sequence numbers sent.")
  (:method((rtt rtt-estimator))
    (setf (seq-next rtt) 0
          (history rtt) nil)))

(defun reset-multiplier(rtt) (setf (multiplier rtt) 1.0))
(defun increase-multiplier(rtt)
  (setf (multiplier rtt)
        (min (* 2 (multiplier rtt) (slot-value rtt 'max-multiplier)))))

(defgeneric ack-seq(seq rtt-estimator)
  (:documentation "Notify the RTT estimator that a particular sequence number
    has been acknowledged.")
  (:method(a (rtt rtt-estimator))
    (let* ((m 0.0)
           (history (nreverse (history rtt)))
           (h (first history)))
      (when (and (not (rtt-history-retx h))
                 (>= a (+ (rtt-history-seq h) (rtt-history-count h))))
        (setf m (- (simulation-time) (rtt-history-time h)))
        (record m rtt)
        (reset-multiplier rtt)
        (when-bind(stats (statistics rtt)) (record m stats))
        (when-bind(stats (global-statistics rtt)) (record m stats)))
      ;;  Now delete all ack history with seq <= ack
      (while(and history (not (> (+ (rtt-history-seq (first history))
                                    (rtt-history-count (first history))) a)))
        (pop history))
      (setf (history rtt) (nreverse history))
      m)))

(defmethod reset((rtt rtt-estimator))
  (setf (history rtt) nil
        (estimate rtt) (slot-value rtt 'initial-estimate)
        (seq-next rtt) 0
        (no-samples rtt) 0)
  (reset-multiplier rtt)
  (when-bind(stats (statistics rtt)) (reset stats)))

(defclass rtt-mdev(rtt-estimator)
  ((gain :initarg :gain :initform 0.1 :type real :accessor gain
         :documentation "Filter gain")
   (variance :initform 0.0 :type real :accessor variance
             :documentation "Current Variance"))
  (:documentation "Mean-Deviation estimator, as discussed by Van Jacobson
`Congestion Avoidance and Control', SIGCOMM 88, Appendix A"))

(defmethod record(m (rtt rtt-mdev) &key time)
  (declare (ignore time))
  (if (> (no-samples rtt) 0)
      (let ((err (- m (estimate rtt))))
        (incf (estimate rtt) (* (gain rtt) err))
        (setf (variance rtt)
              (+ (variance rtt) (* (gain rtt) (- (abs err) (variance rtt))))))
      (setf (estimate rtt) m
            (variance rtt) m))
  (incf (no-samples rtt)))

(defmethod reset((rtt rtt-mdev))
  (call-next-method)
  (setf (variance rtt) 0.0))

(defun retransmit-timeout(rtt)
  ;;As suggested by Jacobson
  (if (< (variance rtt) (/ (estimate rtt) 4.0))
      (* 2 (estimate rtt) (multiplier rtt))
      (* (+ (estimate rtt) (* 4 (variance rtt))) (multiplier rtt))))




