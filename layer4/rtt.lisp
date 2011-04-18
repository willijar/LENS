;; Round Trip estimation for TCP
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:


;; See Tanenbaum "Computer Networks" (TCN) section 6.5.
;;; Code:

;;; round trip time estimation

(in-package :protocol.layer4)

(defstruct rtt-history
  (seq 0 :type (unsigned-byte 32))       ; First sequence number in packet sent
  (count 0 :type (unsigned-byte 16))     ; Number of bytes sent
  (time 0.0 :type time-type)  ; Time this one was sent
  (retx nil :type boolean))   ; True if this has been retransmitted

(defclass rtt-estimator()
  ((next :initform 0 :type (unsigned-byte 32)
         :documentation  "Next expected sequence to be sent")
   (history :initform nil :type list
            :documentation "History list of sent packets")
   (estimate :initarg :initial-estimate :type time-type :initform 1.0
             :documentation "Current estimate of RTT")
   (no-samples :initform 0 :type integer :accessor no-samples
               :documentation "Number of samples")
   (multiplier :initform 1.0 :type real :accessor multiplier
               :documentation "RTO multiplier")
   (statistics :initarg :statistics :initform nil
               :documentation "List of statistics collection objects")
   (initial-estimate :initform 1.0 :type time-type :allocation :class
                     :documentation "Default initial estimate")
   (max-multiplier :initform 64.0 :type real :allocation :class
                   :documentation "Maximum value of multiplier"))
  (:documentation "A virtual base class which defines the behavior of
a round trip time estimator used by TCP."))

(defun seq-in-segment(sequence-number segment-start segment-no-bytes)
  "Return true if the sequence number corresponds to a byte in segment
with the given sequence start number and byte count."
  (declare ((unsigned-byte 32) sequence-number segment-start segment-no-bytes)
           (optimize speed (safety 0)))
  (let ((segment-end (modulus+ segment-start segment-no-bytes 32)))
    (declare ((unsigned-byte 32) segment-end))
    ;; deal with wrap around edge case
    (if (>= segment-end segment-start)
         (and (>= sequence-number segment-start)
              (< sequence-number segment-end))
         (or (< sequence-number segment-end)
             (>= sequence-number segment-start)))))

(defun ack-after-segment(ack-number segment-start segment-no-bytes)
  "Return true if an acknowledgement number is after a sequence with a
given segment-start and no-bytes"
  (declare ((unsigned-byte 32) ack-number segment-start segment-no-bytes)
           (optimize speed (safety 0)))
  (let ((segment-end (modulus+ segment-start segment-no-bytes 32))
        (wrap-around (modulus+ segment-start #x7FFFFFFF 32)))
    (declare ((unsigned-byte 32) segment-end wrap-around))
    ;; deal with edge cases from modular arithmetic wrap around
    (if (> wrap-around segment-end)
        (and (>= ack-number segment-end)
             (< ack-number wrap-around))
        (or (>= ack-number segment-end)
            (< ack-number wrap-around)))))

(defun rtt-sent-seq(seq no-bytes rtt)
  "Note that a particular sequence has been sent"
  (with-slots(next history) rtt
    (cond
      ((= seq next) ;; next expected one log on end
       (push
        (make-rtt-history :seq seq :count no-bytes :time (simulation-time))
        history)
       (setf next (modulus+ seq no-bytes 32)))
      (t ;; is a retransmit - find in list and mark as retx
       (let ((h
              (find-if
               #'(lambda(h)
                   (seq-in-segment
                    seq (rtt-history-seq h) (rtt-history-count h)))
               history)))
         (when h
           (setf (rtt-history-retx h) t)
           ;; ensure retx doesnt extend next
           (when (> (+ seq no-bytes) next)
             (setf next (modulus+ seq no-bytes 32))
             (setf (rtt-history-count h)
                   (- (+ seq no-bytes) (rtt-history-seq h))))))))))

(defun rtt-ack-seq(a rtt)
  "Notify the RTT estimator that a particular sequence number has been
acknowledged."
  (let ((m 0.0))
    (with-slots(history statistics) rtt
      (let* ((h (first (last history))))
        (when h
          (when (and (not (rtt-history-retx h))
                     (seq-in-segment
                      a (rtt-history-seq h) (rtt-history-count h)))
            (setf m (- (simulation-time) (rtt-history-time h)))
            (math:record m rtt)
            (reset-multiplier rtt)
            (dolist(s statistics)  (math:record m s)))
          ;;  Now delete all ack history with seq<= ack
          (setf history
                (mapcar
                 #'(lambda(h)
                     (unless (ack-after-segment a (rtt-history-seq h)
                                                (rtt-history-count h))
                       (list h)))
                 history)))))
    m))

(defun rtt-sent-clear(rtt)
  (with-slots(next history) rtt
    (setf next 0
          history nil)))

(defun reset-multiplier(rtt) (setf (slot-value rtt 'multiplier ) 1.0))

(defun rtt-increase-multiplier(rtt)
  (with-slots(multiplier max-multiplier) rtt
    (setf multiplier (min (* 2 multiplier max-multiplier)))))

(defmethod reset((rtt rtt-estimator))
  (with-slots(next history estimate initial-estimate no-samples) rtt
    (setf history nil
          estimate initial-estimate
          next 0
          no-samples 0)
    (reset-multiplier rtt)))

(defclass rtt-mdev(rtt-estimator)
  ((gain :initarg :gain :initform 0.1 :type real :accessor gain
         :documentation "Filter gain")
   (variance :initform 0.0 :type real :accessor variance
             :documentation "Current Variance"))
  (:documentation "Mean-Deviation estimator, as discussed by Van Jacobson
`Congestion Avoidance and Control', SIGCOMM 88, Appendix A"))

(defmethod math:record(m (rtt rtt-mdev) &key time)
  (declare (ignore time))
  (with-slots(no-samples estimate gain variance) rtt
    (if (> no-samples 0)
        (let ((err (- m estimate)))
          (incf estimate (* gain err))
          (incf variance (* gain (- (abs err) variance ))))
        (setf estimate m
              variance  m))
    (incf no-samples)))

(defmethod reset((rtt rtt-mdev))
  (call-next-method)
  (setf (slot-value rtt 'variance) 0.0))

(defun rtt-retransmit-timeout(rtt)
  ;;As suggested by Jacobson
  (with-slots(estimate variance multiplier) rtt
    (if (< variance  (/ estimate 4.0))
        (* 2 estimate multiplier)
        (* (+ estimate (* 4 variance)) multiplier))))

(export '(rtt-estimator rtt-mdev rtt-ack-seq rtt-retransmit-timeout rtt-increase-multiplier))