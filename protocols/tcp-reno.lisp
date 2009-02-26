;; TCP Reno implementation
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :protocol.tcp)

(defclass tcp-reno(tcp)
  ((fast-recovery-mode :initform nil :type boolean
                       :accessor fast-recovery-mode))
  (:documentation "Implementation of the Reno variation of TCP"))

(defmethod newack(seq (tcp tcp-reno))
  ;; New acknowledgement up to sequence number seq
  (let ((seg-size (seg-size tcp)))
    (cond
      ((fast-recovery-mode tcp)
       ;; If in fast recovery and have a new data ack, reset cWnd
       ;; to  the ssthresh calculated when we entered fast recovery
       ;; and exit fast recovery mode
       (setf (cwnd tcp) (ssthresh tcp)
             (fast-recovery-mode tcp) nil))
      ((< (cwnd tcp) (ssthresh tcp))
       ;; slow start mode add one seg-size to cwnd
       (incf (cwnd tcp) seg-size))
      (t ;; Congestion avoidance mode, adjust by (segsize*segize) / cWnd
       (setf (cwnd tcp) (max 1.0 (/ (* seg-size seg-size) (cwnd tcp))))))
    (note-time-seq log-cwin (cwnd tcp) tcp)
    (common-newack seq tcp)))

(defmethod dupack(tcp-header count (tcp tcp-reno))
  ;; Dup ack received
  (note-time-seq log-dupack count tcp)
  (cond
    ((fast-recovery-mode tcp)
     (incf (cwnd tcp) (seg-size tcp))
     (tcp-send-pending tcp))
    ((= count 3)
     (setf (fast-recovery-mode tcp) t)
     ;;As Per RFC2581
     (setf (ssthresh tcp)
           (max (/ (window tcp) 2) (* 2 (seg-size tcp))))
     (note-time-seq log-ssthresh (ssthresh tcp) tcp)
    ;; reenter slow start
     (setf (cwnd tcp) (* 3 (seg-size tcp)))
     (retransmit tcp)))
  (note-time-seq log-cwin (cwnd tcp) tcp))

(defmethod retx-timeout-event((tcp tcp-reno))
  ;;As Per RFC2581
  (setf (ssthresh tcp)
        (max (/ (window tcp) 2) (* 2 (seg-size tcp))))
  (note-time-seq log-ssthresh (ssthresh tcp) tcp)
  ;; reset cwnd to seg-size
  (setf (cwnd tcp) (seg-size tcp))
  (note-time-seq log-cwin (cwnd tcp) tcp);
  ;; Start from highest ack
  (setf (next-tx-seq tcp) (highest-rx-ack tcp))
  (setf (fast-recovery-mode tcp) nil)
  (lens.math::increase-multiplier (rtt tcp))
  (retransmit tcp))
