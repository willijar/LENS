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

(defclass tcp-newreno(tcp-reno)
  ((reno-high-tx-mark :initform 0 :type seq :accessor reno-high-tx-mark)
   (partial-ack-count :initform 0 :type counter :accessor partial-ack-count))
  (:documentation "Implementation of the NewReno variation of TCP"))

(defmethod newack(seq (tcp tcp-tahoe))
  ;; New acknowledgement up to sequence number seq
  (let ((skip-timer nil)
        (seg-size (seg-size tcp)))
    (cond
      ((fast-recovery-mode tcp)
       ;; If in fast recovery and have a new data ack, check for
       ;; full or partial ack, per rfc3782
       (cond
         ((>= seq (reno-high-tx-mark tcp))
          ;; Reset cWnd and exit fastRecovery
          (setf (cwnd tcp)
                (min (ssthresh tcp) (+ seg-size (unack-data-count tcp)))
                (fast-recovery-mode tcp) nil
                (partial-ack-count tcp) 0))
         (t
          (let ((delta (- seq (highest-rx-ack tcp))))
            (decf (cwnd tcp) delta)
            (when (>= delta seg-size) (incf (cwnd tcp) (seg-size tcp))))
          (setf (highest-rx-ack tcp) seq)
          (setf skip-timer (> (partial-ack-count tcp) 0))
          (retransmit tcp))))
      ((< (cwnd tcp) (ssthresh tcp))
       ;; slow start mode add one seg-size to cwnd
       (incf (cwnd tcp) seg-size))
      (t ;; Congestion avoidance mode, adjust by (segsize*segize) / cWnd
       (setf (cwnd tcp) (max 1.0 (/ (* seg-size seg-size) (cwnd tcp))))))
    (note-time-seq log-cwin (cwnd tcp) tcp)
    (common-newack seq tcp skip-timer)))

(defmethod dupack(tcp-header count (tcp tcp-newreno))
  ;; Dup ack received
  (note-time-seq log-dupack count tcp)
  (cond
    ((fast-recovery-mode tcp)
     (incf (cwnd tcp) (seg-size tcp))
     (tcp-send-pending tcp))
    ((and (= count 3) (> (ack-number tcp-header) (reno-high-tx-mark tcp)))
     ;;Count of three indicates triple dupack has been received
     ;;and covers at least "recover" bytes
     (setf (fast-recovery-mode tcp) t
           (partial-ack-count tcp) 0
           (ssthresh tcp)  (max  (/ (window tcp) 2) (* 2 (seg-size tcp))))
     (note-time-seq log-ssthresh (ssthresh tcp) tcp)
     (setf (cwnd tcp) (* 3 (seg-size tcp))
           (reno-high-tx-mark tcp) (next-tx-seq tcp))
           (retransmit tcp)))
  (note-time-seq log-cwin (cwnd tcp) tcp))

(defmethod retx-timeout-event((tcp tcp-newreno))
  ;;As Per RFC2581
  (setf (ssthresh tcp)
        (max (/ (window tcp) 2) (* 2 (seg-size tcp))))
  (note-time-seq log-ssthresh (ssthresh tcp) tcp)
  ;; reset cwnd to seg-size
  (setf (cwnd tcp) (initial-cwnd tcp))
  (note-time-seq log-cwin (cwnd tcp) tcp)
  ;; Start from highest ack
  (setf (next-tx-seq tcp) (highest-rx-ack tcp)
        (fast-recovery-mode tcp) nil
        (partial-ack-count tcp) 0)
  (lens.math::increase-multiplier (rtt tcp))
  (retransmit tcp))
