;; TCP Tahoe implementation
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

(defclass tcp-tahoe(tcp)
  ()
  (:documentation "Implementation of the Tahoe variation of TCP"))

(defmethod newack(seq (tcp tcp-tahoe))
  ;; New acknowledgement up to sequence number seq
  ;;  Adjust congestion window in response to new ack's received"
  (let ((seg-size (seg-size tcp)))
    (incf (cwnd tcp)
          (if (> (cwnd tcp) (ssthresh tcp))
              ;; Slow start mode, add one segSize to cWnd
              seg-size
              ;; Congestion avoidance mode, adjust by (segsize*segize) / cWnd
              (max 1.0 (/ (* seg-size seg-size) (cwnd tcp))))))
  (note-time-seq log-cwin (cwnd tcp) tcp)
  (common-newack seq tcp))

(defmethod dupack(tcp-header count (tcp tcp-tahoe))
  ;; Dup ack received
  (note-time-seq log-dupack count tcp)
  (when (= count 3) ; triple duplicate ack
    ;As Per RFC2581
    (setf (ssthresh tcp)
          (max (/ (window tcp) 2) (* 2 (seg-size tcp))))
    (note-time-seq log-ssthresh (ssthresh tcp) tcp)
    ;; reenter slow start
    (setf (cwnd tcp) (* (initial-cwnd tcp) (seg-size tcp)))
    (note-time-seq log-cwin (cwnd tcp) tcp);
    ;; For Tahoe, we also reset next-tx-seq
    (setf (next-tx-seq tcp) (highest-rx-ack tcp))
    (tcp-send-pending tcp)))

(defmethod retx-timeout-event((tcp tcp-tahoe))
  ;;As Per RFC2581
  (setf (ssthresh tcp)
        (max (/ (window tcp) 2) (* 2 (seg-size tcp))))
  (note-time-seq log-ssthresh (ssthresh tcp) tcp)
  ;; reset cwnd to seg-size
  (setf (cwnd tcp) (seg-size tcp))
  (note-time-seq log-cwin (cwnd tcp) tcp);
  ;; Start from highest ack
  (setf (next-tx-seq tcp) (highest-rx-ack tcp))
  (lens.math::increase-multiplier (rtt tcp))
  (retransmit tcp))
