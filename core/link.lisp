;; <description>
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer1)

;; links uses a split-phase approach
;; - send starts sending a packet,
;;   send-complete is called to signal when sending is finished.
;; - receive-start signals the start of reception
;;   receive is called when packet reception is finished.
;; they receive from and send packets to interfaces

(defgeneric send-complete(sender packet receiver &key fail &allow-other-keys)
  (:documentation "Called by receiver when sending completed to notify
  receiver - fail-reason indicates if packet was dropped - null if successful")
  (:method(sender packet receiver &key &allow-other-keys)
    (declare (ignore sender packet receiver))))

(defgeneric receive-start(receiver packet sender)
  (:documentation "Called by sender to start receiver receiving a packet.")
  (:method(receiver packet sender)
    (declare (ignore receiver packet sender))))

(defvar *default-bandwidth* 1e6 "Default link bandwidth")
(defvar *default-delay* 1e-3 "Default link delay")
(defvar *default-ber* 0 "Default bit error rate on a link")

;; Generic interface for link
(defgeneric delay(start end)
 (:documentation "Return the propagation delay in bits/sec between
two things")
   (:method(start end)
    (declare (ignore start end))
    *default-delay*))

(defgeneric bandwidth(interface)
  (:documentation "Return the bandwidth in bits/sec")
  (:method(entity) (declare (ignore entity)) *default-bandwidth*))

(defgeneric bit-error-rate(sender receiver)
  (:documentation "Return the bit error rate for a simple link")
  (:method(sender receiver)
    (declare (ignore sender receiver))
    *default-ber*))

(defgeneric link(entity)
  (:documentation "Return the link associated with an entity"))

(defgeneric peer-interfaces(link interface)
  (:documentation "Return a sequence of all the peer interfaces this link
connects interface to"))

(defgeneric default-peer-interface(link)
  (:documentation "Return the default peer (gateway) on a link"))

(defgeneric receive-own-broadcast-p(link)
  (:documentation "If true interfaces receive their own braodcast")
  (:method(link) (declare (ignore link)) nil))

;; this is equivalent to link-real in GTNetS
(defclass link()
  ((bandwidth
    :type real :initarg :bandwidth :reader bandwidth
    :initform *default-bandwidth*
    :documentation "Link bandwidth in bits/sec")
   (delay
    :type real :initarg :delay :initform *default-delay*
    :documentation "Link Propagation Delay in sec")
   (bit-error-rate  :initarg :bit-error-rate
                    :initform *default-ber*
                    :documentation "Bit Error Rate for this link")
   (weight :type number :initarg :weight :accessor weight :initform 1
    :documentation "Link weight (for some routing protocols)")
   (bytes-sent :type integer :initform 0 :accessor bytes-sent
               :documentation "Total packets sent on this link")
   (packets-sent :type integer :initform 0 :accessor packets-sent
                 :documentation "Total packets sent on this link")
   (utilisation-start
    :type time-type :initform (simulation-time)
    :accessor utilisation-start
    :documentation "Start of utilisation measurement interval"))
  (:documentation "Base Class for simple links"))

(defmethod send((link link) packet local-interface
                &key (peer-address (dst-address (peek-pdu packet))))
  (let* ((no-bits (* 8 (length-bytes packet)))
         (txtime (/ no-bits (bandwidth local-interface))))
    ;; schedule packet transmit complete event
    (schedule txtime (list #'send-complete local-interface packet link))
    ;; schedule packet arrival at interface(s)
    (flet ((schedule-receive(peer-interface packet)
             (let ((delay (delay local-interface peer-interface))
                   (errors
                    (1-
                     (expt
                      (1- (bit-error-rate local-interface peer-interface))
                      no-bits))))
               (schedule delay
                         (list #'receive-start peer-interface packet link))
               (schedule (+ delay txtime)
                         (list #'receive peer-interface packet link
                               :errors errors)))))
      (if (broadcast-p peer-address)
          (dolist(peer-interface (peer-interfaces link local-interface))
            (schedule-receive peer-interface (copy packet)))
          (schedule-receive
           (if peer-address
               (find peer-address (peer-interfaces link local-interface)
                     :key #'hardware-address :test #'address=)
               (default-peer-interface link))
           packet)))))

(defmethod send-complete :before (interface packet (link link)
                                  &key fail &allow-other-keys)
  (declare (ignore interface fail))
  (incf (bytes-sent link) (length-bytes packet))
  (incf (packets-sent link))
  (setf (slot-value link 'busy-p) nil))

(defmethod delay((link link) peer-interface)
  (declare (ignore peer-interface))
  (slot-value link 'delay))

(defmethod bit-error-rate((sender interface) (receiver interface))
  (slot-value (link sender) 'bit-error-rate))

(defmethod bandwidth((sender interface))
  (slot-value (link sender) 'bandwidth))

(defmethod print-object((link link) stream)
  (print-unreadable-object (link stream :type t :identity t)
    (format stream
            "~0:/print-eng/bit/sec ~0:/print-eng/sec delay ~0:/print-eng/BER"
            (slot-value link 'bandwidth)
            (slot-value link 'delay)
            (slot-value link 'bit-error-rate))))

(defmethod reset((link link))
  (setf (utilisation-start link) (simulation-time)
        (packets-sent link) 0
        (bytes-sent link) 0)
  (setf (slot-value link 'busy-p) nil))

(defvar *default-link* '(point-to-point)
  "List of default arguments for make-instance to make a default link")

(defun utilisation(link)
  (let ((now (simulation-time)))
    (if (= now (utilisation-start link))
        0
        (/ (* 8 (bytes-sent link))
           (* (bandwidth link) (- now (utilisation-start link)))))))
