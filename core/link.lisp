;; Physical Link interface and base implementation
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

;; Generic interface for link
(defgeneric propagation-speed(link)
  (:documentation "Return the signal speed on link")
  (:method(link) (declare (ignore link)) +c+))

(defgeneric bandwidth(link)
  (:documentation "Return the bandwidth of a link in bits/sec"))

(defgeneric link(entity)
  (:documentation "Return the link associated with an entity"))

(defgeneric peer-interfaces(link interface)
  (:documentation "Return the list of interfaces this interface is connected to on link")
  (:method(link interface) (remove interface (interfaces link))))

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

(defmethod initialize-instance :after ((link link) &key &allow-other-keys)
  (dolist(interface (interfaces link))
    (setf (slot-value interface 'link) link)))

(defmethod send((link link) packet local-interface
                &key (peer-address (dst-address (peek-pdu packet))))
  (let* ((no-bits (* 8 (length-bytes packet)))
         (txtime (/ no-bits (bandwidth link))))
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
  (incf (packets-sent link)))

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
        (bytes-sent link) 0))

(defvar *default-link* '(point-to-point)
  "List of default arguments for make-instance to make a default link")

(defun utilisation(link)
  (let ((now (simulation-time)))
    (if (= now (utilisation-start link))
        0
        (/ (* 8 (bytes-sent link))
           (* (bandwidth link) (- now (utilisation-start link)))))))
