;; UDP implementation
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :protocol.layer4)

(defclass udp-header(pdu)
  ((protocol-number :initform 17 :accessor protocol-number :allocation :class)
   (src-port :type ipport :initform 0 :accessor src-port :initarg :src-port)
   (dst-port :type ipport :initform 0 :accessor dst-port :initarg :dst-port)
   (msg-size :type word :initform 0 :accessor msg-size :initarg :msg-size)
   (checksum :type word :accessor checksum :initarg :checksum)
   (seq :type seq :initform 0 :accessor seq :initarg :seq
        :documentation "Sequence id's are not part of UDP, but are useful"))
  (:documentation "UDP PDU class"))

(defmethod length-bytes((h udp-header)) 8)

(defmethod copy((h udp-header))
  (copy-with-slots h '(src-port dst-port msg-size checksum seq)))

(defmethod pdu-trace((pdu udp-header) detail os &key packet text)
  (declare (ignore packet))
  (format os " ~@[~A~] L4-UDP" text)
  (write-pdu-slots
   pdu
   '(src-port dst-port msg-size (checksum "~4,'0X") seq)
   detail
   os))

(defvar *default-udp-packet-size* 512 "Default UDP datagram size in octets")

(defclass udp-pending()
  ((data :initarg :data :reader data)
   (dst-address :initarg :dst-address :reader dst-address)
   (dst-port :initarg :dst-port :reader dst-port)
   (bytes-sent :initform 0 :accessor bytes-sent))
  (:documentation "Record of pending udp requests"))

(defmethod print-object((p udp-pending) stream)
  (print-unreadable-object(p stream :type t :identity t)
    (format stream "~:/print-eng/bytes/~:/print-eng/bytes sent"
            (bytes-sent p) (length-bytes (data p)))))

(defclass udp-demux(protocol)
  ()
  (:documentation "Protocol demultiplexer for received UDP packets"))

(register-protocol 'udp-demux 17)

(defmethod receive((udp udp-demux) packet (ipv4 layer3:ipv4) &key &allow-other-keys)
  (let* ((h (peek-pdu packet))
         (socket (find (dst-port h) (bindings udp) :test #'local-port)))
    (if socket
        (receive socket packet ipv4)
        (drop udp packet :text "L4-NP"))))

(defclass udp(socket)
  ((packet-size :initform *default-udp-packet-size*
                :initarg :packet-size :accessor packet-size
                :documentation "Size of packets")
   (seq :type seq :initform 0 :accessor seq
        :documentation "Seq number of next packet")
   (pending-data :type list :initform nil :accessor pending-data
                 :documentation "Queue of pending data"))
  (:documentation "A model of the User Datagram Protocol."))

(defmethod default-trace-detail((protocol udp))
  `(src-port dst-port msg-size))

(defmethod connect((protocol udp) (peer-address ipaddr) (peer-port integer))
  (setf (peer-address protocol) peer-address
        (peer-port protocol) peer-port)
  (let ((application (application protocol)))
    (when application
      (connection-complete application protocol))))

(defmethod close-connection((udp udp))
  (closed (application udp) udp)
  (when-bind(interface (interface udp))
    (delete-notifications udp interface))
  t)

(defmethod reset((udp udp))
  (when-bind(interface (interface udp))
    (delete-notifications udp interface))
  (while (dequeue (pending-data udp))))

(defmethod send((dmux udp-demux) (packet packet) (sender udp) &rest args)
  "udp-demux just sends packet on to ipv4"
  (apply #'send (find-protocol 'layer3:ipv4 (node dmux)) packet dmux args))

(defmethod send(application data (udp udp)
                &key (dst-address (peer-address udp))
                (dst-port (peer-port udp)) &allow-other-keys)
  (assert (and dst-address dst-port)
          (dst-address dst-port)
          "send of data to ~A:~A invalid for UDP" dst-address dst-port)
  (setf (pending-data udp)
        (nconc
         (pending-data udp)
         (list
          (make-instance 'udp-pending
                         :data data
                         :dst-port dst-port
                         :dst-address dst-address)
          (pending-data udp))))
  (udp-send-pending udp))

(defun udp-send-pending(udp)
  (dolist(pd (pending-data udp))
    (when-bind (interface (find-interface (dst-address pd) udp))
      (loop
           (let ((data (if (and (= (bytes-sent pd) 0)
                                (< (length-bytes (data pd))
                                   (packet-size udp)))
                           (data pd)
                           (layer5:copy-from-offset
                            (packet-size udp) (bytes-sent pd) (data pd) ))))
             (unless (layer1:buffer-available-p
                      (+ 20 (length-bytes data)) interface)
               (add-notification udp #'udp-send-pending interface)
               (return))
             (let ((packet (make-instance 'packet)))
               (push-pdu data packet)
               (incf (bytes-sent pd) (length-bytes data))
               (push-pdu
                (make-instance 'udp-header
                               :src-port (local-port udp)
                               :dst-port (dst-port pd)
                               :msg-size (length-bytes data)
                               :seq (incf (seq udp)))
                packet)
               (send (find-protocol 'layer3:ipv4 (node udp)) packet udp
                     :dst-address (dst-address pd)
                     :src-address (src-address udp)
                     :ttl (ttl udp)
                     :tos (tos udp))
               (sent (application udp) (length-bytes data) udp)))
         (when (= (bytes-sent pd) (length-bytes (data pd)))
           (setf (pending-data udp) (delete pd (pending-data udp)))
           (return))))))
