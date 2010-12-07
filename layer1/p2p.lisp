;; A simple point-to-point link connecting two interfaces
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer1)

(defclass point-to-point(link)
  ((simplex-p :type boolean :initarg :simplex :initform nil :reader simplex-p
              :documentation "If true this is a simplex link")
   (unidirectional-p :type boolean :initarg :unidirectional :initform nil
                     :reader unidirectional-p
                     :documentation "If true this link is unidirectional")
   (local-interface
    :type interface :initarg :local-interface :reader local-interface
    :documentation "The local (sending) interface of this link")
   (peer-interface
    :initform nil :type interface
    :initarg :peer-interface :reader peer-interface
    :reader default-peer-interface
    :documentation "The remote (receiving) interface on this link"))
  (:documentation "A serial point to point link"))

(defmethod initialize-instance :after ((link point-to-point)
                                       &key interfaces &allow-other-keys)
  (when interfaces
    (assert (= 2 (length interfaces))
            (interfaces)
            "Point-to-point link only has two interfaces")
    (setf (slot-value link 'local-interface) (first interfaces)
          (slot-value link 'peer-interface) (second interfaces))))

(defmethod send :before ((link point-to-point) packet (interface interface)
                         &key &allow-other-keys)
  (when (and (unidirectional-p link)
             (not (eql interface (local-interface link))))
    (error "Attempt to send packet the wrong way along a unidirectional link"))
  (when (and (simplex-p link)
             (slot-value local-interface 'tx-packet)
             (slot-value peer-interface 'tx-packet))
    (error "Attempt to send a packet in both directions over a simplex link")))

(defmethod peer-interfaces((link point-to-point) interface)
  (if (eql interface (local-interface link))
      (list (peer-interface link))
      (unless (unidirectional-p link)
        (list (local-interface link)))))
