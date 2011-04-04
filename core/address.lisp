;; $Id: address.lisp,v 1.2 2007/11/10 10:45:02 willijar Exp willijar $
;; Internet Address and Mask functionality
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educations Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; address and mask classes
;; All address classes should be invariant so instances can be shared.

;; classname returns instance associated with entity or creates a new address
;; address= will return true if two addresses match
;; address< enables address sorting in a consistent way
;; next-address will return the next address along in a sequence

;;; Code:

(in-package :address)

(defgeneric src-address(entity)
  (:documentation "Return the source address on an entity"))

(defgeneric dst-address(entity)
  (:documentation "Return the destination address of an entity"))

(defgeneric hardware-address(entity)
  (:documentation "Return the hardware (local) address of an entity"))

(defgeneric network-address(entity)
  (:documentation "Return the network address of an entity"))

(defgeneric network-mask(entity)
  (:documentation "Return the network mask for a subnet of an entity"))

(defgeneric address=(a b &optional mask)
  (:documentation "Return true if addresses are equal"))

(defclass address()
  ((address-bytes :type (unsigned-byte *) :initarg :bytes
                  :reader address-bytes))
  (:documentation "Generic class for addresses"))

(declaim (inline subnet))
(defun subnet(addr mask)
  (if mask
      (logand mask (slot-value addr 'address-bytes))
      (slot-value addr 'address-bytes)))

(defmethod address=((a address) (b address) &optional mask)
  (or (eql a b)
      (if mask
          (= (subnet a mask) (subnet b mask))
          (= (slot-value a 'address-bytes) (slot-value b 'address-bytes)))))

(defun address<(a b) ;; used for sorting
  (< (slot-value a 'address-bytes) (slot-value b 'address-bytes)))

(defgeneric broadcast-p(address)
  (:documentation "Return true if a broadcast address")
  (:method((address (eql :broadcast))) t))

(define-condition address-condition(condition)
  ())

(defclass hardware-address(address)
  ()
  (:documentation "Base class for hardware addresses"))

(defclass network-address(address)
  ()
  (:documentation "Base class for hardware addresses"))

(deftype ip() '(unsigned-byte 32))
(deftype mac() '(unsigned-byte 48))
(deftype ipport() '(unsigned-byte 16))

(defclass macaddr(hardware-address)
  ((nextmac :initform 0 :allocation :class :type mac))
  (:documentation "Standard mac address"))

(defmethod broadcast-p((addr macaddr))
  (= (slot-value addr 'address-bytes) #xFFFFFFFFFFFF))

(defmethod length-bytes((addr macaddr)) 6)

(defgeneric macaddr(arg)
  (:documentation "Create a mac address entity. May be aliased")
  (:method(mac) (make-instance 'macaddr :mac mac)))

(defmethod make-load-form((macaddr macaddr) &optional env)
  (declare (ignore env))
  `(macaddr ,(slot-value macaddr 'address-bytes)))

(defmethod initialize-instance :after ((addr macaddr)
                                       &key mac &allow-other-keys)
  (setf (slot-value addr 'address-bytes)
        (cond
          ((or (not mac) (eql mac :next)) (incf (slot-value addr 'nextmac)))
          ((numberp mac)
           (check-type mac mac)
           (setf (slot-value addr 'nextmac)
                 (max mac (slot-value addr 'nextmac)))
           mac)
          ((eql mac :broadcast) #xFFFFFFFFFFFF)
          (t (error "Invalid Mac Address specification ~S" mac)))))

(defmethod print-object((addr macaddr) stream)
  (if *print-escape*
      (print-unreadable-object (addr stream :type t :identity nil)
        (write (slot-value addr 'address-bytes) :stream stream :base 16))
      (write (slot-value addr 'address-bytes) :stream stream :base 16)))

(defclass ipaddr(network-address)
  ()
  (:documentation "An IPv4 Address"))

(defmethod broadcast-p((addr ipaddr))
  (= (slot-value addr 'address-bytes) #xFFFFFFFF))

(defmethod length-bytes((addr ipaddr)) 4)

(defmethod make-load-form((ipaddr ipaddr) &optional environment)
  (make-load-form-saving-slots ipaddr :environment environment))

(defun dotted-to-ipaddr (dotted)
  "String --> number."
  (declare (string dotted))
  (let ((ll (mapcar #'parse-integer (split-sequence #\. dotted :count 4))))
    (+ (ash (first ll) 24) (ash (second ll) 16)
       (ash (third ll) 8) (fourth ll))))

(defun ipaddr-to-dotted (ipaddr)
  "ipaddr --> string."
  (declare (type ip ipaddr))
  (format nil "~d.~d.~d.~d"
          (logand #xff (ash ipaddr -24)) (logand #xff (ash ipaddr -16))
          (logand #xff (ash ipaddr -8)) (logand #xff ipaddr)))

(defgeneric ipaddr(entity)
  (:documentation "Make and return an ip address.")
  (:method((ip integer))
    (assert (typep ip 'ip)
            (ip)
            'address-out-of-range :addr ip)
    (make-instance 'ipaddr :bytes ip))
  (:method((dotted string))
    (make-instance 'ipaddr :bytes (dotted-to-ipaddr dotted)))
  (:method((ip (eql :broadcast)))
    (make-instance 'ipaddr :bytes #xFFFFFFFF)))

(defun read-address(in &optional char arg)
  (declare (ignore char arg))
  (ipaddr (read in t nil t)))

(set-dispatch-macro-character #\# #\I #'read-address)

(defvar *print-ip-format* :dotted
  "Default format for printing ip addresses")

(defgeneric print-ip-format(stream)
  (:documentation "Format control for print ip addresses to a
stream. Can be :dotted for dotted format, a number to output in a
given base or nil which will output as a default unreadable
object. Used only if *print-readably* is false as a reader for ip
addresses is defined.")
  (:method(stream) (declare (ignore stream)) *print-ip-format*))

(defmethod print-object((addr ipaddr) stream)
  (let ((format (print-ip-format stream)))
    (cond
      (*print-escape*
       (format stream "#I\"~A\"" (ipaddr-to-dotted (slot-value addr 'address-bytes))))
      ((eql format :dotted)
       (write-string (ipaddr-to-dotted (slot-value addr 'address-bytes)) stream))
      ((integerp format)
       (write (slot-value addr 'address-bytes) :stream stream :base format))
      (t
       (print-unreadable-object (addr stream :type t :identity t)
         (princ (ipaddr-to-dotted (slot-value addr 'address-bytes)) stream))))))

(defun make-address-mask(mask-len &optional (address-length 32))
  (when (symbolp mask-len)
    (setf mask-len (ecase mask-len (:a 8) (:b 16) (:c 24))))
  (unless (integerp address-length)
    (setf address-length (* 8 (length-bytes address-length))))
  (let ((v 0))
    (multiple-value-bind(q r) (floor mask-len 8)
      (unless (zerop r) (error "Mask length not a multiple of 8 bits"))
      (dotimes(x q)
        (setf v (dpb  #xFF (byte 8 (- address-length (* 8 x))) v))))
    v))

(defmethod network-mask(entity)
  (make-address-mask entity))

(defun ipaddr-allocator(&optional
                        (start (ipaddr "192.168.0.0"))
                        (mask (make-address-mask 16)))
  (let ((last start))
    #'(lambda()
        (setf last (ipaddr (1+  (address-bytes last))))
          (assert (address= start last mask)
                  (last)
                  "Next allocated address ~A not in ~A/~A"
                  last start (logcount mask))
          last)))

(defvar ipaddr-allocator (ipaddr-allocator) "Default ip address allocator")

(defmethod ipaddr((ip (eql :next)))
  (funcall ipaddr-allocator))


