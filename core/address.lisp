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

(defclass address()
  ((datum :type (unsigned-byte 32) :initarg :datum))
  (:documentation "Generic class for addresses"))

(defmethod length-bytes((address address)) 4)

(defun address=(a b)
  (or (eql a b)
      (and (eql (type-of a) (type-of b))
           (= (slot-value a 'datum) (slot-value b 'datum)))))

(defun address<(a b)
  (< (slot-value a 'datum) (slot-value b 'datum)))

(defgeneric broadcast-p(address)
  (:documentation "Return true if a broadcast address")
  (:method(address) (= (slot-value address 'datum) #xFFFFFFFF)))

(define-condition address-condition(condition)
  ())

(defclass hardware-address(address)
  ()
  (:documentation "Base class for hardware addresses"))

(defclass network-address(address)
  ()
  (:documentation "Base class for hardware addresses"))

(deftype ip() '(unsigned-byte 32))
(deftype mac() '(unsigned-byte 32))
(deftype ipport() '(unsigned-byte 16))

(defclass macaddr(hardware-address)
  ((nextmac :initform 0 :allocation :class :type mac))
  (:documentation "Standard mac addresse"))

(defgeneric macaddr(arg)
  (:documentation "Create a mac address entity. May be aliased")
  (:method(mac) (make-instance 'macaddr :mac mac)))

(defmethod make-load-form((macaddr macaddr) &optional env)
  (declare (ignore env))
  `(macaddr ,(slot-value macaddr 'datum)))

(defmethod initialize-instance :after ((addr macaddr)
                                       &key mac &allow-other-keys)
  (setf (slot-value addr 'datum)
        (cond
          ((or (not mac) (eql mac :next)) (incf (slot-value addr 'nextmac)))
          ((numberp mac)
           (check-type mac mac)
           (setf (slot-value addr 'nextmac)
                 (max mac (slot-value addr 'nextmac)))
           mac)
          ((eql mac :broadcast) #xFFFFFFFF)
          (t (error "Invalid Mac Address specification ~S" mac)))))

(defmethod print-object((addr macaddr) stream)
  (if *print-escape*
      (print-unreadable-object (addr stream :type t :identity t)
        (write (slot-value addr 'datum) :stream stream :base 16))
      (write (slot-value addr 'datum) :stream stream :base 16)))

(defclass ipaddr(network-address)
  ()
  (:documentation "An IP Address"))

(defmethod make-load-form((ipaddr ipaddr) &optional environment)
  (make-load-form-saving-slots ipaddr :environment environment))

(define-condition address-out-of-range(address-condition error)
  ((addr :initarg :addr :reader addr)
   (subnet :initarg :subnet :initform nil :reader error-subnet))
  (:report (lambda(c s)
             (format s "Address ~S out of range ~@[for subnet ~S~]"
                     (addr c) (error-subnet c)))))

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
    (make-instance 'ipaddr :datum ip))
  (:method((dotted string))
    (make-instance 'ipaddr :datum (dotted-to-ipaddr dotted)))
  (:method((ip (eql :broadcast)))
    (make-instance 'ipaddr :datum #xFFFFFFFF)))

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
       (format stream "#I\"~A\"" (ipaddr-to-dotted (slot-value addr 'datum))))
      ((eql format :dotted)
       (write-string (ipaddr-to-dotted (slot-value addr 'datum)) stream))
      ((integerp format)
       (write (slot-value addr 'datum) :stream stream :base format))
      (t
       (print-unreadable-object (addr stream :type t :identity t)
         (princ (ipaddr-to-dotted (slot-value addr 'datum)) stream))))))

(defclass ipmask(network-address)
  ()
  (:documentation "An IP Mask"))

(defmethod make-load-form((ipmask ipmask) &optional env)
  (declare (ignore env))
  `(ipmask ,(logcount ipmask)))

(defmethod print-object((mask ipmask) stream)
  (if *print-escape*
      (print-unreadable-object (mask stream :type t :identity t)
        (write (slot-value mask 'datum) :stream stream :base 16))
      (write (slot-value mask 'datum) :stream stream :base 16)))

;; we keep a static array of all 32 ipmasks
(defvar *ipmasks*
  (let ((array (make-array (1+ 32))))
    (let ((m #xFFFFFFFF00000000))
      (loop :for c :from 1 :upto 32
            :do (setf (aref array c)
                      (make-instance 'ipmask
                                     :datum (ldb (byte 32 0) (ash m (- c)))))))
    array)
  "Static array of all 32 ipmasks")

(defgeneric ipmask(entity)
  (:documentation "Construct an ipmask from entity")
  (:method((nobits integer)) (aref *ipmasks* nobits))
  (:method((s string)) (aref *ipmasks* (parse-integer s :radix 16))))

(defun subnet(ipaddr mask)
  (make-instance 'ipaddr
                 :datum (logand (slot-value mask 'datum)
                                (slot-value ipaddr 'datum))))

(defun ipaddr-allocator(&optional (start (ipaddr "192.168.0.0"))
                        (mask (ipmask 16)))
  (let ((last start)
        (subnet (subnet start mask)))
    #'(lambda()
        (let ((next (ipaddr (1+ (slot-value last 'datum)))))
          (assert (address= (subnet next mask) subnet)
                  (next subnet)
                  'address-out-of-range
                  :addr next
                  :subnet subnet)
          (setf last next)))))

(defvar ipaddr-allocator (ipaddr-allocator) "Default ip address allocator")

(defmethod ipaddr((ip (eql :next)))
  (funcall ipaddr-allocator))
