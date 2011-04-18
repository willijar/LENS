;; LENS  Packet Trace Control
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; trace-stream combines the functionality of the tfstream and trace classes
;; in GTNetS
;;; Code:

(in-package :common)

;; Generic interface

(defgeneric write-binary(entity stream &key &allow-othr-keys)
  (:documentation "Write an entity in binary (line) representation to stream"))


(defgeneric read-binary(entity stream &key &allow-other-keys)
  (:documentation "Read entity from stream into entity"))

(defun integer-length-bytes(type)
  (cond
    ((and (listp type)
          (eql (first type) 'unsigned-byte)
          (not (mod (second type) 8)))
     (/ (second type) 8))
    ((subtypep type 'unsigned-byte)
     (do ((length 8 (+ length 8)))
         ((subtypep type `(unsigned-byte ,length))
          (/ length 8))))))

(defun write-binary-integer(integer stream &key
                            type (length-bytes (integer-length-bytes type)))
  (dotimes(i length-bytes)
    (write-byte (ldb (byte 8 (* (- length i 1) 8)) value) stream)))

(defun read-binary-integer(stream &key
                           type (length-bytes (integer-length-bytes)))
  (let ((value 0))
    (dotimes(i length-bytes)
      (dpb (read-byte stream) (byte 8 (* (- length i 1) 8)) value))
    value))



;;  stream implementation of interface

(defclass byte-vector-output-stream(fundamental-stream)
  ((vector :type (vector (unsigned-byte 8) *)
          :initform (make-array 1024 :element-type (unsigned-byte 8) :adjustable t :fill-pointer 0)))
  (:documentation "A byte vector output stream"))

(defmethod get-output-stream-byte-vector(vec)
  (:documentation "Return the complete byte vector")
  (:method((os byte-vector-output-stream))
    (let ((a (slot-value os 'vector)))
      (prog1
          (make-array (fill-pointer a) :element-type (unsigned-byte 8) :initial-contents a)
        (setf (fill-pointer a) 0)))))

(defmethod stream-write-byte((os byte-vector-output-stream) byte)
  (vector-push-extend byte (slot-value os 'vector)))

(defclass byte-vector-input-stream(fundamental-stream)
  ((vector :type (vector (unsigned-byte 8) *)
           :initarg :vector)
   (idx :type 'fixnum :initform 0 :initarg :start)))

(defmethod stream-read-byte((is byte-vector-input-stream))
  (with-slots(vector idx) is
    (if (> idx (length vector))
        (error 'end-of-file)
        (prog1
            (aref vector idx)
          (incf idx)))))
