;; Handling for external (e.g.) representations
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:


;;; Code:

(in-package :common)

(defgeneric write-binary(entity stream &key &allow-other-keys)
  (:documentation "Output entity onto stream as a sequence of bytes. big-endian format should be used")
  (:method ((value integer) stream
            &key type
            (length (cond ((subtypep type '(unsigned-byte 8)) 1)
                 ((subtypep type '(unsigned-byte 16)) 2)
                 ((subtypep type '(unsigned-byte 32)) 4)
                 ((subtypep type '(unsigned-byte 48)) 6)
                 ((subtypep type '(unsigned-byte 64)) 8)
                 (t (error "No bytes definition for type ~A" type))))
             &allow-other-keys)
    (dotimes(i length)
      (write-byte (ldb (byte 8 (* (- length i 1) 8)) value) stream))))

(defgeneric read-binary(type stream  &key &allow-other-keys)
  (:documentation "Read binary representation of an entity from stream")
  (:method ((specialiser (eql integer)) stream
            &key
            type
            (length (cond ((subtypep type '(unsigned-byte 8)) 1)
                 ((subtypep type '(unsigned-byte 16)) 2)
                 ((subtypep type '(unsigned-byte 32)) 4)
                 ((subtypep type '(unsigned-byte 48)) 6)
                 ((subtypep type '(unsigned-byte 64)) 8)
                 (t (error "No bytes definition for type ~A" type))))
             &allow-other-keys)
    (let ((value 0))
      (dotimes(i length)
        (dpb (read-byte stream) (byte 8 (* (- length i 1) 8)) value))
      value)))
