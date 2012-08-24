;; tcp Message
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; message data is a data segment which contains a set of messages
;; messages are represented by their end offset in the message data and
;; an expected response size from a server.

;;; Code:

(in-package :protocol.layer5)

(defstruct message
  (offset 0 :type integer)
  (created (simulation-time))
  (response-size nil))

(defclass message-data(data)
  ((messages :type list :initform nil :accessor messages :initarg :messages
             :documentation "List of messages in this data segment")))

(defmethod print-object((m message-data) stream)
  (print-unreadable-object(m stream :type t :identity t)
    (format stream "~D bytes ~D messages"
            (length-bytes m) (length (messages m)))))

(defun make-message-data(length-bytes &key response-size)
  (make-instance 'message-data
                 :length-bytes length-bytes
                 :messages (list (make-message :response-size response-size
                                               :offset length-bytes))))

(defmethod data-concatenate((a message-data) (b message-data))
  (let ((result (call-next-method))
        (offset (length-bytes a)))
    (setf (messages result)
          (append
           (messages a)
           (mapcar
            #'(lambda(m)
                (make-message
                 :created (message-created m)
                 :response-size (message-response-size m)
                 :offset (+ (message-offset m) offset)))
            (messages b))))
    result))

(defmethod data-subseq((data message-data) start &optional length-bytes)
  (declare (ignore length-bytes))
  (let* ((result (call-next-method))
         (end (+ start (length-bytes result))))
    (setf (messages result)
          (mapcan
           #'(lambda(m)
               (let ((o (message-offset m)))
                 (when (and (> o start) (<= o end))
                   (list
                    (make-message
                     :response-size (message-response-size m)
                     :created (message-created m)
                     :offset (- o start))))))
           (messages data)))
    result))