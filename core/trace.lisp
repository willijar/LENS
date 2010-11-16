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

(in-package :trace)

;; Generic interface with sensible defaults

(deftype trace-status() '(member :enabled :disabled :default))

(defgeneric trace-status(entity stream)
  (:documentation "Return the trace status for given protocol layer on stream")
  (:method(entity stream) (declare (ignore entity stream)) :default))

(defgeneric trace-enabled-p(entity stream)
  (:documentation "Determine if tracing enabled for entity on stream - checking  heirarchically - node, entity (protocol) and layer")
  (:method(entity stream) (eql (trace-status entity stream) :enabled))
  (:method(entity (stream stream))
    (when (open-stream-p stream) (call-next-method))))

(defgeneric trace-detail(entity stream)
  (:documentation "Return the trace detail for given entity on stream. t means all")
  (:method(entity stream) (declare (ignore entity stream)) t))

;; specific trace stream implementation of interface

(defclass trace-stream(fundamental-stream)
  ((os :initarg :stream :reader os :initform *standard-output*)
   (col-index :initform 0 :accessor col-index)
   (node :initform nil :reader node :documentation "Current node")
   (print-time-format
    :initform "~7,3F" :initarg :time-format
    :accessor print-time-format :type string
    :documentation "Format for outputing time in trace")
   (print-ipformat
    :initform :dotted :accessor print-ipformat :initarg :ipformat
    :documentation "How ip addresses are to be printed on this stream")
   (last-log-time
    :initform -1.0 :type time-type :accessor last-log-time
    :documentation "")
   (status :type hash-table :initform (make-hash-table) :reader status)
   (detail :type hash-table :initform (make-hash-table) :reader detail))
  (:documentation "A packet trace stream"))

(defmethod reset((ts trace-stream))
  (terpri (os ts))
  (setf (col-index ts) 0)
  (setf (last-log-time ts) (simulation-time)))

(defmethod stream-element-type ((stream trace-stream))
  (stream-element-type (os stream)))

(defmethod close ((stream trace-stream) &key abort)
  (close (os stream) :abort abort))

(defmethod open-stream-p((stream trace-stream))
  (open-stream-p (os stream)))

(defmethod stream-line-column ((stream trace-stream))
  (col-index stream))

(defun check-sim-time(trace-stream)
  (let ((tm (simulation-time))
        (os (os trace-stream)))
    (unless (= tm (last-log-time trace-stream))
      (eol trace-stream))
    (setf (last-log-time trace-stream) tm)
    (when (zerop (col-index trace-stream))
      (let ((msg (format nil "~? N~D"
                         (print-time-format trace-stream)
                         (list tm)
                         (if (node trace-stream)
                             (uid (node trace-stream)) -1))))
        (write-string msg os)
        (setf (col-index trace-stream) (length msg))))))

(defmethod stream-write-char ((stream trace-stream) char)
  (cond
    ((eql char #\newline)
     (terpri (os stream))
     (setf (col-index stream) 0))
    (t (check-sim-time stream)
       (incf (col-index stream))
       (write-char char (os stream)))))

(defmethod stream-write-sequence((ts trace-stream)
                                 sequence start end
                                 &key &allow-other-keys)
  (declare (type (integer 0 *) start end))
  (check-sim-time ts)
  (let ((os (os ts)))
    (loop :for index :from start :below end
          :for c = (elt sequence index)
          :when (eql c #\newline)
          :do (write-char #\space os)
          :else
          :do (write-char c os)))
  sequence)

(defmethod stream-write-string((ts trace-stream) string &optional start end)
  (stream-write-sequence ts string start end))

(defmethod stream-line-length((ts trace-stream))
  (stream-line-length (os ts)))


;; trace status and trace detail are set by node, protocol
;; or protocol layer number

(defvar *lens-trace-output*
  (make-instance 'trace-stream :stream *standard-output*)
  "Global trace stream(s)")

(defun reset-traces() (reset *lens-trace-output*))
(eval-when(:load-toplevel :execute) (pushnew #'reset-traces *reset-hooks*))

(defmethod trace-status(protocol (stream trace-stream))
  (gethash protocol (status stream) :default))

(defgeneric (setf trace-status)(value entity stream)
  (:documentation "Set the trace status for given protocol, node or layer")
  (:method(value entity (stream trace-stream))
    (check-type value trace-status)
    (setf (gethash entity (status stream)) value))
  (:method(value entity (streams list))
    (dolist(stream streams) (setf (trace-status entity stream) value)))
  (:method(value entity (stream (eql 'nil)))
    "Set trace status for *lens-trace-output*"
    (when *lens-trace-output*
      (setf (trace-status entity *lens-trace-output*) value))))

(defmethod trace-enabled-p((protocol protocol:protocol) stream)
  "Determine if tracing enabled for entity on stream - checking
heirarchically - node, entity (protocol) and layer"
  (when (open-stream-p stream)
    (let ((ts (trace-status (node protocol) stream)))
      (when (eql ts :default)
        (setf ts (trace-status protocol stream)))
      (when (eql ts :default)
        (setf ts (trace-status (protocol:layer protocol) stream)))
      (eql ts :enabled))))

;; trace detail is passed to a PDU trace to control its output
;; a value of t means trace everything

(defmethod trace-detail(entity (stream trace-stream))
    "Return the list of all for entity on stream details"
    (or (gethash entity (detail stream))
        (call-next-method)))

(defgeneric (setf trace-detail)(value entity stream)
  (:documentation
   "Set the trace  detail for given entity on stream")
  (:method((value list) entity (stream trace-stream))
    (setf (gethash entity (detail stream)) value))
  (:method(value entity (streams list))
    (dolist(stream streams)
      (setf (trace-detail entity stream) value)))
  (:method(value entity (streams (eql 'nil)))
    (when *lens-trace-output*
      (setf (trace-detail entity *lens-trace-output*) value))))

(defgeneric eol(stream)
  (:documentation "Write end of line eol to a stream type entity")
  (:method ((stream stream)) (write-char #\newline stream))
  (:method ((stream trace-stream))
            (unless (zerop (col-index stream))
              (write-char #\newline stream)
              (setf (col-index stream) 0)))
  (:method((streams list)) (dolist(stream streams) (eol stream))))

(defun write-pdu-slots(pdu slots mask stream)
  "Helper to write the slots of a PDU to stream. mask is the detail mask
specifying which slots to write. slots is a list of either slot names
or a list of slot name and format string"
  (dolist(slot slots)
    (multiple-value-bind(slot format)
        (if (listp slot)
            (values (first slot) (second slot))
            (values slot " ~A"))
      (when (member slot mask)
        (format stream format
                (if (functionp slot)
                    (funcall slot pdu)
                    (slot-value pdu slot)))))))

(defgeneric pdu-trace(pdu detail stream &key packet text)
  (:documentation "All PDUs should define this method to trace their
output according to detail onto stream")
  (:method((pdu (eql :drop)) detail stream &key packet text)
    "Trace a packet drop"
    (declare (ignore detail))
    (format stream " D-~A ~D" text (uid packet))
    (eol stream))
  (:method((pdu null) detail stream &key packet text)
    (format stream " ~A ~D" text (uid packet))))

(defun write-trace(protocol pdu &key (node (node protocol)) packet text (stream *lens-trace-output*))
  (dolist(stream (if (listp stream) stream (list stream)))
    (when (trace-enabled-p node protocol stream)
      (setf (slot-value stream 'node) node)
      (pdu-trace pdu
                 (if protocol (trace-detail protocol stream) nil)
                 stream
                 :packet packet
                 :text text))))
