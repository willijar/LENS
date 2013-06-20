;; Statistics base API
;; Copyright (C) 2013 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of LENS

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; LENS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(in-package :lens)

(defgeneric title(instance)
  (:documentation "Return the publishable title for a result"))

(defgeneric record(recorder time value)
  (:documentation "Must be implemented for all result recorders"))

(defgeneric report(recorder stream)
  (:documentation "Report results from recorder to stream"))

(defstruct weighted ;; structure for collecting weighted values
  (weight 1.0 :type double-float)
  (value))

(defmethod record(recorder time (value weighted))
  "By default most recorders only want value"
  (record recorder time (weighted-value value)))

(defvar *statistic-filter-generators* (make-hash-table)
  "Mapping betetween statistic filter names and function generators")

(defvar *result-recorders* (make-hash-table)
  "Mapping between statistic recorder name and classes")

(defmacro define-statistic-filter(name (var &rest statevars) &body body)
  "Define and register a statistic filter function. var is the
numberic value to be processed, statevars are the state value
definitions as per let"
  (eval-when(:load-toplevel :execute)
    (setf
     (gethash name *statistic-filter-generators*)
    (function (lambda()
      (eval
      `(let (,@statevars)
          (function (lambda(,var)
            (or (progn ,@body)
                (throw 'filter-abort ,name)))))))))))

(defun make-statistic-filter(name)
  (let ((f (gethash name *statistic-filter-generators*)))
    (when f (funcall f))))

(defun define-result-recorder(classname &optional (name classname))
  (setf (gethash name *result-recorders*)
        (find-class classname)))

(defun make-result-recorder(spec owner)
  (multiple-value-bind(name args)
      (if (listp spec) (values (car spec) (cdr spec)) (values spec))
    (let ((class (gethash name *result-recorders*)))
      (if class
          (apply #'make-instance class
                 (nconc (list :name name :owner owner) args))
          (error "Unknown result recorder ~A" name)))))

(defclass statistic-listener(listener)
  ((title :type string :reader title)
   (signal-values :type list :initform nil :accessor signal-values
                  :documentation "Cache of last signal values received")
   (source :type function :initform nil :initarg :source :reader source
           :documentation "Filter function for this statistic")
   (recorders :initarg :recorder :initform nil :reader recorders
              :documentation "The result recorders for this statistic"))
  (:documentation "Listener for statistics"))

(defmethod finish((instance statistic-listener))
  (map nil #'finish (recorders instance)))

(defun filter-code-walk(instance expression)
  "Return filter code from a filter expression"
  (let ((signals nil))
    (labels ((do-expand(form)
              (typecase form
                (symbol
                 (if (signal-id form)
                     (progn
                       (pushnew form signals)
                       `(signal-value ',form))
                     form))
               (list
                (let* ((name (first form))
                       (args (rest form))
                       (filter (make-statistic-filter name)))
                  (if filter
                      `(funcall ,filter ,@(mapcar #'do-expand args))
                      `(,name ,@(mapcar #'do-expand args)))))
               (t form))))
      (values
       (eval
        `(flet((signal-value(name)
                 (let ((v (getf (signal-values ,instance) name 'no-value)))
                   (when (eql v 'no-value) (throw 'filter-abort nil))
                   v)))
           (lambda() ,(do-expand expression))))
       signals))))

(defmethod initialize-instance :after ((instance statistic-listener)
                                       &key statistic &allow-other-keys)
  (setf (slot-value instance 'title)
        (or (getf statistic :title) (string (name instance))))
  (let* ((spec
          (multiple-value-bind(v f-p)
              (read-parameter instance 'result-recording-modes
                              '(read :multiplep t))
            (if f-p v (list :default))))
         (recording-modes
          (append (getf statistic :default)
                  (when (member :all spec) (getf statistic :optional)))))
    (loop :for a :on spec :by #'cdr
       :when (eql (car a) '+)
       :do (pushnew (second a) recording-modes)
       :when (eql (car a) '-)
       :do (setf recording-modes
                 (delete (second a) recording-modes :test #'equal)))

    (setf (slot-value instance 'recorders)
            (mapcar
             #'(lambda(recorder-mode)
                 (make-result-recorder recorder-mode instance))
             recording-modes)))
  (multiple-value-bind(filter signals)
      (filter-code-walk instance
                        (or (getf statistic :source) (name instance)))
    (setf (slot-value instance 'source)  filter)
    (dolist(signal signals)
      (subscribe (owner instance) signal instance))))

(defclass result-recorder(owned-object)
  ()
  (:documentation "A base class for all result recorders"))

(defmethod title((instance result-recorder))
  (format nil "~A[~A]"
          (title (owner instance))
          (class-name (class-of instance))))

;; map listener receive-signal with source onto statistic receive-signal with time
(defmethod receive-signal((listener statistic-listener) signal
                          source (value timestamped))
  (receive-signal listener signal
                  (timestamped-time value) (timestamped-value value)))

(defmethod receive-signal((listener statistic-listener) signal
                          source value)
  (receive-signal listener signal (simulation-time) value))

(defmethod receive-signal((listener statistic-listener) signal (time real)
                          value)
  (unless (>= time (warmup-period *simulation*))
    (return-from receive-signal))
  (setf (getf (signal-values listener) signal) value)
  (catch 'filter-abort
    (let ((value (funcall (source listener))))
      (dolist(recorder (recorders listener))
        (record recorder time value)))))

(defmethod info((r result-recorder))
  (with-output-to-string(os) (report r os)))

(defclass scalar-recorder(result-recorder)
  ())

(defgeneric recorded-value(scalar-recorder)
  (:documentation "Return the value to record for a scalar recorder"))

(defmethod finish((r scalar-recorder))
  (let ((os (scalar-stream *simulation*)))
    (when (and os (scalar-recording r))
      (report r os))))

(defmethod report((r scalar-recorder) stream)
  (format stream "scalar ~A ~A ~A~%"
          (full-path-string (owner (owner r))) (title r) (recorded-value r)))

(defun add-statistics(sim)
  (labels((do-add-statistics(module)
            (loop :for a :on (properties module) :by #'cddr
               :when (eql (car a) :statistic)
               :do (make-instance 'statistic-listener
                                  :owner module
                                  :name (car (second a))
                                  :statistic (rest (second a))))
            (when (typep module 'compound-module)
              (for-each-submodule module #'do-add-statistics))))
    (do-add-statistics (network sim))))

(eval-when(:load-toplevel :execute)
  (pushnew 'add-statistics *simulation-init-hooks*))
