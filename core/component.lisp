;; Component base class definition and implementation
;; Copyright (C) 2013-2014 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of Lisp Educational Network Simulator (LENS)

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

;; Component class adds in random number sequence mapping and tracing
;; functionality on top of parameter and signal handling. Base class
;; for all modules and channels in simulation which require these
;; capabilities.

;; Note that both tracing (using tracelog) and rng mapping depend on
;; the *context* dynamic global variable being set to the correct component.
;; This is set for initialize-instance, finish and for handle-message.
;; Functions or methods designed to be used to access a component
;; directly outside these contexts MUST explicitely bind *context* around
;; around any dynamic context using random number generation or tracing.
;; and SHOULD do it in all cases as a matter of safe practice.

;;; Code:

(in-package :lens)

(defclass component(parameter-object entity-with-signals)
  ((collect-trace-info
    :type boolean :initform nil :parameter t
    :documentation "If true evenlog outputs will be traced for this component")
   (rng-map :type array :reader rng-map
            :documentation "RNG map for this component")
   (initialized-p
    :initform nil :reader initialized-p
    :documentation "True if this component has been initialized."))
  (:default-initargs :num-rngs 1)
  (:metaclass parameter-class))

(defmethod print-object((m component) os)
  (print-unreadable-object(m os :type t :identity nil)
    (write-string (lens::full-path-string m) os)))

(defmethod initialize-instance :around
    ((component component) &key &allow-other-keys)
  (let ((*context* component))
    (call-next-method)))

(defmethod initialize-instance :before
    ((component component) &key num-rngs &allow-other-keys)
  (setf (slot-value component 'rng-map) (make-array num-rngs)))

(defmethod configure :before ((component component))
  (let* ((map (slot-value component 'rng-map))
         (path (copy-list (full-path component)))
         (last (last path))
         (format `(integer :min 0 :max ,(num-rngs *simulation*))))
    (dotimes(i (length map))
      (rplacd last (list (intern (format nil "RNG-~D" i))))
      (setf (aref map i)
            (multiple-value-bind(m found-p)
                (read-parameter
                 path (configuration component) format)
              (aref (rng-map *simulation*) (if found-p m 0)))))
    map))

(defmethod finish :around ((component component))
  (let ((*context* component))
    (call-next-method)))

(defun scalar-recording(component)
  (multiple-value-bind(v f-p)
      (read-parameter component 'scalar-recording 'boolean)
    (if f-p v t)))

(defun vector-recording(component)
  (multiple-value-bind(v f-p)
      (read-parameter component 'vector-recording  'boolean)
    (if f-p v t)))
