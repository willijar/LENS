;; Scenario based physical process implementation
;; Copyright (C) 2014 Dr. John A.R. Williams

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

;; The model takes a list of timevarying sources at specified locations

;;; Code:
(in-package :lens.wsn)

(defclass scenario-physical-process(physical-process)
  ((k :parameter t :type real :initform 0.25 :initarg :k
                     :documentation "multiplicative parameter (k)")
   (a :parameter t :type real :initform 1.0 :initarg :a
                      :documentation "exponential attenuation exponent (a) for values vs distance*/k/")
   (sources :parameter t :type list :properties (:format read)
            :reader sources
            :documentation "List of lists showing how sources evolve
            over time. Each source is represented as a coordinate and
            a list of conses of times and values in order."
            :initform
            `((,(make-coord 10.0 10.0) (0.0 . 30.5) (5.0 . 45) (12.0 . 7.3)))))
  (:default-initargs :description "fire")
  (:metaclass module-class)
  (:documentation "A Physical process made up of a number of discrete
physical sources whose output evolves over time. The effect of
multiple sources in a certain point is additive. At a certain location
and time the physical process value is given by

\\begin{equation}
V(p,t) = \\sum_{i=\\text{all sources}} \\frac{V_i(t)}{(K d_i(p_i,t))^a}
\\end{equation}

where $V(p,t)$ denotes the value of the physical process at point $p$
and time $t$, $V_i(t)$ denotes the value of the $i^{th}$ source at time
$t$, $K$ and $a$ are specified by module parameters [[k]] and [[a]]
that determine how the value from a source diffused.
"))

(defmethod measure((process scenario-physical-process)
                   (measurand (eql 'temperature))
                   (location coord)
                   time)
  (let ((k (slot-value process 'k))
        (a (slot-value process 'a)))
    (reduce
     #'+
     (sources process)
     :key
     #'(lambda(source)
          (let ((distance (distance location (car source)))
                (value
                 (loop :for r :on (rest source)
                    :for a = (car r)
                    :until (not (cdr r))
                    :finally (return 0)
                    :when (and (>= time (car a)) (< time (car (cadr r))))
                    :do
                    (progn
                    (let* ((b (cadr r))
                           (dt (- (car b) (car a)))
                           (dx (- (cdr b) (cdr a)))
                           (coeff (/ (- time (car a)) dt)))
                      (return (+ (cdr a) (* coeff dx)) ))))))
            (* value (expt (1+ (* k distance)) (- a))))))))
