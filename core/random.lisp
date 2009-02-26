;; $Id$
;; Constructors for random number generators for
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; we return random number generators as closures.

;;; Code:

(in-package :lens.math)

(defun random-variable(func &rest args)
  (cons (if (symbolp func) (symbol-function func) func) args))

(deftype random-variable() 'list "Random variable type")

(defun random-value(random)
  (etypecase random
    (number random)
    (function (funcall random))
    (list (apply (first random) (rest random)))))

(defun constant(a) a)

(defun uniform(&optional (a 1.0d0) b	)
  (if b
      (+ a (random (- b a)))
      (random a)))

(defun exponential(&optional (r 1.0d0))
  (- (* r (log (uniform)))))

(defun pareto(scale shape)
  (* scale (/ 1.0d0 (expt (uniform) (/ 1.0d0 shape)))))

(defun paretoII(scale shape)
  (* scale (- (/ 1.0d0 (expt (uniform) (/ 1.0d0 shape))))))

(let ((parity nil)
      nextresult)
  (defun normal(avg std)
    (cond ((= std 0) avg)
          (parity (setf parity nil)
                  (+ avg (* std nextresult)))
          (t
           (flet((sam() (- (* 2 (uniform)) 1)))
             (loop :for sam1 = (sam)
                   :for sam2 = (sam)
                   :for rad = (+ (* sam1 sam1) (* sam2 sam2))
                   :while (>= rad 1)
                   :finally
                   (let ((rad (sqrt (/ (* -2 (log rad)) rad))))
                     (setf nextresult (* sam2 rad)
                           parity t)
                     (return (+ avg (* sam1 rad std))))))))))

(defun lognormal(avg std)
  (exp (normal avg std)))




