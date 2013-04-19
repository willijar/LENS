;; Trie (prefix tree) based pattern matching
;; Copyright (C) 2011 Dr. John A.R. Williams

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

;; We start with a generic trie structure which includes glob style
;; pattern matching - '* matches a single element and '** any sequence

;;; Code:

(in-package :lens)

(defclass trie()
  ((trie-prefix :initarg :prefix :reader trie-prefix
                :documentation "The prefix matched by this branch")
   (trie-value :initarg :value :reader trie-value
               :documentation "The value stored at this branch")
   (trie-children :initform nil
                  :initarg :children :type list :accessor trie-children
                  :documentation "The children trie's"))
  (:documentation "A trie data structure for prefix matching"))

(defmethod print-object((trie trie) stream)
  (print-unreadable-object(trie stream :type t :identity t)
    (format stream "~A" (trie-prefix trie))
    (when (slot-boundp trie 'trie-value)
      (format stream "=~A" (trie-value trie)))
    (when (trie-children trie)
      (format stream " (~D children)" (length (trie-children trie))))))

(defgeneric make-trie(sequence value)
  (:documentation "Make a heirarchy from a sequence with value stored"))

(defgeneric nmerge-trie(trie1 trie2)
  (:documentation "merge children of trie2 into trie1 and return
  modified trie1"))

(defgeneric nmerge-child(trie1 trie2)
  (:documentation "merge trie2 as a child into trie 1 returning the
  modified trie1. Note trie1 may share structure with trie1."))

(defgeneric trie-match(pattern structure)
  (:documentation "Matching function returns value from structure
  matching pattern and whether match was found"))

(defmethod make-trie((pattern list) value)
  (if (rest pattern)
      (make-instance 'trie
                     :prefix (first pattern)
                     :children (list (make-trie (rest pattern) value)))
      (make-instance 'trie
                     :prefix (first pattern)
                     :value value)))

(define-condition trie-condition(serious-condition)
  ((trie :initarg :trie :reader trie))
  (:report (lambda(condition stream)
               (format stream "Trie problem with ~A" (trie condition)))))

(define-condition trie-merge-condition(trie-condition)
  ((trie2 :initarg :trie2 :reader trie2))
  (:report (lambda(condition stream)
               (format stream "Error merging ~A into ~A"
                       (trie2 condition) (trie condition)))))

(defmethod nmerge-trie(trie1 trie2)
  (flet ((do-merge(trie1 trie2)
           (when (slot-boundp trie2 'trie-value)
             (setf (slot-value trie1 'trie-value)
                   (slot-value trie2 'trie-value)))
           (dolist(child (trie-children trie2))
             (nmerge-child trie1 child))
           trie1))
    (restart-case
        (if (or (not (eql (trie-prefix trie1) (trie-prefix trie2)))
                (and (slot-boundp trie2 'trie-value)
                     (slot-boundp trie1 'trie-value)))
            (error 'trie-merge-condition :trie trie1 :trie2 trie2)
            (do-merge trie1 trie2))
      (merge-anyway() (do-merge trie1 trie2)))))

(defmethod nmerge-child(trie1 child)
  (let ((m (find (trie-prefix child) (trie-children trie1)
                  :key #'trie-prefix)))
    (if m
        (nmerge-trie m child)
        (setf (trie-children trie1)
              (cons child (trie-children trie1)))))
  trie1)

(defun match-range(value range)
  (labels((do-match(range)
          (if (eql (first range) '|:|)
              (when (and (or (not (second range)) (>= value (second range)))
                        (or (not (third range)) (<= value (third range))))
                (return-from match-range t))
              (dolist(r range)
                (etypecase r
                  (number (when (= r value) (return-from match-range t)))
                  (list (do-match r)))))))
    (when (and (numberp value) (listp range))
      (do-match range)))
  nil)

(defmethod trie-match((pattern list) (trie trie))
  (when (or (eql (first pattern) (trie-prefix trie))
         (eql (trie-prefix trie) '*)
         (eql (trie-prefix trie) '**)
         (match-range (first pattern) (trie-prefix trie)))
     (let ((more (rest pattern))
           (any-child nil)
           (any-suffix nil))
        ;; we have a match!!
       (unless more (return-from trie-match
                      (if (slot-boundp trie 'trie-value)
                          (values (trie-value trie) t)
                          (values nil nil))))
        ;; look an exact suffix match first
       (dolist(child (trie-children trie))
         (cond
           ((eql (trie-prefix child) '*) (setf any-child child))
           ((eql (trie-prefix child) '**) (setf any-suffix child))
           (t
            (multiple-value-bind(value found-p)
                (trie-match more child)
              (when found-p (return-from trie-match (values value found-p)))))))
       (when any-child
         (multiple-value-bind(value found-p)
             (trie-match more any-child)
           (when found-p (return-from trie-match (values value found-p)))))
       (when any-suffix
         (maplist
          #'(lambda(pattern)
              (multiple-value-bind(value found-p)
                  (trie-match pattern any-suffix)
                (when found-p (return-from trie-match (values value found-p)))))
          more))))
    (values nil nil))

