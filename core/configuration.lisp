;; Trie (prefix tree) based pattern matching for configuration file
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

;; We start with a generic trie structure which includes glob style
;; pattern matching - '* matches a single element and '** any subsequence
;; We use string-equal so package names are not needed in config file.

;;; Code:

(in-package :lens)

(defstruct (parameter-source
             (:print-object
              (lambda(s os)
                  (format os "\"~A\" line ~A"
                          (parameter-source-pathname s)
                          (parameter-source-line-number s)))))
  pathname
  line-number)

(defclass trie()
  ((trie-prefix :initarg :prefix :reader trie-prefix
                :documentation "The prefix matched by this branch")
   (trie-value :initarg :value :reader trie-value
               :documentation "The value stored at this branch")
   (trie-children :initform nil
                  :initarg :children :type list :accessor trie-children
                  :documentation "The children trie's")
   (trie-source :initarg :source :accessor trie-source))
  (:documentation "A trie data structure for prefix matching"))

(defmethod print-object((trie trie) stream)
  (print-unreadable-object(trie stream :type t :identity t)
    (format stream "~A" (trie-prefix trie))
    (when (slot-boundp trie 'trie-value)
      (format stream "=~A" (trie-value trie)))
    (when (trie-children trie)
      (format stream " (~D children)" (length (trie-children trie))))
    (when (slot-boundp trie 'trie-source)
      (format stream " ~A" (trie-source trie)))))

(defgeneric make-trie(sequence value &optional source)
  (:documentation "Make a heirarchy from a sequence with value stored"))

(defgeneric nmerge-trie(trie1 trie2)
  (:documentation "merge children of trie2 into trie1 and return
  modified trie1"))

(defgeneric nmerge-child(trie1 trie2)
  (:documentation "merge trie2 as a child into trie 1 returning the
  modified trie1. Note trie1 may share structure with trie1."))

(defgeneric trie-match(pattern structure)
  (:documentation "Matching function returns value from structure
  matching pattern, whether match was found and source of value"))

(defgeneric trie-delete(pattern structure)
  (:documentation "Delete branch from a trie matching pattern"))

(defmethod make-trie((pattern list) value  &optional source)
  (if (rest pattern)
      (make-instance 'trie
                     :prefix (first pattern)
                     :source source
                     :children (list (make-trie (rest pattern) value source)))
      (make-instance 'trie
                     :prefix (first pattern)
                     :source source
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
        (if (or (not (string-equal (trie-prefix trie1) (trie-prefix trie2)))
                (and (slot-boundp trie2 'trie-value)
                     (slot-boundp trie1 'trie-value)))
            (error 'trie-merge-condition :trie trie1 :trie2 trie2)
            (do-merge trie1 trie2))
      (merge-anyway() (do-merge trie1 trie2)))))

(defmethod nmerge-child(trie1 child)
  (let ((m (find (trie-prefix child) (trie-children trie1)
                  :key #'trie-prefix :test #'string-equal)))
    (if m
        (nmerge-trie m child)
        (setf (trie-children trie1)
              (cons child (trie-children trie1)))))
  trie1)

(defun match-range(value range)
  (labels((do-match(range)
          (if (string-equal (first range) '|:|)
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

(defgeneric trie-equal(pattern trie-prefix)
  (:documentation "Equality test for input pattern agains a trie-prefix")
  (:method(pattern (trie-prefix string))
    (string-equal pattern trie-prefix))
  (:method(pattern trie)
    (eql pattern trie))
  (:method((pattern integer) (trie-prefix string))
    (let ((p (position #\- trie-prefix)))
      (if p
          (let ((min (parse-integer trie-prefix
                                    :start 0 :end p :junk-allowed t))
                (max (parse-integer trie-prefix
                                    :start (1+ p) :junk-allowed t)))
            (and min max (<= min pattern max)))
          (let ((v (parse-integer trie-prefix :junk-allowed t)))
            (and v (= v pattern)))))))

(defmethod trie-match((pattern list) (trie trie))
  (when (or (trie-equal (first pattern) (trie-prefix trie))
         (string-equal (trie-prefix trie) '*)
         (string-equal (trie-prefix trie) '**)
         (match-range (first pattern) (trie-prefix trie)))
     (let ((more (rest pattern))
           (any-child nil)
           (any-suffix nil))
        ;; we have a match!!
       (unless more
         (return-from trie-match
           (if (slot-boundp trie 'trie-value)
               (values (trie-value trie) t (trie-source trie))
               (values nil nil))))
        ;; look an exact suffix match first
       (dolist(child (trie-children trie))
         (cond
           ((string-equal (trie-prefix child) '*) (setf any-child child))
           ((string-equal (trie-prefix child) '**) (setf any-suffix child))
           (t
            (multiple-value-bind(value found-p source)
                (trie-match more child)
              (when found-p
                (return-from trie-match (values value found-p source)))))))
       (when any-child
         (multiple-value-bind(value found-p source)
             (trie-match more any-child)
           (when found-p
             (return-from trie-match (values value found-p source)))))
       (when any-suffix
         (maplist
          #'(lambda(pattern)
              (multiple-value-bind(value found-p source)
                  (trie-match pattern any-suffix)
                (when found-p
                  (return-from trie-match (values value found-p source)))))
          more))))
    (values nil nil))

(defmethod trie-delete((pattern list) (trie trie))
  (let ((child (find (first pattern) (trie-children trie) :test #'trie-equal
                     :key #'trie-prefix)))
    (when child
      (if (rest pattern)
          (trie-delete (rest pattern) child)
          (progn
            (setf (trie-children trie)
                  (delete child (trie-children trie)))
            child)))))

(defun read-ini-line(is parameter-source)
    "Returns either a string representing a section title, a trie
representing a value, a pathname for an extension file or nil if no
 ((more data"
    (let ((line ;; read continuation lines - ignore comments
           (wstrim
            (with-output-to-string(os)
              (loop
                 (incf (parameter-source-line-number parameter-source))
                 (let* ((s (read-line is))
                        (p (position #\# s :from-end t)))
                   (when p (setf s (subseq s 0 p)))
                   (let ((end (1- (length s))))
                   (unless (or (< end 0) (char= (char s 0) #\#))
                     (when (char/=  (char s end) #\\)
                       (write-string (subseq s 0 (1+ end)) os)
                       (return))
                     (write-string s os :end end)))))))))
      (cond
        ((and (char= (char line 0) #\[)
              (char= (char line (1- (length line))) #\]))
         (wstrim (subseq line 1 (1- (length line)))))
        ((let ((p (position #\= line)))
           (when p
             (make-trie
              (cons nil (split-sequence:split-sequence
                         #\.(wstrim (subseq line 0 p))))
              (wstrim (subseq line (1+ p)))
              (copy-parameter-source parameter-source)))))
        ((zerop (search "include" line :test #'char-equal))
         (parse-namestring (wstrim (subseq line 7))))
        (t
         (error "Parse error in configuration file at ~A" line)))))

(defun read-configuration(pathname &optional (key "General"))
  "Read a configuration file"
  (let ((sections (make-hash-table :test #'equal))
        (current-section "General"))
    (labels((do-read-file(pathname)
              (with-open-file(is pathname :direction :input)
                (let ((saved-section current-section)
                      (source
                       (make-parameter-source
                        :pathname pathname :line-number 0))
                      (*default-pathname-defaults*
                       (merge-pathnames pathname)))
                  (handler-case
                      (loop
                         (let ((v (read-ini-line is source)))
                           (etypecase v
                             (string (setf current-section v)
                                     (unless (gethash v sections)
                                       (setf (gethash v sections)
                                             (make-instance
                                              'trie
                                              :prefix nil :source source))))
                             (pathname
                              (do-read-file (merge-pathnames v)))
                             (trie
                              (let ((s (gethash current-section sections)))
                                (setf (gethash current-section sections)
                                      (if s (nmerge-trie s v) v)))))))
                    (end-of-file(e)
                        (declare (ignore e))
                        (setf current-section saved-section)))))))
      (do-read-file pathname))
    ;; deal with extends (section inheritence)
    (when (string-equal key "General")
      (return-from read-configuration (gethash "General" sections)))
    (let ((merged nil)
          (the-section nil))
      (labels ((do-merge-section(name)
                 (unless (member name merged :test #'string-equal)
                   (push name merged)
                   (let ((section (gethash name sections)))
                     (unless section
                       (error "No configuration section ~A found" name))
                     (let ((extends (trie-match '(nil "extends") section)))
                       (trie-delete '(nil "extends") section)
                       (setf the-section
                             (if the-section
                                 (nmerge-trie the-section section)
                                 section))
                       (if extends
                           (map 'nil #'do-merge-section
                                (map 'list #'wstrim
                                     (split-sequence::split-sequence #\, extends)))
                         (do-merge-section "General")))))))
        (do-merge-section key))
      the-section)))