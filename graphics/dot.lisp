;; $Id$
;; <description>
;; Copyright 2007 Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See the LICENSE file provided or <http://www.gnu.org/licenses>

;;; Commentary:

;;

;;; Code:

(in-package :lens-graphics)

(defgeneric attributes(type entity)
  (:documentation "Return the graphical attributes of specified type for an entity"))

(defmethod attributes((type (eql :dot)) (node node))
  `(:label ,(format nil "~A" (node::uid node)) :shape "circle"))

(defmethod attributes((type (eql :dot)) (link link))
  `(:label ))

(defun attributes-to-dot(os attributes)
  (write-char #\[ os)
  (loop :for a :on attributes :by #'cddr
        :do (format os "~A=~S " (car a) (cadr a)))
  (write-string "];" os))

(defun connectivity(nodes &key (neighbours #'routing::neighbours))
  (map 'list
       #'(lambda(n) (cons n (funcall neighbours n)))
       nodes))

(defun is-two-way(interface)
    (some
     #'(lambda(peer)
         (member interface (interface:peer-interfaces peer)))
     (interface::peer-interfaces interface)))

(defun dot(os graph)
  (format os "graph network {~%")
  (dolist(n graph)
    (let ((node (first n)))
      (format os "~A " (node::uid node))
      (attributes-to-dot os (attributes :dot node))
      (terpri os)))
  (dolist(n graph)
    (let ((node (first n))
          (links (rest n)))
      (dolist(link links)
        (format os "~A -> ~A;~%"
                (node::uid node)
                (node::uid (routing::neighbour-node link))))))
  (format os "};~%"))







