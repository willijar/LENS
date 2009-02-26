;; $Id$
;; Implementation of
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:


(in-package :routing)

(defun dijkstra(start vertices outgoing-edges
                &key (edge-cost #'second) (edge-end-vertice #'first))
  "Dijkstra's SPF algorithm - determines shortest path from start to
all other vertices (a list). outgoing-edges function should, given a vertice,
return all edges on which the edge-cost and end-end-vertice functions should
return the cost and end vertices respectively.

Returns three hashtables keyed by the end vertices
 - the previous vertices on the route
 - the cost to that vertice
 - the edge (link) from the previous vertice on the route."
  (let ((d (make-hash-table))
        (previous (make-hash-table))
        (links (make-hash-table))
        (inf most-positive-long-float))
    ;; initialize d to unknown distance
    (loop :for v :in vertices
          :do (setf (gethash v d) inf))
    (setf (gethash start d) 0)
    ;; could also allow multiple values for previous and d to allow for
    ;; multiple shortest routes
    (let ((q (copy-list vertices)))
      (flet ((extract-min() ;; could replace by using priority queue
               (let ((min-d inf)
                     (u nil))
                 (loop :for v :in q
                       :do (let ((dv (gethash v d)))
                             (when (< dv min-d)
                               (setf u v
                                     min-d dv))))
                 (setf q (delete u q)) ;; inefficient - scans again
                 u)))
      (loop :while q
            :for u = (extract-min)
            :do
            (loop :for edge :in (funcall outgoing-edges u)
                  :do (let ((nd (+ (gethash u d) (funcall edge-cost edge)))
                            (v (funcall edge-end-vertice edge)))
                        (when (< nd (gethash v d))
                          (setf (gethash v d) nd
                                (gethash v previous) u
                                (gethash v links) edge)))))))
    (values previous d links)))

(defun dijkstra-extract-route(end previous)
  "Return a list of the route in order to end from the previous output
of dijkstra"
  (let ((route (list end)))
    (loop :for n = (gethash end previous) :then (gethash n previous)
          :while n
          :do (push n route))
    route))

(defun dijkstra-first-hops(previous)
  "Return a hash table mapping first hop nodes to list of destination nodes
using previous output from dijkstra"
  (let ((results (make-hash-table)))
    (maphash
     #'(lambda(vertice dummy)
         (declare (ignore dummy))
         (let* ((route (dijkstra-extract-route vertice previous))
                (first-hop (second route)))
             (loop :for a :in (rest route)
                   :do (pushnew a (gethash first-hop results)))))
     previous)
    results))




#|
;;; Dijkstra test

(let ((vertices '(0 1 2 3 4))
      (edges '(((1 10) (2 5))
               ((3 1) (2 2))
               ((4 2) (1 3) (3 9))
               ((4 4))
               ((0 7) (3 6)))))
  (flet((edges(n) (elt edges n)))
    (loop :for start :in vertices
          :do (multiple-value-bind(previous d)
                  (dijkstra start vertices #'edges)
                (format t "~%~D~%" start)
                (loop :for end :in vertices
                      :do (format t "~S (~D)~%" (extract-route end previous)
                                  (gethash end d)))))))

-->

|#