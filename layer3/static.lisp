;; Implementation of static routing
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Static routing is simple and easy to use by the user, but is memory
;; intensive.  It is recommended for simulation topologies on the
;; order of a few hundred Larger topologies should use either manual
;; or nix-vector routing

;;; Code:

(in-package :layer3)

(defclass routing-static(fib)
  ((link-cost :type function :initarg :link-cost
              :initform #'(lambda(a b) (declare (ignore a b)) 1)
              :documentation "Function to determine cost of link from
              interface a to b when determining routes"))
  (:documentation "A static routing table which fills in static routes
  upon demand"))

(defmethod getroute((address network-address) (routing routing-static)
                      &key (link-cost (slot-value routing 'link-cost))
                    &allow-other-keys)
  (or (call-next-method) ;; lookup fib
      (default-route routing) ;; or default route or use Dijkstra
      (let* ((vertices (map 'vector #'routing-neighbours (nodes)))
             (source (node routing))
             (destination
              (or (node address)
                  (error "Unable to route - no node with address ~A" address))))
         (labels ((end-id(v) (uid (node (vertex-end v))))
                  (edges(i) (mapcar #'end-id (aref vertices i)))
                  (vertex(i j) (find j (aref vertices i) :key #'end-id))
                  (cost(i j)
                    (let ((v (vertex i j)))
                      (funcall link-cost (vertex-start v) (vertex-end v)))))
           (let* ((previous
                   (alg:dijkstra
                    (uid source) (length (nodes))
                    :edges-fn #'edges :cost-fn #'cost))
                  (first-hops (alg:extract-first-hops previous))
                  #+nil(route (alg:extract-route (uid destination) previous)))
            (when (< (aref previous (uid destination)) 0)
              (error "Unable to route - ~A not connected to ~A"
                     source destination))
            ;; determine most common route and set default route to this
            (let ((max 0) (maxhop nil))
              (map 'nil
                   #'(lambda(h)
                       (let ((c (count h first-hops)))
                         (when (> c max)
                           (setf max c
                                 maxhop h))))
                   (remove-duplicates first-hops))
              (setf (default-route routing) (vertex (uid source) maxhop))
              ;; add in all first hops for this node
              (dotimes(i (length (nodes)))
                (let ((hop (aref first-hops i)))
                  (unless (or (= hop maxhop) (< hop 0))
                    (setf (getroute (network-address (node i)) routing)
                          (vertex (uid source) hop))))))
            ;; add in routes to destination to other nodes
            #+nil(dolist(dst (cdr route))
                   (setf (getroute address (routing (node dst)))
                         (vertex dst (uid destination))))))
        (or (call-next-method)
            (default-route routing)))))

(defmethod reinitialise-routes((routing routing-static) entity)
  (setf (default-route routing) nil)
  (call-next-method))

;; if no default set set static as it
(eval-when(:compile-toplevel :load-toplevel)
  (unless *default-routing* (setf *default-routing* '(routing-static))))

;; Floyd–Warshall algorithm

;;  1 /* Assume a function edgeCost(i,j) which returns the cost of the edge from i to j
;;  2    (infinity if there is none).
;;  3    Also assume that n is the number of vertices and edgeCost(i,i) = 0
;;  4 */
;;  5
;;  6 int path[][];
;;  7 /* A 2-dimensional matrix. At each step in the algorithm, path[i][j] is the shortest path
;;  8    from i to j using intermediate vertices (1..k−1).  Each path[i][j] is initialized to
;;  9    edgeCost(i,j) or infinity if there is no edge between i and j.
;; 10 */
;; 11
;; 12 procedure FloydWarshall ()
;; 13    for k := 1 to n
;; 14       for i := 1 to n
;; 15          for j := 1 to n
;; 16             path[i][j] = min ( path[i][j], path[i][k]+path[k][j] );

;;  1 procedure FloydWarshallWithPathReconstruction ()
;;  2    for k := 1 to n
;;  3       for i := 1 to n
;;  4          for j := 1 to n
;;  5             if path[i][k] + path[k][j] < path[i][j] then
;;  6                path[i][j] := path[i][k]+path[k][j];
;;  7                next[i][j] := k;
;;  8
;;  9 procedure GetPath (i,j)
;; 10    if path[i][j] equals infinity then
;; 11      return "no path";
;; 12    int intermediate := next[i][j];
;; 13    if intermediate equals 'null' then
;; 14      return " ";   /* there is an edge from i to j, with no vertices between */
;; 15   else
;; 16      return GetPath(i,intermediate) + intermediate + GetPath(intermediate,j);
