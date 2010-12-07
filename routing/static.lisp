;; Implementation of global static routing
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

(defclass static-routing-table()
  ((nodes :initarg :nodes :initform (node:nodes) :reader routing-nodes
          :documentation "Nodes to be included in routing")
   (costs :type (array real) :reader costs
          :documentation "Array of routing costs")
   (vertices :type (array real) :reader vertices
         :documentation "Array of vertices")
   (cost-fn :type function :initarg :cost-fn
            :initform #'(lambda(a b)  1)))
  (:documentation "A shared global static routing table"))

(defmethod reinitialise-routes((table static-routing-table) changed)
  ;;Naively clear route map and call initialise-routes
  (declare (ignore changed))
  (with-slots(nodes node-map costs vertices) table
    (let ((n (length nodes)))
    (setf costs (make-array (list n n)
                            :element-type 'short-float
                            :inital-element +infinity+)
          vertices (make-array (list n n) :element-type 'vertex))
      (loop :for node :across nodes
         :do (dolist(vertex (routing-neighbours node))
               (add-route table vertex))))))

(defun node-position(node nodes)
  (if (eql nodes (node:nodes))
      (uid node)
      (position (node (vertex-start vertex)) nodes)))

(defmethod add-route((routing static-routing-table) vertex &key &allow-other-keys)
  (with-slots(costs vertices nodes) table
    (let ((i (node-position (node (vertex-start vertex)) nodes))
          (j (node-position (node (vertex-end vertex)) nodes)))
      (setf (aref vertices i j) vertex
            (aref costs i j) (funcall cost-fn
                                      (vertex-start vertex)
                                      (vertex-end vertex))))))

(defun static-table-find-route(from to table)
  (with-slots(vertices nodes) table
    (let ((i (node-position from nodes))
          (j (node-position to nodes)))
      (when (and i j) (aref vertices i j)))))

(defun static-table-find-routes(source table)
  "use Dijkstra to generate shortes routes from source"
  (with-slots(costs vertices nodes) table
    (multiple-value-bind(previous routecosts)
        (dijkstra (node-postion source nodes) (length nodes)
                  :cost-fn #'(lambda(i j) (aref costs i j))
                  :edges-fn #'(lambda(i)
                                (let ((acc nil))
                                  (dotimes(j (length nodes))
                                    (when (aref vertices i j)
                                      (push j acc)))
                                  acc)))
      (dotimes(i (length nodes))
)

(defmethod find-route((node node) (routing static-routing-table) &optional packet)
  (declare (ignore packet))
  (static-table-find-route (node routing) node (routing-table routing)))

(defclass routing-manual(routing)
  ((routing-table :initarg :table :reader routing-table :allocation :class))
  (:documentation "The static routing class. This routing object
calculates all the routes needed at a single instance. Static routing
is simple and easy to use by the user, but is memory intensive.  It is
recommended for simulation topologies on the order of a few hundred
Larger topologies should use either manual or nix-vector routing."))

(defmethod add-route((routing routing-manual) vertex &key &allow-other-keys)
  (assert (eql (node routing) (node (vertex-start vertex))))
  (add-route (routing-table routing) vertex))

(defmethod find-route((node node) (routing routing-manual) &optional packet)
  (find-route node (routing-table routing) packet))

(defclass routing-static(routing-manual)
  ()
  (:documentation "The static routing class. This routing object
calculates all the routes needed at a single instance. Static routing
is simple and easy to use by the user, but is memory intensive.  It is
recommended for simulation topologies on the order of a few hundred
Larger topologies should use either manual or nix-vector routing."))

(defmethod find-route((node node) (routing routing-static) &optional packet)
  (or
   (static-table-find-route (node routing) node (routing-table routing))
   (progn
     (static-table-find-routes node (routing-table routing))
     (static-table-find-route (node routing) node (routing-table routing)))))

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
