;; Some user helper macros
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :lens-user)

(defmacro with-new-instances((&rest defs) &body body)
  "Macro lexically binding variables to instances created using
make-instance.  over the body. defs is a list of definitions. The
first element of the definition is a variable name or a list of
variable names, each bound to a new instance. The rest of the def is
the arguments to pass to make-instance for each variable name."
  `(let (,@(mapcan #'(lambda(def)
			(mapcar
			 #'(lambda(name)
			     `(,name (make-instance ,@(rest def))))
			 (if (listp (car def)) (car def) (list (car def)))))
		     defs))
    ,@body))

(defmacro def-new-instances((&rest defs) &body body)
  "Globall bind variables to instances created using
make-instance.  over the body. defs is a list of definitions. The
first element of the definition is a variable name or a list of
variable names, each bound to a new instance. The rest of the def is
the arguments to pass to make-instance for each variable name."
  `(progn
     ,@(mapcan #'(lambda(def)
		   (mapcar
		    #'(lambda(name)
			`(defparameter ,name (make-instance ,@(rest def))))
		    (if (listp (car def)) (car def) (list (car def)))))
	       defs)
     ,@body
     (values ,@(mapcan
		#'(lambda(def)
		    (if (listp (car def)) (car def) (list (car def))))
		defs))))

(defun connect(graph &rest args)
  (dolist(row graph)
    (let ((src (first row)))
      (unless (typep src 'node) (setf src (node src)))
      (dolist(dst (rest row))
        (unless (typep dst 'node) (setf dst (node dst)))
        (apply #'point-to-point src dst args)))))

(defun start-simulation(&key (granularity 10000) step)
  "Start or restart the simulation scheduler. If the optional `foreground`
argument is true the scheduler will run in the current thread, otherwise it
will run on a backgound thread."
  ;; should check for stop-simulation on scheduler
  (scheduler::run (scheduler) :granularity granularity :step step))

(defun run(&optional time)
  (setf *user-output* *standard-output*)
  (when time (schedule time #'stop-simulation))
  (start-simulation)
  (values))

(defun stop-simulation()
  (scheduler::stop (scheduler) :abort t)
  (format *user-output* "~%-- Simulation stopped at ~,4f~%" (simulation-time)))

(defun load-test(name)
  (clear-nodes)
  (reset :all)
  (load (merge-pathnames (make-pathname :name name :type "lisp")
                         #.(asdf:system-relative-pathname :lens "tests/"))
        :verbose nil :print nil))

;;; topology generation functions and helpers

(defun make-nodes(n &optional (ipaddrs ipaddr-allocator))
  (loop :for i :from 1 :upto n
        :collect (make-instance 'node
                                :ipaddr (when ipaddrs (funcall ipaddrs)))))

(defun dumbell-topology(n-left n-right &key
                        (ipaddrs  ipaddr-allocator)
                        (left-ipaddrs ipaddrs)
                        (right-ipaddrs left-ipaddrs)
                        (bandwidth *default-bandwidth*)
                        (bottleneck-multiplier 1.0))
  "Create nodes in a dumbell topology where some number of nodes on
the left and right side of the topology communicate through a single
bottleneck link. Creates `n-left` nodes on the left hand side and
`n-right` nodes on the right connected to a left and right router
respectively using the specified link-type. The ip addresses will
be allocated successively using left-ipaddrs and right-ipaddrs.
bottleneck-multiplier is the ratio of the bandwidth of the
bottlenect link to the other links.

Arguments:
- `n-left`: an integer, the number of nodes on the left side of the
- `n-right`: an integer
- `ipaddrs`: an ipaddr-allocator (default default ipaddr-allocator)
- `left-ipaddrs`: an ipaddr-allocator (default ipaddrs)
- `right-ipaddrs`: an ipaddr-allocator (default left-ipaddrs)
- `bandwidth`: bandwidth of leaf node links
- `bottleneck-multiplier`: multiplier for bottleneck bandwidth

Results:

- `left-nodes`: a list of nodes
- `left-router`: a node
- `right-router`: a node
- `right-nodes`: a list of nodes"
  (let* ((left-router
          (make-instance 'node  :network-address (funcall left-ipaddrs)))
         (left-nodes (make-nodes n-left left-ipaddrs))
         (right-router
          (make-instance 'node :network-address (funcall right-ipaddrs)))
         (right-nodes (make-nodes n-right right-ipaddrs)))
    (dolist(n left-nodes)
      (point-to-point n left-router :bandwidth bandwidth))
    (dolist(n right-nodes)
      (point-to-point right-router n :bandwidth bandwidth))
    (point-to-point left-router right-router
                    :bandwidth (* bandwidth bottleneck-multiplier))
  (values left-nodes left-router right-router right-nodes)))

(defun star-topology(n-leaf &key (bandwidth *default-bandwidth*) node
                     (ipaddrs ipaddr-allocator))
  "Create nodes in a star topology. `n-leaf` is the number of leaf
nodes, `node` is an existing node to be used as the core - if nil
a new node will be created as core. The leaf nodes are connected to
the core node using links with the specified bandwidth. ipaddresses are
allocated using `ipaddrs`.

Arguments:

- `n-leaf`: an integer
- `ipaddrs`: an ipaddr-allocator
- `bandwidth`:  bandwidth of leaf node links
- `node`: a node

Results:

- `leaf-nodes`: a list of nodes
- `core-node`: a node"
  (let ((core (or node (make-instance 'node
                                      :network-address (funcall ipaddrs))))
        (leaves (make-nodes n-leaf ipaddrs)))
    (dolist(n leaves)
      (point-to-point n core :bandwidth bandwidth))
    (values leaves core)))

;; data analysis

(defun save-data(path data)
  (with-open-file(os path :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create)
    (dolist(row data)
      (dolist(n row) (format os " ~12f" n))
      (terpri os))))

(defun linspace(start end &optional (steps 100))
  (loop :for x :from start :to end :by (/ (- end start) steps) :collect x))