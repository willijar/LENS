;;; A simple constant bit rate simulation testing udp implementation and
;;; routing through a simple chaing of nodes.

(in-package :lens-user)

;; always start tests by clearing nodes and reseting all
(clear-nodes)
(reset :all)

;; then set up topology
;; make 6 nodes with successive ip addresses
(make-nodes 6)
;; connect them all in a row using default links
(connect '((0 1)
           (1 2)
           (2 3)
           (3 4)
           (4 5)))

;; configure tracing
(setf (trace-status (node 0) *lens-trace-output*) :enabled)
(setf (trace-status (node 5) *lens-trace-output*) :enabled)

;; notify routing of new topology
(topology-changed (nodes))

;; put in sources and sinks and appropriate monitoring
(defparameter src
  (make-instance
   'abr-source
   :name "source"
   :node (node 0)
   :peer-address (ipaddr (node 5))
   :peer-port 20000))

(defparameter sink
  (make-instance
   'udp-sink
   :name "sink"
   :node (node 5)
   :delay-statistics (make-instance 'average-min-max)
   :bandwidth-statistics (make-instance 'average-min-max)
   :local-port 20000))

(start src)
(start sink)

;; Always schedule simulation to stop at some time
(schedule 25 #'stop-simulation)