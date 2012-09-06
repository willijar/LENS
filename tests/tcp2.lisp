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

;; configure Layer 4 tracing
;(setf (trace-status (node 0) *lens-trace-output*) :default)
;(setf (trace-status (node 5) *lens-trace-output*) :default)
(setf (trace-status 4 *lens-trace-output*) :enabled)

;; notify routing of new topology
(topology-changed (nodes))

;; put in sources and sinks and appropriate monitoring

(defparameter server
  (make-instance
   'message-responder
   :local-port 20000
   :node (node 5)
   :response-statistics (make-instance 'average-min-max)))

(defparameter client
  (make-instance
   'message-source
   :node (node 0)
   :data-size 100
   :response-size 100
   :loop-count 1
   :peer-address (ipaddr (node 5))
   :peer-port 20000))

(start server)
(start client)

;; Always schedule simulation to stop at some time
(schedule 100 #'stop-simulation)