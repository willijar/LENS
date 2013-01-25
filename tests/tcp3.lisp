;;; A simple constant bit rate simulation testing udp implementation and
;;; routing through a simple chaing of nodes.

(in-package :lens-user)

;; always start tests by clearing nodes and reseting all
(clear-nodes)
(reset :all)

;; then set up topology
;; make 2 nodes with successive ip addresses
(make-nodes 5)
;; connect them all in a row using default links
(connect '((0 1) (1 2) (2 3) (3 4)))

;; configure tracing
;;(setf (trace-status (node 0) *lens-trace-output*) :default)
;;(setf (trace-status (node 2) *lens-trace-output*) :default)
(setf (trace-status 4 *lens-trace-output*) :enabled)

;; notify routing of new topology
(topology-changed (nodes))

;; put in sources and sinks and appropriate monitoring

(defparameter server
  (make-instance
   'message-responder
   :local-port 20000
   :node (node 4)
   :response-statistics (make-instance 'average-min-max)))

(defparameter client
  (make-instance
   'message-source
   :node (node 0)
   :data-size 8000000
   :response-size 100
   :loop-count 1
   :peer-address (ipaddr (node 4))
   :peer-port 20000))

(start server)
(start client)

;; Always schedule simulation to stop at some time
;;(schedule 800 #'stop-simulation)

(start-simulation :granularity nil)
