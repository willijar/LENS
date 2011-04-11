;;; A simple simulation testing dropping of udp packets due to a short queue
(in-package :lens-user)

(clear-nodes)
(reset :all)

;; make 6 nodes with successive ip addresses
(make-nodes 6)
(setf (trace-status (node 0) *lens-trace-output*) :enabled)
(setf (trace-status (node 5) *lens-trace-output*) :enabled)
;; connect them all in a row using default links
(connect '((0 1)
           (1 2)
           (2 3)
           (3 4)
           (4 5)))

;; notify routing of new topology
(topology-changed (nodes))

;; put udp layer at either end and send 100 kbytes from one to the other
(defparameter udp0
  (make-instance 'udp
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

(setf (layer1:limit-bytes
       (layer1:packet-queue (aref (interfaces (node 0)) 0)))
      1000)

(protocol:send udp0 2500 nil)

(start sink)

(schedule 10 #'stop-simulation)

;; run the scheduler
;;(start-simulation t)


