(in-package :lens-user)

(clear-nodes)
(reset :all)

(defparameter *n-leaf* 5 "Number leaf nodes for dumbell")
(defparameter *n-star* 5 "Number leaf nodes for each star")
(defparameter *cbr-rate* 250000 "CBR bit rate for each source")
(defparameter *start-rng* (random-variable 'uniform 0 0.1)
  "Random variable for start times")
(setf *default-bandwidth* 10e6)
(setf *default-delay* 10e-3)
(setf *default-link* '(point-to-point))

(defparameter left-router nil)
(multiple-value-bind(left-nodes left-router right-router right-nodes)
    (dumbell-topology *n-leaf* *n-leaf* :bottleneck-multiplier 0.1)
  (map 'nil
       #'(lambda(left right)
           ;; for each leaf on dumbell create a star
           (let ((sl (star-topology *n-star* :node left :ipaddrs
                                    (ipaddrs (ipaddr left) 256)))
                 (sr (star-topology *n-star* :node right :ipaddrs
                                    (ipaddrs (ipaddr right) 256))))
             (map 'nil
                  #'(lambda(left right)
                  ;; for each leaf on each star connect set up
                  ;; respective applications and sinks
                      (let ((src (make-instance
                                  'cbr-source
                                  :node left
                                  :rate *cbr-rate*
                                  :peer-address (ipaddr right)
                                  :peer-port 12345))
                            (sink (make-instance
                                   'udp-sink
                                   :node right
                                   :port 12345
                                   :delay-statistics
                                   (make-instance 'average-min-max)
                                   :bandwidth-statistics
                                   (make-instance 'average-min-max))))
                        (start sink)
                        (schedule (random-value *start-rng*) `(start ,src))))
                  sl sr)))
       left-nodes right-nodes))

;; notify routing of new topology
(topology-changed)

;; schedule simulation to stop after 100 seconds
(schedule 100 #'stop-simulation)
