(in-package :lens-user)

;; plot a graph of delay vs utilisation

;; (utilisation link)
;; (loss-statistics udp-sink)
;; (delay-statistics udp-sink)
;;

(clear-nodes)
;(reset :all)

(defparameter *n* 100 "Number sources")
(defparameter *cbr-rate* 95000 "CBR bit rate for each source")

(defparameter *start-rng* (random-variable 'uniform 0 0.1)
  "Random variable for start times")
(setf *default-bandwidth* 10e6)
(setf *default-delay* 10e-3)
(setf *default-link* '(point-to-point))

(make-nodes 2)
(connect '((0 1)))
(topology-changed)

(defparameter link (link (aref (interfaces (node 0)) 0)))

(defparameter sources (loop :for x :from 1 :to *n*
                            :collect
                            (make-instance
                             'cbr-source
                             :node (node 0)
                             :rate *cbr-rate*
                             :peer-address (ipaddr (node 1))
                             :peer-port 12345)))

(dolist(src sources) (schedule (random-value *start-rng*) `(start ,src)))

(defparameter sink (make-instance
                    'udp-sink
                    :node (node 1)
                    :local-port 12345
                    :delay-statistics
                    (make-instance 'average-min-max)
                    :bandwidth-statistics
                    (make-instance 'average-min-max)
                    :loss-statistics
                    (make-instance 'average-min-max)))

(start sink)

;(schedule 1d0 `(link::reset-utilisation ,link))

(schedule 10d0
#'(lambda()
    (stop-simulation)
    (format *user-output* "utilisation=~A loss=~A delay=~A~%"
            (utilisation link)
            (average (loss-statistics sink))
            (average (delay-statistics sink)))))
