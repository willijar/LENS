(in-package :lens)

(defclass Txc1(module)
  ()
  (:gates
   (in :input)
   (out :output))
  (:metaclass module-class))

(defmethod initialize((module Txc1) &optional (stage 0))
  (when (and (zerop stage) (eql (name module) 'tic))
    (send module (make-instance 'message :name 'TicTocMsg) 'out))
  t)

(defmethod handle-message((module Txc1) msg)
  (send module msg 'out))

(defclass TicToc1(network)
  ()
  (:submodules
   (tic Txc1)
   (toc Txc1))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))
