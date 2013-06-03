(in-package :lens.wsn)

(defclass SensorNetwork(Network)
  ((field :parameter t
          :type coord :reader field
          :initform (make-coord :x 30.0d0 :x 30.0d0 :z 0.0d0)
          :documentation "Size of deployment field")
   (num-nodes :parameter t :type fixnum :reader num-nodes
              :initform 30 :documentation "Number of nodes")
   (deployment :parameter t :type list :reader deployment :initform 'uniform
               :properties (:format 'read)
               :documentation "Node deployment spec"))
  (:metaclass compound-module-class)
  (:submodules
   (wireless-channel wireless-channel)
   (nodes num-nodes node)
   (physical-processes 0 physical-process)))