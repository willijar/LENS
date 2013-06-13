(in-package :lens.wsn)

(defclass WSN(Network)
  ((field :parameter t
          :type coord :reader field
          :initform (make-coord 30.0d0 30.0d0)
          :documentation "Size of deployment field")
   (num-nodes :parameter t :type fixnum :reader num-nodes
              :initform 30 :documentation "Number of nodes")
   (deployment :parameter t :type list :reader deployment :initform 'uniform
               :properties (:format 'read)
               :documentation "Node deployment spec"))
  (:metaclass compound-module-class)
  (:submodules
   (wireless-channel wireless-channel)
   (node num-nodes node)
   (physical-processes 0 physical-process)))

(defgeneric nodes(network)
  (:method((network WSN)) (submodule network 'node)))