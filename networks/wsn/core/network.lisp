(in-package :lens.wsn)

(defclass WSN(Network)
  ((field :parameter t
          :type coord :reader field
          :initform (make-coord 30.0d0 30.0d0)
          :documentation "Size of deployment field")
   (num-nodes :parameter t :type fixnum :reader num-nodes
              :initform 30 :documentation "Number of nodes")
   (num-physical-processes :parameter t :type fixnum
                           :reader num-physical-processes :initform 0
                           :documentation "Number of physical processes")
   (deployment :parameter t :type list :reader deployment :initform 'uniform
               :properties (:format read)
               :documentation "Node deployment spec"))
  (:metaclass compound-module-class)
  (:submodules
   ;; order matters here as nodes config depends on physical processes
   (wireless-channel wireless-channel)
   (physical-processes num-physical-processes physical-process)
   (node num-nodes node)))

(defgeneric nodes(network)
  (:method((network WSN)) (submodule network 'node)))