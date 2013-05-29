(in-package :lens.wsn)

(defclass SensorNetwork(Network)
  ((field :parameter t
          :type coord :reader field :initform #S(coord :x 30 :x 30 :z 0)
          :documentation "Size of deployment field")
   (num-nodes :parameter t :type fixnum :documentation "Number of nodes")
   (deployment :type list :reader deployment :initform nil
               :documentation "Node deployment spec"))
  (:metaclass compound-module-class)
  (:submodules
   (wireless-channel wireless-channel num