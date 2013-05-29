(in-package :lens.wsn)

(defclass node(compound-module)
  ()
  (:gates
   (toWirelessChannel :output)
   (toPhysicalProcess :output 0)
   (fromWirelessChannel :input)
   (fromPhysicalProcess :input 0))
  (:submodules
   (mobility mobility)
   (resources resources)
   (sensors sensors)
  (:connections

  (:metaclass compound-module-class)