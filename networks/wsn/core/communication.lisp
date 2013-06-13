(in-package :lens.wsn)

(defclass communications(compound-module)
  ()
  (:gates
   (application :inout)
   (receive :input))
  (:submodules
   (routing routing)
   (mac mac)
   (radio radio))
  (:connections
   (<=> application (routing application))
   (<=> (routing mac) (mac routing))
   (<=> (mac radio) (radio mac))
   (<= (radio receive) receive))
  (:metaclass compound-module-class)
  (:documentation "Communications module"))

(defclass communications-control-message(message)
  ()
  (:documentation "Base class for all communications control
  messages (information going up to higher layers). We create classes
  for these so the layers can specialize handle-message on them and
  pick out those they need"))

(defclass network-control-message(communications-control-message)
  ())

(defclass mac-control-message(communications-control-message)
  ())

(defclass radio-control-message(communications-control-message)
  ())

(defclass communications-control-command(message)
  ((name :accessor command :initarg :command
         :documentation "Command is held as message name")
   (argument :accessor argument :initarg :argument
             :documentation "Additional arguments with command"))
  (:documentation "Base class for all communications control
  commands (commands going down to lower layers."))

(defclass network-control-command(communications-control-command)
  ())

(defclass mac-control-command(communications-control-command)
  ())

(defclass radio-control-command(communications-control-command)
  ())
