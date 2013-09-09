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
  ((name :accessor command :initarg :command
         :documentation "Command is held as message name")
    (argument :accessor argument :initarg :argument
             :documentation "Additional arguments with command"))
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


(defmethod handle-message((instance application)
                          (message communications-control-message))
  ;; by default applications can ignore control messages
)

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

(define-condition invalid-command(warning)
  ((module :reader module :initarg :module)
   (command :reader command :initarg :command)
   (argument :reader argument :initarg :argument))
  (:report (lambda(c os)
             (format os "Invalid command ~A on ~A with argument ~A"
                     (command c) (module c) (argument c)))))

(defgeneric handle-control-command(module command argument)
  (:documentation "Breakdown of MAC commands - return true if successful")
  (:method(module command argument)
    (warn 'invalid-command :module module :command command :argument argument)))
