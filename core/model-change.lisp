(in-package :lens)

(defclass model-change-notification()
  ()
  (:documentation " Common base class for data objects that accompany PRE_MODEL_CHANGE and POST_MODEL_CHANGE notifications (signals).)"))

(defmacro def-change-notification(name (&optional (classes '(model-change-notification))) fields &rest rest)
  `(defclass ,name(,@classes)
     (,@(mapcar
         #'(lambda(field)
             (list field
                   :initarg (intern (string field) :keyword)
                   :reader (intern (format nil "NOTIFICATION-~A" field))))
         fields))
     ,@rest))

(def-change-notification pre-module-add-notification()
  (module-type module-name parent-module vector-size index)
  (:documentation " Fired at the top of cModuleType::create(); fields
  contain the cModuleType object, and the arguments of the create()
  method call."))

(def-change-notification pre-module-delete-notification()
  (module)
  (:documentation "Fired at the top of cModule::deleteModule(). The module still exists at this point."))

(def-change-notification post-module-delete-notification()
  (module parent-module vector-size index)
  (:documentation "Fired at the end of cModule::deleteModule(). The
  module object no longer exists at this point, and its submodules
  have also been deleted."))

(def-change-notification pre-module-reparent-notification()
  (module new-parent-module)
  (:documentation "Fired at the top of cModule::changeParentTo(), before any changes have been done."))

(def-change-notification post-module-reparent-notification()
  (module old-parent-module)
  (:documentation "Fired at the end of cModule::changeParentTo()."))

(def-change-notification pre-gate-add-notification()
  (module gate-name gate-type vector-p)
  (:documentation "This notification is fired at the top of cModule::addGate(), that is, when a gate or gate vector is added to the module.

Note: this notification is fired for the gate or gate vector as a
whole, and not for individual gate objects in it. That is, a single
notification is fired for an inout gate (which is a gate pair) and
for gate vectors as well.

Fields in this class carry the module object on which the gate or gate vector
being created, and the arguments of the addGate() method call."))

(def-change-notification post-gate-add-notification()
  (module gate-name)
  (:documentation "This notification is fired at the bottom of
  cModule::addGate(), that is, when a gate or gate vector was added to
  the module.

Note: this notification is fired for the gate or gate vector as a
whole, and not for individual gate objects in it. That is, a single
notification is fired for an inout gate (which is a gate pair) and for
gate vectors as well.

Fields in this class carry the module object on which the gate or gate
vector was created, and the name of the gate or gate vector."))

(def-change-notification pre-gate-delete-notification()
  (module gate-name)
  (:documentation

(def-change-notification post-gate-delete-notification()
  (module gate-name gate-type vector-p)
  (:documentation "Fired at the end of cModule::deleteGate(). The gate
  or gate vector no longer exists at this point.

Note: this notification is fired for the gate or gate vector as a
whole, and not for individual gate objects in it. That is, a single
notification is fired for an inout gate (which is a gate pair) and for
gate vectors as well."))

(def-change-notification pre-gate-vector-resize-notification()
  (module gate-name new-size)
  (:documentation "Fired at the top of cModule::setGateSize(). Note
  that other cModule methods used for implementing the NED gate++
  syntax also expand the gate vector, and fire this
  notification. These methods are getOrCreateFirstUnconnectedGate()"))

(def-change-notification post-gate-vector-resize-notification()
  (module gate-name old-size)
  (:documentation "Fired at the end of cModule::setGateSize(). Note
  that other cModule methods used for implementing the NED \"gate++\"
  syntax also expand the gate vector, and fire this
  notification. These methods are getOrCreateFirstUnconnectedGate()
  and getOrCreateFirstUnconnectedGatePair())."))

(def-change-notification pre-gate-connect-notification()
  (gate target-gate channel)
  (:documentation "This notification is fired at the top of
cGate::connectTo().  This notification is fired on the module that
contains the source gate. of the connection. If you wish to listen on
the target gate of the connection being connected, you should add the
listener to the parent module (as notifications propagate up)."))

(def-change-notification post-gate-connect-notification()
  (gate)
  (:documentation "This notification is fired at the end of cGate::connectTo(), to announce that a connection between the given gate and its peer (gate->getNextGate()) has been created.

This notification is fired on the module that contains the source
gate.  of the connection. If you wish to listen on the target gate of
the connection being connected, you should add the listener to the
parent module (as notifications propagate up)."))

(def-change-notification pre-gate-disconnect-notification()
  (gate)
  (:documentation "This notification is fired at the top of
  cGate::disconnect(), to announce that the connection between the
  given gate and its peer (gate->getNextGate()) is about to be
  deleted.

This notification is fired on the module that contains the source gate
of the connection. If you wish to listen on the target gate of the
connection being disconnected, you should add the listener to the
parent module (as notifications propagate up)."))

(def-change-notification post-gate-disconnect-notification()
  (gate target-gate channel)
  (:documentation "This notification is fired at the end of cGate::disconnect(), to announce that the connection between the given gates has been deleted.
This notification is fired on the module that contains the source gate.
of the connection. If you wish to listen on the target gate of the
connection being disconnected, you should add the listener to the
 parent module (as notifications propagate up)."))

(def-change-notification path-change-notification()
  (start-gate end-gate changed-gate)
  (:documentation "Base class for path change notifications. Like gate
  connect/disconnect notifications, they are fired when a gate is
  connected or disconnected; the difference is that path change
  notifications are fired on the owner modules of the start AND end
  gates of the path that contains the connection (two notifications!),
  NOT on the module of the gate being connected or disconnected. See
  also cGate's getPathStartGate() and getPathEndGate() methods.

The purpose of this notification is to make it possible to get away
with only local listeners in simple modules. If this notification
didn't exist, users would have to listen for gate connect/disconnect
notifications at the top-level module, which is not very efficient (as
ALL pre/post model change events from all modules would then have to
be propagated up to the top)."))

(def-change-notification pre-path-create-notification(path-change-notification)
  ()
  (:documentation "This notification is fired at the end of
  cGate::connectTo() on the owner modules of the start AND end gates
  of the connection path that was formed when the gate was
  connected. See cPathChangeNotification for more details."))

(def-change-notification post-path-create-notification(path-change-notification)
  ()
  (:documentation "This notification is fired at the end of
  cGate::connectTo() on the owner modules of the start AND end gates
  of the connection path that was formed when the gate was
  connected. See cPathChangeNotification for more details."))

(def-change-notification pre-path-cut-notification(path-change-notification)
  ()
  (:documentation "This notification is fired at the top of
  cGate::disconnect() on the owner modules of the start AND end gates
  of the connection path that is about to be cut when the gate gets
  connected. See cPathChangeNotification for more details."))

(def-change-notification post-path-cut-notification(path-change-notification)
  ()
  (:documentation "This notification is fired at the end of
  cGate::disconnect() on the owner modules of the start AND end gates
  of the connection path that was cut when the gate got
  disconnected. See cPathChangeNotification for more details."))

(def-change-notification pre-parameter-change-notification()
  (module parameter-name)
  (:documentation "This notification is fired before a module or
  channel parameter value was changed."))

(def-change-notification post-parameter-change-notification()
  (module parameter-name)
  (:documentation "This notification is fired post a module or channel
  parameter value was changed."))