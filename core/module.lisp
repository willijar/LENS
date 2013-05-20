(in-package :lens)

(defgeneric build-gates(instance)
  (:documentation "Initialise gates on instance using gatespec in class."))

(defgeneric build-submodules(instance)
  (:documentation "Build submodules inside a compound module"))

(defgeneric build-connections(instance)
  (:documentation "Buildc connectiuons between submodules and gates in
  a compound module"))

(defgeneric build-inside(instance)
  (:documentation "create submodules and connect them"))

(defgeneric arrived(module message gate time)
  (:documentation "Called when a message arrives at a gate which is not further
    connected (that is, next-gate is NULL)"))

(defgeneric send(from-module message gateid &key delay)
  (:documentation "Sends a message through the gate with given delay"))

(defgeneric send-direct(from-module
                        togate message &key propagation-delay duration)
  (:documentation "Send a message directly to another module.

 If the target gate is further connected (i.e. getNextGate()!=NULL),
 the message will follow the connections that start at that gate.  For
 example, when sending to an input gate of a compound module, the
 message will follow the connections inside the compound module.

 It is permitted to send to an output gate, which will also cause the
 message to follow the connections starting at that gate.  This can be
 useful, for example, when several submodules are sending to a single
 output gate of their parent module.

 It is not permitted to send to a gate of a compound module which is
 not further connected (i.e. getNextGate()==NULL), as this would cause
 the message to arrive at a compound module.

 Also, it is not permitted to send to a gate which is otherwise
 connected i.e. where getPreviousGate()!=NULL. This means that modules
 MUST have dedicated gates for receiving via sendDirect(). You cannot
 have a gate which receives messages via both connections and
 sendDirect().

 When a nonzero duration is given, that signifies the duration of the
 packet transmission, that is, the time difference between the
 transmission (or reception) of the start of the packet and that of
 the end of the packet.  The destination module can choose whether it
 wants the simulation kernel to deliver the packet object to it at the
 start or at the end of the reception. The default is the latter; the
 module can change it by calling setDeliverOnReceptionStart() on the
 final input gate (that is, on
 inputGate->getPathEndGate()). setDeliverOnReceptionStart() needs to
 be called in advance, for example in the initialize() method of the
 module.  When a module receives a packet, it can call the
 isReceptionStart() and getDuration() methods on the packet to find
 out whether it represents the start or the end of the reception, and
 the duration of the transmission.

 For messages that are not packets (i.e. not subclassed from cPacket),
 the duration parameter must be zero."))

(defgeneric schedule-at(module message &key time)
  (:documentation
"Schedules a self-message. It will be delivered back to the module
 via  handle-message() at simulation time. This method
 is the way you can implement timers or timeouts. Timers can also
 be cancelled via cancelEvent() (See below.)

 When the message is delivered at the module, you can call
 <tt>msg->isSelfMessage()</tt> to tell it apart from messages arriving
 from other modules. <tt>msg->getKind()</tt> can be used to further
 classify it, or of you need to manage an unbounded number of timers,
 you can set <tt>msg->getContextPointer()</tt> before scheduling to
 point to the data structure the message belongs to -- this way
 you can avoid having to search through lists or other data structures
 to find out where a just-arrived self-message belongs.

 cancelEvent() can be used to cancel the self-message before it arrives.
 This is useful for implementing timeouts: if the event occurs 'in time'
 (before timeout), the scheduled self-message can be cancelled.

 Given a cMessage pointer, you can check whether it is currently
 scheduled by calling <tt>msg->isScheduled()</tt>. If it is scheduled,
 you cannot schedule it again without calling cancelEvent() first.
 However, after the message was delivered to the module or cancelled,
 you can schedule it again -- so you can reuse the same message
 object for timeouts over and over during the whole simulation."))

(defclass module-class(parameter-class)
  ((%gatespec
    :type list :initform nil
    :documentation "Gate specification used to build gates for this class"))
  (:documentation "Metaclass for entities with gates"))

;; metaclass inheritance helper functions
(defun named-specifications-inheritance
    (localspecs direct-superclasses classtype
  classslot)
  "Given a local specification and the direct superclasses and class
slot names check than we are not overwriting based on name (first
element of specs). Return merged list of specification. Checks on
validity of format of localspec should occur before calling this
function."
  (let ((specs (copy-list localspecs)))
    (loop :for superclasses :on direct-superclasses
       :for classa = (car superclasses)
       :when (typep classa classtype)
       :do
       (let ((i (intersection localspecs (slot-value classa classslot)
                              :key #'first)))
         (assert (not i)
                 ()
                 "~A specification conflict ~A from superclass ~A" classslot
                 i classa)
         (loop :for remainder :on (rest superclasses)
            :for classb = (car remainder)
            :when (typep classb classtype)
            :do
            (let ((i (intersection (slot-value classa classslot)
                                   (slot-value classb classslot)
                                   :key #'first)))
              (assert (not i)
                      ()
                      "~A specification conflict ~A between superclasses ~A and ~A"
                      classslot i classa classb)))
         (setf specs (append specs (slot-value classa classslot)))))
    specs))

(defun merge-local-typespec(spec class required-type)
  (let ((type (first spec))
        (args (rest spec)))
    (let ((classspec (find type (slot-value class '%localtypes) :key #'first)))
      (when classspec
        (setf type (first classspec)
              args (append args (rest classspec)))))
    (assert (subtypep type required-type)
            ()
            "Specified type ~A is not a ~A as required." type required-type)
    (cons type args)))

(defmethod initialize-instance :after
     ((class module-class) &key gates &allow-other-keys)
  "Necessary to ensure gates is on allowed initialization list"
  (declare (ignore gates)))

(defmethod shared-initialize :after
    ((class module-class) slot-names
     &key gates direct-superclasses &allow-other-keys)
  ;; check gates syntax at class initialization time.
  (dolist(spec gates)
    (assert (and (typep (first spec) 'symbol)
                 (typep (second spec) 'gate-direction)
                 (typep (third spec)  '(or function number symbol null)))
            ()
            "Invalid gate specification ~A" spec))
  (setf (slot-value class '%gatespec)
        (named-specifications-inheritance
         gates direct-superclasses 'module-class '%gatespec)))

(defmethod sb-mop:validate-superclass ((class module-class)
                                       superclass)
  (typep superclass '(or standard-class parameter-class)))

(defclass module(component)
  ((gate-slots
    :type hash-table :initform (make-hash-table) :reader gate-slots))
  (:metaclass module-class))

(defgeneric from-gate(message)
  (:method((message message))
    (let ((from (slot-value message 'from)))
      (when (typep from 'gate) from))))

(defgeneric to-gate(message)
  (:method((message message))
    (let ((to (slot-value message 'to)))
      (when (typep to 'gate) to))))

(defgeneric from-module(message)
  (:method((message message))
    (let ((from (slot-value message 'from)))
      (if (typep from 'module)
          from
          (parent-module from)))))

(defgeneric to-module(message)
  (:method((message message))
    (let ((to (slot-value message 'to  )))
      (if (typep to 'module)
          to
          (parent-module to)))))

(defun self-message-p(message)
  (eql (slot-value message 'from) (slot-value  message 'to)))

(defmethod configuration(object)
  "Default to simulation configuration for all objects"
  (declare (ignore object))
  (configuration *simulation*))

(defmethod initialize-instance :after ((module module) &key &allow-other-keys)
  (build-gates module))

(defmethod build-gates((module module))
  (dolist(spec (slot-value (class-of module) '%gatespec))
    (let ((size (third spec)))
      (setf (gethash (first spec) (gate-slots module))
            (make-instance
             'gate-slot
             :owner module
             :name (first spec)
             :direction (second spec)
             :initial-size (etypecase size
                            (null nil)
                            (number size)
                            (symbol (slot-value module size))
                            (function (funcall size module))))))))

(defun for-each-gate(module operator)
  (labels((over(slot direction)
            (let ((v (funcall direction slot)))
              (when v
                (if (vectorp v) (map nil operator v) (funcall operator v))))))
    (maphash
   #'(lambda(k v)
       (declare (ignore k))
       (over v #'input)
       (over v #'output))
   (gate-slots module))))

(defmethod gate((module module) (name symbol) &key direction index)
   (let ((slot (gethash name (gate-slots module))))
     (when slot (gate slot direction :index index))))

(defmethod gate((module module) (gateid list)
                &key direction (index (second gateid)))
  "Allow gate arrays to be indexed by list of name and index"
   (let ((slot (gethash (first gateid) (gate-slots module))))
     (when slot (gate slot direction :index index))))

(defmethod for-each-child((module module) (operator function))
  (call-next-method)
  (for-each-gate module operator))

(defmethod arrived ((module module) (message message) (gate gate) time)
  (setf (to message) gate)
  (let ((onstart (deliver-on-reception-start gate)))
    (if (typep message 'packet)
        (setf (reception-start-p message) onstart
              (arrival-time message)
              (if onstart time (+ time (duration message))))
        (setf (arrival-time message) time)))
  (schedule message))

(defmethod send((from-module module) (message message) (outgate gate)
                &key (delay 0))
  (assert (next-gate outgate))
  (assert (not (eql (gate-direction outgate) :input)))
  (assert (eql (owner message) from-module))
  (assert (eql (parent-module outgate) from-module))
  (assert (not (scheduled-p message)))
  (assert (>= delay 0))
  (when (typep message 'packet) (setf (duration message) 0))
  (let ((delay-end-time (+ (simulation-time) delay)))
    (setf (from message) outgate
          (sent-time message) delay-end-time)
    (deliver message outgate delay-end-time)))

(defmethod send((module module) (message message) gateid &key (delay 0))
  (send module message (gate module gateid :direction :output) :delay delay))

(defmethod send-direct(from-module (to-gate gate) (message message)
                       &key (propagation-delay 0) (duration 0))
  (assert (not (previous-gate to-gate)))
  (assert (not (eql (gate-direction to-gate) :output)))
  (assert (and (>= propagation-delay 0) (>= duration 0)))
  (assert (eql (owner message) from-module))
  (assert (not (scheduled-p message)))
  (setf (from message) from-module
        (sent-time message) (simulation-time))
  (if (typep message 'packet)
      (setf (duration message) duration)
      (assert (zerop duration)))
  (deliver message to-gate (+ (simulation-time) propagation-delay)))

(defmethod schedule-at((module module) (message message) &key time)
  "Schedule a self message"
  (setf (from message) module
        (sent-time message) (simulation-time)
        (to message) module
        (arrival-time message) time)
  (schedule message))

(defclass compound-module-class(module-class)
  ((%localtypes :type list :initarg :types :initform nil
           :documentation "Specified local type mapping.")
   (%submodules :type list :initarg :submodules :initform nil
                :documentation "Submodule specifications")
   (%connections :type list :initarg :connections :initform nil
                 :documentation "Connection specification for this class"))
  (:documentation "Metclass for entities with gates"))

(defmethod sb-mop:validate-superclass ((class compound-module-class)
                                       superclass )
  (typep superclass '(or standard-class parameter-class module-class)))

(defmethod initialize-instance :after
     ((class compound-module-class)
      &key types submodules connections &allow-other-keys)
  "Necessary to ensure types, submodules and connections is on allowed
initialization list"
  (declare (ignore types submodules connections)))

(defmethod shared-initialize :after
    ((class compound-module-class) slot-names
     &key types submodules connections direct-superclasses &allow-other-keys)
  (dolist(spec types)
    (assert (and (symbolp (first spec)) (symbolp (second spec)) (cddr types))
            ()
            "Invalid local type specification ~A" spec))
  (setf (slot-value class '%submodules)
        (named-specifications-inheritance
         (mapcar
          #'(lambda(spec)
              (let ((name (first spec)))
                (assert (symbolp name)
                        ()
                        "Invalid submodule name ~A" name)
                (multiple-value-bind(sizespec typespec)
                    (let ((v (second spec)))
                      (if (or (numberp v)
                              (functionp v)
                              (and (third spec)
                                   (symbolp (third spec))
                                   (not (eql (symbol-package (third v))
                                             (find-package :keyword))))
                              (member v (slot-value class '%gatespec)
                                      :key #'first))
                          (values v (cddr spec))
                          (values nil (cdr spec))))
                  (cons name
                        (cons sizespec
                              (merge-local-typespec typespec class 'module))))))
          submodules)
         direct-superclasses
         'compound-module-class
         '%submodules))
  (setf (slot-value class '%connections)
        (connection-code-walk class connections)))

(defun gate-accessor(class spec direction)
  "Return a closure which when given the instance as an argument will
return the gate given by spec. Validates spec based on class definitions"
  (multiple-value-bind(modulename moduleindex name index)
      (etypecase spec
        (symbol (values nil nil spec nil))
        (list (if (= (length spec) 4)
                  (values-list spec)
                  (values (first spec) nil (second spec) (third spec)))))
    (when modulename
      (let ((modulespec
             (find modulename (slot-value class '%submodules) :key #'first)))
        (unless modulespec
          (error "Invalid module name ~A in gate address ~A" modulename spec))
        (when (or (and moduleindex (not (second modulespec)))
                  (and (not moduleindex) (second modulespec)))
          (error "Invalide module index ~A in gate address ~A"
                 moduleindex spec))
        ;; since module specified need to check gates in submodule class
        (setf class (find-class (third modulespec)))))
    (let ((gatespec (find name (slot-value class '%gatespec) :key #'first)))
      (unless gatespec
        (error "Invalid gate name ~A in gate address ~A" name spec))
      (when (or (and index (not (third gatespec)))
                (and (not index) (third gatespec)))
        (error "Invalid gate index ~A in gate address ~A" index spec)))
    (if modulename
        #'(lambda(instance)
            (gate (submodule instance modulename :index moduleindex)
                  name :index index :direction direction))
        #'(lambda(instance)
            (gate instance  name :index index :direction :input)))))

(defun connection-func(class spec)
  (multiple-value-bind(dir channelspec arg1 arg2)
      (ecase (length spec)
        (3 (values (first spec) nil (second spec) (third spec)))
        (4 (values-list spec)))
    (flet ((connect-func(ffrom ffto)
             (if channelspec
                 (let ((channelspec
                        (merge-local-typespec channelspec class 'channel)))
                   ;; if no name specified in initargs use class name as name
                   (unless (getf (cdr channelspec) :name)
                     (setf (getf (cdr channelspec) :name) (car channelspec)))
                 #'(lambda(instance)
                     (let ((channel
                            (apply #'make-instance
                                   `(,(car channelspec)
                                      :owner ,instance
                                      ,@(rest channelspec)))))
                     (connect (funcall ffrom instance)
                              (funcall ffto instance)
                              :channel channel
                              :leave-uninitialized t)
                     (push channel (slot-value instance 'channels)))))
                 #'(lambda(instance)
                     (connect (funcall ffrom instance)
                              (funcall ffto instance)
                              :leave-uninitialized t)))))
      (ecase dir
        (=> (connect-func (gate-accessor class arg1 :output)
                          (gate-accessor class arg2 :input)))
        (<= (connect-func (gate-accessor class arg2 :output)
                          (gate-accessor class arg1 :input)))
        (<=> #'(lambda(instance)
                  (funcall (connect-func (gate-accessor class arg1 :output)
                                         (gate-accessor class arg2 :input))
                           instance)
                  (funcall (connect-func (gate-accessor class arg2 :output)
                                         (gate-accessor class arg1 :input))
                           instance)))))))

(defun connection-code-walk(class specs)
  (mapcar
   #'(lambda(spec)
       (if (listp spec)
           (if (member (first spec) '(=> <= <=>))
               (let ((f (connection-func class spec)))
                 `(funcall ,f *context*))
               (connection-code-walk class spec))
           spec))
   specs))

(defclass compound-module(module)
   ((submodules :type hash-table :initform (make-hash-table)
                 :reader submodules)
    (channels :type list :initform nil :reader channels))
  (:metaclass compound-module-class))

(defmethod index((module module))
  (let ((v (gethash (name module) (submodules (owner module)))))
    (when (vectorp v)
      (position module v))))

(defmethod initialize-instance :after ((module compound-module)
                                       &key &allow-other-keys)
  (build-submodules module)
  (build-connections module))

(defun make-submodule
    (instance name
     &optional (initargs
                (cddr (find name (slot-value (class-of instance) '%submodules)
                            :key #'first))))
  "Attempt to add a submodule to instance - throw error if submodule
already exists."
  (let ((sm (gethash name (submodules instance)))
        (initargs `(,(first initargs) :name ,name :owner ,instance
                     ,@(rest initargs))))
    (assert (or (arrayp sm) (null sm))
            ()
            "Submodule ~A of ~A already exists." name instance)
    (let ((submodule (apply #'make-instance initargs)))
      (if (vectorp sm)
          (vector-push-extend sm submodule)
          (setf (gethash name (submodules instance)) submodule))
      submodule)))

(defmethod build-submodules((module compound-module))
  (dolist(spec (slot-value (class-of module) '%submodules))
    (let ((name (first spec))
          (size (second spec))
          (initargs (cddr spec)))
      (assert (subtypep (third spec) 'module)
              ()
              "Unrecognised submodule type ~A" (third spec))
      (assert (not (gethash name (submodules module)))
              ()
              "Multiple declarations for submodule ~A in ~A" name module)
      (if size
          (progn
            (setf (gethash name (submodules module))
                    (make-array size
                                :element-type 'module
                                :adjustable t
                                :fill-pointer 0))
              (dotimes(x size) (make-submodule module name initargs)))
          (make-submodule module name initargs)))))

(defmethod build-connections((module compound-module))
  (let ((*context* module))
    (map nil #'eval (slot-value (class-of module) '%connections))))

(defgeneric submodule(module name &key index)
  (:documentation "Return submodule of given name (and index if in an array)")
  (:method((module compound-module) (name symbol) &key index)
    (let ((submodule (gethash name (submodules module))))
      (if index (aref submodule index) submodule))))

(defun for-each-submodule(module operator)
  (maphash
   #'(lambda(k v)
       (declare (ignore k))
       (if (vectorp v) (map nil operator v) (funcall operator v)))
   (submodules module)))

(defun for-each-channel(module operator)
  (map nil operator (channels module)))

(defmethod for-each-child((module compound-module) (operator function))
  (call-next-method)
  (for-each-submodule module operator)
  (for-each-channel module operator))

(defmethod repair-signal-flags :after ((component compound-module))
   (for-each-submodule component #'repair-signal-flags)
   (for-each-channel component #'repair-signal-flags))

(defmethod initialize((module compound-module) &optional stage)
  (flet ((init(c) (or (initialized-p c) (initialize c stage))))
     (or (every #'init (channels module))
         (every #'init (submodules module)))))

(defclass network(compound-module)
  ((gate-slots :initform nil))
  (:default-initargs :gates nil)
  (:metaclass compound-module-class)
  (:documentation "Base class for networks
 - essentially compound modules without gates"))

(defmethod initialize-instance :after((network network) &key &allow-other-keys)
  (assert (null (slot-value (class-of network) '%gatespec)))
  (unless (slot-boundp network 'name)
    (setf (slot-value network 'name) (class-name (class-of network)))))

(defmethod build-gates((network network)))

(defmethod parent-module((network network))
nil)

(defmethod full-path((network network))
  (list (name (owner network)) (name network)))

(defmethod index((network network))
  nil)

;; #|
;; Syntax wanted

;; (defnetwork network(basis)
;;   (:types
;;    ((C DatarateChannel :datarate 100MBps)))
;;   (:submodules
;;    (node 6 iNode :address index)
;;    (node1 Node)
;;    (node2 Node))
;;   (:connections
;;    (<-> C (node 1 port) (node 2 port))
;;    (<-> C (next-gate port node1) (next-gate port node4))
;;    (<-> C (next-gate port node4) (next-gate port node6))))


;; (defsimple app(basis)
;;   ((parameters etc))
;;   (:gates
;;    (in :input)
;;    (out :output)))


;; (defmodule node(basis)
;;   ((paramaters etc))
;;   (:gates
;;    (port () :inout))
;;   (:submodules
;;    (app App)
;;    (routing Routing)
;;    (queue (length port) Queue))
;;   (:connections
;;    (-> (routing localOut) (app in))
;;    (<- (routing localIn) (app out))
;;    (for(i 0 (1- (sizeof port)))
;;        (-> (routing out i) (queue in i))
;;        (<- (routing in i) (queue out  i))
;;        (<-> (queue line i) (port i)))))

;; |#
