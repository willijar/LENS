(in-package :lens)

(defgeneric build-gates(class instance)
  (:documentation "Initialise gates on instance using gatespec in class."))

(defgeneric build-inside(class instance)
  (:documentation "create submodules and connect them"))

(defclass module-class(parameter-class)
  ((%gatespec
    :type list :initform nil
    :documentation "Gate specification used to build gates for this class"))
  (:documentation "Metclass for entities with gates"))

(defun inheritence(new-names superclass slot-name type)
  "Generate an error if there is a redefinition of names")

(defmethod initialize-instance :after
    ((class module-class) &key gates direct-superclasses &allow-other-keys)
  ;; check no conflicts in inheritence of gates from superclasses
  (loop
     :for superclasses :on direct-superclasses
     :for classa = (car superclasses)
     :for a = (slot-value classa '%gatespec)
     :for i = (intersection gates a :key #'first)
     :if i :do (error "Gate specification conflict ~A from superclasses ~A" (first (first i)) a )
     :do (loop :for rest :on (cdr superclasses) :for classb = (car rest)
            :for b = (slot-value classb '%gatespec)
            :for i = (intersection a b :key #'first)
            :when i :do  (error "Gate specification conflict ~A in superclasses ~A and ~A" (first (first i))  classa classb)))
  (setf (slot-value class '%gatespec)
        (append gates (reduce #'append superclasses :key #'(lambda(c) (slot-value c '%gatespec))))))

(defmethod sb-mop:validate-superclass ((class module-class)
                                       superclass )
  (typep superclass '(or standard-class parameter-class)))

(defmethod build-gates((class module-class) entity)
  (setf (slot-value entity 'gates)
        (mapcar
         #'(lambda(spec)
             (let* ((sizespec (third spec))
                    (size
                    (etypecase sizespec
                      (number sizespec)
                      (symbol (slot-value entity sizespec))
                      (list (eval sizespec)))))
               (make-instance 'gate-slot
                              :name (first spec)
                              :direction (second spec)
                              :initial-size size)))
         (slot-value class '%gatespec))))

(defclass compound-module-class(module-class)
  ((%types :type list :initarg :types :initform nil
           :documentation "Specified local type mapping.")
   (%submodules :type list :initarg :submodules :initform nil
                :documentation "Submodule specifications")
   (%connections :type list :initarg :connections :initform nil
                 :documentation "Connection specification for this class"))
  (:documentation "Metclass for entities with gates"))

(defmethod sb-mop:validate-superclass ((class compound-module-class)
                                       superclass )
  (typep superclass '(or standard-class parameter-class module-class)))

(defgeneric arrived(module message gate time)
  (:documentation "Called when a message arrives at a gate which is not further
    connected (that is, next-gate is NULL)"))

(defclass module(parameter-object component)
  ((gates :type list :initform nil :reader gates)
   (:metaclass module-class)))

(defmethod gate((module module) (name symbol) &key direction index)
  (let ((slot (find name (gates module) :key #'name)))
    (when slot (gate slot direction :index index))))

(defgeneric for-each-gate(module operator)
  (:method((module module) (operator function))
    (dolist(gate-slot (gates module))
      (for-each-child gate-slot operator))))

(defmethod for-each-child((module module) (operator function))
  (call-next-method)
  (for-each-gate module operator))

(defmethod initialize-instance :after ((module module) &key &allow-other-keys)
  (build-gates (class-of module) module)
  (build-inside (class-of module module) module))

(defclass compound-module(module)
  ((submodules :type list :initform nil :reader submodules)
   (channels :type list :initform nil :reader channels))
  (:metaclass compound-module-class))

(defgeneric for-each-submodule(module operator)
  (:method((module compound-module) (operator function))
    (dolist(module (submodules module))
      (if (typep module 'module)
          (funcall operator module)
          (for-each-child module operator)))))

(defgeneric for-each-channel(module operator)
  (:method((module compound-module) (operator function))
    (map nil operator (channels module))))

(defmethod repair-signal-flags :after ((component compound-module))
  (for-each-submodule component #'repair-signal-flags)
  (for-each-channel component #'repair-signal-flags))

(defmethod for-each-child((module compound-module) (operator function))
  (call-next-method)
  (for-each-channel module operator)
  (for-each-submodule module operator))

(defmethod initialize((module compound-module) &optional (stage 0))
  (flet ((init(c) (initialize c stage)))
    (or (some #'init (channels module))
        (some #'init (modules module)))))

(defclass network(compound-module)
  ()
  (:initargs :gates nil)
  (:metaclass parameter-class))




#|
Syntax wanted

(defnetwork network(basis)
  (:types
   ((C DatarateChannel :datarate 100MBps)))
  (:submodules
   (node (6) iNode (:address index))
   (node1 Node)
   (node2 Node))
  (:connections
   (<-> C (next-gate port node1) (next-gate port node2))
   (<-> C (next-gate port node1) (next-gate port node4))
   (<-> C (next-gate port node4) (next-gate port node6))))


(defsimple app(basis)
  ((parameters etc))
  (:gates
   (in :input)
   (out :output)))


(defmodule node(basis)
  ((paramaters etc))
  (:gates
   (port () :inout))
  (:submodules
   (app App)
   (routing Routing)
   (queue (length port) Queue))
  (:connections
   (-> (routing localOut) (app in))
   (<- (routing localIn) (app out))
   (for(i 0 (1- (sizeof port)))
       (-> (routing out i) (queue in i))
       (<- (routing in i) (queue out  i))
       (<-> (queue line i) (port i)))))

|#
