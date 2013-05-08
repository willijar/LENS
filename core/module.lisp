(in-package :lens)

(defgeneric build-gates(instance)
  (:documentation "Initialise gates on instance using gatespec in class."))

(defgeneric build-submodules(instance)
  (:documentation "Build submodules inside a compound module"))

(defgeneric build-connections(instance)
  (:documentation "Buildc connectiuons between submodules and gates in a compound module"))

(defgeneric build-inside(instance)
  (:documentation "create submodules and connect them"))


(defgeneric arrived(module message gate time)
  (:documentation "Called when a message arrives at a gate which is not further
    connected (that is, next-gate is NULL)"))

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
    (assert (subtypep type required-type))
    (cons type args)))

(defmethod initialize-instance :after
    ((class module-class) &key gates direct-superclasses &allow-other-keys)
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

(defclass module(component parameter-object)
  ((gate-slots :type hash-table :initform (make-hash-table) :reader gate-slots))
  (:metaclass module-class))

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
  (maphash
   #'(lambda(k v)
       (declare (ignore k))
       (for-each-child operator v))
   (gate-slots module)))

(defmethod gate((module module) (name symbol) &key direction index)
   (let ((slot (gethash name (gate-slots module))))
     (when slot (gate slot direction :index index))))

(defmethod for-each-child((module module) (operator function))
  (call-next-method)
  (for-each-gate module operator))

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
    ((class compound-module-class) &key types submodules connections
     direct-superclasses &allow-other-keys)
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
                      (if (or (numberp v) (functionp v)
                              (member v (class-slots class)
                                      :key #'slot-definition-name)
                              (member v (slot-value class '%gatespec) :key #'first))
                          (values v (cddr spec))
                          (values nil (cdr spec))))
                  (cons name (cons sizespec (merge-local-typespec typespec class 'module))))))
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
          (error "Invalide module index ~A in gate address ~A" moduleindex spec))
        ;; since module specified need to check gates in suibmodule class
        (setf class (find-class (third modulespec)))))
    (let ((gatespec (find name (slot-value class '%gatespec) :key #'first)))
      (unless gatespec
        (error "Invalid gate name ~A in gate address ~A" name spec))
      (when (or (and index (not (second gatespec)))
                (and (not index) (second gatespec)))
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
                 #'(lambda(instance)
                     (let ((channel
                            (apply #'make-instance
                                   `(,(car channelspec)
                                      :owner ,instance
                                      ,(rest channelspec)))))
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
        (initargs `(,(first initargs) :owner ,instance ,@(rest initargs))))
    (assert (or (arrayp sm) (null sm))
            ()
            "Submodule ~A of ~A already exists." name instance)
    (let ((submodule (apply #'make-instance initargs)))
      (if (arrayp sm)
          (vector-push-extend sm submodule)
          (setf (gethash name (submodules instance)) submodule))
      submodule)))

(defmethod build-submodules((module compound-module))
  (dolist(spec (slot-value (class-of module) '%submodules))
    (let ((name (first spec))
          (size (second spec))
          (initargs (cddr spec)))
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
       (if (arrayp v) (map nil operator v) (funcall operator v)))
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

(defmethod initialize((module compound-module) &optional (stage 0))
   (flet ((init(c) (initialize c stage)))
     (or (some #'init (channels module))
         (some #'init (submodules module)))))




;; (defclass network(compound-module)
;;   ()
;;   (:initargs :gates nil)
;;   (:metaclass parameter-class))


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
