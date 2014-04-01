;; module and compound-module interfaces and implementation
;; Copyright (C) 2013-2014 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; LENS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; module classes must use the module-class metaclass which adds in the
;; :gates argument for class specification
;; compound-module classes must use the compound-module-class metaclass
;; which adds in the :gates, :submodules and :connections
;; arguments to the class definitions

;;; Code:

(in-package :lens)

(defgeneric build-gates(module)
  (:documentation "* Arguments

- module :: a [[module]]

* Description

Build the module gates on the basis of the gate specification from the
=:gates= argument in the [[module-class]] specification.

This method is called during a [[module]] instance initialisation."))

(defgeneric build-inside(module)
  (:documentation "* Arguments

- module :: a [[module]]

* Description

Build the the submodules (calling [[build-submodules]]) of
[[compound-module]] /module/ as per the =:submodules= class argument and
connect them (calling [[build-connections]] as per the
=:connections= class argument.

This method is recursively called upon [[network]] creation.")
  (:method(instance) (for-each-submodule instance #'build-inside)))

(defgeneric build-submodules(module)
  (:documentation "* Arguments

- module :: a [[compound-module]]

* Description

Build the submodules inside a [[compound-module]]
as per the =:submodules= class argument. This may be specialised
usefully to add in additional submodule creation algoritmically.")
  (:method(instance) (declare (ignore instance))))

(defgeneric build-connections(module)
  (:documentation "* Arguments

- module :: a [[compound-module]]

* Description

Build connections between submodules and gates of /module/ as per the
=:connections=. This may be specialised usefully to add in build the
network connections algorithmically."))

(defgeneric for-each-submodule(module operator)
  (:documentation "* Arguments

- module :: a [[module]]
- operator :: a =function=

* Description

Applies /operator/ over each submodule of a /module/.

Objects with submodules must provide this to iterate over
submodules. [[module]] and [[compound-module]] classes provide
implementations automatically however if additional submodules
beyond those specified in the class are created it may be necessary
to specialise this function to include them..")
  (:method(module operator) (declare (ignore module operator))))

(defgeneric arrived(module message gate arrival-time)
  (:documentation "* Arguments

- module :: a [[module]]
- message :: a [[message]]
- gate :: a [[gate]]
- arrival-time :: a [[time-type]]

*Description

Called when the /message/ arrives at the /gate/ which is not further
connected (that is, next-gate is NULL) of of /module/. /arrival-time/
is when the message is to be delivered.

The default implementation will fill in the arrival gate details in the /message/ and schedule it for delivery at the /arrival-time/. Packets will be schedule at the /arrival-time/ + their [[duration]] unless [[deliver-on-reception-start-p]] is set true for the gate in which case they will the [[reception-start-p]] will be set true for the packet and they will be delivered at /arrival-time/.
"))

(defgeneric send(module message gateid &key delay)
  (:documentation "* Arguments

- module :: a [[module]]
- message :: a [[message]]
- gateid :: a gate descriptor (a [[gate]] or list of gate name, direction and index.
- delay :: a [[time-type]]

* Description

Schedule sending /message/ through the specified /gate/ of given
/module/ after given /delay/. The /delay/ is added onto the current
[[simulation-time]] to determine delivery time and the source gate of
the message will be sent. [[deliver]] is called to actually schedule
the /message/. "))

(defgeneric send-direct(module
                        togate message &key propagation-delay duration)
  (:documentation "* Arguments

- module :: a [[module]]
- togate :: a  [[gate]]
- message :: a [[message]]

* Keyword Arguments

- propagation-delay :: a [[time-type]]
- duration :: a [[time-type]]

* Description

Send /message/ directly to the /togate/ gate of another module.

If the target /togate/ is further connected
the message will follow the connections that start at that gate.  For
example, when sending to an input gate of a [[compound-module]], the
message will follow the connections inside the compound module.

It is permitted to send to an output gate, which will also cause the
message to follow the connections starting at that gate.  This can be
useful, for example, when several submodules are sending to a single
output gate of their parent module.

It is not noramlly permitted to send to a gate of a [[compound-module]] which is
not further connected unless [[function handle-message]] has been specialised for that [[compound-module]] class.

Also, it is not permitted to send to a gate which is otherwise
connected i.e. which has a [[previous-gate]]. This means that modules
*must* have dedicated gates for receiving via [[send-direct]]. You cannot
have a gate which receives messages via both connections and
[[send-direct]].

When a nonzero /duration/ is given, that signifies the duration of the
packet transmission, that is, the time difference between the
transmission (or reception) of the start of the packet and that of the
end of the packet. The destination module can choose whether it wants
the simulation kernel to deliver the packet object to it at the start
or at the end of the reception. The default is the latter; the module
can change it by calling [[deliver-on-reception-start]] on the final
input gate (that is the [[path-end-gate]]). [[deliver-on-reception-start]]
needs to be called in advance, for example in the [[initialize]] method of
the module. When a module receives a packet, it can call the
[[reception-start-p]] and [duration]] methods on the packet to find out
whether it represents the start or the end of the reception, and the duration of
the transmission.

For messages that are not packets the /duration/ must be zero."))

(defgeneric schedule-at(module message &key time delay)
  (:documentation
"* Arguments

- module :: a [[module]]
- message :: a [[message]]

* Keyword Arguments
- time :: a [[time-type]]
- delay :: a [[time-type]]

* Description

Schedule a self-message. The /message/ will be delivered back to
/module/ via [[handle-message]] either at absolute simulation time
/time/ or after delay /delay/ which will be added onto the current
simulation time. This function is the way you can implement timers or
timeouts. Timers can also be cancelled via [[cancel]].
When the message is delivered at the module, you can call
[[self-message]] to tell it apart from messages arriving
from other modules.

[[cancel]] can be used to cancel the self-message before it arrives.
This is useful for implementing timeouts: if the event occurs 'in time'
 (before timeout), the scheduled self-message can be cancelled.

Given a message you can check whether it is currently
scheduled by calling [[scheduled-p]]. If it is scheduled,
you cannot schedule it again without calling [[cancel]] first.
However, after the message was delivered to the module or cancelled,
you can schedule it again -- so you can reuse the same message
object for timeouts over and over during the whole simulation.

 * Notes

The preferred way of implementing timers is now provided using
[[timer-message]]s, [[set-timer]] and [[cancel-timer]].

"))

(defclass module-class(parameter-class)
  ((%gatespec
    :type list :initform nil
    :documentation "The parsed gate specification used to build gates for this class"))
  (:documentation "* Additional Class Options
- gates :: ( (/gate-name/ /direction/ [/size]/)*)

           * /gate-name/ : a =symbol=

           * /direction/ : =(:input | :output | :inout)=

           * /size/ : an =integer=

* Description

Metaclass for entities with gates. Must be used as
metaclass for [[module]] classes.

The gates class option specified what gates are to be created for
instances of classes of this type. The /gate-name/ specifies the
symbolic name to be used to identify the gate and must be unique for
this module. If /direction/ is specified as =:inout= both input
and output gates will be created. If /size/ is specified an array
of gates will be created. A size of zero can be useful to allow for
the automatic creation of the gates on demand depending on the
connections the module."))

;; metaclass inheritance helper functions
(defun named-specifications-inheritance
    (localspecs direct-superclasses classtype classslot)
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
         (loop
            :for remainder :on (rest superclasses)
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

(defun merge-local-typespec(spec class)
  (unless (listp spec) (setf spec (list spec)))
  (let ((type (first spec))
        (args (rest spec)))
    (let ((classspec (find type (slot-value class '%localtypes) :key #'first)))
      (when classspec
        (setf type (second classspec)
              args (append args (cddr classspec)))))
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
    :type hash-table :initform (make-hash-table) :reader gate-slots
    :documentation "Hash table mapping gate names to [[gate-slot]]
    instances as specified in the =:gates= slot option in the class
    specification of subclasses." ))
  (:metaclass module-class)
  (:documentation "Base class for all [[module]]s which must have
  metaclass [[module-class]].

Modules are used to implement protocols by specialising on the
following methods.

- [[initialize]] method may be used to specify initial configuration
  of the module after creation but before the simulation starts. It
  may for example send a self message to initiate some process.
- [[handle-message]] is used to receive and process all incoming messages.
- [[send]] is used to send messages out of a gate.
- [[schedule-at]] is used to schedule self messages."))

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

(defmethod build-inside((module module))
  (build-submodules module)
  (for-each-submodule module #'build-inside))

(defmethod finish((module module))
  (for-each-submodule module #'finish)
  (call-next-method))

(defun for-each-gate(module operator)
  (labels((over(slot direction)
            (let ((v (funcall direction slot)))
              (when v
                (if (vectorp v) (map nil operator v) (funcall operator v))))))
    (when (gate-slots module)
      (maphash
     #'(lambda(k v)
         (declare (ignore k))
         (over v #'input)
         (over v #'output))
     (gate-slots module)))))

(defmethod gate((module module) (name symbol) &key direction index)
   (let ((slot (gethash name (gate-slots module))))
     (assert slot()
             "No gate ~A in module ~A" name module)
     (gate slot direction :index index)))

(defun gate-size(module name)
  (let ((slot (gethash name (gate-slots module))))
    (when slot (length (or (input slot) (output slot))))))

(defmethod gate((module module) (gateid list)
                &key (direction (third gateid)) (index (second gateid)))
  "Allow gate arrays to be indexed by list of name and index"
   (let ((slot (gethash (first gateid) (gate-slots module))))
     (assert slot()
             "No gate ~A in module ~A" (first gateid) module)
     (gate slot direction :index index)))

(defmethod for-each-child((module module) (operator function))
  (for-each-gate module operator)
  (for-each-submodule module operator))

(defmethod arrived ((module module) (message message) (gate gate) time)
  (setf (to message) gate)
  (let ((onstart (deliver-on-reception-start-p gate)))
    (if (typep message 'packet)
        (setf (reception-start-p message) onstart
              (arrival-time message) (if onstart time (+ time (duration message))))
        (setf (arrival-time message) time)))
  (schedule message))

(defmethod send((from-module module) (message message) (outgate gate)
                &key (delay 0))
  (assert (not (eql (gate-direction outgate) :input))
          ()
          "Cannot send via input gate ~A" outgate)
  (assert (next-gate outgate)
          ()
          "Output gate ~A not connected" outgate)
  (assert (or (not (owner message)) (eql (owner message) from-module))
          ()
          "~A cannot send message ~A currently owned by ~A."
          from-module message (owner message))
  (assert (not (scheduled-p message))
          ()
          "~A cannot be sent; already scheduled." message)
  (assert (>= delay 0)
          ()
          "Negative delay ~A" delay)
  (when (typep message 'packet) (setf (duration message) 0))
  (let ((delay-end-time (+ (simulation-time) delay)))
    (setf (from message) outgate
          (sent-time message) delay-end-time)
    (deliver message outgate delay-end-time)))

(defmethod send((module module) (message message) gateid &key (delay 0))
  (send module message (gate module gateid :direction :output) :delay delay))

(defmethod send-direct(from-module (to-gate gate) (message message)
                       &key (propagation-delay 0) (duration 0))
  (assert (not (previous-gate to-gate))
          ()
          "Module must have dedicated gates for receiving vai send-direct. from side of ~A must not be connected" to-gate)
  (assert (and (>= propagation-delay 0) (>= duration 0))
          ()
          "Propagation and duration parameters cannot be negative.")
  (assert (or (not (owner message)) (eql (owner message) from-module))
          ()
          "~A cannot send message ~A currently owned by ~A."
          from-module message (owner message))
  (assert (not (scheduled-p message))
          ()
          "Already scheduled Message ~A cannot be sent." message)
  (setf (from message) from-module
        (sent-time message) (simulation-time))
  (if (typep message 'packet)
      (setf (duration message) duration)
      (assert (zerop duration)))
  (deliver message to-gate (+ (simulation-time) propagation-delay)))

(defmethod schedule-at((module module) (message message)
                       &key (delay 0) (time (+ (simulation-time) delay)))
  "Schedule a self message"
  (assert (not (scheduled-p message))
          ()
          "Already scheduled Message ~A cannot be rescheduled." message)
  (assert (or (not (owner message)) (eql (owner message) module))
          ()
          "~A cannot schedule message ~A currently owned by ~A."
          module message (owner message))
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
  (:documentation "* Additional Class Options

- types :: ( (/typename/ /initargs/)* )

           * /typename/ : a =symbol=

           * /initargs/ : (classname {keyword argument}*)

- submodules :: ( (/submodule-name/ [/sizespec/] ((/classname/ | /typename/) {/keyword/ /argument/}* ) *)

           * /submodule-name/ : a =symbol=

           * /sizespec/ : (integer | (sizeof gate-name) | slot-name)

- connections :: ( /(gate-specifier/ [/channel-spec/] /direction/ /gate-specifier/)* )

            * /gate-specifier/ : (/gate-name/ | (/submodule-name/ /gate-name/))

            * /channel-spec/ : ((/classname/ | /typename/) {/keyword/ /argument/}* )

            * /direction/ : (=> | <=> | <=)

* Description

Metaclass for all compound modules classes - the base class for
modules with gates, submodules and connections between those
submodules and gates. Must be used as metaclass for
[[compound-module]] classes.

See [[module-class]] for details on the =:gates=  class option.

The =:types= class option provides a way of providing a mapping
between a single symbolic /typename/ and a list of /initargs/ which
would correspond to the /classname/ and keyword arguments passed in
construction of either a submodule or channel. These names thus
provide a useful shortcut when defining submodules or channels. If a
/typename/ is specified with some additional arguments they will
override the default ones.

The =:submodules= class option provides a list of submodule class
specifications consisting of the local name for the submodule, an
optional /sizespec/ if an array of submodules is to be created and the
arguments to =make-instance= to be used. A previously defined local
type shortcut may be used instead of the classname. At creation the
=:owner= keywword will be added to the /initargs/ with the current
instance as the argument. A /sizespec/ may either be an integer, a
symbolic slot-name corresponding to one of the slots in the class or
=(sizeof gate-name)= which will correspond to the size of the array of
gates with the given gate-name.

The =:connections= class option specifies connections between
gates.  Gates are specified either as the gate name if a gate in the
current module or a list of submodule name and gate name for
submodules. They may additionally have an index parameter if
corresponding to gate arrays. The direction specifier specifies the
direction of connection, =<\=>= may be used to provide connections in
both directions between =:inout= gates. An optional channel specifier
may be used as the second argument specifying the channel type and
initargs for creating the channel. The type may be a local type
definied with the =:types= slot option. The =:name= argument may be
used to give individual channels names - otherwise they will be named
after their type name. The =:owner= keyword argument will be added
with the current object instance as the argument.
"))

(defmethod sb-mop:validate-superclass ((class compound-module-class)
                                       superclass )
  (typep superclass '(or standard-class parameter-class module-class)))

(defmethod initialize-instance :after
     ((class compound-module-class)
      &key types submodules connections &allow-other-keys)
  "Necessary to ensure types, submodules and connections is on allowed
initialization list"
  (declare (ignore types submodules connections)))

;; TODO allow size to be determined by slot or sizeof a gate in
;; module instantiation
(defmethod shared-initialize :after
    ((class compound-module-class) slot-names
     &key types submodules connections direct-superclasses &allow-other-keys)
  (dolist(spec types)
    (assert (and (symbolp (first spec)) (symbolp (second spec)))
            ()
            "Invalid local type specification ~A" spec))
  (setf (slot-value class '%submodules)
        (named-specifications-inheritance
         (mapcar #'(lambda(spec) (submodule-generators class spec))
                 submodules)
         direct-superclasses 'compound-module-class '%submodules))
  (setf (slot-value class '%connections)
        (connection-code-walk class connections)))

(defun submodule-generators(class spec)
  "Given the module specification form return the siz especification
and initargs specification - either nil and a list for a single module
or two functions - one of the instance to return the size and one of
the instance and index to return the module initargs for each module
in a vector. The specification may either be a name and class initargs
for a single module or a name, a size specification and a classtype
for a vector followed by either the rest of the initargs if all
elements or the same or else a range plist with the complete
initargs (including types)"
  (let* ((name (first spec))
         (v (second spec))
         (sizespec
           ;; size may be a number, function, (sizeof gatename) or a slot name
          (cond
            ((and (listp v) (member (first v) '(lambda function)))
             (eval v))
            ((and (listp v) (eql (car v) 'sizeof))
             #'(lambda(instance) (gate-size instance (second v))))
            ((and (symbolp v)
                  (or (not (third spec)) ;; spec is single classname
                      (and (symbolp (third spec)) ;; or classname with keywords
                      (eql (symbol-package (third spec))
                           (find-package :keyword)))))
             ;; then it is single modulenetwork
             (return-from submodule-generators
               `(,name nil ,@(merge-local-typespec (cdr spec) class))))
            ((numberp v)
             #'(lambda(instance) (declare (ignore instance)) v))
            ((symbolp v) ;; default assume it is a slot name
             #'(lambda(instance) (slot-value instance v)))
            (t (error "Unknown size specification ~A for module array." v))))
         (typespec (cddr spec)))
    ;; if we get here we have a module vector - determine if typespec is
    ;; a range plist or the same for each element
;;network
    (list
     name
     sizespec
     (cond
       ((range-list-p typespec)
        (let ((typespec (copy-list typespec)))
          (loop
             :for a :on typespec :by #'cddr
             :do (setf (second a)
                       (merge-local-typespec (second a) class)))
          #'(lambda(instance index)
              (declare (ignore instance))
              (range-getf typespec index))))
      (t
       (let ((typespec (merge-local-typespec typespec class)))
         #'(lambda(instance index)
             (declare (ignore instance index))
             typespec)))))))

(defun gate-accessor(class spec direction)
  "Return a closure which when given the instance as an argument will
return the gate given by spec. Validates spec based on class definitions"
  (multiple-value-bind(modulename moduleindex name index)
      (etypecase spec
        (symbol (values nil nil spec nil))
        (list
         (case (length spec)
           (4 (values-list spec))
           (3 (if (numberp (second spec))
                  (values (first spec) (second spec) (third spec))
                  (values (first spec) nil (second spec) (third spec))))
           (2 (if (numberp (second spec))
                  (values nil nil (first spec) (second spec))
                  (values (first spec) nil (second spec) nil))))))
    (if modulename
        (let ((modulespec
               (find modulename (slot-value class '%submodules) :key #'first)))
          (unless modulespec
            (error "Invalid module name ~A in gate address ~A" modulename spec))
          (when (or (and moduleindex (not (second modulespec)))
                    (and (not moduleindex) (second modulespec)))
            (error "Invalid module index ~A in gate address ~A"
                   moduleindex spec))
          #'(lambda(instance)
              (gate (submodule instance modulename :index moduleindex)
                    name :index index :direction direction)))
        (let ((gatespec (find name (slot-value class '%gatespec) :key #'first)))
          (unless gatespec
            (error "Invalid gate name ~A in gate address ~A" name spec))
          (when (or (and index (not (third gatespec)))
                    (and (not index) (third gatespec)))
            (error "Invalid gate index ~A in gate address ~A" index spec))
          #'(lambda(instance)
              (gate instance  name :index index :direction direction))))))

(defun connection-func(class spec)
  (multiple-value-bind(dir channelspec arg1 arg2)
      (ecase (length spec)
        (3 (values (first spec) nil (second spec) (third spec)))
        (4 (values-list spec)))
    (labels ((class-gate-p(gatespec) ;; i.e. is not a submodule gate
               (or (symbolp gatespec)
                   (and (= (length gatespec) 2) (numberp (second gatespec)))
                   (null (first gatespec))))
             (connect-func(from to)
               (let ((ffrom
                      (gate-accessor
                       class from (if (class-gate-p from) :input :output)))
                     (ffto
                       (gate-accessor
                        class to (if (class-gate-p to) :output :input))))
               (if channelspec
                 (let ((channelspec
                        (merge-local-typespec channelspec class)))
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
                              :leave-uninitialized t))))))
      (ecase dir
        (=> (connect-func arg1 arg2))
        (<= (connect-func arg2 arg1))
        (<=> #'(lambda(instance)
                 (funcall (connect-func arg1 arg2) instance)
                 (funcall (connect-func arg2 arg1) instance)))))))

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
  (:metaclass compound-module-class)
  (:documentation "Base class for all compound-modules using the [[compound-module-class]] metaclass. See [[class compound-module-class]] for details of the additional class slot options available.

Typically no further implementation beyond the class specification is
used with compound modules as messages will be automatically routed in
the gates of submodules as per the =:connections= specifications. It
is however allowed to have unconnected gates in which case
[[handle-message]] must be implemented to receive the messages. This
would allow some implementation in compound modules which they might
then send to several contained submodules.

The [[build-submodules]] and [[build-connections]] may be usefully
extended to allow algorithmic creation of the contained network."))

(defmethod size((module module))
  (let ((v (gethash (name module) (submodules (owner module)))))
    (when (vectorp v) (length v))))

(defmethod finish((module compound-module))
  (for-each-channel module #'finish)
  (call-next-method))

(defmethod build-inside((module compound-module))
  (build-submodules module)
  (build-connections module)
  (for-each-submodule module #'build-inside))

(defun make-submodule
    (instance name
     &optional (initargs
                (cddr (find name (slot-value (class-of instance) '%submodules)
                            :key #'first))))
  "Attempt to add a submodule to instance - throw error if submodule
already exists. Checks for a typename configuration parameter to allow
specification of a specific subclass."
  (let* ((sm (gethash name (submodules instance)))
         (basetype (first initargs))
         (initargs `(:name ,name
                     :owner ,instance
                     ,@(when (arrayp sm) (list :index (length sm)))
                     ,@(rest initargs))))
    (assert (subtypep basetype 'module)
              ()
              "Unrecognised submodule type ~A" basetype)
    (assert (or (arrayp sm) (null sm))
            ()
            "Submodule ~A of ~A already exists." name instance)
    (let ((typename
           (or
            (read-parameter instance
                            (if (vectorp sm)
                                (list name (length sm) 'typename)
                                (list name 'typename))
                            'read)
            basetype)))
      (let ((submodule (apply #'make-instance typename initargs)))
        (if (arrayp sm)
            (vector-push-extend submodule sm)
            (setf (gethash name (submodules instance)) submodule))
      submodule))))

(defmethod build-submodules((module compound-module))
  (dolist(spec (slot-value (class-of module) '%submodules))
    (let ((name (first spec))
          (size (second spec))
          (initargs (cddr spec)))
      (assert (not (gethash name (submodules module)))
              ()
              "Multiple declarations for submodule ~A in ~A" name module)
      (if size
          (let ((size (funcall size module)))
            (setf (gethash name (submodules module))
                  (make-array size
                              :element-type 'module
                              :adjustable t
                              :fill-pointer 0))
              (dotimes(x size)
                (make-submodule module name (funcall (first initargs) module x))))
          (make-submodule module name initargs)))))

(defmethod build-connections((module compound-module))
  (let ((*context* module))
    (map nil #'eval (slot-value (class-of module) '%connections))))

(defgeneric submodule(module address &key index)
  (:documentation "* Arguments

- module :: a [[module]]

- address :: a /submodule-specifier/

   * /submodule-specifier/ : ( (/submodule-name/ | ( /submodule-name/ index))*)

   * /submodule-name/ : a =symbol=

   * /index/ L an =integer=

* Description

Return the submodule of a module given a heirarchical address. This
will recurse throguh the submodule structure in order if the address
is a list of submodules names. At each stage the submodule name or the
name and index are used to recurse further down. It will return an
error if there is no such named submodule.")
  (:method((module compound-module) (name symbol) &key index)
    (let ((submodule (gethash name (submodules module))))
      (assert submodule (submodule)
              "No ~A submodule found in compound module ~A" name module)
      (if index (aref submodule index) submodule)))
  (:method((module compound-module) (address list) &key index)
    (assert (null index))
    (if (integerp (second address))
        (let ((module (submodule module (first address)
                                 :index (second address))))
          (if (cddr address)
              (submodule module (cddr address))
              module))
        (let ((module (submodule module (first address))))
          (if (cdr address)
            (submodule module (cdr address))
            module)))))

(defmethod for-each-submodule((module compound-module) operator)
  (call-next-method)
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

(defmethod initialize list ((module compound-module) &optional stage)
  (let ((initialized-p t))
    (flet ((init(c)
             (unless (initialized-p c)
               (unless (initialize c stage) (setf initialized-p nil)))))
      (for-each-channel module #'init)
      (for-each-submodule module #'init))
    initialized-p))

(defgeneric connected-p(instance)
  (:method ((gate gate))
    (if (typep (parent-module gate) 'compound-module)
        (and (previous-gate gate) (next-gate gate))
        (connected-outside-p gate))))

(defun path-ok-p(gate)
  (not (or (typep (parent-module (path-start-gate gate)) 'compound-module)
           (typep (parent-module (path-end-gate gate)) 'compound-module))))

(defclass network(compound-module)
  ((gate-slots :initform nil))
  (:default-initargs :gates nil)
  (:metaclass compound-module-class)
  (:documentation "Base class for networks. This is the required type
  for the top-level [[compound-module]] of a simulation network and it
  is required that it has no gate specification. It is specified in
  the =network= simulation parameter."))

(defmethod initialize-instance :after((network network) &key &allow-other-keys)
  (assert (null (slot-value (class-of network) '%gatespec)))
  (unless (slot-boundp network 'name)
    (setf (slot-value network 'name) (class-name (class-of network))))
  (build-inside network))

(defmethod build-gates((network network)))

(defmethod parent-module((network network))
nil)

(defmethod full-path((network network))
  (list (name (owner network)) (name network)))

(defmacro defmodule(name (&rest superclasses) (&rest vars) &rest args)
  (unless (member 'module superclasses)
    (setf superclasses (cons 'module superclasses)))
  `(defclass ,name(,@superclasses)
     (,@vars)
     ,@args
     (:metaclass module-class)))
