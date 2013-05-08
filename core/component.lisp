(in-package :lens)

(defclass component(entity-with-signals parameter-object)
  ((rng-map :type array :reader rng-map
            :documentation "RNG map for this component")
   (initialized-p :initform nil :reader initialized-p
                  :documentation "True if this component has been initialized."))
  (:metaclass parameter-class))

(defmethod initialize-instance :around
    ((component component) &key &allow-other-keys)
  (let ((*context* component))
    (call-next-method)))

(defmethod initialize-instance :after
    ((component component) &key config num-rngs &allow-other-keys)
  (setf (slot-value component 'rng-map)
        (if num-rngs
            (let* ((map (make-array num-rngs))
                   (path (copy-list (full-path component)))
                   (last (last path))
                   (format `(integer :min 0 :max ,(num-rngs *simulation*))))
              (dotimes(i num-rngs)
                (rplacd last (intern (format nil "RNG-~D" i)))
                (setf (aref map i)
                      (multiple-value-bind
                            (m found-p) (read-parameter path config format)
                        (aref (rng-map *simulation*) (if found-p m 0))))))
            (rng-map *simulation*))))

(defgeneric initialize(component &optional stage)
  (:documentation "Allowed depth-first staged initialization. Return
  true if action taken otherwise return nil to parent")
  (:method :around((component component) &optional (stage 0))
    (let ((*context* component))
      (prog1
          (when (call-next-method) (initialize component (1+ stage)))
        (when (zerop stage) (setf (slot-value component 'initialized-p) t)))))
  (:method((component component) &optional stage)
    (declare (ignore stage))
    nil))

(defmethod finish((component component))
  (for-each-child component #'finish))
