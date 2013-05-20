(in-package :lens)

(defclass component(parameter-object entity-with-signals)
  ((rng-map :type array :reader rng-map
            :documentation "RNG map for this component")
   (initialized-p
    :initform nil :reader initialized-p
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

(defmethod finish :around ((component component))
  (let ((*context* component))
    (call-next-method)))
