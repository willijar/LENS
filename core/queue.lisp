(in-package :lens)

(register-signal 'drop
 "Signal called with packet when it is dropped e.g. due to buffer overflow")

(register-signal 'buffer-length
 "Signal called with queue object when its length is changed")

(register-signal 'buffer-time
 "Sgnal called with a queueing time when an object ois dequeued")

(defstruct (timestamped-queue (:include alg:vector-queue))
  (average-timestamp 0.0d0 :type time-type))

(defmethod enqueue(x (q timestamped-queue))
  (let ((record (make-timestamped :value x))
        (size (size q)))
    (setf (timestamped-queue-average-timestamp q)
          (if (zerop size)
              (timestamped-time record)
              (/ (+ (timestamped-time record)
                    (* size (timestamped-queue-average-timestamp q)))
                 (1+ (size q)))))
    (call-next-method record q)))

(defmethod dequeue((q timestamped-queue))
  (let ((record (call-next-method)))
    (setf (timestamped-queue-average-timestamp q)
          (if (zerop (size q))
              0.0d0
          (/ (- (* (1+ (size q)) (timestamped-queue-average-timestamp q))
                (timestamped-time record))
             (size q))))
    (values (timestamped-value record) (timestamped-time record))))

(defgeneric average-queue-time(q)
  (:method((q timestamped-queue))
    (- (timestamped-queue-average-timestamp q) (simulation-time))))

(defclass packet-buffer(entity-with-signals parameter-object)
  ((queue :reader queue :type timestamped-queue)
   (buffer-size
    :initarg :buffer-size
    :type fixnum :parameter t :initform 32 :reader buffer-size
    :documentation "max size in messages")
   (buffer-size-bytes
    :initarg :buffer-size-bytes
    :type fixnum :parameter t :initform nil :reader buffer-size-bytes
    :documentation "max size in bytes")
   (byte-length :initform 0 :type fixnum :reader byte-length))
  (:metaclass parameter-class))

(defmethod initialize-instance :after((buffer packet-buffer)
                                      &key &allow-other-keys)
  (setf (slot-value buffer 'queue)
        (if (buffer-size buffer)
            (make-timestamped-queue
             :vector (make-array (buffer-size buffer))
             :extend-size nil)
            (make-timestamped-queue))))

(defmethod size((buffer packet-buffer)) (size (queue buffer)))

(defmethod empty-p((buffer packet-buffer)) (empty-p (queue buffer)))

(defmethod enqueue(p (buffer packet-buffer))
  (when (or (and (buffer-size buffer)
                 (= (size buffer) (buffer-size buffer)))
            (and (buffer-size-bytes buffer)
                 (> (+ (byte-length p) (byte-length buffer))
                    (buffer-size-bytes buffer))))
    (emit buffer 'drop p)
    (return-from enqueue nil))
  (enqueue p (queue buffer))
  (incf (slot-value buffer 'byte-length) (byte-length p))
  (emit buffer 'buffer-length buffer)
  p)

(defmethod peek((buffer packet-buffer))
  (peek (queue buffer)))

(defmethod dequeue((buffer packet-buffer))
  (multiple-value-bind(p timestamp) (dequeue (queue buffer))
    (decf (slot-value buffer 'byte-length) (byte-length p))
    (emit buffer 'buffer-length buffer)
    (emit buffer 'buffer-time (- timestamp (simulation-time)))
    (values p timestamp)))

(defmethod average-queue-time((buffer packet-buffer))
  (average-queue-time (queue buffer)))

(defconstant +default-history-size+ 5)

(defclass history-buffer()
  ((queue :type alg::vector-wrap-queue :reader queue)
   (test :initform #'equalp :initarg :test :reader buffer-test)
   (key :initform #'identity :initarg :key :reader buffer-key))
  (:documentation "A class for recording history of seen objects."))

(defmethod initialize-instance :after ((instance history-buffer)
                                      &key (size +default-history-size+)
                                      (element-type t)
                                      &allow-other-keys)
  (setf (slot-value instance 'queue)
        (alg::make-queue :element-type element-type
                         :initial-size size
                         :implementation :wrap)))

(defmethod duplicate-p(x (buffer history-buffer) &optional (record t))
  (or
   (find
    (buffer-key x)
    (alg::vector-wrap-queue-vector (queue buffer))
    :test #'(lambda(a b) (when b (funcall (buffer-test buffer) a b))))
   (progn
     (when record (enqueue (funcall (buffer-key x)) (queue buffer)))
     nil)))


