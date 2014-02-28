(in-package :lens.wsn)

(defclass no-temporal-model(module)
  ()
  (:metaclass module-class))

(defmethod path-loss-signal-variation((model no-temporal-model)
                                       path-loss)
  (declare (ignore path-loss))
  0.0)

(defclass temporal-model(module)
  ((path :parameter t :type pathname :reader path
                   :documentation "Path to file with parameters")
   ;; parameters read from file
   (coherence-time
    :type time-type :reader coherence-time
    :documentation "Time beyond which signals are considered uncorrelated")
   ;; signal variability parameters in db
   (min-signal-variation :type real :reader min-signal-variation)
   (max-signal-variation :type real :reader max-signal-variation)
   (signal-variation-resolution :type real :reader signal-variation-resolution)
   (no-signals :type integer)
   ;; PDF
   (coherence-pdf :type vector :reader coherence-pdf
                  :documentation "Coherence PDF if previous signal unknown or time passed > coherence time")
   (correlation-pdf
    :type list :reader correlation-pdf :initform nil
    :documentation "Main list of PDFs by correlation time"))
  (:metaclass module-class)
  (:documentation "Implementation of a file based channel temporal model"))

(defun pdf-p(entity)
  "Check if is valid pdf"
  (and (arrayp entity)
       (every #'(lambda(x) (or (numberp x) (pdf-p x))) entity)))

(defmethod initialize list ((model temporal-model)
                            &optional (stage 0))
  (case stage
    (0
     (with-open-file(is (merge-pathnames (path model)))
       (let ((params (read is)))
         (dolist(name '(coherence-time min-signal-variation
                        max-signal-variation signal-variation-resolution))
           (setf (slot-value model name) (getf params name))))
       (with-slots (no-signals) model
         (setf no-signals
               (1+ (/ (- (max-signal-variation model)
                         (min-signal-variation model))
                      (signal-variation-resolution model))))
         (loop
            (let* ((pdf (read is nil))
                   (time (first pdf)))
              (unless pdf (return))
              (cond
                ((= time (coherence-time model))
                 ;; coherence pdf
                 (assert (and (pdf-p (second pdf))
                              (not (slot-boundp model 'coherence-pdf))))
                 (setf (slot-value  model 'coherence-pdf) (second pdf)))
                ((< time (coherence-time model))
                 (let ((arg (rest pdf))
                       (pdf (make-array no-signals)))
                   (do ((i 0 (1+ i)))
                       ((= i no-signals))
                     (let ((arg (elt arg i)))
                       (assert (= (car arg)
                                  (+ (min-signal-variation model)
                                     (* i (signal-variation-resolution model)))))
                       (assert (pdf-p (second arg)))
                       (setf (aref pdf i) (second arg))))
                   (push (cons time pdf) (slot-value model 'correlation-pdf))))
                (t
                 (error "Coherence PDF provided outside correlation time")))))))
     ;; ensure correlation data is in dcesending time order
     (setf (slot-value model 'correlation-pdf)
           (sort
            (slot-value model 'correlation-pdf) #'> :key #'car))))
  t)

(defun draw-from-pdf(pdf)
  (let*((n (lens::%genintrand (length pdf) 0))
        (v (aref pdf n)))
    (tracelog "v(~D/~D)=~A" n (length pdf) v)
    (etypecase v
      (number v)
      (vector (draw-from-pdf v)))))

(defun pdf-index(model value)
  (max 0
       (min
        (1- (slot-value model 'no-signals))
        (floor (- value (min-signal-variation model))
               (signal-variation-resolution model)))))

(defmethod run-temporal-model((model temporal-model) time value)
  "Return the new signal variation value and remaining time processed by temporal model"
  (let ((*context* model))
  (if (or (zerop time) (>= time (coherence-time model)))
      (values (draw-from-pdf (coherence-pdf model)) time)
      (let ((remaining-time time))
        (dolist(p (correlation-pdf model))
          (let* ((correlation-time (car p))
                 (pdfs (cdr p)))
            (while (> remaining-time correlation-time)
              (decf remaining-time correlation-time)
              (setf value
                    (draw-from-pdf (aref pdfs (pdf-index model value)))))))
        (values value (- remaining-time time))))))
