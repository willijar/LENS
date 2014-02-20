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

   ;; PDF
   (coherence-pdf :type vector :reader coherence-pdf
                  :documentation "Coherence PDF if previous signal unknown or time passed > coherence time")
   (correlation-pdf
    :type list :reader correlation-pdf
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
        (setf (slot-value name 'instance) (getf params name))))
    (let ((n (1+ (/ (- (max-signal-variation model)
                       (min-signal-variation model))
                    (signal-variation-resolution model)))))
      (loop
         (let* ((pdf (read is :eof-error-p nil))
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
                (pdf (make-array n)))
            (do ((i 0 (1+ i)))
                ((< i n))
              (assert (= (car arg)
                         (+ (min-signal-variation model)
                            (* i (signal-variation-resolution model)))))
              (assert (pdf-p (second arg)))
              (setf (aref pdf i) (second arg)))
            (push (cons time pdf) (slot-value model 'correlation-pdf)))
          )
         (t
          (error "Coherence PDF provided outside correlation time")))))))
  ;; ensure correlation data is in dcesending time order
  (setf (slot-value model 'correlation-pdf)
        (sort
         (slot-value model 'correlation-pdf) #'> :key #'car))))
  t)

(defun draw-from-pdf(pdf)
  (let((v (aref pdf (intuniform 0 (length pdf)))))
    (etypecase v
      (number v)
      (vector (draw-from-pdf v)))))

(defun pdf-index(model value)
  (max 0
    (min (/ (- (max-signal-variation model)
                       (min-signal-variation model))
            (signal-variation-resolution model))
       (floor (- value (min-signal-variation model))
              (signal-variation-resolution model)))))

(defmethod run-temporal-model((model temporal-model) time value)
  "Return the new signal variation value and remaining time processed by temporal model"
  (if (or (zerop time) (>= time (coherence-time model)))
      (values (draw-from-pdf (coherence-pdf model)) time)
      (let ((remaining-time time))
        (dotimes(i (length (correlation-pdf model)))
          (let* ((p (aref (correlation-pdf model) i))
                 (correlation-time (car p))
                 (pdfs (cdr p)))
            (while (> remaining-time correlation-time)
              (decf remaining-time correlation-time)
              (setf value
                    (draw-from-pdf (aref pdfs (pdf-index model value)))))))
        (values value (- remaining-time time)))))