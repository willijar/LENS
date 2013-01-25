(in-package :protocol.layer4.tcp)

  (defclass tcp-cubic (tcp)
  (
 (fast-recovery :type boolean :initform nil :accessor fast-recovery
                  :documentation "True if fast recovery in progress")
  (epoch-start :accessor epoch-start :initform 0  :initarg :epoch-start
	  :documentation "elapsed time from last congestion")
  (w-max
  :type (unsigned-byte 16) :initform 0 :accessor w-max :initarg :w-max
    :documentation " w-max is value of w when congestion has occured")
  (w-last-max
   :type (unsigned-byte 16) :accessor w-last-max :initform 0 :initarg :w-last-max
   :documentation "  w-last-max is the value of w before last w-max (sectioin 3.6)")

  (wtcp
    :type (unsigned-byte 16) :accessor wtcp :initform 0 
    :documentation " wtcp is the congestion window size of normal tcp")
  (k 
	:accessor k :initform 0)
  (ti 
    :accessor ti :initform 0)
  (wind
    :accessor wind :initform 0)
	(dmin 
	:accessor dmin :initform 0)
  (loss-indicator
	:accessor loss-indicator :initform 0 )
  (recover :type (unsigned-byte 16) :accessor recover
                  :documentation "High tx mark for new reno")
  (partial-ack-count :initform 0 :accessor partial-ack-count
                      :documentation "Number of parial acks in a row")
  (rtt1 :initform 0 :accessor rtt1)
  (x :initform 0 :accessor x)
(w-last-max2 :initform 0 :accessor w-last-max2)
(loss-count :initform 0 :accessor loss-count)

  
 )
  (:documentation "Cubic TCP implementation"))

(defmethod new-ack ((tcp tcp-cubic) &key packet &allow-other-keys)

  (let ((ack-number (ack-number (peek-pdu packet)))
        (seg-size (maximum-segment-size tcp))
        (slow-start-threshold  (slow-start-threshold tcp))
        (skip-timer nil))

	
		
	   
	   
    (cond
      ((fast-recovery tcp)
       ;; if in fast recovery check for full or partial ack as per rfc 3781
       (cond
         ((>= ack-number (recover tcp)) ;; full ack
          (setf (fast-recovery tcp) nil
                (congestion-window tcp) (min slow-start-threshold
                                             (+ seg-size (unack-data-count tcp)))
                (partial-ack-count tcp) 0))
         (t ;; partial ack
          (decf (congestion-window tcp) (seq- ack-number (highest-rx-ack tcp)))
          (when (>= (seq- ack-number (highest-rx-ack tcp)) seg-size)
            (incf  (congestion-window tcp) seg-size))
          (setf (highest-rx-ack tcp) ack-number)
          (setf skip-timer (> (partial-ack-count tcp) 0))
          (incf (partial-ack-count tcp))
          (retransmit tcp))))
      ((< (congestion-window tcp) slow-start-threshold)
       ;; Slow start mode, add one segSize to cWnd
        (incf (congestion-window tcp) seg-size) 

















)
      (t ;; Congestion avoidance mode, adjust by (ackBytes*segSize) / cWnd
	




                       ;; (if  (= (loss-indicator tcp) 1) (setf (epoch-start tcp)(last-rx-time tcp)))
			(setf (ti tcp) (+ (- (last-rx-time tcp) (epoch-start tcp)) (rtt1 tcp)));; elapsed time from last congestion 
	       		;;(if ()()
			;;(setf (wtcp tcp) (+ (* (w-max tcp) (- 1 0.2))(/ (*(* 3 0.2 ) (ti tcp)) (*(- 2 0.2) (rtt1 tcp) )))))

			(if (< (congestion-window tcp) (wtcp tcp))
				 	(setf (congestion-window tcp) (wtcp tcp))
					;;else
					(progn
						
						
						
						
						(setf (k tcp) (expt (/ (* (w-max tcp) 1.65) 0.04) 1/3))
						
						(setf (wind tcp)  (+ (w-max tcp) (* 0.04 (expt (- (ti tcp) (k tcp)) 3))))
						(incf (congestion-window tcp) (/ (abs (- (wind tcp)  (congestion-window tcp))) (congestion-window tcp)))
						(setf (x tcp) (+ (x tcp) 1))
						(setf (loss-indicator tcp) 0)






					)

                        )
			 
			 
			 
			 






      

))
    (call-next-method tcp :packet packet :skip-timer skip-timer)))
;;************************************************************************************************************************************************************
;;************************************************************************************************************************************************************
   
(defmethod dup-ack((tcp tcp-cubic) header c)
(setf (x tcp) (+ (x tcp) 1))

;;(setf (y tcp) (+ (y tcp) 1))
  (let ((seg-size (maximum-segment-size tcp)))
    (cond
      ((fast-recovery tcp)
       ;; inflate congestion window with every dup ack and send data
       (incf (congestion-window tcp) seg-size)
       (send-pending-data tcp))
      ((= c 3)
       ;; triple dupack received - enter fast recovery mode as per RFC2581
        
	(progn (setf (loss-count tcp) (+ (loss-count tcp) 1))
        (setf (w-last-max tcp) (w-max tcp));;remember last value of w-max********************************************************************	
        (setf (loss-indicator tcp) 1 );;indicate that congestion has occured
	(setf (w-max tcp) (congestion-window tcp));;remember the window size before reduction,section 3.5
    	(if (< (w-max tcp) (w-last-max tcp) ) (setf (w-max tcp) (* (/ (- 2 0.2) 2) (w-max tcp)))) 
	(setf (epoch-start tcp) (last-rx-time tcp));;remember the time that congestion has occured
      ;;((fast-recovery tcp) t)
        (setf (slow-start-threshold tcp) (* (congestion-window tcp) 0.65))
        (setf (congestion-window tcp) (* (congestion-window tcp) 0.65));;window reduction, section 3.5
            
        (setf (recover tcp) (next-tx-seq tcp)))
			 
       (retransmit tcp))))

)

;;###########################################################################################################################################################
(defmethod retransmit-packet-timeout((tcp tcp-cubic))
   (setf (w-last-max tcp) 0) (setf (loss-indicator tcp) 0) (setf (partial-ack-count tcp) 0) 
   (setf (ti tcp) 0) (setf (wind tcp) 0)
   (setf (wtcp tcp) 0) (setf (k tcp) 0) (setf (epoch-start tcp) 0) 
   (call-next-method))
;;###########################################################################################################################################################   
   
    

