;; Some useful definitions for power and error calculations
;; Copyright (C) 2014 Dr. John A.R. Williams

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

;; Note that Castelia used approximate formulaes - these are
;; used here if :castelia-compatibility is set (in *features*)

;;; Code:
(in-package :lens.wsn)

;; these are exact fp functions
#-castelia-compatability
(defun ratio-to-db(ratio)
  (if (> ratio 1e-10)
      (* 10.0 (log ratio 10.0))
      -100.0))

#-castelia-compatability
(defun db-to-ratio(db)
  (expt 10.0 (* db 0.1)))

#-castelia-compatability(defun dbm+(a b)
  (ratio-to-db (+ (db-to-ratio a) (db-to-ratio b))))

#-castelia-compatability(defun dbm-(a b)
  (ratio-to-db (- (db-to-ratio a) (db-to-ratio b))))

;;;; these are approximate versions used in Castalia
#+castelia-compatability(defun dbm+(a b)
  (let ((diff (- a b)))
    (cond
      ((> diff 7.0) a)
      ((< diff -7.0) b)
      ((> diff 5.0) (+ 1.0 a))
      ((< diff -5.0) (+ 1.0 b))
      ((> diff 3.0) (+ 1.5 a))
      ((< diff -3.0) (+ 1.5 b))
      ((> diff 2.0) (+ 2.0 a))
      ((< diff -2.0) (+ 2.0 b))
      ((> diff 1.0) (+ 2.5 a))
      ((< diff -1.0) (+ 2.5 b))
      ((> diff 0.0) (+ a 3.0))
      (t (+ b 3.0)))))

#+castelia-compatability(defun dbm-(a b)
  (let ((diff (- a b)))
    (cond
      ((< diff 0.5) -200.0)
      ((< diff 1.0) (- b 9.0))
      ((< diff 2.0) (- b 5.0))
      ((< diff 3.0) (- b 2.0))
      ((< diff 4.0) b)
      ((< diff 5.0) (+ b 2.0))
      ((< diff 6.0) (- a 1.6))
      ((< diff 7.0) (- a 1.2))
      ((< diff 8.0) (- a 0.9))
      ((< diff 9.0) (- a 0.7))
      ((< diff 12.0) (- a 0.5))
      (t a))))

#+castelia-compatability
(let ((a #(-12.041200 -9.030900 -7.269987 -6.020600 -5.051500 -4.259687
            -3.590219 -3.010300 -2.498775 -2.041200 -1.627273 -1.249387
            -0.901766 -0.579919 -0.280287)))
   (defun ratio-to-db(ratio)
     (cond
       ((>= ratio 1.0) 0.0)
       ((< ratio 0.0625) -100.0)
       (t (aref a (1- (floor (* 16 ratio)))))))

 (defun db-to-ratio(db)
   (cond
     ((> db 9.0) 8.0)
     ((> db 6.0) 4.0)
     ((> db 3.0) 2.0)
     ((> db 1.25) 1.3333)
     (t 1.0))))

(defvar +ideal-modulation-threshold+ 5.0)

(flet((db-to-ratio(db) (expt 10.0 (* db 0.1))))
(defgeneric snr2ber(encoding snr-db &optional bits-per-noise-bandwidth)
  (:documentation "* Arguments

- encoding :: a +symbol+ or array of [[custom-coding]]
- snr-db :: a +real+
- bits-per-noise-bandwidth :: a +real+

* Description

Given a particular /encoding/, signal to noise ratio in db /snr-db/
and the ratio of data rate to noise bandwidth
/bits-per-noise-bandwidth/ return the bit error rate")
  (:method((encoding (eql 'fsk)) snr-db &optional bpnb)
    (* 0.5 (exp (* -0.5 (/ (db-to-ratio snr-db) bpnb)))))
  (:method((encoding (eql 'psk)) snr-db &optional bpnb)
    (* 0.5 (erfc (sqrt (/ (db-to-ratio snr-db) bpnb)))))
  (:method((encoding (eql 'diffdpsk)) snr-db &optional bpnb)
    (* 0.5 (exp (/ (db-to-ratio snr-db)  bpnb))))
  (:method((encoding (eql 'ideal)) snr-db &optional bpnb)
    (declare (ignore bpnb))
    (if (< snr-db +ideal-modulation-threshold+) 1.0d0 0.0d0))))

(defun probability-of-exactly-N-errors(ber num-errors num-bits)
  (declare (double-float ber) (fixnum num-errors) (fixnum num-bits))
  (cond
    ((= num-errors 0)
     (expt (- 1.0d0 ber) num-bits))
    ((>= num-errors num-bits)
     (expt ber num-bits))
    (t
     (when (> num-errors (/ num-bits 2))
       (setf num-errors (- num-bits num-errors)))
     (do((i 1 (1+ i))
         (combinations 1.0d0 (* (/ combinations i) (- (1+ num-bits) i))))
        ((>= i num-errors)
         (* combinations
            (expt ber num-errors)
            (expt (- 1.0d0 ber) (- num-bits num-errors))))))))

(defun erfc(x) (- 1d0 (erf x)))
(defun erfcinv(x) (erfinv (- 1d0 x)))

(defun erf(x)
  "Error function calculation. This is a translation of a FORTRAN
  program by W. J. Cody,Argonne National Laboratory, NETLIB/SPECFUN,
  March 19, 1990.  The main computation evaluates near-minimax
  approximations from 'Rational Chebyshev approximations for the error
  function' by W. J. Cody, Math. Comp., 1969, PP. 631-638."
  (let* ((xbreak 0.46875d0)
	 (y (abs x))
	 (result
	  (cond
	   ((<= y xbreak)	  ; evaluate  erf  for  |x| <= 0.46875
	    (let* ((a #(3.16112374387056560d00  1.13864154151050156d02
			3.77485237685302021d02  3.20937758913846947d03
			1.85777706184603153d-1))
		   (b #(2.36012909523441209d01 2.44024637934444173d02
			1.28261652607737228d03 2.84423683343917062d03))
		   (z (* y y))
		   (xnum (* z (aref a 4)))
		   (xden z))
	      (dotimes (i 3)
		(setq xnum (* (+ (aref a i) xnum) z))
		(setq xden (* (+ (aref b i) xden) z)))
	      (/ (* x (+ xnum (aref a 3))) (+ xden (aref b 3)))))
	   ((<= y 4)	   ; evaluate  erfc  for 0.46875 <= |x| <= 4.0
	    (let* ((c #(5.64188496988670089d-1 8.88314979438837594d00
			6.61191906371416295d01 2.98635138197400131d02
			8.81952221241769090d02 1.71204761263407058d03
			2.05107837782607147d03 1.23033935479799725d03
			2.15311535474403846d-8))
		   (d #(1.57449261107098347d01 1.17693950891312499d02
			5.37181101862009858d02 1.62138957456669019d03
			3.29079923573345963d03 4.36261909014324716d03
			3.43936767414372164d03 1.23033935480374942d03))
		   (xnum (* y (aref c 8)))
		   (xden y))
	      (dotimes (i 7)
		(setq xnum (* (+ (aref c i) xnum) y))
		(setq xden (* (+ (aref d i) xden) y)))
	      (/ (+ xnum (aref c 7)) (+ xden  (aref d 7)))))
	   (t			       ; evaluate  erfc  for |x| > 4.0
	    (let* ((p #(3.05326634961232344d-1 3.60344899949804439d-1
			1.25781726111229246d-1 1.60837851487422766d-2
			6.58749161529837803d-4 1.63153871373020978d-2))
		   (q #(2.56852019228982242d00 1.87295284992346047d00
			5.27905102951428412d-1 6.05183413124413191d-2
			2.33520497626869185d-3))
		   (z (/ 1 (* y y)))
		   (xnum (* (aref p 5) z))
		   (xden z))
	      (dotimes (i 4)
		(setq xnum (* (+ (aref p i) xnum) z))
		(setq xden (* (+ (aref q i) xden) z)))
	      (let ((result (* z (/ (+ xnum  (aref p 4))
				    (+ xden (aref q 4))))))
		(/ (- (/ 1 (sqrt pi)) result) y))))))
	 (z (/ (floor (* y 16)) 16))
	 (dl (* (- y z) (+ y z)))
	 (r (* (exp (- (* z z))) (exp (- dl)) result)))
    (cond ((> x xbreak)
	   (+ (- 0.5 r) 0.5))
	  ((< x (- xbreak))
	   (- (- r 0.5) 0.5))
	  (t result))))

(defun erfinv(y)
  "ERFINV Inverse error function.

  X = ERFINV(Y) is the inverse error function for each element of X.
  The inverse error functions satisfies y = erf(x), for -1 <= y < 1
  and -inf <= x <= inf."
  (declare (double-float y))
  ; Exceptional cases.
  (when (> (abs y) 1.0d0)
    (error 'arithmetic-error :operation 'erfinv :operands y))
  (when (or (= y -1.0d0) (= y 1.0d0))
    (error 'floating-point-overflow :operation 'erfinv :operands y))
  ; Coefficients in rational approximations.
  (let* ((a #( 0.886226899d0 -1.645349621d0  0.914624893d0 -0.140543331d0))
	 (b #(-2.118377725d0  1.442710462d0 -0.329097515d0  0.012229801d0))
	 (c #(-1.970840454d0 -1.624906493d0  3.429567803d0  1.641345311d0))
	 (d #( 3.543889200d0  1.637067800d0))
	 (y0  .7)
	 (x (cond
	     ((<= (abs y) y0) ; central range
	      (let ((z (* y y)))
		(* y (/ (+ (aref a 0)
			   (* z (+ (aref a 1)
				   (* z (+ (aref a 2)
					   (* z (aref a 3)))))))
			(+ 1
			   (* z (+ (aref b 0)
				   (* z (+ (aref b 1)
					   (* z (+ (aref b 2)
						   (* z (aref b 3)))))))))))))
	     ((and (< y0 y) (< y 1)) ;end point
	      (let ((z (sqrt (- (log (/ (- 1 y) 2))))))
		(/ (+ (aref c 0)
		      (* z (+ (aref c 1)
			      (* z (+ (aref c 2)
				      (* z (aref c 3)))))))
		   (+ 1
		      (* z (+ (aref d 0)
			      (* z (aref d 1))))))))
	     ((and (> (- y0) y) (> y -1)) ; other end point
	      (let ((z (sqrt (- (log (/ (+ 1 y) 2))))))
		(- (/ (+ (aref c 0)
			 (* z (+ (aref c 1)
				 (* z (+ (aref c 2)
					 (* z (aref c 3)))))))
		      (+ 1
			 (* z (+ (aref d 0)
				 (* z (aref d 1))))))))))))
    ; Two steps of Newton-Raphson correction to full accuracy.
    ; Without these steps, erfinv(y) would be about 3 times
    ; faster to compute, but accurate to only about 6 digits.
    (dotimes (i 2)
      (setq x (- x (/ (- (erf x) y) (* (/ 2 (sqrt pi)) (exp (-(* x x))))))))
    x))

(let ((ber-array
       (coerce
  #(0.01723590060469	;; BER for SNR 6.0dB  PER(200bits) = 0.03
		0.01515859222543	;; BER for SNR 6.2dB
		0.01326127169356	;; ...
		0.01153737654219
		0.00997961569163
		0.00858004434130	;; BER for SNR 7.0dB
		0.00733015079524
		0.00622095368274
		0.00524310766416
		0.00438701538062
		0.00364294312896	;; BER for SNR 8.0dB
		0.00300113753889
		0.00245194041114
		0.00198589885920
		0.00159386799020
		0.00126710356868	;; BER for SNR 9.0dB
		0.00099734242733
		0.00077686881248
		0.00059856536337
		0.00045594799891
		0.00034318459603	;; BER for SNR 10.0dB
		0.00025509795641
		0.00018715413812
		0.00013543774315
		0.00009661616875
		0.00006789512818	;; BER for SNR 11.0dB
		0.00004696790793
		0.00003196084958
		0.00002137743081
		0.00001404308722
		0.00000905258912	;; BER for SNR 12.0dB
		0.00000572139679)	;; BER for SNR 12.2dB  PER(4000bits) = 0.9773
  '(array float 1))))
(defmethod snr2ber((encoding (eql 'diffqpsk)) snr-db &optional bpnb)
  (declare (ignore bpnb) (float snr-db))
	;; The values of the SNR parameter should be within 6.0 and 12.2 dB if not
	;; we should issue a warning. here we just return appropriate values
  (cond
    ((< snr-db 6.0) 1.0d0)
    ((>= snr-db 12.2) 0.0d0)
    (t
     (multiple-value-bind(index a) (floor (- snr-db 6.0) 0.2)
       (+ (* a (aref ber-array index))
          (* (- 1 a) (aref ber-array (1+ index)))))))))

(defmethod snr2ber((custom array) snr-db &optional bpnb)
  (declare (ignore bpnb) (type float snr-db))
  (let ((n (1- (length custom))))
    (flet ((snr(i) (custom-modulation-snr (aref custom i))))
      (cond
        ((> (snr 0) snr-db) 0.0)
        ((< (snr n) snr-db) 1.0)
        (t
         (dotimes(i n)
           (when (>= (snr (1+ i)) snr-db)
             (if (> (- (snr (1+ i)) snr-db)  (- (snr i) snr-db))
                 (return (custom-modulation-ber (aref custom i)))
                 (return (custom-modulation-ber (aref custom (1+ i))))))))))))
