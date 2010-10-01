;; $Id$
;; Media Access Control
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer.link)

(defconstant ethertype-ip #x0800)
(defconstant ethertype-arp #x0806)
(defenumeration (mac-state (unsigned-byte 32))
  ((mac-idle     #x0000)
   (mac-polling  #x0001)
   (mac-recv     #x0010)
   (mac-send     #x0100)
   (mac-rts      #x0200)
   (mac-cts      #x0400)
   (mac-ack      #x0800)
   (mac-coll     #x1000)))

(defenumeration (mac-frame-type (unsigned-byte 32))
  ((mf-beacon   #x0008)
   (mf-control  #x0010)
   (mf-slots    #x001a)
   (mf-rts      #x001b)
   (mf-cts      #x001c)
   (mf-ack      #x001d)
   (mf-cf-end   #x001e)
   (mf-poll     #x001f)
   (mf-data     #x0020)
   (mf-data-ack #x0021)))

(defclass mac-header(pdu)
  ((mac-frame-type :type mac-frame-type :initarg :frame-type)
   (source-address :type mac-addr :initarg :source-address)
   (destination-address :type mac-addr :initarg :destination-address)
   (transmission-time :type time-type :initform *current-time* :initarg :transmission-time)
   (slot-start-time :type time-type :initarg :slot-start-time)))


(defclass mac(protocol)
  ((mac-address :type mac-addr :initarg :mac-address :reader mac-address)
   (bandwidth :type real :initform 10e6 :initarg :bandwidth :reader bandwidth)
   (delay :type time-type :initform 0 :initarg :delay :reader delay
          :documentation "MAC overhead")
   (interface :type physical-interface :initarg :interface :reader interface)
   (taps :initform nil :type list :initarg :taps :accessor taps)
         :documentation "List of functions to be called for all packets received by host")
   (state :type mac-state :accessor mac-state))