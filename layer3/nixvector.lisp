;;;; nixvector routing
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :routing)

(defclass nixvector-option(pdu)
  ((nixvector :type unsigned-byte :reader nixvector :initarg :nixvector
              :documentation "The nix vector")
   (used :type fixnum :initarg :used
         :documentation "Used portiuon of this nix vector" )
   (size :type fixnum :initarg :size
         :documentation "Total size of this nixvector")))


(defmethod size((pdu nixvector-option)) 0)

(defmethod copy((pdu nixvector-option))
  (with-slots(nixvector used size) pdu
    (make-instance 'nixvector-option
                   :nixvector nixvector :used used :size size)))

