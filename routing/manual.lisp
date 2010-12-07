;;;; Copyright (C) 2010 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :layer3)

(defclass routing-manual(fib)
  ()
  (:documentation "Manual routing. The routing protocol doesn't manage
routing - the user must add the entries explicitly."))

(defmethod reinitialise-routes((routing routing-manual) changed)
  (declare (ignore changed)))

(defmethod getroute((address network-address) (routing routing-manual)
                      &key &allow-other-keys)
  (or (call-next-method)
      (default-route routing)))