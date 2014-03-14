;; Communications compound module definitions for WSN
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

;;

;;; Code:
(in-package :lens.wsn)

(defclass communications(compound-module)
  ()
  (:gates
   (application :inout)
   (receive :input))
  (:submodules
   (routing routing)
   (mac mac)
   (radio radio))
  (:connections
   (<=> application (routing application))
   (<=> (routing mac) (mac routing))
   (<=> (mac radio) (radio mac))
   (<= (radio receive) receive))
  (:metaclass compound-module-class)
  (:documentation "Communications module"))

(defclass communications-control-message(message)
  ((name :accessor command :initarg :command
         :documentation "Command is held as message name")
    (argument :accessor argument :initarg :argument
             :documentation "Additional arguments with command"))
  (:documentation "Base class for all communications control
  messages (information going up to higher layers). We create classes
  for these so the layers can specialize handle-message on them and
  pick out those they need"))

(defclass network-control-message(communications-control-message)
  ())

(defclass mac-control-message(communications-control-message)
  ())

(defclass radio-control-message(communications-control-message)
  ())

(defclass communications-control-command(message)
  ((name :accessor command :initarg :command
         :documentation "Command is held as message name")
   (argument :accessor argument :initarg :argument
             :documentation "Additional arguments with command"))
  (:documentation "Base class for all communications control
  commands (commands going down to lower layers."))

(defclass network-control-command(communications-control-command)
  ())

(defclass mac-control-command(communications-control-command)
  ())

(defclass radio-control-command(communications-control-command)
  ())

(define-condition invalid-command(warning)
  ((module :reader module :initarg :module)
   (command :reader command :initarg :command)
   (argument :reader argument :initarg :argument))
  (:report (lambda(c os)
             (format os "Invalid command ~A on ~A with argument ~A"
                     (command c) (module c) (argument c)))))

(defgeneric handle-control-command(module command argument)
  (:documentation "Breakdown of MAC commands - return true if successful")
  (:method(module command argument)
    (warn 'invalid-command :module module :command command :argument argument)))
