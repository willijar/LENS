;; main WSN network definition
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

;; includes nodes, wireless channel and physical processes

;;; Code:
(in-package :lens.wsn)

(defclass WSN(Network)
  ((field :parameter t
          :type coord :reader field
          :initform (make-coord 30.0d0 30.0d0)
          :documentation "Size of network deployment field")
   (num-nodes :parameter t :type fixnum :reader num-nodes
              :initform 30 :documentation "Number of nodes in network")
   (num-physical-processes :parameter t :type fixnum
                           :reader num-physical-processes :initform 1
                           :documentation "Number of physical processes")
   (deployment :parameter t :type list :reader deployment :initform 'uniform
               :properties (:format read)
               :documentation "Node deployment specification. See [[mobility]] module on valid values for this and it's semantics."))
  (:metaclass compound-module-class)
  (:submodules
   ;; order matters here as nodes config depends on physical processes
   (wireless-channel wireless-channel)
   (physical-processes num-physical-processes physical-process)
   (node num-nodes node))
  (:documentation "Network for all Wirelesss sensor networks bringing
  together a [[wireless-channel]], a set of sensing [[nodes]]
  representing motes and a number of [[physical-process]]s being
  sensed.."))

(defgeneric nodes(network)
  (::documentation "* Arguments

- network :: a network

* Description

Returns a vector of all nodes in the wireless sensor network.")
  (:method((network WSN)) (submodule network 'node)))
