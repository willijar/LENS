;; Deployment and mobility for wireless sensor networks
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

(defstruct orientation
  "Structure representing orientation of a node - may useful if
directional antennas are to be modelled."
  (phi 0.0 :type float :read-only t)
  (theta 0.0 :type float :read-only t))

(defmethod dfv::parse-input((spec (eql 'orientation)) value
                            &key &allow-other-keys)
  (let ((coords
         (dfv::parse-input 'list value
                      :type '(number :coerce-to float)
                      :min-length 2 :max-length 2)))
    (make-orientation :phi (first coords)
                      :theta (second coords))))

(defclass mobility(wsn-module)
  ((location :type coord :parameter t :initarg :location :reader location
             :documentation "Initial location [[coord]] for node.")
   (orientation
    :type orientation :parameter t :initarg :orientation :reader orientation
    :initform (make-orientation)
    :documentation "Initial [[orientation]] for [[node]]")
   (static-p :initform t :initarg :static-p :reader static-p
             :documentation "If true (default) this the node is
             static. Subclasses should specialise this
             appropriately."))
  (:metaclass module-class)
  (:documentation "* Description

Superclass for [[mobility]] submodules of a [[node]] handling node
mobility. Base class of this initialises the location from the
[[deployment]] pand [[field]] arameters of the network to initialise
all node positions for static nodes. Deployment takes the current
forms.

* Network Parameters

- field :: a /list/ of the /x/, /y/ and optionally /z/ sizes of the network in m
- deployment :: a symbol /deployment-type/ for the whole network
       or a [[range-list]] mapping node ids to the /deployment-type/
       to nodes.

* Deployment Types

Specify how nodes are located over the network [[field]].

- uniform :: random uniform deployment over entire field.
- center :: located in the center of the field
- (grid dx dy dz) :: nodes are located on a grid with cell dimensions (dz dy dz)
- (randomized dx dy dz) :: nodes are randomly located withing
   the cells of dimensions (dx dy dz)
"))

(defmethod initialize-instance :after ((instance mobility)
                                       &key &allow-other-keys)
  (unless (slot-boundp instance 'location)
    (parse-deployment instance)))

(defgeneric (setf location)(location module)
  (:documentation "* Arguments

- location :: a [[coord]]
- module :: a [[mobility]] module

* Description

Updates the position of the node of the mboility module. Emits the
+node-move+ signal with the new locations for modules which need to
now about changes in location (such as [[wireless-channel]] or
[[sensor]]) to [[subscribe]] to.")
  (:method((location coord) (instance mobility))
    (assert (not (static-p instance))
            ()
            "Attempt to change location of static node ~A" (node instance))
    (tracelog "changed location to ~A" location)
    (setf (slot-value instance 'location) location)
    (emit instance 'node-move location)
    location))

(defmethod initialize list ((instance mobility) &optional (stage 0))
  (case stage
    (0  (tracelog "initial location is ~A" (location instance))))
  t)

(defun parse-deployment(instance)
  (let* ((node (node instance))
         (network (network node))
         (xlen (coord-x (field network)))
         (ylen (coord-y (field network)))
         (zlen (coord-z (field network))))
    (multiple-value-bind(deployment start-index)
        (let ((deployment (deployment network)))
          (if (range-list-p deployment)
              (range-getf (deployment network) (index node))
              (values deployment 0)))
      (when deployment
        (setf
         (slot-value instance 'location)
         (cond
           ((eql deployment 'uniform)
            (make-coord (uniform 0 xlen) (uniform 0 ylen) (uniform 0 zlen)))
           ((eql deployment 'center)
            (make-coord (/ xlen 2) (/ ylen 2) (/ zlen 2)))
           ((and (listp deployment)
                 (member (first deployment) '(grid randomized)))
            (let* ((gridi (- (index node) start-index))
                   (gridx (second deployment))
                   (gridy (third deployment))
                   (gridz (fourth deployment))
                   (grid
                    (make-coord
                     (* (mod gridi gridx) (/ xlen (1- gridx)))
                     (* (mod (floor gridi gridx) gridy) (/ ylen (1- gridy)))
                     (if (and gridz zlen)
                         (* (mod (floor gridi (* gridx gridy)) gridz)
                            (/ zlen (1- gridz)))
                         0.0))))
              (if (eql (first deployment) 'randomized)
                  (flet((rn(len grid)
                          (let ((s (/ len grid)))
                            (min (- s) (max s (normal 0 (* 0.3 s)))))))
                    (coord+
                     grid
                     (make-coord
                      (rn xlen gridx)
                      (rn ylen gridy)
                      (if (and gridz zlen) (rn zlen gridz) 0.0))))
                grid)))
         (t (error "Unknown deployment parameter: ~A" deployment))))))))
