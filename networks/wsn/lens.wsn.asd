;;; LENS System definition -*- Lisp -*-
;; Copyright (C) 2013 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :cl-user)

(asdf:defsystem :lens.wsn
    :description "Wireless Sensor Networks for Lisp Educational
    Network Simulator (LENS)"
    :author "Dr. John A.R. Williams <J.A.R.Williams@aston.ac.uk>"
    :maintainer "Dr. John A.R. Williams <J.A.R.Williams@aston.ac.uk>"
    :licence "GPL v3"
    :depends-on (:lens)
    :components
    ((:module "core"
              :components
              ((:file "defpackage")
               (:file "common" :depends-on ("defpackage"))
               (:file "physical-process" :depends-on ("defpackage"))
               (:file "network" :depends-on ("defpackage"))
               (:file "mobility" :depends-on ("common" "network"))
               (:file "resources" :depends-on ("defpackage"))
               (:file "sensor" :depends-on ("physical-process" "mobility"))
               (:file "node" :depends-on ("common" "communication"))
               (:file "communication" :depends-on ("defpackage"))
               (:file "wireless-channel" :depends-on ("mobility"))
               (:file "application" :depends-on ("common" "node" "sensor"))))
     (:module "communications" :depends-on ("core")
              :components
              ((:file "common")
               (:file "routing-base" :depends-on ("common"))
               (:file "mac-base" :depends-on ("common" "routing-base"))
               (:file "radio-support-functions")
               (:file "radio-base"
                      :depends-on
                      ("common" "mac-base" "radio-support-functions"))
               (:module "routing" :depends-on ("routing-base")
                       :components
                       ((:file "bypass-routing")
                        (:file "multipath-rings-routing")))
               (:module "mac" :depends-on ("mac-base" "routing-base")
                       :components
                       ((:file "bypass-mac")
                        (:file "tuneable-mac")
                        (:file "tmac")))))
     (:module "application" :depends-on ("core")
              :components
              ((:file "value-reporting" :depends-on ("value-propagation"))
               (:file "value-propagation")
               (:file "throughput-test")))
     (:module "physical-process" :depends-on ("core")
              :components
              ((:file "scenario")
               (:file "direct")))
     (:module "mobility" :depends-on ("core")
              :components
              ((:file "line-mobility")
               (:file "no-mobility")))))