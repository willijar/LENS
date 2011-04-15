;;; LENS System definition -*- Lisp -*-
;; Copyright (C) 2007-2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :cl-user)

(asdf:defsystem :lens
    :description "Lisp Educational Network Simulator (LENS)"
    :author "Dr. John A.R. Williams <J.A.R.Williams@aston.ac.uk>"
    :maintainer "Dr. John A.R. Williams <J.A.R.Williams@aston.ac.uk>"
    :licence "GPL v3"
    :version "2.0"
    :long-description "A Network Simulator in LISP inspired by GTNetS and NS2"
    :depends-on (:split-sequence :trivial-gray-streams :clrs :closer-mop)
    :components
    ((:module "core"
              :components
              ((:file "defpackage")
               (:file "common" :depends-on ("defpackage"))
               (:file "random" :depends-on ("defpackage"))
               (:file "compatibility" :depends-on ("defpackage"))
               (:file "scheduler" :depends-on
                      ("defpackage" "common" "compatibility"))
               (:file "packet" :depends-on ("defpackage" "scheduler"))
               (:file "address" :depends-on ("defpackage" "common"))
               (:file "packet-queue"
                      :depends-on ("defpackage" "common" "scheduler" "packet"
                                                "protocol"))
               (:file "protocol"
                      :depends-on ("address" "common" "packet"))
               (:file "interface"
                      :depends-on ("defpackage" "packet-queue" "link"))
               (:file "trace" :depends-on
                      ("defpackage" "common" "scheduler" "compatibility"
                                    "protocol"))
               (:file "routing" :depends-on ("common" "address"))
               (:file "node"
                       :depends-on ("common" "address" "protocol" "routing"))
               (:file "link"
                       :depends-on ("common" "scheduler" "random"
                                    "address" "packet-queue" "node"))
               (:file "application"
                       :depends-on ("common" "scheduler" "protocol" "node"))
               (:file "statistics" :depends-on ("common" "scheduler"))
               (:file "user"
                       :depends-on ("common" "address" "scheduler" "link"
                                             "node"))))
     (:module "nodes" :depends-on ("core")
              :components ((:file "node")))
     (:module "layer1" :depends-on ("core")
              :components
              ((:file "p2p")))
     (:module "layer2" :depends-on ("core")
              :components
              ((:file "arp")
               (:file "llcsnap")
               (:file "ieee802.3" :depends-on ("llcsnap"))))
     (:module "layer3" :depends-on ("core")
              :components
              ((:file "icmp")
               (:file "ipv4" :depends-on ("icmp"))
               (:file "fib")
               (:file "manual" :depends-on ("fib"))
               (:file "static" :depends-on ("fib"))))
     (:module "layer4" :depends-on ("layer3" "core")
              :components
              ((:file "udp")
               (:file "tcp")))
     (:module "layer5" :depends-on ("core")
              :components
              ((:file "abr-source")
               (:file "udp-sink")))))
