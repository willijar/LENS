;;; LENS System definition -*- Lisp -*-
;; $Id$
;; Copyright (C) 2007 Dr. John A.R. Williams

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
    :long-description "A Network Simulator is LISP inspired by GTNetS"
    :depends-on (:split-sequence :trivial-gray-streams)
    :components
    ((:module "core"
              :components
              ((:file "defpackage")
               (:file "queues" :depends-on ("defpackage"))
               (:file "common" :depends-on ("defpackage"))
               (:file "random" :depends-on ("defpackage"))
               (:file "statistics" :depends-on ("common" "scheduler"))
               (:file "compatibility" :depends-on ("defpackage"))
               (:file "address" :depends-on ("defpackage" "common"))
               (:file "trace" :depends-on ("defpackage" "common" "scheduler"))
               (:file "scheduler"
                      :depends-on
                      ("defpackage" "common" "compatibility" "queues"))
               (:file "protocol"
                      :depends-on ("address" "common" "packet-queue"))
               (:file "node"
                      :depends-on ("common" "address" "routing" "protocol"))
               (:file "application"
                      :depends-on ("common" "scheduler" "protocol"))
               (:file "packet" :depends-on ("defpackage" "scheduler"))
               (:file "packet-queue"
                      :depends-on ("defpackage" "packet" "scheduler"))
               (:file "interface"
                      :depends-on ("defpackage" "packet-queue" "link"))
               (:file "link"
                      :depends-on ("common" "scheduler" "queues" "random"
                                   "address" "packet-queue" "node"))
               (:file "routing" :depends-on ("common" "address"))
               (:file "user"
                      :depends-on ("common" "address" "scheduler" "link"
                                            "node"))))
     (:module "nodes" :depends-on ("core")
              :components
              ((:file "node")))
     (:module "links" :depends-on ("core")
              :components
              ((:file "p2p")
               (:file "ethernet")))
     (:module "applications" :depends-on ("core" "protocols" "routing")
              :components
              ((:file "cbr-source")
               (:file "udp-sink")
               (:file "tcp-source")
               (:file "tcp-server")))
     (:module "protocols" :depends-on ("core")
              :components
              ((:file "llcsnap")
               (:file "ieee802.3" :depends-on ("llcsnap"))
               (:file "defpackage")
               (:file "ipv4" :depends-on ("defpackage"))
               (:file "udp" :depends-on ("ipv4"))
               (:file "tcp" :depends-on ("ipv4"))
               (:file "icmp" :depends-on ("tcp"))
               (:file "tcp-tahoe" :depends-on ("tcp"))
               (:file "tcp-reno" :depends-on ("tcp"))
               (:file "tcp-newreno" :depends-on ("tcp-reno"))
               (:file "arp")))
     (:module "routing" :depends-on ("core")
              :components
              ((:file "dijkstra")
               (:file "manual")
               (:file "static" :depends-on ("manual" "dijkstra"))))
     (:module "graphics" :depends-on ("core")
              :components
              ((:file "defpackage")
               (:file "dot" :depends-on ("defpackage"))))))


(declaim (optimize (debug  3)))