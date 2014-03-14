;;; LENS System definition -*- Lisp -*-
;; Copyright (C) 2007-2014 Dr. John A.R. Williams

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
    :long-description "A Network Simulator in LISP inspired by OMNET++"
    :depends-on (:split-sequence :data-format-validation
                 :trivial-gray-streams :clrs :closer-mop)
    :components
    ((:module "core"
              :components
              ((:file "defpackage")
               (:file "common" :depends-on ("defpackage"))
               (:file "object" :depends-on ("common"))
               (:file "queue" :depends-on ("signals" "module"))
               (:file "parameters" :depends-on ("object" "configuration"))
               (:file "configuration" :depends-on ("defpackage"))
               (:file "message" :depends-on ("object" "simulation"))
               (:file "compatibility" :depends-on ("defpackage"))
               (:file "component" :depends-on ("signals" "simulation"))
               (:file "rng" :depends-on ("defpackage" "object"))
               (:file "simulation"
                      :depends-on ("defpackage" "common" "compatibility"
                                    "parameters" "rng"))
               (:file "signals" :depends-on ("object"))
               (:file "timers" :depends-on ("module"))
               (:file "model-change" :depends-on ("defpackage"))
               (:file "gate" :depends-on
                      ("object" "model-change" "channel" "message"))
               (:file "channel" :depends-on ("signals" "component"))
               (:file "module" :depends-on
                      ("message" "gate" "component" "parameters" "simulation"))
               (:file "statistics" :depends-on
                      ("simulation" "parameters" "signals" "module"))
               (:file "statistics-impl" :depends-on ("statistics"))))))