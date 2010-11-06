;; $Id$
;; <description>
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Compatibility layer for different Lisp implementations
;; Providing threading interface

;;; Code:

(in-package :scheduler)

(defun make-thread(function name)
  #+sb-thread(sb-thread:make-thread function :name name))

(defun kill-thread(thread)
  #+sb-thread(sb-thread:destroy-thread thread))

(defun yield-thread()
  #+sb-thread(sb-thread::release-foreground))

(in-package :trace)

(import 'sb-gray:stream-line-length)