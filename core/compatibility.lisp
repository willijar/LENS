;; Compatibility layer for different Lisp implementations
;; Copyright (C) 2013-2014 Dr. John A.R. Williams

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

(in-package :lens)

(defun make-thread(function name)
  "Create a new thread of =name= that runs =function= with the argument
  list designator provided (defaults to no argument). When the function
  returns the thread exits.

* Notes
Compatibility layer function currently defined only for SBCL implementation.
 "

  #+sb-thread(sb-thread:make-thread function :name name))

(defun kill-thread(thread)
"Terminate the thread identified by =thread=, by interrupting it and causing
it to call =quit=.

The unwind caused by [[[kill-thread]] is asynchronous, meaning that eg. thread
executing

;;; (let (foo)
;;;    (unwind-protect
;;;        (progn
;;;           (setf foo (get-foo))
;;;           (work-on-foo foo))
;;;      (when foo
;;;        ;; An interrupt occurring inside the cleanup clause
;;;        ;; will cause cleanups from the current UNWIND-PROTECT
;;;        ;; to be dropped.
;;;        (release-foo foo))))

might miss calling =release-foo= despite =get-foo- having returned true if the
interrupt occurs inside the cleanup clause, eg. during execution of
=release-foo=.

* Notes

Compatibility layer function currently defined only for SBCL implementation."

  #+sb-thread(sb-thread:terminate-thread thread))

(defun yield-thread()
"Yield the processor to other threads.

* Notes

Compatibility layer function currently defined only for SBCL implementation"
  #+sb-thread(sb-thread::release-foreground))
