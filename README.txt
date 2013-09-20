.. -*-RST-*-

=========================================
Lisp Educational Network Simulator (LENS)
=========================================

:Author:    Dr John A.R. Williams
:Contact:   J.A.R.Williams@jarw.org.uk
:date:      2013/07/11
:status:    Developmental
:version:   0.1
:copyright: ©2013 Dr. J.A.R. Williams

:abstract: The Lisp Educational Network Simulator (LENS) is a
  discrete event simulation framework written in Common Lisp. It
  provides a generic architecture for modelling in
  problem domains where the system may be represented by the
  exchanging of messages between entities. It is being used to develop
  simulations for research into Wireless Sensor Networks.

  The decision to write a new simulator in |LISP| was based upon a
  number of observations:

  * Simulation environments require a core model and a means for
    specifying particular simulations at run time. Most simulators
    have the core model written in a compiled language (e.g. C++) for
    efficiency and have a separate embedded run time interpreter
    (e.g. Tcl or a domain specific language) for describing the
    particular simulation. |LISP| provides the full language and
    compiler environment at run-time so there is no need for this
    separation. 
 
  * Users of a simulator will want to describe their problem in a way
    which matches their particular domain. |LISP| is an extensible
    language which is especially good for writing domain specific
    languages.

  * |LISP| enables introspection of all aspects of the simulated
    system while it is running without any additional
    programming. Most other simulators provide only limited
    introspection capabilities which have to be explicitly programmed
    in.

  The architecture of LENS is inspired by the OMNET++ simulator
  framework written in C++ however may aspects of that design become
  simpler when implemented in |LISP|.

.. |LENS| replace:: LENS

.. |JARW| replace:: John A.R. Williams

.. |LISP| replace:: Common Lisp

Support
=======

For questions, bug reports, feature requests, improvements, or patches
please email <J.A.R.Williams@jarw.org.uk>.


Overview
========

------------------
Modelling Concepts
------------------
----------------
Programming LENS
----------------

----------
Using LENS
----------

Modules
=======

Messages and Packets
====================

Simulation Library
==================

Configuring Simulations
=======================

Result Recording and Analysis
=============================

Tracelog
========



Acknowledgements
================

OMNET++