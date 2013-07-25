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

:abstract: The Lisp Educational Network Simulator (LENS) is an
  discrete event simulation framework written in Common Lisp. It
  provides a generic architecture for discrete event modelling of
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
    separation. The core model can be modified 
 
  * Users of a simulator will want to describe their problem in a way
    which matches their particular domain. |LISP| is an extensible
    language which is especially good for writing domain specific
    languages in.

  * |LISP| It enables introspection of all aspects of the
    simulated system while it is running without any additional
    programming. Most other simulators provide only limited
    introspection capabilities which have to be explicitly programmed in.

  The architecture of LENS is inspired by the OMNET++ simulator
  framework written in C++.




  attempt to develop a Network Simulator using good design
  principles and appropriate abstractions to make it easy to
  understand and extend and more suitable for educational use.

  Lisp was chosen as a particularly suitable language for this
  task. Firstly Lisp can be used interactively allowing interactive
  use of simulations with full introspective capabilities of all
  aspects of the model - even while a simulation is are running. It is
  a particularly good language on which to overlay a problem domain
  specific language - something needed in a simulation environment. As
  a programming language it does not limit design choices and
  paradigms as many other languages do and so supports whatever
  abstractions best suit the problem space.

  

.. |LENS| replace:: LENS

.. |JARW| replace:: John A.R. Williams

.. |LISP| replace:: Common Lisp

Support
=======

For questions, bug reports, feature requests, improvements, or patches
please email <J.A.R.Williams@jarw.org.uk>.


Overview
========

Modelling Concepts
------------------

Programming LENS
----------------

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

Eventlog
========



Acknowledgements
================

OMNET++