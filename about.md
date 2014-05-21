---
layout: page
title: About
group: navigation
---
{% include JB/setup %}

_By Martin Elsman and Mads Tofte_

### History, Goals and Approach

<img width="160" alt="Old logo" align="right" src="{{BASE_PATH}}/images/Oldlogo.gif">

The development of the MLKit began in 1989 at Edinburgh
University. Originally, the project had two purposes:

1. to provide an implementation that is consistent with the language definition
2. to provide a service to the research community by providing a highly modular system, parts of which can be reused by other compiler writers

These goals are still intact. Specifically, in order to facilitate
code reuse and reliability of the front-end of the MLKit, we have
rewritten the Match compiler (using the same approach as Moscow ML
uses) and cleaned up the implementation of the static semantics of the
Core language (the StatObject module).

Since the work on region-based memory management started in the MLKit
(in 1994), goals specific to region-based memory management have been
added:

1. to provide an implementation that could provide ML programmers with a high degree of control over memory resources
2. to push the compilation technology for regions to the point where programs could be compiled at a tolerable speed and result in fast-running programs

Version 2 of the Kit did provide good control over memory resources
for Core ML programs, but it did not compile all of Standard ML and
compilation was very slow.

Version 3 compiles all of Standard ML, in particular Modules are
compiled using a technique called static interpretation. Moreover,
considerable effort was devoted to tuning the system. The general
approach we take in the Kit is to try to get the functionality right
first and then gradually replace inefficient data structures and
algorithms with better ones. A nice side-effect of this strategy is
that the Kit contains more and more reusable modules that implement
classical data structures and algorithms from the literature. Examples
include sorting, union-find, Patricia trees and directed graphs
(strongly connected components, etc.)

Version 4.0.0 has support for garbage collection in combination with
region inference and the HP backend of version 3.0 has been replaced
with a native backend for Intel's x86 architecture and a bytecode
backend. Many more features have been added to the MLKit since version
3.0; see the documentation for details.

### Acknowledgments

The MLKit is software partly delivered by the DART research project,
which is sponsored by the Danish Research Council for Natural
Sciences.  People whom we wish to thank for contributing with bug
reports include, but are not limited to (in alphabetical order) Johnny
Andersen, Ken Friis Larsen, Daniel Wang, and Stephen Weeks.

### Other Standard ML Implementations

* [SML/NJ](http://www.smlnj.org)
* [Moscow ML](http://www.itu.dk/~sestoft/mosml.html)
* MLj
* [Poly/ML](http://www.polyml.org/)
* [MLton](http://www.mlton.org)
* [SML.NET](http://www.cl.cam.ac.uk/research/tsg/SMLNET/)
* [SML#](http://www.pllab.riec.tohoku.ac.jp/smlsharp/)