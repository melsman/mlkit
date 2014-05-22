---
title: Home Page
layout: front
---
{% include JB/setup %}

#### MLKit Properties

SML'97
: The MLKit compiler covers all of Standard ML,
as defined in the 1997 edition of the Definition of Standard ML. The
MLKit implements most of the latest Standard ML Basis Library
specification.

Supports ML Basis Files
: The MLKit compiles large
programs, including itself, around 80.000 lines of Standard ML plus
the Standard ML Basis Library. The support for ML Basis Files makes it
easy to compile large programs with different Standard ML
compilers. Currently, both MLton and the MLKit supports the concept of
ML Basis Files. The MLKit has a system, based on MLB-files, for
avoiding unnecessary recompilation upon changes of source code.

Region-Based Memory Management
: The MLKit integrates
reference-tracing garbage collection with region-based memory
management. Memory allocation directives (both allocation and
deallocation) are inferred by the compiler, which uses a number of
program analyses concerning lifetimes and storage layout.

Documentation
: A comprehensive guide on programming with
the MLKit is available from the Documentation page.

#### Other features

Open Source License
: MLKit is open source; it is
distributed under the GNU General Public License (GPL). The runtime
system and libraries are also distributed under the MIT licence, thus,
executables constructed with the MLKit are non-restricted.

Region Profiling
: The MLKit includes a graphical region
profiler, which helps gain detailed control over memory reuse. The
example graph to the right shows a region profile (region sizes as a
function of time) of two hundred generations of the `Game of
Life'.

Good for Real-Time
: Programmers who are interested in
real-time programming can exploit the possibility of disabling
reference-tracing garbage collection. In this case, there will be no
interruptions of unbounded duration at runtime.

Interface to C
: MLKit applications can call C functions
using standard C calling conventions; the region scheme can even take
care of allocating and deallocating regions used by C functions thus
invoked.

Efficient implementation of Modules
: The MLKit compiles Standard ML Modules, using a compilation scheme called Static
Interpretation, which eliminates Modules entirely at compile
time.

Two Backends
: Two backends are provided, one that
generates x86 native machine code for the Linux and MacOS operating
systems and one that generates bytecode.

Binary Distributions
: Binary distributions are available
  from the download page. Source code distributions are available as
  well.
