---
layout: page
title: Static Interpretation
---
{% include JB/setup %}
_By Niels Hallenberg and Martin Elsman_

<div style="float:right; margin-left:20px;margin-bottom:20px;margin-top:20px;">
<img width="400" src="{{BASE_PATH}}/images/Bootstrap.png" />
<div style="max-width:400px;width:auto;height:auto;font-size:80%;"><b>Figure 1. The bootstrapping process illustrated using T-diagrams. Each
compiler in the diagram is characterized by three languages: the
source language that it compiles, the target language that it
generates code for, and the implementation language that it is written
in.</b></div>
</div>

In this note, we demonstrate the process of bootstrapping the
MLKit. An overview of the process is pictured in Figure 1.

The lowest block in the picture denotes the MLton compiler, which is
used to compile the MLKit sources to a version of the MLKit that, when
running, uses MLton's runtime system. This version of the MLKit is
pictured to the right of the MLton executable and is named MLKit1.

In the following, we assume that the MLKit is installed in the
$HOME/kit directory.

To build MLKit1, enter the directory kit. From within this directory,
type the following commands to generate the MLKit1 compiler:

    $ ./autobuild
    $ ./configure
    ...
    $ make clean
    ...
    $ make
    ...

The make command produces a file bin/mlkit, which is a MLton
executable. The image is compiled using the Basis Library of MLton and
executes using the runtime system of Mlton.  Once MLKit1 is compiled,
using the steps presented above, write

    $ make bootstrap

from within the kit directory. This command will generate both MLKit2
and MLKit3 within the directory kit/bootstrap and test whether the
resulting executables are identical after stripping symbol table
information.

Notice that the MLKit2 compiler uses the runtime system of the MLKit,
which combines region inference and garbage collection. However, the
MLKit2 compiler is still affected by MLton. For instance, many
constants, including bit-vectors for garbage collection, are
calculated using the Basis Library of MLton. Thus, the MLKit2
executable may work even if the MLKit implementation of the Basis
Library is buggy.

### Testing the Bootstrapped Compiler

It is possible to apply all the MLKit tests in the test directory to
the bootstrapped versions of the MLKit. To do this, enter the
directory bootstrap/mlkit-v3/test and perform the test as follows:

    $ cd bootstrap/mlkit-v3/test
    $ make test_mlkit

As a result, a test report is generated.

### Profiling the MLKit

The MLKit can compile itself only when the compiler is capable of
using garbage collection (in combination with region inference) when
compiling programs.  To construct a region profile of the MLKit in
action, we first generate a version of the MLKit with region profiling
enabled; we call it kit3P and we build it using MLKit3:

    $ cd $HOME/mlkit-v3
    $ rm -rf $HOME/mlkit-v3P
    $ make bootstrap COMP_FLAGS=-prof INSTDIR=$HOME/mlkit-v3P

To generate a region profile of the MLKit in action, we use kit3P to
compile a smaller program kitkbjul9.sml found in the test
directory. We enter the test directory and compile kitkbjul9.sml using
the mlkit program (which have region profiling enabled), located in
the bin directory:

    $ cd $HOME/mlkit-v3P/test
    $ ../bin/mlkit kitkbjul9.sml
    ...
    [wrote executable file: run]
    $ rp2ps -region -name "MLKit compiling kitkbjul9" -sampleMax 2000 -eps 137 mm
    Region profiling to output file region.ps.
    Using name MLKit compiling kitkbjul9.
    Using 2000 samples.
    Using encapsulated postscript with width 388 pt.
    $ gv -seascape region.ps

Figure 2 shows the region profile obtained with the commands shown above.

<div width="400" style="float:right;width:200;">
<img width="400" src="{{BASE_PATH}}/images/Mlkit_compiling_kitkbjul9.png"/>
<div>Figure 2. A region profile of the MLKit compiling the program
kitkbjul9.sml, which is located in the test directory.  Controlling
Profiling Options.</div>
</div>

To control the profiling options at runtime as described in the
Section on Region Profiling, we use the program mlkit.img in the bin
directory and supply all command line parameters necessary, which
includes an install directory (i.e., the directory specified when
building this particular version of the MLKit). The install directory
can be found by looking in the file bin/mlkit.  The figure below shows
a region profile similar to the one above except that the number of
profile ticks is increased using the option -microsec 100000.

<div width="400" style="float:right;width:200;">
<img width="400" src="{{BASE_PATH}}/images/Mlkit_compiling_kitkbjul9_400msec.png"/>
<div>Figure 3. A region profile of the MLKit compiling the program kitkbjul9.sml. Memory is traversed once every 0.1 second.
</div>
</div>

Here is how the above figure was produced:

    $ cd $HOME/mlkit-v3P/test
    $ ../bin/mlkit.img -realtime -microsec 100000 $HOME/mlkit-v3P/ kitkbjul9.sml 
    ...
    [wrote executable file: run]
    $ rp2ps -region -sampleMax 2000 -eps 137 mm -name "MLKit compiling kitkbjul9 (100000 microsec)"         
    Region profiling to output file region.ps.
    Using name MLKit compiling kitkbjul9 (100000 microsec).
    Using 2000 samples.
    Using encapsulated postscript with width 388 pt.
    $ gv -seascape region.ps