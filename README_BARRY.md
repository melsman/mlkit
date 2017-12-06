## Barry

Barry is a barifier (i.e., simplifier) for Standard ML, based on the
frontend of the MLKit. Barry transforms Standard ML, including
Modules, into a subset of Core Standard ML. Barry has the following
features:

* Static interpretation of Modules. All Modules language constructs
  are eliminated by Barry, including functors, functor applications,
  and signature constraints.

* Pattern match compilation. Barry transforms complex patterns into
  simple ones.

* Optimization. By default, Barry performs various optimizations,
  including function inlining and specialization of higher-order
  recursive functions, such as map and foldl. Optimizations can be
  controlled using compile-time flags.

## Compilation

Barry is known to work on Linux and MacOS boxes. To compile Barry from
the MLKit sources, simply type

    $ make barry

from within the `kit/` directory. If compilation succeeds, an executable
file `kit/bin/barry` should now be available. If you wish to install
Barry on your system, type

    $ sudo make install_barry

## How it Works

Barry can be controlled by passing options to the barry executable at
the command line. To see a list of all possible options, type

    $ bin/barry --help

Notice that the present version of Barry supports a series of options
that are irrelevant to Barry; in future versions of Barry, this
nuisance may disappear. In its simplest form, Barry works by passing
it an mlb-file or a Standard ML file on the command line:

    $ bin/barry kitdemo/Set.mlb

Output is written to files in the `MLB/` directory. To avoid inclusion of
the Standard ML Basis Library, which comes with Barry, use the option
`-no_basislib`. You may wish to inspect some of the output from
compiling the Basis Library, to learn how Barry deals with primitives.

## Modifying the Barry Pretty-Printer

Barry works by pretty-printing `LambdaExp` fragments (`LambdaExp` is an
intermediate language in the MLKit) as Standard ML code. If you want
to look at the code, see the file [[src/Compiler/Lambda/LambdaExp.sml]].

## License and Copyright

The MLKit compiler is distributed under the GNU Public License,
version 2. See the file [MLKit-LICENSE](/doc/license/MLKit-LICENSE)
for details. The runtime system (`/src/Runtime/`) and libraries
(`basis/`) is distributed under the more liberal MIT License.
