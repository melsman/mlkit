## MLKit

The [MLKit](http://elsman.com/mlkit) is a compiler toolkit for the
Standard ML language, including **The MLKit with Regions**, which features native
backends for the X64 and Arm64 architectures, based on region inference, and
**SMLtoJs**, which features a JavaScript backend targeting web browsers. The two
compilers share the same frontend and compilation management scheme.

The MLKit covers all of Standard ML, as defined in the 1997 edition of
[The Definition of Standard ML](https://mitpress.mit.edu/books/definition-standard-ml-revised-edition) and supports most of the [Standard ML
Basis Library](http://elsman.com/mlkit/basis.html).

## Test Statistics and Benchmarking

[![CI](https://github.com/melsman/mlkit/workflows/CI/badge.svg)](https://github.com/melsman/mlkit/actions) [Benchmarking](https://elsman.com/mlkit-bench/)

## Installation

Under macOS, MLKit is available through Homebrew: Just execute `brew
install mlkit`. Under Linux, you may download the latest binary
tgz-distribution of MLKit from
https://github.com/melsman/mlkit/releases/latest

Once downloaded and unpacked, execute `make install` from within the
top-directory of the unpacked distribution. You may install MLKit in a
directory different from `/usr/local/mlkit` by instead typing
`PREFIX=myinstallpath make install`.

## Features

- **Compiles all of Standard ML**. The MLKit compiles all of Standard ML, [including Modules](http://elsman.com/mlkit/staticinterp.html), as specified by
  the Definition of Standard ML. The MLKit also supports most of the
  [Standard ML Basis Library](http://elsman.com/mlkit/basis.html).

- **Compiles large programs**. The MLKit compiles large programs,
  [including itself](http://elsman.com/mlkit/bootstrap.html), around
  80.000 lines of Standard ML plus the Standard ML Basis Library. The
  support for [ML Basis
  Files](http://elsman.com/mlkit/mlbasisfiles.html) makes it easy to
  compile a program with different Standard ML compilers. Currently,
  both [MLton](http://mlton.org) and the MLKit supports the concept of
  ML Basis Files. The MLKit works well together with
  [smlpkg](https://github.com/diku-dk/smlpkg), a generic package
  manager for Standard ML libraries and programs.

- **Documentation is available**. Man-pages and general documentation
  is available from the [MLKit home
  page](http://melsman.github.io/mlkit).

## MLKit - Native Backends

This version of the compiler is based on region inference and has the
following features:

- An X64 native backend for Linux and macOS, and an Arm64 native backend
  for macOS on Apple Silicon.

- Memory allocation directives (both allocation and deallocation) are
  inferred by the compiler, which uses a number of program analyses
  concerning lifetimes and storage layout. The MLKit compiler is
  unique among ML implementations in this respect.

- A comprehensive guide on [Programming with Regions in the
  MLKit](https://github.com/melsman/mlkit/raw/master/doc/mlkit.pdf) is available,
  which also demonstrates how to create memory profiles of program
  executions using the supplied region profiler and how to interact
  with C programs.

- Region inference may be augmented with reference-tracing garbage
  collection to achieve better memory behavior.

## ReML

ReML extends Standard ML with explicit regions, effects, and constraints on
regions and effects. Programmers can express these annotations in their
programs, and the compiler checks them alongside its region and effect
inference.

ReML supports both the X64 and Arm64 native backends. It is built alongside
MLKit by `make mlkit` and is available as `bin/reml` in the build tree or
`reml` after installation. See the [explicit-region examples and tests](test/explicit_regions)
for examples of the language.

ReML supports parallel threads, but does not currently support
reference-tracing garbage collection.

## SMLtoJs - The JavaScript Backend

This version of the compiler generates efficient JavaScript, primarily
for [executing Standard ML code in the
browser](/README_SMLTOJS.md). There is also an [online version of
SMLtoJs](https://diku-dk.github.io/sml-ide/), which makes it possible
to write, compile, and execute Standard ML code in a web browser.

## The Barry Backend

The repository also includes the sources for
[Barry](/README_BARRY.md), a Standard ML source-to-source compiler
that eliminates modules, using static interpretation, and generates
optimised Core-language Standard ML code.

## License and Copyright

The MLKit compiler is distributed under the GNU Public License,
version 2. See the file [MLKit-LICENSE](/doc/license/MLKit-LICENSE)
for details. The runtime system (`/src/Runtime/`) and libraries
(`basis/`) is distributed under the more liberal MIT License.

## Compilation Requirements

To compile, install, and use the MLKit, a Linux box running Ubuntu
Linux, Debian, gentoo, or similar is needed. The MLKit also works on
macOS and has also earlier been reported to run on the FreeBSD/x64
platform, with a little tweaking.

To compile the MLKit, a Standard ML compiler is needed, which needs to
be one of the following:

__[MLton](http://mlton.org) >= 20051202:__
```bash
$ mlton
MLton 20051202 (built Sat Dec 03 04:20:11 2005 on pavilion)
```

If a version prior to 20201023 is used, you may need to adjust the
`mlton`-flags setup in the file `Makefiledefault`.

__A working MLKit compiler >= 4.3.0:__
```bash
$ mlkit -V
MLKit version 4.3.0, Jan 25, 2006 [X86 Backend]
```

Moreover, `gcc` is needed for compiling the runtime system and related
tools.

## Compilation

For native Apple Silicon builds, follow
[Native ARM64 on macOS](#native-arm64-on-macos) below. The following commands
build the X64 backend.

After having checked out the sources from Github, execute the command:
```bash
$ ./autobuild
```

Now, `cd` to the toplevel directory of the repository and execute the
appropriate set of commands:

__Compile with MLton alone (Tested with 3Gb RAM):__
```bash
$ ./configure
$ make mlkit
```

__Compile with existing MLKit (Tested with 1Gb RAM):__
```bash
$ ./configure --with-compiler=mlkit
$ make mlkit
```

If you later want to install the MLKit in your own home directory, you
should also pass the option `--prefix=$HOME/mlkit` to `./configure` above.

For binary packages, we use
```bash
$ ./configure --sysconfdir=/etc --prefix=/usr
```

### Native ARM64 on macOS

To build native MLKit, ReML, and tools on an Apple Silicon Mac, you need
Xcode command-line tools, Autoconf, and a working MLKit on `PATH`. An installed
X64 MLKit can bootstrap the build using Rosetta 2. Use a checkout path without
spaces and run these commands from the repository root:

```bash
export SML_LIB="$PWD"
./autobuild
DARWIN_NATIVE=1 ./configure CC=/usr/bin/gcc --with-compiler=mlkit
make -j3
```

The first compiler build uses the installed MLKit's own Basis library,
cached objects, and matching runtime, so a `DARWIN_NATIVE=0` build is unnecessary. It produces an
ARM-emitting bootstrap compiler at `bin/mlkit-arm64`, which then builds the
native compilers. Compiler builds use `-gc`; the Makefile also configures the
bootstrap compiler's larger stack automatically.

The native executables are available at the usual `bin/mlkit`, `bin/reml`,
`bin/kittester`, `bin/mlkit-mllex`, `bin/mlkit-mlyacc`, and `bin/rp2ps` paths
(linked to `bin/darwin-arm64`). Verify and run MLKit with:

```bash
lipo -archs bin/mlkit   # should print arm64
bin/mlkit --version
bin/mlkit             # start the REPL
```

Keep `SML_LIB` pointing at the checkout when using these build outputs. The
bootstrap step selects its installed library separately, so this setting
does not force the installed X64 compiler to use the checkout's runtime.
To select a different bootstrap executable, pass
`MLKIT_BOOTSTRAP=/path/to/mlkit` to `make`. For an unpacked
seed whose library is not configured in an installed `mlb-path-map`, also
pass `MLKIT_BOOTSTRAP_SML_LIB=/path/to/seed/lib/mlkit`.

The standard targets work for both backends: `make mlkit` (also the default),
`make build_basislibs` (an alias for `mlkit_basislibs`), `make mlkit_libs`,
`make test`, `make bootstrap`, and `make install`. `make all` also builds
SMLtoJs and its libraries. The configured `DARWIN_NATIVE` selects the backend;
set it when running `configure`, not on the `make` command line.

For a separate native installation in your home directory, add
`--prefix="$HOME/mlkit-arm64"` to the configure command above, then run:

```bash
make build_basislibs
make install
export SML_LIB="$HOME/mlkit-arm64/lib/mlkit"
"$HOME/mlkit-arm64/bin/mlkit"
```

Use a prefix without spaces. `make install` installs the previously built
compilers, tools, and Basis caches; it also supports `DESTDIR` for packaging.

See the [ARM64 compiler instructions](doc/arm64-compiler.md) for testing and
bootstrap targets, and the [runtime notes](doc/arm64-runtime.md) for runtime
variants and SDK/toolchain details.

## Pre-compile Basis Library and Kit-Library

Execute the following command:
```bash
$ make mlkit_libs
```

## Bootstrapping (optional - works with 1Gb RAM)

This step is optional. If you want the resulting executable compiler
to be bootstrapped (compiled with itself), execute the command:
```bash
$ make bootstrap && make mlkit_libs
```

Be aware that this step takes some time.

## Installation after Compilation

For a system-wide installation of the MLKit, including installation of
man-pages and tools, execute the command:
```bash
$ sudo make install
```

For a personal installation, with `--prefix=$HOME/mlkit` given to
`./configure`, execute the following command:
```bash
$ make install
```

## Making a Binary Package

To build a binary package, execute the command
```bash
$ make mlkit_x64_tgz
```

This command leaves a package `mlkit-X.Y.Z-x64.tgz` in the `dist/`
directory. For building a binary package, the installation step above
is not needed and the bootstrapping step is optional. The binary package includes both the MLKit with Regions compiler (i.e., the `mlkit` executable) and [SMLtoJs](/README_SMLTOJS.md) (i.e., an executables named `smltojs`).

## Try It

To test the installation, copy the directory `/usr/share/mlkit/kitdemo` to
somewhere in your own directory, say `$HOME/kitdemo`:
```bash
$ cp -a /usr/share/mlkit/kitdemo $HOME/kitdemo
$ cd $HOME/kitdemo
$ mlkit helloworld.sml
```

The MLKit should produce an executable file `run`:
```bash
$ ./run
hello world
```

## Trying Without Installing

You can run `mlkit` without installing it, but you should then point
the environment variable `SML_LIB` at the build directory (which
contains the `basis` and the `lib` directories) whenever you run
`mlkit`.  E.g:

```bash
$ SML_LIB=$PWD bin/mlkit
```

## More Information

See the [MLKit home page](http://melsman.github.io/mlkit) for information about related papers, etc.

General documentation for the MLKit is located in the directories `doc/mlkit`
and `man/man1`. License information is located in the file
`doc/license/MLKit-LICENSE`.

## Comments and Bug Reports

The MLKit has a number of [known bugs and limitations](http://elsman.com/mlkit/bugs.html). To file a bug-report, create an issue at the Github page.

## Appendix A: Directory Structure of the Sources

    kit/
       README
       configure
       Makefile.in
       src/
       basis/
       doc/mlkit.pdf
          /license/MLKit-LICENSE
       man/man1/rp2ps.1
       kitdemo/
       test/

## Appendix B: Quick Compilation and Installation Guide

We assume that MLton >= 20051202 is installed on the system as
described above.

After having checked out the sources from Github, execute the command:
```bash
$ ./autobuild
```

To compile the MLKit, execute the following commands:
```bash
$ ./configure
$ make mlkit
$ make bootstrap
$ make mlkit_libs
```

The `make bootstrap` command is optional.

To install the MLKit and related tools, execute:
```bash
$ sudo make install
```

See the section "Try It" above to test the installation.

## Appendix C: Displaying Region Flow Graphs with VCG

The [VCG tool](http://www.cs.uni-sb.de/RW/users/sander/html/gsvcg1.html) can be used to show region flow graphs.
