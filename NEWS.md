## MLKit NEWS

* mael 2024-09-23: Optimiser bug fix with static selection of tuples containing
  tuples.

* mael 2024-09-17: Cleanup and tests of Barry, a Standard ML barrifier.

* mael 2024-06-27: Fix recursive-function specialisation bug (issue #177).

### MLKit version 4.7.11 is released

* mael 2024-06-04: Tooling bug fixes.

### MLKit version 4.7.10 is released

* mael 2024-05-31: Simpler pretty printing of boxity decisions when passing the
  flag --report_boxities.

* mael 2024-05-11: Provide support for a new flag `-objs` that together with
  `-no_delete_target_files` will allow the user to see which object-files are to
  be linked. The object files, including the file `base_object_file.o` are
  reported in the file `run` or `f.objs` if `-output f.objs` is given as
  argument to the `mlkit` command (issue #171).

* mael 2024-03-06: Use C99 flexible C structs for ML strings and
  tables (internal cleanup).

### MLKit version 4.7.9 is released

* mael 2024-03-04: Fix issue with Char.isCntrl (issue #162).

* mael 2024-03-04: Cleanup and fixes. Infinite reals and NaNs may now
  be parsed as specified in the Basis Bibrary manual (issue #163).

### MLKit version 4.7.8 is released

* mael 2023-12-29: Cleanup and ensurance that mlkit does not write
  into the installation directory when the REPL is launched or when
  linking with compiled versions of the SML Basis Library (PR #158).

* mael 2023-12-29: MacOS Sonoma / Xcode 15 support.

### MLKit version 4.7.7 is released

* mael 2023-12-15: Cleanup and various optimisation improvements
  (e.g., constant constructor-switch folding).

* mael 2023-12-07: Improved FFI auto-conversion. It is now easier to
  call c code using MLKit's auto-conversion features (PR #154).

* mael 2023-12-07: MLKit's basis library now supports most of the
  optional modules implementing the MONO_ARRAY, MONO_VECTOR,
  MONO_ARRAY_SLICE, MONO_VECTOR_SLICE, and MONO_ARRAY2 signatures for
  many underlying word and integer representations (PR #150, PR #151).

### MLKit version 4.7.6 is released

* mael 2023-11-16: Unboxed representation of datatypes using high bits
  for tagging (PR #149). This optimisation allows for unboxing of many
  types including whole language grammars, patricia trees (finite
  maps), union-find data-structures, and more. It works well together
  with unboxed datatypes that use the low bits for tagging, such as
  lists. The flag --report_boxities may be used to inspect the
  inferred representations (boxities) of declared datatypes.

* mael 2023-11-04: Unboxed representation of single constructor
  datatypes, even when the constructor argument is represented
  unboxed (PR #142).

* mael 2023-11-04: MLKit now has a REPL (Read-Eval-Print-Loop). It is
  invoked when no files are passed to the mlkit executable (PR #138).

### MLKit version 4.7.5 is released

* mael 2023-10-10: ReML released as part of the distribution.

* mael 2023-10-04: Improved documentation of basis library, which is
  now documented with [sigdoc](http://github.com/diku-dk/sigdoc) at
  https://elsman.com/mlkit/docs/pkg_idx.html .

* mael 2023-10-04: Addition of IEEE_REAL signature and IEEEReal
  structure. Improved Real support.

### MLKit version 4.7.4 is released

* mael 2023-10-01: Improved basis library documentation.

* mael 2023-10-01: Initial support for explicit region and effect
  annotations, including constraints on function invocations. The
  modified source language, which we call ReML (a Standard ML program
  is also a ReML program) is enabled with the -er flag. Examples are
  available in the test/explicit_regions folder.

* mael 2023-04-27: Generate position-independent machine code.

### MLKit version 4.7.3 is released

* mael 2023-01-03: Preliminary parallelism support through the Thread
  structure (basis/par.mlb). This feature is currently supported only
  when GC is disabled.

* mael 2023-01-03: Revised manual for version 4.7.2.

* mael 2022-12-25: Added MONO_ARRAY2 signature and RealArray2
  structure.

* mael 2023-03-02: Fixed problem with dangling pointers in exception
  values, which caused generational garbage collection to fail in
  bootstrapped MLKit compiler (issue #118).

* mael 2022-10-06: Reals may now be passed to functions unboxed in
  floating point registers. They may also be captured unboxed in
  closures (PR #73).

### MLKit version 4.7.2 is released

* mael 2022-09-15: The region type system has now been made simpler by
  avoiding word-regions altogether and by limiting regions of type
  RT_BOT to be related to explicit region annotated programs (PR
  #110). This change also eliminated a bug that prevented MLKit from
  compiling MLton (issue #103).

* mael 2022-09-07: Fixed unsoundness of gc-safety. The combination of
  garbage collection and region inference has now been made safe by
  ensuring (for certain) that no dangling pointers are introduced at
  runtime (PR #109).

* mael 2022-01-24: More efficient static environment serialisation
  (issue #103).

* mael 2022-01-20: Fixed bug caused by not renaming bound exception
  constructors during inlining (issue #104).

### MLKit version 4.6.1 is released

* mael 2022-01-20: Make use of better finite map structures (based on
  AVL trees) for all compiler and elaboration environments.

* mael 2022-01-20: Faster serialisation of compiler bases by adopting
  a non-sharing strategy in case there are many hash-collisions.

* mael 2022-01-18: The maximum size of a string is now 4Gb (previously
  16Mb). For the JavaScript backend, the maximum size is now 1Gb
  (previously 16Mb). Same for Word8Vector.vector, CharArray.array, and
  Word8Array.array.

* mael 2022-01-18: Fixed problem with dropped region variable being
  passed to a function. Such situations can arise if the particular
  function call instance cause no allocation in the dropped region,
  while another function call instance will provide a proper region
  which will be allocated into (by, for instance, a parameter-passed
  function) (problem mentioned in issue #97).

* mael 2022-01-18: Fixed name clash error related to referencing
  similarly named (but different) files from different mlb-files. The
  rule is now that two identically named mlb-files containing
  identical content must be identical (issue #97).

* mael 2022-01-18: Fixed behavior of VectorSlice.foldli,
  ArraySlice.foldli, Word8VectorSlice.foldli, ... to have the index
  reference the position in the slice rather than in the underlying
  vector or array. Problem fixed for both the JavaScript and the
  native basis. (issue #90).

### MLKit version 4.6.0 is released

* mael 2021-12-31: Revised the manual for version 4.6.0.

* mael 2021-12-31: Improved region profiler to show maximum memory
  usage precisely and not as the sum of the maximum memory used in
  regions and the maximum stack size.

* mael 2021-11-15: Removed all SMLserver code as well as the KAM
  bytecode backend as it is no longer maintained. The successor to
  SMLserver will be based on the native backend.

### MLKit version 4.5.9 is released

* mael 2021-09-17: Fixed smltojs problem with generating statements
  consisting of lambda expressions.

* mael 2021-09-17: Fixed problem with dealing with comments in
  ML-Yacc; fix ported from upstream implementation.

* mael 2021-07-15: PackReal{Big,Little} fixes (due to Ken Friis
  Larsen).

* mael 2021-06-23: Support for the Unix structure (due to Troels
  Henriksen).

* mael 2021-06-23: Fixes and additions to the Posix structure.

* mael 2021-02-17: Preliminary support for fork-join based
  task-parallelism. See the `par-benchmarks` folder in the
  [mlkit-bench](http://github.com/melsman/mlkit-bench) github
  repository for examples.

* mael 2021-02-17: Prettier printing of multiplicity terms (e.g.,
  -Pcee).

* mael 2021-02-15: More Real functionality (realCeil and friends).

### MLKit version 4.5.7 is released

* mael 2020-11-09: Fixed error with respect to `-report_gc` and
  `-verbose_gc` (issue #50).

### MLKit version 4.5.1 is released

* mael 2020-10-20: Static elimination of some array-bounds checks for
  Array2 arrays.

* mael 2020-10-20: More efficient implementation of Array2 structure.

* mael 2020-10-21: Static stack-alignment in native backend.

### MLKit version 4.5.0 is released

* mael 2020-04-30: Support for Word64, Word63, Int64, and Int63
  structures in the native backend. Default int and word sizes are now
  64 bits (63 bits when gc is enabled).

* mael 2020-04-30: Unboxing optimisation for double-precision floats.

* mael 2020-04-30: Preliminary support for source-level region
  annotations.

* mael 2019-07-03: Fixed bug on macOS wrt setting stacksize.

* mael 2019-07-03: Fixed bug on macOS wrt Time exception during
  timings.

* mael 2019-01-22: MLKit now uses a native 64bit backend.

* mael 2018-08-25: Support for automatic deployment of binary builds.

### MLKit version 4.3.12 is released

### MLKit version 4.3.9 is released

* mael 2016-03-12: Support for parallel builds using `make -j`.

* mael 2016-03-12: Eliminated hard dependency on pdflatex.

* mael 2016-03-12: Modified KitTester to generate HTML reports
  instead of pdf through LaTeX.

### MLKit version 4.3.8 is released

* mael 2016-03-09: Fixed issue with assembler invocation on Mac OS X.

* mael 2015-05-11: Fixed a few runtime system compiler warnings.

* mael 2015-05-08: Fixed inefficiency in region inference algorithm
  for programs containing highly continuation-based nested recursive
  functions.

* mael 2014-05-23: Moved the sources to github. Updated documentation
  and README files.

### MLKit version 4.3.7 is released

* mael 2012-08-29: Port of the MLKit to Mac OS X. Still the backend
  generates 32-bit code.

* mael 2012-08-29: Fixed bug in `Posix.ProcEnv.uname`.

* mael 2012-04-24: Automatic enabling of aggressive optimisations
  when garbage collection is enabled.

### MLKit version 4.3.6 is released

* mael 2012-04-23: Cleaning up source code and allowed for the
  compiler and runtime system to build on 64-bit systems, using
  the -m32 flag to gcc, gas, and ld.

* mael 2011-05-31: Renamed many files to allow for checkout of
  repository on case-insensitive file systems.

* mael 2011-02-23: Optimized sizes of pickles generated for symbol
  table information. Now, using the new Bitstream module in
  src/Pickle, it is possible to output single bits to the
  stream. Also reverted pickler dictionaries for sharing to be
  localized for each type, which opens up for using short
  locators. Sizes are now approximately half the size of before.

* mael 2011-02-23: Fixed Posix.c bug when compiling runtime system
  under ubuntu > 9.10 (reported by Niels Hallenberg).

* mael 2008-10-06: Compiling with SML/NJ is no longer supported.

* mael 2007-11-14: Fixed bug reported by Vesa Karvonen concerning
  unification of region types.

* mael 2007-11-09: Improved type checking of the internal language
  LambdaExp. Type schemes are now normalized (and inforced to be so)
  so that bound names always occur in the body of the type scheme
  prior to region inference. This normalization happens after
  compilation into the LambdaExp language.

* mael 2007-11-06: Improved type checking of the internal language
  LambdaExp. LambdaExp programs are now closed w.r.t. type variables.

* mael 2007-11-02: Fixed bug in opacity elimination. Bug reported by
  Vesa Karvonen; see `test/vesa2.sml` for a small example that didn't work
  before the bug fix.

* mael 2007-11-01: Support added for taking mlb-path-map-files as
  MLKit command-line arguments using the option
  `--mlb_path_maps`. Patch supplied by Vesa Karvonen.

* mael 2007-10-31: Infinite precision integers are now allowed in
  patterns. See file `test/intinf2.sml`.

* mael 2007-10-26: Signature BOOL now specifies true and
  false. Signature `LIST` specifies `::` and `nil`. Structure `List` declares
  `::` and `nil`. Structure `Bool` declares `true` and `false`.

* mael 2007-10-26: Improved error message for a particular kind of
  overloading resolution error. Elaboration of the program

      val _ = fn (a,b) => (a + 1) + (b + 0w1)

  results in the introduction of a type variable with an empty set
  of overload possibilities. We now check during unification that
  overloading sets do not become empty...

* mael 2007-10-26: Bug fix: array types should admit equality even if
  the argument to the array type constructor does not. Bug report
  submitted by Vesa Karvonen.

* mael 2007-07-04: Error messages for errors in mlb-files have been
  improved; see `test/error1.mlb`, `test/error2.mlb`, `test/error3.mlb`,
  and `test/error4.mlb`.

* mael 2007-04-19: Executing mlkit with no command args now returns
  minimal help info.

* mael 2007-04-12: Functor bodies are now stored directly in module
  interpretation bases. Upon compilation of a functor application,
  the functor body is extracted from the interpretation basis and
  compiled. Previously, functor bodies were stored in designated
  functor body files, which led to problems with moving compilation
  bases around for package installations, etc.

* mael 2007-03-22: Added mlkit-mlyacc executable and man-pages for
  mlkit-mlyacc and mlkit-mllex.

* mael 2007-03-12: Added mlkit-mllex executable by including a
  modified copy of mllex from SML/NJ 110.60. The copy of the source
  code is in `src/Tools/ml-lex`. The source code is compiled with
  either mlton or with mlkit, depending on the setting of MLCOMP at
  configure-time.

* mael 2006-10-29: Applied patch by Vesa Karvonen. The patch:

  - Exposes the Basis library `Text:TEXT` module, which wasn't exposed
    for some reason and also removes the opaque signature ascription
    which would (incorrectly) generate new types char, string, vector,
    ...

  - Extends the MLB parser to handle an arbitrary number of
    annotations in MLB files.  Unrecognized annotations result in a
    warning and have no other semantic effect.

  - Extends the MLB lexer to recognize simple quoted strings.  This is
    required to parse MLton's annotations properly.

  - Extends the expansion of MLB path variables so that path variables
    are allowed at any position of a path rather than just at the
    beginning.

  -- Vesa Karvonen

### MLKit version 4.3.0 is released

* mael 2006-01-27: Summary of major changes since 4.1.4

  - Updated Basis Library

  - Support for separate compilation with MLB-files

  - Garbage collection of regions is now the default

  - Improved documentation - man-pages, wiki, etc.

  - Various bug-fixes

* mael 2006-01-27: Added Makefile entry for constructing a source
  distribution (in directory `/dist/`). Run `make mlkit_src_tgz`.

* mael 2006-01-26: Added Makefile entry for constructing a binary
  distribution (in directory `/dist/`). After running `make
  bootstrap` (see below), run `make mlkit_i386_tgz`.

* mael 2006-01-26: Simplified the bootstrap process: after running
  `./configure` and `make nj`, simply run `make bootstrap`; see the
  README file for details.

* mael 2006-01-26: Updated license information, which is now
  available in the `doc/license` directory; see the file MLKit-LICENSE.

* mael 2006-01-24: Garbage collection is now used by default in
  combination with gc. To disable garbage collection, the flag `-no_gc`
  should be passed to the mlkit executable.

* mael 2006-01-06: Added support for annotations in
  mlb-files. Currently, only the flag safeLinkTimeElimination is
  supported, which helps linking in parts of the basis library only
  when it is needed. See the file basis/basis.mlb for an example.

* mael 2005-12-12: The MLKit menu system is no longer supported - it
  has now been removed from the sources. Instead, the user of the
  MLKit must provide options to the executable. To get an overview of
  which options are available, type `mlkit -h` on the command line.

* mael 2005-12-12: The MLKit Manual (`/doc/manual/mlkit.pdf`) is
  now updated to document the support for MLB-files. Notice that
  pm-files are no longer supported; see below.

* mael 2005-12-04: Fixed bug related to where .bdy-files (functor
  bodies) are stored. They are now stored in the MLB-directory of the
  functor-hosting mlb-file.

* mael 2005-11-29: Started work on updating the Standard ML Basis
  Library implementation - see

      http://www.itu.dk/research/mlkit/index.php/MLKit_Basis_Library_Implementation

  for getting an overview of the status.

* mael 2005-11-11: Finished work on generational garbage
  collection. The mlkit option `-gengc` can now be used to construct
  executables that combines generational garbage collection and
  regions. Pass the `-help` option to the generated executable to see
  which runtime options are available; the option `-only_major_gc` can
  be used to disable minor garbage collections. Statistics can be
  printed on stderr using the options `-verbose_gc` or `-report_gc` (the
  option `-verbose_gc` somewhat slows down the executable). NOTE:
  generational gc is still experimental - there is at least one bug we
  need to track down.

* varming 2005-11-01 Added some Posix support needed to build
  KitTester + some more.

* mael 2005-11-01: Using MLton 20050731, mlton successfully compiled
  the MLKit, resulting in a very fast (but large) mlkit executable -
  cool. Notice that MLton 20041109 is buggy and doesn't compile the
  MLKit correctly; see http://mlton.org/Bugs20041109.

  To compile the MLKit with mlton, the following commands were used
  (on abacus.itu.dk):

      $ cd /home/mael/kit/src/Compiler
      $ /home/mael/mlton-20050731/build/bin/mlton @MLton ram-slop 0.7 gc-summary -- \
  	  -drop-pass deepFlatten -verbose 3 native.mlb
      $ cp native /home/mael/kit/bin/mlkit_mlton2005
      $ ls -l /home/mael/kit/bin/mlkit_mlton2005
      -rwxr-xr-x  1 mael users 105135787 Nov  1 09:35 /home/mael/kit/bin/mlkit_mlton2005
      $ strip /home/mael/kit/bin/mlkit_mlton2005
      $ $ ls -l /home/mael/kit/bin/mlkit_mlton2005
      -rwxr-xr-x  1 mael users 58855368 Nov  1 13:51 /home/mael/kit/bin/mlkit_mlton2005

  To compile the MLKit again with mlkit_mlton2005, use the following
  command:

      $ ../../bin/mlkit_mlton2005 /home/mael/kit -gc native.mlb

  Executing this command generates an executable file run:

      $ ls -l run
      -rwxr-xr-x  1 mael users 11353914 Nov  1 14:22 ./run
      $ strip ./run
      $ ls -l run
      -rwxr-xr-x  1 mael users 7603544 Nov  1 14:36 ./run

  Thus, albeit faster, the size of the mlkit_mlton executable is 8
  times the size of the mlkit_mlkit executable.

* mael 2005-10-04: Bootstrapping. Bootstrapping now works with the
  native backend:

      $ cd kit
      $ make clean
      $ ./configure
      $ make mlkit
      $ rm -rf ~/mlkit-v1
      $ make bootstrap_first INSTDIR=$HOME/mlkit-v1
      $ cd ~/mlkit-v1
      $ rm -rf ~/mlkit-v2
      $ ulimit -s unlimited
      $ make bootstrap_next INSTDIR=$HOME/mlkit-v2
      $ cd ~/mlkit-v2
      $ rm -rf ~/mlkit-v3
      $ make bootstrap_next INSTDIR=$HOME/mlkit-v3
      $ cd ..
      $ strip ~/mlkit-v2/bin/mlkit.img
      $ strip ~/mlkit-v3/bin/mlkit.img
      $ diff ~/mlkit-v2/bin/mlkit.img ~/mlkit-v3/bin/mlkit.img
      $

* mael 2005-09-27: Added very limited Posix support necessary for
  bootstrapping. See basis/POSIX.sml.

* mael 2005-09-25: Region profiling is now supported with mlb-files.
  When "-prof" is passed to the mlkit executable, each region/
  effect variable gets a unique id (threaded through the
  compilation). The solution applied is to add an option to the
  mlkit executable: -regionvar N. With this option, the first
  generated region variable gets id N+1. The compiler stores a
  pair (first regionvar, last regionvar) in a file f.rev after having
  compiled f.sml. For each compilation, mlbmake reads the last
  regionvar from the previous fn.rev-file M, which is then used with
  -regionvar M in compilation of f(n+1).sml.

* mael 2005-09-24: The mlkit no longer accepts .pm-files. The
  mlkit executable accepts files with the following extensions:

  - `.mlb:` batch-based compilation of projects following the
    mlb-syntax; starts a new process for each source-file
    compilation. The mlb-subsystem uses the -c, -load, and -link
    options for compiling individual source files into object code and
    for linking object code.

  - `.{sml,sig}:` Compiling a file F with extension .sml or .sig, without
    use of the -c flag, is similar to compiling the mlb-file:

        local $(SML_LIB)/basis/basis.sml
        in F
        end

    If the flag -c is used, no executable is generated; instead
    interface files (extensions .{eb,eb1}) are generated together with
    linkable object code. The option -load may be used for loading
    interface files before compilation. These options are used by the
    MLB compilation subsystem.

  For both .mlb-files and .sml-files, the -o flag allows for
  specifying a different output file than a.out.

* mael 2005-07-15: Fixed KitTester (src/Tools/Tester) so that it
  accepts mlb-files in the grammar for tst-files instead of
  pm-files. Also, simplify the system to allow only the following
  annotations:

      nobasislib     ; do not import basis library
      ccl            ; compare compiler logs
      tx             ; time executable
      tc             ; time compiler
      ecte           ; expect compile time error
      ue             ; expect uncaught exception

  The annotations "prof", "gc", and "gengc" should not be supported -
  in this way, the same tst-file can be used for all tests
  (kam-backend,native-backend,test of gengc, test of gc, test of
  profiling, etc). For this to work, KitTester should accept options
  that can be redirected to the MLKit executable for each test
  program compilation.

* mael 2005-07-15: Fixed bug related to recompilation of functors. If
  a functor is modified, the .bdy-file for the functor body is
  modified, but the eb-file for the file in which the functor was
  declared was not necessarily modified upon a change in the source
  file (which could cause a lack of recompilation). The fix inserts
  an md5-sum of the .bdy-file in the functor closure in the eb-file
  for the file in which the functor is declared. Thus a change in the
  source code for the function will now result in a change in the
  eb-file.

* mael 2005-05-10: Integrated mlb-file support in mlkit
  executable. For full isolation, the implementation forks a
  different process for each compilation of a source file.

* mael 2005-02-22: The grouping of source code for mlb-files (shown
  below) is now also used for cm-files when the MLKit is compiled
  with SML/NJ. See e.g. kit/src/build.sml and kit/src/build_kam.sml.

* mael 2004-12-17: To actually split sources so that Barry,
  SMLserver, Kit(native) and Kit(bytecode) can be built
  independently, we have the following mlb-hierachy (transitive
  dependencies are omitted), where the mlb-files in boxes denote main
  mlb-files that, when built, result in mlkit executables:

	    pickle.mlb      edlib.mlb
		\             /
		 \           /
		   tools.mlb
		      |
		      |
	       syntax_objects.mlb
		      |
		      |
	      special_objects.mlb
		  /         \
		 /           \
      compiler_objects.mlb   basics.mlb
		\             /
		 \           /
		  manager.mlb
		      |
		      |
		 compiler.mlb
		  /         \
		 /           \
	    +---------+       \
	    |barry.mlb|    regions.mlb
	    +---------+    /        \
			  /          \
		   +----------+   +------------+
		   |native.mlb|   |bytecode.mlb|
		   +----------+   +------------+

* mael 2004-12-16: Defunctorized much of the MLKit sources to make
  bootstrapping more tractable with the new recompilation scheme
  (serialization of bases). There are now MLB-files and CM-files for
  compiling MLKit, so that it can be compiled both with SML/NJ,
  MLton, and MLkit itself. Compiling with MLton on my 1GB(+1GB swap)
  does not work - I suspect more memory is needed. I hope it
  works when I try it on a 4GB machine.

* mael 2004-12-15: Changed directory kit/basislib to kit/basis and
  kit/basislib/basislib.pm to kit/basis/basis.pm.

* mael 2004-12-14: mlbmake: It is now checked that a source file
  (i.e., sml-file) is included at most once in an entire MLB
  source-hierarchy. MLB-files may be referred to multiple times,
  although recursive references to MLB files are not allowed.

* mael 2004-12-14: mlbmake: As suggested by Stephen Weeks we now use
  $(SML_LIB) as a path variable for refering to a root for the MLB
  libraries. Thus, programmers can now use $(SML_LIB)/basis/basis.mlb
  to refer to the basis library in both MLKit and MLton.

* mael 2004-08-05: Bug fix: bug reported by Stephen Weeks
  concerning type scheme generalization is now fixed (see
  kit/test/weeks6.sml).

* mael 2004-08-04: Finished initial work on supporting export of ML
  functions to C. Only functions of type int->int may be exported at
  present and the support works only with pure region based memory
  management (i.e., without reference tracing garbage
  collection). See the example export.mlb in the test directory.

* mael 2004-08-02: Initiated work on supporting export of ML
  functions to C. The special identifier "_export" is now declared in
  the initial environment with type \/'a,'b . string * ('a -> 'b) ->
  unit. In a call `_export("myFun",fn a:int => a+1)`, the string
  `"myFun"` is the name of the assembled function (following C calling
  conventions), and the function `(fn a:int => a+1)` is the ML function
  called when the C function `"myFun"` is called from C code.

### MLKit version 4.1.4 is released

* mael 2004-06-09: Work initiated on pickling (serializing) compiler
  bases to disk so as to obtain a more flexible recompilation system
  that does not require users to compile all program code whenever
  the MLKit interactive menu system is launched.

* mael 2003-05-14: Added three new distinct region types to allow
  for generational garbage collection of regions with untagged
  pairs, untagged triples, and untagged references. Generational
  garbage collection with regions is not yet fully implemented.

* mael 2003-05-13: Reals now only take up three words (instead of
  four) when tagging is enabled.

* mael 2003-05-08: Fixed bug related to free type variables in type
  bindings.

* mael 2003-02-18: Added support for inlining of functor
  applications. To specify that functor applications in a file should
  be inlined, one can now write inline specifications in pm-files
  using the syntax 'inline funapps in ... end'. Inlining of functor
  applications improves performance, because it triggers
  optimizations, such as function inlining, but it also slows down
  recompilation.

* mael 2002-12-05: Fixed bug in opacity elimination
  (test/opaque3.sml).

* mael 2002-12-05: Fixed bug concerning free type variables at
  top-level in local function (`test/ftv.sml`).

### MLKit version 4.1.3 is released (internally)

* mael 2002-11-08: Implemented uncurrying of functions. This
  optimization has an important impact on execution times of compiled
  programs. The optimization is safe-for space w.r.t. the region-based
  memory management. The optimization can be disabled with the option
  -no_uncurry.

* mael 2002-11-05: Improved pattern-match compilation. The generated
  code now shares (in most cases) the extraction of values from the
  root node in a match.

### MLKit version 4.1.2 is released (internally)

* mael 2002-10-28: Started work on Barry - a Standard ML barifier
  (e.g., simplifier). Barry eliminates modules, pattern matching,
  complex derived forms, resolves infix resolution, and performs a
  series of optimisations. Barry is basically the frontend of the ML
  Kit with a pretty printer, which outputs intermediate language
  constructs in the MLKit (LambdaExp fragments) as Standard ML
  code. See the file [README_BARRY.md](README_BARRY.md).

* mael 2002-10-25: Support for untagged representation of pairs when
  garbage collection is enabled.

* mael 2002-09-20: Fixed bug caused by Timing.checkRealTimer raising
  exception Time on some systems when -timing flag is used.

* mael 2002-09-17: Added FreeBSD support - patches contributed by
  Koshy A Joseph.

### MLKit version 4.1.1 is released

* mael 2002-08-25: Removed Posix support due to licensing problems
  with code from mlton.

* mael 2002-08-25: Removed region type real. Added region type
  pair for implementation of almost tag-free garbage collection -
  not yet fully implemented.

### MLKit version 4.1.0 is released

* mael 2002-04-06: Manual improved -- the pdf-version of the manual
  now uses Type 1 fonts, exclusively.

* mael 2001-12-16: Posix bug-fixes. Runtime system clean-up.
  Change of prim-function for interfacing with C. It is easier
  now to interface with C functions that take regions as
  arguments; a few macros help generate different code when
  profiling is enabled.

* mael 2001-12-13: Ported Posix structure from MLton. Calls to C
  are in many cases simpler now because auto-conversion is
  extended to work with a larger set of types, including
  strings (and CharArrays).

* mael 2001-11-25: Assembler in-lining of sub/update/size
  operations on bytetables (CharArray, Word8Array, CharVector,
  Word8Vector, String).

* mael 2001-11-24: Better implementation of Array/Vector (constant
  subscripting/updating).

* mael 2001-11-24: Better implementation of CharArray/Word8Array
  (basislib/ByteTable.sml); no ref-indirection.

* mael 2001-11-24: Better implementation of strings/arrays/vectors
  by supporting large objects in the runtime system; values that do
  not fit in region pages are now allocated with malloc and placed
  in a list of large objects in the region descriptor for the
  region. Upon region de-allocation, the large objects are freed
  using free.

* mael 2001-11-20: Better reporting of garbage collection statistics
  and fragmentation (runtime flag `-verbose_gc`, when gc is enabled).

* mael 2001-11-15: Unboxing of datatypes. Previously, only a certain
  set of predefined types were implemented unboxed. Now, enumerations
  and many other simple datatypes, such as trees, are represented
  unboxed; file src/Compiler/Lambda/CompileDec.sml.

* mael 2001-11-02: Bug fix involving Int32 operations in the presence
  of profiling and GC; file `src/Compiler/Backend/FetchAndFlush.sml`.

* mael 2001-11-01: Added structures Pack32Little, Pack32Big, IntInf
  to basis library implementation; implementations are ported from
  SML/NJ.

* mael 2001-10-30: Bug fix involving unboxing of function arguments
  in OptLambda.sml; see test file test/unbox.sml.

* mael 2001-10-30: Bug fix involving manipulation of compiler bases
  when restricting interpretation bases in ManagerObjects.sml; see
  test file test/functor3.sml.

* mael 2001-10-26: Bug fix involving recursive value bindings; see
  test programs `test/valrecpat.sml`, `test/valrecpat1.sml`,
  `test/valrecpat2.sml`, `test/valrecpat3.sml`, and `test/valrecpat4.sml`.

* mael 2001-10-26: Bug fix involving polymorphism and parallel
  value bindings. Bug reported by Stephen Weeks; see test program
  `test/weeks5.sml`.

* mael 2001-10-23: Bug fix in X86-backend; when tagging is enabled an
  Overflow exception was raised when the largest Word31-word appeared
  in a pattern.

* nh 2001-10-20: The Garbage Collection algorithm now uses two stacks;
  one for infinite regions and one for finite regions. Each finite
  region is only traversed once and it is still a non-recursive
  algorithm. The stack size is determined dynamically.

* mael 2001-10-14: It is now possible to disable region inference in
  the MLKit by specifying the flag no_region_inference (or no_ri) on
  the command line. The flag has the effect that all infinite regions
  are collapsed with the top-level infinite regions
  (`DropRegions.sml`). The option has no effect on finite regions;
  and region parameters may still happen to be instantiated to either
  finite or infinite regions (the top-level ones).

* mael 2001-09-30: Added regular expression support (structure RegExp)
  to the basis library. The implementation is based on an
  implementation by Ken Friis Larsen. See the file
  `/basislib/REG_EXP.sml` for details.

* mael 2001-09-30: Program manager changes: (1) It is now
  possible for two files in different subdirectories to have the same
  name and be listed in the same pm-file. (2) Target-files generated
  when compiling a pm-file (i.e., uo-files for the bytecode backend
  and o-files for the native x86 backend) are now stored in a PM
  directory located in the same directory as the compiled pm-file is
  located; this change allows for a pm-file to refer to source files
  located in directories for which the user does not have
  write-permission.

### MLKit version 4.0.0 is released

* mael 2001-08-16: Fixed bug in separate compilation framework; match
  function in ClosConvEnv functor was non-exhaustive.

* mael 2001-07-19: Added Interrupt exception to front end; it is
  now possible to handle an Interrupt exception raised by pressing
  Control-C.

### MLKit version 3.9.1 is released

* mael 2001-07-05: Bug fix: valrec may now overwrite identifier
  status. Bug reported by Johnny Andersen.

* mael 2001-07-05: Quotation support. When the option `-quotation` is
  given to the compiler at startup, the datatype

      datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a

  is available in the top-level environment. Moreover, the back-tick character
  cease to be allowed in symbolic identifiers. Values of the 'a
  frag datatype may be constructed using the quotation/antiquotation
  syntax:

      val s = "world"
      val a : string frag list = `hello ^s - goodbye`

* mael 2001-07-05: An uncaught exception now causes the program to
  return -1 to the shell (instead of 0). Suggestion by Stephen Weeks.

* mael 2001-07-05: Bug fix: The function OS.FileSys.tmpName now
  returns a file name for a file in `/tmp/` (instead of in `/etc/`). Bug
  reported by Stephen Weeks.

### MLKit version 3.9.0 is released

* many bug fixes

* replaced HP backend with x86 native backend

* bytecode backend added as an alternative to the x86 native backend;
  primarily, the bytecode backend is added for portability and for
  use with the SMLserver project, which adds Standard ML language
  support to AOLserver -- a webserver from America Online

* added support for reference tracing garbage collection

* Word31, Word32, Word8, Int31 and Int32 structures added

### 1998-12-03: MLKit version 3.0 is released

### 1997: MLKit version 2.0 is released

### 1993: MLKit version 1.0 is released
