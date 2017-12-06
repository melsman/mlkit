## SMLtoJs NEWS

### SMLtoJs version 4.3.10 is released

* mael 2017-12-06: Better support for tail-recursive functions, by
  making proper closures for escaping lambdas. Updated relevant List
  basis library functions to be properly tail-recursive.

* mael 2017-12-06: Tests can now be scripted using phantomjs; just
  type 'make test' in the `js/test` directory.

* mael 2013-06-03: Fixed bug with not installing `prims.js`. Now also
  installing tests.

* mael 2009-12-01: It is now possible to compile and run Standard ML
  programs in a browser. See the README_SMLTOJS file for details.

* mael 2009-12-01: The compiler has been rewritten so as to allow for
  tail-call optimization. The generated code is now much faster...

* mael 2008-10-25: Certain datatype constructors are now represented
  unboxed. This unboxing includes lists and enumerations.

* mael 2008-10-23: Datatype Constructors are now represented as
  numbers and there is support now for unboxing simple datatypes such
  as lists.

* mael 2008-08-16: SMLtoJs version 4.3.5 is released.

* mael 2008-08-13: Added support for accessing compiled SML functions
  from Javascript programs. At the SML side, values are exported using
  the _export keyword. Here is an example:

      fun myfun x = x + 1
      val () = _export("myfun", myfun)

  Expressions of the form `_export("myfun", v)` has type unit. The first
  argument to _export must be an immediate string and the second
  argument must be a value of type t1 -> t2, where t1 and t2 are
  arbitrary types. There are no dynamic checks that values passed to
  exported functions are compatible with type t1; the responsibility
  for this is entirely the programmer's.

  After the _export construct has been executed, the exported function
  can be called from Javascript, as follows:

      var a = SMLtoJs.myfun(4);

* mael 2008-01-14: Added support for linking with external libraries;
  use `smltojs --help` to get information about the `--jslibs` command
  line option.

* mael 2007-09-05: SMLtoJs version 4.3.4 is released.

* mael 2007-09-05: Support for the following Basis Library structures:
  Date, Time, Timer, Random, OS.Path and related test pages.

* mael 2007-09-05: Change in target file names to avoid .sml.js
  extensions. Such extensions break compatibility with SMLserver, which
  are setup to handle .sml-extensions...

* mael 2007-08-31: SMLtoJs version 4.3.3 is released.

* mael 2007-08-31: Various bug fixes.

* mael 2007-08-08: SMLtoJs version 4.3.2 is released.