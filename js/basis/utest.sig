(** Generic functionality for running unit tests.

This structure provides functionality for keeping track of the result
of running unit tests. Using tstStart and tstEnd allows for printing a
report of the unit test execution.
*)

signature UTEST = sig
  val tstStart   : string -> unit
  val tstEnd     : unit -> unit

  val range      : int * int -> (int -> bool) -> bool

  val check      : bool -> string
  val check'     : (unit -> bool) -> string
  val checkrange : int * int -> (int -> bool) -> string

  val tst0       : string -> string -> unit
  val tst        : string -> bool -> unit
  val tst'       : string -> (unit -> bool) -> unit
  val tstrange   : string -> int * int -> (int -> bool) -> unit
end

(**

[tstStart name] initializes the unit test execution and resets error
counters. It also prints out a message stating that the test 'name'
has started.

[tstEnd()] reads the error counters and prints a report.

[range (first,last) f] returns true if f(x) is true for all x in the
interval [first;last]. Returns false, otherwise.

[check b] returns "OK" if b is true and "ERR" if b is false. Updates
the error counters appropriately.

[check' f] returns "OK" if f() returns true, "ERR" if f() returns
false, and "EXN" if f() raises an exception. Updates the error
counters appropriately.

[tst name b] assumes b is a boolean resulting from a subtest
'name'. Writes out a subtest report and updates the error counters
appropriately.

[tst' name f] executes the test function f() for the subtest
'name'. Writes out a subtest report based on the result and updates
the error counters appropriately.

*)
