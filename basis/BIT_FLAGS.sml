(** General operations on POSIX bit flags. *)

signature BIT_FLAGS =
  sig
    eqtype flags
    val toWord    : flags -> SysWord.word
    val fromWord  : SysWord.word -> flags
    val all       : flags
    val flags     : flags list -> flags
    val intersect : flags list -> flags
    val clear     : flags * flags -> flags
    val allSet    : flags * flags -> bool
    val anySet    : flags * flags -> bool
  end

(**

[eqtype flags] This type is the abstract representation of a set of
system flags.

[toWord flags]
[fromWord word]

These functions convert between the abstract flags type and a
bit-vector that is represented as a system word. The interpretation of
the bits is system-dependent, but follows the C language binding for
the host operating system. Note that there is no error checking on the
fromWord function's argument.

[all] represents the union of all flags. Note that this may well be a
superset of the flags value defined in a matching structure. For
example, BIT_FLAGS is used to define the flags specified by the POSIX
standard; a POSIX-conforming operating system may provide additional
flags that will not be defined in the Posix structure but could be set
in the all value.

[flags l] returns a value that represents the union of the flags in
the list l. The expression flags [] denotes the empty set.

[intersect l] returns a value that represents the intersection of the
sets of flags in the list l. The expression intersect [] denotes all.

[clear (fl1, fl2)] returns the set of those flags in fl2 that are not
set in fl1, i.e., the set difference fl2 \ fl1. It is equivalent to:
fromWord(SysWord.andb(SysWord.notb (toWord fl1), toWord fl2))

[allSet (fl1, fl2)] returns true if all of the flags in fl1 are also
in fl2 (i.e., this tests for inclusion of fl1 in fl2).

[anySet (fl1, fl2)] returns true if any of the flags in fl1 is also in
fl2 (i.e., this tests for non-empty intersection).

[Discussion]

The number of distinct flags in an implementation of the BIT_FLAGS
interface must be less than or equal to the number of bits in the
SysWord.word type. In addition, fromWord o toWord must be the identity
function, and toWord o fromWord must be equivalent to fn w =>
SysWord.andb(w, toWord all)

*)
