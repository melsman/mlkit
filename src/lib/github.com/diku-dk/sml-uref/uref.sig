(** Unifiable references.
 *
 * Unifiable references provide a Union/Find data type with a ref-like
 * interface.  A Union/Find structure consists of a type constructor
 * 'a uref with operations for creating an element of type 'a uref (uRef),
 * getting the contents of an element (!!), checking for equality of
 * two elements (equal), and for unifying two elements (unify).
 *)

signature UREF = sig
  type 'a uref

  val uref    : 'a -> 'a uref
  val !!      : 'a uref -> 'a
  val ::=     : 'a uref * 'a -> unit

  val unify   : ('a * 'a -> 'a) -> 'a uref * 'a uref -> unit

  val eq      : 'a uref * 'a uref -> bool
  val compare : ('a * 'a -> order) -> 'a uref * 'a uref -> order
end

(**

[type 'a uref] is the type of uref-elements with contents of type 'a.

[uref x] creates a new element with contents x.

[!!e] returns the contents of e.

[e ::= x] updates the contents of e to be x.

[unify f (e, e')] makes e and e' equal; if v and v' are the contents
of e and e', respectively, before unioning them, then the contents of
the unioned element is f(v,v').

[eq (e, e')] returns true if and only if e and e' are either made by
the same call to uref or if they have been unioned (see below).

[compare cmp (e, e')] returns EQUAL if eq (e, e') returns true;
otherwise return cmp (!!e, !!e').

Discussion:

The uref type constructor is analogous to the ref type constructor as
expressed in the following table:

 -------------------------------------------------------------------
 type                  'a ref                'a uref
 -------------------------------------------------------------------
 introduction          ref                   uref
 elimination           !                     !!
 equality              =                     eq
 updating              :=                    ::=
 unioning                                    unify
 -------------------------------------------------------------------

The main difference between 'a ref and 'a uref is in the unify
operation.  Without unify, 'a ref and 'a uref can be used
interchangebly.  An assignment to a reference changes only the
contents of the reference, but not the reference itself.  In
particular, any two pointers that were different (in the sense of the
equality predicate = returning false) before an assignment will still
be so.  Their contents may or may not be equal after the assignment,
though.  In contrast, applying the unify operation to two uref
elements makes the two elements themselves equal (in the sense of the
predicate equal returning true).  As a consequence their contents will
also be identical; the actual content is determined by a binary
function parameter to unify.

AUTHOR:

This software was originally authored by Fritz
Henglein. Simplifications have been made by Henning Niss (eliminating
redundant matches) and Martin Elsman (removed some exposed
functionality such as link and union.

Copyright (c) 199x-2020 Fritz Henglein, Henning Niss, University of
Copenhagen.

*)
