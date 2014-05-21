signature EDLIB_GENERAL =
sig

(* GENERAL DEFINITIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		21 Sep 89

Maintenance:	Author


DESCRIPTION

   Types, exceptions and functions that are widely used or that don't fit
   anywhere else.

NOTES

   This would be the place to define the equality function, but that can't be
   redefined.

RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:03  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.7  91/02/12  18:21:01  18:21:01  db (Dave Berry)
Added the oo function for composing a unary function and a binary
curried function.

Revision 1.6  91/02/11  18:32:59  18:32:59  db (Dave Berry)
Added comments to the type specifications.

Revision 1.5  91/02/04  15:43:18  15:43:18  db (Dave Berry)
Removed Io exception.  Instream and Outstream now provide their own
Io exceptions (usually the same as the pervasive one).

Revision 1.4  91/01/25  17:33:19  17:33:19  db (Dave Berry)
Added before function.

Revision 1.3  91/01/25  16:55:00  16:55:00  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:06:16  17:06:16  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:47:49  16:47:49  db (Dave Berry)
Initial revision


*)


(* PERVASIVES *)

  exception Bind and Match and Interrupt

  type exn and unit

  val o: ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
  val <> : ''a * ''a -> bool

  (* The next two exceptions are used by the redefinition of the
     pervasive arithmetic operations. *)
  exception Overflow
  and OldDiv


(* TYPES *)

  datatype 'a Option = None | Some of 'a
   (* Option is used when a value is optional, or when an operation may or
      may not return a value. *)

  type Nat = int
   (* Nat is used when a function expects a positive integer or zero.  Such
      a function should raise the Nat exception if it is passed a negative
      integer. *)

  datatype ('a, 'b) Result = OK of 'a | Fail of 'b
   (* Result is used when a function can either succeed, returning a value,
      or fail, returning an error value. *)


(* SYSTEM *)

  exception NotImplemented of string
   (* NotImplemented fn; raised if the function called fn isn't provided
      in this implementation of the library. *)
 
  exception Nat of string * int
   (* Nat (fn, n); raised if the function named fn is passed a negative
      integer n to an argument of type Nat. *)


(* FUNCTIONS *)

  val id: 'a -> 'a
   (* id x; returns x.  The identity function. *)

  val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
   (* curry f; (curry f) x y = f (x, y). *)

  val uncurry: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
   (* uncurry f; (uncurry f) (x, y) = f x y. *)

  (* infix 3 oo *)
  val oo: ('c -> 'd) * ('a -> 'b -> 'c) -> 'a -> 'b -> 'd
   (* f oo g; composition of a unary and curried binary function.
     (f oo g) x y = f (g x y). *)

  (* infix 0 before *)
  val before: 'a * 'b -> 'a
   (* x before y; evaluates x and y in order, and returns the value of x. *)

  val iterate: Nat -> ('a -> 'a) -> 'a -> 'a
   (* iterate n f;  performs self-composition of f, n times.
      In other words, iterate n f base = f (f ... (f base) ...),
      with n occurrences of f. *)
      
  val repeat: Nat -> ('a -> 'b) -> 'a -> unit
   (* repeat n f arg; applies f to arg n times, presumably for
      the side effects performed by f.  In other words,
      repeat n f arg = (f arg; ...; f arg; ()). *)

  val until: ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
   (* until p f arg; returns f (f ...  (f arg) ...) for the smallest number
      of application of f such that p applioed to the result is true.
      f is applied at least once. *)

  val primRec: ('a -> Nat -> 'a) -> 'a -> Nat -> 'a
   (* primRec f init n; Primitive recursion.  Returns the nth application
      of f, starting from init.  Differs from iterate in the type of f. *)

end
