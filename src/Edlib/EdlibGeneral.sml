(*$General: GENERAL *)

structure EdlibGeneral: EDLIB_GENERAL =

(* GENERAL DEFINITIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        21 Sep 89

Maintenance:	Author

RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:05  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.6  91/02/12  18:13:27  18:13:27  db (Dave Berry)
Added the oo function for composing a unary function with a binary
curried function.  This was originally defined in Combinator.sml,
which still contains a reference to this definition.

Revision 1.5  91/02/04  14:53:59  14:53:59  db (Dave Berry)
Removed Io exception.  Instream and Outstream now contain their own
Io exceptions.

Revision 1.4  91/01/25  20:17:12  20:17:12  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.3  91/01/25  17:34:24  17:34:24  db (Dave Berry)
Added before function.

Revision 1.2  91/01/24  17:21:12  17:21:12  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:52:25  14:52:25  db (Dave Berry)
Initial revision


*)

struct


(* PERVASIVES *)

  exception Bind = Bind
  and Match = Match
  and Interrupt

  type unit = unit
  and  exn = exn

  val op o = op o
  val op <> = op <>

  exception Overflow
  and OldDiv = Div


(* TYPES *)

  datatype 'a Option = None | Some of 'a

  datatype ('a, 'b) Result = OK of 'a | Fail of 'b

  type Nat = int


  exception Nat of string * int

  exception NotImplemented of string


(* FUNCTIONS *)

  fun id x = x

  fun curry f x y = f (x, y)

  fun uncurry f (x, y) = f x y

  infix 3 oo;
  fun op oo (f, g) x y = f (g x y);

  infix 0 before
  fun x before _ = x;

  local
    fun iterate' 0 _ x = x
    |   iterate' n f x = iterate' (n-1) f (f x)
  in
    fun iterate n f x =
	  if n < 0 then raise Nat ("iterate", n)
	  else iterate' n f x
  end

  local
    fun repeat' 0 _ _ = ()
    |   repeat' n f x = (f x; repeat' (n-1) f x)
  in
    fun repeat n f x =
	  if n < 0 then raise Nat ("repeat", n)
	  else repeat' n f x
  end

  local
    fun primRec' _ x 0 = x
    |   primRec' f x n = primRec' f (f x n) (n-1)
  in
    fun primRec f x n =
	  if n < 0 then raise Nat ("primRec", n)
	  else primRec' f x n
  end

  fun until p f x =
    if p x then x
    else until p f (f x);
end
