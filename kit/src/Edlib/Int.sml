
structure Int: INT =

(* INTEGERS

Created by:	Dave Berry LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:11  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.8  91/03/06  16:38:15  16:38:15  db (Dave Berry)
Added print function(s).

Revision 1.7  91/02/22  16:43:56  16:43:56  db (Dave Berry)
Renamed **! exception to Power.

Revision 1.6  91/02/11  20:02:17  20:02:17  db (Dave Berry)
Removed Object sub-structure.  Moved comparison and string functions here
from IntObject.sml (now called IntParse.sml).  This forms part of the
major reorganisation of the library.

Revision 1.5  91/01/31  17:47:49  17:47:49  db (Dave Berry)
Added type.

Revision 1.4  91/01/30  19:01:23  19:01:23  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.3  91/01/25  20:17:21  20:17:21  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:21:20  17:21:20  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:53:58  14:53:58  db (Dave Berry)
Initial revision


*)

struct


(* PERVASIVES *)

  (* We implement the pervasives as they "should" be: *)

  type int = int

  exception Overflow = Overflow
  and Div = Div

  val op + =
	fn (x: int, y) =>
	  x + y
	  handle Plus => raise Overflow
  val op - =
	fn (x: int, y) =>
	  x - y
	  handle Sub => raise Overflow
  val op * =
	fn (x: int, y) =>
	  x * y
	  handle Prod => raise Overflow
  val op div =
	fn (x: int, 0) =>
	  raise Div
	|  (x: int, y) =>
	  x div y
	  handle OldDiv => raise Overflow
  val op mod =
	fn (x: int, 0) =>
	  raise Div
	|  (x: int, y) =>
	  x mod y
  val ~ =
	fn (x: int) =>
	  ~ x
	  handle Neg => raise Overflow
  val abs =
	fn (x: int) =>
	  abs x
	  handle Abs => raise Overflow

  val real = real


(* SYSTEM *)

  val minInt = SOME ~4096

  val maxInt = SOME 4095


(* TYPE *)

  type T = int


(* OBSERVERS *)

  val fixedWidth = false

  fun lt x y = (x: int) < y
  fun gt x y = (x: int) > y
  fun le x y = (x: int) <= y
  fun ge x y = (x: int) >= y
  fun eq x y = (x: int) = y
  fun ne x y = (x: int) <> y


(* CONVERTERS *)

  val string = Int.toString

  fun print os i = TextIO.output (os, string i)


(* MANIPULATORS *)

  infix 7 divMod
  fun x divMod y = (x div y, x mod y)

  infix 7 quot rem quotRem
  fun x quot y =
    (case (x < 0, y < 0)
     of (true, true) => x div y
     |  (true, false) => ~(x div ~y)
     |  (false, true) => ~(~x div y)
     |  (false, false) => ~x div ~y
    )

  fun x rem y =
    x - y * (x quot y)

  fun x quotRem y = (x quot y, x rem y)

  fun max x y: int = if x > y then x else y

  fun min x y: int = if x < y then x else y

  fun maxMin x y: int * int = if x < y then (y, x) else (x, y)

  infix 5 --
  fun x -- y = if x > y then nil
  		else x :: (x + 1 -- y)

  infix 8 **
  exception Power of int * int
  fun x ** 0 = 1
  |   x ** 1 = x
  |   x ** 2 = x * x
  |   x ** n =
    if n < 0 then raise Power (x, n)
    else
      let val f = if n mod 2 = 0 then 1 else x
      in ((x ** (n div 2)) ** 2) * f
      end
    handle
      Prod => raise Power (x, n)
end
