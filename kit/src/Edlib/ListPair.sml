(*$ListPair: LIST_PAIR *)

structure ListPair: LIST_PAIR =

(* PAIRS OF LISTS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author

RCS LOG

$Log$
Revision 1.2  1998/11/11 18:59:15  mael
tuning

Revision 1.1  1998/01/22 17:01:16  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.4  91/01/25  20:17:26  20:17:26  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.3  91/01/25  15:44:02  db
Used functions defined in CoreUtils structure where appropriate.

Revision 1.2  91/01/24  17:21:28  17:21:28  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:58:56  14:58:56  db (Dave Berry)
Initial revision


*)

struct

  exception Zip
  fun zip ([], []) = []
  |   zip (x::xs, y::ys) = (x,y) :: zip (xs, ys)
  |   zip (_, _) = raise Zip

  fun unzip []           = ([] ,[])
  |   unzip ((x,y)::xys) =
        let val (xs, ys) = unzip xys
	in (x :: xs, y :: ys)
	end

  local
     fun from1  ([], l2)    = l2
     |   from1  (x::xs, l2) = x :: from2 (xs, l2)
     and from2  (l1, [])    = l1
     |   from2  (l1, y::ys) = y :: from1 (l1, ys)
  in
     val interleave = from1
  end

  fun unravel []  = ([] ,[])
  |   unravel [x] = ([x],[])
  |   unravel (x::y::xys) =
         let val (xs, ys) = unravel xys in
            (x::xs, y::ys)
         end

  fun merge p (l, []) = l
  |   merge p ([], l) = l
  |   merge p (l1 as (x::xs), l2 as (y::ys)) =
         if p x y then x :: merge p (xs, l2)
                      else y :: merge p (l1, ys)
end
