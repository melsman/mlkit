(*$ListSort : LIST_SORT List ListPair General *)

structure ListSort: LIST_SORT =

(* FUNCTIONS FOR SORTING AND PERMUTING LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        6 Feb 1991

Maintenance:	Author 


DESCRIPTION

   A straightforward implementation; not always the most efficient.


NOTES

   These functions were priginally in the main List structure.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:17  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.1  91/02/11  20:10:55  20:10:55  db (Dave Berry)
Initial revision


*)

struct


(* MANIPULATORS *)

  fun sort p [] = []
  |   sort p [x] = [x]
  |   sort p [x1,x2] = if p x1 x2 then [x1, x2] else [x2, x1]
  |   sort p l =
        let val (l1, l2) = ListPair.unravel l
         in ListPair.merge p (sort p l1, sort p l2)
        end

  local
    fun plug a [] = [[a]]
    |   plug a (l as x::xs) =
        (a::l) :: (map (fn l => x :: l) (plug a xs))
  in
    fun perms [] = [[]]
    |   perms (x::xs) =
	  List.foldR (EdlibGeneral.curry op @) [] (map (plug x) (perms xs))
  end

end
