(*$ListPair: LIST_PAIR *)

structure ListPair: LIST_PAIR =

(* PAIRS OF LISTS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author

RCS LOG

$Log$
Revision 1.2  2004/12/16 09:04:09  mael
changed directory kit/basislib to kit/basis and kit/basislib/basislib.mlb to kit/basis/basis.mlb (etc) so that the same mlb-files can be compiled with MLKit and MLton

Revision 1.1  2000/05/05 14:22:19  kfl
Changes made to the kit so as it can be compiled with mosml 1.99
----------------------------------------------------------------
  * Made a mosmlhacks.pm file.  The only thing in there now is the
    Word32 hack.
  * Added Compiler/compilerx86.pm
  * Used mosmlhack.pm in Compiler/compilerx86.pm
  * Changed sources.pm to use Compiler/compilerx86.pm and
    Compiler/Backend/X86/ExecutionX86.sml
  * Changes from Martin in basis/Initial.sml
  * Changed
	    val garbage_collection = ref true
    to
	    val garbage_collection = ref false
    in src/Common/Flags.sml
  * Changed substring to String.substring in Parsing/Topdec.lex.sml
  * Changed `status' to `~1' on line 128 in
    Manager/ManagerObjects.sml
  * Renamed Edlib/List.sml Edlib/edList.sml
    and Edlib/ListPair.sml Edlib/edListPair.sml
  * Parsing/Topdec.grm.sig renamed to Parsing/Topdec.grm-sig.sml (also
    in Common/common.pm)
  * Renamed Common/BasicIO.sml Common/commonBasicIO.sml
  * Corrected a sharing spec bug (not decteded by sml/nj) in
    Compiler/Backend/X86/CodeGenX86.sml

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
