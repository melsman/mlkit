(*$String : STRING General StringType List *)

structure String: STRING =

(* ASCII STRINGS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        4 Oct 1989

Maintenance:	Author

DESCRIPTION

   Standard functions on the built-in type "string".

   Functions such as  search  and  index  take an integer
   offset.  This is because it's more efficient to index into
   a string than to take a substring.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:35  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.16  1991/10/22  18:34:46  db
Fixed bug in string function.

Revision 1.15  91/03/08  17:00:39  17:00:39  db (Dave Berry)
Renamed existing eq and ne functions to eqMode and neMode; added new
eq and ne functions that match the type given in generic signatures.

Revision 1.14  91/03/06  16:38:36  16:38:36  db (Dave Berry)
Added print function(s).

Revision 1.13  91/02/22  19:09:50  19:09:50  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.

Revision 1.12  91/02/22  15:34:42  15:34:42  db (Dave Berry)
Changed references to LexOrdList.prefixes to List.prefixes, because
the function has been moved.

Revision 1.11  91/02/22  14:48:27  14:48:27  db (Dave Berry)
Removed words, words', wordSingles and wordSingles' functions.
These have been replaced by a more general version in StringParse.sml.

Revision 1.10  91/02/21  18:07:56  18:07:56  db (Dave Berry)
Added Mode datatype, changed search, index and substitution functions (etc.)
to take a mode as the first parameter.

Revision 1.9  91/02/14  16:48:27  16:48:27  db (Dave Berry)
Added StringType to the dependency list.

Revision 1.8  91/02/12  14:41:46  14:41:46  db (Dave Berry)
Moved file and fromFile functions to StringParse.sml.
Added dependency on LexOrdList.

Revision 1.7  91/02/11  20:49:12  20:49:12  db (Dave Berry)
Removed Object sub-structures.
Moved list-like functions to StringListOps.sml.
Removed inclusion of StringType.
Several references to other structures changed to match the new names
of those structures.
This forms part of the major reorganisation of the library.

Revision 1.6  91/02/04  15:10:53  15:10:53  db (Dave Berry)
InStream and OutSream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.5  91/01/31  17:47:55  17:47:55  db (Dave Berry)
Added type.

Revision 1.4  91/01/30  19:01:30  19:01:30  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.3  91/01/30  17:47:01  17:47:01  db (Dave Berry)
Added skipSpaces function.

Revision 1.2  91/01/25  20:21:36  20:21:36  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.1  90/12/20  15:28:42  15:28:42  db (Dave Berry)
Initial revision


*)

struct

  open OldString
  open EdlibGeneral

(* PERVASIVES *)

  type string = string

  exception Chr = Chr
  and Ord = Ord

  val size = size
  val ord = ord
  val chr = chr
  val explode = explode
  val implode = implode
  val op ^ = op ^


(* TYPES *)

  type T = string

  datatype Mode = IgnoreCase | MatchCase



(* CREATORS *)

  exception Size of string * int

  fun create n s =
	if n < 0 then raise Size ("create", n)
	else iterate n (fn x => s ^ x) ""


(* CONVERTORS *)

  local
    fun show' h =
      case h
      of "\n"   => ["\\n"]
      |  "\t"   => ["\\t"]
      |  "\\"   => ["\\\\"]
      |  "\""   => ["\\\""]
      |  " "    => [" "]
      |  "\127" => ["\\127"]
      |  _ =>
            if StringType.isVisible h then [h]
            else if StringType.isControl h then
              ["\\^", chr (ord h + ord "@")]
            else
              let val i = ord h
                  val s = CoreUtils.intToString i
                  val s' = if i < 10 then "00" ^ s
                           else if i < 100 then "0" ^ s
                           else s
              in ["\\", s]
              end

    fun show nil = nil
    |   show (h::t) = show' h @ show t
  in
    fun string s = "\"" ^ implode (show (explode s)) ^ "\""
  end

  fun print os s = TextIO.output (os, string s)


(* ITERATORS *)

  fun map f s = implode (List.map f (explode s))

  fun apply f s = List.apply f (explode s)

  fun mapAll p f s = implode (List.mapAll p f (explode s))

  fun applyAll p f s = (List.applyAll p f (explode s))


(* SELECTORS *)

  exception Subscript of string * int

  exception Extract of int * int

  fun extract start finish s =
	implode (List.extract start finish (explode s))
	handle List.Extract x => raise Extract x


(* SOME MANIPULATORS *)

  exception Empty of string

  fun upper s =
        if StringType.isLower s
        then chr (ord s + ord "A" - ord "a") ^ extract 1 (size s) s
        else s
        handle StringType.Empty _ => raise Empty "upper"

  fun lower s =
        if StringType.isUpper s
        then chr (ord s + ord "a" - ord "A") ^ extract 1 (size s) s
        else s
        handle StringType.Empty _ => raise Empty "lower"

  fun ascii s =
        if StringType.isAscii s then s
        else chr (ord s - 128) ^ extract 1 (size s) s
        handle StringType.Empty _ => raise Empty "ascii"

  fun control s =
        let val s' = chr (ord s - 64)
        in if StringType.isControl s'
           then s' ^ extract 1 (size s) s
           else s
        end
        handle Chr => s
        |      StringType.Empty _ => raise Empty "control"


(* LOCAL *)

  fun ignoreCase p =
	fn x => p (lower x) orelse p (upper x)
   (* ignoreCase p; returns a predicate based on p that ignores the case
      of its argument. *)

  fun dropPrefix p s = implode (List.dropPrefix p (explode s))


(* OBSERVERS *)

  fun forAll MatchCase p s = List.forAll p (explode s)
  |   forAll IgnoreCase p s = List.forAll (ignoreCase p) (explode s)

  fun exists MatchCase p s = List.exists p (explode s)
  |   exists IgnoreCase p s = List.exists (ignoreCase p) (explode s)

  fun prefixes MatchCase s1 s2 n =
      ( List.prefixes (explode s1) (explode (extract n (size s2) s2))
	handle Extract _ => raise Subscript ("prefixes", n)
      )
  |   prefixes IgnoreCase s1 s2 n =
      ( List.prefixes (List.map lower (explode s1))
			    (List.map lower (explode (extract n (size s2) s2)))
	handle Extract _ => raise Subscript ("prefixes", n)
      )

  fun postfixes MatchCase s1 s2 n =
      ( List.prefixes (rev (explode s1))
			      (rev (explode (extract 0 n s2)))
	handle Extract _ => raise Subscript ("postfixes", n)
      )
  |   postfixes IgnoreCase s1 s2 n =
      ( List.prefixes
	    (List.map lower (rev (explode s1)))
	    (List.map lower (rev (explode (extract n (size s2) s2))))
	handle Extract _ => raise Subscript ("prefixes", n)
      )

  fun eqMode MatchCase s s' = (s = s')
  |   eqMode IgnoreCase s s' = (map lower s = map lower s')

  fun neMode MatchCase s s' = (s <> s')
  |   neMode IgnoreCase s s' = (map lower s <> map lower s')

  val eq = eqMode MatchCase

  val ne = neMode MatchCase

  val fixedWidth = false


(* MANIPULATING THE NTH ELEMENT *)

  infix 9 sub
  fun s sub n = List.sub (explode s, n)
		handle List.Subscript x => raise Subscript x

  fun nth n s = s sub n
		handle Subscript _ => raise Subscript ("nth", n)


(* SEARCHING AND INDEXING *)

  local
    fun search' _ nil _ = Fail ()
    |   search' s (s' as _::t') n =
      if List.prefixes s s' then OK n
      else search' s t' (n+1)	(* Boyer and Moore?  Never heard of them! *)
  in
    fun search mode s' s n =
	  if n < 0 orelse n >= size s then raise Subscript ("search", n)
	  else if s' = "" then OK n
	  else if mode = MatchCase then
	    search' (explode s') (explode (extract n (size s) s)) n
	  else
	    search' (List.map lower (explode s'))
		    (List.map lower (explode (extract n (size s) s))) n
    fun revSearch mode s' s n =
	  if n < 0 orelse n > size s then raise Subscript ("revSearch", n)
	  else if s' = "" then OK (size s - 1)
	  else 
	  let val s1' = (List.map lower (List.rev (explode s')))
	      val s1  = (List.map lower (List.rev (explode (extract 0 n s))))
	  in case search' s1' s1 0 of
	       (OK i) => OK (n - i - size s')
	     |  Fail () => Fail ()
	  end
  end

  fun occurs mode s' s n =
	case search mode s' s n 
	of OK _ => true
	|  Fail () => false
	handle Subscript _ => raise Subscript ("occurs", n)

  fun revOccurs mode s' s n =
	case revSearch mode s' s n 
	of OK _ => true
	|  Fail () => false
	handle Subscript _ => raise Subscript ("revOccurs", n)

  fun index MatchCase p s n =
      (( case List.index p (explode (extract n (size s) s)) of
	    OK i => OK (i + n)
	  | x => x
       )
       handle Extract _ => raise Subscript ("index", n)
      )
  |   index IgnoreCase p s n =
      (( case List.index (ignoreCase p) (explode (extract n (size s) s)) of
	    OK i => OK (i + n)
	  | x => x
       )
       handle Extract _ => raise Subscript ("index", n)
      )

  fun revIndex MatchCase p s n =
      (( case List.index p (rev (explode (extract 0 n s))) of
	    OK i => OK (n - i - 1)
	  | x => x
       )
       handle Extract _ => raise Subscript ("revIndex", n)
      )
  |   revIndex IgnoreCase p s n =
      (( case List.index (ignoreCase p) (rev (explode (extract 0 n s))) of
	    OK i => OK (n - i - 1)
	  | x => x
       )
       handle Extract _ => raise Subscript ("revIndex", n)
      )


(* OTHER MANIPULATORS *)

  exception Char of string * string

  fun skipSpaces s = dropPrefix (not o StringType.isVisible) s

  fun subst MatchCase c s' s =
	if size c <> 1 then raise Char ("subst", c)
	else implode (List.updateAll (eqMode MatchCase c) s' (explode s))
  |   subst IgnoreCase c s' s =
	if size c <> 1 then raise Char ("subst", c)
	else
	  let fun changeFn ch =
		    if StringType.isLower ch then map lower s'
		    else if StringType.isUpper ch then map upper s'
		    else s'
	  in implode (List.changeAll (eqMode IgnoreCase c) changeFn (explode s))
	  end

  fun showAscii s =
	let val s' = string s
	in extract 1 (size s') s'
	end

  fun rev s = implode (List.rev (explode s))

  fun padL c w s =
	if size c <> 1 then raise Char ("padL", c)
	else if size s >= w then s
	else (create (w - size s) c) ^ s

  fun padR c w s =
	if size c <> 1 then raise Char ("padL", c)
	else if size s >= w then s
	else s ^ (create (w - size s) c)

  fun padC c w s =
	if size c <> 1 then raise Char ("padL", c)
	else if size s >= w then s
	else
	  let val n = w - size s
	      val l = n div 2
	      val r = if n mod 2 = 0 then n div 2 else n div 2 + 1
	  in (create l c) ^ s ^ (create r c)
	  end

  fun truncL w s =
	if size s <= w then s
	else extract (size s - w) (size s) s

  fun truncR w s =
	if size s <= w then s
	else extract 0 w s

  fun truncC w s =
	if size s <= w then s
	else
	  let val n = size s - w
	      val r = n div 2
	      val l = if n mod 2 = 0 then n div 2 else n div 2 + 1
	  in extract l r s
	  end

  fun dropL c s =
	if size c <> 1 then raise Char ("dropL", c)
	else dropPrefix (fn x => (x = c)) s

  fun dropR c s =
	if size c <> 1 then raise Char ("dropR", c)
	else rev (dropPrefix (fn x => (x = c)) (rev s))

end
