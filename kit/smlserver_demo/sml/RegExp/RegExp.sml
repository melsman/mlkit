(* Bug:

This does not work.

  regExp "Name: ([a-zA-Z ]+) Tlf: ([0-9 ]+)" "Name: Hans Hansen Tlf: 66 66 66 66

  If gives no match, but there is a match.

It seems that the first group includes a space too much such that no
match is found - some backtracking needs to be done 2001-09-10, Niels

Another bug:
  regExpBool "(a(b+))*" "babbabbb" gives true
  regExp "(a(b+))*" "babbabbb" gives ["", "", ""] 

2001-09-10, Niels
*)

structure RegExp :> REG_EXP =
  struct
    structure R = RegExpFn (structure P = AwkSyntax
			    structure E = BackTrackEngine)
    
(*    type regexp = R.regexp*)

    exception RegExp = RegExpSyntax.RegExp

    fun regcomp s = R.compileString s

    (* returns true if some substring of s matches regex, false otherwise. *)
    fun regExpBool regexp_s s = 
      case StringCvt.scanString (R.find (regcomp regexp_s)) s of
	NONE => false
      | SOME m => true

  (* returns SOME(vec) if some substring of s
   matches regex, NONE otherwise.  In case of success, vec is the
   match vector, a vector of substrings such that vec[0] is the
   (longest leftmost) substring of s matching regex, and vec[1],
   vec[2], ... are substrings matching the parenthesized groups in pat
   (numbered 1, 2, ... from left to right in the order of their
   beginning of the underlying string.  For a group that takes part in
   the match repeatedly, such as the group (b+) in "(a(b+))*" when
   matched against "babbabbb", the corresponding substring is the last
   (rightmost) one matched. *)
(*    fun regexec regexp s = (* substring vector option *)
      let
	fun sub_mt_item s NONE = Substring.all ""
	  | sub_mt_item s (SOME{pos=pos,len=len}) = Substring.substring(s,pos,len)
      in
	case StringCvt.scanString (R.find regexp) s of
	  NONE => NONE
	| SOME m => SOME (Vector.fromList (MatchTree.toList (sub_mt_item s) m))
      end*)

    fun pp_mt_item NONE = "NONE"
      | pp_mt_item  (SOME{pos=pos,len=len}) = "pos: " ^ (Int.toString pos) ^ ", len: " ^ (Int.toString len)

    fun pp_mt r s = 
      let
      in
	case StringCvt.scanString (R.find (regcomp r)) s of
	  NONE => ""
	| SOME m => MatchTree.pp pp_mt_item m
      end

    fun sub_mt_item s NONE = ""
      | sub_mt_item s (SOME{pos=pos,len=len}) = Substring.string (Substring.substring(s,pos,len))

    fun regExp r s = 
      case StringCvt.scanString (R.find (regcomp r)) s of
	NONE => NONE
      | SOME m => SOME (MatchTree.toList (sub_mt_item s) m)

end
(*

val res = StringCvt.scanString (Regex.find (regexp "[a-zA-ZA0-9ÆØÅaæøå '\\-]+")) "???abc???"

fun eat m_opt = 
  case m_opt of
  NONE => "None\n"
| SOME m => 
    let
      val (len,pos) = case MatchTree.root m
	of NONE => (0,0)
	| SOME {len,pos} => (len,pos)
    in
      "Some, len: " ^ (Int.toString len) ^", pos: " ^ (Int.toString pos) ^ "\n"
    end

val _ = print (eat res)
  end

*)