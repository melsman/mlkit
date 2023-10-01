(* StringCvt -- new basis 1995-04-06, 1995-10-26, 1996-04-01, 1997-06-03 *)
(* From Moscow ML *)

(** SigDoc *)
structure StringCvt : STRING_CVT = struct
  fun concat x = CharVector.concat x
  fun s ^ s' = concat [s, s']
  fun implode x = CharVector.fromList x
  fun size x = CharVector.length x

  fun rev l =
    let fun rev_rec(p as ([], acc)) = p
	    | rev_rec(x::xs, acc) = rev_rec(xs, x::acc)
    in #2 (rev_rec(l,nil))
    end

  (* Body *)
  datatype radix = BIN | OCT | DEC | HEX;
  datatype realfmt =
    SCI of int option   (* scientific,  arg = # dec. digits, dflt=6 *)
  | FIX of int option   (* fixed-point, arg = # dec. digits, dflt=6 *)
  | GEN of int option   (* auto choice of the above,                *)
                        (* arg = # significant digits, dflt=12      *)
  | EXACT

  type cs = int	  (* the state of a string character source   *)

  type ('a, 'b) reader = 'b -> ('a * 'b) option

  fun padLeft c n s =
    let val ssize = size s
	  fun f 0 = []
	    | f n = c :: f (n-1)
    in if n <= ssize then s
	 else (implode (f (n-ssize))) ^ s
    end

  fun padRight c n s =
    let val ssize = size s
	  fun f 0 = []
	    | f n = c :: f (n-1)
    in if n <= ssize then s
	 else s ^ (implode (f (n-ssize)))
    end

  fun scanString scan s =
    let val len = size s
	  fun getc i = if i >= len then NONE
		       else SOME (CharVector.sub (s,i), i+1)
    in case scan getc 0
	   of NONE => NONE
	    | SOME (res, _) => SOME res
    end

  fun scanList scan cs =
    let fun getc [] = NONE
	    | getc (c::cs) = SOME(c,cs)
    in scan getc cs
    end

  fun splitl p getc src =
    let fun h (cs, src) =
          case getc src
	    of NONE => (implode (rev cs), src)
	     | SOME(c, rest) => if p c then h (c::cs, rest)
				else (implode (rev cs), src)
    in h ([], src)
    end

  fun takel p getc src = #1 (splitl p getc src);
  fun dropl p f s = #2(splitl p f s)

  local fun isSpace c  = c = #" " orelse #"\009" <= c andalso c <= #"\013"
  in fun skipWS getc = dropl isSpace getc
  end

end (*structure StringCvt*)
