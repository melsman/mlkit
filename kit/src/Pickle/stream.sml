(* Stream module
 * Copyright, Martin Elsman 2003-01-07 
 *)

structure Stream : STREAM =
  struct
    type IN = int
    type OUT = int
    type loc = word
    type 'k stream = loc * char list
    fun out (e, (loc:loc,es)) = (loc+0w1, e::es)
    fun get (loc:loc,e::es) = (e, (loc+0w1, es))
      | get _ = raise Fail "empty in-stream"
    fun outw (w,s) =
	let val >> = Word.>>
	    infix >>
	    fun extract i = Char.chr(Word.toInt(Word.andb(w >> i,0wxff)))
	    val s = out(extract 0w0,s)
	    val s = out(extract 0w8,s)
	    val s = out(extract 0w16,s)
	in out(extract 0w24,s)
	end
    fun getw (loc:loc,c0::c1::c2::c3::es) = 
	let val w0 = Word.fromInt(Char.ord c0)
	    val w1 = Word.fromInt(Char.ord c1)
	    val w2 = Word.fromInt(Char.ord c2)
	    val w3 = Word.fromInt(Char.ord c3)
	    val << = Word.<<
	    infix <<
	    val w = w0 + (w1 << 0w8) + (w2 << 0w16) + (w3 << 0w24) 
	in (w, (loc+0w4, es))
	end
      | getw _ = raise Fail "empty in-stream"

    fun outcw (w,s) =
	if w <= 0w254 then out(Char.chr(Word.toInt w),s)
	else 
	    let val s = out(Char.chr 255,s)
	    in outw(w,s)
	    end
    fun getcw s =
	let val (c,s) = get s
	    val w = Word.fromInt(Char.ord c)
	in if w = 0w255 then getw s
	   else (w,s)
	end

    fun getLoc (loc,es) = loc
    fun toString (loc,es) = implode (rev es)
    fun openOut () = (0w0:loc,nil)
    fun openIn (s:string) = (0w0:loc, explode s)
  end
