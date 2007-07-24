(* Stream module
 * Copyright, Martin Elsman 2003-01-07 
 * GPL Licence
 *)

structure Stream : STREAM =
  struct
    type loc = word
    type instream = loc * char list
    type outstream = loc * char list
    fun out (e, (loc:loc,es)) = (loc+0w1, e::es)
    fun get (loc:loc,e::es) = (e, (loc+0w1, es))
      | get _ = raise Fail "empty in-stream"

    fun outw1 (e, (loc:loc,es)) = (loc+0w1, Byte.byteToChar e::es)
    fun getw1 (loc:loc,e::es) = (Byte.charToByte e, (loc+0w1, es))
      | getw1 _ = raise Fail "empty in-stream"

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

    fun outcw2 (w,s) = (* 255*256=65280 *)
	if w <= 0w65279 then 
	    let val c1 = Char.chr(Word.toInt (w div 0w256))
		val c2 = Char.chr(Word.toInt (w mod 0w256))
	    in out(c2,out(c1,s))
	    end
	else 
	    let val s = out(Char.chr 255,s)
	    in outw(w,s)
	    end
    fun getcw2 s =
	let val (c,s) = get s
	    val w = Word.fromInt(Char.ord c)
	in if w = 0w255 then getw s
	   else 
	       let val (c2,s) = get s
		   val w2 = Word.fromInt(Char.ord c2)
	       in (w*0w256 + w2,s)
	       end
	end

    fun outw32 (w,s) =
	let val >> = Word32.>>
	    infix >>
	    fun extract i = Char.chr(Word32.toInt(Word32.andb(w >> i,0wxff)))
	    val s = out(extract 0w0,s)
	    val s = out(extract 0w8,s)
	    val s = out(extract 0w16,s)
	in out(extract 0w24,s)
	end
    fun getw32 (loc:loc,c0::c1::c2::c3::es) = 
	let val w0 = Word32.fromInt(Char.ord c0)
	    val w1 = Word32.fromInt(Char.ord c1)
	    val w2 = Word32.fromInt(Char.ord c2)
	    val w3 = Word32.fromInt(Char.ord c3)
	    val << = Word32.<<
	    infix <<
	    val w = w0 + (w1 << 0w8) + (w2 << 0w16) + (w3 << 0w24) 
	in (w, (loc+0w4, es))
	end
      | getw32 _ = raise Fail "empty in-stream"

    fun outcw32 (w,s) =
	if w <= 0w254 then out(Char.chr(Word32.toInt w),s)
	else 
	    let val s = out(Char.chr 255,s)
	    in outw32(w,s)
	    end
    fun getcw32 s =
	let val (c,s) = get s
	    val w = Word32.fromInt(Char.ord c)
	in if w = 0w255 then getw32 s
	   else (w,s)
	end

    fun getLocIn (loc,es) = loc
    fun getLocOut (loc,es) = loc
    fun toString (loc,es) = implode (rev es)
    fun openOut () = (0w0:loc,nil)
    fun openIn (s:string) = (0w0:loc, explode s)
  end
