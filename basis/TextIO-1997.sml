 
structure TextIO : TEXT_IO =
  struct

    exception InputChar
    type elem = char
    type vector = string

    type instream = {ic: int, name: string}
    type outstream = {oc: int, name: string}

    (* Primitives *)
    fun sub_ (s:vector,i:int) : elem = prim ("__bytetable_sub", (s,i))
    fun size (s:vector): int = prim ("__bytetable_size", s)
    fun alloc_string_ (i:int) : vector = prim("allocStringML", i)
    fun update_ (s:vector,i:int,c:elem) : unit = prim("__bytetable_update", (s, i, c))
    fun chr_unsafe (i:int):char = prim ("id", i)

    val stdIn : instream = {ic=Initial.stdIn_stream, name="<stdIn>"}
    val stdOut : outstream = {oc=Initial.stdOut_stream, name="<stdOut>"}
    val stdErr : outstream = {oc=Initial.stdErr_stream, name="<stdErr>"}

    fun closeIn ({ic,...} : instream) : unit = prim ("closeStream", ic)

    fun inputN_ (ic:int, n:int) : string = prim ("inputStream", (ic, n))    
    fun input1_ (ic:int) : int = prim ("input1Stream", ic)    

    fun lookahead_ (ic: int): int = prim ("lookaheadStream", ic) 

    exception CannotOpen
    fun raiseIo fcn nam exn = 
      raise IO.Io {function = fcn^"", name = nam^"", cause = exn} 
    fun openIn (f: string) : instream = 
      {ic=prim("openInStream",(f, CannotOpen)), 
       name=f} handle exn => raiseIo "openIn" f exn
    fun openOut(f: string): outstream = 
      {oc=prim("openOutStream",(f, CannotOpen)), 
       name=f} handle exn => raiseIo "openOut" f exn
    fun openAppend(f: string): outstream =
      {oc=prim("openAppendStream",(f, CannotOpen)), 
       name=f} handle exn => raiseIo "openAppend" f exn

    fun closeOut({oc, name}: outstream) : unit = prim ("closeStream", oc)

    fun flushOut({oc,name}: outstream) : unit = prim ("flushStream", oc)

    fun output0(os as {oc,name},str,function):unit =
      (prim ("outputStream", (oc, str, IO.ClosedStream));
       if os = stdErr then flushOut os else ())
      handle exn as IO.ClosedStream => raiseIo function name exn


    (* Body *)

    fun input1 ({ic, name} : instream) : char option =
      let val res = input1_ ic
      in if res < 0 then NONE
	 else SOME (chr_unsafe res)
      end

    fun input ({ic, name} : instream) : string = inputN_ (ic, 64)

    fun inputN ({ic, name} : instream, n) =
      if n < 0 orelse n > String.maxSize then raise Size
      else 
	if n <= 64 then inputN_ (ic, n)
	else
	  let 
	    fun loop(n,acc) =
	      if n <= 64 then concat(rev (inputN_(ic, n) :: acc))
	      else
		let val s = inputN_(ic, 64)
                    val sz = size s
		in if sz = 0 then concat(rev acc)
		   else loop(n-sz, s :: acc)
		end
	  in loop(n,nil)
	  end

    fun inputAll (is as {ic, name} : instream) : string =
      let fun loop (buffsz: int, acc: string list) : string =
	       let val s = inputN (is, buffsz)
	       in if size s < buffsz then concat(rev (s::acc))
		  else loop(2* buffsz, s::acc)
	       end
      in loop(64, [])
      end

    type cs = int (* character source state *)

    val failscan = Initial.failscan
    fun scanStream0 (scan, is as {ic, name} : instream) =
      let
        val buf  = alloc_string_ 512	(* characters recently read     *)
	val read = ref 0		(* number of characters read    *)
	fun getc charno =
	    if charno < !read then		(* already read         *)
		if charno >= !read - 512 then	(* still in buffer      *)
		    SOME(sub_ (buf, charno mod 512), charno+1)
		else				(* no longer in buffer  *)
		    raise failscan
	    else	        (* charno = !read; read a new character *)
	      case input1 is
		of SOME c => (update_ (buf, charno mod 512, c);
			      read := !read + 1;
			      SOME(c, charno+1))
		 | NONE => NONE 
      in case scan getc 0 
	   of NONE => NONE
	    | SOME(res, _) => SOME res
      end

    fun scanStream scan is = scanStream0(scan, is)
(*    
    fun inputNoBlock (is : instream) : vector option =
      raise Fail "not implemented"
*)

    fun lookahead ({ic, name} : instream) : char option =
      let val res = lookahead_ ic
      in if res < 0 then NONE
	 else SOME(chr_unsafe res)
      end

    fun endOfStream is = (lookahead is = NONE)

    fun inputLine(is) = 
       let fun loop(acc) =  
	         case input1 is
		   of SOME (c as #"\n") => implode(rev(c :: acc))
		    | SOME c => loop(c::acc)
		    | NONE => case acc 
				of [] => "" 
				 | _ => implode(rev(#"\n" :: acc)) 
       in loop([])
       end

    fun output(os, str) = output0(os, str, "output")

    fun outputSubstr (os, sus) =
      let val str = Substring.string sus
      in output0(os,str,"outputSubstr")
      end

    fun output1 (os, c) =
      let val str = String.str c
      in output0(os,str,"output1")
      end

    val print = print

  end
