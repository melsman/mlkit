(* A simple I/O library *)

structure SimpleIO = struct
	type instream = {ic: int, name: string}

	exception CannotOpen

	fun getCtx () : foreignptr = prim("__get_ctx",())

	fun chr_unsafe (i:int):char = prim ("id", i)

	fun input1_ (ic:int) : int = prim ("input1Stream", ic)

	fun input1 ({ic, name} : instream) : char option =
		let val res = input1_ ic
		in if res < 0 then NONE
		   else SOME (chr_unsafe res)
		end

	fun openIn (f: string) : instream =
		{ic=prim("openInStream",(getCtx(), f, CannotOpen)),
		 name=f} handle exn => raise Fail "openIn"

	fun closeIn ({ic,...} : instream) : unit = prim ("closeStream", ic)

	fun inputLine is =
		let fun loop(acc) =
			case input1 is
			  of SOME (c as #"\n") => SOME(implode(rev(c :: acc)))
			| SOME c => loop(c::acc)
			| NONE => case acc
						of [] => NONE
					  | _ => SOME(implode(rev(#"\n" :: acc)))
		in loop([])
		end
end
