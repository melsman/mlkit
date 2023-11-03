structure Crash: CRASH =
  struct
    exception CRASH

    fun println s = print (s ^ "\n")

    fun impossible msg =
      let
	val msg = "Impossible: " ^ msg
      in
	println msg;
	raise CRASH
      end

    fun assert (msg, condition) =
      if condition then ()
      else
	let
	  val msg = "Assert fails: " ^ msg
	in
	  println msg;
	  raise CRASH
	end

    fun unimplemented msg =
      let
	val msg = "Unimplemented: " ^ msg
      in
	println msg;
	raise CRASH
      end
  end
