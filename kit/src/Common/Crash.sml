structure Crash: CRASH =
  struct
    exception CRASH

    fun impossible msg =
      let
	val msg = "Impossible: " ^ msg
      in
	BasicIO.println msg;
	raise CRASH
      end

    fun assert(msg, condition) =
      if condition then ()
      else
	let
	  val msg = "Assert fails: " ^ msg
	in
	  BasicIO.println msg;
	  raise CRASH
	end

    fun unimplemented msg =
      let
	val msg = "Unimplemented: " ^ msg
      in
	BasicIO.println msg;
	raise CRASH
      end
  end
