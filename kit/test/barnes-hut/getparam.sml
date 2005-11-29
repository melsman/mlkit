(* getparam.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *)

structure GetParam : sig

    exception EOF

    val initParam : (string list * string list) -> unit
    val getParam : string -> string
    val getIParam : string -> int
    val getRParam : string -> real
    val getBParam : string -> bool

  end = struct

    exception EOF

    val defaults = ref ([] : string list)

  (* ignore arg vector, remember defaults. *)
    fun initParam (argv, defl) = defaults := defl

    fun prompt items = (
	  TextIO.output(TextIO.stdOut, String.concat items);
	  TextIO.flushOut TextIO.stdOut)

    structure SS = Substring

  (* export version prompts user for value. *)
    fun getParam name = let
	  fun scanBind [] = NONE
	    | scanBind (s::r) = let
		val (_, suffix) = SS.position name (SS.full s)
		in
		  if (SS.isEmpty suffix)
		    then scanBind r
		    else SOME(SS.string(SS.triml (size name+1) suffix))
		end
	  fun get default = (case (TextIO.inputLine TextIO.stdIn)
		 of NONE => raise EOF
		  | SOME "\n" => default
		  | SOME s => substring(s, 0, size s - 1)
		(* end case *))
	  in
	    if (null (! defaults))
	      then raise Fail "getParam called before initParam"
	      else ();
	    case (scanBind (! defaults))
	     of (SOME s) => (
		  prompt ["enter ", name, " [", s, "]: "];
		  get s)
	      | NONE => (prompt ["enter ", name, ": "]; get "")
	    (* end case *)
	  end

    local
      fun cvt scanFn = let
	    fun cvt' name = let
		  fun get () = (case getParam name of "" => get () | s => s)
		  val param = get ()
		  in
		    (valOf (scanFn param)) handle _ => (cvt' name)
		  end
	    in
	      cvt'
	    end
    in
  (* get integer parameter *)
    val getIParam = cvt Int.fromString
  (* get real parameter *)
    val getRParam = cvt Real.fromString
  (* get bool parameter *)
    val getBParam = cvt Bool.fromString
    end (* local *)

  end; (* GetParam *)
