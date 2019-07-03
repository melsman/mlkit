
signature COMPILER_TIMINGS =
  sig
    exception ReadTimingsError of string
    val from_file : string -> (string * Time.time) list
    val pr : (string * Time.time) list -> unit
  end

structure CompilerTimings : COMPILER_TIMINGS =
  struct

    fun pr [] = ()
      | pr ((n,t)::rest) = (print (n ^ ": " ^ Time.toString t ^ "\n"); pr rest)

    exception ReadTimingsError of string

    fun scanChars getc src =
      let val src = StringCvt.skipWS getc src
      in case StringCvt.splitl (not o Char.isSpace) getc src
 	   of ("",src) => NONE
	    | res => SOME res
      end

    fun read_entry is : (string * Time.time) option =
      case TextIO.scanStream scanChars is
	of SOME name =>
	  ((case TextIO.scanStream Time.scan is
	      of SOME t => SOME(name,t)
	       | NONE => raise ReadTimingsError "time expected")
           handle Time.Time => raise ReadTimingsError "Time raised when trying to read time value")
	 | NONE => NONE

    fun read is = case read_entry is
		    of SOME e => e :: read is
		     | NONE => nil


    (* Compressing timings *)

    fun compress (ss : (string * Time.time) list) : (string * Time.time) list =
      let
	fun unref [] = []
	  | unref ((n,ref t)::rest) = (n,t):: unref rest

	fun insert ((n,t),nts) =
	  let
	    fun loop [] = nts@[(n,ref t)]
	      | loop ((n', r as ref t')::rest) =
	      if n = n' then (r:= Time.+(t,t'); nts)
	      else loop rest
	  in loop nts
	  end

	fun loop ([],nts) = unref nts
	  | loop (e::rest,nts) = loop(rest,insert(e,nts))

      in loop (ss,[])
      end


    fun from_file s : (string * Time.time) list =
      let val is = (TextIO.openIn s)
	    handle _ => raise ReadTimingsError ("failed to open compiler timings file `" ^ s ^ "'.\n")
      in let val ss = (read is)
	       handle ReadTimingsError s =>
		 (TextIO.closeIn is;
		  raise ReadTimingsError ("error when trying to read compiler timings: " ^ s ^ ".\n"))
	     val ss = compress ss
	 in TextIO.closeIn is;
	   ss
	 end
      end

  end
