(* Compile an msp file into an sml-file *)

signature MSP_COMP =
  sig
    val msp_comp : string -> string
  end

functor MspComp (val error : string -> 'a
		 val pr : string
		 val pre : string) : MSP_COMP =
  struct

    fun getc (nil) = NONE
      | getc (c::cs) = SOME(c,cs)

    fun putc (c, cs) = c::cs

    fun tr #"\n" = "\\n\\\n\\ "
      | tr c = Char.toString c

    fun out_html (html, os) = 
      TextIO.output(os, "val _ = " ^ pr ^ " \"" ^ String.translate tr html ^ "\"\n")

    fun out_msp_string(msp, os) =
      TextIO.output(os, "val _ = " ^ pr ^ " ( " ^ msp ^ " )\n")

    fun out_msp_wseq(msp, os) =
      TextIO.output(os, "val _ = " ^ pr ^ " (Msp.flatten ( " ^ msp ^ " ))\n")

    fun out_msp_int(msp, os) =
      TextIO.output(os, "val _ = " ^ pr ^ " (Int.toString ( " ^ msp ^ " ))\n")

    fun out_msp_dec(msp, os) = 
      (TextIO.output(os, msp); TextIO.output(os, "\n"))

    fun read_chars (is, nil) = SOME(is)
      | read_chars (is, c::cs) = case getc is
				   of SOME (c', is) => 
				     if c = c' then read_chars (is, cs)
				     else NONE
				    | NONE => NONE
				    
    fun read_str (is, str) = 
      case read_chars (is, explode str)
	of SOME is => SOME is
	 | NONE => NONE

    fun read_html (is, acc, os) : unit = 
      case read_str (is, "<?MSP")
	of SOME is => (out_html (implode (rev acc), os);
		       case getc is
			 of SOME (c,is) =>
			   let val (out_msp, is) = case c
						     of #"=" => (out_msp_string, is)
						      | #"#" => (out_msp_int, is)
						      | #"$" => (out_msp_wseq, is)
						      | _ => (out_msp_dec, putc(c,is))
			   in read_msp(is, nil, out_msp, os)
			   end
			  | NONE => error (" While compiling msp-file into sml-file:\n" ^
					   " Expecting '?>' - but reached end of file"))
	 | NONE =>
	  (case getc is
	     of SOME (c,is) => read_html (is, c::acc, os)
	      | NONE => out_html (implode (rev acc), os))

    and read_msp (is, acc, out_msp, os) : unit =
      case read_str (is, "?>")
	of SOME is => (out_msp (implode (rev acc), os);
		       read_html (is, nil, os))
	 | NONE => 
	  (case getc is
	     of SOME (c,is) => read_msp (is, c::acc, out_msp, os)
	      | NONE => error (" While compiling msp-file into sml-file:\n" ^
			       " Expecting '?>' - but reached end of file"))

    fun fromFile (filename:string) : char list =
      let val is = TextIO.openIn filename 
	val s = TextIO.inputAll is handle E => (TextIO.closeIn is; raise E)
      in TextIO.closeIn is; explode s
      end

    fun msp_comp (msp_file : string) : string =
      let val sml_file = (*OS.Path.base*) msp_file ^ ".sml"
	val os = TextIO.openOut(sml_file)
	  handle _ => error ("Msp-compilation: failed to open file `" ^ sml_file ^ "' for writing")
	val is = fromFile msp_file 
	  handle _ => (TextIO.closeOut os;
		       error ("Msp-compilation: failed to open file `" ^ msp_file ^ "' for reading"))
      in TextIO.output(os, pre ^ "\n\n");
	read_html (is, nil, os) handle E => (TextIO.closeOut os; raise E); 
	TextIO.closeOut os; sml_file
      end 
  end
