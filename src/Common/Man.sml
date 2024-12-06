
structure Man :
    sig
	val gen : {cmd:unit->string,
		   date:string,
		   extraOptions: (string * string list * string list)list,
		   version:string}
	    -> string
    end =
struct

  fun isSMLtoJs exe : bool =
      String.isSubstring "smltojs" exe

  fun isReML exe : bool =
      String.isSubstring "reml" exe

  val homepage = "http://melsman.github.io/mlkit"
  val homepage_smltojs = "http://www.smlserver.org/smltojs"

    fun concatWith2 (s1,s2) nil = ""
      | concatWith2 (s1,s2) [x] = x
      | concatWith2 (s1,s2) [x1,x2] = x1 ^ s2 ^ x2
      | concatWith2 (s1,s2) l =
	let fun loop [x,y] = x ^ s1 ^ s2 ^ y      (* ", " ^ " and " *)
	      | loop (x::xs) = x ^ s1 ^ loop xs
              | loop _ = raise Fail "concatWith2.impossible"
	in loop l
	end

    fun addBetween (_ : 'a) ([] : 'a list) = [] : 'a list
      | addBetween _ (x::[]) = [x]
      | addBetween s (x::y::zz) = x:: s :: (addBetween s (y :: zz))

    fun printDefs () =
      let
        val formatDefaults =
	    List.mapPartial (fn ({default,long,short,...} : Flags.options)
                                => Option.map (fn d => case List.getItem short
                                                        of NONE =>
                                                           Option.valOf(Option.map (fn (l,_) => {name = "--" ^ l, value = d}) (List.getItem long))
                                                         | SOME (s,_) => {name = "-" ^ s, value = d} )
                                              default)
        val opts = formatDefaults (Flags.getOptions ())
        fun printDef {name,value} = String.concat ["\\fB", name, "\\fR", " ", value]
      in
        String.concat (addBetween ",\n" (List.map printDef opts))
      end

    fun printOpts extra =
      let
        fun pLong (l,short,kind) = let
                               val kk = case kind of NONE => "" | SOME a => " " ^ a
                             in
                               String.concat (addBetween ", " ((List.map (fn x => "--" ^ x ^ kk) l) @ (List.map (fn x => "-" ^ x ^ kk) short)))
                             end
        fun printOps ({long,short,desc,kind,...} : Flags.options) =
               String.concat [".IP \"\\fB", pLong (long,short,kind), "\\fR\" 4\n",".IX Item \"", pLong (long,short,kind), "\"\n",
                              desc,"\n"]
        fun genExtra (l,s,d) = {long = [l], short = s, desc = String.concat d, kind = NONE, default = NONE, menu=nil}
        val extra' = List.map genExtra extra

        fun cmp c ([],[]) = EQUAL
          | cmp c ([],_) = LESS
          | cmp c (_,[]) = GREATER
          | cmp c (x::xs,y::ys) = case c (x,y)
                                  of EQUAL => cmp c (xs,ys)
                                   | GREATER => GREATER
                                   | LESS => LESS
        val sort = Listsort.sort (fn ({long = l1,...},{long = l2,...}) => cmp String.compare (l1,l2))
      in
        String.concat (List.map printOps (sort (Flags.getOptions () @ extra')))
      end

    structure Devel =
      struct

        val developers = ["Lars Birkedal", "Martin Elsman",
		          "Niels Hallenberg", "Tommy H. Olesen",
		          "Mads Tofte", "Carsten Varming"]

        val contributers = ["Peter Bertelsen",
			    "Vesa Karvonen",
			    "Ken Friis Larsen",
			    "Henning Niss",
			    "Peter Sestoft",
                            "Troels Henriksen"]

        val smltojs_developers = ["Martin Elsman"]
        val reml_developers = ["Martin Elsman"]
      end

    fun mkStr s = "\"" ^ s ^ "\""

    fun files exe =
	[(Configuration.etcdir ^ "/" ^ exe ^ "/mlb-path-map",
          "System-wide configuration of library and runtime system locations"),
	 ("~/." ^ exe ^ "/mlb-path-map",
          "User specific configuration of library and runtime system locations")]

    fun header exe date version =
	let val title =
                if isSMLtoJs exe then
                  mkStr "SMLtoJs - a Standard ML to JavaScript compiler"
		else mkStr "MLKit - a compiler for Standard ML"
	in
	    String.concat [".TH ", exe, " 1 \"", date, "\" \"version ",
			   version, "\" ",title,"\n"]
	end

    fun name exe =
	let val text =
		if isSMLtoJs exe then
                  "Standard ML to JavaScript compiler"
                else if isReML exe then
                  "Standard ML with Explicit Regions and Effects"
		else "A fullblown Standard ML compiler"
	in
	    ".SH NAME\n" ^ exe ^ " \\- " ^ text ^ " \n"
	end

    fun defaults () =
	String.concat [".SH DEFAULTS\n",
		       printDefs(),
		       ".\n"]

    fun synopsis exe =
	String.concat [".SH SYNOPSIS\n",
		       exe, " [OPTION]... [file.sml | file.sig | file.mlb]\n\n",
		       "All possible options are listed below.\n"]

    fun description exe =
	let val name =
                if isSMLtoJs exe then "SMLtoJs"
                else if isReML exe then "ReML"
                else "MLKit"
            val (result, homepage) =
		if isSMLtoJs exe
                then ("an HTML-file, containing references to generated JavaScript files, ", homepage_smltojs)
		else ("an executable file\n.B run\n", homepage)

            val what =
                if isReML exe then
                  "ReML is Standard ML with support for programming with explicit regions, \n\
                  \explicit effects, and effect constraints. With ReML, atomic effects \n\
                  \include get-effects, put-effects, and mut-effects. Whereas ReML include\n\
                  \parallel thread support, ReML does not support integration with \n\
                  \reference-tracing garbage collection."
                else ""
	in
	  String.concat[".SH DESCRIPTION\n",
                        what,
			"When invoked, \n.B ", exe, "\nwill compile the specified sources into ", result,
			"through a series of translation phases. The various options can be used to control the ",
			"printing of intermediate forms and to control which optimizations are performed. If source files ",
			"are organised in ML Basis Files (files with extension .mlb), the compiler will memoize symbol table ",
			"information and object code in the dedicated MLB directories located relative to the source files.\n\n",
                        if not(isSMLtoJs exe) then
                          "If " ^ exe ^ " is invoked without source files, an interactive REPL is started. Type " ^
                          "':help;' within the REPL to learn more about its use.\n\n"
                        else
                          "",
			"To learn more about programming with ", name, ", consult the ", if isReML exe then "MLKit" else name, " web page at\n\n",
			".B ", homepage, "\n"]
	end

    fun options extraOptions =
	String.concat [".SH OPTIONS\n", printOpts extraOptions]

    val exit =
	String.concat [".SH EXIT STATUS\nExits with status 0 on success and -1 on failure.\n"]

    fun environment exe =
	String.concat [".SH ENVIRONMENT\n",
		       "A library install directory must be provided ",
		       "in an environment variable SML_LIB, as a path-definition ",
		       "in the user's personal path-map ~/." ^ exe ^ "/mlb-path-map, ",
                       "or in a system wide path-map " ^ Configuration.etcdir ^ "/" ^ exe ^ "/mlb-path-map.\n"]

    val files = fn exe =>
        String.concat [".SH FILES\n",
		       String.concat (List.map (fn (f,e) => ".IP " ^ f ^ "\n" ^ e ^ "\n" ) (files exe))]

    fun examples exe =
        if isSMLtoJs exe then
          String.concat [".SH EXAMPLES\n",
                         "For examples, consult the SMLtoJs home page.\n"]
        else if isReML exe then
          String.concat [".SH EXAMPLES\n",
                         "For examples, consult the files in the 'test/reml' folder within \n\
                         \the github repository at http://github.com/melsman/mlkit.\n"]
        else
	  let val (name, title) =
		  ("MLKit", "MLKit manual \"Programming with Regions in the MLKit\"")
	  in
	    String.concat [".SH EXAMPLES\n",
			   "For examples, consult the ", title,
			   ", which is available from the ", name, " home page.\n"]
	  end

    fun standard exe =
	let
          val based_on_mlkit_maybe =
	      if isSMLtoJs exe then
                "SMLtoJs is based on the MLKit. "
	      else if isReML exe then
                "ReML is based on the MLKit. "
              else ""
          val maybe_all_basis =
              if isSMLtoJs exe then ["\n"]
              else ["See the MLKit home page ",
		    "for a detailed overview of the support for the Standard ML Basis Library.\n"]

	in
	  String.concat ([".SH STANDARDS\n", based_on_mlkit_maybe,
			  "The MLKit implements Standard ML (Revised 1997) and has almost full ",
			  "support for the Standard ML Basis Library (version of 2002). "] @ maybe_all_basis)
	end

    fun credits exe =
	let val c =
              if isSMLtoJs exe then
                ["SMLtoJs was developed by " ^ concatWith2 (", ", " and ") Devel.smltojs_developers ^ ". ",
                 "Many people have helped developing the MLKit on which SMLtoJs is built; see the MLKit home page for details."]
              else if isReML exe then
                ["ReML was developed by " ^ concatWith2 (", ", " and ") Devel.reml_developers ^ ". ",
                 "Many people have helped developing the MLKit on which ReML is built; see the MLKit home page for details."]
              else
                ["The MLKit (version 2 and beyond) was developed by ",
	         concatWith2 (", "," and ") Devel.developers,
	         ". People who have contributed with bug-fixes and improvements include ",
	         concatWith2 (", ", " and ") Devel.contributers,
	         ". Nick Rothwell and David N. Turner took part in the development of the MLKit version 1.\n"]
	in
	    String.concat ([".SH CREDITS\n"] @ c)
	end

    fun seealso exe =
        String.concat [".SH SEE ALSO\n",
			   "See the MLKit manual \"Programming with Regions in the MLKit\", available from the ",
			   "MLKit home page\n\n",
			   ".B ", homepage, "\n\nfor an in-depth introduction to programming with regions in the MLKit.\n\n",
			   "The home page also provides an overview of which parts of ",
			   "the Standard ML Basis Library the MLKit implements, along with download and installation instructions."]

    fun gen {cmd:unit->string,date:string,
	     extraOptions : (string * string list * string list) list,
	     version:string} : string =
      let val exe = cmd()
      in String.concat [header exe date version,
			name exe,
			synopsis exe,
			defaults(),
			description exe,
			standard exe,
			examples exe,
			options extraOptions,
			exit,
			environment exe,
			files exe,
(*			diag, *)
			credits exe,
			seealso exe
			]
      end
  end
