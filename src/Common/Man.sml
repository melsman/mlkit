
structure Man = 
  struct
      val exe = "mlkit"
      fun concatWith2 (s1,s2) l =
	  let fun loop nil = ""
		| loop [x,y] = x ^ s2 ^ y
		| loop (x::xs) = x ^ s1 ^ loop xs
	  in loop l
	  end
      val developers = ["Lars Birkedal",
			"Martin Elsman",
			"Niels Hallenberg",
			"Tommy H. Olesen",
			"Mads Tofte",
			"Carsten Varming"]
      val contributers = ["Peter Bertelsen",
			  "Ken Friis Larsen", 
			  "Henning Niss",
			  "Peter Sestoft"]
    val files = [("/etc/mlkit/map", "User configuration")]
    val header = String.concat [".TH ", exe, " 1 \"", 
                                Date.fmt "%B %d, %Y" (Date.fromTimeLocal (Time.now())),
                                "\" \"version ", Version.version, "\" ",
                                "\"MLKit - a compiler for Standard ML\"\n"]
    val name = ".SH NAME\n" ^ exe ^ " \\- A fullblown Standard ML compiler\n"
    val defaults = String.concat [".SH DEFAULTS\n", 
                                  Flags.help_man_defaults ()
                                 ]
    val synopsis = String.concat [".SH SYNOPSIS\n",
				  exe, " [OPTION]... [file.sml | file.sig | file.mlb]\n\n",
				  "All possible options are listed below.\n"]
    val description = String.concat[".SH DESCRIPTION\nWhen invoked, \n.B ", exe, "\nwill compile the specified sources into an execuatable file ",
				    "\n.B run\nthrough a series of translation phases. Various options (see below) can be used to control the ",
				    "printing of intermediate forms and to which degree various optimizations are performed. If source files ",
				    "are organised in ML Basis Files (files with extension .mlb), the compiler will memoize symbol table ",
				    "information and object code in the dedicated MLB directories located together with the source files, so ",
				    "as to minimize necessary recompilation upon changes of source code.\n\n",
				    "To learn more about programming with the MLKit, consult the MLKit web page at\n\n",
				    ".B http://www.itu.dk/research/mlkit\n"]

    fun printExtra (long, short, desc) = 
          let val name = "--" ^ long ^  (String.concat (map (fn x => ", -" ^ x) short))
          in
            String.concat [".IP \"\\fB", name,
                           "\\fR\" 4\n",
                           ".IX Item \"", name, "\"\n",
                           String.concat desc, "\n"]
          end
    val extraOptions = List.map printExtra Main.extraOptions
    val options = String.concat [".SH OPTIONS\n", 
                                 String.concat extraOptions,
                                 Flags.help_man_option ()]
    val exit = String.concat [".SH EXIT STATUS\n"]
    val environment = String.concat [".SH ENVIRONMENT\n"]
    val files = String.concat [".SH FILES\n",
                               String.concat
                                 (List.map (fn (f,e) => ".I " ^ f ^ "\n.RS\n" ^ e ^ "\n" ) files)
                              ]
    val diag = String.concat [".SH DIAGNOSTICS\n",
                              "The following diagnostics may be issued on stderr:\n"]
    val examples = String.concat [".SH EXAMPLES\n"]
    val standard = String.concat [".SH STANDARDS\n",
                                  "The MLKit implements Standard ML (Revised 1997) and has almost full\
                                  \ support for the Standard ML Basis Library (version of 2002).\n"]
    val credits = String.concat [".SH CREDITS\nThe MLKit (version 2 and beyond) has been developed by ",
				 concatWith2 (", ",", and ") developers,
				 ". People who have contributed with bug-fixes and improvements include ",
				 concatWith2 (", ", ", and ") contributers,
				 ". Nick Rothwell and David N. Turner took part in the development of the MLKit version 1."]
    val seealso = String.concat [".SH SEE ALSO\n"]
    fun main _ = 
      let
        val _ = print (String.concat [header,
                                      name,
                                      synopsis,
                                      defaults,
                                      description,
                                      options,
                                      exit,
                                      environment,
                                      files,
                                      diag,
                                      examples,
                                      standard,
                                      credits,
                                      seealso
                                      ])
      in OS.Process.success
      end
  end
