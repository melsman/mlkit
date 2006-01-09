
structure Man = 
  struct
    val files = [("/etc/mlkit/map", "User configuration")]
    val header = String.concat [".TH mlkit 1 \"", 
                                Date.fmt "%B %d, %Y" (Date.fromTimeLocal (Time.now())),
                                "\" \"version ", Version.version, "\" ",
                                "\"MLKit for Standard ML\"\n"]
    val name = ".SH NAME\nmlkit \\- A fullblown Standard ML compiler\n"
    val defaults = String.concat [".SH DEFAULTS\n", 
                                  Flags.help_man_defaults ()
                                 ]
    val synopsis = String.concat [".SH SYNOPSIS\n"]
    val desciption = ".SH DESCRIPTION\n.B mlkit\nwill when invoked, compile\n"
    fun printExtra (long, short, desc) = 
          let val name = "--" ^ long ^  (String.concat (map (fn x => ", -" ^ x) short))
          in
            String.concat [".IP \"\\fB", name,
                           "\\fR\" 4\n",
                           ".IX Item \"", name, "\"\n",
                           desc, "\n"]
          end
    val extraOptions = List.map printExtra
                          [("version", ["v","V"], "print version.")]
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
                              "The following diagnostics may be ussued on stderr:\n"]
    val examples = String.concat [".SH EXAMPLES\n"]
    val standard = String.concat [".SH STANDARDS\n",
                                  "The MLKit implements the Standard ML of 1997 and has limited\
                                  \ support of the Standard Library of 2002.\n"]
    val credits = String.concat [".SH CREDITS\n"]
    val seealso = String.concat [".SH SEE ALSO\n"]
    fun main _ = 
      let
        val _ = print (String.concat [header,
                                      name,
                                      synopsis,
                                      defaults,
                                      desciption,
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
