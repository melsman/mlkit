structure ProfileMain =
struct
  fun run args =
      let val file = ref "profile.rp"
          val haveFile = ref false
          val output = ref "profile.html"
          fun help () =
              (print "Usage: rpview [profile.rp] [--output profile.html]\n";
               OS.Process.exit OS.Process.success)
          fun options [] = ()
            | options ("--output"::path::rest) = (output := path; options rest)
            | options ("-o"::path::rest) = (output := path; options rest)
            | options ("--help"::_) = help ()
            | options (value::rest) =
              if String.isPrefix "-" value orelse !haveFile then raise Fail ("unexpected argument: " ^ value)
              else (file := value; haveFile := true; options rest)
          val () = options args
          val inputId = OS.FileSys.fileId (!file)
          val same = (inputId = OS.FileSys.fileId (!output)) handle OS.SysErr _ => false
          val () = if same then raise Fail "input and output must be different files" else ()
          val samples = ProfileReader.read (!file)
          val page = ProfilePage.html samples
          val out = TextIO.openOut (!output)
          val () = (TextIO.output(out,page) handle e => (TextIO.closeOut out; raise e))
      in TextIO.closeOut out; print(!output ^ "\n") end
end
fun profileError message =
    (TextIO.output(TextIO.stdErr,"rpview: " ^ message ^ "\n"); OS.Process.exit OS.Process.failure)
val () = (ProfileMain.run (CommandLine.arguments ())
          handle Fail message => profileError message
               | e => profileError (General.exnMessage e))
