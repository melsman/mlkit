(* Build-time embedding keeps the installed viewer a single executable. *)
val () =
    case CommandLine.arguments () of
        [input,output] =>
        let val src = TextIO.openIn input
            val dst = TextIO.openOut output
            val () = TextIO.output(dst,"structure ProfileHtml = struct\nval template = String.concat [\n")
            fun loop first =
                case TextIO.inputLine src of
                    NONE => ()
                  | SOME line =>
                    (TextIO.output(dst,(if first then "" else ",\n") ^ "\"" ^ String.toString line ^ "\""); loop false)
        in loop true; TextIO.output(dst,"\n]\nend\n"); TextIO.closeIn src; TextIO.closeOut dst end
      | _ => raise Fail "embed input.html output.sml"
