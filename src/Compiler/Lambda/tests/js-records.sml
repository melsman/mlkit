val () = Flags.turn_off "values_64bit"
val (js,_) = ExpToJs.toJs(ExpToJs.Env.initial,RecordFixture.make true)
val () = case CommandLine.arguments() of
             [file] => ExpToJs.toFile(file,js)
           | _ => raise Fail "usage: records-js output.js"
