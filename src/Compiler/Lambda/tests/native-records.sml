val () = List.app Flags.turn_off ["parallelism","tag_pairs"]
val () = case CommandLine.arguments() of
             ["plain",_] => List.app Flags.turn_off ["tag_values","garbage_collection","generational_garbage_collection"]
           | ["gc",_] => (Flags.turn_off "generational_garbage_collection";
                          List.app Flags.turn_on ["garbage_collection","tag_values"])
           | ["gengc",_] => List.app Flags.turn_on ["tag_values","garbage_collection","generational_garbage_collection"]
           | _ => raise Fail "usage: native-records {plain|gc|gengc} output-prefix"
val () = Flags.turn_off "parallelism"
val prefix = List.nth(CommandLine.arguments(),1)
val () = Compile.preHook()
val (_,lowered) = Compile.lower(CompBasis.initial,RecordFixture.make false)
val (_,code) = N.compile(ClosExp.init,lowered,false)
val () = G.emit(G.CG code,prefix ^ ".s")
val () = G.emit(G.generate_link_code([#main_lab code],([],[])),prefix ^ "-link.s")
