structure Environment :> ENVIRONMENT =
  struct
  local
    datatype State = Out | In | Dollar | Esc of State
    exception ParseErr of string
    structure BM = Binarymap
    val varMap = ref NONE : (string,string) BM.dict option ref

    fun lookup l =(let val n = String.implode(List.rev l)
                   in
                     List.rev(String.explode(
                      case OS.Process.getEnv n
                      of NONE => ((BM.find (Option.valOf(!varMap), n))
                                 handle BM.NotFound => raise ParseErr (n ^ " not defined"))
                       | SOME v => v))
                   end)

    fun myread Out [] [] acc = String.implode(List.rev acc)
      | myread Out (#"$"::cc) [] acc = myread Dollar cc [] acc
      | myread Out (#"\\"::cc) [] acc = myread (Esc Out) cc [] acc
      | myread Out (c::cc) [] acc = myread Out cc [] (c::acc)
      | myread Dollar (#"("::cc) [] acc = myread In cc [] acc
      | myread In (#")"::cc) k acc = myread Out cc [] ((lookup k) @ acc)
      | myread In (#"\\"::cc) k acc = myread (Esc In) cc k acc
      | myread In (c::cc) k acc = myread In cc (c::k) acc
      | myread (Esc In) (c::cc) k acc = (case c of #"$" => myread In cc (c :: k) acc
                                                 | #"\\" => myread In cc (c :: k) acc
                                                 | _ => raise ParseErr ("\\" ^ (Char.toString c) ^
                                                                         " not an escape character"))
      | myread (Esc Out) (c::cc) k acc = (case c of #"$" => myread Out cc (c :: k) acc
                                                  | #"\\" => myread Out cc (c :: k) acc
                                                  | _ => raise ParseErr ("\\" ^ (Char.toString c) ^
                                                                         " not an escape character"))
      | myread _ _ _ _ = raise ParseErr "Invalid character sequence"

    fun toMap l =
          let
            val tokens = String.tokens Char.isSpace l
            val line = case tokens
                       of [] => NONE
                        | [lv,def] => SOME(lv,def)
                        | _ => 
                           raise ParseErr ("Bad sequence of tokens in definition of: " ^
                                           (List.nth(tokens,0)))
            val (lv,def) = Option.valOf line
            val def' = myread Out (String.explode def) [] []
          in varMap := SOME(BM.insert(Option.valOf (!varMap), lv, def'))
          end

    exception FileNotFound

    fun readfile f =
          let
            val fh =  (TextIO.openIn f) handle IO.Io _ => raise FileNotFound
            fun close () = TextIO.closeIn fh 
            fun loop lc = (case (TextIO.inputLine fh)
                           of SOME s => (toMap s;loop (lc + 1))
                            | NONE => close())
                         handle ParseErr s => ((close ()) handle _ => () ;
                           raise ParseErr ("On line " ^ (Int.toString lc) ^ ":" ^ s))
                              | IO.Io {cause,...} => (close (); raise ParseErr (exnMessage cause))
                              | Option.Option => loop(lc+1)
                              | Exn => (close (); raise Exn)
          in loop 0
          end
    fun fillMap () = 
          case !varMap
          of NONE => 
              let
                val _ = varMap := SOME(BM.mkDict String.compare)
                val user = Option.map (fn x=> x^"/.mlkit/mlb-path-map") (OS.Process.getEnv "HOME")
                val system = SOME(Conf.etcdir ^ "/mlkit/mlb-path-map")
                val files = [system,user]
              in List.app (fn x => (case x
                                    of NONE => ()
                                     | SOME x' => (readfile x')
                                                handle FileNotFound => ()
                                                     | ParseErr s => 
                                                         raise ParseErr ("In file: " ^ x' ^ " " ^ s)
                                                     | ? => raise ?)) files
              end
           | SOME _ => ()

    fun error (s : string) = 
        (print ("\nError: " ^ s ^ ".\n\n"); raise Fail "MlbProject.error")

  in
      fun getEnvVal key = (case OS.Process.getEnv(key)
                      of NONE => (fillMap (); Option.join (Option.map (fn v => BM.peek(v, key))
                                                                      (!varMap)))
                       | SOME v => SOME v) handle ParseErr s => error s 
       (* Put in handler for exceptions: ParseErr, Io, Option.Option *)
  end
end

