structure Environment :> ENVIRONMENT =
  struct
  local
    datatype State = Out | In | Dollar | Esc of State
    exception ParseErr of string
    structure BM = Binarymap
    val varMap = ref NONE : (string,string) BM.dict option ref

    fun lookup l =(let
                     val n = String.implode(List.rev l)
                   in
                     List.rev(String.explode(
                      case BM.peek (Option.valOf(!varMap), n)
                      of NONE => (case OS.Process.getEnv n
                                  of NONE => raise ParseErr (n ^ " not defined")
                                   | SOME v => v)
                       | SOME v => v))
                   end)

    fun escapeChars #"\\" = #"\\"
      | escapeChars #"$" = #"$"
      | escapeChars #"n" = #"\n"
      | escapeChars #"r" = #"\r"
      | escapeChars #"t" = #"\t"
      | escapeChars c = raise ParseErr ("\\" ^ (Char.toString c) ^ " not an escape character")

    fun myread x =
      let
        fun
        myread (#"$",(Out,[],acc)) = (Dollar,[],acc)
      | myread (#"\\",(Out,[],acc)) = (Esc Out,[],acc)
      | myread (c,(Out,[],acc)) = (Out,[],(c::acc))
      | myread (#"(",(Dollar,[],acc)) = (In,[],acc)
      | myread (#")",(In,k,acc)) = (Out,[],(lookup k) @ acc)
      | myread (#"\\",(In,k,acc)) = ((Esc In),k,acc)
      | myread (c,(In,k,acc)) = (In,(c::k),acc)
      | myread (c,((Esc In),k,acc)) = (In,(escapeChars c :: k),acc)
      | myread (c,((Esc Out),k,acc)) = (Out,(escapeChars c :: k),acc)
      | myread _ = raise ParseErr "Invalid character sequence"
      in CharVector.fromList (List.rev (#3 (CharVector.foldl myread (Out,[],[]) x)))
      end

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
            val def' = myread def
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
                val system = SOME(Configuration.etcdir ^ "/mlkit/mlb-path-map")
                val files = [system,user]
              in List.app (fn x => (case x
                                    of NONE => ()
                                     | SOME x' => (readfile x')
                                                handle FileNotFound => ()
                                                     | ParseErr s => 
                                                         raise ParseErr ("In file: " ^ x' ^ " " ^ s)
                                    )) files
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

