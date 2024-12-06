(* Functionality for reading environment variables from ordinary environment
   variables or by lazily reading mlb-path-map files, stored in predefined
   locations.
*)

functor Environment(val program_name : unit -> string) :> ENVIRONMENT =
  struct
    datatype State = Out | In | Dollar | Esc of State
    exception ParseErr of string
    structure SM = StringFinMap
    val varMap = ref NONE : string SM.map option ref

    fun lookup m l =
        let val n = String.implode(List.rev l)
        in List.rev(String.explode(
                       case SM.lookup m n of
                           NONE => (case OS.Process.getEnv n of
                                        NONE => raise ParseErr (n ^ " not defined")
                                      | SOME v => v)
                         | SOME v => v))
        end

    fun escapeChars #"\\" = #"\\"
      | escapeChars #"$" = #"$"
      | escapeChars #"n" = #"\n"
      | escapeChars #"r" = #"\r"
      | escapeChars #"t" = #"\t"
      | escapeChars c = raise ParseErr ("\\" ^ (Char.toString c) ^ " not an escape character")

    fun myread m x =
        let fun myread (#"$",(Out,[],acc)) = (Dollar,[],acc)
              | myread (#"\\",(Out,[],acc)) = (Esc Out,[],acc)
              | myread (c,(Out,[],acc)) = (Out,[],(c::acc))
              | myread (#"(",(Dollar,[],acc)) = (In,[],acc)
              | myread (#")",(In,k,acc)) = (Out,[],lookup m k @ acc)
              | myread (#"\\",(In,k,acc)) = ((Esc In),k,acc)
              | myread (c,(In,k,acc)) = (In,(c::k),acc)
              | myread (c,((Esc In),k,acc)) = (In,(escapeChars c :: k),acc)
              | myread (c,((Esc Out),k,acc)) = (Out,(escapeChars c :: k),acc)
              | myread _ = raise ParseErr "Invalid character sequence"
        in CharVector.fromList (List.rev (#3 (CharVector.foldl myread (Out,[],[]) x)))
        end

    fun addFileLine (l,m) = (* raises Option.Option for empty lines! *)
        let val tokens = String.tokens Char.isSpace l
        in case tokens of
               [] => m
             | [k,v] => SM.add(k,myread m v,m)
             | _ => raise ParseErr ("Bad sequence of tokens in definition of: " ^
                                    (List.nth(tokens,0)))
        end

    exception FileNotFound

    fun addFile (f,m) =
        let val fh = (TextIO.openIn f) handle IO.Io _ => raise FileNotFound
            fun close () = TextIO.closeIn fh
            fun loop lc m =
                (case (TextIO.inputLine fh) of
                     SOME s => loop (lc+1) (addFileLine (s,m))
                   | NONE => (close(); m))
                handle ParseErr s =>
                       ( (close ()) handle _ => ()
                       ; raise ParseErr ("On line " ^ (Int.toString lc) ^ ":" ^ s)
                       )
                     | IO.Io {cause,...} => (close (); raise ParseErr (exnMessage cause))
                     | Exn => (close (); raise Exn)
        in loop 0 m
        end
        handle FileNotFound => m
             | ParseErr s => raise ParseErr ("In file " ^ f ^ ": " ^ s)

    fun getMap () =
        case !varMap of
            NONE => let val sysmpm : string =
                            Configuration.etcdir ^ "/" ^ (program_name()) ^ "/mlb-path-map"
                        val usermpm : string option =
                            Option.map (fn x=> x ^ "/." ^ (program_name()) ^ "/mlb-path-map")
                                       (OS.Process.getEnv "HOME")
                        val optmpms : string list = Flags.get_stringlist_entry "mlb_path_maps"
                        val mpms = sysmpm :: (case usermpm of SOME mpm => mpm :: optmpms
                                                            | NONE => optmpms)
                        val m = List.foldl addFile SM.empty mpms
                    in varMap := SOME m
                     ; m
                    end
          | SOME m => m

    fun error (s : string) =
        (print ("\nError: " ^ s ^ ".\n\n"); raise Fail "Environment.error")

    fun getEnvVal key =
        (case OS.Process.getEnv key of
             NONE => SM.lookup (getMap()) key
           | SOME v => SOME v)
        handle ParseErr s => error s
end
