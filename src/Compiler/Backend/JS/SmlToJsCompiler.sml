(* Loaded after the IDE. Keep compiler dependencies out of the UI entry point. *)
structure SmlToJsCompiler = struct
  open SmlToJsComp

  fun timeit f x =
      let val rt = Timer.startRealTimer()
        val res = f x
        val t = Timer.checkRealTimer rt
      in (res,t)
      end

  val basislibs = ["Initial","General","Option", "List", "ListPair",
                   "Vector", "VectorSlice", "Array", "ArraySlice", "Array2", "ByteTable", "ByteSlice",
                   "StringCvt", "String2", "Substring", "Text", "Bool", "IntInfRep",
                   "Word32", "Word8", "Word31", "Pack32Little", "Pack32Big", "Byte",
                   "Int32", "Int31",
                   "Math", "Real",
                   "IntInf",
                   "Time", "Random", "Path", "Date", "Timer", "TextIO",
                   "JsCore", "Js", "Html", "Rwp", "XMLrpcClient", "dojo", "formlets", "utest"
                  ]

  val envRef : Env.t option ref = ref NONE

  fun exnMsg (e:exn) : string = prim("execStmtJS", ("return e.toString()","e",e))

  fun compute f inputstring =
      if not (Option.isSome (!envRef)) then print "[Compiler is still loading. Please try again shortly.]\n"
      else
      let
        fun load_env_all() =
            case !envRef of
              SOME e => e
            | NONE => raise Fail "impossible: load_env_all"
        val timing = true
        val () = print ("[Compiling file " ^ f ^ "]\n")
        fun printtime s t =
            if timing then print ("[" ^ s ^ " time: " ^ Time.toString t ^ "]\n")
            else ()
        val e = load_env_all()
        val ((e',mc),compiletime) = timeit compile (e,inputstring)
        val _ = printtime "Compile" compiletime
      in
        let val () = print "[Executing]\n"
            val ((),exectime) = timeit execute mc
        in print "\n";
           printtime "Execution" exectime
        end handle ? => print ("Uncaught exception " ^ General.exnName ? ^ "\n")
      end

  fun initialize {out : string -> unit, ready : unit -> unit, failed : string -> unit} =
      let
        infix ++
        fun e ++ e' = Env.plus (e,e')
        fun load_env n =
            let val () = out "."
                val eb_s = JsCore.exec0{stmt="return " ^ n ^ "_sml_eb;",res=JsCore.string}()
            (* val () = out ("Unpickling " ^ n ^ "\n") *)
            in Pickle.unpickle Env.pu eb_s
            end handle ? => (out ("load_env problem: " ^ exnMsg ? ^ "\n"); raise ?)

        fun load_envs e nil =
            (envRef := SOME e;
             JsCore.exec0
                 {stmt="if (window.performance && performance.mark && performance.measure) { \
                       \performance.mark('smltojs-compiler-ready'); \
                       \performance.measure('smltojs-startup', {start: 0, end: 'smltojs-compiler-ready'}); \
                       \performance.measure('smltojs-basis-init', 'smltojs-basis-start', 'smltojs-compiler-ready'); }",
                  res=JsCore.unit} ();
             out " Done]\n";
             ready())
          | load_envs e (n::ns) =
            (Js.setTimeout 0 (fn () =>
                 load_envs (e ++ load_env n) ns
                 handle error => failed (exnMsg error)); ())
      in JsCore.exec0
             {stmt="if (window.performance && performance.mark) performance.mark('smltojs-basis-start');",
              res=JsCore.unit} ();
         out "[Loading Basis Library ";
         load_envs (Env.initial()) basislibs
      end


  val () = SmlToJsBridge.compute := compute
  val () = SmlToJsBridge.initialize := initialize
  val () = JsCore.exec0
      {stmt="window.smltojsLoading.compilerLoaded = true;",res=JsCore.unit} ()
end
