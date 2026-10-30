(* Loaded after the IDE. Keep compiler dependencies out of the UI entry point. *)
structure SmlToJsCompiler = struct
  open SmlToJsComp

  fun timeit f x =
      let val rt = Timer.startRealTimer()
        val res = f x
        val t = Timer.checkRealTimer rt
      in (res,t)
      end

  (* Load REAL before its structures: the signature introduces LargeReal.real. *)
  val basislibs = [
      "Initial.sml", "General.sml", "Option.sml", "List.sml", "ListPair.sml",
      "Vector.sml", "VectorSlice.sml", "Array.sml", "ArraySlice.sml", "Array2.sml",
      "ByteTable.sml", "ByteSlice.sml", "StringCvt.sml", "String2.sml", "Substring.sml",
      "Text.sml", "Bool.sml", "IntInfRep.sml", "Word32.sml", "Word8.sml",
      "Word31.sml", "Pack32Little.sml", "Pack32Big.sml", "Byte.sml", "Int32.sml",
      "Int31.sml", "Math.sml", "IntInf.sml", "IEEEReal.sml", "REAL.sig",
      "Real.sml", "Math16.sml", "Real16.sml", "Math32.sml", "Real32.sml",
      "narrow-real-default.sml", "real64tables.sml", "realtables.sml", "PackReal16.sml", "PackReal32.sml",
      "MONO_VECTOR.sml", "MONO_ARRAY.sml", "MONO_VECTOR_SLICE.sml", "MONO_ARRAY_SLICE.sml", "MONO_ARRAY2.sml",
      "PACK_REAL.sml", "Time.sml", "Random.sml", "Path.sml", "Date.sml",
      "Timer.sml", "TextIO.sml", "JsCore.sml", "Js.sml", "Html.sml",
      "Rwp.sml", "XMLrpcClient.sml", "dojo.sml", "formlets.sml", "utest.sml",
      "MATH.sig", "IEEE_REAL.sig"
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
                val eb_s = JsCore.exec1{stmt="return window[n + '_eb'];",
                                        arg1=("n",JsCore.string),res=JsCore.string}
                    (String.translate (fn #"." => "_" | c => String.str c) n)
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
