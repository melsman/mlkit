(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi *)

structure ExportParseGen =
  struct
    fun err msg = output (std_err, msg)

    exception Interrupt;

    (* This function applies operation to ().  If it handles an interrupt
       signal (Control-C), it raises the exception Interrupt. Example:
       (handleInterrupt foo) handle Interrupt => print "Bang!\n" *)

    fun handleInterrupt (operation : unit -> unit) =
      let exception Done
          val old'handler = System.Signals.inqHandler(System.Signals.SIGINT)
          fun reset'handler () =
            System.Signals.setHandler(System.Signals.SIGINT, old'handler)
      in (callcc (fn k =>
             (System.Signals.setHandler(System.Signals.SIGINT,SOME(fn _ => k));
               operation ();
               raise Done));
           err ("\n--- Interrupt smlyacc ---\n");
           raise Interrupt)
          handle Done => (reset'handler ())
               | exn  => (reset'handler (); raise exn)
      end

    fun parseGen (argv, environment) =
      let fun parse_gen () =
            (case argv of
                (name :: file :: nil) => 
                  (ParseGen.parseGen file;
                   System.Unsafe.CInterface.exit 0)
              | (name :: _) =>
                  (err("Usage: sml-yacc filename\n");
                   System.Unsafe.CInterface.exit 1)
              |  _ => (err("? smlyacc: internal error!\n");
                       System.Unsafe.CInterface.exit 2))
      in
        (handleInterrupt parse_gen)
          handle
             Interrupt => System.Unsafe.CInterface.exit 3
           | (Io msg) =>
               (err ("? smlyacc: uncaught exception Io " ^ msg ^ "\n");
                System.Unsafe.CInterface.exit 4)
           | any =>
               (err ("? smlyacc: uncaught exception " ^
                     (System.exn_name any) ^ "\n");
                System.Unsafe.CInterface.exit 5)
      end
  end;
   
exportFn("sml-yacc",ExportParseGen.parseGen);
