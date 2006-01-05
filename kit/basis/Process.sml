(* Process *)

structure Process : OS_PROCESS =
  struct
    type status = int

    val success : status = 0
    val failure : status = ~1

  (* Make sure this function is in sync with Unix.fromStatus = Unix.W_EXITED *)
    fun isSuccess 0 = true
      | isSuccess _ = false

    local 
      val exn = Fail "Process"
      fun system_ (s: string) : status = prim("sml_system", (s,exn))
      fun getenv_ (s: string) : string = prim("sml_getenv", (s, exn))
    in 
      fun system s = system_ s handle _ => failure  
      fun getEnv s = (SOME (getenv_ s)) handle _ => NONE
    end

    fun terminate (s:status) : 'a = prim("terminateML", s)

    local 
      val exittasks = Initial.exittasks
      val exitCalled = Initial.exitCalled
      val exp = Initial.RaisedInExit
    in 
      fun atExit newtask = if !exitCalled then () else exittasks := newtask :: !exittasks

      fun exit status =
        if (!exitCalled) then raise exp else 
        (exitCalled := true ; 
         List.app (fn f => (f ()) handle _ => ()) (!exittasks);
         exittasks := [] ; 
         terminate status)
    end

    fun sleep t = 
            let
              val s = Int.fromLarge(Time.toSeconds t)
              val m = Int.fromLarge(Time.toMicroseconds(Time.-(t, Time.fromSeconds (Int.toLarge s))))
            in
              if (s < 0 orelse m < 0) then () else 
              (prim("sml_microsleep", (s : int, m : int)) : (int * int * int) ; ())
            end
  end

