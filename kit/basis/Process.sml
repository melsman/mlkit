(* Process *)

structure Process : OS_PROCESS =
  struct
    type status = int

    val success : status = 0
    val failure : status = ~1

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
    in 
      fun atExit newtask = exittasks := newtask :: !exittasks
      fun exit status =
	(List.app (fn f => f ()) (!exittasks);
	 terminate status)
    end
  end

