structure Initial2 =
  struct
    type syserror = int

    exception SysErr of string * syserror Option.option
    val _ = prim ("sml_setFailNumber", (SysErr ("as",NONE) : exn, 2 : int)) : unit

    fun mkerrno_ (i : int) : syserror = prim("id", i)

    fun errno_ () : syserror = prim("sml_errno", ())

    fun formatErr mlOp (SOME operand) reason =
	mlOp ^ " failed on `" ^ operand ^ "': " ^ reason
      | formatErr mlOp NONE reason =
	mlOp ^ " failed: " ^ reason

    (* Raise SysErr from ML function *)
    fun raiseSysML mlOp operand reason =
	raise SysErr (formatErr mlOp operand reason, NONE)

    fun errorMsg (err : syserror) : string = prim("sml_errormsg", err)

    (* Raise SysErr with OS specific explanation if errno <> 0 *)
    fun raiseSys mlOp operand reason =
	let val errno = errno_ ()
	in if errno = 0 then raiseSysML mlOp operand reason
	   else raise SysErr
		      (formatErr mlOp operand (errorMsg errno),
		       SOME (mkerrno_ errno))
	end

    local
      fun app f [] = ()
        | app f (x::xs) = (f x ; app f xs)
    in
      fun exitCallback ( i : int) : int =
            if !Initial.exitCalled
            then 1+i
            else
              (Initial.exitCalled := true
              ; app (fn f => (f ()) handle _ => ()) (!Initial.exittasks)
              ; 0+i)
    end

    val _ = _export ("sml_exitCallback", exitCallback)

  end
