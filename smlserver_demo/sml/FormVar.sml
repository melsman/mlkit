signature FORMVAR =
  sig
    val getNat : string -> int option
    val getInt : string -> int option
    val getReal : string -> real option
    val getNum : string -> real option
    val getString : string -> string option
  end
  
structure FormVar : FORMVAR =
  struct
    fun getNat (s : string) : int option =
      case Ns.Conn.formvar s
	of NONE => NONE
	 | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c then
		   ((case Int.scan StringCvt.DEC List.getItem l
		       of SOME (n, nil) => SOME n
			| _ => NONE) handle Overflow => NONE)
		 else NONE
	        | nil => NONE
	  end
    fun getInt (s: string) : int option =
      case Ns.Conn.formvar s
	of NONE => NONE
	 | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Int.scan StringCvt.DEC List.getItem l
		       of SOME (n, nil) => SOME n
			| _ => NONE) handle Overflow => NONE)
		 else NONE
	        | nil => NONE
	  end
    fun getReal (s: string) : real option =
      case Ns.Conn.formvar s
	of NONE => NONE
	 | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Real.scan List.getItem l
		       of SOME (n, nil) => SOME n
			| _ => NONE) handle Overflow => NONE)
		 else NONE
	        | nil => NONE
	  end
    fun getNum (s: string) : real option =
      case Ns.Conn.formvar s
	of NONE => NONE
	 | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Real.scan List.getItem l
		       of SOME (n, nil) => SOME n
			| _ => 
			 (case Int.scan StringCvt.DEC List.getItem l
			    of SOME (n, nil) => SOME(real n)
			     | _ => NONE)) handle Overflow => NONE)
		 else NONE
	        | nil => NONE
	  end
    fun getString (s: string) : string option = Ns.Conn.formvar s
  end
