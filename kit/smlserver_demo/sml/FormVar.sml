  signature FORMVAR =
    sig
      val getNat      : string -> int option
      val getInt      : string -> int option
      val getReal     : string -> real option
      val getNum      : string -> real option
      val getString   : string -> string option
      val getIntRange : int -> int -> string -> int option

      val getNatOrFail    : string -> int
      val getIntOrFail    : string -> int
      val getRealOrFail   : string -> real
      val getNumOrFail    : string -> real
      val getStringOrFail : string -> string
      val getIntRangeOrFail : int -> int -> string -> int

      val returnError : string -> 'a
    end
  
structure FormVar : FORMVAR =
  struct
    fun returnError (err:string) : 'a = 
      (Ns.Quot.return
       `<html>
         <head><title>Form Error</title></head>
         <body bgcolor=white>
            <h2>Error processing form data</h2>
            ^err <hr> <i>Served by SMLserver</i>
         </body>
        </html>`;
       Ns.exit())

    fun wrapFail (f : string->'a) (s:string): 'a =
      f s handle Fail err => returnError err

    fun wrapOption (f : string->'a) (s:string): 'a option =
      SOME (f s) handle Fail err => NONE
      
    fun fail s = raise Fail s

    fun noFormVar s       = fail ("No form variable '" ^ s ^ "'") 
    fun emptyFormVar s    = fail ("Empty form variable '" ^ s ^ "'")       
    fun typeMismatch ty s = fail ("Form variable '" ^ s ^ "' should be " ^ ty)
    fun tooLarge ty s     = fail (ty ^ " in form variable '" ^ s ^ "' is too large")

    fun getNat0 (fv : string) : int =
      case Ns.Conn.formvar fv of 
	NONE => noFormVar fv
      | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c then
		   ((case Int.scan StringCvt.DEC List.getItem l
		       of SOME (n, nil) => n
			| _ => typeMismatch "a positive integer" fv) 
		       handle Overflow => tooLarge "Positive integer" fv)
		 else typeMismatch "a positive integer" fv
	        | nil => emptyFormVar fv
	  end
    fun getInt0 (fv: string) : int =
      case Ns.Conn.formvar fv of 
	NONE => noFormVar fv
      | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Int.scan StringCvt.DEC List.getItem l
		       of SOME (n, nil) => n
			| _ => typeMismatch "an integer" fv) 
		       handle Overflow => tooLarge "Integer" fv)
		 else typeMismatch "an integer" fv
	        | nil => emptyFormVar fv
	  end
    fun getReal0 (fv: string) : real =
      case Ns.Conn.formvar fv of 
	NONE => noFormVar fv
      | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Real.scan List.getItem l
		       of SOME (n, nil) => n
			| _ => typeMismatch "a real" fv) 
		       handle Overflow => tooLarge "Real" fv)
		 else typeMismatch "a real" fv
	        | nil => emptyFormVar fv
	  end
    fun getNum0 (fv: string) : real =
      case Ns.Conn.formvar fv of 
	NONE => noFormVar fv
      | SOME s => 
	  let val l = explode s
	  in case l
	       of c::_ => 
		 if Char.isDigit c orelse c = #"-" orelse c = #"~" then
		   ((case Real.scan List.getItem l
		       of SOME (n, nil) => n
			| _ => 
			 (case Int.scan StringCvt.DEC List.getItem l
			    of SOME (n, nil) => real n
			     | _ => typeMismatch "a numeral" fv)) 
		       handle Overflow => tooLarge "Numeral" fv)
		 else typeMismatch "a numeral" fv
	        | nil => emptyFormVar fv
	  end
    fun getString0 (fv: string) : string = 
      case Ns.Conn.formvar fv of
	SOME s => if size s = 0 then emptyFormVar fv 
		  else s
      | NONE => noFormVar fv

    fun getIntRange0 a b (fv: string) : int =
      let val i = getInt0 fv
      in if a <= i andalso i <= b then i
	 else fail ("Integer form variable `" ^ fv ^ "' is out of range")
      end

    val getNat    = wrapOption getNat0
    val getInt    = wrapOption getInt0
    val getReal   = wrapOption getReal0
    val getNum    = wrapOption getNum0
    val getString = wrapOption getString0
    fun getIntRange a b = wrapOption (getIntRange0 a b)

    val getNatOrFail    = wrapFail getNat0
    val getIntOrFail    = wrapFail getInt0
    val getRealOrFail   = wrapFail getReal0
    val getNumOrFail    = wrapFail getNum0
    val getStringOrFail = wrapFail getString0
    fun getIntRangeOrFail a b = wrapFail (getIntRange0 a b)
  end


