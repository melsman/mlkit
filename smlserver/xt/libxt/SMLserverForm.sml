(* Typed representable objects that may be passed as form
 * variables. *)

structure SMLserverFormUnsafe : SMLSERVER_FORM_UNSAFE =
    struct
	datatype 't var = BASE of string * (string -> 't option)
                        | LIST of string list * (string list -> 't option)
	                | OPTION of string option * (string option -> 't option)
	                | MISSING 
	type 't Type = 't -> 't var
	fun Int i    = BASE (Int.toString i, Int.fromString)
	fun String s = BASE (s, fn x => SOME x)
	fun Bool b   = BASE (Bool.toString b, Bool.fromString)

	datatype 't arg = Ok of 't | Wrong | Missing
	fun wrapOk f v = 
	    (case f v of 
		 SOME v => Ok v 
	       | NONE => Wrong)
		 handle _ => Missing
	fun get (BASE(v,f)) = wrapOk f v
	  | get (LIST(v,f)) = wrapOk f v
	  | get (OPTION(v,f)) = wrapOk f v
	  | get (MISSING) = Missing

	fun toString (BASE(v,f)) = v
	  | toString _ = raise Fail "Form.toString not implemented for non-base objects"
	fun fromInt s    = BASE (s, Int.fromString)
	fun fromString s = BASE (s, fn x => SOME x)
	fun fromBool s   = BASE (s, Bool.fromString)
	fun fromList (f: string -> 'a var) (ss: string list) : 'a list var =	    
	    LIST (ss, 
		  foldr (fn (s,NONE) => NONE
   		          | (s,SOME xs) => (case get(f s) of
						Ok x => SOME (x::xs)
					      | _ => NONE)) (SOME nil))
	    
	fun fromOption (f: string -> 'a var) (so: string option) : 'a option var =
	    OPTION (so,
		    fn so => 
		    SOME(case so of
			     SOME s => (case get (f s) of
					    Ok x => SOME x
					  | _ => NONE)
			   | NONE => NONE))
	fun missing() = MISSING	    
    end
