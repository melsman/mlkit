(* Typed representable objects that may be passed as form
 * variables. *)

signature OBJ =
    sig
	type 't obj
	val fromInt     : int -> int obj
	val fromString  : string -> string obj
	val fromBool    : bool -> bool obj
	val valOf       : 'a obj -> 'a option

(*	val fromList    : ('a -> 'a obj) -> 'a list -> 'a list obj *)
    end

signature OBJ_EXTRA =
    sig
	include OBJ
	val toString    : 't obj -> string
	val fromBool'   : string -> bool obj
	val fromString' : string -> string obj
	val fromInt'    : string -> int obj
	val fromList'   : (string -> 'a obj) -> string list -> 'a list obj
	val fromOption' : (string -> 'a obj) -> string option -> 'a option obj
    end

structure Obj :> OBJ_EXTRA =
    struct
	datatype 't obj = BASE of string * (string -> 't option)
                        | LIST of string list * (string list -> 't option)
	                | OPTION of string option * (string option -> 't option)
	fun fromInt i    = BASE (Int.toString i, Int.fromString)
	fun fromString s = BASE (s, fn x => SOME x)
	fun fromBool b   = BASE (Bool.toString b, Bool.fromString)

	fun toString (BASE(v,f)) = v
	  | toString _ = raise Fail "Obj.toString not implemented for non-base objects"
	fun fromInt' s    = BASE (s, Int.fromString)
	fun fromString' s = BASE (s, fn x => SOME x)
	fun fromBool' s   = BASE (s, Bool.fromString)
	fun valOf (BASE(v,f)) = f v
	  | valOf (LIST(v,f)) = f v
	  | valOf (OPTION(v,f)) = f v
	fun fromList' (f: string -> 'a obj) (ss: string list) : 'a list obj =	    
	    LIST (ss, 
		  foldr (fn (s,NONE) => NONE
		          | (s,SOME xs) => (case valOf(f s) of
						SOME x => SOME (x::xs)
					      | NONE => NONE)) (SOME nil))

	fun fromOption' (f: string -> 'a obj) (so: string option) : 'a option obj =
	    OPTION (so,
		    fn so => 
		    SOME(case so of
			     SOME s => valOf (f s)
			   | NONE => NONE))
				       
    end
