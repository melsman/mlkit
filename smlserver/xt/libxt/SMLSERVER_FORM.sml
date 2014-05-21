signature SMLSERVER_FORM =
    sig
	type 't var

	(* Support for extracting values from forms *)
	datatype 't arg = Ok of 't | Wrong | Missing 
	val get : 't var -> 't arg

	(* Support for constructing form content to be used in 
	 * hidden form variables, radio boxes, etc. *)
	type 't Type = 't -> 't var
	val Int    : int Type
	val String : string Type
	val Bool   : bool Type
(*
	val Pair   : 'a Type * 'b Type -> ('a * 'b) Type
	val Option : 'a Type -> 'a option Type
	val List   : 'a Type -> 'a list Type
*)
    end

signature SMLSERVER_FORM_UNSAFE =
    sig
	include SMLSERVER_FORM

	(* The function toString is used for generating actual
	 * XHTML to send to clients *)
	val toString   : 't var -> string
		    
	(* The following functions are used for scriptlet 
	 * functor instantiations *)
	val fromBool   : string -> bool var
	val fromString : string -> string var
	val fromInt    : string -> int var
	val fromList   : (string -> 'a var) -> string list -> 'a list var
	val fromOption : (string -> 'a var) -> string option -> 'a option var
	    
	val missing    : unit -> 'a var
    end
