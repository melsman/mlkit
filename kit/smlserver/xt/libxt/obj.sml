(* Typed representable objects that may be passed as form
 * variables. *)

signature OBJ =
    sig
	type 't obj
	val fromInt     : int -> int obj
	val fromString  : string -> string obj
	val fromBool    : bool -> bool obj
	val valOf       : 'a obj -> 'a option
    end

signature OBJ_EXTRA =
    sig
	include OBJ
	val toString    : 't obj -> string
	val fromBool'   : string -> bool obj
	val fromString' : string -> string obj
	val fromInt'    : string -> int obj
    end

structure Obj :> OBJ_EXTRA =
    struct
	type 't obj = string * (string -> 't option)
	fun fromInt i    = (Int.toString i, Int.fromString)
	fun fromString s = (s, fn x => SOME x)
	fun fromBool b   = (Bool.toString b, Bool.fromString)

	fun toString (v,f) = v
	fun fromInt' s    = (s, Int.fromString)
	fun fromString' s = (s, fn x => SOME x)
	fun fromBool' s   = (s, Bool.fromString)
	fun valOf (v,f)   = f v
    end
