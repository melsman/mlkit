signature WEB_SERIALIZE =
sig 
  type 'a Type = {name: string,
		  to_string: 'a -> string,
		  from_string: string -> 'a}
  val Pair : 'a Type -> 'b Type -> ('a * 'b) Type
  val Option : 'a Type -> ('a option) Type
  val List : 'a Type -> ('a list) Type
  val Triple : 'a Type -> 'b Type -> 'c Type 
	       -> ('a * 'b * 'c) Type
  val Unit : unit Type
  val Int : int Type
  val Real : real Type
  val Bool : bool Type
  val Char : char Type
  val String : string Type
  val Time : Time.time Type
end

(* 
 [Pair aType bType] returns the pair type representing the
 pairs (a,b) where a is of type aType and b is of type
 bType.

 [Option aType] returns the type aType option, representing
 a option where a is of type aType.

 [List aType] returns the list type representing the list
 of elements of type aType.

 [Triple aType bType cType] similar to Pair except that the
 triple is represented with as one Pair embedded in another
 Pair: ((a,b),c) where a is of type aType, b is of type
 bType and c is of type cType.

 [Unit] predefined type representing units.

 [Int] predefined type representing integers.

 [Real] predefined type representing reals.

 [Bool] predefined type representing booleans.

 [Char] predefined type representing characters.

 [String] predefined type representing strings.

 [Time] predefined type representing Time.time values.
*)

