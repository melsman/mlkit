signature WEB_SERIALIZE =
sig 
	type 'a Type = {name: string,
			to_string: 'a -> string,
			from_string: string -> 'a}
  val Pair : 'a Type -> 'b Type -> ('a * 'b) Type
  val Option : 'a Type -> ('a option) Type
  val List : 'a Type -> ('a list) Type
  val Triple : 'a Type -> 'b Type -> 'c Type -> ('a * 'b * 'c) Type
  val Unit : unit Type
  val Int : int Type
  val Real : real Type
  val Bool : bool Type
  val Char : char Type
  val String : string Type
  val Time : Time.time Type
end

