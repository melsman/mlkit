signature NS_SET =
  sig
    type set

    (* get the first value associated with key, if present *)
    val get : set * string -> string option
    val getOpt : set * string * string -> string

    (* Return the current size of a set *)
    val size : set -> int

    (* Check if a key in a set is unique, case sensitive *)
    val unique : set * string -> bool       

    (* Return the key name of a field *)
    val key : set * int -> string option    

    (* Return the value of a field *)
    val value : set * int -> string option

    (* Return the list representation of a set *)
    val list : set -> (string * string) list

    (* Return the elements that satisfy 
     * the property *)
    val filter : (string * string -> bool) 
      -> set -> (string * string) list

    (* Fold over a set *)
    val foldl : ((string * string) * 'a -> 'a) 
      -> 'a -> set -> 'a
    val foldr : ((string * string) * 'a -> 'a) 
      -> 'a -> set -> 'a
  end
