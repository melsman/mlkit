(* Strbase -- internal auxiliaries for String and Substring 
   1995-04-13, 1995-11-06  *)

signature STR_BASE =
  sig
    type substring = string * int * int

    val dropl     : (char -> bool) -> substring -> substring
    val dropr     : (char -> bool) -> substring -> substring
    val takel     : (char -> bool) -> substring -> substring
    val taker     : (char -> bool) -> substring -> substring
    val splitl    : (char -> bool) -> substring -> substring * substring
    val splitr    : (char -> bool) -> substring -> substring * substring
	
    val translate : (char -> string) -> substring -> string
      
    val tokens    : (char -> bool) -> substring -> substring list
    val fields    : (char -> bool) -> substring -> substring list
      
    val foldl     : (char * 'a -> 'a) -> 'a -> substring -> 'a
    val fromMLescape : ('a -> (char * 'a) option) -> ('a -> (char * 'a) option)
    val toMLescape   : char -> string
    val fromCescape  : ('a -> (char * 'a) option) -> ('a -> (char * 'a) option)
    val toCescape    : char -> string
    val fromCString  : string -> string option 

    val explode   : string -> char list
  end

