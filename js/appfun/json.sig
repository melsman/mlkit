(** JSON library *)

signature JSON = sig
  type obj                      (* object map *)
  datatype t = RAW of string    (* embedded JSON string *)
             | OBJECT of obj
             | STRING of string
             | ARRAY of t list
             | NULL
             | BOOL of bool
             | NUMBER of string

  (* Operations on object maps *)
  val objFromList      : (string * t) list -> obj
  val objFromKeyValues : (string * string) list -> obj
  val objLook          : obj -> string -> t option
  val objFold          : ((string * t) * 'a -> 'a) -> 'a -> obj -> 'a
  val objList          : obj -> (string * t) list
  val objAdd           : obj -> string -> t -> obj
  val objEmp           : obj

  (* Operations on JSON values *)
  val toString      : t -> string
  val fromKeyValues : (string * string) list -> t

  val foldlArray    : (t * 'a -> 'a) -> 'a -> t -> 'a
  val foldrArray    : (t * 'a -> 'a) -> 'a -> t -> 'a

  val getBool       : t -> string -> bool   (* may raise Fail *)
  val getString     : t -> string -> string (* may raise Fail *)
  val getStringOpt  : t -> string -> string -> string

  (* Operations directly on strings containing json *)
  val fromString     : string -> t
  val foldlArrayJson : (t * 'a -> 'a) -> 'a -> string -> 'a
end

(**

[type obj] object map type.

[datatype t] JSON representation type.

[obj*] various operations for constructing and deconstructing object
maps.

[toString v] returns a string representation of the JSON value v.

[fromKeyValues kvs] returns an object map representation of the
key-value pairs in kvs.

[foldlArray f a t] results in a leftfold over a JSON array value in
t. Raises Fail(msg) if t is not a JSON array.

[foldrArray f a t] results in a rightfold over a JSON array value in
t. Raises Fail(msg) if t is not a JSON array.

[getBool t k] returns the boolean value associated with k in the JSON
object map t. Raises fail if t is not a JSON object map or if k is not
associated with a boolean value in t.

[getString t k] returns the string associated with k in the JSON
object map t. Raises fail if t is not a JSON object map or if k is not
associated with a string in t.

[getStringOpt t k v] returns the string associated with k in the JSON
object map t or v in case no value is associated with k in t. Raises
fail if t is not a JSON object map or if k is associated with a value
in t that is not a string.

[fromString s] returns a JSON representation of the JSON string
s. Raises Fail(msg) in case of a parse error.

[foldlArrayJson f a s] returns the result of folding over the JSON
array in s. This function avoids building the intermediate list
representation of the JSON array value. Raises Fail(msg) in case s
does not contain a JSON array.

Copyright 2015-2020, Martin Elsman, MIT-license.

*)
