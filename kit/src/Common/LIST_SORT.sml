(*$LIST_SORT*)
signature LIST_SORT = 
sig
  val sort : ('a * 'a -> bool) -> 'a list -> 'a list

  (* sort lt l   sorts l according to the ordering lt *)
end;