signature WEB_MIME = sig
  val getMime : string -> string
  val addEncoding : string -> string
end 
(*
 [getMime s] returns the mime-type of the file s based on the file's
 extension and it's content.  

 [addEncoding s] adds configured encoding to mime-type s.
*)
