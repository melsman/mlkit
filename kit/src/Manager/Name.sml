
(*$Name: NAME *)

functor Name() : NAME =
  struct

    (* Names may be generated fresh and 
     * matched if marked generative. *)

    type name = {key: int ref, mark: bool ref}
      (* only allow equality through use of key. *)

    fun key ({key = ref i,...} : name) : int = i

    (* Bucket for generated names *)
    val bucket = ref ([] : name list) 

    local val init = ref 0
          val count = ref (!init)
    in
       fun reset() = count := (!init)
       fun commit () = init := (!count)
       fun new () : name = 
	 let val key = ref (!count)
	     val name = {key = key, mark = ref false}
	 in bucket := name :: !bucket;
	    count := !count + 1; name
	 end
    end
 
    fun mark_gen ({mark=ref true,...} : name) = ()
      | mark_gen ({mark,...} : name) = mark := true
    fun unmark_gen ({mark=ref false,...} : name) = ()
      | unmark_gen ({mark,...} : name) = mark := false

    fun match ({key,mark=m as ref true}:name, 
	       {key=key',mark=m' as ref true}:name) = 
      (key := (!key'); m := false; m' := false)
      | match _ = () 
      
      (* match(n,n')  matches n to n' if both n and n' are
       * marked generative. After a succeding match, n and n' 
       * are unmarked. *)
  end