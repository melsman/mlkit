
functor Name(structure Crash : CRASH) : NAME =
  struct

    fun die s = Crash.impossible ("Name." ^ s)

    (* Names may be generated fresh and 
     * matched if marked generative. *)

    type matchcount = int

    local val current = ref 0
    in fun current_matchcount() = !current
       val matchcount_lt = op <
       fun incr_matchcount() = (current := !current + 1)
    end

    type name0 = {key: int, rigid: bool, gen_mark: bool ref} 

    type name = name0 ref

    fun key (ref {key=k,...} : name) = k

    (* Bucket for generated names *)
    val bucket = ref ([] : name list) 
(*
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

    fun is_gen ({mark=ref true,...}: name) = true
      | is_gen _ = false

    fun match ({key,mark=m as ref true}:name, 
	       {key=key',mark=m' as ref true}:name) = 
      (key := (!key'); m := false; m' := false)
      | match _ = () 
      
      (* match(n,n')  matches n to n' if both n and n' are
       * marked generative. After a succeding match, n and n' 
       * are unmarked. *)
*)
    local val c = ref 1
          fun incr() = let val a = !c
		       in c:= a + 1; a
		       end
    in
      fun new () : name = 
	let val name = ref {key=incr(),rigid=false,gen_mark=ref false}
	in bucket := name :: !bucket; name
	end
    end

    fun mk_rigid (ref {rigid=true,...}) = ()
      | mk_rigid (r as ref {key,...}) = r := {key=key, rigid=true, gen_mark=ref false}

    fun rigid (ref {rigid=true,...} : name) = true
      | rigid _ = false

    fun match(n1 as ref {gen_mark=ref true,rigid=false,...} : name, 
	      ref (n0 as {gen_mark=gen_mark as ref true,...})) =
      (gen_mark := false;
       n1 := n0;
       incr_matchcount() (* ; print ".\n"*) )
      | match _ = ()

    fun mark_gen (ref {gen_mark,...} : name) = gen_mark:= true

    fun unmark_gen (ref {gen_mark,...} : name) = gen_mark:= false

    fun is_gen (ref {gen_mark,...} : name) = !gen_mark

    fun reset () = ()
    fun commit () = ()

  end