
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

    local val c = ref 0
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

    (* Because the runtime system needs to know about the labels
     * of certain symbols, we predefine some names here *)
      
    fun new_rigid () : name =
      let val n = new()
      in mk_rigid n; bucket := nil; n
      end

    val reg_top = new_rigid()             (* name 0 *)
    val reg_bot = new_rigid()             (* name 1 *)
    val reg_string = new_rigid()          (* name 2 *)
    val reg_pair = new_rigid()            (* name 3 *)
    val reg_array = new_rigid()           (* name 4 *)
    val reg_ref = new_rigid()             (* name 5 *)
    val reg_triple = new_rigid()          (* name 6 *)

    val exn_DIV = new_rigid()             (* name 7 *)
    val exn_MATCH = new_rigid()           (* name 8 *)
    val exn_BIND = new_rigid()            (* name 9 *)
    val exn_OVERFLOW = new_rigid()        (* name 10 *)
    val exn_INTERRUPT = new_rigid()       (* name 11 *)

    local
	open Pickle
	val pu00 = tup3Gen(int, bool, ref0Gen bool)
	fun toRec (k,r,g) = {key=k, rigid=r, gen_mark=g}
	fun fromRec {key, rigid, gen_mark} = (key,rigid,gen_mark)
	val pu0 = convert (toRec,fromRec) pu00
    in
	val pu = ref0Gen pu0
	val pu_matchcount = int
    end
  end