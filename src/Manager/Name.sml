
functor Name(structure Crash : CRASH) : NAME =
  struct

    fun die s = Crash.impossible ("Name." ^ s)

    (* Support for a base in a name (compilation unit) *)
    local
	val baseCurrent : string ref = ref "CompilerInitial"
    in
	fun baseSet s = baseCurrent := s
	fun baseGet () = !baseCurrent
    end

    (* Names may be generated fresh and 
     * matched if marked generative. *)

    type matchcount = int

    local val current = ref 0
    in fun current_matchcount() = !current
       val matchcount_lt = op <
       fun incr_matchcount() = (current := !current + 1)
       val matchcount_invalid = ~1
    end

    (* The string is the base *)
    type name0 = {key: int*string, rigid: bool, gen_mark: bool ref} 

    type name = name0 ref

    fun key (ref {key=k,...} : name) = k

    fun eq (n1,n2) = key n1 = key n2

    fun lt (n1,n2) =
	let val (i1,s1) = key n1
	    val (i2,s2) = key n2
	in i1 < i2 orelse (i1=i2 andalso s1 < s2)
	end

    (* used by Manager to alpha-rename export bases *)
    fun assignKey (r as ref {key=(_,s),rigid,gen_mark=g},i) =
	if rigid then die "assignKey on rigid name"
	else r := {key=(i,s),rigid=rigid,gen_mark=g}

    (* Bucket for generated names *)
    val bucket = ref ([] : name list) 

    local val c = ref 0
          fun incr() = let val a = !c
		       in c:= a + 1; a
		       end
    in
      fun new () : name = 
	let val key = (incr(),baseGet())
	    val name = ref {key=key,rigid=false,gen_mark=ref false}
	in bucket := name :: !bucket; name
	end
    end

    fun mk_rigid (ref {rigid=true,...}) = ()
      | mk_rigid (r as ref {key,...}) = r := {key=key, rigid=true, gen_mark=ref false}

    fun rigid (ref {rigid,...} : name) = rigid

    val rematching = ref false

    fun match(n1 as ref {gen_mark=ref true,rigid=false,key=k1} : name, 
	      ref (n0 as {gen_mark=gen_mark as ref true,key=k2,...})) =
	if #2 k1 = #2 k2 orelse !rematching then
	    (gen_mark := false;
	     n1 := n0;
	     incr_matchcount() (* ; print ".\n"*) )
	else ()
      | match _ = ()

    fun mark_gen (ref {gen_mark,...} : name) = gen_mark:= true

    fun unmark_gen (ref {gen_mark,...} : name) = gen_mark:= false

    fun is_gen (ref {gen_mark,...} : name) = !gen_mark

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
	fun toRec (k,r,g) = {key=k, rigid=r, gen_mark=g}
	fun fromRec {key, rigid, gen_mark} = (key,rigid,gen_mark)
	val pu0 = convert (toRec,fromRec) 
	    (tup3Gen0(pairGen(int,string), bool, refOneGen bool))
    in
	val pu = 
	    hashConsEq eq
	    (register "Name" [reg_top, reg_bot, reg_string, reg_pair,
			      reg_array, reg_ref, reg_triple,	     
			      exn_DIV, exn_MATCH, exn_BIND,
			      exn_OVERFLOW, exn_INTERRUPT]
	     (ref0EqGen eq pu0))
	val pu_matchcount = int
    end
  end