(* Type names *)

functor TyName(
   structure TyCon                  : TYCON
   structure Name                   : NAME
   structure Flags                  : FLAGS
   structure PrettyPrint            : PRETTYPRINT
   structure Report                 : REPORT
   structure IntFinMap              : MONO_FINMAP where type dom = int
   structure Crash                  : CRASH
	      ) : TYNAME =
  struct

    val tag_integers = Flags.is_on0 "tag_integers"

    (* Type names are based on names which may be `matched'. In
     * particular, if two type names, n1 and n2, are successfully
     * matched, eq(n1,n2) = true. This may affect the canonical
     * ordering of type names. *)

    type tycon = TyCon.tycon
    type name = Name.name
    type rank = int

    type TyName = {tycon: tycon, 
		   name: name, 
		   arity: int,
		   rank: rank ref,
		   equality: bool}

    structure Rank =
      struct
	type rank = rank
	local val current_rank = ref 0
              val rank_bucket : rank ref list ref = ref []
	in
	  fun current () = !current_rank
	  fun new () = (current_rank := !current_rank + 1;
			let val rr = ref (!current_rank)
			in rank_bucket := rr :: ( !rank_bucket ) ;
			  rr 
			end)
	  fun min(r1,r2) = Int.min (r1, r2)
	  fun reset () =
	    (List.app (fn rr => rr := 0) (!rank_bucket);
	     rank_bucket := [];
	     current_rank := 0)
	  val op <= : rank * rank -> bool = op <=

	  fun from_TyName({rank=ref rank,...}:TyName) = rank
	end
      end
			  

    fun freshTyName {tycon: tycon, arity: int, equality: bool} =
      let val name = Name.new()
      in (* if tycon = TyCon.tycon_EXN then print ("generating fresh type name exn(" ^ 
	                                           Int.toString(Name.key name) ^ ")\n") else (); *)
	{tycon=tycon, name=name, rank=Rank.new(), arity=arity, equality=equality}
      end

    fun arity ({arity, ...} : TyName) : int = arity

    fun equality ({equality, ...} : TyName) : bool = equality

    fun tycon ({tycon, ...} : TyName) : tycon = tycon

    fun name ({name, ...} : TyName) : name = name

    (* We should only allow matching of type names when their attributes 
     * are equal; otherwise changes in attributes are not caught by
     * the system.. *)
    fun match(tn1,tn2) =
      if equality tn1 = equality tn2 andalso arity tn1 = arity tn2 then 
	Name.match(name tn1, name tn2)
      else ()

    val id = Name.key o name

    val op eq = (fn (tn1,tn2) => id tn1 = id tn2)  

    val tyName_BOOL = freshTyName{tycon=TyCon.tycon_BOOL, arity=0, equality=true}
    val tyName_INT31  = freshTyName{tycon=TyCon.tycon_INT31, arity=0, equality=true}
    val tyName_INT32  = freshTyName{tycon=TyCon.tycon_INT32, arity=0, equality=true}
    val tyName_WORD8 = freshTyName{tycon=TyCon.tycon_WORD8, arity=0, equality=true}
    val tyName_WORD31 = freshTyName{tycon=TyCon.tycon_WORD31, arity=0, equality=true}
    val tyName_WORD32 = freshTyName{tycon=TyCon.tycon_WORD32, arity=0, equality=true}
    val tyName_REAL = freshTyName{tycon=TyCon.tycon_REAL, arity=0, equality=false}
    val tyName_STRING = freshTyName{tycon=TyCon.tycon_STRING, arity=0, equality=true}
    val tyName_CHAR = freshTyName{tycon=TyCon.tycon_CHAR, arity=0, equality=true}
    val tyName_LIST  = freshTyName{tycon=TyCon.tycon_LIST,  arity=1, equality=true}
    val tyName_WORD_TABLE = freshTyName{tycon=TyCon.tycon_WORD_TABLE, arity=1, equality=true}
    val tyName_REF = freshTyName{tycon=TyCon.tycon_REF, arity=1, equality=true}
    val tyName_EXN = freshTyName{tycon=TyCon.tycon_EXN, arity=0, equality=false}
    val _ = Rank.reset()

    fun tyName_IntDefault() = if tag_integers() then tyName_INT31 else tyName_INT32
    fun tyName_WordDefault() = if tag_integers() then tyName_WORD31 else tyName_WORD32

    val print_type_name_stamps = Flags.is_on0 "print_type_name_stamps"

    fun pr_TyName (tn: TyName) : string =
      let val str = TyCon.pr_TyCon (tycon tn)
      in
	if print_type_name_stamps() then
	  let val eq = if equality tn then "E " else ""
	    val id = Int.toString (Name.key (name tn))
	    in str ^ "(" ^ eq ^ id ^ ")"
	  end
	else 
	  (if tag_integers() then 
	     (if eq(tn, tyName_INT31) then "int"
	      else if eq(tn, tyName_WORD31) then "word"
		   else str)
	   else 
	     (if eq(tn, tyName_INT32) then "int"
	      else if eq(tn, tyName_WORD32) then "word"
		   else str))
      end

    fun unboxed (tn : TyName) : bool = 
      eq(tn, tyName_BOOL) 
      orelse eq(tn, tyName_CHAR) 
      orelse eq(tn, tyName_WORD8) 
      orelse eq(tn, tyName_INT31) 
      orelse eq(tn, tyName_WORD31) 
      orelse eq(tn, tyName_LIST) 
      orelse ( not(tag_integers()) andalso ( eq(tn,tyName_INT32) 
					    orelse eq(tn,tyName_WORD32) ))

    structure QD : QUASI_DOM =
      struct
	type dom = TyName
	type name = Name.name
	val name = name
	val pp = pr_TyName
      end

    structure Map = QuasiMap(structure IntFinMap = IntFinMap
			     structure QD = QD
			     structure Name = Name
			     structure Crash = Crash
			     structure PP = PrettyPrint
			     structure Report = Report)

    structure Set = QuasiSet(structure IntFinMap = IntFinMap
			     structure QD = QD
			     structure Name = Name
			     structure Crash = Crash
			     structure PP = PrettyPrint)

    type StringTree = PrettyPrint.StringTree
    val layout = PrettyPrint.LEAF o pr_TyName

    structure TestMap =
      struct
	(*
	val _ = print "[test begin]\n"
	fun error s = print ("error: " ^ s ^ "\n")
	fun assert s false = error s
	  | assert _ _ = ()

	fun new s = freshTyName{tycon=TyCon.mk_TyCon s, arity=0, equality=false}
	  
	val t = new "t"

	val l = ["t1", "t2", "t3", "t4", "t5"]

	val ts as [t1,t2,t3,t4,t5] = map new l

	val m = Map.fromList (map (fn t => (t, pr_TyName t)) ts)

	val _ = case Map.lookup m t2
		  of SOME s => assert "test1" (s=pr_TyName t2)
		   | _ => error "test2"
	val m' = Map.restrict (m,[t3,t4])

	val _ = case Map.lookup m' t2
		  of NONE => ()
		   | _ => error "test3"

	val s4 = pr_TyName t4
	val _ = case Map.lookup m' t4
		  of SOME s => assert "test4" (s=s4)
		   | _ => error "test5"

	val _ = Name.mark_gen (name t4)	    
	val _ = Name.mark_gen (name t)	    
	val _ = match(t4,t)
	val _ = Name.unmark_gen (name t4)	    
	val _ = Name.unmark_gen (name t)	    
	val _ = case Map.lookup m' t
		  of SOME s => assert "test6" (s=s4)
		   | _ => error "test7"
	val _ = case Map.lookup m' t4
		  of SOME s => assert "test8" (s=s4)
		   | _ => error "test9"

	val _ = print "[end of test]\n"
*)
      end
  end;
