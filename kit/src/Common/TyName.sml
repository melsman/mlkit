(* Type names *)

functor TyName(
   structure TyCon                  : TYCON
   structure Name                   : NAME
   structure Flags                  : FLAGS
   structure PrettyPrint            : PRETTYPRINT
   structure Report                 : REPORT
   structure IntStringFinMap        : MONO_FINMAP where type dom = int * string
   structure Crash                  : CRASH
	      ) : TYNAME =
  struct
      
    val print_type_name_stamps = Flags.add_bool_entry
	{long="print_type_name_stamps", short=SOME "Ptypestamps", item=ref false, neg=false,
	 menu=["Layout", "print type name stamps"], desc=
	 "Print type name stamps and their attributes in types\n\
	  \and expressions."}

    fun die s = Crash.impossible ("TyName." ^ s)
    val tag_values = Flags.is_on0 "tag_values"

    (* Type names are based on names, which may be `matched'. In
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
		   equality: bool,
		   unboxed: bool ref}

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
	      
	  val pu = Pickle.int
	  val pu_rankrefOne = Pickle.refOneGen pu
	end
      end
			  

    fun fresh unboxed {tycon: tycon, arity: int, equality: bool} =
      let val name = Name.new()
      in (* if tycon = TyCon.tycon_EXN then print ("generating fresh type name exn(" ^ 
	                                           Int.toString(Name.key name) ^ ")\n") else (); *)
	{tycon=tycon, name=name, rank=Rank.new(), arity=arity, equality=equality, unboxed=ref unboxed}
      end

    fun freshTyName r = fresh false r

    fun arity ({arity, ...} : TyName) : int = arity

    fun equality ({equality, ...} : TyName) : bool = equality

    fun tycon ({tycon, ...} : TyName) : tycon = tycon

    fun name ({name, ...} : TyName) : name = name

    val id = Name.key o name

    (* We should only allow matching of type names when their attributes 
     * are equal; otherwise changes in attributes are not caught by
     * the system.. *)
    fun match(tn1,tn2) =
      if (equality tn1 = equality tn2 
	  andalso arity tn1 = arity tn2 
	  andalso !(#unboxed tn1) = !(#unboxed tn2)) then 
	Name.match(name tn1, name tn2)
      else ()

    val op eq = fn (tn1,tn2) => Name.eq(name tn1, name tn2)  

    val tyName_BOOL = fresh true {tycon=TyCon.tycon_BOOL, arity=0, equality=true}
      
    val tyName_INT31  = fresh true {tycon=TyCon.tycon_INT31, arity=0, equality=true}
    val tyName_INT32  = freshTyName{tycon=TyCon.tycon_INT32, arity=0, equality=true}
    val tyName_WORD8 = fresh true {tycon=TyCon.tycon_WORD8, arity=0, equality=true}
    val tyName_WORD31 = fresh true {tycon=TyCon.tycon_WORD31, arity=0, equality=true}
    val tyName_WORD32 = freshTyName{tycon=TyCon.tycon_WORD32, arity=0, equality=true}
    val tyName_REAL = freshTyName{tycon=TyCon.tycon_REAL, arity=0, equality=false}
    val tyName_STRING = freshTyName{tycon=TyCon.tycon_STRING, arity=0, equality=true}
    val tyName_CHAR = fresh true {tycon=TyCon.tycon_CHAR, arity=0, equality=true}
    val tyName_LIST = fresh true {tycon=TyCon.tycon_LIST,  arity=1, equality=true}
    val tyName_FRAG = freshTyName{tycon=TyCon.tycon_FRAG,  arity=1, equality=true}
    val tyName_REF = freshTyName{tycon=TyCon.tycon_REF, arity=1, equality=true}
    val tyName_ARRAY = freshTyName{tycon=TyCon.tycon_ARRAY, arity=1, equality=true}
    val tyName_VECTOR = freshTyName{tycon=TyCon.tycon_VECTOR, arity=1, equality=true}
    val tyName_CHARARRAY = freshTyName{tycon=TyCon.tycon_CHARARRAY, arity=0, equality=true}
    val tyName_FOREIGNPTR = fresh true {tycon=TyCon.tycon_FOREIGNPTR, arity=0, equality=false}
    val tyName_EXN = freshTyName{tycon=TyCon.tycon_EXN, arity=0, equality=false}
    val _ = Rank.reset()

    fun tyName_IntDefault() = if tag_values() then tyName_INT31 else tyName_INT32
    fun tyName_WordDefault() = if tag_values() then tyName_WORD31 else tyName_WORD32

    fun pr_TyName (tn: TyName) : string =
      let val str = TyCon.pr_TyCon (tycon tn)
      in
	if print_type_name_stamps() then
	  let val eq = if equality tn then "E " else ""
	      val (i,b) = Name.key (name tn)
	      val id = Int.toString i
	  in str ^ "(" ^ eq ^ id ^ b ^ ")"
	  end
	else 
	  (if tag_values() then 
	     (if eq(tn, tyName_INT31) then "int"
	      else if eq(tn, tyName_WORD31) then "word"
		   else str)
	   else 
	     (if eq(tn, tyName_INT32) then "int"
	      else if eq(tn, tyName_WORD32) then "word"
		   else str))
      end

    fun unboxed_num32 tn = 
      not(tag_values()) andalso (eq(tn,tyName_INT32)
				   orelse eq(tn,tyName_WORD32))

    fun unboxed tn = unboxed_num32 tn orelse !(#unboxed tn)

    fun setUnboxed (tn: TyName) : unit = 
	if unboxed tn then 
	    die ("setUnboxed.tyname " ^ pr_TyName tn ^ " already marked as unboxed")
	else #unboxed tn := true

    val pu = 
	Pickle.hashConsEq eq
	(Pickle.register "TyName"
	 [tyName_BOOL,tyName_INT31,tyName_INT32,tyName_WORD8,tyName_WORD31,
	  tyName_WORD32,tyName_REAL,tyName_STRING,tyName_CHAR,tyName_LIST,tyName_FRAG,
	  tyName_REF,tyName_ARRAY,tyName_VECTOR,tyName_CHARARRAY,tyName_FOREIGNPTR,tyName_EXN]
	 let open Pickle
	     fun to ((t,n,a),r,e,u) : TyName = 
		 {tycon=t, name=n, arity=a, rank=r,
		  equality=e, unboxed=u}
	     fun from ({tycon=t, name=n, arity=a, rank=r,
			equality=e, unboxed=u} : TyName) = ((t,n,a),r,e,u)	    
	 in newHash (#1 o Name.key o name)
	     (convert (to,from)
	      (tup4Gen0(tup3Gen0(TyCon.pu,Name.pu,int),
			refOneGen int,bool,refOneGen bool)))
	 end)

    structure QD : QUASI_DOM =
      struct
	type dom = TyName
	type name = Name.name
	val name = name
	val pp = pr_TyName
      end

    structure Map = QuasiMap(structure IntStringFinMap = IntStringFinMap
			     structure QD = QD
			     structure Name = Name
			     structure Crash = Crash
			     structure PP = PrettyPrint
			     structure Report = Report)

    structure Set = QuasiSet(structure IntStringFinMap = IntStringFinMap
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
