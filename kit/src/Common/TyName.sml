(* Type names *)

functor TyName(
   structure TyCon                  : TYCON
   structure Name                   : NAME
   structure Flags                  : FLAGS
   structure PrettyPrint            : PRETTYPRINT
   structure Report                 : REPORT
	      ) : TYNAME =
  struct

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

    local val print_type_name_stamps = Flags.lookup_flag_entry "print_type_name_stamps"
    in fun pr_TyName (tn: TyName) : string =
         let val str = TyCon.pr_TyCon (tycon tn)
	 in if !print_type_name_stamps then
	      let val eq = if equality tn then "E " else ""
		  val id = Int.toString (Name.key (name tn))
	      in str ^ "(" ^ eq ^ id ^ ")"
	      end
	    else str
	 end
    end

    val tyName_BOOL = freshTyName{tycon=TyCon.tycon_BOOL, arity=0, equality=true}
    val tyName_INT  = freshTyName{tycon=TyCon.tycon_INT, arity=0, equality=true}
    val tyName_WORD = freshTyName{tycon=TyCon.tycon_WORD, arity=0, equality=true}
    val tyName_WORD8 = freshTyName{tycon=TyCon.tycon_WORD8, arity=0, equality=true}
    val tyName_REAL = freshTyName{tycon=TyCon.tycon_REAL, arity=0, equality=false}
    val tyName_STRING = freshTyName{tycon=TyCon.tycon_STRING, arity=0, equality=true}
    val tyName_CHAR = freshTyName{tycon=TyCon.tycon_CHAR, arity=0, equality=true}
    val tyName_LIST  = freshTyName{tycon=TyCon.tycon_LIST,  arity=1, equality=true}
    val tyName_WORD_TABLE = freshTyName{tycon=TyCon.tycon_WORD_TABLE, arity=1, equality=true}
    val tyName_REF = freshTyName{tycon=TyCon.tycon_REF, arity=1, equality=true}
    val tyName_EXN = freshTyName{tycon=TyCon.tycon_EXN, arity=0, equality=false}
    val _ = Rank.reset()

    structure Map : MONO_FINMAP = EqFinMap(structure Report = Report
					   structure PP = PrettyPrint
					   type dom = TyName
					   val eq = eq)

    structure Set : KIT_MONO_SET = EqSetList(structure PP = PrettyPrint
					     type elt = TyName
					     val eq = eq)

    type StringTree = PrettyPrint.StringTree
    val layout = PrettyPrint.LEAF o pr_TyName
  end;
