(* Type names *)

(*$TyName : TYCON NAME FLAGS TYNAME REPORT PRETTYPRINT
            ORDER_FINMAP OrderFinMap KIT_MONO_SET OrderSet*)
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

    type TyName = {tycon: tycon, 
		   name: name, 
		   arity: int, 
		   equality: bool}

    fun freshTyName {tycon: tycon, arity: int, equality: bool} =
      {tycon=tycon, name=Name.new(), arity=arity, equality=equality}

    fun arity ({arity, ...} : TyName) : int = arity

    fun equality ({equality, ...} : TyName) : bool = equality

    fun tycon ({tycon, ...} : TyName) : tycon = tycon

    fun name ({name, ...} : TyName) : name = name

    fun match(tn1,tn2) = Name.match(name tn1, name tn2)

    val id = Name.key o name

    val op < = (fn (tn1,tn2) => id tn1 < id tn2) 

    val op eq = (fn (tn1,tn2) => id tn1 = id tn2)  

    fun pr_TyName (tn: TyName) : string =
      let val str = TyCon.pr_TyCon (tycon tn)
      in if !Flags.DEBUG_TYPES then
	   let val eq = if equality tn then "E " else ""
	       val id = Int.string (Name.key (name tn))
	   in str ^ "(" ^ eq ^ id ^ ")"
	   end
	 else str ^ (Int.string (Name.key (name tn)))
      end

    val tyName_BOOL = freshTyName{tycon=TyCon.tycon_BOOL, arity=0, equality=true}
    val tyName_INT  = freshTyName{tycon=TyCon.tycon_INT, arity=0, equality=true}
    val tyName_WORD = freshTyName{tycon=TyCon.tycon_WORD, arity=0, equality=true}
    val tyName_REAL = freshTyName{tycon=TyCon.tycon_REAL, arity=0, equality=false}
    val tyName_STRING = freshTyName{tycon=TyCon.tycon_STRING, arity=0, equality=true}
    val tyName_CHAR = freshTyName{tycon=TyCon.tycon_CHAR, arity=0, equality=true}
    val tyName_LIST = freshTyName{tycon=TyCon.tycon_LIST, arity=1, equality=true}
    val tyName_REF = freshTyName{tycon=TyCon.tycon_REF, arity=1, equality=true}
    val tyName_EXN = freshTyName{tycon=TyCon.tycon_EXN, arity=0, equality=false}
    structure Order : ORDERING =
      struct
	type T = TyName
	fun lt t1 t2 = t1 < t2
      end
    
    structure Map : ORDER_FINMAP = OrderFinMap(structure Order = Order
					       structure PP = PrettyPrint
					       structure Report = Report)

    structure Set : KIT_MONO_SET = OrderSet(structure Order = Order
					    structure PP = PrettyPrint)

    type StringTree = PrettyPrint.StringTree
    val layout = PrettyPrint.LEAF o pr_TyName
  end;
