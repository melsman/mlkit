(*$LambdaExp: LVARS CON EXCON TYNAME PRETTYPRINT CRASH LAMBDA_EXP FLAGS*)

functor LambdaExp(structure Lvars: LVARS
		  structure Con: CON
		  structure Excon: EXCON
		  structure TyName: TYNAME
		  structure PP: PRETTYPRINT  
		  structure Crash: CRASH
		  structure Flags: FLAGS) : LAMBDA_EXP =
  struct
    type lvar = Lvars.lvar
    type con = Con.con
    type excon = Excon.excon
    type TyName = TyName.TyName

    type tyvar = int
    fun equality_tyvar tyvar = (tyvar mod 2) = 1
    fun pr_tyvar tyvar = (if equality_tyvar tyvar then "''a" else "'a") ^ Int.string tyvar
    val lt_tyvar : tyvar * tyvar -> bool = op <
    local 
      val init_tyvar_no = ref 0
      val init_eqtyvar_no = ref 1
      val tyvar_no = ref (!init_tyvar_no)
      val eqtyvar_no = ref (!init_eqtyvar_no)
      fun incr2 c = let val n = !c in c := !c + 2; n end
    in
      fun fresh_tyvar () = incr2 tyvar_no
      fun fresh_eqtyvar () = incr2 eqtyvar_no
      fun reset() = (tyvar_no := !init_tyvar_no; eqtyvar_no := !init_eqtyvar_no)
      fun commit() = (init_tyvar_no := !tyvar_no; init_eqtyvar_no := !eqtyvar_no)
    end


    datatype Type =
        TYVARtype   of tyvar
      | ARROWtype   of Type list * Type list
      | CONStype    of Type list * TyName
      | RECORDtype  of Type list

    fun foldl (g: 'a -> 'b -> 'a) (acc: 'a) [] = acc
      | foldl g acc (x::xs) = foldl g (g acc x) xs

    fun foldType (g : 'a -> Type -> 'a) (acc: 'a) (tau : Type) : 'a =
      case tau of
        TYVARtype _ => g acc tau
      | ARROWtype(taus1,taus2) => g (foldTypes g (foldTypes g acc taus2) taus1 ) tau
      | CONStype(taus,_) => g(foldTypes g acc taus)tau
      | RECORDtype(taus) => g(foldTypes g acc taus)tau
    and foldTypes g acc taus = foldl (foldType g) acc taus
        
    fun size_type tau = foldType (fn n:int => fn _ => n+1)

    val intType = CONStype([], TyName.tyName_INT)
    val boolType = CONStype([], TyName.tyName_BOOL)
    val exnType = CONStype([], TyName.tyName_EXN)
    val realType = CONStype([], TyName.tyName_REAL)
    val stringType = CONStype([], TyName.tyName_STRING)
    val unitType = RECORDtype([])

    datatype TypeList =                               (* To allow the result of a declaration *)  
        Types of Type list                            (* to be a raised Bind exception. *)
      | Frame of {declared_lvars: {lvar : lvar, tyvars: tyvar list, Type: Type} list,
		  declared_excons: (excon * Type Option) list}
      | RaisedExnBind

    datatype 'Type prim =                             (* The primitives are always fully applied ! *)
        CONprim of {con : con, instances : 'Type list}
      | DECONprim of {con : con, instances : 'Type list}
      | EXCONprim of excon
      | DEEXCONprim of excon
      | RECORDprim 
      | SELECTprim of int        
      | UB_RECORDprim                                 (* Unboxed record. *)
      | NOTprim                                       (* Pervasives, Definition p. 75. *)
      | NEG_INTprim 
      | NEG_REALprim
      | ABS_INTprim
      | ABS_REALprim
      | FLOORprim
      | REALprim
      | SQRTprim
      | SINprim
      | COSprim
      | ARCTANprim
      | EXPprim
      | LNprim
      | SIZEprim
      | CHRprim
      | ORDprim
      | EXPLODEprim
      | IMPLODEprim
      | DEREFprim of {instance: 'Type}
      | REFprim of {instance: 'Type}
      | ASSIGNprim of {instance: 'Type}
      | DIV_REALprim                
      | DIV_INTprim
      | MODprim
      | MUL_REALprim
      | MUL_INTprim
      | PLUS_REALprim
      | PLUS_INTprim
      | MINUS_REALprim
      | MINUS_INTprim
      | STRING_CONCATprim
      | EQUALprim of {instance: 'Type}
      | NOTEQUALprim of {instance: 'Type}
      | LESS_REALprim
      | LESS_INTprim
      | GREATER_REALprim
      | GREATER_INTprim
      | LESSEQ_REALprim
      | LESSEQ_INTprim
      | GREATEREQ_REALprim
      | GREATEREQ_INTprim
      | OPEN_INprim                                   (* I/O *)
      | OPEN_OUTprim
      | INPUTprim
      | LOOKAHEADprim
      | CLOSE_INprim
      | END_OF_STREAMprim
      | OUTPUTprim
      | CLOSE_OUTprim
      | USEprim                                       (* NOT Standard ML *)
      | FLUSH_OUTprim                                 (* NOT Standard ML *)
      | STD_INprim
      | STD_OUTprim
      | CCALLprim of string * {instance : 'Type}      (* NOT Standard ML *)
      | RESET_REGIONSprim of {instance: 'Type}        (* NOT Standard ML, for programmer-directed, but safe, resetting of
						       * regions *)
      | FORCE_RESET_REGIONSprim of {instance: 'Type}  (* NOT Standard ML, for programmer-controlled, unsafe resetting of
						       * regions *)

    datatype LambdaPgm = PGM of datbinds * LambdaExp

    and datbinds = DATBINDS of (tyvar list * TyName * (con * Type Option) list) list list
      (* list of mutual recursive datatype declarations *)

    and LambdaExp =
        VAR      of {lvar: lvar, instances : Type list}
      | INTEGER  of int			
      | STRING   of string
      | REAL     of real
      | FN       of {pat : (lvar * Type) list, body : LambdaExp}
      | LET      of {pat : (lvar * tyvar list * Type) list,
		     bind : LambdaExp,
		     scope: LambdaExp}
      | FIX      of {functions : {lvar : lvar, 
				  tyvars : tyvar list,
				  Type : Type,
				  bind : LambdaExp} list,
		     scope : LambdaExp}
      | APP      of LambdaExp * LambdaExp
      | EXCEPTION of excon * Type Option * LambdaExp
      | RAISE    of LambdaExp * TypeList
      | HANDLE   of LambdaExp * LambdaExp
      | SWITCH_I of int Switch
      | SWITCH_S of string Switch
      | SWITCH_C of con Switch
      | SWITCH_E of excon Switch
      | PRIM     of Type prim * LambdaExp list
      | FRAME    of {declared_lvars: {lvar : lvar, tyvars: tyvar list, Type: Type} list,
                     declared_excons: (excon * Type Option) list}
                       (* a frame is the result of a structure-level
                        * declaration. 
			*)

    and 'a Switch = SWITCH of LambdaExp * ('a * LambdaExp) list * LambdaExp Option

      
    fun foldTD (fcns as (f:'a->LambdaExp->'a, g: 'a -> Type -> 'a))
               (acc:'a) (lamb:LambdaExp) =
      let
	val new_acc = f acc lamb
	
	fun foldSwitch (SWITCH(arg, selections, wildcard)) =
	  let
	    val acc' = foldl (foldTD fcns) (foldTD fcns new_acc arg) (map #2 selections)
          in
	    case wildcard
	      of Some lamb => foldTD fcns acc' lamb
	       | None => acc'
          end
          
      in
	case lamb of
          VAR{instances, ...} => foldl g new_acc instances
        | INTEGER _ => new_acc
        | STRING _ => new_acc
        | REAL _ => new_acc
	| FN{pat,body} => foldTD fcns (foldl (foldType g) new_acc (map #2 pat)) body
	| LET{pat,bind,scope} => foldTD fcns (foldTD fcns (foldl (foldType g) new_acc (map #3 pat)) bind) scope
	| FIX{functions,scope} => foldTD fcns (foldl (foldTD fcns) (foldl (foldType g) new_acc (map #Type functions))  (map #bind functions)) scope
	| APP(lamb1, lamb2) => foldTD fcns (foldTD fcns new_acc lamb1) lamb2
	| EXCEPTION(excon,tauOpt,lamb) => 
             (case tauOpt of None => foldTD fcns new_acc lamb
                 | Some tau => foldTD fcns (g new_acc tau) lamb
             )
	| RAISE(lamb,taus) => foldTD fcns new_acc lamb
	| HANDLE(lamb1, lamb2) => foldTD fcns (foldTD fcns new_acc lamb1) lamb2
	| SWITCH_I switch => foldSwitch switch
	| SWITCH_S switch => foldSwitch switch
	| SWITCH_C switch => foldSwitch switch
	| SWITCH_E switch => foldSwitch switch
	| PRIM(prim,lambs) => foldl (foldTD fcns) new_acc lambs
        | FRAME _ => acc
      end
	
    and foldPrim (g: 'a -> Type -> 'a) (acc:'a) (prim:Type prim) : 'a =
      case prim of
        CONprim{instances,...} => foldl (foldType g) acc instances
      | DECONprim{instances,...} => foldl (foldType g) acc instances
      | DEREFprim{instance} => (foldType g) acc instance 
      | REFprim{instance} => (foldType g) acc instance 
      | ASSIGNprim{instance} => (foldType g) acc instance 
      | EQUALprim{instance} => (foldType g) acc instance 
      | NOTEQUALprim{instance} => (foldType g) acc instance 
      | CCALLprim(_,{instance}) => (foldType g) acc instance 
      | RESET_REGIONSprim{instance} => (foldType g) acc instance 
      | FORCE_RESET_REGIONSprim{instance} => (foldType g) acc instance 
      | _ => acc

   fun size (e: LambdaExp) = foldTD(fn n:int => fn exp => n+1, 
                                    fn n: int => fn tau => n) 
                                   0 e

   fun size_incl_types (e: LambdaExp) = foldTD(fn n:int => fn exp => n+1, 
                                              fn n: int => fn tau => n+1) 
                                       0 e
                                    

   (* prettyprinting. *)
    type StringTree = PP.StringTree

    fun layoutPrim layoutType prim = 
     case prim of
        CONprim{con,instances} => 
          if !Flags.print_types then
   		  PP.NODE{start= Con.pr_con con, finish="", 
		  indent=2,children=map layoutType instances,childsep=PP.RIGHT","}
          else PP.LEAF (Con.pr_con con)
      | DECONprim{con,instances} => 
          if !Flags.print_types then
		  PP.NODE{start= "decon(" ^ Con.pr_con con,finish=")", 
		  indent=2,children=map layoutType instances,childsep=PP.RIGHT","}
          else 
		  PP.NODE{start= "decon(" ^ Con.pr_con con,finish=")", 
		  indent=2,children=[],childsep=PP.NONE}
      | EXCONprim excon => 
	  PP.LEAF(Excon.pr_excon excon)
      | DEEXCONprim excon => 
	  PP.LEAF("deexcon" ^ Excon.pr_excon excon)
      | RECORDprim => PP.LEAF("record")
      | SELECTprim i => PP.LEAF("select(" ^ Int.string i ^ ")")
      | UB_RECORDprim => PP.LEAF("ubrecord") 
      | NOTprim => PP.LEAF("not")
      | NEG_INTprim => PP.LEAF("~" )
      | NEG_REALprim => PP.LEAF("~")
      | ABS_INTprim => PP.LEAF("abs")
      | ABS_REALprim => PP.LEAF("abs")
      | FLOORprim => PP.LEAF("floor")
      | REALprim => PP.LEAF("real")
      | SQRTprim => PP.LEAF("sqrt")
      | SINprim => PP.LEAF("sin")
      | COSprim => PP.LEAF("cos")
      | ARCTANprim => PP.LEAF("arctan")
      | EXPprim => PP.LEAF("exp")
      | LNprim => PP.LEAF("ln")
      | SIZEprim => PP.LEAF("size")
      | CHRprim => PP.LEAF("chr")
      | ORDprim => PP.LEAF("ord")
      | EXPLODEprim => PP.LEAF("explode")
      | IMPLODEprim => PP.LEAF("implode")
      | DEREFprim {instance} => 
          if !Flags.print_types then
	     PP.NODE{start="!(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NONE}
          else PP.LEAF "!"
      | REFprim {instance} => 
          if !Flags.print_types then
	     PP.NODE{start="ref(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NONE}
          else PP.LEAF" ref "
      | ASSIGNprim {instance} => 
          if !Flags.print_types then
	       PP.NODE{start=":=(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NONE}
          else PP.LEAF " := "
      | DIV_REALprim => PP.LEAF("/")
      | DIV_INTprim => PP.LEAF("div")
      | MODprim => PP.LEAF("mod")
      | MUL_REALprim => PP.LEAF("*")
      | MUL_INTprim => PP.LEAF("*")
      | PLUS_REALprim => PP.LEAF("+")
      | PLUS_INTprim => PP.LEAF("+")
      | MINUS_REALprim => PP.LEAF("-")
      | MINUS_INTprim => PP.LEAF("-")
      | STRING_CONCATprim => PP.LEAF("^" )
      | EQUALprim {instance} => 
          if !Flags.print_types then
	      PP.NODE{start="=(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NONE}
          else
	    PP.LEAF " = "
      | NOTEQUALprim {instance} => 
          if !Flags.print_types then
	      PP.NODE{start="<>(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NONE}
          else PP.LEAF " <> "
      | LESS_REALprim =>PP.LEAF("<")
      | LESS_INTprim =>PP.LEAF("<")
      | GREATER_REALprim => PP.LEAF(">")
      | GREATER_INTprim => PP.LEAF(">")
      | LESSEQ_REALprim => PP.LEAF("<=")
      | LESSEQ_INTprim => PP.LEAF("<=")
      | GREATEREQ_REALprim => PP.LEAF(">=")
      | GREATEREQ_INTprim => PP.LEAF(">=")
      | OPEN_INprim => PP.LEAF("open_in" )
      | OPEN_OUTprim => PP.LEAF("open_out")
      | INPUTprim => PP.LEAF("intput")
      | LOOKAHEADprim => PP.LEAF("lookahead")
      | CLOSE_INprim => PP.LEAF("close_in")
      | END_OF_STREAMprim => PP.LEAF("end_of_stream")
      | OUTPUTprim => PP.LEAF("output")
      | CLOSE_OUTprim => PP.LEAF("close_out")
      | USEprim => PP.LEAF("use" )
      | FLUSH_OUTprim => PP.LEAF("flush_out" )
      | STD_INprim => PP.LEAF("std_in" )
      | STD_OUTprim => PP.LEAF("std_out" )
      | CCALLprim (s,{instance}) => 
          if !Flags.print_types then
	      PP.NODE{start="ccall("^s,finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NONE}
          else PP.LEAF("ccall("^s^")" )
      | RESET_REGIONSprim {instance} => 
          if !Flags.print_types then
	      PP.NODE{start="resetRegions(", finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NONE}
          else PP.LEAF("resetRegions")
      | FORCE_RESET_REGIONSprim {instance} => 
          if !Flags.print_types then
	      PP.NODE{start="forceResetting(", finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NONE}
          else PP.LEAF("forceResetting")

    fun layoutSwitch layoutLambdaExp show_const (SWITCH(lamb,rules,wildcardOpt)) =
      let
	fun child(x,lamb) =
	  PP.NODE{start="",finish="",indent=0,
		  children=[PP.LEAF (show_const x),
			    layoutLambdaExp lamb],
		  childsep=PP.LEFT " => "}
      in
	PP.NODE{start="SWITCH(",finish=")",indent=3,
		children=[layoutLambdaExp lamb,
			  PP.NODE{start="",finish="",indent=0,
				  children=(map child rules) @ 
				  (case wildcardOpt of 
				     None => []
				   | Some lamb => 
				       [PP.NODE{start="",finish="",indent=0,
						children=[PP.LEAF "_",
							  layoutLambdaExp lamb],
						childsep=PP.LEFT " => "}]),
				     childsep=PP.LEFT " | "}],
		childsep = PP.RIGHT " of "} 
      end


    fun layoutPgm (PGM(DATBINDS dblist,lamb)) =
      let
	val layoutcb =
	  map (fn (con,tauopt) =>
	       PP.NODE{start="",finish="",indent=0, childsep=PP.LEFT " of ",
		       children=PP.LEAF (Con.pr_con con) :: layoutTypeOpt tauopt})
				     
	fun layoutdb (tyvars,tyname,cb) =
	  let
	    val tyvars_tynameT = 
	      case layoutTyvarseq tyvars
		of Some t => PP.NODE {start="",finish="",childsep=PP.RIGHT " ",indent=0,
				      children=[t, PP.LEAF(TyName.pr_TyName tyname)]}
		 | None => PP.LEAF(TyName.pr_TyName tyname)
	    val cbT = PP.NODE{start="",finish="",indent=0,
			      children=layoutcb cb,
			      childsep=PP.LEFT" | "}
	  in
	    PP.NODE{start="",finish="",indent=0,
		    children=[tyvars_tynameT,PP.LEAF " = ",cbT],
		    childsep=PP.NONE}
	  end
	  
	fun layoutMutualRec_db db =
	  PP.NODE{start="DATATYPE [",finish="] ",indent=3,
		  children=map layoutdb db,childsep=PP.LEFT" and "}

	val dbTs = map layoutMutualRec_db dblist

	val lambT = layoutLambdaExp lamb
      in
	PP.NODE{start="",finish="",indent=0,
		children=dbTs @ [lambT],childsep=PP.NONE}
      end

    and layoutLambdaExp lamb: StringTree =
      case lamb of 
        VAR {lvar=lv,instances=taus} => 
	  PP.NODE{start=Lvars.pr_lvar lv ^ ":(", finish=")",indent=0,
		  children=map layoutType taus,
		  childsep=PP.RIGHT ","}
      | INTEGER i => PP.LEAF(Int.string i)
      | STRING s => PP.LEAF(String.string s)
      | REAL r => PP.LEAF(Real.string r)
      | FN {pat,body} => 
	  PP.NODE{start="FN ",finish="", indent=1,
		  children=[layoutFnPat pat,
			    layoutLambdaExp body], 
		  childsep=PP.RIGHT "=> "
		  }
      | LET {pat,bind,scope} => 
	  let 
	    val binding = PP.NODE{start="", finish="",
				  indent=0, childsep=PP.RIGHT " = ",
				  children=[layoutLetPat pat,
					    layoutLambdaExp bind]}
	  in
	    PP.NODE{start="LET ",finish=" END ",indent=4,
		    children=[binding,layoutLambdaExp scope],
		    childsep=PP.LEFT " IN "}
	  end
      | FIX{functions, scope} =>
	  let
	    fun layoutFunction {lvar, tyvars, Type, bind} =
	      PP.NODE{start=Lvars.pr_lvar lvar ^ ": ", finish="", indent=3,
		      children=[layoutTypeScheme(tyvars,Type),
				layoutLambdaExp bind], childsep=PP.RIGHT " = "}
	  in
	    PP.NODE{start="FIX ", finish=" END ", indent=4,
		    childsep=PP.LEFT " IN ",
		    children=[PP.NODE{start="", finish="", indent=0,
				      children=map layoutFunction functions,
				      childsep=PP.RIGHT ", "
				      },
			      layoutLambdaExp scope]
		    }
	  end
      | APP(lamb1, lamb2) =>
	  PP.NODE{start="APP(", finish=")", indent=3,
		  children=[layoutLambdaExp lamb1, layoutLambdaExp lamb2],
		  childsep=PP.RIGHT ", "
		  }
      | EXCEPTION(excon,None,lamb) =>
	  PP.NODE{start="EXCEPTION " ^ Excon.pr_excon excon ^ " in",finish="",
		  indent=0, children=[layoutLambdaExp lamb],
		  childsep=PP.NONE}
      | EXCEPTION(excon,Some tau,lamb) =>
	  PP.NODE{start="",finish="", indent=0,
		  children=[PP.NODE{start="EXCEPTION " ^ Excon.pr_excon excon ^ ": ",
				    finish="",indent=0,
				    children=[layoutType tau],
				    childsep=PP.NONE},
			    layoutLambdaExp lamb], 
		  childsep=PP.RIGHT " in "
		  }
      | RAISE(lamb,typelist) =>
	  PP.NODE{start="RAISE(", finish=")", indent=3,
		  children=[layoutLambdaExp lamb,
			    layoutTypeList typelist], childsep=PP.RIGHT ","
		  }

      | HANDLE(lamb1, lamb2) =>
	  PP.NODE{start="HANDLE(", finish=")", indent=3,
		  children=[layoutLambdaExp lamb1, layoutLambdaExp lamb2],
		  childsep=PP.LEFT " with "
		  }
      | SWITCH_I sw => 
	  layoutSwitch layoutLambdaExp Int.string sw
      | SWITCH_S sw => 
	  layoutSwitch layoutLambdaExp (fn x => x) sw
      | SWITCH_C sw => 
	  layoutSwitch layoutLambdaExp Con.pr_con sw
      | SWITCH_E sw => 
	  layoutSwitch layoutLambdaExp Excon.pr_excon sw
      | PRIM(prim,lambs) =>
	  PP.NODE{start="PRIM(",finish=")",indent=3,
		  children=[layoutPrim layoutType prim,
			    PP.NODE{start="[",finish="]",indent=1,
				    children=map layoutLambdaExp lambs,
				    childsep=PP.RIGHT ","}],
		  childsep=PP.RIGHT ", "}
      | FRAME fr => layoutFrame "FRAME" fr

    and layoutFrame str {declared_lvars, declared_excons} =
      let
	fun lvar_child({lvar,tyvars,Type}) =
	  PP.NODE{start=Lvars.pr_lvar lvar ^ ": ", finish="", indent=3,
		  children=[layoutTypeScheme(tyvars,Type)],
		  childsep=PP.NONE
		  }
	  
	val lvars_children = map lvar_child  declared_lvars
	fun excon_child(excon, ty_opt) =
	  let val (connect,type_tree) = 
	    case ty_opt of
	      None => ( "", PP.LEAF "")
	    | Some tau => (" of ", layoutType tau)
	  in
	    PP.NODE{start=Excon.pr_excon excon  ^ connect, finish="", indent=3,
		    children=[type_tree],
		    childsep=PP.NONE
		             }	  
	  end
	val excons_children = map excon_child  declared_excons
	  
      in
	PP.NODE{start = str ^ "(", finish = ")", indent = 6,
		children = excons_children @ lvars_children,
		childsep = PP.RIGHT ", "}
      end

    and layoutTyvarseq tyvars =
      case tyvars
	of nil => None
	 | [tv] => Some(PP.LEAF (pr_tyvar tv))
	 | tvs => Some(PP.NODE{start="(", finish=")", indent=1,
			       children=map (PP.LEAF o pr_tyvar) tvs,
			       childsep=PP.RIGHT ", "
			      }
		      )

       
    and layoutType tau =
       case tau of
	 TYVARtype tv => PP.LEAF (pr_tyvar tv)
       | ARROWtype(taus,taus') => 
	   PP.NODE{start="(",finish=")",indent=1,
		   children=[layoutTypes taus,layoutTypes taus'],
		   childsep=PP.LEFT "->"}
       | CONStype(taus,tyname) =>
	   (case layoutTypeseq taus of
	      None => PP.LEAF (TyName.pr_TyName tyname)
	    | Some x => PP.NODE{start="",finish=" " ^ TyName.pr_TyName tyname,indent=1,
			       children=[x],childsep=PP.NONE})
       | RECORDtype taus => 
	   (case taus of 
	      [] (* unit *) => PP.LEAF "{}"
	    | _ => PP.NODE{start="(",finish=")",indent=1,
			   children=map layoutType taus,
			   childsep=PP.RIGHT"*"})

    and layoutTypeseq taus =
      case taus of 
	[] => None
      | [tau] => Some(layoutType tau)
      | taus => Some(PP.NODE{start="(",finish=")",indent=1,
			     children=map layoutType taus,
			     childsep=PP.LEFT", "})

    and layoutTypes taus = PP.NODE {start="<", finish=">", childsep=PP.LEFT ", ", indent=0,
				    children = map layoutType taus}

    and layoutTypeList tl =
      (case tl
	 of Types taus => PP.NODE{start="Types(", finish=")", indent=1,
				  children = [layoutTypes taus], childsep=PP.NONE}
	  | Frame fr => layoutFrame "Frame" fr
	  | RaisedExnBind => PP.LEAF "RaisedExnBind")
 
    and layoutTypeScheme(tyvars,tau) =
      let 
	val tyvarsT = layoutTyvarseq tyvars
	val tauT = layoutType tau
      in
	case tyvarsT of
	  None => tauT
	| Some T => 
	    PP.NODE{start="FORALL",finish="",indent=1,
		    children=[T,tauT],
		    childsep=PP.RIGHT"."}
      end

    and layoutTypeOpt (Some tau) = [layoutType tau]
      | layoutTypeOpt None = []


    and layoutFnPat atpats =
	PP.NODE {start="<", finish=">", indent=0, children=map layoutFnAtPat atpats,
		 childsep=PP.RIGHT ","}

    and layoutLetPat atpats =
	PP.NODE {start="<", finish=">", indent=0, children=map layoutLetAtPat atpats,
		 childsep=PP.RIGHT ","}

    and layoutFnAtPat (lvar, Type) =
      PP.NODE {start=Lvars.pr_lvar lvar ^ ":", finish="", indent=0,
	       children=[layoutType Type], childsep=PP.NONE}

    and layoutLetAtPat (lvar, tyvars, Type) =
      PP.NODE {start=Lvars.pr_lvar lvar ^ ":", finish="", indent=0,
	       children=[layoutTypeScheme(tyvars,Type)], childsep=PP.NONE}


    val layoutLambdaPgm = layoutPgm
  end;
