
functor LambdaExp(structure Lvars: LVARS
		  structure Con: CON
		  structure Excon: EXCON
		  structure TyName: TYNAME
		  structure PP: PRETTYPRINT  
		  structure Crash: CRASH
		  structure Flags: FLAGS) : LAMBDA_EXP =
  struct

    structure EqSet = Edlib.EqSet
    structure Int = Edlib.Int
    structure List = Edlib.List
    structure String = Edlib.String

    type lvar = Lvars.lvar
    type con = Con.con
    type excon = Excon.excon
    type TyName = TyName.TyName

    fun die s = Crash.impossible ("LambdaExp." ^ s)

    type tyvar = word
    fun equality_tyvar tyvar = Word.mod(tyvar, 0w2) = 0w1
    fun pr_tyvar tyvar = (if equality_tyvar tyvar then "''a" else "'a") ^ Word.toString tyvar
    val lt_tyvar : tyvar * tyvar -> bool = op <
    local 
      val init_tyvar_no = ref 0w0
      val init_eqtyvar_no = ref 0w1
      val tyvar_no = ref (!init_tyvar_no)
      val eqtyvar_no = ref (!init_eqtyvar_no)
      fun incr2 c = let val n = !c in c := !c + 0w2; n end
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

    fun foldl' (g: 'a -> 'b -> 'a) (acc: 'a) [] = acc
      | foldl' g acc (x::xs) = foldl' g (g acc x) xs

    fun foldType (g : 'a -> Type -> 'a) (acc: 'a) (tau : Type) : 'a =
      case tau of
        TYVARtype _ => g acc tau
      | ARROWtype(taus1,taus2) => g (foldTypes g (foldTypes g acc taus2) taus1 ) tau
      | CONStype(taus,_) => g(foldTypes g acc taus)tau
      | RECORDtype(taus) => g(foldTypes g acc taus)tau
    and foldTypes g acc taus = foldl' (foldType g) acc taus
        
    fun size_type tau = foldType (fn n:int => fn _ => n+1)

    val intType = CONStype([], TyName.tyName_INT)
    val boolType = CONStype([], TyName.tyName_BOOL)
    val exnType = CONStype([], TyName.tyName_EXN)
    val realType = CONStype([], TyName.tyName_REAL)
    val stringType = CONStype([], TyName.tyName_STRING)
    val unitType = RECORDtype([])

    val tyvars = foldType (fn tyvarset =>
			   (fn TYVARtype tyvar => EqSet.insert tyvar tyvarset
			     | _ => tyvarset)) EqSet.empty

    datatype TypeList =                               (* To allow the result of a declaration *)  
        Types of Type list                            (* to be a raised Bind exception. *)
      | Frame of {declared_lvars: {lvar : lvar, tyvars: tyvar list, Type: Type} list,
		  declared_excons: (excon * Type option) list}
      | RaisedExnBind

    datatype 'Type prim =                             (* The primitives are always fully applied ! *)
        CONprim of {con : con, instances : 'Type list}
      | DECONprim of {con : con, instances : 'Type list}
      | EXCONprim of excon
      | DEEXCONprim of excon
      | RECORDprim 
      | SELECTprim of int        
      | UB_RECORDprim                                 (* Unboxed record. *)
      | NEG_INTprim 
      | NEG_REALprim
      | ABS_INTprim
      | ABS_REALprim
      | DEREFprim of {instance: 'Type}
      | REFprim of {instance: 'Type}
      | ASSIGNprim of {instance: 'Type}
      | MUL_REALprim
      | MUL_INTprim
      | PLUS_REALprim
      | PLUS_INTprim
      | MINUS_REALprim
      | MINUS_INTprim
      | EQUALprim of {instance: 'Type}
      | EQUAL_INTprim
      | LESS_REALprim
      | LESS_INTprim
      | GREATER_REALprim
      | GREATER_INTprim
      | LESSEQ_REALprim
      | LESSEQ_INTprim
      | GREATEREQ_REALprim
      | GREATEREQ_INTprim
      | CCALLprim of {name : string,                  (* NOT Standard ML *)
		      instances : 'Type list,
		      tyvars : tyvar list,
		      Type : 'Type} 
      | RESET_REGIONSprim of {instance: 'Type}        (* NOT Standard ML, for programmer-directed, but safe, resetting of
						       * regions *)
      | FORCE_RESET_REGIONSprim of {instance: 'Type}  (* NOT Standard ML, for programmer-controlled, unsafe resetting of
						       * regions *)

    datatype LambdaPgm = PGM of datbinds * LambdaExp

    and datbinds = DATBINDS of (tyvar list * TyName * (con * Type option) list) list list
      (* list of mutual recursive datatype declarations *)

    and LambdaExp =
        VAR      of {lvar: lvar, instances : Type list}
      | INTEGER  of int			
      | STRING   of string
      | REAL     of string
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
      | EXCEPTION of excon * Type option * LambdaExp
      | RAISE    of LambdaExp * TypeList
      | HANDLE   of LambdaExp * LambdaExp
      | SWITCH_I of int Switch
      | SWITCH_S of string Switch
      | SWITCH_C of con Switch
      | SWITCH_E of excon Switch
      | PRIM     of Type prim * LambdaExp list
      | FRAME    of {declared_lvars: {lvar : lvar, tyvars: tyvar list, Type: Type} list,
                     declared_excons: (excon * Type option) list}
                       (* a frame is the result of a structure-level
                        * declaration. 
			*)

    and 'a Switch = SWITCH of LambdaExp * ('a * LambdaExp) list * LambdaExp option

      
    fun foldTD (fcns as (f:'a->LambdaExp->'a, g: 'a -> Type -> 'a))
               (acc:'a) (lamb:LambdaExp) =
      let
	val new_acc = f acc lamb
	
	fun foldSwitch (SWITCH(arg, selections, wildcard)) =
	  let
	    val acc' = foldl' (foldTD fcns) (foldTD fcns new_acc arg) (map #2 selections)
          in
	    case wildcard
	      of SOME lamb => foldTD fcns acc' lamb
	       | NONE => acc'
          end
          
      in
	case lamb of
          VAR{instances, ...} => foldl' g new_acc instances
        | INTEGER _ => new_acc
        | STRING _ => new_acc
        | REAL _ => new_acc
	| FN{pat,body} => foldTD fcns (foldl' (foldType g) new_acc (map #2 pat)) body
	| LET{pat,bind,scope} => foldTD fcns (foldTD fcns (foldl' (foldType g) new_acc (map #3 pat)) bind) scope
	| FIX{functions,scope} => foldTD fcns (foldl' (foldTD fcns) (foldl' (foldType g) new_acc (map #Type functions))  (map #bind functions)) scope
	| APP(lamb1, lamb2) => foldTD fcns (foldTD fcns new_acc lamb1) lamb2
	| EXCEPTION(excon,tauOpt,lamb) => 
             (case tauOpt of NONE => foldTD fcns new_acc lamb
                 | SOME tau => foldTD fcns (g new_acc tau) lamb
             )
	| RAISE(lamb,taus) => foldTD fcns new_acc lamb
	| HANDLE(lamb1, lamb2) => foldTD fcns (foldTD fcns new_acc lamb1) lamb2
	| SWITCH_I switch => foldSwitch switch
	| SWITCH_S switch => foldSwitch switch
	| SWITCH_C switch => foldSwitch switch
	| SWITCH_E switch => foldSwitch switch
	| PRIM(prim,lambs) => foldl' (foldTD fcns) new_acc lambs
        | FRAME _ => acc
      end
	
    and foldPrim (g: 'a -> Type -> 'a) (acc:'a) (prim:Type prim) : 'a =
      case prim of
        CONprim{instances,...} => foldl' (foldType g) acc instances
      | DECONprim{instances,...} => foldl' (foldType g) acc instances
      | DEREFprim{instance} => (foldType g) acc instance 
      | REFprim{instance} => (foldType g) acc instance 
      | ASSIGNprim{instance} => (foldType g) acc instance 
      | EQUALprim{instance} => (foldType g) acc instance 
      | CCALLprim {instances, ...} => foldl' (foldType g) acc instances
      | RESET_REGIONSprim{instance} => (foldType g) acc instance 
      | FORCE_RESET_REGIONSprim{instance} => (foldType g) acc instance 
      | _ => acc

   fun size (e: LambdaExp) = foldTD(fn n:int => fn exp => n+1, 
                                    fn n: int => fn tau => n) 
                                   0 e

   fun size_incl_types (e: LambdaExp) = foldTD(fn n:int => fn exp => n+1, 
                                              fn n: int => fn tau => n+1) 
                                       0 e

   (* safeLambdaPgm: This predicate approximates whether a lambda
    * program performs side effects; it is used to determine if a
    * program unit can be discharged at link time in case it is not
    * used. *)

   local
     fun safe_prim prim =
       case prim
	 of CONprim _ => true
	  | DECONprim _ => true
	  | EXCONprim _ => true
	  | DEEXCONprim _ => true
	  | RECORDprim => true
	  | SELECTprim _ => true       
	  | UB_RECORDprim => true
	  | NEG_INTprim => false
	  | NEG_REALprim => true
	  | ABS_INTprim => false
	  | ABS_REALprim => true
	  | DEREFprim _ => true
	  | REFprim _ => true
	  | ASSIGNprim _ => false
	  | MUL_REALprim => true
	  | MUL_INTprim => false
	  | PLUS_REALprim => true
	  | PLUS_INTprim => false
	  | MINUS_REALprim => true
	  | MINUS_INTprim => false
	  | EQUALprim _ => true
	  | EQUAL_INTprim => true
	  | LESS_REALprim => true
	  | LESS_INTprim => true
	  | GREATER_REALprim => true
	  | GREATER_INTprim => true
	  | LESSEQ_REALprim => true
	  | LESSEQ_INTprim => true
	  | GREATEREQ_REALprim => true
	  | GREATEREQ_INTprim => true
	  | CCALLprim _ => false
	  | RESET_REGIONSprim _ => false
	  | FORCE_RESET_REGIONSprim _ => false

     fun safe_sw safe_exp (SWITCH(exp,sel,opt)) =
       foldl (fn ((_,exp), acc) => acc andalso safe_exp exp)
       (safe_exp exp andalso 
	case opt
	  of SOME exp => safe_exp exp
	   | NONE => true) 
       sel

     fun safe_exp exp =
       case exp
	 of VAR _ => true
	  | INTEGER _ => true			
	  | STRING _ => true
	  | REAL _ => true
	  | FN _ => true
	  | LET {bind, scope, ...} => safe_exp bind andalso safe_exp scope
	  | FIX {scope,...} => safe_exp scope
	  | APP _ => false
	  | EXCEPTION (_,_,exp) => safe_exp exp
	  | RAISE _ => false
	  | HANDLE (exp,_) => safe_exp exp
	  | SWITCH_I sw => safe_sw safe_exp sw
	  | SWITCH_S sw => safe_sw safe_exp sw
	  | SWITCH_C sw => safe_sw safe_exp sw
	  | SWITCH_E sw => safe_sw safe_exp sw
	  | PRIM (prim,exps) => foldl (fn (exp,acc) => acc andalso safe_exp exp) (safe_prim prim) exps
	  | FRAME _ => true

   in (* local *)

     fun safeLambdaPgm(PGM(_,exp)) = safe_exp exp

   end (* local *)

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
		  indent=2,children=[],childsep=PP.NOSEP}
      | EXCONprim excon => 
	  PP.LEAF(Excon.pr_excon excon)
      | DEEXCONprim excon => 
	  PP.LEAF("deexcon" ^ Excon.pr_excon excon)
      | RECORDprim => PP.LEAF("record")
      | SELECTprim i => PP.LEAF("select(" ^ Int.string i ^ ")")
      | UB_RECORDprim => PP.LEAF("ubrecord") 
      | NEG_INTprim => PP.LEAF("~" )
      | NEG_REALprim => PP.LEAF("~")
      | ABS_INTprim => PP.LEAF("abs")
      | ABS_REALprim => PP.LEAF("abs")
      | DEREFprim {instance} => 
          if !Flags.print_types then
	     PP.NODE{start="!(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NOSEP}
          else PP.LEAF "!"
      | REFprim {instance} => 
          if !Flags.print_types then
	     PP.NODE{start="ref(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NOSEP}
          else PP.LEAF" ref "
      | ASSIGNprim {instance} => 
          if !Flags.print_types then
	       PP.NODE{start=":=(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NOSEP}
          else PP.LEAF " := "
      | MUL_REALprim => PP.LEAF("*")
      | MUL_INTprim => PP.LEAF("*")
      | PLUS_REALprim => PP.LEAF("+")
      | PLUS_INTprim => PP.LEAF("+")
      | MINUS_REALprim => PP.LEAF("-")
      | MINUS_INTprim => PP.LEAF("-")
      | EQUAL_INTprim => 
          if !Flags.print_types then
            PP.LEAF("=[int]")
          else
            PP.LEAF("=")
      | EQUALprim {instance} => 
          if !Flags.print_types then
	      PP.NODE{start="=(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NOSEP}
          else
	    PP.LEAF " = "
      | LESS_REALprim =>PP.LEAF("<")
      | LESS_INTprim =>PP.LEAF("<")
      | GREATER_REALprim => PP.LEAF(">")
      | GREATER_INTprim => PP.LEAF(">")
      | LESSEQ_REALprim => PP.LEAF("<=")
      | LESSEQ_INTprim => PP.LEAF("<=")
      | GREATEREQ_REALprim => PP.LEAF(">=")
      | GREATEREQ_INTprim => PP.LEAF(">=")
      | CCALLprim {name, instances, tyvars, Type} => 
          if !Flags.print_types then
	      PP.NODE {start="ccall (" ^ name ^ " ", finish=")", indent=2,
		       children=map layoutType instances, childsep=PP.LEFT ", "}
          else PP.LEAF ("ccall " ^ name)
      | RESET_REGIONSprim {instance} => 
          if !Flags.print_types then
	      PP.NODE{start="resetRegions(", finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NOSEP}
          else PP.LEAF("resetRegions")
      | FORCE_RESET_REGIONSprim {instance} => 
          if !Flags.print_types then
	      PP.NODE{start="forceResetting(", finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NOSEP}
          else PP.LEAF("forceResetting")

    fun layoutSwitch layoutLambdaExp show_const (SWITCH(lamb,rules,wildcardOpt)) =
      let
	fun child(x,lamb) =
	  PP.NODE{start="",finish="",indent=0,
		  children=[PP.LEAF (show_const x),
			    layoutLambdaExp(lamb,0)],
		  childsep=PP.LEFT " => "}
      in
	PP.NODE{start="(case ",finish=")",indent=3,
		children=[layoutLambdaExp(lamb,0),
			  PP.NODE{start="",finish="",indent=0,
				  children=(map child rules) @ 
				  (case wildcardOpt of 
				     NONE => []
				   | SOME lamb => 
				       [PP.NODE{start="",finish="",indent=0,
						children=[PP.LEAF "_",
							  layoutLambdaExp(lamb,0)],
						childsep=PP.LEFT " => "}]),
				     childsep=PP.LEFT " | "}],
		childsep = PP.RIGHT " of "} 
      end


   fun parenthesise(st: PP.StringTree): PP.StringTree=
     PP.NODE{start = "(", finish= ")", indent=1, children = [st], childsep = PP.NOSEP}


   fun layoutTyvarseq tyvars =
      case tyvars
	of nil => NONE
	 | [tv] => SOME(PP.LEAF (pr_tyvar tv))
	 | tvs => SOME(PP.NODE{start="(", finish=")", indent=1,
			       children=map (PP.LEAF o pr_tyvar) tvs,
			       childsep=PP.RIGHT ", "
			      }
		      )
       
   fun layoutType tau =
       case tau of
	 TYVARtype tv => PP.LEAF (pr_tyvar tv)
       | ARROWtype(taus,taus') => 
	   PP.NODE{start="(",finish=")",indent=1,
		   children=[layoutTypes taus,layoutTypes taus'],
		   childsep=PP.LEFT "->"}
       | CONStype(taus,tyname) =>
	   (case layoutTypeseq taus of
	      NONE => PP.LEAF (TyName.pr_TyName tyname)
	    | SOME x => PP.NODE{start="",finish=" " ^ TyName.pr_TyName tyname,indent=1,
			       children=[x],childsep=PP.NOSEP})
       | RECORDtype taus => 
	   (case taus of 
	      [] (* unit *) => PP.LEAF "{}"
	    | _ => PP.NODE{start="(",finish=")",indent=1,
			   children=map layoutType taus,
			   childsep=PP.RIGHT"*"})

    and layoutTypeseq taus =
      case taus of 
	[] => NONE
      | [tau] => SOME(layoutType tau)
      | taus => SOME(PP.NODE{start="(",finish=")",indent=1,
			     children=map layoutType taus,
			     childsep=PP.LEFT", "})

    and layoutTypes taus = PP.NODE {start="<", finish=">", childsep=PP.LEFT ", ", indent=0,
				    children = map layoutType taus}

    and layoutTypeList tl =
      (case tl
	 of Types taus => PP.NODE{start="Types(", finish=")", indent=1,
				  children = [layoutTypes taus], childsep=PP.NOSEP}
	  | Frame fr => layoutFrame "Frame" fr
	  | RaisedExnBind => PP.LEAF "RaisedExnBind")
 
    and layoutTypeOpt (SOME tau) = [layoutType tau]
      | layoutTypeOpt NONE = []


    and layoutFrame str {declared_lvars, declared_excons} =
      let
	fun lvar_child({lvar,tyvars,Type}) =
	  PP.NODE{start=Lvars.pr_lvar lvar ^ ": ", finish="", indent=3,
		  children=[layoutTypeScheme(tyvars,Type)],
		  childsep=PP.NOSEP
		  }
	  
	val lvars_children = map lvar_child  declared_lvars
	fun excon_child(excon, ty_opt) =
	  let val (connect,type_tree) = 
	    case ty_opt of
	      NONE => ( "", PP.LEAF "")
	    | SOME tau => (" of ", layoutType tau)
	  in
	    PP.NODE{start=Excon.pr_excon excon  ^ connect, finish="", indent=3,
		    children=[type_tree],
		    childsep=PP.NOSEP
		             }	  
	  end
	val excons_children = map excon_child  declared_excons
	  
      in
	PP.NODE{start = str ^ "(", finish = ")", indent = 6,
		children = excons_children @ lvars_children,
		childsep = PP.RIGHT ", "}
      end

  and layoutTypeScheme(tyvars,tau) =
      let 
	val tyvarsT = layoutTyvarseq tyvars
	val tauT = layoutType tau
      in
	case tyvarsT of
	  NONE => tauT
	| SOME T => 
	    PP.NODE{start="FORALL",finish="",indent=1,
		    children=[T,tauT],
		    childsep=PP.RIGHT"."}
      end

   fun layVarSigma(lvar,alphas,tau) =
     if !Flags.print_types 
       then
         let val sigma_t = layoutTypeScheme(alphas, tau)
             val start:string = Lvars.pr_lvar lvar ^ " :" 
         in PP.NODE{start = start, finish = "", indent = String.size start +1,
                    childsep = PP.NOSEP, children = [sigma_t]}
         end
     else PP.LEAF(Lvars.pr_lvar lvar)

   fun layPatLet [] = PP.LEAF("()")
     | layPatLet [one as (lvar,tyvars,tau)] = 
           layVarSigma(lvar,tyvars,tau)
     | layPatLet pat = PP.HNODE{start = "(", finish = ")", childsep = PP.RIGHT",", 
                                children = map (fn (lvar,tyvars,tau) => 
                                              layVarSigma(lvar,tyvars,tau)) pat}


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
		of SOME t => PP.NODE {start="",finish="",childsep=PP.RIGHT " ",indent=0,
				      children=[t, PP.LEAF(TyName.pr_TyName tyname)]}
		 | NONE => PP.LEAF(TyName.pr_TyName tyname)
	    val cbT = PP.NODE{start="",finish="",indent=0,
			      children=layoutcb cb,
			      childsep=PP.LEFT" | "}
	  in
	    PP.NODE{start="",finish="",indent=0,
		    children=[tyvars_tynameT,PP.LEAF " = ",cbT],
		    childsep=PP.NOSEP}
	  end
	  
	fun layoutMutualRec_db db =
	  PP.NODE{start="datatype ",finish="",indent=3,
		  children=map layoutdb db,childsep=PP.LEFT" and "}

	val dbTs = map layoutMutualRec_db dblist

	val lambT = layoutLambdaExp(lamb,0)
      in
	PP.NODE{start="",finish="",indent=0,
		children=dbTs @ [lambT],childsep=PP.NOSEP}
      end

    and layout_infix (context:int) (precedence: int) (operator: string) expressions = 
        if context > precedence then
          PP.NODE{start = "(", finish= ")", indent = 1, childsep = PP.RIGHT operator,
                  children = map (fn e => layoutLambdaExp(e,0)) expressions}
        else
          PP.NODE{start = "", finish= "", indent =0, childsep = PP.RIGHT operator,
                  children = map (fn e => layoutLambdaExp(e,0)) expressions}

    and layoutLambdaExp(lamb,context:int): StringTree =
      case lamb of 
        VAR {lvar=lv,instances=taus} => 
          if !Flags.print_types then 
            PP.NODE{start=Lvars.pr_lvar lv ^ ":(", finish=")",indent=0,
                    children=map layoutType taus,
                    childsep=PP.RIGHT ","}
          else PP.LEAF(Lvars.pr_lvar lv)
      | INTEGER i => PP.LEAF(Int.string i)
      | STRING s => PP.LEAF(String.string s)
      | REAL r => PP.LEAF(r)
      | FN {pat,body} => 
	  PP.NODE{start="(fn ",finish=")", indent=1,
		  children=[layoutFnPat pat,
			    layoutLambdaExp(body,0)], 
		  childsep=PP.RIGHT "=> "
		  }
      | LET {pat,bind,scope} => layout_let_fix_and_exception lamb
      | FIX{functions, scope} =>
	  let
	    fun layoutFunction {lvar, tyvars, Type, bind} =
              if !Flags.print_types then
                PP.NODE{start=Lvars.pr_lvar lvar ^ ": " , finish="", indent=3,
                        children=[layoutTypeScheme(tyvars,Type),
                                  layoutLambdaExp(bind,0)], childsep=PP.RIGHT " = "}
              else
                PP.NODE{start=Lvars.pr_lvar lvar ^ " = ", finish="", indent=3,
                        children=[layoutLambdaExp(bind,0)], childsep=PP.NOSEP}
	  in
	    PP.NODE{start="fix ", finish=" end ", indent=4,
		    childsep=PP.LEFT " in ",
		    children=[PP.NODE{start="", finish="", indent=0,
				      children=map layoutFunction functions,
				      childsep=PP.RIGHT ", "
				      },
			      layoutLambdaExp(scope,0)]
		    }
	  end
      | APP(lamb1, lamb2) =>
	  PP.NODE{start= if context>13 then "(" else "", 
                  finish=if context>13 then ")" else "", 
		  childsep=PP.RIGHT " ",
                  indent=1,
		  children=[layoutLambdaExp(lamb1,13), layoutLambdaExp(lamb2,14)]
		  }
      | EXCEPTION(excon,NONE,lamb) =>
	  PP.NODE{start="exception " ^ Excon.pr_excon excon ^ " in",finish="",
		  indent=0, children=[layoutLambdaExp(lamb,0)],
		  childsep=PP.NOSEP}
      | EXCEPTION(excon,SOME tau,lamb) =>
	  PP.NODE{start="",finish="", indent=0,
		  children=[PP.NODE{start="exception  " ^ Excon.pr_excon excon ^ ": ",
				    finish="",indent=0,
				    children=[layoutType tau],
				    childsep=PP.NOSEP},
			    layoutLambdaExp(lamb,0)], 
		  childsep=PP.RIGHT " in "
		  }
      | RAISE(lamb,typelist) =>
	  PP.NODE{start=if context>=13 then "raise(" else "raise",
                  finish=if context>=13 then ")" else "", 
                  indent=6,
		  children=[layoutLambdaExp(lamb,12),
			    layoutTypeList typelist], childsep=PP.RIGHT ","
		  }

      | HANDLE(lamb1, lamb2) =>
          PP.NODE{start=if context>=12 then "(" else "", 
                  finish=if context>=12 then ")" else "", 
                  indent=3,
		  children=[layoutLambdaExp(lamb1,12), layoutLambdaExp(lamb2,12)],
		  childsep=PP.LEFT " handle "
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
         (case (prim,lambs) of
           (RECORDprim,_) =>
             PP.NODE{start="(",finish=")",indent=1,
                     children=(map (fn e => layoutLambdaExp(e,0))  lambs),
                     childsep=PP.RIGHT ","}
         | (UB_RECORDprim,_) =>
             PP.NODE{start="<",finish=">",indent=1,
                     children=(map (fn e => layoutLambdaExp(e,0)) lambs),
                     childsep=PP.RIGHT ","}
         | (SELECTprim i, [lamb]) => 
             PP.NODE{start="#" ^ Int.string i ^ "(",finish=")",indent=1,
                     children=[layoutLambdaExp(lamb,0)],
                     childsep=PP.NOSEP}
         | (DEREFprim{instance},[lamb]) =>
             PP.NODE{start="!(",finish=")",indent=2,
                     children=[layoutLambdaExp(lamb,0)],
                     childsep=PP.NOSEP}
         | (REFprim{instance},[lamb]) =>
             PP.NODE{start="ref(",finish=")",indent=2,
                     children=[layoutLambdaExp(lamb,0)],
                     childsep=PP.NOSEP}
         | (ASSIGNprim{instance},_) => layout_infix context 3 " := "lambs
         | (MUL_REALprim, [_,_]) => layout_infix context 7 " * " lambs
         | (MUL_INTprim, [_,_]) =>  layout_infix context 7 " * " lambs
         | (PLUS_REALprim, [_,_]) => layout_infix context 6 " + " lambs
         | (PLUS_INTprim, [_,_]) =>  layout_infix context 6 " + " lambs
         | (MINUS_REALprim, [_,_]) => layout_infix context 6 " - "lambs
         | (MINUS_INTprim, [_,_]) =>  layout_infix context 6 " - "lambs
         | (EQUALprim{instance},[_,_]) => layout_infix context 4 " = "lambs
         | (EQUAL_INTprim, [_,_]) => layout_infix context 4 " = "lambs
         | (LESS_REALprim, [_,_]) => layout_infix context 4 " < "lambs
         | (LESS_INTprim, [_,_]) => layout_infix context 4 " < "lambs
         | (GREATER_REALprim, [_,_]) => layout_infix context 4 " > "lambs
         | (GREATER_INTprim, [_,_]) => layout_infix context 4 " > "lambs
         | (LESSEQ_REALprim, [_,_]) => layout_infix context 4 " <= "lambs
         | (LESSEQ_INTprim, [_,_]) => layout_infix context 4 " <= "lambs
         | (GREATEREQ_REALprim, [_,_]) => layout_infix context 4 " >= "lambs
         | (GREATEREQ_INTprim, [_,_]) => layout_infix context 4 " >= "lambs
         | _ => 
  	  PP.NODE{start="PRIM(",finish=")",indent=3,
		  children=[layoutPrim layoutType prim,
			    PP.NODE{start="[",finish="]",indent=1,
				    children=map(fn x => layoutLambdaExp(x,0)) lambs,
				    childsep=PP.RIGHT ","}],
		  childsep=PP.RIGHT ", "}
        )
      | FRAME fr => layoutFrame "FRAME" fr

    and layout_let_fix_and_exception lexp = 
          let 
            val inInfo = ref ""
            fun layout_rec lexp = 
                  case lexp of 
                    LET{pat, bind, scope} =>
                        let
                          val (binds, body) = layout_rec scope
                          val _ = inInfo := "(* let *)"
                        in
                           (mk_valbind(pat,bind)::binds, body)
                        end
                  | FIX({functions,scope}) =>
                        let
                          val (binds', body) = layout_rec scope
                          val _ = inInfo := "(* fix *)"
                        in
                          (mk_mutual_binding (List.rev functions):: binds', body)
                        end
                  | EXCEPTION(excon, ty_opt, scope) =>
                        let 
                          val (binds', body) = layout_rec scope
                          val _ = inInfo := "(* exn *)"
                        in
                           (mk_excon_binding(excon, ty_opt)::binds', body)
                        end
                  | _ => ([],layoutLambdaExp(lexp,0))

           val (l, body) = layout_rec lexp
           val bindings =  PP.NODE{start = "", finish = "", childsep = PP.RIGHT "; ", 
                                indent = 0, children = l}
          in
            PP.NODE{start= "let ",
                    finish=" end " (* ^ (!inInfo) *),   (*martin*)
                    indent=4,
                    children=[bindings,body],
                    childsep=PP.LEFT (" in " (* ^ (!inInfo) *) )} (*martin*)
          end

      and mk_valbind(pat, e) = 
        let 
            val child1 = layPatLet pat   (*NB*)
         in
            PP.NODE{start = "val ",finish="",childsep=PP.RIGHT " = ",
                 indent=4,  children=[child1, layoutLambdaExp(e,0)] }
        end
      and mk_excon_binding(excon, ty_opt) = 
            (* exception EXCON : mu  (* exn value or name at RHO *) or
               excpetion EXCON : mu 
            *)
         (case ty_opt of
            NONE =>  PP.NODE{start = "exception ",finish="",childsep=PP.RIGHT " : ",
                 indent=4,  children=[PP.LEAF(Excon.pr_excon excon)] }
          | SOME ty => PP.NODE{start = "exception ",finish="",childsep=PP.RIGHT " ",
                 indent=4,  children=[PP.LEAF(Excon.pr_excon excon), PP.LEAF ":", layoutType ty]}
        )
      and  mk_mutual_binding(functions) = 
        let fun mk_fix({lvar,tyvars,Type, bind as (FN{pat, body, ...})})
                     (no, rest_of_mutual_binding) =
              (*   
                   fun f  : sigma  
                       (x_1, ..., x_n) = 
                       body
                            OR
                   fun f (x_1, ..., x_n)  = body   
                            OR
                   fun f (x_1, ..., x_n)  = 
                         body
                            OR
                   fun f (x1:mu_1,
                          ...
                          xn: mu_n
                         )  = 
                         body   
              *)
              (no-1,
                   let
                     val sigma_t = layoutTypeScheme(tyvars,Type)
                     val t1 = let val s: string = Lvars.pr_lvar lvar ^ 
                                 (if !Flags.print_types then ":" else "")
                              in  PP.NODE{start = s, finish = "", indent = String.size s +1,
                                          childsep = PP.NOSEP, children = [sigma_t]}
                              end
                     val formals = PP.HNODE{start="(", finish = ") =", childsep = PP.RIGHT ", ", 
                                                  children = map (fn (lvar,_) => PP.LEAF(Lvars.pr_lvar lvar))
                                                                 pat}
                     val keyword = if no = 1 then "fun " else "and "
                     val body_t = PP.NODE{start = "", finish ="", indent = 4, childsep = PP.NOSEP,
                                          children = [layoutLambdaExp(body, 0)]}
                    in
                      PP.NODE{start = keyword , finish = "", indent = 4, childsep = PP.NOSEP, 
                              children = [t1, formals, body_t]}
                    end
                  :: rest_of_mutual_binding)
            | mk_fix _ _ = die "mk_fix: rhs of fix does not begin with lambda"
       in
        PP.NODE{start = "", finish = "", indent = 0,
                childsep = PP.NOSEP, 
                children = #2(List.foldL mk_fix (List.size functions,[]) functions)}
       end



    and layoutFnPat atpats =
	PP.NODE {start="<", finish=">", indent=0, children=map layoutFnAtPat atpats,
		 childsep=PP.RIGHT ","}

    and layoutLetPat atpats =
	PP.NODE {start="<", finish=">", indent=0, children=map layoutLetAtPat atpats,
		 childsep=PP.RIGHT ","}

    and layoutFnAtPat (lvar, Type) =
      if !Flags.print_types then
        PP.NODE {start=Lvars.pr_lvar lvar ^ ":", finish="", indent=0,
                 children=[layoutType Type], childsep=PP.NOSEP}
      else
        PP.LEAF(Lvars.pr_lvar lvar)

    and layoutLetAtPat (lvar, tyvars, Type) =
      if !Flags.print_types then
        PP.NODE {start=Lvars.pr_lvar lvar ^ ":", finish="", indent=0,
	         children=[layoutTypeScheme(tyvars,Type)], childsep=PP.NOSEP}
      else
        PP.LEAF(Lvars.pr_lvar lvar)

    val layoutLambdaPgm = layoutPgm
    val layoutLambdaExp = fn e => layoutLambdaExp(e,0)
  end;
