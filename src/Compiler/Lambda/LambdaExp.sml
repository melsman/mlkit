
functor LambdaExp(structure Lvars: LVARS
		  structure Con: CON
		  structure Excon: EXCON
		  structure TyName: TYNAME
		  structure PP: PRETTYPRINT  
		  structure Crash: CRASH
		  structure Flags: FLAGS) : LAMBDA_EXP =
  struct

    structure StrSet = OrderSet(structure Order = struct type T = string
							 fun lt (a:string) b = a < b
						  end
				structure PP = PP)

    fun uncurry f (x,y) = f x y

    fun quote s = "\"" ^ String.toString s ^ "\""

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

    val int31Type = CONStype([], TyName.tyName_INT31)
    val int32Type = CONStype([], TyName.tyName_INT32)
    fun intDefaultType() = CONStype([], TyName.tyName_IntDefault())
    val word31Type = CONStype([], TyName.tyName_WORD31)
    val word32Type = CONStype([], TyName.tyName_WORD32)
    fun wordDefaultType() = CONStype([], TyName.tyName_WordDefault())
    val boolType = CONStype([], TyName.tyName_BOOL)
    val foreignptrType = CONStype([], TyName.tyName_FOREIGNPTR)
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
      | DECONprim of {con : con, instances : 'Type list, lv_opt:lvar option}
      | EXCONprim of excon
      | DEEXCONprim of excon
      | RECORDprim 
      | SELECTprim of int        
      | UB_RECORDprim                                 (* Unboxed record. *)
      | DROPprim
      | DEREFprim of {instance: 'Type}
      | REFprim of {instance: 'Type}
      | ASSIGNprim of {instance: 'Type}
      | EQUALprim of {instance: 'Type}
      | CCALLprim of {name : string,                  (* Primitives, etc. *)
		      instances : 'Type list,
		      tyvars : tyvar list,
		      Type : 'Type} 
      | RESET_REGIONSprim of {instance: 'Type}        (* NOT Standard ML, for programmer-directed, 
						       * but safe, resetting of regions *)
      | FORCE_RESET_REGIONSprim of {instance: 'Type}  (* NOT Standard ML, for programmer-controlled, 
						       * unsafe resetting of regions *)

    datatype LambdaPgm = PGM of datbinds * LambdaExp

    and datbinds = DATBINDS of (tyvar list * TyName * (con * Type option) list) list list
      (* list of mutual recursive datatype declarations *)

    and LambdaExp =
        VAR      of {lvar: lvar, instances : Type list}
      | INTEGER  of Int32.int * Type
      | WORD     of Word32.word * Type
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
      | SWITCH_I of {switch: Int32.int Switch, precision: int}
      | SWITCH_W of {switch: Word32.word Switch, precision: int}
      | SWITCH_S of string Switch
      | SWITCH_C of (con*lvar option) Switch
      | SWITCH_E of (excon*lvar option) Switch
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
        | WORD _ => new_acc
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
	| SWITCH_I {switch,precision} => foldSwitch switch
	| SWITCH_W {switch,precision} => foldSwitch switch
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


   (* -----------------------------------------------------------------
    * safeLambdaPgm: This predicate approximates whether a lambda
    * program performs side effects; it is used to determine if a
    * program unit can be discharged at link time in case it is not
    * used. It is also used by the optimiser so as to remove bindings
    * of values that are not used.
    * ----------------------------------------------------------------- *)
   
   local
     exception NotSafe

     val safeCNames = StrSet.fromList 
       ["__plus_word31", "__plus_word32ub", "__plus_word32b",
	"__minus_word31", "__minus_word32ub", "__minus_word32b",
	"__mul_word31", "__mul_word32ub", "__mul_word32b",
	"__less_word31", "__less_word32ub", "__less_word32b",
	"__greater_word31", "__greater_word32ub", "__greater_word32b",
	"__lesseq_word31", "__lesseq_word32ub", "__lesseq_word32b",
	"__greatereq_word31", "__greatereq_word32ub", "__greatereq_word32b",
	"__less_int31", "__less_int32ub", "__less_int32b",
	"__greater_int31", "__greater_int32ub", "__greater_int32b",
	"__lesseq_int31", "__lesseq_int32ub", "__lesseq_int32b",
	"__greatereq_int31", "__greatereq_int32ub", "__greatereq_int32b",
	"lessStringML",
	"greaterStringML",
	"lesseqStringML",
	"greatereqStringML",
	"__less_real",
	"__greater_real",
	"__lesseq_real",
	"__greatereq_real"]

     fun safeCName n = if StrSet.member n safeCNames then () 
		       else raise NotSafe

     fun safe_prim prim =
       case prim
	 of CONprim _          => ()
	  | DECONprim _        => ()
	  | EXCONprim _        => ()
	  | DEEXCONprim _      => ()
	  | RECORDprim         => ()
	  | SELECTprim _       => ()
	  | EQUALprim _        => ()
	  | DROPprim           => ()            
	  | CCALLprim {name,...} => safeCName name 
	   
	       (* likewise for other primitives that do not perform side effects
		* and cannot raise exceptions *)
	  | _ => raise NotSafe 

     fun safe_sw safe (SWITCH(e,sel,opt_e)) =
       let fun safe_sel [] = ()
	     | safe_sel ((a,e)::rest) = (safe e; safe_sel rest)
	   fun safe_opt (SOME e) = safe e
	     | safe_opt NONE = ()
       in (safe e; safe_sel sel; safe_opt opt_e)
       end

     fun safe lamb =
       case lamb
	 of VAR _	                => ()
	  | INTEGER _                   => ()
	  | WORD _                      => ()
	  | STRING _	                => ()
	  | REAL _	                => ()
	  | FN _	                => ()
	  | LET {bind,scope,...}        => (safe bind; safe scope)
	  | FIX {scope,...}             => safe scope
	  | APP _	                => raise NotSafe
	  | EXCEPTION (_,_,scope)       => safe scope
	  | RAISE _                     => raise NotSafe
	  | HANDLE(lamb, _)             => safe lamb
	  (* if `lamb' is safe, then the actual handler can never be
	   * activated. If `lamb' is unsafe, then the entire expression
	   * is unsafe anyway. *)
	  | SWITCH_I {switch,precision} => safe_sw safe switch
	  | SWITCH_W {switch,precision} => safe_sw safe switch
	  | SWITCH_S sw                 => safe_sw safe sw
	  | SWITCH_C sw                 => safe_sw safe sw
	  | SWITCH_E sw                 => safe_sw safe sw
	  | PRIM(prim,lambs)            => (safe_prim prim; app safe lambs) 
	  | FRAME _                     => ()
   in
     fun safeLambdaExps lambs = (app safe lambs; true) handle NotSafe => false
     fun safeLambdaExp lamb = safeLambdaExps [lamb]
     fun safeLambdaPgm(PGM(_,exp)) = safeLambdaExp exp
   end   

(*

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
	  | DROPprim => true
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
	  | WORD _ => true			
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
*)

   (* prettyprinting. *)
   type StringTree = PP.StringTree

   val barify_p = ref false

   fun member eq c nil = false
     | member eq c (y::ys) = eq(c,y) orelse member eq c ys

   fun strip_ s =
       if String.size s > 0 then
	   if CharVector.sub(s,0) = #"_" orelse CharVector.sub(s,0) = #"'" then
	       let fun do_strip (#"_"::rest) = do_strip rest
		     | do_strip (#"'"::rest) = do_strip rest
		     | do_strip rest = rest
	       in implode(do_strip (explode s))
	       end
	   else s
       else s

   fun is_symb (c : char):bool =
       case c of
	   #"!" => true | #"%" => true | #"&" => true | #"$" => true | #"#" => true 
	 | #"+" => true | #"-" => true | #"/" => true | #":" => true | #"<" => true 
	 | #"=" => true | #">" => true | #"?" => true | #"@" => true | #"\\" => true
	 | #"~" => true | #"`" => true | #"^" => true | #"|" => true | #"*" => true
	 | _ => false

   fun unsymb (s:string) : string =
     let fun do_unsymb nil = nil
	   | do_unsymb (c::cs) = if is_symb c then do_unsymb cs 
				 else c :: do_unsymb cs
     in
       if String.size s < 1 then s
       else if is_symb (CharVector.sub(s,0)) then implode(#"v" :: do_unsymb (explode s))
	    else s
     end

   fun pr_lvar lv = if !barify_p then unsymb(Lvars.pr_lvar' lv)
		    else Lvars.pr_lvar lv

   fun pr_excon ex = 
       if !barify_p then 
	   if member Excon.eq ex [Excon.ex_DIV,Excon.ex_MATCH,Excon.ex_OVERFLOW,Excon.ex_INTERRUPT] 
	       then Excon.pr_excon ex
	   else unsymb(Excon.pr_excon' ex)
       else Excon.pr_excon ex

   fun pr_con c = 
       if !barify_p then 
	   if Con.eq(c,Con.con_CONS) then "op ::"
	   else if member Con.eq c [Con.con_NIL,Con.con_FALSE,Con.con_TRUE] 
		    then Con.pr_con c 
		else unsymb(Con.pr_con' c)
       else Con.pr_con c

    fun layoutPrim layoutType prim = 
     case prim of
        CONprim{con,instances} => 
	    let val con_s = pr_con con
	    in
		if !Flags.print_types then
		    PP.NODE{start= con_s, finish="", 
			    indent=2,children=map layoutType instances,childsep=PP.RIGHT","}
		else PP.LEAF con_s
	    end
      | DECONprim{con,instances,lv_opt} =>
	      if !barify_p then 
		  (case lv_opt of
		       SOME lvar => PP.LEAF (pr_lvar lvar) 
		     | NONE => PP.LEAF "TODO")
	      else
	      if !Flags.print_types then
		  PP.NODE{start= "decon(" ^ pr_con con,finish=")", 
			  indent=2,children=map layoutType instances,childsep=PP.RIGHT","}
	      else 
		  PP.NODE{start= "decon(" ^ pr_con con,finish=")", 
			  indent=2,children=[],childsep=PP.NOSEP}
      | EXCONprim excon => 
	  PP.LEAF(pr_excon excon)
      | DEEXCONprim excon => 
	  PP.LEAF("deexcon" ^ pr_excon excon)
      | RECORDprim => PP.LEAF("record")
      | SELECTprim i => PP.LEAF("select(" ^ Int.toString i ^ ")")
      | UB_RECORDprim => PP.LEAF("ubrecord") 
      | DROPprim => PP.LEAF("DROP")
      | CCALLprim{name="__neg_int31",...} => PP.LEAF("~" )
      | CCALLprim{name="__neg_int32ub",...} => PP.LEAF("~" )
      | CCALLprim{name="__neg_real",...} => PP.LEAF("~" )
      | CCALLprim{name="__abs_int31",...} => PP.LEAF("abs" )
      | CCALLprim{name="__abs_int32ub",...} => PP.LEAF("abs" )
      | CCALLprim{name="__abs_real",...} => PP.LEAF("abs" )
      | CCALLprim{name="floorFloat",...} => PP.LEAF("floor" )
      | CCALLprim{name="realInt",...} => PP.LEAF("real" )
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
      | CCALLprim{name="__mul_real", ...} => PP.LEAF("*")
      | CCALLprim{name="__mul_int31", ...} => PP.LEAF("*")
      | CCALLprim{name="__mul_int32", ...} => PP.LEAF("*")
      | CCALLprim{name="__plus_real", ...} => PP.LEAF("+")
      | CCALLprim{name="__plus_int31", ...} => PP.LEAF("+")
      | CCALLprim{name="__plus_int32", ...} => PP.LEAF("+")
      | CCALLprim{name="__minus_real", ...} => PP.LEAF("-")
      | CCALLprim{name="__minus_int31", ...} => PP.LEAF("-")
      | CCALLprim{name="__minus_int32", ...} => PP.LEAF("-")
      | CCALLprim{name="__equal_int31", ...} => 
	    if !Flags.print_types then PP.LEAF("=[int31]")
	    else PP.LEAF("=")
      | EQUALprim {instance} => 
          if !Flags.print_types then
	      PP.NODE{start="=(",finish=")",indent=2,
		  children=[layoutType instance],childsep=PP.NOSEP}
          else PP.LEAF " = "
      | CCALLprim{name="__less_real", ...} => PP.LEAF("<")
      | CCALLprim{name="__less_int31", ...} => PP.LEAF("<")
      | CCALLprim{name="__less_int32", ...} => PP.LEAF("<")
      | CCALLprim{name="__less_string", ...} => PP.LEAF("<")
      | CCALLprim{name="__less_word31", ...} => PP.LEAF("<")
      | CCALLprim{name="__less_word32", ...} => PP.LEAF("<")

      | CCALLprim{name="__greater_real", ...} => PP.LEAF(">")
      | CCALLprim{name="__greater_int31", ...} => PP.LEAF(">")
      | CCALLprim{name="__greater_int32", ...} => PP.LEAF(">")
      | CCALLprim{name="__greater_string", ...} => PP.LEAF(">")
      | CCALLprim{name="__greater_word31", ...} => PP.LEAF(">")
      | CCALLprim{name="__greater_word32", ...} => PP.LEAF(">")

      | CCALLprim{name="__lesseq_real", ...} => PP.LEAF("<=")
      | CCALLprim{name="__lesseq_int31", ...} => PP.LEAF("<=")
      | CCALLprim{name="__lesseq_int32", ...} => PP.LEAF("<=")
      | CCALLprim{name="__lesseq_string", ...} => PP.LEAF("<=")
      | CCALLprim{name="__lesseq_word31", ...} => PP.LEAF("<=")
      | CCALLprim{name="__lesseq_word32", ...} => PP.LEAF("<=")

      | CCALLprim{name="__greatereq_real", ...} => PP.LEAF(">=")
      | CCALLprim{name="__greatereq_int31", ...} => PP.LEAF(">=")
      | CCALLprim{name="__greatereq_int32", ...} => PP.LEAF(">=")
      | CCALLprim{name="__greatereq_string", ...} => PP.LEAF(">=")
      | CCALLprim{name="__greatereq_word31", ...} => PP.LEAF(">=")
      | CCALLprim{name="__greatereq_word32", ...} => PP.LEAF(">=")

      | CCALLprim {name, instances, tyvars, Type} => 
          if !Flags.print_types then
	      PP.NODE {start="ccall (" ^ name ^ " ", finish=")", indent=2,
		       children=map layoutType instances, childsep=PP.LEFT ", "}
          else 
	      if !barify_p then PP.LEAF ("Prim." ^ strip_ name)
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
	  PP.NODE{start=x ^ " => ",finish="",indent=2,childsep=PP.NOSEP,
		  children=[layoutLambdaExp(lamb,0)]}
	val head = PP.HNODE{start="(case ",finish="",childsep=PP.NOSEP,
			    children=[layoutLambdaExp(lamb,0)]}
	val rules = map (fn (x,e) => (show_const x,e)) rules
	val rules = (case wildcardOpt of 
			 SOME e => rules @ [("_", e)]
		       | NONE => rules)
	val children = PP.NODE{start="",finish="",indent=3,childsep=PP.LEFT " | ",
			       children=map child rules}
      in	  
	  PP.NODE{start="",finish=")",indent=0,childsep=PP.RIGHT " of ",
		  children=[head,children]}
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

    and layoutTypes [tau] = layoutType tau
      | layoutTypes taus = PP.NODE {start="<", finish=">", childsep=PP.LEFT ", ", indent=0,
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
	  PP.NODE{start=pr_lvar lvar ^ ": ", finish="", indent=3,
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
	    PP.NODE{start=pr_excon excon  ^ connect, finish="", indent=3,
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
             val start:string = pr_lvar lvar ^ " :" 
         in PP.NODE{start = start, finish = "", indent = String.size start +1,
                    childsep = PP.NOSEP, children = [sigma_t]}
         end
     else PP.LEAF(pr_lvar lvar)

   fun layPatLet [] = PP.LEAF("_")   (* wild card *)
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
		       children=PP.LEAF (pr_con con) :: layoutTypeOpt tauopt})
				     
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
		PP.NODE{start=pr_lvar lv ^ ":(", finish=")",indent=0,
			children=map layoutType taus,
			childsep=PP.RIGHT ","}
            else PP.LEAF(pr_lvar lv)
      | INTEGER (i,tau) => 
          if !Flags.print_types then 
            PP.NODE{start=Int32.toString i ^ ":", finish=" ",indent=0,
                    children=[layoutType tau],
                    childsep=PP.NOSEP}
          else PP.LEAF(Int32.toString i)
      | WORD (w,tau) =>
	    if !Flags.print_types then 
	      PP.NODE{start="0x" ^ Word32.toString w ^ ":", finish=" ",indent=0,
		      children=[layoutType tau],
		      childsep=PP.NOSEP}
	    else PP.LEAF("0x" ^ Word32.toString w)

      | STRING s => PP.LEAF(quote s)
      | REAL r => PP.LEAF(r)
      | FN {pat,body} => 
	  PP.NODE{start="(fn ",finish=")", indent=4,
		  children=[layoutFnPat pat,
			    layoutLambdaExp(body,0)], 
		  childsep=PP.RIGHT " => "
		  }
      | LET _ => layout_let_fix_and_exception lamb
      | FIX _ => layout_let_fix_and_exception lamb
      | EXCEPTION _ => layout_let_fix_and_exception lamb
(*
	  let
	    fun layoutFunction {lvar, tyvars, Type, bind} =
              if !Flags.print_types then
                PP.NODE{start=pr_lvar lvar ^ ": " , finish="", indent=3,
                        children=[layoutTypeScheme(tyvars,Type),
                                  layoutLambdaExp(bind,0)], childsep=PP.RIGHT " = "}
              else
                PP.NODE{start=pr_lvar lvar ^ " = ", finish="", indent=3,
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
*)
      | APP(lamb1, lamb2) =>
	  PP.NODE{start= if context>13 then "(" else "", 
                  finish=if context>13 then ")" else "", 
		  childsep=PP.RIGHT " ",
                  indent=1,
		  children=[layoutLambdaExp(lamb1,13), layoutLambdaExp(lamb2,14)]
		  }
(*
      | EXCEPTION(excon,NONE,lamb) =>
	  PP.NODE{start="exception " ^ pr_excon excon ^ " in",finish="",
		  indent=0, children=[layoutLambdaExp(lamb,0)],
		  childsep=PP.NOSEP}
      | EXCEPTION(excon,SOME tau,lamb) =>
	  PP.NODE{start="",finish="", indent=0,
		  children=[PP.NODE{start="exception  " ^ pr_excon excon ^ " of ",
				    finish="",indent=0,
				    children=[layoutType tau],
				    childsep=PP.NOSEP},
			    layoutLambdaExp(lamb,0)], 
		  childsep=PP.RIGHT " in "
		  }
*)
      | RAISE(lamb,typelist) =>	  
	  PP.NODE{start="raise ",
                  finish="", 
                  indent=6,
		  children=[layoutLambdaExp(lamb,0)] @ (if !Flags.print_types 
							    then [layoutTypeList typelist]
							else []), 
		  childsep=PP.RIGHT ","
		  }

      | HANDLE(lamb1, lamb2) =>
          PP.NODE{start=if context>=12 then "(" else "", 
                  finish=if context>=12 then ")" else "", 
                  indent=3,
		  children=[layoutLambdaExp(lamb1,12), layoutLambdaExp(lamb2,12)],
		  childsep=PP.LEFT " handle "
		  }
      | SWITCH_I {switch, precision} => 
	  layoutSwitch layoutLambdaExp Int32.toString switch
      | SWITCH_W {switch, precision} => 
	  layoutSwitch layoutLambdaExp (fn w => "0x" ^ Word32.toString w) switch
      | SWITCH_S sw => 
	  layoutSwitch layoutLambdaExp (fn x => x) sw
      | SWITCH_C sw => 
	  let fun pr_c (c,NONE) = pr_con c
		| pr_c (c,SOME lv) = if !barify_p then pr_con c ^ " " ^ pr_lvar lv
				     else pr_con c
	  in layoutSwitch layoutLambdaExp pr_c sw
	  end
      | SWITCH_E sw => 
	  let fun pr_exc (e,NONE) = pr_excon e
		| pr_exc (e,SOME lv) = if !barify_p then pr_excon e ^ " " ^ pr_lvar lv
				       else pr_excon e
	  in layoutSwitch layoutLambdaExp pr_exc sw
	  end
      | PRIM(prim,lambs) =>
         (case (prim,lambs) of
           (RECORDprim,_) =>
             PP.NODE{start="(",finish=")",indent=1,
                     children=(map (fn e => layoutLambdaExp(e,0))  lambs),
                     childsep=PP.RIGHT ","}
         | (UB_RECORDprim,_) =>
	      let val (s,f) = if !barify_p then ("(",")") else ("<",">")
	      in PP.NODE{start=s,finish=f,indent=1,
			 children=(map (fn e => layoutLambdaExp(e,0)) lambs),
			 childsep=PP.RIGHT ","}
	      end
         | (SELECTprim i, [lamb]) => 
	      let val i = if !barify_p then i+1 else i
	      in
		  PP.NODE{start="#" ^ Int.toString i ^ "(",finish=")",indent=1,
			  children=[layoutLambdaExp(lamb,0)],
			  childsep=PP.NOSEP}
	      end
         | (DEREFprim{instance},[lamb]) =>
             PP.NODE{start="!(",finish=")",indent=2,
                     children=[layoutLambdaExp(lamb,0)],
                     childsep=PP.NOSEP}
	 | (DECONprim{con,instances,lv_opt},[lamb]) =>
	      if !barify_p then 
		  case lv_opt of
		      SOME lvar => PP.LEAF (pr_lvar lvar) 
		    | NONE => PP.LEAF "TODO"
	      else
	      if !Flags.print_types then
		  PP.NODE{start= "decon(" ^ pr_con con,finish=")", 
			  indent=2,children=map layoutType instances @ [layoutLambdaExp(lamb,0)],
			  childsep=PP.RIGHT","}
	      else 
		  PP.NODE{start= "decon(" ^ pr_con con ^ ",",finish=")", 
			  indent=2,children=[layoutLambdaExp(lamb,0)],childsep=PP.NOSEP}
         | (REFprim{instance},[lamb]) =>
             PP.NODE{start="ref(",finish=")",indent=2,
                     children=[layoutLambdaExp(lamb,0)],
                     childsep=PP.NOSEP}
	 | (EXCONprim excon, []) => PP.LEAF(pr_excon excon)
	 | (CONprim{con,instances}, []) => 
	     if !Flags.print_types then
		 PP.NODE{start= pr_con con, finish="", 
			 indent=2,children=map layoutType instances,childsep=PP.RIGHT","}
	     else PP.LEAF (pr_con con)
         | (DROPprim,[lamb]) => layoutLambdaExp(lamb,context)
         | (ASSIGNprim{instance},_) => layout_infix context 3 " := "lambs
         | (CCALLprim{name="__mul_real", ...}, [_,_]) => layout_infix context 7 " * " lambs
         | (CCALLprim{name="__mul_int31", ...}, [_,_]) =>  layout_infix context 7 " * " lambs
         | (CCALLprim{name="__mul_int32ub", ...}, [_,_]) =>  layout_infix context 7 " * " lambs
         | (CCALLprim{name="__plus_real", ...}, [_,_]) => layout_infix context 6 " + " lambs
         | (CCALLprim{name="__plus_int31", ...}, [_,_]) =>  layout_infix context 6 " + " lambs
         | (CCALLprim{name="__plus_int32ub", ...}, [_,_]) =>  layout_infix context 6 " + " lambs
         | (CCALLprim{name="__minus_real", ...}, [_,_]) => layout_infix context 6 " - " lambs
         | (CCALLprim{name="__minus_int31", ...}, [_,_]) =>  layout_infix context 6 " - "lambs
         | (CCALLprim{name="__minus_int32ub", ...}, [_,_]) =>  layout_infix context 6 " - "lambs

         | (CCALLprim{name="divFloat", ...}, [_,_]) =>  layout_infix context 7 " / "lambs

         | (EQUALprim{instance},[_,_]) => layout_infix context 4 " = "lambs

         | (CCALLprim{name="__less_word31", ...}, [_,_]) => layout_infix context 4 " < "lambs
         | (CCALLprim{name="__less_word32ub", ...}, [_,_]) => layout_infix context 4 " < "lambs
         | (CCALLprim{name="__less_int31", ...}, [_,_]) => layout_infix context 4 " < "lambs
         | (CCALLprim{name="__less_int32ub", ...}, [_,_]) => layout_infix context 4 " < "lambs
         | (CCALLprim{name="__less_real", ...}, [_,_]) => layout_infix context 4 " < "lambs
         | (CCALLprim{name="__less_string", ...}, [_,_]) => layout_infix context 4 " < "lambs

         | (CCALLprim{name="__greater_word31", ...}, [_,_]) => layout_infix context 4 " > "lambs
         | (CCALLprim{name="__greater_word32ub", ...}, [_,_]) => layout_infix context 4 " > "lambs
         | (CCALLprim{name="__greater_int31", ...}, [_,_]) => layout_infix context 4 " > "lambs
         | (CCALLprim{name="__greater_int32ub", ...}, [_,_]) => layout_infix context 4 " > "lambs
         | (CCALLprim{name="__greater_real", ...}, [_,_]) => layout_infix context 4 " > "lambs
         | (CCALLprim{name="__greater_string", ...}, [_,_]) => layout_infix context 4 " > "lambs

         | (CCALLprim{name="__lesseq_word31", ...}, [_,_]) => layout_infix context 4 " <= "lambs
         | (CCALLprim{name="__lesseq_word32ub", ...}, [_,_]) => layout_infix context 4 " <= "lambs
         | (CCALLprim{name="__lesseq_int31", ...}, [_,_]) => layout_infix context 4 " <= "lambs
         | (CCALLprim{name="__lesseq_int32ub", ...}, [_,_]) => layout_infix context 4 " <= "lambs
         | (CCALLprim{name="__lesseq_real", ...}, [_,_]) => layout_infix context 4 " <= "lambs
         | (CCALLprim{name="__lesseq_string", ...}, [_,_]) => layout_infix context 4 " <= "lambs

         | (CCALLprim{name="__greatereq_word31", ...}, [_,_]) => layout_infix context 4 " >= "lambs
         | (CCALLprim{name="__greatereq_word32ub", ...}, [_,_]) => layout_infix context 4 " >= "lambs
         | (CCALLprim{name="__greatereq_int31", ...}, [_,_]) => layout_infix context 4 " >= "lambs
         | (CCALLprim{name="__greatereq_int32ub", ...}, [_,_]) => layout_infix context 4 " >= "lambs
         | (CCALLprim{name="__greatereq_real", ...}, [_,_]) => layout_infix context 4 " >= "lambs
         | (CCALLprim{name="__greatereq_string", ...}, [_,_]) => layout_infix context 4 " >= "lambs

         | _ => 
	     if !barify_p then 
		 let fun layoutArgs [lamb] = layoutLambdaExp(lamb,context)
		       | layoutArgs lambs =
		     PP.HNODE {start="(",finish=")",childsep=PP.RIGHT",",
			       children=map (fn x => layoutLambdaExp(x,0)) lambs}
		     val (s,f) = if context > 13 then ("(",")") else ("","")
		 in
		     PP.HNODE{start=s,finish=f,childsep=PP.RIGHT " ", 
			      children=[layoutPrim layoutType prim,
					layoutArgs lambs]}
		 end
	     else
		 PP.NODE{start="PRIM(",finish=")",indent=3,
			 children=[layoutPrim layoutType prim,
				   PP.NODE{start="[",finish="]",indent=1,
					   children=map(fn x => layoutLambdaExp(x,0)) lambs,
					   childsep=PP.RIGHT ","}],
			 childsep=PP.RIGHT ", "}
        )
      | FRAME fr => 
	      if !barify_p then 
		  let val {declared_lvars,declared_excons} = fr
		      val lvs = map (fn l => PP.LEAF ("val " ^ pr_lvar(#lvar l) ^ " = " ^ 
						      pr_lvar(#lvar l))) declared_lvars
		      val exs = map (fn l => PP.LEAF ("exception " ^ pr_excon (#1 l) ^ " = " ^
						      pr_excon (#1 l))) declared_excons
			  
		  in PP.NODE {start="",finish="",childsep = PP.RIGHT " ",
			      indent=0,children=lvs@exs}
		  end
	      else layoutFrame "FRAME" fr

    and layout_let_fix_and_exception lexp = 
          let 
            fun layout_rec lexp = 
                  case lexp of 
                    LET{pat, bind, scope} =>
                        let
                          val (binds, body, frame) = layout_rec scope
                        in
                           (mk_valbind(pat,bind)::binds, body, frame)
                        end
                  | FIX({functions,scope}) =>
                        let
                          val (binds', body, frame) = layout_rec scope
                        in
                          (mk_mutual_binding (rev functions):: binds', body, frame)
                        end
                  | EXCEPTION(excon, ty_opt, scope) =>
                        let 
                          val (binds', body, frame) = layout_rec scope
                        in
                           (mk_excon_binding(excon, ty_opt)::binds', body, frame)
                        end
		  | FRAME _ => ([],layoutLambdaExp(lexp,0),true)
                  | _ => ([],layoutLambdaExp(lexp,0),false)

           val (l, body, frame:bool) = layout_rec lexp
           val bindings =  PP.NODE{start = "", finish = "", childsep = PP.RIGHT " ", 
				   indent = 0, children = l}
	   val start = 
	       if frame andalso !barify_p then "local " else "let "
          in
            PP.NODE{start=start,
                    finish=" end ",
                    indent=4,
                    children=[bindings,body],
                    childsep=PP.LEFT " in "}
          end

      and mk_valbind(pat, e) = 
        let 
            val child1 = layPatLet pat   (*NB*)
         in
            PP.NODE{start = "val ",finish="",childsep=PP.RIGHT " = ",
                 indent=4,  children=[child1, layoutLambdaExp(e,0)] }
        end
      and mk_excon_binding(excon, ty_opt) = 
            (* exception EXCON    (* exn value or name at RHO *) or
               excpetion EXCON of tau 
            *)
         (case ty_opt of
            NONE =>  PP.LEAF ("exception " ^ pr_excon excon)
          | SOME ty => PP.NODE{start = "exception ",
			       finish="", childsep=PP.RIGHT " of ", indent=10,  
			       children=[PP.LEAF(pr_excon excon), layoutType ty]}
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
		     val keyword = if no = 1 then "fun" else "and"
                     val t1 = 
			 if !Flags.print_types then
			     let val sigma_t = layoutTypeScheme(tyvars,Type) 
				 val s = pr_lvar lvar
			     in
				 PP.NODE{start = s ^ ":", finish = "", indent = String.size s +1,
					 childsep = PP.NOSEP, children = [sigma_t]}
			     end
			 else PP.LEAF (pr_lvar lvar)
                     val formals_t = 
			 case pat of
			     [(lvar,_)] => PP.LEAF (pr_lvar lvar ^ " = ")
			   | _ => 
				 if !barify_p then 
				     PP.HNODE{start="(", finish = ") = ", childsep = PP.RIGHT ", ", 
					      children = map (fn (lvar,_) => PP.LEAF(pr_lvar lvar)) pat}
				 else 
				     PP.HNODE{start="<", finish = "> = ", childsep = PP.RIGHT ", ", 
					      children = map (fn (lvar,_) => PP.LEAF(pr_lvar lvar)) pat}
                     val head_t = PP.HNODE{start="", finish ="", childsep = PP.RIGHT " ",
					   children = [PP.LEAF keyword,t1,formals_t]}
                     val body_t = PP.NODE{start = "", finish ="", indent = 2, childsep = PP.NOSEP,
                                          children = [layoutLambdaExp(body, 0)]}
                    in
                      PP.NODE{start = "", finish = "", indent = 0, childsep = PP.NOSEP, 
                              children = [head_t, body_t]}
                    end
                  :: rest_of_mutual_binding)
            | mk_fix _ _ = die "mk_fix: rhs of fix does not begin with lambda"
       in
        PP.NODE{start = "", finish = "", indent = 0,
                childsep = PP.NOSEP, 
                children = #2(foldl (uncurry mk_fix) (length functions,[]) functions)}
       end

    and layoutFnPat atpats =
	if !barify_p then
	    case atpats of
		[atpat] => layoutFnAtPat atpat
	      | _ => PP.NODE {start="<", finish=">", indent=0, children=map layoutFnAtPat atpats,
			      childsep=PP.RIGHT ","}
	else
	    PP.NODE {start="<", finish=">", indent=0, children=map layoutFnAtPat atpats,
		     childsep=PP.RIGHT ","}
(*
    and layoutLetPat atpats =
	PP.NODE {start="<", finish=">", indent=0, children=map layoutLetAtPat atpats,
		 childsep=PP.RIGHT ","}
*)
    and layoutFnAtPat (lvar, Type) =
	if !barify_p then
	    (* print type if record *)
	    (case Type of
		 RECORDtype _ =>
		     PP.HNODE{start=pr_lvar lvar ^ " : ",finish="",childsep=PP.NOSEP,
			      children=[layoutType Type]}
	       | _ => PP.LEAF (pr_lvar lvar))
	else
	    if !Flags.print_types then
		PP.NODE {start=pr_lvar lvar ^ ":", finish="", indent=0,
			 children=[layoutType Type], childsep=PP.NOSEP}
	    else PP.LEAF(pr_lvar lvar)
(*
    and layoutLetAtPat (lvar, tyvars, Type) =
      if !Flags.print_types then
        PP.NODE {start=pr_lvar lvar ^ ":", finish="", indent=0,
	         children=[layoutTypeScheme(tyvars,Type)], childsep=PP.NOSEP}
      else
        PP.LEAF(pr_lvar lvar)
*)
    val layoutLambdaPgm = layoutPgm
    val layoutLambdaExp = fn e => layoutLambdaExp(e,0)

(*
    local
	fun pp_con out c = out (pr_con c)
	fun pp_tv out tv = (out:string->unit) (pr_tyvar tv)
	fun pp_tn out tn = (out:string->unit) (TyName.pr_TyName tn)

	fun pp_ty (out:string->unit) (ty:Type) : unit =
	    let fun pp (ARROWtype([ty1],[ty2])) = (pp ty1; out " -> "; pp ty2)
		  | pp (TYVARtype tv) = pp_tv out tv
		  | pp (CONStype(nil,tn)) = pp_tn out tn
		  | pp (CONStype(tys,tn)) = (out "(" ; pp_tys out "," tys ; out ")" ; pp_tn out tn)
		  | pp (RECORDtype tys) = (out "(" ; pp_tys out "*" tys ; out ")")
		  | pp _ = die "pp_ty"
	    in pp ty
	    end
	and pp_tys out sep nil = ()
	  | pp_tys out sep [ty] = pp_ty out ty
	  | pp_tys out sep (ty::tys) = (pp_ty out ty ; out sep ; pp_tys out sep tys)

	fun pp_tyopt out NONE = ()
	  | pp_tyopt out (SOME ty) = (out " of "; pp_ty out ty)

	fun pp_d out nil = ()
	  | pp_d out (d::ds) =
	    let fun pp_tvs nil = ()
		  | pp_tvs l = 
		  let fun pp nil = ()
			| pp (tv::tvs) = (pp_tv out tv; out ","; pp tvs)
		  in out "(" ; pp l ; out ")"
		  end
		fun pp_cs nil = ()
		  | pp_cs [(c,tyopt)] = (pp_con out c ; pp_tyopt out tyopt)
		  | pp_cs ((c,tyopt)::cs) = (pp_con out c ; pp_tyopt out tyopt ; out " | "; pp_cs cs)
		fun pp h (tvs,tn,cs) = 
		    (out h; pp_tvs tvs; pp_tn out tn; out " = "; pp_cs cs)
	    in pp "datatype " d
		; app (pp "and ") ds
	    end
    
	fun pp_ds out (DATBINDS l) =
	    let fun pp_ds' nil = ()
		  | pp_ds' (x::xs) = (pp_d out x ; pp_ds' xs)
	    in pp_ds' l
	    end
    
	fun pp_LambdaExp out e = ()
    in
	fun pp_LambdaPgm {file:string, pgm=PGM(datbinds,e)} : unit =
	    let val os = TextIO.openOut file
		fun out s = TextIO.output(os, s)
	    in  pp_ds out datbinds;
		pp_LambdaExp out e
	    end
    end
*)

    fun barify a = 
	(barify_p := true; 
	 layoutPgm a before barify_p := false)

    (* Picklers *)
    val pu_tyvar = Pickle.word

    val (pu_Type,pu_Types) =
	let open Pickle
	    fun toInt (TYVARtype _) = 0
	      | toInt (ARROWtype _) = 1
	      | toInt (CONStype _) = 2
	      | toInt (RECORDtype _) = 3
	    val pu_TypeList : Type Pickle.pu -> Type list Pickle.pu = 
		Pickle.cache listGen

	    fun fun_TYVARtype _ = 
		con1 TYVARtype (fn TYVARtype tv => tv | _ => die "pu_Type.TYVARtype")
		pu_tyvar
	    fun fun_ARROWtype pu =
		con1 ARROWtype (fn ARROWtype p => p | _ => die "pu_Type.ARROWtype")
		(pairGen0(pu_TypeList pu,pu_TypeList pu))
	    fun fun_CONStype pu =
		con1 CONStype (fn CONStype p => p | _ => die "pu_Type.CONStype")
		(pairGen0(pu_TypeList pu,TyName.pu))
	    fun fun_RECORDtype pu =
		con1 RECORDtype (fn RECORDtype a => a | _ => die "pu_Type.RECORDtype")
		(pu_TypeList pu)
	    val pu = dataGen("LambdaExp.Type",toInt,[fun_TYVARtype,fun_ARROWtype, fun_CONStype, fun_RECORDtype])
	in (pu, pu_TypeList pu)
	end
    val pu_tyvars = Pickle.listGen pu_tyvar
	
    val pu_TypeScheme = Pickle.pairGen(pu_tyvars,pu_Type)

    val pu_lv_opt = Pickle.optionGen Lvars.pu

    val pu_TypeOpt = Pickle.optionGen pu_Type

    val pu_frame =
	let open Pickle
	    val pu_dlv = 
		convert (fn (lv,tvs,t) => {lvar=lv,tyvars=tvs,Type=t},
			 fn {lvar=lv,tyvars=tvs,Type=t} => (lv,tvs,t))
		(tup3Gen(Lvars.pu,pu_tyvars,pu_Type))
	in convert (fn (dlvs,dexns) => {declared_lvars=dlvs,declared_excons=dexns},
		    fn {declared_lvars=dlvs,declared_excons=dexns} => (dlvs,dexns))
	    (pairGen(listGen pu_dlv,listGen(pairGen(Excon.pu,pu_TypeOpt))))
	end

    val pu_TypeList =
	let open Pickle
	    fun toInt (Types _) = 0
	      | toInt (Frame _) = 1
	      | toInt RaisedExnBind = 2
	    fun fun_Types _ =
		con1 Types (fn Types a => a | _ => die "pu_TypeList.Types")
		pu_Types
	    fun fun_Frame _ =
		con1 Frame (fn Frame a => a | _ => die "pu_TypeList.Frame")
		pu_frame
	    val fun_RaisedExnBind = con0 RaisedExnBind
	in dataGen("LambdaExp.TypeList",toInt,[fun_Types,fun_Frame,fun_RaisedExnBind])
	end

    val pu_prim =
	let open Pickle
	    fun toInt (CONprim _) = 0
	      | toInt (DECONprim _) = 1
	      | toInt (EXCONprim _) = 2
	      | toInt (DEEXCONprim _) = 3
	      | toInt RECORDprim = 4
	      | toInt (SELECTprim _) = 5
	      | toInt UB_RECORDprim = 6
	      | toInt DROPprim = 7
	      | toInt (DEREFprim _) = 8
	      | toInt (REFprim _) = 9
	      | toInt (ASSIGNprim _) = 10
	      | toInt (EQUALprim _) = 11
	      | toInt (CCALLprim _) = 12
	      | toInt (RESET_REGIONSprim _) = 13
	      | toInt (FORCE_RESET_REGIONSprim _) = 14

	    fun fun_CONprim _ = 
		con1 CONprim (fn CONprim a => a | _ => die "pu_prim.CONprim")
		(convert (fn (c,il) => {con=c,instances=il}, fn {con=c,instances=il} => (c,il))
		 (pairGen0 (Con.pu,pu_Types)))
	    fun fun_DECONprim _ =
		con1 DECONprim (fn DECONprim a => a | _ => die "pu_prim.DECONprim")
		(convert (fn (c,il,lvo) => {con=c,instances=il,lv_opt=lvo}, fn {con=c,instances=il,lv_opt=lvo} => (c,il,lvo))
		 (tup3Gen0 (Con.pu,pu_Types,pu_lv_opt)))		
	    fun fun_EXCONprim _ = 
		con1 EXCONprim (fn EXCONprim a => a | _ => die "pu_prim.EXCONprim")
		Excon.pu
	    fun fun_DEEXCONprim _ =
		con1 DEEXCONprim (fn DEEXCONprim a => a | _ => die "pu_prim.DEEXCONprim")
		Excon.pu
	    val fun_RECORDprim = con0 RECORDprim
	    fun fun_SELECTprim _ =
		con1 SELECTprim (fn SELECTprim a => a | _ => die "pu_prim.SELECTprim")
		int
	    val fun_UB_RECORDprim = con0 UB_RECORDprim
	    val fun_DROPprim = con0 DROPprim
	    fun fun_DEREFprim _ =
		con1 DEREFprim (fn DEREFprim a => a | _ => die "pu_prim.DEREFprim")
		(convert(fn t => {instance=t},#instance) pu_Type)
	    fun fun_REFprim _ =
		con1 REFprim (fn REFprim a => a | _ => die "pu_prim.REFprim")
		(convert(fn t => {instance=t},#instance) pu_Type)
	    fun fun_ASSIGNprim _ =
		con1 ASSIGNprim (fn ASSIGNprim a => a | _ => die "pu_prim.ASSIGNprim")
		(convert(fn t => {instance=t},#instance) pu_Type)
	    fun fun_EQUALprim _ =
		con1 EQUALprim (fn EQUALprim a => a | _ => die "pu_prim.EQUALprim")
		(convert(fn t => {instance=t},#instance) pu_Type)
	    fun fun_CCALLprim _ =
		con1 CCALLprim (fn CCALLprim a => a | _ => die "pu_prim.CCALLprim")
		(convert (fn (n,il,(tvs,t)) => {name=n,instances=il,tyvars=tvs,Type=t}, 
			  fn {name=n,instances=il,tyvars=tvs,Type=t} => (n,il,(tvs,t)))
		 (tup3Gen0 (string,pu_Types,pu_TypeScheme)))
	    fun fun_RESET_REGIONSprim _ =
		con1 RESET_REGIONSprim (fn RESET_REGIONSprim a => a | _ => die "pu_prim.RESET_REGIONSprim")
		(convert(fn t => {instance=t},#instance) pu_Type)
	    fun fun_FORCE_RESET_REGIONSprim _ =
		con1 FORCE_RESET_REGIONSprim (fn FORCE_RESET_REGIONSprim a => a | _ => die "pu_prim.FORCE_RESET_REGIONSprim")
		(convert(fn t => {instance=t},#instance) pu_Type)
	in dataGen("LambdaExp.prim",toInt,[fun_CONprim,
					   fun_DECONprim,
					   fun_EXCONprim,
					   fun_DEEXCONprim,
					   fun_RECORDprim,
					   fun_SELECTprim,
					   fun_UB_RECORDprim,
					   fun_DROPprim,
					   fun_DEREFprim,
					   fun_REFprim,
					   fun_ASSIGNprim,
					   fun_EQUALprim,
					   fun_CCALLprim,
					   fun_RESET_REGIONSprim,
					   fun_FORCE_RESET_REGIONSprim])
	end

    fun pu_Switch pu_a pu_LambdaExp =
	let open Pickle
	in convert (SWITCH,fn SWITCH a => a)
	    (tup3Gen0(pu_LambdaExp,listGen(pairGen0(pu_a,pu_LambdaExp)),optionGen pu_LambdaExp))
	end

    val pu_con_lvopt = Pickle.pairGen(Con.pu,pu_lv_opt)

    val pu_excon_lvopt = Pickle.pairGen(Excon.pu,pu_lv_opt)

    val pu_LambdaExp =
	let open Pickle
	    fun toInt (VAR _) = 0
	      | toInt (INTEGER _) = 1
	      | toInt (WORD _) = 2
	      | toInt (STRING _) = 3
	      | toInt (REAL _) = 4
	      | toInt (FN _) = 5
	      | toInt (LET _) = 6
	      | toInt (FIX _) = 7
	      | toInt (APP _) = 8
	      | toInt (EXCEPTION _) = 9
	      | toInt (RAISE _) = 10
	      | toInt (HANDLE _) = 11
	      | toInt (SWITCH_I _) = 12
	      | toInt (SWITCH_W _) = 13
	      | toInt (SWITCH_S _) = 14
	      | toInt (SWITCH_C _) = 15
	      | toInt (SWITCH_E _) = 16
	      | toInt (PRIM _) = 17
	      | toInt (FRAME _) = 18

	    fun fun_VAR pu_LambdaExp =
		con1 VAR (fn VAR a => a | _ => die "pu_LambdaExp.VAR")
		(convert (fn (lv,il) => {lvar=lv,instances=il}, fn {lvar=lv,instances=il} => (lv,il))
		 (pairGen0(Lvars.pu,pu_Types)))
	    fun fun_INTEGER pu_LambdaExp =
		con1 INTEGER (fn INTEGER a => a | _ => die "pu_LambdaExp.INTEGER")
		(pairGen0(int32,pu_Type))
	    fun fun_WORD pu_LambdaExp =
		con1 WORD (fn WORD a => a | _ => die "pu_LambdaExp.WORD")
		(pairGen0(word32,pu_Type))
	    fun fun_STRING pu_LambdaExp =
		con1 STRING (fn STRING a => a | _ => die "pu_LambdaExp.STRING")
		string
	    fun fun_REAL pu_LambdaExp =
		con1 REAL (fn REAL a => a | _ => die "pu_LambdaExp.REAL")
		string
	    fun fun_FN pu_LambdaExp =
		con1 FN (fn FN a => a | _ => die "pu_LambdaExp.FN")
		(convert (fn (p,e) => {pat=p,body=e}, fn {pat=p,body=e} => (p,e))
		 (pairGen0(listGen(pairGen0(Lvars.pu,pu_Type)),pu_LambdaExp)))
	    fun fun_LET pu_LambdaExp =
		con1 LET (fn LET a => a | _ => die "pu_LambdaExp.LET")
		(convert (fn (p,b,s) => {pat=p,bind=b,scope=s}, fn {pat=p,bind=b,scope=s} => (p,b,s))
		 (tup3Gen0(listGen(tup3Gen0(Lvars.pu,pu_tyvars,pu_Type)),
			   pu_LambdaExp, pu_LambdaExp)))
	    fun fun_FIX pu_LambdaExp =
		let val pu_function = 
		    convert (fn (lv,tvs,t,e) => {lvar=lv,tyvars=tvs,Type=t,bind=e},
			     fn {lvar=lv,tyvars=tvs,Type=t,bind=e} => (lv,tvs,t,e))
		    (tup4Gen0(Lvars.pu,pu_tyvars,pu_Type,pu_LambdaExp))
		in con1 FIX (fn FIX a => a | _ => die "pu_LambdaExp.FIX")
		    (convert (fn (fs,s) => {functions=fs,scope=s}, fn {functions=fs,scope=s} => (fs,s))
		     (pairGen0(listGen pu_function,
			      pu_LambdaExp)))
		end
	    fun fun_APP pu_LambdaExp =
		con1 APP (fn APP a => a | _ => die "pu_LambdaExp.APP")
		(pairGen0(pu_LambdaExp,pu_LambdaExp))
	    fun fun_EXCEPTION pu_LambdaExp =
		con1 EXCEPTION (fn EXCEPTION a => a | _ => die "pu_LambdaExp.EXCEPTION")
		(tup3Gen0(Excon.pu,pu_TypeOpt,pu_LambdaExp))
	    fun fun_RAISE pu_LambdaExp =
		con1 RAISE (fn RAISE a => a | _ => die "pu_LambdaExp.RAISE")
		(pairGen0(pu_LambdaExp,pu_TypeList))
	    fun fun_HANDLE pu_LambdaExp =
		con1 HANDLE (fn HANDLE a => a | _ => die "pu_LambdaExp.HANDLE")
		(pairGen0(pu_LambdaExp,pu_LambdaExp))
	    fun fun_SWITCH_I pu_LambdaExp =
		con1 SWITCH_I (fn SWITCH_I a => a | _ => die "pu_LambdaExp.SWITCH_I")
		(convert (fn (sw,p) => {switch=sw,precision=p}, fn {switch=sw,precision=p} => (sw,p))
		 (pairGen0(pu_Switch int32 pu_LambdaExp,int)))
	    fun fun_SWITCH_W pu_LambdaExp =
		con1 SWITCH_W (fn SWITCH_W a => a | _ => die "pu_LambdaExp.SWITCH_W")
		(convert (fn (sw,p) => {switch=sw,precision=p}, fn {switch=sw,precision=p} => (sw,p))
		 (pairGen0(pu_Switch word32 pu_LambdaExp,int)))
	    fun fun_SWITCH_S pu_LambdaExp =
		con1 SWITCH_S (fn SWITCH_S a => a | _ => die "pu_LambdaExp.SWITCH_S")
		(pu_Switch string pu_LambdaExp)
	    fun fun_SWITCH_C pu_LambdaExp =
		con1 SWITCH_C (fn SWITCH_C a => a | _ => die "pu_LambdaExp.SWITCH_C")
		(pu_Switch pu_con_lvopt pu_LambdaExp)
	    fun fun_SWITCH_E pu_LambdaExp =
		con1 SWITCH_E (fn SWITCH_E a => a | _ => die "pu_LambdaExp.SWITCH_E")
		(pu_Switch pu_excon_lvopt pu_LambdaExp)
	    fun fun_PRIM pu_LambdaExp =
		con1 PRIM (fn PRIM a => a | _ => die "pu_LambdaExp.PRIM")
		(pairGen0(pu_prim,listGen pu_LambdaExp))
	    fun fun_FRAME pu_LambdaExp =
		con1 FRAME (fn FRAME a => a | _ => die "pu_LambdaExp.FRAME")
		pu_frame
	    
	in dataGen("LambdaExp.LambdaExp",toInt,[fun_VAR,
			  fun_INTEGER,
			  fun_WORD,
			  fun_STRING,
			  fun_REAL,
			  fun_FN,
			  fun_LET,
			  fun_FIX,
			  fun_APP,
			  fun_EXCEPTION,
			  fun_RAISE,
			  fun_HANDLE,
			  fun_SWITCH_I,
			  fun_SWITCH_W,
			  fun_SWITCH_S,
			  fun_SWITCH_C,
			  fun_SWITCH_E,
			  fun_PRIM,
			  fun_FRAME])
	end

  end
