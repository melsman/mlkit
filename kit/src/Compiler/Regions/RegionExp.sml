
functor RegionExp(
  structure Flags: FLAGS
  structure Con: CON
  structure Excon: EXCON
  structure Eff: EFFECT
  structure R: RTYPE
    sharing type R.place = Eff.place
  structure TyName: TYNAME
  structure Crash: CRASH
  structure PP: PRETTYPRINT
    sharing type PP.StringTree = R.StringTree = Eff.StringTree
  structure Lvar : LVARS
  structure Lam: LAMBDA_EXP
    sharing type Lam.lvar = Lvar.lvar
    sharing type Lam.tyvar = R.tyvar
) : REGION_EXP = 
struct

    fun uncurry f (a,b) = f a b

    fun die s  = Crash.impossible ("RegionExp." ^ s)

    type lvar = Lvar.lvar
    type con = Con.con
    type excon = Excon.excon
    type TyName = TyName.TyName
    type place = Eff.place
    type effect = Eff.effect
    type cone = Eff.cone
    type tyvar = Lam.tyvar

    type Type = R.Type
     and sigma  = R.sigma
     and il = R.il
     and coneLayer = Eff.coneLayer

    datatype constructorKind = CONSTANT | VALUE_CARRYING
    datatype datbinds = DATBINDS of (TyName * (con * constructorKind * sigma) list) list list

    datatype metaType =          
                            (* describes normal expressions: *)
        Mus of (Type*place) list       
                            (* To allow the result of a declaration: *)  
      | Frame of {declared_lvars: {lvar : lvar,
				   compound : bool,
				   create_region_record : bool, 
                                   sigma: sigma ref,
                                   place: place}list,
		  declared_excons: (excon* (Type*place)option) list}
      | RaisedExnBind (* to be a raised Bind exception. *)


    datatype ('a,'b) LambdaPgm = PGM of 
           {expression: ('a,'b)trip,
            export_datbinds: datbinds,
            export_basis: effect list}

      (* list of mutual recursive datatype declarations *)

    and ('a,'b)trip = TR of ('a,'b)LambdaExp * metaType * effect
    and ('a,'b)LambdaExp =
        VAR      of {lvar: lvar, il_r : (il * (il * cone -> il * cone)) ref, alloc: 'a option}
      | INTEGER  of int * 'a            
      | STRING   of string * 'a
      | REAL     of string * 'a
      | UB_RECORD of ('a,'b) trip list 
      | FN       of {pat : (lvar * (Type*place)) list, 
                     body : ('a,'b)trip,
                     alloc: 'a}
      | LETREGION_B of {B: effect list ref, discharged_phi: effect list ref, body: ('a,'b)trip}
      | LET      of {pat : (lvar * tyvar list * Type * place) list,
                     bind : ('a,'b)trip,
                     scope: ('a,'b)trip}
      | FIX      of {shared_clos: 'a,
                     functions : {lvar : lvar, 
                                  occ  : (il * (il * cone -> il * cone)) ref list ref,
                                  tyvars : tyvar list,
                                  rhos: place list ref,
                                  epss: effect list ref,
                                  Type : Type,
                                  formal_regions: 'b list option,
                                  bind : ('a,'b)trip} list,
                     scope : ('a,'b)trip}
      | APP      of ('a,'b)trip * ('a,'b)trip
      | EXCEPTION of excon * bool * (Type*place) * 'a * ('a,'b)trip
                             (* Type*place: of exception constructor 
                                bool: true if exception is nullary *)
      | RAISE    of ('a,'b)trip
      | HANDLE   of ('a,'b)trip * ('a,'b)trip
      | SWITCH_I of ('a,'b,int) Switch 
      | SWITCH_S of ('a,'b,string) Switch 
      | SWITCH_C of ('a,'b,con) Switch 
      | SWITCH_E of ('a,'b,excon) Switch 
      | CON0     of {con : con, il : il, aux_regions: 'a list, alloc: 'a}
      | CON1     of {con : con, il : il, alloc: 'a} * ('a,'b)trip
      | DECON    of {con : con, il : il} * ('a,'b)trip
      | EXCON    of excon * ('a * ('a,'b)trip) option     (* nullary excons are looked up in dyn env. *)
      | DEEXCON  of excon * ('a,'b)trip
      | RECORD   of 'a * ('a,'b)trip list
      | SELECT   of int * ('a,'b)trip    
      | DEREF    of ('a,'b)trip
      | REF      of 'a * ('a,'b)trip
      | ASSIGN   of 'a * ('a,'b)trip * ('a,'b)trip
      | EQUAL    of {mu_of_arg1: Type * place , mu_of_arg2: Type*place, alloc: 'a} * ('a,'b)trip * ('a,'b)trip
      | CCALL    of {name : string,
		     mu_result : Type * place, 
		     rhos_for_result : ('a * int option) list}
	            * ('a,'b)trip list
      | RESET_REGIONS of {force: bool, alloc : 'a, regions_for_resetting: 'a list} * ('a,'b)trip     (* for programmer-directed resetting of regions;
								      * resetting is forced iff "force" is true.
								      * Forced resetting is not guaranteed to be sound *)
      | FRAME    of {declared_lvars: {lvar : lvar, 
                                      sigma: sigma ref,
                                      place: place} list,
                     declared_excons: (excon * (Type*place) option) list}
                       (* a frame is the result of a structure-level
                        * declaration. 
                        *)

    and ('a,'b,'c) Switch = SWITCH of ('a,'b)trip *
                   ('c * ('a,'b)trip) list * ('a,'b)trip option

  fun cons_if_there(NONE, l) = l
    | cons_if_there(SOME x, l) = x::l 

  (* mkPhiTr(tr) traverses tr and collects the allow effects
     that are bound locally in tr (in LET, FIX and LETREGION). 
     It also collects all effect nodes that decorate subexpressions.
   *)

  fun mkPhiTr(TR(e,_,phi)) acc = mkPhiExp e (phi::acc)
  and mkPhiExp(e) acc = 
    let fun mkPhiSw(SWITCH(tr0, l, opt))acc  =
                mkPhiTr tr0 (foldl (uncurry mkPhiTr) acc
                              (cons_if_there(opt,map #2 l)))
    in
      case e of
        UB_RECORD(ts) => foldl (uncurry mkPhiTr) acc ts
      | FN {body, ...} => mkPhiTr body acc
      | LETREGION_B{B: effect list ref, body: ('a,'b)trip, ...} =>
           mkPhiTr body (!B @ acc)
      | LET{pat, bind,scope} => mkPhiTr scope (mkPhiTr bind acc)
      | FIX{shared_clos,functions,scope} =>
          let 
            val acc' = foldl (fn ({epss as ref arreffs,bind, ...}, acc) =>
                                       mkPhiTr bind (arreffs @ acc))
                                   acc
                                   functions
          in
              mkPhiTr scope acc'
          end
      | APP(tr1, tr2) => mkPhiTr tr1 (mkPhiTr tr2 acc)
      | EXCEPTION(_,_,_,_, tr) => mkPhiTr tr acc
      | RAISE tr =>  mkPhiTr tr acc 
      | HANDLE(tr1, tr2) => mkPhiTr tr1 (mkPhiTr tr2 acc)
      | SWITCH_I sw => mkPhiSw sw acc
      | SWITCH_S sw => mkPhiSw sw acc
      | SWITCH_C sw => mkPhiSw sw acc
      | SWITCH_E sw => mkPhiSw sw acc
      | CON0 _ => acc
      | CON1 (_,tr) => mkPhiTr tr acc
      | DECON (_,tr) => mkPhiTr tr acc
      | EXCON (_,NONE) => acc
      | EXCON (_,SOME(_,tr)) => mkPhiTr tr acc
      | DEEXCON (_,tr) => mkPhiTr tr acc
      | RECORD (_,trs) => foldl (uncurry mkPhiTr) acc trs
      | SELECT (_,tr) => mkPhiTr tr acc
      | DEREF tr => mkPhiTr tr acc
      | REF (_,tr) => mkPhiTr tr acc
      | ASSIGN (_,tr1,tr2) => mkPhiTr tr1 (mkPhiTr tr2 acc)
      | EQUAL (_,tr1,tr2) => mkPhiTr tr1 (mkPhiTr tr2 acc)
      | CCALL (_,trs) => foldl (uncurry mkPhiTr) acc trs
      | RESET_REGIONS (_, tr) => mkPhiTr tr acc
      | FRAME _ => acc
      | _ => acc
  end

  fun mkPhi(tr,exported_regvars_and_arroweffects) = 
        mkPhiTr tr exported_regvars_and_arroweffects

  fun countletregions _ = ()


  (*****************************)
  (*                           *)
  (* Pretty printing           *)
  (*                           *)
  (*****************************)

  fun isWordRegion(rho) = 
        case Eff.get_place_ty rho of
          SOME Eff.WORD_RT => true
        | _ => false

  type StringTree = PP.StringTree
  fun layPair(t1,t2) = PP.NODE{start = "(", finish = ")", indent = 1, childsep = PP.RIGHT", ", 
                               children = [t1, t2]}

  fun get_opt l = foldr (fn (opt, acc) => 
                         case opt of SOME t => t::acc | NONE => acc) [] l


  val printcount = ref 1  (* controls when effects are printed *)

  local
    val (layTau, layMu) = R.mk_layout false  (* do not omit region info in frames *)
    fun layMus mus = PP.NODE{start = "(", finish = ")", indent = 1, childsep = PP.RIGHT",", 
                            children = map layMu mus}
  in
    fun layout_declared_lvar{lvar, sigma = ref sigma, place} =
      if not(!Flags.print_word_regions) andalso isWordRegion place then
         PP.NODE{start = Lvar.pr_lvar lvar ^ ": ", finish = "",
                indent = 5, childsep = PP.NOSEP, children = [R.mk_lay_sigma false sigma]}
      else
         PP.NODE{start = Lvar.pr_lvar lvar ^ ": (", finish = ")",
                indent = 5, childsep = PP.RIGHT",", children = [R.mk_lay_sigma false sigma,
                                                             Eff.layout_effect place]}

    fun layout_declared_lvar'{lvar, compound, create_region_record, sigma, place} =
      layout_declared_lvar{lvar=lvar,sigma=sigma,place=place}

    fun layout_declared_excon(excon,mu_opt) = PP.LEAF(Excon.pr_excon(excon))

    fun layMeta (Mus mus) = layMus mus
      | layMeta (Frame{declared_lvars, declared_excons}) = 
              let val l1 = map layout_declared_lvar'  declared_lvars
                  val l2 = map layout_declared_excon declared_excons
              in PP.NODE{start = "{|", finish = "|}", indent = 2, childsep = PP.RIGHT ", ", children = l1 @ l2}
              end
      | layMeta (RaisedExnBind) = PP.LEAF "raisedBind" 

  end;

  fun mkLay (omit_region_info: bool) (layout_alloc: 'a -> StringTree option)
                                     (layout_bind: 'b -> StringTree option) =
    let
      open PP

      fun alloc_string alloc = case (layout_alloc alloc) of SOME t => " " ^PP.flatten1 t | NONE => ""

      fun layList f l = NODE{start = "[", finish = "]", indent = 1, childsep = RIGHT ",",
                             children = map f l}
          

      fun layHlist f l = HNODE{start = "[", finish = "]", childsep = RIGHT ",",
                             children = map f l}

      infix ^^
      (*  s ^^ st_opt:   tag  the string tree option st_opt onto the string s *)
      fun s ^^ NONE = s
        | s ^^ (SOME st') = s ^ PP.flatten1(st')

      fun pp_fun_allocation a =
          case layout_alloc a of
            SOME st => PP.flatten1 st
          | NONE => ""

      val (layTau, layMu) = R.mk_layout omit_region_info
      fun layMus mus = NODE{start = "(", finish = ")", indent = 1, childsep = RIGHT",", 
                            children = map layMu mus}


      fun layVarMu(x,mu) = LEAF (concat[Lvar.pr_lvar x, ":",  PP.flatten1(layMu mu)])

      fun layPatFn  [] = LEAF("() => ")
        | layPatFn  [(x,mu)] = NODE{start = "", finish = "=>", indent = 0, childsep = NOSEP,
                                    children = [layVarMu(x,mu)]}
        | layPatFn  pat = HNODE{start = "(", finish = ") =>", childsep = RIGHT",", 
                              children = map layVarMu pat}

(*old
      fun layVarLet(lvar,_,[],[], tau) =
          NODE{start = Lvar.pr_lvar lvar ^ ":", finish = "", indent  =2 , childsep = RIGHT" ",
               children = [layTau tau]}
        | layVarLet(lvar,_,alphas,effs, tau) =
          NODE{start = Lvar.pr_lvar lvar ^ ":", finish = "", indent  =2 , childsep = RIGHT" ",
               children = [HNODE{start = "/\\", finish = "", childsep = NOSEP, 
                                 children = map (LEAF o Lam.pr_tyvar) alphas},
                           HNODE{start = "", finish = ". ", childsep = NOSEP, 
                                 children = map Eff.layout_effect effs},
                           layTau tau]}
old*)
     fun layVarSigma(lvar,alphas,rhos,epss, tau,p) =
         let val sigma_t = R.mk_lay_sigma' omit_region_info (alphas, rhos, epss, tau)
             val start:string = Lvar.pr_lvar lvar ^ " " ^
                                 (if !Flags.print_types then ":" else "")
             val sigma_rho_t = if !Flags.print_regions andalso !Flags.print_types andalso
				 (!Flags.print_word_regions orelse not(isWordRegion p)) then 
                                  NODE{start = "(", finish = ")", childsep = RIGHT",", 
                                       indent = 1, 
                                       children = [sigma_t, Eff.layout_effect p]} 
                               else sigma_t

         in PP.NODE{start = start, finish = "", indent = size start +1,
                    childsep = PP.NOSEP, children = [sigma_rho_t]}
         end



      fun layPatLet [] = LEAF("()")
        | layPatLet [one as (lvar,tyvars,tau,p)] = 
             layVarSigma(lvar,tyvars,[],[],tau,p)
        | layPatLet pat = HNODE{start = "(", finish = ")", childsep = RIGHT",", 
                              children = map (fn (lvar,tyvars,tau,p) => 
                                              layVarSigma(lvar,tyvars,[],[],tau,p)) pat}

    fun layoutSwitch laytrip show_const (SWITCH(lamb,rules,wildcardOpt)) =
      let
        fun child(x,lamb) =
          PP.NODE{start="",finish="",indent=0,
                  children=[PP.LEAF (show_const x),
                            laytrip(lamb,0)],
                  childsep=PP.RIGHT " => "}
        val t1 = PP.NODE{start="(case ",finish=" ",indent=6, childsep = PP.NOSEP, 
                         children=[laytrip(lamb,0)]}
        val t2 = PP.NODE{start = "of " , finish = ") (*case*) ", indent = 3,
                         childsep=PP.LEFT " | ",
                         children = (map child rules) @ 
                                  (case wildcardOpt of 
                                     NONE => []
                                   | SOME lamb => 
                                       [PP.NODE{start="",finish="",indent=0,
                                                children=[PP.LEAF "_",
                                                          laytrip(lamb,0)],
                                                childsep=PP.RIGHT " => "}])}

      in 
       PP.NODE{start = "", finish = "", indent = 0, childsep = PP.NOSEP, 
               children = [t1,t2]}
      end

      fun lay_il (lvar_string:string, terminator: string, il) : StringTree =
          let val (taus,rhos,epss)= R.un_il(il)
              val taus_opt = if !(Flags.print_types) then SOME(layList layTau taus) else NONE
              val rhos_opt = if !(Flags.print_regions) then SOME(layHlist Eff.layout_effect rhos) else NONE
              val epss_opt = if !(Flags.print_effects) then SOME(layList Eff.layout_effect epss) else NONE
          in
              case get_opt [taus_opt,rhos_opt,epss_opt] of
                [] => LEAF(lvar_string ^ terminator)
              | l => NODE{start = lvar_string ^ "(", finish = ")" ^ terminator, indent = 1, childsep = RIGHT", ",
                          children = l}
          end

      (* precedence levels: lam : 1
                            + - etc : 2
                            app   : 3 *)
              (* n is precedence of parent - or 0 if no parens around lamb are needed *)

      fun layBin(bop:string, n, t1, t2, a) =
         case layout_alloc a of
           NONE => (* put parenthesis, if precedence dictates it *)
                   if n>=2 then 
                      NODE{start = "(", finish = ")", indent = 1, childsep = PP.RIGHT bop, 
                           children = [layTrip(t1,2), layTrip(t2,2)]}
                   else
                      NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT bop, 
                           children = [layTrip(t1,2), layTrip(t2,2)]}
         | SOME t_alloc => (* assume allocation string is short: flatten it and use it as terminator *)
                   let val s_alloc = PP.flatten1 t_alloc
                   in NODE{start = "(", finish = ") " ^ s_alloc, indent =1, childsep = PP.RIGHT bop,
                           children = [layTrip(t1,2), layTrip(t2,2)]}
                   end

      and layExp(lamb: ('a, 'b) LambdaExp,n): StringTree =  
        case lamb of 
          VAR{lvar, il_r, alloc = NONE} => 
            (case R.un_il(#1(!il_r)) of 
               ([],[],[]) => LEAF(Lvar.pr_lvar lvar)
             | _ => lay_il(Lvar.pr_lvar lvar, "", #1(! il_r)))
(*old
(types,rhos,epss) => NODE{start = Lvar.pr_lvar lvar^"(", finish = ")", indent = 1, childsep = RIGHT", ",
                         children  = [layList layTau types,
                                      layHlist Eff.layout_effect rhos,
                                      layList Eff.layout_effect epss]})
*)
          
        | VAR{lvar, il_r, alloc = SOME a} => 
            lay_il(Lvar.pr_lvar lvar, " at" ^^ layout_alloc a, #1(! il_r))
          
        | INTEGER(i, a) => LEAF(Int.toString i ^^ layout_alloc a)
        | STRING(s, a) => LEAF(String.toString s ^^ layout_alloc a)
        | REAL(r, a) => LEAF(r ^^ layout_alloc a)
        | UB_RECORD(args) =>
            PP.NODE{start = "<", finish = ">" , indent = 1, childsep = PP.RIGHT", ", 
                    children = map (fn trip => layTrip(trip,0)) args}
        | CON0{con, il, aux_regions, alloc} => (* nullary constructor *)
            let
               val alloc_s = 
                   case (layout_alloc alloc) of SOME t => " " ^PP.flatten1 t | NONE => ""
            in PP.LEAF(Con.pr_con con ^ alloc_s) (*lay_il(Con.pr_con con, alloc_s, il)*)
            end
        | CON1({con, il, alloc}, trip) => (* unary constructor *)
            let
               val alloc_s = 
                   case (layout_alloc alloc) of SOME t => " " ^PP.flatten1 t | NONE => ""
               val t1 = PP.LEAF(Con.pr_con con^alloc_s) (* lay_il(Con.pr_con con, alloc_s, il)*)
            in 
             PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT " ", 
                     children = [t1, layTrip(trip,3)]}
            end
        | DECON({con, il},trip) => (* destruction *)
            let
               val t1 = PP.LEAF ("decon_"^Con.pr_con con) (*lay_il("decon_" ^ Con.pr_con con , "", il)*)
            in 
             PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT " ", 
                     children = [t1, layTrip(trip,3)]}
            end
            
        | EXCON(excon, NONE) => (* nullary exception constructor *)
             PP.LEAF(Excon.pr_excon excon)
        | EXCON(excon, SOME (alloc,t)) => (* unary exception constructor *)
             let 
               val alloc_s = alloc_string alloc
             in
               PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT " ", 
                       children = [PP.LEAF(Excon.pr_excon excon ^ alloc_s), layTrip(t,3)]}
            end
        | RECORD(alloc, args) =>
            let 
               val alloc_s = alloc_string alloc
            in
            PP.NODE{start = "(", finish = ")" ^ alloc_s, indent = 1, childsep = PP.RIGHT", ", 
                    children = map (fn trip => layTrip(trip,0)) args}
            end
        | SELECT(i, trip) =>
             PP.NODE{start = "#"^Int.toString i ^ " ", finish = "", indent = 4, childsep = PP.NOSEP,
                     children = [layTrip(trip,3)]}
        | FN{pat, body, alloc}=> layLam((pat,body,alloc), n, "")
        | APP(TR(VAR{lvar, il_r, alloc = SOME alloc},_,_), t2) =>
           let
                  (*        f il at rho (exp) 
                                      OR
                            f il at rho
                              (exp)                                     
                  *)

             val alloc_s = 
                   case (layout_alloc alloc) of SOME t => " " ^PP.flatten1 t | NONE => ""
             val t1 = lay_il(Lvar.pr_lvar lvar, alloc_s, #1(! il_r))
          in
             PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT " ", 
                     children = [t1, layTrip(t2,3)]}
          end
        | APP(t1, t2) =>
           NODE{start = if n>3 then "(" else "", 
                finish = if n>3 then ")" else "", 
                childsep = RIGHT " ", indent = 1,
                children = [layTrip(t1,3), layTrip(t2,4)]}
        | EXCEPTION _ => layout_let_fix_and_exception lamb
        | HANDLE(t1,t2) =>
           NODE{start = if n>=2 then "(" else "", 
                finish = if n>=2 then ")" else "", 
                childsep = RIGHT " handle ", indent = 1,
                children = [layTrip(t1,2), layTrip(t2,2)]}
        | RAISE(t1) =>            
           NODE{start = if n>=3 then "raise(" else "raise ", 
                finish = if n>=3 then ")" else "", 
                childsep = NOSEP, indent = 6,
                children = [layTrip(t1,2)]}
        | LET{pat, bind, scope} => layout_let_fix_and_exception lamb
        | FIX _ => layout_let_fix_and_exception lamb
        | REF(alloc, t) =>
            let val s = alloc_string alloc
            in  PP.NODE{start = if n>3 then "(ref " ^ s ^ " "
                                else "ref " ^ s ^ " ", 
                        finish = if n>3 then ")" else "", 
                        indent = 6, childsep = PP.NOSEP,
                        children = [layTrip(t,4)]}
            end
        | DEREF t =>
            PP.NODE{start = if n>3 then "(! " else " ! ",
                    finish = if n>3 then ")" else "", 
                    indent = 3, childsep = PP.NOSEP,
                    children = [layTrip(t,4)]}
        | ASSIGN(alloc,t1,t2) =>
            let val s = alloc_string alloc
            in  PP.NODE{start = "(" , finish = ")"^s, indent = 1, childsep = PP.RIGHT " := ",
                        children = [layTrip(t1,2), layTrip(t2,2)]}
            end
        | EQUAL({mu_of_arg1,mu_of_arg2, alloc},arg1,arg2) =>
            let val eq = if !Flags.print_regions then  "=" ^ alloc_string alloc else "="
                val ty = if !(Flags.print_types) 
                           then concat["(* domain of = is: ", 
                                        PP.flatten1(layMu mu_of_arg1), "*",
                                        PP.flatten1(layMu mu_of_arg2), " *)"]
                         else ""
            in
               PP.NODE{start= if n>=2 then "(" else "", 
                       finish = if n>=2 then ")" else "", 
                       indent = 0, childsep = PP.RIGHT (eq^ty),
                       children = [layTrip(arg1,2), layTrip(arg2, 2)]}
            end
        | CCALL ({name, mu_result, rhos_for_result}, args) =>
            PP.NODE {start = "ccall(", finish = "):"
		     ^ (if !Flags.print_types then PP.flatten1(layMu mu_result) else "")
		     ^ (if !Flags.print_regions then 
			  PP.flatten1(layHlist (PP.LEAF o alloc_string o #1) rhos_for_result)
			else ""),
	             indent = 6, childsep = PP.RIGHT ", ", 
		     children = PP.LEAF name :: (map (fn t => layTrip(t,0)) args)}
        | RESET_REGIONS({force, alloc,regions_for_resetting}, t) =>
           let val fcn = if force then "forceResetting " else "resetRegions "
               val aux_regions_t = HNODE{start="[",finish="]", childsep=NOSEP,
                            children=[layHlist  (fn a => PP.LEAF(alloc_string a)) regions_for_resetting]}
           in PP.NODE{start = "(" ^ fcn , finish = ")" ^ (if !Flags.print_regions then alloc_string alloc else ""),
                      indent = size fcn + 2, childsep = PP.NOSEP,
                      children = [aux_regions_t,layTrip(t,0)]}
           end
        | LETREGION_B{B = ref [], body,...} => layTrip(body,n) 
        | LETREGION_B{B, body,...} =>
            (case Eff.layoutEtas (!B)  of
               [] => layTrip(body,n)
            | binders =>
                 let 
                   val t1 = 
                     NODE{start = "letregion ", finish = " ", childsep = NOSEP, indent = 10,
                          children = [HNODE{start = "", finish = "", childsep = RIGHT", ", 
                                            children = binders}]}
                   val t2 = 
                     NODE{start = "in ", finish = "", childsep = NOSEP, indent = 3,
                          children = [layTrip(body,0)]}
                   val t3 = 
                     NODE{start = "end (*", finish = "*)", childsep = NOSEP, indent =  6,
                          children = [HNODE{start = "", finish = "", childsep = RIGHT", ", 
                                            children = binders}]}
                 in NODE{start = "", finish = "", indent = 0, childsep = NOSEP, children = [t1,t2,t3]}
                 end
               )
        | SWITCH_I(sw) => layoutSwitch layTrip Int.toString  sw
        | SWITCH_S(sw) => layoutSwitch layTrip (fn s => s) sw
        | SWITCH_C(sw) => layoutSwitch layTrip Con.pr_con sw
        | SWITCH_E(sw) => layoutSwitch layTrip Excon.pr_excon sw
        | FRAME{declared_lvars, declared_excons} =>
             let val l1 = map layout_declared_lvar  declared_lvars
                 val l2 = map layout_declared_excon declared_excons
             in NODE{start = "{|", finish = "|}", indent = 0, childsep = NOSEP, 
                     children = l1 @ l2}
             end
                                    
        | _ => LEAF "pretty-printing of this region expression not yet implemented"

      and layTrip(TR(e,Mus mus,rea),n) = 
        let val t1 = 
                case (e, mus) of
                  (FN{pat, body, alloc}, [(R.FUN(_,eps,_),_)])=> 
                    layLam((pat,body,alloc), n, 
                           PP.flatten1(Eff.layout_effect eps) ^ " ")
                | _ => layExp(e,n)
            val tick = (printcount:= !printcount+1; !printcount)
        in
           t1
           (* if false (*tick mod 10 = 0*) then*)
(*for more info:
           case e of 

             LETREGION_B _ =>          (*mads: comment out "LETREGION_B" to get effects on all subexpressions*)
              let (*val t2 = layMus mus*)
                  val t3 = Eff.layout_effect_deep rea
              in  NODE{start = "(", finish = ")", indent = 1, childsep = RIGHT":", 
                       children = [t1,(*t2,*)t3]}
              end
           | _ => t1
(*            else t1*)
for more info*)
        end
        | layTrip(TR(e, _, rea), n) = layExp(e,n)


      and layLam((pat,body,alloc), n, eps: string) = 
           (* (fn eps alloc pat =>  
               lamb
              )
           *)
          let 
             val start_s = concat["fn ", eps, pp_fun_allocation alloc, " "]
             val pat_t = layPatFn pat
             val first_line = NODE{start = start_s, finish = "", indent = size(start_s),
                                   children = [pat_t], childsep = NOSEP}
          in
              PP.NODE{start= if n>1 then "(" else "", 
                      finish=if n>1 then ")" else "", 
                      indent=1, childsep = PP.NOSEP,
                      children=[first_line,layTrip(body,1)]}
          end
      and layout_let_fix_and_exception lexp =
          let 
            val inInfo = ref ""
            fun layout_rec lexp = 
                  case lexp of 
                    LET{pat, bind, scope = t2 as TR(e2,_,_)} =>
                        let
                          val (binds, body) = layout_rec e2
                          val _ = inInfo := "(* let *)"
                        in
                           (mk_valbind(pat,bind)::binds, body)
                        end
                  | FIX({shared_clos,functions,scope = t2 as TR(e2, _,_)}) =>
                        let
                          val (binds', body) = layout_rec e2
                          val _ = inInfo := "(* fix *)"
                        in
                          (mk_mutual_binding (layout_alloc shared_clos,rev functions):: binds', body)
                        end
                  | EXCEPTION(excon, nullary, mu, alloc, scope as TR(e2, _,_)) =>
                        let 
                          val (binds', body) = layout_rec e2
                          val _ = inInfo := "(* exn *)"
                        in
(*                        ((append_info_with_name omit_region_info " (* exn value or name " " *)" info  *)
                           (mk_excon_binding(excon,nullary, layout_alloc alloc, mu)::binds', body)
                        end

                  | LETREGION_B{B = ref [], body as TR(e',_,_),...} => layout_rec e'
                  | LETREGION_B{B , body as TR(e',_,_),...} => 
                      (case Eff.layoutEtas(!B) of  [] => layout_rec e'
                       | _ => ([], layExp(lexp,0)))
                  | _ => ([],layExp(lexp,0))

           val (l, body) = layout_rec lexp
           val bindings =  NODE{start = "", finish = "", childsep = RIGHT "; ", indent = 0, children = l}
          in
            PP.NODE{start= "let ",
                    finish=" end " (* ^ (!inInfo) *),   (*martin*)
                    indent=4,
                    children=[bindings,body],
                    childsep=LEFT (" in " (* ^ (!inInfo) *) )} (*martin*)
          end

      and mk_valbind(pat, t) = 
        let 
            val child1 = layPatLet pat
         in
            NODE{start = "val ",finish="",childsep=RIGHT " = ",
                 indent=4,  children=[child1, layTrip(t,0)] }
        end
      and mk_excon_binding(excon, nullary, alloc_t, mu) = 
            (* exception EXCON : mu  (* exn value or name at RHO *) or
               excpetion EXCON : mu 
            *)
         (case alloc_t of
            NONE =>  NODE{start = "exception ",finish="",childsep=RIGHT " : ",
                 indent=4,  children=[LEAF(Excon.pr_excon excon), layMu mu] }
          | SOME t => NODE{start = "exception ",finish="",childsep=RIGHT " ",
                 indent=4,  children=[LEAF(Excon.pr_excon excon), LEAF ":", layMu mu, 
                                      LEAF("(* exn value or name " ^ PP.flatten1 t ^ " *)")]}
        )
      and  mk_mutual_binding(opt_alloc, functions) = 
        let fun mk_fix({lvar,occ,tyvars,rhos = ref rhos,epss= ref epss,Type, formal_regions, bind as TR(FN{pat, body, ...},_,_)})
                     (no, rest_of_mutual_binding) =
              (*   
                   fun f at rho : sigma  
                       (x_1, ..., x_n) = 
                       body
                            OR
                   fun f at rho (rho1, ..., rho_k, x_1, ..., x_n)  = body   
                            OR
                   fun f at rho (rho1, ..., rho_k, x_1, ..., x_n)  = 
                         body
                            OR
                   fun f at rho(rho1, ..., rho_k, 
                                x1:mu_1,
                                ...
                                xn: mu_n
                               )  = 
                         body   
              *)
              (no-1,
                (case formal_regions of
                   NONE=>
                    let
                     val sigma_t = R.mk_lay_sigma' omit_region_info (tyvars,rhos,epss,Type)
                     val alloc_s = case opt_alloc of NONE => "" | SOME t => PP.flatten1 t
                     val t1 = let val s: string = Lvar.pr_lvar lvar ^ " " ^ alloc_s ^ 
                                 (if !Flags.print_types then ":" else "")
                              in  PP.NODE{start = s, finish = "", indent = size s +1,
                                          childsep = PP.NOSEP, children = [sigma_t]}
                              end
                     val formals = PP.HNODE{start="(", finish = ") =", childsep = PP.RIGHT ", ", 
                                                  children = map (fn (lvar,_) => PP.LEAF(Lvar.pr_lvar lvar))
                                                                 pat}
                     val keyword = if no = 1 then "fun " else "and "
                     val body_t = PP.NODE{start = "", finish ="", indent = 4, childsep = PP.NOSEP,
                                          children = [layTrip(body, 0)]}
                    in
                      PP.NODE{start = keyword , finish = "", indent = 4, childsep = PP.NOSEP, 
                              children = [t1, formals, body_t]}
                    end

                 | SOME formals => 
                 let
                     fun layout_bind' b = case layout_bind b of SOME t => t | _ => die ".layout_bind'"
                     val region_binder_trees = PP.HNODE{start="", finish = "", childsep = PP.RIGHT ", ", 
                                                  children = map layout_bind' formals}
                     val formals_t = PP.NODE{start = "(", finish = ") = ", indent = 1, childsep = PP.RIGHT", ",
                                           children = region_binder_trees :: map layVarMu pat}
                     val alloc_s = case opt_alloc of
                           NONE => ""
                         | SOME t => PP.flatten1 t

                     val fun_f = 
                       (if no = 1 then
                          "fun " ^ Lvar.pr_lvar lvar ^ alloc_s
                        else
                          "and " ^ Lvar.pr_lvar lvar) ^ alloc_s

                     val header = PP.NODE{start = fun_f, finish ="", indent = 0, childsep = PP.NOSEP,
                                          children = [formals_t]}
                     val body_t = PP.NODE{start = "", finish ="", indent = 4, childsep = PP.NOSEP,
                                          children = [layTrip(body, 0)]}
                 in
                   PP.NODE{start = "" , finish = "", indent = 0, childsep = PP.NOSEP, 
                           children = [header, body_t]}

                 end
              )
                  :: rest_of_mutual_binding
            )
            | mk_fix _ _ = die "mk_fix: rhs of fix does not begin with lambda"
       in
        PP.NODE{start = "", finish = "", indent = 0,
                childsep = PP.NOSEP, 
                children = #2(foldl (uncurry mk_fix) (length functions,[]) functions)}
       end

    in
       (fn e => layExp(e,0),
       fn t => layTrip(t,0),
       layMus,
       layMeta)
    end


  fun layoutLambdaExp(layout_alloc: ('a -> StringTree option))
                       (layout_binder: ('b -> StringTree option))
                       (e: ('a, 'b)LambdaExp) :StringTree = 
            #1(mkLay(not(!Flags.print_regions))
                  layout_alloc layout_binder) e

  fun layoutLambdaExp' e  =
      layoutLambdaExp
       (if !Flags.print_regions 
          then (fn rho => SOME(PP.LEAF("at " ^ PP.flatten1(Eff.layout_effect rho))))
        else (fn _ => NONE))
       (fn _ => NONE) 
       e


  fun layoutLambdaTrip(layout_alloc: ('a -> StringTree option))
                       (layout_binder: ('b -> StringTree option))
                       (t: ('a, 'b)trip) :StringTree = 
            #2(mkLay(not(!Flags.print_regions))
                       layout_alloc layout_binder) t

  fun layoutLambdaPgm(layout_alloc: ('a -> StringTree option))
                       (layout_binder: ('b -> StringTree option))
                       (p: ('a, 'b)LambdaPgm as 
                            PGM{expression = TR(lamb,meta,rea),
                                 export_datbinds = DATBINDS dblist,
                                 export_basis}):StringTree = 
      let
        val layout_sigma = R.mk_lay_sigma  (not(!Flags.print_regions))
        val (layExp,layTrip,layMus,layMeta) = mkLay(not(!Flags.print_regions))
                                     layout_alloc layout_binder
        val layoutcb =
          map (fn (con,_,sigma) =>PP.NODE{start="",finish="",indent=0,
                            children=[PP.LEAF (Con.pr_con con),
                                      layout_sigma sigma],
                            childsep=PP.RIGHT " : "})
        fun layoutdb (tyname,cb) =
          let
            val tynameT = PP.LEAF(TyName.pr_TyName tyname)
            val cbT = PP.NODE{start="{",finish="}",indent=0,
                              children=layoutcb cb,
                              childsep=PP.RIGHT", "}
          in
            PP.NODE{start="",finish="",indent=0,
                    children=[tynameT,cbT],
                    childsep=PP.RIGHT" : "}
          end
        fun layoutMutualRec_db db =
          PP.NODE{start="EXPORT DATATYPE: ",finish="; ",indent=3,
                  children=map layoutdb db,childsep=PP.LEFT" and "}
        val dbTs = map layoutMutualRec_db dblist
        val lambT = layoutLambdaExp  layout_alloc layout_binder lamb
        val t1 = PP.NODE{start="",finish="",indent=0,
                         children=dbTs @ [lambT],childsep=PP.NOSEP}
        val t2 = PP.NODE{start = "META TYPE: ", finish = "", childsep = PP.NOSEP, indent = 2,
                         children = [layMeta meta]}
        val t3 = PP.NODE{start = "EFFECT: ", finish = "", childsep = PP.NOSEP, indent = 2,
                         children = [Eff.layout_effect_deep rea]}
        val t4 = PP.NODE{start = "EXPORT REGION BASIS: [", finish = "]", indent = 1, childsep = PP.RIGHT ", ",
                               children = Eff.layoutEtas export_basis}
      in
        PP.NODE{start = "", finish = "", indent = 0, childsep = PP.NOSEP, children = [t1,t4,t2,t3]}
      end

    fun normPgm(PGM{expression, export_datbinds, export_basis}, tick: unit -> int) =
    let
(*
       val count = ref initval
       fun tick()  = (count:= !count+1; !count)
*)
       val normVar = Eff.setkey tick

       fun normTrip(TR(e,_,_)) = norm e
       and norm e = 
         let 
           fun normsw(SWITCH(tr1,rhsides, tr_opt)) =
                (normTrip tr1;
                 app (fn (_, tr) => normTrip tr) rhsides;
                 case tr_opt of NONE => ()
                 | SOME tr => normTrip tr)
         in 
          case e of
             UB_RECORD ts => app normTrip ts
           | FN{body,...} => normTrip body
           | LETREGION_B{B,body,...} =>
               (app normVar (!B);
                normTrip body)
           | LET{bind, scope, ...} => (normTrip bind;  normTrip scope)
           | FIX{functions, scope, ...} =>
                (app (fn {rhos, epss, bind, ...} => 
                       (app normVar (!rhos); 
                        app normVar (!epss);
                        normTrip bind)) functions;
                 normTrip scope)
           | APP(tr1,tr2) => (normTrip tr1; normTrip tr2)
           | EXCEPTION(_,_,_,_,body) => normTrip body
           | RAISE(tr) => normTrip tr
           | HANDLE(tr1,tr2) => (normTrip tr1; normTrip tr2)
           | SWITCH_I(sw) => normsw sw
           | SWITCH_S(sw) => normsw sw
           | SWITCH_C(sw) => normsw sw
           | SWITCH_E(sw) => normsw sw
	   | CON0 _ => ()
	   | CON1 (_,tr) => normTrip tr
	   | DECON (_,tr) => normTrip tr
	   | EXCON (_,NONE) => ()
	   | EXCON (_,SOME(_,tr)) => normTrip tr
	   | DEEXCON (_,tr) => normTrip tr
	   | RECORD (_,trs) => app normTrip trs
	   | SELECT (_,tr) => normTrip tr
	   | DEREF tr => normTrip tr
	   | REF (_,tr) => normTrip tr
	   | ASSIGN (_,tr1,tr2) => (normTrip tr1; normTrip tr2)
	   | EQUAL (_,tr1,tr2) => (normTrip tr1; normTrip tr2)
	   | CCALL (_,trs) => app normTrip trs
	   | RESET_REGIONS (_, tr) => normTrip tr
           | FRAME{declared_lvars, ...} =>()
           | _ => ()
       end
    in
       (* re_number exported region and effect variables *)
       (*app (*(Eff.setkey tick)*) normVar export_basis; commented out; mads *)
       (* re_number bound variables in expression*)
       normTrip expression 
    end
  end
