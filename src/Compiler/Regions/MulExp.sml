(*$MulExp: CON EXCON EFFECT RTYPE TYNAME CRASH PRETTYPRINT LVARS LAMBDA_EXP REGION_EXP FLAGS REGION_EXP MUL_EXP MUL REGION_STAT_ENV*)
functor MulExp(
  structure Flags: FLAGS
  structure Con: CON
  structure Excon: EXCON
  structure RegionExp: REGION_EXP
    sharing type RegionExp.con = Con.con
    sharing type RegionExp.excon = Excon.excon
  structure Eff: EFFECT
  structure Mul: MUL
    sharing type Mul.effectvar = Eff.effect = RegionExp.effect
  structure R: RTYPE
    sharing type R.place = Eff.place = RegionExp.place = Mul.place
    sharing type R.Type = RegionExp.Type
    sharing type R.il = RegionExp.il
    sharing type R.sigma = RegionExp.sigma
  structure TyName: TYNAME
    sharing type TyName.TyName = RegionExp.TyName
  structure Crash: CRASH
  structure PP: PRETTYPRINT
    sharing type PP.StringTree = R.StringTree = Eff.StringTree = Mul.StringTree = RegionExp.StringTree
  structure Lvar : LVARS
  structure Lam: LAMBDA_EXP
    sharing type Lam.lvar = Lvar.lvar = RegionExp.lvar  = Mul.lvar
        and type Lam.tyvar = R.tyvar = RegionExp.tyvar
  structure RSE: REGION_STAT_ENV
    sharing type RSE.excon = Excon.excon 
        and type RSE.lvar = Lvar.lvar
        and type RSE.TypeAndPlaceScheme = R.sigma
        and type RSE.Type = R.Type
        and type RSE.place = R.place 
) : MUL_EXP = 
struct
    structure RegionExp = RegionExp

    fun say s = (output(std_out, s ^"\n"); output(!Flags.log, s^ "\n"))
    fun say' s = (output(std_out, s ); output(!Flags.log, s))
    fun outtree t = PP.outputTree(say', t, !Flags.colwidth)

    val print_K_normal_forms = Flags.lookup_flag_entry "print_K_normal_forms"
    val show_K = print_K_normal_forms
    fun die s  = Crash.impossible ("MulExp." ^ s)

    type lvar = Lvar.lvar
    type con = Con.con
    type excon = Excon.excon
    type TyName = TyName.TyName
    type place = Eff.place
    type ateffect = Eff.effect
    type effect = Eff.effect
    type effectvar = effect
    type cone = Eff.cone
    type mul = Mul.mul
    type mulef = Mul.mulef
    type tyvar = Lam.tyvar
    type dependency_map = Mul.dependency_map
    type mularefmap = Mul.mularefmap
    type qmularefset = Mul.qmularefset
    type efenv = Mul.efenv
    type mularef = Mul.mularef

    type Type = R.Type
     and sigma  = R.sigma
     and il = R.il
     and coneLayer = Eff.coneLayer

    type constructorKind = RegionExp.constructorKind
    type datbinds = RegionExp.datbinds
    type metaType = RegionExp.metaType

    datatype callKind = JMP      (* tail call     to fix-bound function *)
                      | FUNCALL  (* non-tail call to fix-bound function *)
                      | FNJMP    (* tail call     to non-fix-bound function *)
                      | FNCALL   (* non-tail call to non-fix-bound function *)

    datatype saveRestore = NOT_YET_DETERMINED
                         | SR of {store: lvar list, fetch: lvar list}

    fun layout_sr NOT_YET_DETERMINED = None
      | layout_sr (SR{store, fetch}) =
          let val t1 = PP.HNODE{start = "{store: ", finish = "}", childsep = PP.RIGHT " ",
                          children = map (PP.LEAF o Lvar.pr_lvar) store}
              val t2 = PP.HNODE{start = "{fetch: ", finish = "}", childsep = PP.RIGHT " ",
                          children = map (PP.LEAF o Lvar.pr_lvar) fetch}
          in Some(t1,t2)
          end

    datatype ('a,'b,'c) LambdaPgm = PGM of 
                        {expression:('a,'b,'c)trip,
                         export_datbinds: datbinds,
			 import_vars: (lvar list * excon list * place list) Option ref,
			 export_vars: lvar list * excon list * place list, 
                         export_basis: effect list,  (* list of region variables and arrow effects *)
                         export_Psi:   mularef ref list
                        }

      (* list of mutual recursive datatype declarations *)

    and ('a,'b,'c)trip = TR of ('a,'b,'c)LambdaExp * metaType * ateffect list * mulef ref

    and ('a,'b,'c)LambdaExp =
        VAR      of {lvar: lvar, il: il, plain_arreffs: (effectvar * ateffect list) list,
                     alloc: 'a Option, rhos_actuals: 'a list ref, other: 'c}
      | INTEGER  of int	* 'a		
      | STRING   of string * 'a
      | REAL     of real * 'a
      | UB_RECORD of ('a,'b,'c) trip list (* unboxed records *)

      | FN       of {pat : (lvar * (Type*place)) list, 
                     body : ('a,'b,'c)trip,
		     free: (lvar list * excon list * place list) Option ref, 
                     alloc: 'a}

      | LETREGION of {B: effect list ref,  (* contains both region variables and arrow effects *)
                      rhos: 'b list ref,   (* LETREGION-bound region variables *)
                      body: ('a,'b,'c)trip}

      | LET      of {k_let: bool,
                     pat : (lvar * il ref list ref * tyvar list * 
                                   effect list ref * Type * place * 'c) list,
		     bind : ('a,'b,'c)trip,
		     scope: ('a,'b,'c)trip}
      | FIX      of {free: (lvar list * excon list * place list) Option ref, 
		     shared_clos: 'a,
                     functions : {lvar : lvar, 
                                  occ : il list,                        (* instantiation lists              *)
                                                                        (* at non-binding occurrences of il *)
				  tyvars : tyvar list,            (* original *)
                                  rhos: place list,               (* region   *)
                                  epss: effect list,              (* type     *)
				  Type : Type,                    (* scheme.  *)
				  rhos_formals: 'b list ref,
                                  bound_but_never_written_into: 'b list Option,
                                  other:  'c,
				  bind : ('a,'b,'c)trip} list,
		     scope : ('a,'b,'c)trip}

      | APP      of callKind Option * saveRestore * ('a,'b,'c)trip * ('a,'b,'c)trip

      | EXCEPTION of excon * bool * (Type*place)  * 'a * ('a,'b,'c)trip
                             (* Type*place: of exception constructor 
                                bool: true if exception is nullary *)
      | RAISE    of ('a,'b,'c)trip
      | HANDLE   of ('a,'b,'c)trip * ('a,'b,'c)trip
      | SWITCH_I of ('a,'b,'c,int)    Switch 
      | SWITCH_S of ('a,'b,'c,string) Switch 
      | SWITCH_C of ('a,'b,'c,con)    Switch 
      | SWITCH_E of ('a,'b,'c,excon)  Switch 
      | CON0     of {con : con, il : il, aux_regions: 'a list, alloc: 'a}
      | CON1     of {con : con, il : il, alloc: 'a} * ('a,'b,'c)trip
      | DECON    of {con : con, il : il} * ('a,'b,'c)trip
      | EXCON    of excon * ('a * ('a,'b,'c)trip) Option     (* nullary excons are looked up in dyn env. *)
      | DEEXCON  of excon * ('a,'b,'c)trip
      | RECORD   of 'a * ('a,'b,'c)trip list
      | SELECT   of int * ('a,'b,'c)trip    
      | DEREF    of ('a,'b,'c)trip
      | REF      of 'a * ('a,'b,'c)trip
      | ASSIGN   of 'a * ('a,'b,'c)trip * ('a,'b,'c)trip
      | EQUAL    of {mu_of_arg1: Type * place , mu_of_arg2: Type*place, alloc: 'a} * ('a,'b,'c)trip * ('a,'b,'c)trip
      | CCALL    of {name : string,
		     mu_result : Type * place, (*mu of result from c function*)
		     rhos_for_result : ('a * int Option) list}
	            * ('a,'b,'c)trip list  (* Calling C functions *)
      | RESET_REGIONS of {force: bool, alloc : 'a,regions_for_resetting: 'a list} * ('a,'b,'c)trip     (* for programmer-directed resetting of regions;
									 * resetting is forced iff "force" is true.
									 * Forced resetting is not guaranteed to be sound *)
      | FRAME    of {declared_lvars: {lvar: lvar, 
                                      sigma: sigma,
                                      other: 'c,
                                      place: place} list,
                     declared_excons: (excon * (Type*place) Option) list}
                       (* a frame is the result of a structure-level
                        * declaration. 
			*)

    and ('a,'b,'c,'d) Switch = SWITCH of ('a,'b,'c)trip * 
                                      ('d * ('a,'b,'c)trip) list * ('a,'b,'c)trip Option



  (**********************************)
  (* Reporting escaping Put effects *)
  (**********************************)

    (* "warn_puts TE e" traverses e and examines the type schemes of all FIX-bound lvars.
   *  For those lvars whose type scheme contains a free put-effect, a warning is printed.
   *  For each escaping region variable, the entire region-static environment is traversed,
   *  and all identifiers (lvars and excons) whose type (scheme) contains rho free, are
   *  listed in the warning message.
   *)

  fun warn(s) =  Flags.warnings:= s :: (!Flags.warnings)

  val already_reported: R.place list ref = ref [];  (* those region variables rho, for which other
						     * lvars with rho free in their type and place 
						     * have already been reported once *)

  fun pp_regvar rho =  PP.flatten1(Eff.layout_effect rho)
  fun flatten [] = []
    | flatten (l::rest) = l @ flatten rest

  type regionStatEnv = RSE.regionStatEnv
  fun warn_if_escaping_puts(TE, lvar, sigma): unit =
        case R.free_puts sigma of
          [] => ()
        | rhos => 
         let fun report_rho rho : string list= 
               if List.exists (fn rho' => Eff.eq_effect(rho,rho')) (!already_reported)
                  then [pp_regvar rho ^ " (see above)\n" ]
               else
                 let val _ = already_reported:= rho :: (!already_reported)
                     val excons_rho = 
                       RSE.FoldExcon (fn ((excon, mu), l: string list) =>
				      if List.exists (fn rho' => Eff.eq_effect(rho,rho'))(R.frv_mu mu)
                                      then Excon.pr_excon excon :: l
				      else l)[] TE 
		     val lvars_and_excons_rho = 
                       RSE.FoldLvar (fn ((lvar, (_,_,sigma,p,_,_)), l: string list) =>
				      if Eff.eq_effect(rho,p) orelse
                                         List.exists (fn rho' => Eff.eq_effect(rho,rho'))
                                         (R.frv_sigma sigma)
                                      then Lvar.pr_lvar lvar :: l
				      else l
				     ) excons_rho TE 

                 in
		   (pp_regvar rho::
		    ", which is also free in the type (schemes) of : "::
		    map (fn s => " " ^ s) lvars_and_excons_rho) @ ["\n"]
		 end
	 in
            warn(implode([Lvar.pr_lvar lvar,
			  "\t has a type scheme with escaping put effects\
			   \ on region(s): \n"]@
                          flatten(map report_rho rhos)))
	 end

    fun warn_puts (TE:regionStatEnv, 
                   (PGM{expression = TR(e,_,_,_), ...}):(place,'a,'b) LambdaPgm ):unit = 
    if not(Flags.is_on "warn_on_escaping_puts")
      then ()
    else      
      let
        val _ = already_reported:= []  (* reset *)
	fun warn_puts TE e = 
	  case e of
	    FIX{shared_clos, functions, scope, ... (*bound_lvars,binds,scope,info*)} => 
	         let val TE' = 
	                List.foldR (fn {lvar,tyvars,rhos,epss,Type,...} => fn TE' => 
			   RSE.declareLvar(lvar, (true,true,R.FORALL(tyvars,rhos,epss,Type), shared_clos , None, None), TE'))
			TE functions
	
	             fun warn_lvar {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
                                    bound_but_never_written_into,other,bind} =
	                  let val sigma = R.FORALL(tyvars,rhos,epss,Type)
	                  in
	                     warn_if_escaping_puts(TE, lvar, sigma);
			     warn_puts_trip TE' bind
	                  end
	         in
	             List.apply warn_lvar functions;
		     warn_puts_trip TE' scope
	         end
             | FN{pat,body,...} =>
                let val TE' = List.foldR (fn (lvar,(tau,rho)) => fn TE' => 
                    RSE.declareLvar(lvar, (true,true,R.type_to_scheme tau, rho , None, None), TE'))
			TE pat
                in 
                   warn_puts_trip TE' body
                end
             | LET{k_let,pat,bind,scope} =>
                   (warn_puts_trip TE bind;
                    let 
                        val TE' = List.foldR (fn (lvar,_,tyvars,ref epss,tau,rho,_)  => fn TE' => 
			   RSE.declareLvar(lvar, (true,true,R.FORALL(tyvars,[],epss,tau), rho , None, None), TE'))
                           TE
            		   pat
                   in warn_puts_trip TE' scope
                   end
                  )
	     | APP(_,_,e1,e2) => (warn_puts_trip TE e1; warn_puts_trip TE e2)           
	     | EXCEPTION(excon, is_nullary, (tau,p), _, body) => 
		     warn_puts_trip (RSE.declareExcon(excon,(tau,p),TE)) body
	     | RAISE(e) => warn_puts_trip TE e
	     | HANDLE(e1,e2) => (warn_puts_trip TE e1; warn_puts_trip TE e2)
	     | SWITCH_I(switch) => warn_puts_i TE switch
	     | SWITCH_S(switch) => warn_puts_s TE switch
	     | SWITCH_C(switch) => warn_puts_c TE switch
	     | SWITCH_E(switch) => warn_puts_e TE switch
             | CON0 _ => ()
             | CON1(_,tr) => warn_puts_trip TE tr
             | DECON(_,tr) => warn_puts_trip TE tr
             | EXCON(_,Some(_, tr)) => warn_puts_trip TE tr
             | DEEXCON(_,tr) =>warn_puts_trip TE tr
             | RECORD(_,l) => List.apply (warn_puts_trip TE) l
             | UB_RECORD l => List.apply (warn_puts_trip TE) l
             | SELECT(_,tr) => warn_puts_trip TE tr
             | DEREF tr => warn_puts_trip TE tr
             | REF(_,tr) => warn_puts_trip TE tr
             | ASSIGN(_,tr1,tr2) => (warn_puts_trip TE tr1; warn_puts_trip TE tr2)
             | EQUAL(_,tr1,tr2)  => (warn_puts_trip TE tr1; warn_puts_trip TE tr2)
             | CCALL(_,l) => List.apply (warn_puts_trip TE) l
             | RESET_REGIONS(_,tr) => warn_puts_trip TE tr
             | FRAME _ => ()
             | LETREGION{body, ...} => warn_puts_trip TE body
	     | _ => ()
	 
           and warn_puts_trip TE (TR(e,_,_,_)) = warn_puts TE e

	   and warn_puts_i TE (SWITCH(e, list, e')) = 
	       (warn_puts_trip TE e;
	        List.apply ((warn_puts_trip TE) o #2) list;
		warn_puts_opt TE  e'
	       )
	   and warn_puts_s TE (SWITCH(e, list, e')) = 
	       (warn_puts_trip TE e;
	        List.apply ((warn_puts_trip TE) o #2) list;
		warn_puts_opt TE  e'
	       )
	   and warn_puts_r TE (SWITCH(e, list,e')) = 
	       (warn_puts_trip TE e;
	        List.apply ((warn_puts_trip TE) o #2) list;
		warn_puts_opt TE e'
	       )
	   and warn_puts_c TE (SWITCH(e, list, e')) = 
	       (warn_puts_trip TE e;
	        List.apply ((warn_puts_trip TE) o #2) list;
		warn_puts_opt TE e'
	       )
	   and warn_puts_e TE (SWITCH(e, list, e')) = 
	       (warn_puts_trip TE e;
	        List.apply ((warn_puts_trip TE) o #2) list;
		warn_puts_opt TE  e'
	       )
	   and warn_puts_opt TE None = ()
	     | warn_puts_opt TE (Some e) = warn_puts_trip TE e
	
      in 
          warn_puts TE e
      end




  (**********************************)
  (* Reporting dangling pointers    *)
  (* from closures                  *)
  (**********************************)

  (* Garbage collection is only sound if there are no dangling pointers.
     By default, region inference allows dangling references which are never
     followed. The following function checks every lambda expresssion in
     the program to see whether it will result in a dangling pointer.

     A lambda abstraction fn pat => e only gives rise to a dangling pointer
     if there is has a free program variable which in its type scheme has
     a free region variable whose level is greater than the level of the
     effect variable associated with the fn. 

     [warn_dangling_pointer TE e] traverses e and examines all lambda abstractions of e
     For those abstractions that contain a region variable whose level is greater
     than  the level of the effect variable associated with the lambda abstraction, 
     a warning is printed. The warning shows the lambda abstraction (in abbreviated form)
     and all the offending variables with their type schemes and for each of them,
     all the offending region variables.

     [warn_dangling_pointers TE e] assumes that the sets of free lambda variables
     and excons of e have already been computed (currently done in PhysSizeInf.sml)

  *)

  fun bad_rhos(fn_level, rhos): place list = 
      List.all (fn rho=> case Eff.level_of rho of
                           Some level_rho => level_rho > fn_level
                         | None => die "bad_rhos: no level"
               ) rhos

  type bad_lvars = (Lvar.lvar * (sigma*place)*place list)list

  fun bad_lvars(fn_level, TE, lvars) : bad_lvars = 
    List.foldL (fn lvar => fn acc => case RSE.lookupLvar TE lvar of
                  Some (_,_,sigma,p,_,_) =>
                    (case bad_rhos(fn_level, p:: R.frv_sigma sigma) of
                       [] => acc
                     | l  => (lvar,(sigma,p), l) :: acc)
                | None => die "bad_lvars: lvar not in scope")
               [] lvars

  type bad_excons = (Excon.excon * (R.Type*place)*place list)list

  fun bad_excons(fn_level, TE, excons) : bad_excons = 
    List.foldL (fn excon => fn acc => case RSE.lookupExcon TE excon of
                  Some (tau,p) =>
                    (case bad_rhos(fn_level, p:: R.frv_mu(tau,p)) of
                       [] => acc
                     | l  => (excon,(tau,p), l) :: acc)
                | None => die "bad_excons: excon not in scope")
               [] excons

  fun show_rhos rhos = implode(map (fn rho => " " ^ pp_regvar rho) rhos)

  fun report_dangling(e, [],[]): unit = ()
    | report_dangling(e, l1: bad_lvars, l2: bad_excons): unit =

    let val source_identification = 
          case e of FN{pat, ...} => 
            "potentially dangling references out of closure for     fn " ^ 
              implode(map (fn (lvar,_) => " " ^ Lvar.pr_lvar lvar) pat) ^ ":\n"
          | _ => die "report_dangling: expression is not a lambda abstraction"
        val bad_lvar_lines = 
             map (fn (lvar,(sigma,p), bad_rhos) => implode["   " ^ Lvar.pr_lvar lvar^ ": " ^ show_rhos bad_rhos ^ "\n"])
                 l1
        val bad_excon_lines = 
             map (fn (excon,(tau,p), bad_rhos) => implode["   " ^ Excon.pr_excon excon^ ": " ^ show_rhos bad_rhos ^ "\n"])
                 l2
    in warn(implode (source_identification::(bad_lvar_lines @ bad_excon_lines)))
    end


  fun warn_dangling_pointers (TE:regionStatEnv, 
                   (PGM{expression = TR(e,_,_,_), ...}):('place,'a,'b) LambdaPgm,
                   get_place: 'place -> place):unit = 
    if not(Flags.is_on "garbage_collection") 
      then ()
    else      
      let
	fun warn_dangle TE (e: ('place,'a,'b)LambdaExp,eps_opt) = 
	  case e of
	    FIX{shared_clos, functions, scope, ... (*bound_lvars,binds,scope,info*)} => 
	         let val TE' = 
	                List.foldR (fn {lvar,tyvars,rhos,epss,Type,...} => fn TE' => 
			   RSE.declareLvar(lvar, (true,true,R.FORALL(tyvars,rhos,epss,Type), get_place shared_clos , None, None), TE'))
			TE functions
	
	         in
		     warn_dangle_trip TE' scope
	         end

             | FN{pat,body,
                      free = ref(Some(lvars, excons, _)),
                      ...} =>
                let val TE' = List.foldR (fn (lvar,(tau,rho)) => fn TE' => 
                    RSE.declareLvar(lvar, (true,true,R.type_to_scheme tau, rho , None, None), TE'))
			TE pat
                    val level_fn = case eps_opt of
                                     Some eps => (case Eff.level_of eps of
                                                    Some int => int 
                                                  | None => die "warn_dangle: latent effect has no level"
                                                 )
                                   | None => die "warn_dangle: no rho of expression"
                                   
                in 
                   warn_dangle_trip TE' body;
                   report_dangling(e, bad_lvars(level_fn,TE,lvars), bad_excons(level_fn,TE,excons))
                end
             | FN{pat,body,free = ref None,...} =>
                 Crash.impossible "warn_dangle: cannot analyse lambda expressions whose sets of free \n\
                                  \program variables and excons have not been computed."
             | LET{k_let,pat,bind,scope} =>
                   (warn_dangle_trip TE bind;
                    let 
                        val TE' = List.foldR (fn (lvar,_,tyvars,ref epss,tau,rho,_)  => fn TE' => 
			   RSE.declareLvar(lvar, (true,true,R.FORALL(tyvars,[],epss,tau), rho , None, None), TE'))
                           TE
            		   pat
                   in warn_dangle_trip TE' scope
                   end
                  )
	     | APP(_,_,e1,e2) => (warn_dangle_trip TE e1; warn_dangle_trip TE e2)           
	     | EXCEPTION(excon, is_nullary, (tau,p), _, body) => 
		     warn_dangle_trip (RSE.declareExcon(excon,(tau,p),TE)) body
	     | RAISE(e) => warn_dangle_trip TE e
	     | HANDLE(e1,e2) => (warn_dangle_trip TE e1; warn_dangle_trip TE e2)
	     | SWITCH_I(switch) => warn_dangle_i TE switch
	     | SWITCH_S(switch) => warn_dangle_s TE switch
	     | SWITCH_C(switch) => warn_dangle_c TE switch
	     | SWITCH_E(switch) => warn_dangle_e TE switch
             | CON0 _ => ()
             | CON1(_,tr) => warn_dangle_trip TE tr
             | DECON(_,tr) => warn_dangle_trip TE tr
             | EXCON(_,Some(_, tr)) => warn_dangle_trip TE tr
             | DEEXCON(_,tr) =>warn_dangle_trip TE tr
             | RECORD(_,l) => List.apply (warn_dangle_trip TE) l
             | UB_RECORD l => List.apply (warn_dangle_trip TE) l
             | SELECT(_,tr) => warn_dangle_trip TE tr
             | DEREF tr => warn_dangle_trip TE tr
             | REF(_,tr) => warn_dangle_trip TE tr
             | ASSIGN(_,tr1,tr2) => (warn_dangle_trip TE tr1; warn_dangle_trip TE tr2)
             | EQUAL(_,tr1,tr2)  => (warn_dangle_trip TE tr1; warn_dangle_trip TE tr2)
             | CCALL(_,l) => List.apply (warn_dangle_trip TE) l
             | RESET_REGIONS(_,tr) => warn_dangle_trip TE tr
             | FRAME _ => ()
             | LETREGION{body, ...} => warn_dangle_trip TE body
	     | _ => ()
	 
           and warn_dangle_trip TE (TR(e,mu as RegionExp.Mus[(R.FUN(_,eps,_),_)],_,_)) = warn_dangle TE (e,Some eps)
             | warn_dangle_trip TE (TR(e,mu,_,_)) = warn_dangle TE (e, None)

	   and warn_dangle_i TE (SWITCH(e, list, e')) = 
	       (warn_dangle_trip TE e;
	        List.apply ((warn_dangle_trip TE) o #2) list;
		warn_dangle_opt TE  e'
	       )
	   and warn_dangle_s TE (SWITCH(e, list, e')) = 
	       (warn_dangle_trip TE e;
	        List.apply ((warn_dangle_trip TE) o #2) list;
		warn_dangle_opt TE  e'
	       )
	   and warn_dangle_r TE (SWITCH(e, list,e')) = 
	       (warn_dangle_trip TE e;
	        List.apply ((warn_dangle_trip TE) o #2) list;
		warn_dangle_opt TE e'
	       )
	   and warn_dangle_c TE (SWITCH(e, list, e')) = 
	       (warn_dangle_trip TE e;
	        List.apply ((warn_dangle_trip TE) o #2) list;
		warn_dangle_opt TE e'
	       )
	   and warn_dangle_e TE (SWITCH(e, list, e')) = 
	       (warn_dangle_trip TE e;
	        List.apply ((warn_dangle_trip TE) o #2) list;
		warn_dangle_opt TE  e'
	       )
	   and warn_dangle_opt TE None = ()
	     | warn_dangle_opt TE (Some e) = warn_dangle_trip TE e
	
      in 
          warn_dangle TE (e,None)
      end




  (*****************************)
  (* Pretty printing  (almost  *)
  (* same as in RegionExp)     *)
  (*****************************)

  type StringTree = PP.StringTree
  fun layPair(t1,t2) = PP.NODE{start = "(", finish = ")", indent = 1, childsep = PP.RIGHT", ", 
                               children = [t1, t2]}

  fun layout_set children = PP.NODE{start = "{", finish = "}", indent = 1, childsep = PP.RIGHT", ", 
                               children = children}
  fun get_opt l = List.foldR (fn opt => fn acc => 
                         case opt of Some t => t::acc | None => acc) [] l


  val printcount = ref 1  (* controls when effects are printed *)

  fun mkLay (omit_region_info: bool) (layout_alloc: 'a -> StringTree Option)
                                     (layout_alloc_short: 'a -> StringTree Option)
                                     (layout_bind: 'b -> StringTree Option)
                                     (layout_other: 'c -> StringTree Option) =
    let
      open PP

      fun colon_pair(t1, t2) = 
           PP.NODE{start = "", finish   ="", indent = 0, childsep = PP.RIGHT ":",
                   children = [t1,t2]}

      fun alloc_string alloc = case (layout_alloc alloc) 
				 of Some t => let val s = PP.flatten1 t
					      in if s = "" then "" else " " ^ s
					      end
				  | None => ""

      fun layList f l = NODE{start = "[", finish = "]", indent = 1, childsep = RIGHT ",",
                             children = map f l}
          
      fun layHlist f l = HNODE{start = "[", finish = "]", childsep = RIGHT ",",
			       children = map f l}

      fun layHseq f l : StringTree list = List.foldR(fn y => fn ts => case f y of Some t => t::ts 
	                                                                        | _ => ts)[]l

      fun layHlistopt f l = HNODE{start = "[", finish = "]", childsep = RIGHT ",",
				  children = layHseq f l}

      infix ^^
      (*  s ^^ st_opt:   tag  the string tree option st_opt onto the string s *)
      fun s ^^ None = s
        | s ^^ (Some st') = s ^ PP.flatten1(st')

      infix ^^^
      (*  t ^^^ st_opt:   tag  the string tree option st_opt onto the tree t *)
      fun t ^^^ None = t
        | t ^^^ (Some t') = NODE{start = "", finish = "", indent = 0, childsep = NONE, children = [t,t']}

      fun pp_fun_allocation a =
          case layout_alloc a of
            Some st => PP.flatten1 st
          | None => ""

      val (layTau, layMu) = R.mk_layout omit_region_info
      fun layMus mus = NODE{start = "(", finish = ")", indent = 1, childsep = RIGHT",", 
                            children = map layMu mus}

      fun layMeta metatype = RegionExp.layMeta metatype

      fun layVarMu(x,mu) = if !Flags.print_types then LEAF (implode[Lvar.pr_lvar x, ":",  PP.flatten1(layMu mu)])
			   else LEAF (Lvar.pr_lvar x)
      fun layPatFn  [] = LEAF("() => ")
        | layPatFn  [(x,mu)] = NODE{start = "", finish = " => ", indent = 0, childsep = NONE,
                                    children = [layVarMu(x,mu)]}
        | layPatFn  pat = HNODE{start = "(", finish = ") =>", childsep = RIGHT",", 
                              children = map layVarMu pat}


     fun layVarSigma(lvar,alphas,rhos,epss, tau, rho) =
         let val sigma_t = R.mk_lay_sigma' omit_region_info (alphas, rhos, epss, tau)
             val start:string = Lvar.pr_lvar lvar ^
                                 (if !Flags.print_types then ":" else "")
             val sigma_rho_t = if !Flags.print_regions andalso !Flags.print_types then 
                                  NODE{start = "(", finish = ")", childsep = RIGHT",", 
                                       indent = 1, 
                                       children = [sigma_t, Eff.layout_effect rho]} 
                               else sigma_t

         in PP.NODE{start = start, finish = "", indent = size start +1,
                    childsep = PP.NONE, children = [sigma_rho_t]}
         end



      fun layPatLet [] = LEAF("()")
        | layPatLet [one as (lvar,_,tyvars,ref epss,tau,rho)] = 
             layVarSigma(lvar,tyvars,[],epss,tau,rho)
        | layPatLet pat = HNODE{start = "(", finish = ")", childsep = RIGHT",", 
                              children = map (fn (lvar,_,tyvars,ref epss,tau,rho) => 
                                              layVarSigma(lvar,tyvars,[],epss,tau,rho)) pat}


    fun layoutSwitch laytrip show_const (SWITCH(lamb,rules,wildcardOpt)) =
      let
        fun child(x,lamb) =
          PP.NODE{start="",finish="",indent=0,
                  children=[PP.LEAF (show_const x),
                            laytrip(lamb,0)],
                  childsep=PP.RIGHT " => "}
        val t1 = PP.NODE{start="(case ",finish=" ",indent=6, childsep = PP.NONE, 
                         children=[laytrip(lamb,0)]}
        val t2 = PP.NODE{start = "of " , finish = "" (*") (*case*) "*), indent = 3,
                         childsep=PP.LEFT " | ",
                         children = (map child rules) @ 
                                  (case wildcardOpt of 
                                     None => []
                                   | Some lamb => 
                                       [PP.NODE{start="",finish="",indent=0,
                                                children=[PP.LEAF "_",
                                                          laytrip(lamb,0)],
                                                childsep=PP.RIGHT " => "}])}

	val t2' = PP.NODE{start="",finish=") (*case*) ", indent = 3, childsep=PP.NONE,
			  children=[t2]}
      in 
       PP.NODE{start = "", finish = "", indent = 0, childsep = PP.NONE, 
               children = [t1,t2']}
      end

      fun lay_il (lvar_string:string, terminator: string, il, rhos_actuals) : StringTree =
          let val (taus,rhos,epss)= R.un_il(il)
              val rho_actuals_t_opt= if !Flags.print_regions  then 
                                        Some(layHlistopt layout_alloc_short rhos_actuals)
                                     else None
	      val taus_opt = if !(Flags.print_types) 
                                  then Some(layList layTau taus) 
                                  else None
	      val rhos_opt = if !Flags.print_types andalso !Flags.print_regions 
                                orelse !(Flags.print_effects)
                             then Some(layHlist Eff.layout_effect rhos) 
                             else None
	      val epss_opt = if !(Flags.print_effects) 
                                  then Some(layList Eff.layout_effect_deep epss) 
                                  else None  (*mads*)
          in
              NODE{start = lvar_string, finish = terminator, indent = 1, childsep = RIGHT" ",
                   children = get_opt [rho_actuals_t_opt, taus_opt,rhos_opt,epss_opt]}
          end

      fun laypoly(lvar,alloc,il,rhos_actuals) =
          case (alloc, R.un_il il)
           of  (None, ([],[],[])) => LEAF (Lvar.pr_lvar lvar)
          | _ => let val alloc_s = 
                     case alloc of 
                          Some alloc => 
                           if null(rhos_actuals) then "" else alloc_string alloc
                        | None => "" 
 		 in lay_il(Lvar.pr_lvar lvar, alloc_s, il, rhos_actuals)
		 end

      fun dont_lay_il (lvar_string:string, terminator: string, il) : StringTree =
          LEAF(lvar_string ^ terminator)

      (* precedence levels: lam : 1
                            + - etc : 2
                            app   : 3 *)
              (* n is precedence of parent - or 0 if no parens around lamb are needed *)

      fun layBin(bop:string, n, t1, t2, Some a) =
         (case alloc_string a 
	    of "" => (* put parenthesis, if precedence dictates it *)
	      if n>=2 then 
		NODE{start = "(", finish = ")", indent = 1, childsep = PP.RIGHT bop, 
		     children = [layTrip(t1,2), layTrip(t2,2)]}
	      else
		NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT bop, 
		     children = [layTrip(t1,2), layTrip(t2,2)]}
	     | s_alloc => (* assume allocation string is short: use it as terminator *)
		NODE{start = "(", finish = ")" ^ s_alloc, indent =1, childsep = PP.RIGHT bop,
		     children = [layTrip(t1,2), layTrip(t2,2)]})
	| layBin (bop:string, n, t1, t2, None) =
	      if n>=2 then 
		NODE{start = "(", finish = ")", indent = 1, childsep = PP.RIGHT bop, 
		     children = [layTrip(t1,2), layTrip(t2,2)]}
	      else
		NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT bop, 
		     children = [layTrip(t1,2), layTrip(t2,2)]}


      and try_bin(lvar, t as TR(UB_RECORD[t1,t2], _, _, _), rhos_actuals as [],n) =
	(* actual dropped... *)
	let open Lvar
	in case primitive lvar
	     of Some prim => 
	       (case prim
		  of PLUS_INT => layBin(" + ", n, t1, t2, None)
		   | MINUS_INT => layBin(" - ", n, t1, t2, None)
		   | MUL_INT => layBin(" * ", n, t1, t2, None) 
		   | DIV_INT => layBin(" div ", n, t1, t2, None)
		   | LESS_INT => layBin(" < ", n, t1, t2, None) 
		   | LESSEQ_INT => layBin(" <= ", n, t1, t2, None) 
		   | GREATER_INT => layBin(" > ", n, t1, t2, None)
		   | GREATEREQ_INT => layBin(" >= ", n, t1, t2, None)  
		   | LESS_FLOAT => layBin(" < ", n, t1, t2, None) 
		   | LESSEQ_FLOAT => layBin(" <= ", n, t1, t2, None) 
		   | GREATER_FLOAT => layBin(" > ", n, t1, t2, None)
		   | GREATEREQ_FLOAT => layBin(" >= ", n, t1, t2, None)  	     
		   | _ => die "try_bin1")
	      | None => raise Match 
	end
	| try_bin(lvar, t as TR(UB_RECORD[t1,t2], _, _, _), rhos_actuals as [rho],n) =
	let open Lvar
	in case primitive lvar
	     of Some prim =>
	       (case prim
		  of PLUS_FLOAT => layBin(" + ", n, t1, t2, Some rho)
		   | MINUS_FLOAT => layBin(" - ", n, t1, t2, Some rho)
		   | MUL_FLOAT => layBin(" * ", n, t1, t2, Some rho) 
		   | DIV_FLOAT => layBin(" / ", n, t1, t2, Some rho)
		   | _ => die "try_bin2")
	      | None => raise Match 
	end
	| try_bin _ = raise Match

      and layExp(lamb: ('a, 'b, 'c) LambdaExp,n): StringTree =  
        case lamb of 
          VAR{lvar,il,alloc=None,rhos_actuals=ref[],plain_arreffs,other} =>  (* fix-bound variables and prims *)
            (case R.un_il(il) of                                             (* are treated below (APP) *)
               ([],[],[]) => LEAF(Lvar.pr_lvar lvar)
             | _ => lay_il(Lvar.pr_lvar lvar, "", il, []))    (* rhos_actuals empty if lvar is not fix-bound *)

        | VAR{lvar,il,alloc=None,rhos_actuals=ref should_not_happen,
                   plain_arreffs,other} =>  (* fix-bound variables and prims *)
            (case R.un_il(il) of                                             (* are treated below (APP) *)
               ([],[],[]) => LEAF(Lvar.pr_lvar lvar)
             | _ => lay_il(Lvar.pr_lvar lvar, "", il, should_not_happen))    (* rhos_actuals should be empty if lvar is not fix-bound *)

        | VAR{lvar, il, alloc = Some (a), rhos_actuals = ref rhos_actuals, plain_arreffs,other} => 
            lay_il(Lvar.pr_lvar lvar, " at" ^^ layout_alloc a, il, rhos_actuals) ^^^ layout_other other

        | INTEGER(i, a) => LEAF(Int.string i ^^ layout_alloc a)
        | STRING(s, a) => LEAF(String.string s ^^ layout_alloc a)
        | REAL(r, a) => LEAF(Real.string r ^^ layout_alloc a)
        | UB_RECORD(args) =>
            PP.NODE{start = "<", finish = ">" , indent = 1, childsep = PP.RIGHT", ", 
                    children = map (fn trip => layTrip(trip,0)) args}
        | CON0{con, il, aux_regions,alloc} => (* nullary constructor *)
            let
               val alloc_s = alloc_string alloc
(*                   case (layout_alloc alloc) of Some t => " " ^PP.flatten1 t | None => "" *)
            in dont_lay_il(Con.pr_con con, alloc_s, il)
            end
        | CON1({con, il, alloc},trip) => (* unary constructor *)
            let
               val alloc_s = alloc_string alloc
(*                   case (layout_alloc alloc) of Some t => " " ^PP.flatten1 t | None => "" *)
               val t1 = dont_lay_il(Con.pr_con con, alloc_s, il)
            in 
             PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT " ", 
                     children = [t1, layTrip(trip,3)]}
            end
        | DECON({con, il},trip) => (* destruction *)
            let
               val t1 = dont_lay_il("decon_" ^ Con.pr_con con , "", il)
            in 
             PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT " ", 
                     children = [t1, layTrip(trip,3)]}
            end
            
        | EXCON(excon, None) => (* nullary exception constructor *)
             PP.LEAF(Excon.pr_excon excon)
        | EXCON(excon, Some (alloc,t)) => (* unary exception constructor *)
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
             PP.NODE{start = "#"^Int.string i ^ " ", finish = "", indent = 4, childsep = PP.NONE,
                     children = [layTrip(trip,3)]}
        | FN{pat,body,free,alloc}=> layLam((pat,body,alloc), n, "")
        | APP(None,_,TR(VAR{lvar, il, alloc, rhos_actuals=ref rhos_actuals, plain_arreffs,other},_,_,_), t2) =>
           (try_bin(lvar,t2,rhos_actuals,n)
            handle Match => 
           let
                  (*        f il at rho (exp) 
                                      OR
                            f il at rho
                              (exp)                                     
                  *)

	     val t1 = laypoly(lvar,alloc,il,rhos_actuals)
          in
             PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT " ", 
                     children = [t1, layTrip(t2,3)]}
          end
         )
        | APP(None,_,t1, t2) =>
           NODE{start = if n>3 then "(" else "", 
                finish = if n>3 then ")" else "", 
                childsep = RIGHT " ", indent = 1,
                children = [layTrip(t1,3), layTrip(t2,4)]}
        | APP(Some JMP, _, TR(VAR{lvar, il, alloc, rhos_actuals=ref rhos_actuals, plain_arreffs,other},_,_,_), t2)=>
           NODE{start = "jmp " ^ Lvar.pr_lvar lvar ^ " ", 
                finish = "", indent = 2, childsep = PP.NONE, children= [layTrip(t2,4)]}
        | APP(Some FUNCALL, _, TR(VAR{lvar, il, alloc, rhos_actuals=ref rhos_actuals, plain_arreffs,other},_,_,_), t2)=>
           (* insert printing of store resave here *)
           NODE{start = "funcall " , 
                finish = "", indent = 8, childsep = PP.RIGHT " ", children= [laypoly(lvar,alloc,il,rhos_actuals),layTrip(t2,4)]}
        | APP(Some FNJMP, _, t1, t2) =>
           NODE{start = if n>3 then "(fnjmp " else "fnjmp ", 
                finish = if n>3 then ")" else "", 
                childsep = RIGHT " ", indent = 1,
                children = [layTrip(t1,3), layTrip(t2,4)]}
        | APP(Some FNCALL, _, t1, t2) =>        (* insert printing of store resave here *)
           NODE{start = if n>3 then "(fncall " else "fncall ", 
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
                childsep = NONE, indent = 6,
                children = [layTrip(t1,2)]}
        | LET{k_let,pat, bind, scope} => layout_let_fix_and_exception lamb
        | FIX _ => layout_let_fix_and_exception lamb
        | REF(alloc, t) =>
            let val s = alloc_string alloc
            in  PP.NODE{start = if n>=3 then "(ref" ^ s ^ " "
                                else "ref" ^ s ^ " ", 
                        finish = if n>=3 then ")" else "", 
                        indent = 6, childsep = PP.NONE,
                        children = [layTrip(t,4)]}
            end
        | DEREF t =>
            PP.NODE{start = if n>=3 then "(!" else "!",
                    finish = if n>=3 then ")" else "", 
                    indent = 3, childsep = PP.NONE,
                    children = [layTrip(t,4)]}
        | ASSIGN(alloc, t1,t2) =>
            let val s = alloc_string alloc
            in  PP.NODE{start = "(" , finish = ")"^s, indent = 1, childsep = PP.RIGHT " := ",
                        children = [layTrip(t1,2), layTrip(t2,2)]}
            end
        | EQUAL({mu_of_arg1,mu_of_arg2, alloc}, arg1, arg2) =>
            let val eq = if !Flags.print_regions then  " =" ^ alloc_string alloc ^ " " else " = "
                val ty = if !(Flags.print_types) 
                           then implode["(* domain of = is: ", 
                                        PP.flatten1(layMu mu_of_arg1), "*",
                                        PP.flatten1(layMu mu_of_arg2), " *)"]
                         else ""
            in
               PP.NODE{start= if n>=2 then "(" else "", 
                       finish = if n>=2 then ")" else "", 
                       indent = 0, childsep = PP.RIGHT (eq^ty),
                       children = [layTrip(arg1,2), layTrip(arg2, 2)]}
            end
        | CCALL ({name, rhos_for_result, mu_result}, args) =>
	    let val rhos_for_result_sts =
	      if !Flags.print_regions
	      then map (PP.LEAF o alloc_string o #1) rhos_for_result
	      else []
	    in PP.NODE {start = "ccall(", finish = ")"
			^ (if !Flags.print_types then ":" ^ PP.flatten1(layMu mu_result) else ""),
			indent = 6, childsep = PP.RIGHT ", ", 
			children = PP.LEAF name :: rhos_for_result_sts
		                    @ (map (fn t => layTrip(t,0)) args)}
	    end
        | RESET_REGIONS({force, alloc,regions_for_resetting}, t) =>
           let val fcn = if force then "forceResetting " else "resetRegions "
               val aux_regions_t = HNODE{start="[",finish="]", childsep=NONE,
                            children=[layHlistopt layout_alloc_short regions_for_resetting]}
           in PP.NODE{start = "(" ^ fcn , finish = ")" ^ (if !Flags.print_regions then alloc_string alloc else ""),
                      indent = size fcn + 2, childsep = PP.NONE,
                      children = [aux_regions_t,layTrip(t,0)]}
           end
        | LETREGION{B, rhos = ref l, body} =>
           if !Flags.print_regions 
           then
            (case  l  of
               [] => layTrip(body,n)
            | _ (*binders*) =>
                 let 
                   val binders: StringTree list = layHseq layout_bind l
                   val t1 = 
                     NODE{start = "letregion ", finish = "", childsep = NONE, indent = 10,
                          children = [HNODE{start = "", finish = "", childsep = RIGHT", ", 
                                            children = binders}]}
                   val t2 = 
                     NODE{start = "in ", finish = "", childsep = NONE, indent = 3,
                          children = [layTrip(body,0)]}
                   val t3 = 
                     NODE{start = "end (*", finish = "*)", childsep = NONE, indent =  6,
                          children = [HNODE{start = "", finish = "", childsep = RIGHT", ", 
                                            children = binders}]}
                 in NODE{start = "", finish = "", indent = 0, childsep = RIGHT" ", children = [t1,t2,t3]}
                 end
               )
            else layTrip(body,n)
        | SWITCH_I(sw) => layoutSwitch layTrip Int.string  sw
        | SWITCH_S(sw) => layoutSwitch layTrip (fn s => s) sw
        | SWITCH_C(sw) => layoutSwitch layTrip Con.pr_con sw
        | SWITCH_E(sw) => layoutSwitch layTrip Excon.pr_excon sw
        | FRAME{declared_lvars, declared_excons} =>
             let val l1 = map (fn {lvar, sigma, place, other} =>
                               NODE{start = Lvar.pr_lvar lvar ^ ": (", finish = ")",
                                    indent = 5, childsep = RIGHT",", 
                                    children = [if !Flags.print_types then 
                                                   R.mk_lay_sigma omit_region_info sigma
                                                else LEAF "_",
                                                Eff.layout_effect place]})
                              declared_lvars
                 val l2 = map (LEAF o Excon.pr_excon) (map #1 declared_excons)
             in NODE{start = "{|", finish = "|}", indent = 0, childsep = RIGHT ", ", 
                     children = l1 @ l2}
             end
                                    
        | _ => LEAF "pretty-printing of this multiplicity expression not yet implemented"

      and layTrip(TR(e,RegionExp.Mus mus,rea,ref psi),n) = 
        let val t1 = 
                case (e, mus) of
                  (FN{pat,body,free,alloc}, [(R.FUN(_,eps,_),_)])=> 
		    let val eps_s = if !Flags.print_effects then PP.flatten1(Eff.layout_effect(*_deep*) eps) ^ " "    (*mads*)
				    else ""
		    in layLam((pat,body,alloc), n, eps_s)
		    end
                | _ => layExp(e,n)
            val tick = (printcount:= !printcount+1; !printcount)
        in
            if false (*tick mod 10 = 0*) then
              let (*val t2 = layMus mus*)
                  val t3 = Mul.layout_mulef psi
              in  NODE{start = "(", finish = ")", indent = 1, childsep = RIGHT":", 
                       children = [t1,(*t2,*)t3]}
              end
            else t1
        end
        | layTrip(TR(e, _, rea,_), n) = layExp(e,n)


      and layLam((pat,body,alloc), n, eps: string) = 
           (* (fn eps alloc pat =>  
               lamb
              )
           *)
          let 
             val start_s = implode["fn ", eps, pp_fun_allocation alloc, " "]
             val pat_t = layPatFn pat
             val first_line = NODE{start = start_s, finish = "", indent = size(start_s),
                                   children = [pat_t], childsep = NONE}
          in
              PP.NODE{start= if n>1 then "(" else "", 
                      finish=if n>1 then ")" else "", 
                      indent=1, childsep = PP.NONE,
                      children=[first_line,layTrip(body,1)]}
          end
      and layout_let_fix_and_exception lexp =
          let 
            val inInfo = ref ""
            fun layout_rec lexp = 
                  case lexp of 
                    LET{k_let,pat , bind, scope = t2 as TR(e2,_,_,_)} =>
                        let
                          val (binds, body) = layout_rec e2
                          val _ = inInfo := "(* let *)"
                          val pat' = map (fn (lvar,il,tyvars,epss,ty,p,_) =>(lvar,il,tyvars,epss,ty,p)) pat
                        in
                           (mk_valbind(pat',bind)::binds, body)
                        end
                  | FIX({free,shared_clos,functions,scope = t2 as TR(e2, _,_,_)}) =>
                        let
                          val (binds', body) = layout_rec e2
                          val _ = inInfo := "(* fix *)"
                        in
                          (mk_mutual_binding (layout_alloc shared_clos,List.rev functions):: binds', body)
                        end
                  | EXCEPTION(excon, nullary, mu, alloc, scope as TR(e2, _,_,_)) =>
                        let 
                          val (binds', body) = layout_rec e2
                          val _ = inInfo := "(* exn *)"
                        in
(*                        ((append_info_with_name omit_region_info " (* exn value or name " " *)" info  *)
                           (mk_excon_binding(excon,nullary, layout_alloc alloc, mu)::binds', body)
                        end
		  | LETREGION{B,rhos=ref [],body=TR(body,_,_,_)} => layout_rec body 
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
            None =>  NODE{start = "exception ",finish="",childsep=RIGHT " : ",
                 indent=4,  children=[LEAF(Excon.pr_excon excon), layMu mu] }
          | Some t => NODE{start = "exception ",finish="",childsep=RIGHT " ",
                 indent=4,  children=[LEAF(Excon.pr_excon excon), LEAF ":", layMu mu, 
                                      LEAF("(* exn value or name " ^ PP.flatten1 t ^ " *)")]}
        )
      and  mk_mutual_binding(opt_alloc, functions) = 
        let fun mk_fix({lvar,occ,tyvars,rhos,epss,Type, rhos_formals= ref rhos_formals, 
                        bound_but_never_written_into,
			bind as TR(FN{pat, body, ...},_,_,_),other})  (no, rest_of_mutual_binding) =
              (*   

                   fun fljadsfj <: sigma> <at rho> <[rho1, ..., rho_k]> (x_1, ..., x_n)  =
                       -------------------------------------------------------------------          
                       body 

              *)
              (no-1,let
	     	     val print_rhos_formals = !Flags.print_regions 
                     val keyword = if no = 1 then "fun " else "and "
                     val sigma_t_opt = if !Flags.print_types then
                                          Some(PP.NODE{start = ":", finish = "", indent = 1, childsep= PP.NONE,
                                                       children = [R.mk_lay_sigma'' (Some o Eff.layout_effect) 
				                                   omit_region_info (tyvars,rhos,epss,Type)]})
                                       else None
                     val rho_formals_opt = if print_rhos_formals then
                                            Some(PP.HNODE{start = "[", finish = "]", childsep= PP.RIGHT", ",
                                                  children = layHseq layout_bind rhos_formals})
                                           else None
                     val dropped_rho_formals_opt = 
                          case bound_but_never_written_into of
                            Some l => if print_rhos_formals then
                                            Some(PP.HNODE{start = "[", finish = "]", childsep= PP.RIGHT", ",
                                                  children = layHseq layout_bind l})
                                      else None
                          | _ => None
                     val value_formals = PP.HNODE{start="(", finish = ")= ", childsep = PP.RIGHT ", ", 
                                                  children = map (fn (lvar,_) => PP.LEAF(Lvar.pr_lvar lvar)) pat}
                     val body_t = layTrip(body, 0)
                     val t1 = PP.NODE{start = "", finish = "", indent = 0, childsep = PP.RIGHT " ",
                                      children = PP.LEAF (Lvar.pr_lvar lvar):: 
                                                 get_opt[sigma_t_opt, opt_alloc, rho_formals_opt,dropped_rho_formals_opt, Some(value_formals)]}
                    in
                      PP.NODE{start = keyword , finish = "", indent = 4, childsep = PP.NONE, 
                              children = [t1, body_t]}
                    end
                  :: rest_of_mutual_binding
             )
            | mk_fix _ _ = die "mk_fix: rhs of fix does not begin with lambda"
       in
        PP.NODE{start = "", finish = "", indent = 0,
                childsep = PP.NONE, 
                children = #2(List.foldL mk_fix (List.size functions,[]) functions)}
       end

    in
       (fn e => layExp(e,0),
       fn t => layTrip(t,0),
       layMus,
       layMeta)
    end


  fun layoutLambdaExp(layout_alloc: ('a -> StringTree Option))
                     (layout_alloc_short: ('a -> StringTree Option))
                     (layout_binder: ('b -> StringTree Option))
                     (layout_other : 'c -> StringTree Option)
                     (e: ('a, 'b, 'c)LambdaExp) :StringTree = 
            #1(mkLay(not(!Flags.print_regions))
                  layout_alloc layout_alloc_short layout_binder layout_other) e

  exception Lookup

  fun lookup env lvar = 
    let fun loop [] = raise Lookup
          | loop ((lvar',tr')::rest) = if Lvar.eq(lvar,lvar') then tr' else loop rest
    in loop env
    end



  fun eval env (tr as TR(e,mu,phi,psi)) = 
  let
    fun eval_sw env (SWITCH(tr0,match,t_opt)) =
      SWITCH(eval env tr0,
             map (fn (c, tr) => (c, eval [] tr)) match,
             case t_opt of None => None | Some t => Some(eval [] t))
    fun e_to_t(e') = TR(e',mu,phi,psi)
  in 
    case e of
      VAR{lvar, ...} => (lookup env lvar handle Lookup => tr)
    | INTEGER _ => tr
    | STRING _ => tr
    | REAL _ => tr
    | UB_RECORD trs => e_to_t(UB_RECORD(map (eval env) trs))
    | FN{pat,body,free,alloc} =>
       e_to_t(FN{pat=pat,body = eval [] body, free=free,alloc=alloc})
    | LETREGION{B,rhos,body} =>
       e_to_t(LETREGION{B=B,rhos=rhos,body=eval env body})
    | LET{k_let = true,
          pat = [(lvar,_,_,_,_,_,_)],
          bind, scope} =>
         eval ((lvar,(eval env bind))::env) scope
    | LET{k_let,
          pat, bind,scope} =>
         e_to_t(LET{k_let = k_let,pat=pat,bind = eval env bind, scope = eval env scope})
    | FIX{free,shared_clos,functions,scope} =>
         e_to_t(FIX{free=free,shared_clos=shared_clos,scope = eval env scope,
             functions = map 	
               (fn{lvar,occ,tyvars,rhos,epss,Type,rhos_formals,bound_but_never_written_into,
                   other,bind} =>
                  {lvar=lvar,occ=occ,tyvars=tyvars,rhos=rhos,epss=epss,Type=Type,
                   rhos_formals=rhos_formals,
                   bound_but_never_written_into=bound_but_never_written_into,
                   other=other,
                   bind = eval [] bind}) functions})
    | APP(ck,sr,tr1,tr2) => e_to_t(APP(ck,sr,eval env tr1, eval env tr2))
    | EXCEPTION(excon,b,mu,a,tr) =>
       e_to_t(EXCEPTION(excon,b,mu,a,eval env tr))
    | RAISE(tr) => e_to_t(RAISE(eval env tr))
    | HANDLE(tr1,tr2) => e_to_t(HANDLE(eval env tr1, eval env tr2))
    | SWITCH_I(sw)    => e_to_t(SWITCH_I(eval_sw env sw))
    | SWITCH_S(sw)    => e_to_t(SWITCH_S(eval_sw env sw))
    | SWITCH_C(sw)    => e_to_t(SWITCH_C(eval_sw env sw))
    | SWITCH_E(sw)    => e_to_t(SWITCH_E(eval_sw env sw))
    | CON0 _ => tr
    | CON1(info,tr) => e_to_t(CON1(info, eval env tr))
    | DECON(info,tr) => e_to_t(DECON(info, eval env tr))
    | EXCON(excon, None) => tr
    | EXCON(excon, Some(a, tr)) => e_to_t(EXCON(excon, Some(a, eval env tr)))
    | DEEXCON(excon,tr) => e_to_t(DEEXCON(excon, eval env tr))
    | RECORD(a, trs) => e_to_t(RECORD(a, map (eval env) trs))
    | SELECT(i,tr) => e_to_t(SELECT(i, eval env tr))
    | DEREF(tr) => e_to_t(DEREF(eval env tr))
    | REF(a, tr) => e_to_t(REF(a, eval env tr))
    | ASSIGN(a,tr1,tr2) => e_to_t(ASSIGN(a,eval env tr1, eval env tr2))
    | EQUAL(info,tr1,tr2)=>e_to_t(EQUAL(info,eval env tr1, eval env tr2))
    | CCALL(info,trs) => e_to_t(CCALL(info, map (eval env) trs))
    | RESET_REGIONS(info,tr) => e_to_t(RESET_REGIONS(info, eval env tr))
    | FRAME f => tr
  end

  fun k_evalPgm(PGM{expression = tr, export_datbinds,import_vars,export_vars,export_basis,export_Psi}) = 
      PGM{expression = eval [] tr,
          export_datbinds=export_datbinds,
          import_vars = import_vars,
          export_vars = export_vars,
          export_basis = export_basis,
          export_Psi = export_Psi}

  fun layoutLambdaTrip(layout_alloc: ('a -> StringTree Option))
                      (layout_alloc_short: ('a -> StringTree Option))
                      (layout_binder: ('b -> StringTree Option))
                      (layout_other : 'c -> StringTree Option)
                      (t: ('a, 'b, 'c)trip) :StringTree = 
            #2(mkLay(not(!Flags.print_regions))
                      layout_alloc layout_alloc_short layout_binder 
                      layout_other) 
              (if !show_K then t else eval [] t)



  fun layoutLambdaPgm(layout_alloc: ('a -> StringTree Option))
                     (layout_alloc_short: ('a -> StringTree Option))
                     (layout_binder: ('b -> StringTree Option))
                     (layout_other: ('c -> StringTree Option))
                     (p as PGM{expression = trip_in as TR(lamb,meta,rea,_),
                               export_datbinds = datbinds as RegionExp.DATBINDS dblist,
			       import_vars,
			       export_vars,
                               export_basis,
                               export_Psi}):StringTree = 
      let
        val layout_sigma = R.mk_lay_sigma  (not(!Flags.print_regions))
        val (layExp,layTrip,layMus,layMeta) = mkLay(not(!Flags.print_regions))
                                     layout_alloc layout_alloc_short layout_binder layout_other
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
          PP.NODE{start="DATATYPE ",finish="; ",indent=3,
                  children=map layoutdb db,childsep=PP.LEFT" and "}
        val dbTs = map layoutMutualRec_db dblist
        val lambT = layoutLambdaExp  layout_alloc layout_alloc_short  layout_binder layout_other 
                      (if !show_K then lamb 
                       else 
                          let val trip = trip_in
                              val TR(e',_,_,_) = eval [] trip
                          in e'
                          end
                      )
        val t1 = PP.NODE{start="",finish="",indent=0,
                         children=dbTs @ [lambT],childsep=PP.NONE}
        val t2 = PP.NODE{start = "META TYPE: ", finish = "", childsep = PP.NONE, indent = 2,
                         children = [layMeta meta]}
        val t3 = PP.NODE{start = "EFFECT: ", finish = "", childsep = PP.NONE, indent = 2,
                         children = map Eff.layout_effect_deep rea}
        val t4 = PP.NODE{start = "EXPORT REGION BASIS: [", finish = "]", indent = 1, childsep = PP.RIGHT ", ",
                               children = Eff.layoutEtas export_basis}
        val t5 = PP.NODE{start = "EXPORT MULTIPLICITY ARROW EFFECTS: [", finish = "]", indent = 1, childsep = PP.RIGHT ", ",
                               children = map (Mul.layout_mularef o !) export_Psi}

      in
        PP.NODE{start = "", finish = "", indent = 0, childsep = PP.NONE, children = [t1,t4,t5,t2,t3]}
      end

   (***********************************************)
   (*    Building a multiplicity-annotated term   *)
   (***********************************************)

  type multrip  = (place, place*Mul.mul, qmularefset ref)trip

  (* extend_env (lvar, epss, EE_acc, Xi_refs, dep_acc) 
          yields (EE_acc', Xi_ref::Xi_refs,dep'), where
     EE_acc' = EE_acc + {lvar = Xi_ref}
     dep' = dep + dependencies from free effect variables of !Xi_ref to Xi_ref,
     Xi_ref is fresh and contains a dummy type scheme for lvar *)


  fun extend_env (lvar, epss, EE_acc,Xi_refs_acc,dep_acc) =
    let 
      val r= ref Mul.empty_qmularefset  (* dummy *)
      val fev_sigma = 
        Eff.setminus(List.all Eff.is_arrow_effect (Eff.subgraph (epss)),
                     epss)
      val dep_acc = Mul.add_dependencies(dep_acc,Mul.MULSCHEME r,fev_sigma)
    in 
      (Mul.declare(EE_acc, lvar, r), r :: Xi_refs_acc, dep_acc)
    end

  fun extend_env_at_fix {lvar,occ,tyvars,rhos,epss,Type,formal_regions,bind} 
                  (EE_acc, Xi_refs_acc, dep_acc) =
                  extend_env (lvar, !epss, EE_acc,Xi_refs_acc,dep_acc) 

  fun extend_env_at_let (lvar, tys, rho, ty) 
                  (EE_acc, Xi_refs_acc, dep_acc) =
                  extend_env (lvar, [], EE_acc,Xi_refs_acc,dep_acc) 


  (* mk_initial_mulexp(EE,tr,Psi,dep): convert region-annotated expression to
     multiplicity-annotated expression with multiplicity 0 everywhere. 
     Static objects that are created in the process (typically type schemes)
     are added to the dependency map dep. *)

  fun mk_phi(effect_node) = Eff.mk_phi(effect_node)
 
  fun sawLetregion() = ()

  fun mk_initial_mulexp(EE,tr, dep): multrip * Mul.dependency_map =
    let
      exception Abort of exn
      fun mk_deptr(EE,tr as RegionExp.TR(e, mu, effect_node), dep) =
        let 
            val (e',dep') = mk_dep(EE,e, dep)
            val phi = mk_phi effect_node
            val r = ref(Mul.makezero_muleffect phi)
            val dep'' = Mul.add_dependencies(dep', Mul.MULEFF r, phi)
        in (TR(e', mu, phi, r), dep'' )
        end
      and mk_dep(EE,e, dep) = 
       let fun mk_dep_sw(EE,RegionExp.SWITCH(t0, choices, else_opt), dep) =
             let val (tr0, dep) = mk_deptr(EE,t0, dep)
                 val rhsides = map #2 choices
                 val (rhs', dep) = mk_deps(EE, rhsides, dep)
                 val choices' = ListPair.zip(map #1 choices, rhs')
                 val (last,dep) = 
                     case else_opt of
                       None => (None, dep)
                     | Some t => let val (t',dep) = mk_deptr(EE,t,dep) in (Some t', dep) end
             in (SWITCH(tr0,choices',last), dep) 
             end
       in
        case e of 
          RegionExp.VAR{lvar, il_r, alloc} => 
             let val (_,rhos,eff_nodes) = R.un_il(#1(!il_r))
		 val arreffs = map (fn eps => (eps, Eff.mk_phi eps)) eff_nodes
                               handle _ => die ("VAR (mk_phi failed), lvar = " ^ Lvar.pr_lvar lvar)
		 val r  = Mul.lookup_efenv(EE, lvar)
                               handle _ => die ("VAR (lookup_efenv failed), lvar = " 
                                                ^ Lvar.pr_lvar lvar)
	     in
	       (VAR{lvar=lvar, il = #1(!il_r) , plain_arreffs = arreffs, 
                    alloc=alloc,rhos_actuals=ref rhos, other = r}, dep)
	     end 
        | RegionExp.INTEGER(i,a) => (INTEGER(i,a), dep)
        | RegionExp.STRING(s,a) => (STRING(s,a), dep)
        | RegionExp.REAL(r,a) => (REAL(r,a), dep)
        | RegionExp.UB_RECORD(ts) => 
            let val (ts', dep) = mk_deps(EE, ts, dep)
            in (UB_RECORD ts', dep)
            end
        | RegionExp.FN{pat,body,alloc} =>
            let val EE' = List.foldL (fn (lvar,_) => fn EE => Mul.declare(EE,lvar,ref(Mul.empty_qmularefset)))
                          EE pat
val (body',dep) = mk_deptr(EE',body, dep)
            in (FN{pat=pat,body=body',free=ref None,alloc=alloc}, dep)
            end
        | RegionExp.LETREGION_B{B,body,...} =>
            let val _ = sawLetregion();  (* for profiling *)
                val (body',dep) = mk_deptr(EE,body, dep)
                val discharged = map Eff.find (!B)
                val discharged_rhos_sorted = rev(Eff.sort(List.all Eff.is_rho (discharged)))
            in (LETREGION{B = B, 
                          rhos = ref (map (fn rho => (rho,Mul.NUM 1)) discharged_rhos_sorted),
                          body = body'},
                dep)
            end
        | RegionExp.LET{pat ,bind,scope} =>
            let val (bind', dep) = mk_deptr(EE,bind, dep)
                val (EE_extended, Xi_refs, dep)  = List.foldR extend_env_at_let (EE,[],dep) pat
                val pat' = map (fn ((lvar, tys, ty,p), Xi_ref) => (lvar,ref[],tys,ref[],ty,p,Xi_ref))
                               (ListPair.zip(pat, Xi_refs))
                val (scope', dep) = mk_deptr(EE_extended,scope, dep)
            in (LET{k_let=false,pat=pat', bind=bind', scope = scope'}, dep)
            end
        | RegionExp.FIX{shared_clos, functions,scope} =>
            let 
              val (EE_extended, Xi_refs, dep) = List.foldR extend_env_at_fix (EE,[], dep) functions
              val (functions', dep) = mk_dep_funcs(EE_extended, functions, Xi_refs, dep)
              val (scope', dep) = mk_deptr(EE_extended, scope, dep)
            in
               (FIX{free = ref None, shared_clos = shared_clos, functions = functions', scope = scope'}, dep)
            end
        | RegionExp.APP(tr1, tr2) =>
            let val (tr1', dep) = mk_deptr(EE,tr1, dep)
                val (tr2', dep) = mk_deptr(EE,tr2, dep)
            in (APP(None,NOT_YET_DETERMINED,tr1',tr2'), dep)
            end
        | RegionExp.EXCEPTION(excon,b,mu,alloc,tr) =>
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (EXCEPTION(excon,b,mu,alloc,tr'),dep)
            end
        | RegionExp.RAISE(tr) =>
            let val (tr',dep) = mk_deptr(EE,tr, dep)
            in (RAISE(tr'),dep)
            end
        | RegionExp.HANDLE(tr1, tr2) =>
            let val (tr1', dep) = mk_deptr(EE,tr1, dep)
                val (tr2', dep) = mk_deptr(EE,tr2, dep)
            in (HANDLE(tr1',tr2'), dep)
            end
        | RegionExp.SWITCH_I sw =>
            let val (sw', dep) = mk_dep_sw(EE,sw, dep)
            in (SWITCH_I sw', dep)
            end
        | RegionExp.SWITCH_S sw =>
            let val (sw', dep) = mk_dep_sw(EE,sw, dep)
            in (SWITCH_S sw', dep)
            end
        | RegionExp.SWITCH_C sw =>
            let val (sw', dep) = mk_dep_sw(EE,sw, dep)
            in (SWITCH_C sw', dep)
            end
        | RegionExp.SWITCH_E sw =>
            let val (sw', dep) = mk_dep_sw(EE,sw, dep)
            in (SWITCH_E sw', dep)
            end
	| RegionExp.CON0 c => (CON0 c, dep) 
	| RegionExp.CON1 (c,tr) => 
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (CON1(c,tr'),dep)
            end
	| RegionExp.DECON(c,tr) =>
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (DECON(c,tr'),dep)
            end
	| RegionExp.EXCON(excon,None) => (EXCON(excon,None), dep)
	| RegionExp.EXCON(excon,Some(p,tr)) =>
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (EXCON(excon,Some(p,tr')),dep)
            end
	| RegionExp.DEEXCON(excon,tr) =>
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (DEEXCON(excon,tr'),dep)
            end
	| RegionExp.RECORD (p,trs) =>
            let val (trs',dep) = mk_deps(EE,trs,dep)
            in (RECORD(p,trs'),dep)
            end
	| RegionExp.SELECT(i,tr) =>
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (SELECT(i,tr'),dep)
            end
	| RegionExp.DEREF tr =>
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (DEREF tr',dep)
            end
	| RegionExp.REF (p, tr) =>
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (REF(p,tr'),dep)
            end
	| RegionExp.ASSIGN(p,tr1,tr2) =>
            let val (tr1',dep) = mk_deptr(EE,tr1,dep)
 	        val (tr2',dep) = mk_deptr(EE,tr2,dep)
            in (ASSIGN(p,tr1',tr2'),dep)
            end
	| RegionExp.EQUAL(c,tr1,tr2) =>
            let val (tr1',dep) = mk_deptr(EE,tr1,dep)
 	        val (tr2',dep) = mk_deptr(EE,tr2,dep)
            in (EQUAL(c,tr1',tr2'),dep)
            end
	| RegionExp.CCALL(c,trs) =>
            let val (trs',dep) = mk_deps(EE,trs,dep)
            in (CCALL(c,trs'),dep)
            end
	| RegionExp.RESET_REGIONS(c,tr) =>           
            let val (tr',dep) = mk_deptr(EE,tr,dep)
            in (RESET_REGIONS(c,tr'),dep)
            end
        | RegionExp.FRAME{declared_lvars, declared_excons} =>
            let 
              val declared_lvars' = map (fn {lvar,sigma,place} =>
                                          let val r = Mul.lookup_efenv(EE,lvar)
                                          in 
                                             {lvar = lvar, sigma = (!sigma), place=place, other = r}
                                          end) declared_lvars
            in 
              (FRAME{declared_lvars = declared_lvars', declared_excons= declared_excons}, dep)
            end
       end 
            (* error handling: common for all rules in the above match *)
            handle Abort exn => raise Abort exn
                | x => (outtree(RegionExp.layoutLambdaExp' e); raise Abort x)

      and mk_dep_funcs(EE, [], [], dep) = ([],dep)
        | mk_dep_funcs(EE, {lvar,occ,tyvars,rhos=ref rhos,epss=ref epss,Type,
                            formal_regions,bind}::rest, 
                           r :: rest_refs, dep)=
             let val (bind', dep) = mk_deptr(EE,bind,dep)
                 val (functions', dep) = mk_dep_funcs(EE, rest, rest_refs, dep)
                 val new_occ = map (#1 o !) (! occ)
             in ({lvar=lvar,occ=new_occ,tyvars=tyvars,rhos=rhos,
                  epss=epss,Type=Type,rhos_formals=ref (map (fn rho=>(rho,Mul.NUM 1)) rhos),
                  bound_but_never_written_into = None,
                  bind=bind',other = r}::functions', dep)
             end
	| mk_dep_funcs _ = die "mk_dep_funcs"

      and mk_deps(EE, [],dep) = ([], dep)
        | mk_deps(EE, tr::ts, dep) = 
            let val (tr',dep) = mk_deptr(EE,tr, dep)
                val (ts',dep) = mk_deps(EE, ts,dep)
            in (tr'::ts',dep) end

    in
     (* Compute dependencies generated by te: *)

       mk_deptr(EE,tr, dep)
       handle Abort exn => raise exn
    end


  (***************************************)
  (* K-normalisation                     *)
  (***************************************)

  local 
    val r = ref 0
  in
    fun fresh _ = (r:= !r + 1; Lvar.new_named_lvar ("k" ^ Int.string(!r)))
  end

  exception Abort

  fun k_norm printnormal dummy_'c (tr: ('_a, 'b, 'c)trip as TR(e,mu,arefss,psir)) 
            (k : ('_a, 'b, 'c)trip -> ('_a, 'b, 'c)trip): ('_a, 'b, 'c)trip =
    (* k normalization for expressions 
     * the argument k is a continuation argument, giving the context into
     * which the normalized form of e should be stuck.
     * The code avoids generating things like `let x = x1 in x'. 
     *)
  let

    fun kne(tr: ('_a, 'b, 'c)trip as TR(e,mu,arefss,psir)) 
            (k : ('_a, 'b, 'c)trip -> ('_a, 'b, 'c)trip): ('_a, 'b, 'c)trip    =
    let

      fun e_to_t(e) = TR(e, mu,arefss,psir)

      fun ty_of (RegionExp.Mus[(tau,_)]) = tau
        | ty_of _ = die "ty_of"

      local val il0 = R.mk_il([],[],[])
      in
        fun lvar_as_term(x,mu) = 
            TR(VAR{lvar=x,il =il0 ,plain_arreffs=[],
                   alloc=None,rhos_actuals= ref [], other = dummy_'c}, mu, [], ref Mul.empty_psi)

        fun lvar_as_term'(x,mu as (tau,rho)) = 
            lvar_as_term(x,RegionExp.Mus[mu])

        fun mk_pat(lvar, mu) = 
          (case mu of
             RegionExp.Mus[(ty,place)] =>
               [(lvar, ref ([]:R.il ref list), [], ref([]:effect list), ty, place, dummy_'c)]
           | _ => die "mk_pat: metatype not (tau,rho)")
      end

      fun atomic(TR(VAR _, _, _, _)) = true
        | atomic(TR(INTEGER _,_,_,_)) = true
        | atomic(TR(RECORD(_,[]), _, _, _)) = true
        | atomic _ = false

      fun one_sub' k (tr1 as (TR(_,mu1,phi1,psi1))) f =
           kne tr1 (fn tr1' =>
            if atomic tr1' then k(f tr1')
            else
             let val x = fresh()
             in k(e_to_t(LET{k_let = true,
                        pat = mk_pat(x,mu1),
                        bind = tr1',
                        scope= f(lvar_as_term(x,mu1))}))
             end)

      fun two_sub (tr1, tr2) f =
        k(
          one_sub' (fn x => x) tr1 (fn atomic1 =>
          one_sub' (fn x => x) tr2 (fn atomic2 =>
          f(atomic1,atomic2))))

      val one_sub = one_sub' k

      fun many_sub trs f = 
        k(let fun loop([], atomics) = f (rev atomics)
                | loop(tr::rest, atomics) = 
                    one_sub' (fn x => x) tr (fn atomic =>
                      loop(rest, atomic::atomics))
          in 
            loop (trs,[])
          end)

      fun kns (sw as (SWITCH(tr0, match, tr_opt))) constr  = 
         one_sub tr0 (fn x_tr_0 => 
            let val match' = map (fn (con,tr) => (con,kne tr (fn  x => x))) match
                val tr_opt' = case tr_opt of
                                Some tr_alt => Some(kne tr_alt (fn x => x))
                              | None => None
            in constr(SWITCH(x_tr_0,match',tr_opt'))
            end)

    in
      case e of
        VAR _ => k tr
      | INTEGER _ => k tr
      | STRING _ => k tr
      | REAL _ =>  k tr
      | UB_RECORD(trs) => many_sub trs (e_to_t o UB_RECORD)
      | FN{pat,body,free,alloc} =>
        k (e_to_t(FN{pat=pat, free=free,alloc=alloc,
              body = kne body (fn x => x)}))
      | LETREGION{B,rhos = rhos as ref [letregion_bound_rho], 
                  body as TR(APP(ck,sr,tr1 as TR(VAR{lvar, alloc = Some _, ...}, _, _, _), tr2), a,b,c)
                 } =>
          one_sub tr2 (fn tr2' => e_to_t
                            (LETREGION{B=B,rhos =rhos,body = 
                                       TR(APP(ck,sr,tr1,tr2'), a,b,c)}))       
      | LETREGION{B,rhos,body} =>
        k (e_to_t(LETREGION{B=B, rhos=rhos, body = kne body (fn x => x)}))
      | LET{k_let, pat, bind, scope} =>
        (if k_let then output(std_out, "term already k-normalised\n") else ();
         kne bind (fn bind' =>
         kne scope (fn scope' =>
         k(e_to_t(LET{k_let = k_let, pat=pat, bind = bind', scope=scope'}))
        )))
      | FIX{free,shared_clos,functions,scope} =>
        kne scope (fn scope' =>
         let val functions' = map (fn{lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
                                      bound_but_never_written_into=bound_but_never_written_into,
                                      other,bind} =>
                           {lvar= lvar, occ=occ,tyvars=tyvars,rhos=rhos,epss=epss,Type=Type,
                            rhos_formals=rhos_formals,other = other,
                            bound_but_never_written_into=bound_but_never_written_into,
                            bind = kne bind (fn x => x)}) functions
         in k(e_to_t(FIX{free=free, shared_clos=shared_clos, functions = functions', scope=scope'}))
         end)
       | APP(ck,sr,opr as (TR(VAR{alloc = Some rho,...},_,_,_)), t2) =>   (* region-polymorphic call *)
               one_sub t2 (fn atomic2 => e_to_t(APP(ck,sr,opr, atomic2)))
       | APP(ck,sr,opr as TR(VAR{alloc = None, ... },_,_,_), 
             t2 as TR((UB_RECORD trs), mu2, phi2, psir2)) =>        (* primitive *)
          many_sub trs (fn trs' => e_to_t(APP(ck,sr,opr, TR(UB_RECORD trs' , mu2, phi2, psir2))))

       | APP(ck,sr,t1 ,t2) => (* other application *)  two_sub(t1,t2) (fn (t1',t2') => e_to_t(APP(ck,sr,t1',t2')))
       | EXCEPTION(excon,b,mu_excon,a,tr1) =>
           k(e_to_t(EXCEPTION(excon,b,mu_excon,a, kne tr1 (fn x => x))))
       | RAISE (tr1 as TR(_,mu2,phi1,psi1)) =>
           one_sub tr1 (e_to_t o RAISE)
       | HANDLE(tr1,tr2) => (*two_sub(tr1,tr2) (e_to_t o HANDLE)*)
             k(e_to_t(HANDLE(kne tr1 (fn x => x) , kne  tr2 (fn x => x))))
       | SWITCH_I(sw) => kns sw (e_to_t o SWITCH_I )
       | SWITCH_S(sw) => kns sw (e_to_t o SWITCH_S )
       | SWITCH_C(sw) => kns sw (e_to_t o SWITCH_C )
       | SWITCH_E(sw) => kns sw (e_to_t o SWITCH_E )
       | CON0 _ => k tr
       | CON1(info,tr1 as TR(_,mu1,phi1,psi1)) =>
           one_sub tr1 (fn t' => e_to_t(CON1(info,t')))
       | DECON(info, tr1 as TR(_,mu1,phi1,psi1)) =>
           one_sub tr1 (fn t' => e_to_t(DECON(info,t')))
       | EXCON(excon,None) => k tr
       | EXCON(excon,Some(a, tr1 as TR(_,mu1,phi1,psi1))) =>
           one_sub tr1 (fn t' => e_to_t(EXCON(excon,Some(a,t'))))
       | DEEXCON(excon, tr1 as TR(_,mu1,phi1,psi1)) =>
           one_sub tr1 (fn t' => e_to_t(DEEXCON(excon,t')))
       | RECORD(a, trs) => many_sub trs  (fn lvars' => (e_to_t(RECORD(a,lvars'))))
       | SELECT(i,tr1 as TR(_,mu1,phi1,psi1)) => 
                     one_sub tr1 (fn t' => e_to_t(SELECT(i,t')))
       | DEREF(tr1) => one_sub tr1 (e_to_t o DEREF)
       | REF(a,tr1) => one_sub tr1 (fn tr' => e_to_t(REF(a,tr')))
       | ASSIGN(a,tr1,tr2) =>
            two_sub (tr1,tr2) (fn (t1,t2) => e_to_t(ASSIGN(a,t1,t2)))
       | EQUAL(info,t1,t2) => 
            two_sub (t1,t2) (fn (t1',t2') => e_to_t(EQUAL(info,t1',t2')))
       | CCALL(info, trs) =>
            many_sub trs (fn trs' => e_to_t(CCALL(info,trs')))
       | RESET_REGIONS(info, tr1) =>
            one_sub tr1 (fn tr1' => e_to_t(RESET_REGIONS(info,tr1')))
       | FRAME _ => k tr

    end (* let *)
       handle Abort => raise Abort
            | _ => (printnormal("\n  ******k-normalisation bummed out at *********  \n", tr); 
                    raise Abort)
  in
     (kne tr k) handle Abort => die "kne failed"
  end

        
  fun k_normPgm printnormal dummy_'c 
       (PGM{expression = tr, export_datbinds,import_vars,export_vars,export_basis,export_Psi}) = 
      PGM{expression = k_norm printnormal dummy_'c tr (fn x => x),
          export_datbinds=export_datbinds,
          import_vars = import_vars,
          export_vars = export_vars,
          export_basis = export_basis,
          export_Psi = export_Psi}

  (* testing of K-normalisation: first equality of terms: *)
  exception EQ_LIST

  fun eq_list eq ([], []) = true
     | eq_list eq (x::xs, x' :: xs') = eq(x,x') andalso eq_list eq (xs,xs')
     | eq_list eq _ = raise EQ_LIST



  fun eq_sw (SWITCH(t0,match,t_opt), SWITCH(t0',match',t_opt')) eq =
            eq(t0,t0') andalso 
            eq_list(fn((_,rhs), (_,rhs'))=>eq(rhs,rhs')) (match,match') andalso
            (case (t_opt,t_opt') of
               (None, None) => true
             | (Some t, Some t') => eq(t,t')
             | _ => raise EQ_LIST)

 fun test_knorm printnormal printerror = 
 let
  fun eqExp (e1, e2) = 
  let 
  in
     (case (e1,e2) of
        (VAR{lvar=lvar1, ...}, VAR{lvar=lvar2,...}) =>
            Lvar.eq(lvar1,lvar2)
      | (INTEGER(i,_), INTEGER(i',_)) => i=i'
      | (STRING(s,_), STRING(s',_)) => s=s'
      | (REAL(r,_), REAL(r',_)) => r=r'
      | (UB_RECORD ts1, UB_RECORD ts2) =>
           eq_list eq (ts1,ts2)
      | (FN{pat = pat1, body = body1, ...}, FN{pat = pat2, body = body2, ...}) =>
           eq_list (fn ((lvar1,_), (lvar2,_)) => Lvar.eq(lvar1,lvar2)) (pat1,pat2) andalso
           eq(body1,body2)
      | (LETREGION{B = r1, rhos = r1', body= body1}, LETREGION{B = r2, rhos = r2', body= body2}) =>
           r1 = r2 andalso r1' = r2' andalso eq(body1,body2)
      | (LET{pat=pat1,bind=bind1,...}, LET{pat=pat2,bind=bind2,...}) =>
           eq_list (fn ((lvar1, _, _, _, _, _, _), (lvar2,_,_,_,_,_, _)) => Lvar.eq(lvar1,lvar2)) (pat1,pat2)
           andalso 
           eq(bind1,bind2)
      | (FIX{functions=functions1, scope = scope1, ...}, FIX{functions=functions2, scope = scope2, ...}) =>
           eq_list (fn ({lvar=lvar1,bind=bind1,...} , {lvar=lvar2,bind=bind2,...}) =>
                           Lvar.eq(lvar1,lvar2) andalso eq(bind1,bind2)) (functions1,functions2)
           andalso eq(scope1,scope2)
      | (APP(ck1,sr1,t1,t1'), APP(ck2,sr2,t2,t2')) =>
           eq(t1,t2) andalso eq(t1',t2')
      | (EXCEPTION(excon1,_,_,_,tr1), EXCEPTION(excon2,_,_,_,tr2)) =>
           Excon.eq(excon1,excon2) andalso eq(tr1,tr2)
      | (RAISE(tr1), RAISE(tr2)) => eq(tr1,tr2)
      | (HANDLE(tr1,tr1'), HANDLE(tr2,tr2')) =>
            eq(tr1,tr2) andalso eq(tr1',tr2')
      | (SWITCH_I(sw1),SWITCH_I(sw2)) => eq_sw (sw1,sw2) eq
      | (SWITCH_S(sw1),SWITCH_S(sw2)) => eq_sw (sw1,sw2) eq
      | (SWITCH_C(sw1),SWITCH_C(sw2)) => eq_sw (sw1,sw2) eq 
      | (SWITCH_E(sw1),SWITCH_E(sw2)) => eq_sw (sw1,sw2) eq
      | (CON0{con=con1, ...}, CON0{con=con2,...}) => Con.eq(con1,con2)
      | (CON1(info1, tr1), CON1(info2, tr2)) => eq(tr1,tr2)
      | (DECON(info1,tr1), DECON(info2,tr2)) => eq(tr1,tr2)
      | (EXCON(_,None), EXCON(_,None)) => true
      | (EXCON(_,Some(_, tr1)), EXCON(_,Some(_, tr2))) => eq(tr1,tr2)
      | (DEEXCON(excon1,tr1), DEEXCON(excon2, tr2)) => eq(tr1,tr2)
      | (RECORD(a, trs1), RECORD(a', trs2)) => eq_list eq (trs1,trs2)
      | (SELECT(i,tr1), SELECT(i',tr2)) => i=i' andalso eq(tr1,tr2)
      | (DEREF(tr1), DEREF(tr2)) => eq(tr1,tr2)
      | (REF(a,tr1), REF(a', tr2)) => eq(tr1,tr2)
      | (ASSIGN(a1,tr1,tr1'), ASSIGN(a2,tr2,tr2')) => eq(tr1,tr2) andalso eq(tr1',tr2')
      | (EQUAL(_,tr1,tr1'), EQUAL(_,tr2,tr2')) => eq(tr1,tr2) andalso eq(tr1',tr2')
      | (CCALL(_,trs1), CCALL(_,trs2)) => eq_list eq (trs1,trs2)
      | (RESET_REGIONS(_,t1), RESET_REGIONS(_,t2)) => eq(t1,t2)
      | (FRAME _, FRAME _) => true
      | _ => (printerror(e1,e2); false)
     ) handle EQ_LIST => (printerror(e1,e2); false)

   end

   and eq(TR(e1,_,_,_), TR(e2, _, _, _)) = eqExp(e1,e2)
  in
     fn dummy_'c => fn (PGM{expression = tr0, ...}) =>

        (printnormal ("\nbefore k-normalisation", tr0);
         let val tr' = k_norm printnormal dummy_'c tr0 (fn x => x)
         in  printnormal ("\nafter  k_normalisation", tr');
             eq(tr0, eval [] tr')
         end)

  end
           
  (**************************************************)
  (* Call conversion:                               *)
  (* Convert APP to JMP, FUNCALL, FNJMP and FNCALL  *)
  (* Insert JOIN_WITH and RETURN_WITH annotations   *)
  (**************************************************)

  datatype continuation = RETURN (* tail call *)
                        | NEXT   (* physically next instruction *)
                        | JOIN   (* continue at joint point for 
                                    closest surrounding SWITCH *)

  exception NOTJMP
  exception NOT_PRIM

  fun appConvert (allocates_space: 'b -> bool)
                 (actuals_regions_match_formal_regions: ('b list * 'a list )->bool) 
                 (remove_from_bound: 'b list * 'a -> 'b list)
                 (prog as PGM{expression = tr: ('a,'b,'c)trip,
                                 export_datbinds,
                                 import_vars,
                                 export_vars,
                                 export_basis,
                                 export_Psi}): ('a,'b,'c)LambdaPgm = 
    let
(*
      fun perhapsTerminate _ (e as (FNJMP _), c)  = (e,c)
        | perhapsTerminate _ (e as (JMP _), c)    = (e,c)
        | perhapsTerminate RETURN (e, c) = (RETURN_WITH e, c)
        | perhapsTerminate JOIN (e, c) = (JOIN_WITH e, c)
        | perhapsTerminate NEXT (e, c) = (e, c)
*)
      fun perhapsTerminate _ (e,c) = (e,c)

      fun cleanup(LETREGION{B, rhos = ref [], body as TR(e,_,_,_)}) = e
        | cleanup(e) = e

      fun tailTrip params_opt =
      let
        fun tailExp(e: ('a,'b,'c)LambdaExp,cont) = 
          let 
            fun tailsw(SWITCH(tr0,choices, else_tr_opt), cont) =
             let val (tr0', cont0) = tail(tr0, NEXT)
                 val choices' = map (fn (con, tr) => 
                                     (con, #1(tail(tr,if cont = NEXT then JOIN else cont)))) choices
                 val else_tr_opt' = case else_tr_opt of None => None
                                    | Some tr => Some(#1(tail(tr,if cont = NEXT then JOIN else cont)))
             in
                SWITCH(tr0',choices',else_tr_opt') 
             end
            
            fun find_call_kind(cont, alloc) = 
              case (cont, alloc) of
                (NEXT, None) =>     Some FNCALL
              | (NEXT, Some _) =>   Some FUNCALL
              | (JOIN, None) =>     Some FNCALL
              | (JOIN, Some _) =>   Some FUNCALL
              | (RETURN, None) =>   Some FNJMP
              | (RETURN, Some _) => Some FUNCALL  

            fun application_not_in_letregion(tr1, tr2,alloc)= (* alloc is Some _ iff tr1 is a fix-bound lvar *)
             let val (tr2', cont') = tail(tr2,NEXT)
                 val (tr1', cont') = tail(tr1,cont')
             in
                (APP(find_call_kind(cont, alloc), NOT_YET_DETERMINED, tr1', tr2'), cont')
             end

          in
             (case e of
                VAR{lvar,other,...} => perhapsTerminate cont(e, NEXT)
              | INTEGER i => perhapsTerminate cont(INTEGER i, NEXT)
              | STRING s => perhapsTerminate cont(STRING s, NEXT)
              | REAL r => perhapsTerminate cont(REAL r, NEXT)
              | UB_RECORD l => let val (trs', cont) = tailList(l, cont)
                               in perhapsTerminate cont(UB_RECORD trs', cont)
                               end
              | APP(_,_,t1 as TR(VAR{lvar, il, alloc, rhos_actuals=ref rhos_actuals, 
                            plain_arreffs,other},_,_,_), t2) =>
                   (try_prim(t1,lvar,t2,rhos_actuals,cont) handle NOT_PRIM => 
                    application_not_in_letregion(t1, t2,alloc)
                   )

              | APP(_,_, tr1,tr2) => application_not_in_letregion(tr1, tr2,None)

              | FIX{free, shared_clos, functions, scope} =>
                   let val (scope', cont) = tail(scope, cont)
                       val functions' = do_functions(functions)
                   in
                     (FIX{free=free, shared_clos=shared_clos, 
                          functions=functions',scope=scope'}, NEXT)
                   end
    
              | LETREGION{B, rhos = rhos, body = body as 
                          TR(APP(ck,sr,tr1 as TR(VAR{lvar,il,plain_arreffs,
                                               alloc = Some rho_0', (* region-polymorphic function *)
                                               rhos_actuals, other},_,_,_),
                                 tr2),
                             mu,rea,psi)} =>
                  let val (tr2', cont') = tail(tr2, NEXT)
                  in

                    (************************************************************)
                    (* first attempt JMP --- this is all described in detail in *)
                    (*  "Programming with Regions", Chapter "The Function Call" *)
                    (********************************************************** *)

                    (case (params_opt, cont) of
                       (None, RETURN) =>
                           if null(!rhos_actuals) 
                              andalso null(remove_from_bound(!rhos, rho_0'))
                           then 
                            ((*say ("JMP " ^ Lvar.pr_lvar lvar);*)
                             (APP(Some JMP, NOT_YET_DETERMINED, tr1, tr2'), NEXT))
                           else raise NOTJMP
                     | (Some (enclosing_formal_rhos, _), RETURN) => 
                         if 
                             actuals_regions_match_formal_regions(
                                    enclosing_formal_rhos, !rhos_actuals)
                         then 
                          (if null    (* was List.forAll (not o allocates_space) *)
                               (remove_from_bound(!rhos, rho_0'))
                           then ((*say ("JMP " ^ Lvar.pr_lvar lvar);*)
                                 (APP(Some JMP, NOT_YET_DETERMINED, tr1, tr2'), NEXT))
                           else raise NOTJMP
                                         (* note: continuations of the form 
                                            GOTO lvar would be useful here *)
                          )
                         else  raise NOTJMP
                     | _ => raise NOTJMP
                    ) 
                    handle NOTJMP =>
                    (case !rhos_actuals of
                     [] => (* remove allocation region of 
                              region vector, also from surrounding letregion *)
                        let val rhos' = remove_from_bound(!rhos, rho_0')
                        in rhos:= rhos';
                           perhapsTerminate cont
                             (cleanup(LETREGION{B=B, rhos= rhos, body = 
                                 TR(APP(Some FUNCALL, NOT_YET_DETERMINED,
                                     tr1, tr2'), mu,rea,psi)}), NEXT)
                        end
(* future optimisation: pass region vector in STD_ARG1:
                          | [act] => (* remove allocation region of 
                               region vector, also from surrounding letregion *)
                              let val rhos' = remove_from_bound(!rhos, rho_0');
                              in rhos:= rhos';
                               perhapsTerminate cont
                                (cleanup(LETREGION{B=B, rhos= rhos, body = 
                                 TR(APP(Some FUNCALL, NOT_YET_DETERMINED, tr1, tr2'),
                                    mu,rea,psi)}), NEXT)
                              end

*)
                          | _  => (* keep number of letregion-bound variables *)
                             perhapsTerminate cont
                              (LETREGION{B=B, rhos= rhos,
                               body = 
                                TR(APP(Some FUNCALL, NOT_YET_DETERMINED,
                                   tr1, tr2'),mu,rea,psi)}, NEXT)
                         )
                  end
                                                   
              | LETREGION{B, rhos, body} => 
                  (* we do not remove regions that do not allocate space, 
                     for the code generator needs the bindings 
                     to generate code *)
                  let val not_all_dummy_regions = List.exists allocates_space (!rhos)  
                      val (body', cont') = 
                          tail(body, if not_all_dummy_regions then NEXT else cont)
                  in if not_all_dummy_regions
                     then perhapsTerminate cont
                           (LETREGION{B=B, rhos = rhos, body = body'}, cont')
                     else  (LETREGION{B=B, rhos = rhos, body = body'}, cont')
                  end
    
              | LET{k_let,pat, bind, scope} =>
                   let val (scope', cont) = tail(scope, cont)
                       val (bind', cont ) = tail(bind, cont)
                   in
                       (LET{k_let=k_let,pat=pat,bind=bind', scope = scope'}, cont)
                   end
    
              | FN{pat,body,free,alloc} =>
                   (* when we go under a lambda which is the lambda of a fix-bound
                      function, we have knowledge of the formal region variables;
                      if we go under any other lambda, knowledge of formal region parameters
                      of closest enclosing region-polymorphic function are lost, so we
                      must call    tailTrip None   instead of just tail: *)
                   let val (body', _) = 
                        case params_opt of
                          Some(formals, true) (* ie., FN is fix-associated *) =>
                            tailTrip (Some(formals, false)) (body, RETURN)
                        | Some(formals, false) (* i.e., FN is not fix-associated *) =>
                            tailTrip None (body, RETURN)
                        | _ => tail (body, RETURN) (* i.e., continue using None *)
                   in perhapsTerminate cont(FN{pat=pat,body=body',free=free,alloc=alloc}, NEXT)
                   end
              | EXCEPTION(excon,b,mu,alloc,tr) =>
                   let val (tr',cont') = tail(tr, cont)
                   in (EXCEPTION(excon,b,mu,alloc,tr'), NEXT)
                   end
              | RAISE(tr) =>
                   let val (tr',cont') = tail(tr, NEXT)
                   in  (RAISE tr', NEXT)
                   end
              | HANDLE(tr1,tr2) =>
                   let val (tr2',_) = tail(tr2, NEXT) 
                                (* first tr2 is evaluated to a closure, then *)
                       val (tr1',_) = tail(tr1, NEXT) 
                                (* tr1 is evaluated; if tr1 succeeds, 
                                   the closure must be de-allocated *)
                   in  (HANDLE(tr1',tr2'), NEXT)
                   end
              | SWITCH_I sw => (SWITCH_I(tailsw(sw, cont)), NEXT)
              | SWITCH_S sw => (SWITCH_S(tailsw(sw, cont)), NEXT)
              | SWITCH_C sw => (SWITCH_C(tailsw(sw, cont)), NEXT)
              | SWITCH_E sw => (SWITCH_E(tailsw(sw, cont)), NEXT)
              | CON0 a => perhapsTerminate cont(CON0 a, NEXT)
              | CON1(opr,tr) =>
                  let val (tr', cont') = tail(tr, NEXT)
                  in perhapsTerminate cont(CON1(opr,tr'), NEXT)
                  end
              | DECON(opr,tr) =>
                  let val (tr', cont') = tail(tr, NEXT)
                  in perhapsTerminate cont(DECON(opr,tr'), NEXT)
                  end
              | EXCON(excon, None) => perhapsTerminate cont(EXCON(excon, None), NEXT)
              | EXCON(excon, Some(l, tr)) => 
                  let val (tr', cont') = tail(tr, NEXT)
                  in perhapsTerminate cont(EXCON(excon, Some(l, tr')), NEXT)
                  end
              | DEEXCON(excon,tr) => 
                  let val (tr', cont') = tail(tr, NEXT)
                  in perhapsTerminate cont(DEEXCON(excon, tr'), NEXT)
                  end
              | RECORD(a, l) =>
                  let val (l', cont') = tailList(l, NEXT)
                  in perhapsTerminate cont(RECORD(a, l'), cont')
                  end
              | SELECT(i, tr) =>
                  let val (tr', _) = tail(tr, NEXT)
                  in perhapsTerminate cont(SELECT(i, tr'), NEXT)
                  end
              | DEREF(tr) => 
                  let val (tr', _) = tail(tr, NEXT)
                  in perhapsTerminate cont(DEREF(tr'), NEXT)
                  end
              | REF(a,tr) => 
                  let val (tr', _) = tail(tr, NEXT)
                  in perhapsTerminate cont(REF(a,tr'), NEXT)
                  end
              | ASSIGN(a,tr1,tr2) =>
                  let val (tr1', _) = tail(tr1, NEXT)
                      val (tr2', _) = tail(tr2, NEXT)
                  in perhapsTerminate cont(ASSIGN(a,tr1',tr2'), NEXT)
                  end
              | EQUAL(tyinfo,tr1,tr2) =>
                  let val (tr1', _) = tail(tr1, NEXT)
                      val (tr2', _) = tail(tr2, NEXT)
                  in perhapsTerminate cont(EQUAL(tyinfo, tr1',tr2'), NEXT)
                  end
              | CCALL(tyinfo,trs) =>
                  let val (trs', _) = tailList(trs,NEXT)
                  in perhapsTerminate cont(CCALL(tyinfo, trs'), NEXT)
                  end
              | RESET_REGIONS({force,alloc,regions_for_resetting},t) => 
                  let val (t',_) = tail(t,NEXT)
                  in perhapsTerminate cont(RESET_REGIONS({force=force, alloc=alloc,
                                                          regions_for_resetting=regions_for_resetting}, 
                                                         t'), NEXT)
                  end
              | FRAME l => (FRAME l, NEXT)
             )
          end

        and try_prim(t1,lvar, t2, rhos_actuals,cont) = 
             (case Lvar.primitive lvar of
                Some _ => 
                     let val (t2', _) = tail(t2, NEXT)
                     in (APP(None, NOT_YET_DETERMINED, t1, t2'),NEXT)
                     end
              | None => raise NOT_PRIM)
        and do_functions [] = []
          | do_functions ({lvar,occ,tyvars,rhos,epss,Type,
                           rhos_formals,bound_but_never_written_into,other,bind}::rest_functions) =
              let val (bind',_) = tailTrip(Some(!rhos_formals,true))(bind, NEXT) (* proceed to next closure *)
              in
                 {lvar=lvar, occ=occ,tyvars=tyvars, rhos=rhos, epss=epss, 
                  Type=Type,rhos_formals=rhos_formals, 
                  bound_but_never_written_into=bound_but_never_written_into,
                  other=other,
                  bind=bind'} :: do_functions rest_functions
              end
            
        and tailList([], cont)= ([], cont)
          | tailList(e::es, cont) = 
               let val (es', cont) = tailList(es,cont)
                   val (e',  cont) = tail(e, cont)
               in  (e'::es', cont)
               end
  
        and tail(tr as TR(e,mu,phi,mularef_ref), cont) = 
            let val (e', cont)  = tailExp(e,cont)
            in (TR(e', mu, phi, mularef_ref), cont)
            end
      in
         tail
      end
    in
      PGM{expression = #1(tailTrip None (tr, RETURN)),
          export_datbinds = export_datbinds,
          import_vars = import_vars,
          export_vars = export_vars,
          export_basis = export_basis,
          export_Psi = export_Psi}
    end

end;
