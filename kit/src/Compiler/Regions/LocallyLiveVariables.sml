(* Locally live variables: second pass of Storage Mode Analysis *)

structure LocallyLiveVariables: LOCALLY_LIVE_VARIABLES = 
struct
  structure Eff = Effect
  structure PP = PrettyPrint
  type lvar = Lvars.lvar
  type lvarset = Lvarset.lvarset
  type place = Eff.place
  type effect = Eff.effect
  type mul = MulExp.mul
  type qmularefset = MulExp.qmularefset
  type excon = Excon.excon
  type StringTree = PrettyPrint.StringTree

  type ('a,'b,'c)LambdaPgm = ('a,'b,'c)MulExp.LambdaPgm
  type pgm = (place, place*mul, qmularefset ref)MulExp.LambdaPgm
  type mulexp = (place, place*mul, qmularefset ref)MulExp.LambdaExp
  type multrip = (place, place*mul, qmularefset ref)MulExp.trip


  (* ---------------------------------------------------------------------- *)
  (*    General Abbreviations                                               *)
  (* ---------------------------------------------------------------------- *)

  fun log s             = TextIO.output(!Flags.log,s ^ "\n")
  fun device(s)         = TextIO.output(!Flags.log, s)            
  fun dump(t)           = PrettyPrint.outputTree(device, t, !Flags.colwidth)
  fun die errmsg        = Crash.impossible ("LocallyLiveVariables." ^ errmsg)
  fun unimplemented x   = Crash.unimplemented ("RegFlow." ^ x)

  (* -----------------------------------------------------------------------*)
  (* Debugging flags; updated from Flags each time main function in module  *)
  (* (AtInference) is called                                                *)
  (* -----------------------------------------------------------------------*)

  (* ---------------------------------------------------------------------- *)
  (*    Utility functions                                                   *)
  (* ---------------------------------------------------------------------- *)

  fun footnote(x,y) = x
  infix footnote

  fun noSome x errmsg =
    case x of 
      NONE => die errmsg
    | SOME y => y



  (* ---------------------------------------------------------------------- *)
  (* Computing locally live variables and inserting them in the syntax      *)
  (* tree at allocation points.                                             *)
  (* ---------------------------------------------------------------------- *)

   fun union_list[] = Lvarset.empty
     | union_list[set] = set
     | union_list(set::rest) = Lvarset.union(set,union_list rest)

   fun union(lvarset1,lvarset2) = Lvarset.union(lvarset1,lvarset2)
   val empty = Lvarset.empty
   fun difference(lvarset, lvars) = 
            foldl (fn (lvar, set) => 
                Lvarset.delete(set,lvar)) lvarset lvars
   fun add(lvarset, lvar) = Lvarset.add(lvarset, lvar)
   fun delete(lvarset, lvar) = Lvarset.delete(lvarset,lvar)
   fun singleton lvar = Lvarset.singleton lvar

   fun findLvar pred (liveset as (lvarset,_)) = Lvarset.findLvar pred lvarset

   fun norm lvarset = lvarset
   fun fromList(lvars) = Lvarset.lvarsetof(lvars)


    (*******************************************)
    (* sets of (locally live) lvars and excons *)
    (*******************************************)

   type liveset = lvarset * Excon.excon list 
   fun norm_liveset(lvarset, excons) = (norm lvarset, excons)

   fun layout_liveset (liveset) = 
      case norm_liveset liveset of 
        (lvarset, excons) =>
         PrettyPrint.NODE{start = "{", finish = "}", indent =1, childsep = PrettyPrint.RIGHT",",
                          children = map (PrettyPrint.LEAF o Lvars.pr_lvar) (Lvarset.members lvarset) @
                                     map (PrettyPrint.LEAF o Excon.pr_excon) excons}





  type mulexp_llv = (place*liveset, place*mul, qmularefset ref)MulExp.LambdaExp
  type trip_llv = (place*liveset, place*mul, qmularefset ref)MulExp.trip

  val empty_liveset = (empty, [])

  fun diff_llv (liveset as (xs,es) : liveset, lvars': lvar list) : liveset =
    let
	val xs'' = difference(xs, lvars')
    in
      (xs'', es)
    end

  fun union_excons ([], es') = es'
    | union_excons (es, [])   = es
    | union_excons (es, es') = 
            foldl (fn (excon, res) => 
		       if List.exists (fn excon' => Excon.eq(excon,excon')) es' then res 
		       else excon :: res) es' es

  fun union_llv((xs, es), (xs',es')) = (union(xs,xs'), union_excons(es,es'))

  fun union_many(l : liveset list): liveset = 
      (union_list(map #1 l),
       foldr union_excons [] (map #2 l))

  fun add_lvar ((lvars, excons), lvar) = 
      (add(lvars, lvar), excons)

  fun add_excon ((lvars, excons), excon) = 
      (lvars, if List.exists(fn excon' => Excon.eq(excon',excon)) excons then excons else excon :: excons)

  fun delete_lvar ((lvars, excons), lvar) = 
      (delete(lvars, lvar), excons)

  fun delete_lvars ((lvars, excons), lvars') = 
      (difference(lvars, lvars'), excons)

  fun delete_excon ((lvars, excons), excon) = 
      (lvars, List.filter(fn excon' => not(Excon.eq(excon',excon))) excons)

  fun findExcon pred (_,excons) = 
           let fun loop[] = NONE
                 | loop (excon::rest) = 
                     case pred excon of
                       NONE => loop rest
                     | SOME result => SOME(excon,result)
           in loop excons
           end

  val union_llvs =
      foldl (fn (liveset, res) => union_llv(liveset, res)) empty_liveset 

  (*******************************************)
  (* annotating programs with livesets       *)
  (*                                         *)
  (* this is done is a backwards scan through*)
  (* the program, which is assumed to be in  *)
  (* K-normal form                           *)
  (* *****************************************)

  exception AbortExpression 

  local open MulExp
  in

    fun cp_triv_exp (VAR{lvar,il,plain_arreffs,fix_bound=false,rhos_actuals = ref [] ,other}) =
             VAR{lvar=lvar,il=il,plain_arreffs=plain_arreffs,
                 fix_bound=false,
                 rhos_actuals= ref [],
                 other = other}
      | cp_triv_exp (VAR{lvar, ...}) = die
            ("cp_triv_exp: lvar badly annotated: " ^ Lvars.pr_lvar lvar)
      | cp_triv_exp (INTEGER(i, t, place)) = INTEGER(i, t, (place, empty_liveset)) (* the 
            lvarset is not
            needed for integers, since no code is generated for it *)
      | cp_triv_exp (WORD(i, t, place)) = WORD(i, t, (place, empty_liveset)) (* the 
            lvarset is not
            needed for integers, since no code is generated for it *)
      | cp_triv_exp (RECORD(place,[])) = RECORD((place,empty_liveset), []) (* the 
            lvarset is not
            needed for the unit value, it is represented unboxed *)
      | cp_triv_exp _ = die
            ("cp_triv_exp: not an atomic expression (expected K-normal form)")
  
    fun cp_triv (TR(e, meta,l, r)) = TR(cp_triv_exp(e), meta,l,r)
  
    fun freeInTrivExp (VAR{lvar,...}) = (singleton lvar, [])
      | freeInTrivExp (INTEGER _    ) = (empty, [])
      | freeInTrivExp (WORD _    )    = (empty, [])
      | freeInTrivExp (RECORD(_,[]))  = (empty, [])
      | freeInTrivExp _  = die
                     "sub-expression not atomic (expected K-normal form)"

    fun freeInTriv(TR(e, _, _, _)) = freeInTrivExp e
  
    fun freeInTrivList []                 = empty_liveset
      | freeInTrivList (TR(VAR{lvar, ...}, _, _, _)::er) = add_lvar(freeInTrivList er, lvar)
      | freeInTrivList (_::er)            = freeInTrivList er
  
    fun freeInTriv' (TR(UB_RECORD trs, _, _, _)) = freeInTrivList trs
      | freeInTriv' tr = freeInTriv tr

    (* Function llv -- compute live variables.
     * Input:  expression e and the (lvars, excons) sets live at e.
     * Effect: decorate e and its subexpressions with the live (lvars, excons)
     * Output: the (lvars, excons) sets free in e itself paired with the
               llv-annotated output expression
     *)
  
    fun llv(tr: multrip as TR(e,meta,l,r), liveset as (xs, es): liveset) : trip_llv * liveset =
      let
          val (e', liveset') = llvExp(e, liveset)
      in
          (TR(e',meta,l,r), liveset')
      end
  
    and llvExp(e: mulexp, liveset as (xs, es) : liveset) : mulexp_llv * liveset  =
      let 
        fun llv_switch(SWITCH(e,branches,e_opt), liveset) =
  	  (* Note: e is trivial *)
  	  let val children = map (fn (c,e) => llv (e,liveset)) branches
              val freeInRhs = union_llvs  (map #2 children)
  	      val (e_opt_llv, freeInOpt) = case e_opt of 
  		                 NONE   => (NONE, empty_liveset)
  			       | SOME e => 
                                      let val (e_llv, llv') = llv (e, liveset)
                                      in (SOME e_llv, llv')
                                      end
              val newBranches = ListPair.zip(map #1 branches,
                                             map #1 children)
  	  in  
               (SWITCH(cp_triv e, newBranches, e_opt_llv),
  	      union_llv(freeInRhs, union_llv(freeInOpt, freeInTriv e)))
  	  end
      in
      case e of
        VAR{lvar,...} => (cp_triv_exp e, (singleton lvar, []))
      | INTEGER(i,t,a) =>  
	  if RType.unboxed t then (cp_triv_exp e, empty_liveset)
	  else (INTEGER(i,t, (a, norm_liveset liveset)), empty_liveset)
      | WORD(i,t,a) =>
	  if RType.unboxed t then (cp_triv_exp e, empty_liveset)
	  else (WORD(i,t, (a, norm_liveset liveset)), empty_liveset)
      | STRING(s,place)  => (STRING(s, (place, norm_liveset liveset)),  empty_liveset)
      | REAL(r,place)    => (REAL(r, (place, norm_liveset liveset)),    empty_liveset)
      | UB_RECORD(trs)   => 
             let val children = map (fn tr => llv(tr, liveset)) trs
             in
                 (UB_RECORD(map #1 children),
                  union_many(map #2 children)
                 )
             end
      | FN{pat,body,free,alloc = p} =>
    	  let val (body',freeInBody) = llv(body, empty_liveset) 
  	      val for_closure: liveset  = 
                               delete_lvars(freeInBody, map #1 pat)
  	  in
            (FN{pat=pat,body = body',free = free, 
                alloc = (p, norm_liveset(union_llv(liveset, for_closure)))},
   	     for_closure)
  	end
                
      | LETREGION{B,rhos,body} =>
          let val (body', liveset') = llv(body, liveset)
          in  
             (LETREGION{B=B,rhos=rhos,body = body'},
              liveset')  (* note that rhos need not be subtracted: they are not lvars *)
          end
      | LET{k_let,pat,bind,scope} =>
    	  let val (scope',freeInScope) = llv(scope, liveset)
              val bound_lvars = map #1 pat
     	      val liveAtRhs   = delete_lvars(freeInScope, bound_lvars)
  	      val (bind',freeInRhs)   = llv(bind, union_llv(liveAtRhs, liveset))
          in 
              (LET{k_let=k_let,pat=pat, bind = bind', scope= scope'},
  	       union_llv(freeInRhs, liveAtRhs))
  	end


      | FIX{free,shared_clos = rho,functions,scope}=>
  	let val (scope',freeInScope) = llv(scope, liveset)
            val children = map (fn function => llv(#bind function, empty_liveset)) functions
  	    val freeInRhs   = union_llvs (map #2 children)
  	    val boundByLhs  = map #lvar functions
  	    (* XXX PS: The calculation in AtInference29a3 seemed wrong: *)
  	    val localFree   = diff_llv(union_llv(freeInRhs, freeInScope),
  				       boundByLhs)
  	in
           (FIX{free =free, shared_clos = (rho, norm_liveset(union_llv(localFree, liveset))),
                functions = 
                  map(fn({lvar,occ,tyvars,rhos,epss,Type,rhos_formals,
                          bound_but_never_written_into,
                          other,bind = _}, bind) =>
                       {lvar=lvar,occ=occ,tyvars=tyvars,rhos=rhos,epss=epss,Type=Type,
                        rhos_formals = rhos_formals,
                        bound_but_never_written_into=bound_but_never_written_into,
                        other=other,bind = bind})
                     (ListPair.zip(functions,map #1 children)),
                scope = scope'},
	    localFree)
  	end

      | APP(ck,sr,tr1 as TR(VAR{lvar = f,il,plain_arreffs,fix_bound=true, 
                                rhos_actuals, other},meta,phi,psi),
                  tr2) =>  (* equation 23 and 24 in popl 96 paper *)
        let 
            val liveset = norm_liveset liveset
            val (tr2',live_tr2) = llv(tr2, liveset)
            val liveset_fx = norm_liveset(union_llv(live_tr2,add_lvar(liveset, f)))  (* see equation 24 *)
        in
          (APP(ck,sr,TR(VAR{lvar=f,il=il,plain_arreffs=plain_arreffs,
                                      fix_bound=true, (* see (24) *)
                                      rhos_actuals = ref (map (fn rho_act => 
                                             (* see (23) *) (rho_act,liveset)) (!rhos_actuals)),
                                      other = other}, meta,phi,psi),
                     tr2'),
           add_lvar(freeInTriv' tr2, f))
        end

      | APP(ck,sr,tr1 as TR(VAR{lvar = f,il,plain_arreffs,fix_bound=false,
                                rhos_actuals = ref [], other},meta,phi,psi),
                  tr2) =>  (* equation missing in popl paper! *)
        let
            (* Note that tr1 and tr2 are trivial (variables) because of
     	       K Normal Form *)
           val (tr2',free_tr2) = llv(tr2,liveset)
           val (tr1',free_tr1) = llv(tr1,liveset)
        in
           (APP(ck,sr,tr1',tr2'),
            union_llv(free_tr1,free_tr2))
        end
(*
      | APP(ck,sr,tr1 as TR(VAR{lvar, il, plain_arreffs,fix_bound=false, rhos_actuals,other}, meta,phi,psi),tr2) =>
           (* non-empty list of actual regions: has to be primitive lvar *)
          (case Lvars.primitive lvar of
             SOME _ => let 
                         val liveset = norm_liveset liveset
                         val (tr2',live_tr2) = llv(tr2, liveset)
                       in
                           (APP(ck,sr,TR(VAR{lvar=lvar,il=il,plain_arreffs=plain_arreffs,
                                      fix_bound=false,
                                      rhos_actuals = ref (map (fn rho_act => 
                                             (* see (23) *) (rho_act,liveset)) (!rhos_actuals)),
                                      other = other}, meta,phi,psi),
                                tr2'),
                            live_tr2) (* do not include free_tr1: the lvar will be inlined! *)
                       end

           | NONE => die  "ill-formed application (expected K-normal form)"
          )
*)
      | APP(ck,sr, _ , _) =>
           die  "ill-formed application; operator not variable (expected K-normal form)"
      
      | EXCEPTION(excon,b,mu,rho,tr1) =>
         let val (tr1',freeInScope) = llv(tr1, liveset)
         in
             (EXCEPTION(excon,b,mu,(rho,norm_liveset(liveset)),tr1'),
              delete_excon(freeInScope, excon))
         end

      | RAISE(tr1) =>
         let val (tr1', free_in_tr1) = llv(tr1,liveset) (* it might be ok to 
                                      replace "liveset" by "empty_liveset" *)
         in (RAISE tr1', free_in_tr1)
         end

      | HANDLE(tr1, tr2) =>
         (* here tr2 is trivial, but tr1 may be non-trivial *)
         let
            val (tr2', live_in_tr2) = llv(tr2, liveset)
            val (tr1', live_in_tr1) = llv(tr1, union_llv(live_in_tr2, liveset))
         in
           (HANDLE(tr1',tr2'), 
            union_llv(live_in_tr2, live_in_tr1)
           )
         end
      | SWITCH_I {switch,precision} => 
          let val (switch', liveset') = llv_switch(switch,liveset)
          in (SWITCH_I {switch=switch',precision=precision},liveset')
          end 
      | SWITCH_W {switch,precision} => 
          let val (switch', liveset') = llv_switch(switch,liveset)
          in (SWITCH_W {switch=switch',precision=precision},liveset')
          end 
      | SWITCH_C(switch) => 
          let val (switch', liveset') = llv_switch(switch,liveset)
          in (SWITCH_C(switch'),liveset')
          end 
      | SWITCH_S(switch) => 
          let val (switch', liveset') = llv_switch(switch,liveset)
          in (SWITCH_S(switch'),liveset')
          end 
      | SWITCH_E(SWITCH(e,branches,e_opt)) => 
  	  (* Note: e is trivial *)
  	  let fun doBranch(c,e) = 
                  let val (e', free_in_e) = llv(e, liveset)
                  in ((c,e'), add_excon(free_in_e,  c))
                  end
              val children = map doBranch branches
              val freeInRules = union_llvs (map #2 children)
  	      val (opt',freeInOpt) = case e_opt of 
  		                 NONE   => (NONE,empty_liveset)
  			       | SOME e => 
                                   let val (e',livee') = llv (e, liveset)
                                   in (SOME e', livee')
                                   end
              val (e', freee')= llv(e,liveset)
  	  in  
            (SWITCH_E(SWITCH(e',map #1 children, opt')),
  	      union_llv(freeInRules, union_llv(freeInOpt, freee')))
  	  end
  
      | CON0{con,il,aux_regions,alloc} =>
         let val livehere= norm_liveset liveset
         in
          (CON0{con=con,il=il,aux_regions= map (fn rho => (rho,livehere)) aux_regions,
                alloc = (alloc,livehere)},
           empty_liveset)
         end

      | CON1({con,il,alloc},tr1) => (* tr1 is trivial *)
         let val (tr1',freeInArgs) = llv(tr1, liveset)
             val livehere = norm_liveset(union_llv(liveset, freeInArgs))
         in 
            (CON1({con=con,il=il,alloc = (alloc,livehere)}, tr1'),
             freeInArgs)
         end
      | DECON({con,il}, tr1) => (* tr1 is trivial *)
         let        
             val (tr1', free_tr1) = llv(tr1, liveset)
         in 
             (DECON({con=con, il=il}, tr1'),
              free_tr1)
         end

      | EXCON(excon,NONE) => (EXCON(excon,NONE), empty_liveset)
      | EXCON(excon,SOME(rho,tr1)) => (* tr1 trivial *)
         let        
             val (tr1', free_tr1) = llv(tr1, liveset)
         in 
             (EXCON(excon,SOME((rho, norm_liveset(union_llv(liveset,free_tr1))),tr1')),
              add_excon(free_tr1,excon))
         end
      | DEEXCON(excon,tr1) => (* tr1 trivial *)
         let        
             val (tr1', free_tr1) = llv(tr1, liveset)
         in 
             (DEEXCON(excon,tr1'),
              free_tr1)
         end
          
      | RECORD(rho, trs) => (* elements of trs trivial *)
         let val children = map (fn tr => llv(tr, liveset)) trs
             val freeInArgs = union_many(map #2 children)
         in
            (RECORD((rho,norm_liveset(union_llv(freeInArgs, liveset))), map #1 children),
             freeInArgs)
	 end
  
      | SELECT(i,tr1) =>(* tr1 trivial *)
         let        
             val (tr1', free_tr1) = llv(tr1, liveset)
         in 
             (SELECT(i, tr1'),free_tr1)
         end
      | DEREF(tr1) => (* tr1 trivial *)
         let        
             val (tr1', free_tr1) = llv(tr1, liveset)
         in 
             (DEREF(tr1'),free_tr1)
         end
      | REF(rho,tr1) => (* tr1 trivial *)
         let
             val (tr1', free_tr1) = llv(tr1, liveset)
         in
             (REF((rho,norm_liveset(union_llv(free_tr1, liveset))), tr1'),
              free_tr1)
         end
      | ASSIGN(rho,tr1,tr2) => (* tr1 and tr2 trivial *)
         let
             val (tr1',free_tr1) = llv(tr1, liveset)
             val (tr2',free_tr2) = llv(tr2, liveset)
         in 
             (ASSIGN((rho,norm_liveset(liveset)), tr1',tr2'),
              union_llv(free_tr1,free_tr2))
         end
      | DROP(tr1) =>
         let val (tr1', free_in_tr1) = llv(tr1,liveset) (* it might be ok to 
                                      replace "liveset" by "empty_liveset" *)
         in (DROP tr1', free_in_tr1)
         end
      | EQUAL({mu_of_arg1,mu_of_arg2,alloc = rho}, tr1, tr2) => (* tr1 and tr2 trivial *)
         let
             val (tr1',free_tr1) = llv(tr1, liveset)
             val (tr2',free_tr2) = llv(tr2, liveset)
         in 
             (EQUAL({mu_of_arg1=mu_of_arg1, mu_of_arg2 = mu_of_arg2, 
                     alloc = (rho, norm_liveset(union_many[free_tr1,free_tr2,liveset]))},
                    tr1',tr2'),
              union_llv(free_tr1,free_tr2))
         end
      | CCALL ({name, mu_result, rhos_for_result}, trs) =>  (* trs consists of trivial expressions only*)
         let 
            val children = map (fn tr => llv(tr, liveset)) trs
            val freeInChildren = union_many(map #2 children)
            val liveset_here = norm_liveset(union_llv(freeInChildren, liveset))
         in
           (CCALL ({name = name, mu_result = mu_result,
		    rhos_for_result = 
		      map (fn (rho, i_opt) => ((rho, liveset_here), i_opt))
		        rhos_for_result},
	           map #1 children),
            freeInChildren)
         end
      | EXPORT(i,tr1) =>
         let val (tr1', free_in_tr1) = llv(tr1,liveset)
         in (EXPORT(i,tr1'), free_in_tr1)
         end
      | RESET_REGIONS({force, alloc = rho,regions_for_resetting}, tr1) => (* tr1 is trivial *)
         let 
            val liveset = norm_liveset liveset
            val (tr1', free_tr1) = llv(tr1, liveset)
         in
            (RESET_REGIONS({force=force, 
                            alloc = (rho, liveset),
                            regions_for_resetting = 
                              map (fn rho => (rho,liveset)) regions_for_resetting}, tr1'),
             free_tr1)
         end
      | FRAME{declared_lvars,declared_excons} =>
         (FRAME{declared_lvars=declared_lvars, declared_excons=declared_excons}, 
          (* we have to assume that subsequent program unit refer to 
             all exported lvars and excons: (previously, I had this set to be empty) *)
           (fromList (map #lvar declared_lvars), map #1 declared_excons)
         )
      end 
          handle Crash.CRASH  => 
                      (log "Locally Live Variables failed at expression:\n";
                       dump(MulInf.layoutExp e);
                       raise AbortExpression)
                
(* llv *)

   (* exported components: *)

    val  llv = fn (PGM{expression,export_datbinds,import_vars,export_vars, 
                       export_basis,export_Psi})=>
      let
         val (tr',_) = llv(expression, empty_liveset)
                       handle AbortExpression => die "llv failed"
      in
         PGM{expression = tr', export_datbinds=export_datbinds,import_vars= import_vars,
             export_vars= export_vars,export_basis= export_basis,export_Psi = export_Psi}
      end

  end (* local open MulExp *)
end; (*LocallyLiveVariables*)


