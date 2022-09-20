functor RegAlloc(structure CallConv: CALL_CONV
                    where type lvar = Lvars.lvar
                 structure LineStmt: LINE_STMT
                   where type con = Con.con
                   where type excon = Excon.excon
                   where type lvar = Lvars.lvar
                   where type place = Effect.effect
                   where type label = AddressLabels.label
                   where type StringTree = PrettyPrint.StringTree
                   sharing type CallConv.cc = LineStmt.cc
                 structure RI : REGISTER_INFO
                   where type lvar = Lvars.lvar)
    : REG_ALLOC =
struct
  structure Labels = AddressLabels
  structure LS = LineStmt
  structure PP = PrettyPrint
  val _ = Flags.add_bool_entry
    {long="print_register_allocated_program", short=NONE, item=ref false, neg=false,
     menu=["Printing of intermediate forms", "print register allocated program (LineStmt)"],
     desc=""}

  val _ = Flags.add_bool_entry
    {long="register_allocation", short=NONE, item=ref true, neg=true,
     menu=["Control", "register allocation"],
     desc="Perform register allocation. Without register allocation\n\
      \enabled, programs run somewhat slower--but they run and\n\
      \you save about 15 percent on compile time."}

  val region_profiling = Flags.is_on0 "region_profiling"

  type place = Effect.place
  type excon = Excon.excon
  type con = Con.con
  type lvar = Lvars.lvar
  type phsize = LS.phsize
  type pp = LS.pp
  type cc = CallConv.cc
  type label = Labels.label
  type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) LS.LinePrg
  type Atom = LS.Atom
  type StoreTypeLI = LS.StoreType

  datatype StoreType =
    STACK_STY of lvar
  | PHREG_STY of lvar * lvar
  | FV_STY    of lvar * label * label

  fun pr_sty (STACK_STY lv) = Lvars.pr_lvar lv ^ ":stack"
    | pr_sty (PHREG_STY(lv,phreg)) = Lvars.pr_lvar lv ^ ":" ^ LS.pr_phreg phreg
    | pr_sty (FV_STY(lv,l1,l2)) = Lvars.pr_lvar lv ^ ":FV(" ^ Labels.pr_label l1 ^ "," ^ Labels.pr_label l2 ^ ")"

  fun pr_atom atom = LS.pr_atom atom

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun chat (s: string) = if !Flags.chat then print (s ^ "\n") else ()
  fun die s  = Crash.impossible ("RegAlloc." ^ s)
  fun fast_pr stringtree =
    (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , stringtree, !Flags.colwidth);
     TextIO.output(!Flags.log, "\n"))

  fun display (title, tree) =
    fast_pr(PP.NODE{start=title ^ ": ",
                    finish="",
                    indent=3,
                    children=[tree],
                    childsep=PP.NOSEP
                    })

  fun one_in_list([]) = die "one_in_list: list has zero elements."
    | one_in_list([x]) = x
    | one_in_list _ = die "one_in_list: list has more than one element."


  val export_ig_flag = false

  (*************************************************)
  (* Make Call Conventions Explicit at Call Points *)
  (*************************************************)
  local
    fun mk_sty lv = LS.V lv (* Flow variables are annotated later *)
    fun resolve_args ([],lss) = lss
      | resolve_args ((atom,phreg)::args,lss) =
        resolve_args(args,LS.ASSIGN{pat=atom,bind=LS.ATOM(LS.PHREG phreg)}::lss)

    fun resolve_res ([],lss) = lss
      | resolve_res ((atom,phreg)::res,lss) =
        resolve_res(res,LS.ASSIGN{pat=LS.PHREG phreg,bind=LS.ATOM atom}::lss)

    fun CC_sw CC_lss (LS.SWITCH(atom_arg,sels,default)) =
        LS.SWITCH(atom_arg,map (fn (s,lss) => (s,CC_lss lss)) sels, CC_lss default)

    fun CC_ls (LS.FNJMP{opr,args,clos,res,bv},rest) =
      let
        val ({clos,args,res,...},assign_list_args,assign_list_res) =
          CallConv.resolve_app RI.args_phreg RI.res_phreg LS.PHREG {clos=clos,args=args,reg_vec=NONE,reg_args=[],res=res}
      in
          resolve_res(assign_list_args,
                      LS.FNJMP{opr=opr,args=args,clos=clos,res=res,bv=bv}::
                      resolve_args(assign_list_res,rest))
      end
      | CC_ls(LS.FNCALL{opr,args,clos,res,bv},rest) =
        let
          val ({clos,args,res,...},assign_list_args,assign_list_res) =
            CallConv.resolve_app RI.args_phreg RI.res_phreg LS.PHREG {clos=clos,args=args,reg_vec=NONE,reg_args=[],res=res}
        in
          resolve_res(assign_list_args,
                      LS.FNCALL{opr=opr,args=args,clos=clos,res=res,bv=bv}::
                      resolve_args(assign_list_res,rest))
        end
      | CC_ls(LS.JMP{opr,args,reg_vec,reg_args,clos,res,bv},rest) =
        let
          val ({clos,args,res,reg_vec,reg_args},assign_list_args,assign_list_res) =
            CallConv.resolve_app RI.args_phreg RI.res_phreg LS.PHREG {clos=clos,args=args,reg_vec=reg_vec,reg_args=reg_args,res=res}
        in
          resolve_res(assign_list_args,
                      LS.JMP{opr=opr,args=args,reg_vec=reg_vec,reg_args=reg_args,clos=clos,res=res,bv=bv}::
                      resolve_args(assign_list_res,rest))
        end
      | CC_ls(LS.FUNCALL{opr,args,reg_vec,reg_args,clos,res,bv},rest) =
        let
          val ({clos,args,res,reg_vec,reg_args},assign_list_args,assign_list_res) =
            CallConv.resolve_app RI.args_phreg RI.res_phreg LS.PHREG {clos=clos,args=args,reg_vec=reg_vec,reg_args=reg_args,res=res}
        in
          resolve_res(assign_list_args,
                      LS.FUNCALL{opr=opr,args=args,reg_vec=reg_vec,reg_args=reg_args,clos=clos,res=res,bv=bv}::
                      resolve_args(assign_list_res,rest))
        end
      | CC_ls(LS.LETREGION{rhos,body},rest) = LS.LETREGION{rhos=rhos,body=CC_lss body}::rest
      | CC_ls(LS.SCOPE{pat,scope},rest) = LS.SCOPE{pat=pat,scope=CC_lss scope}::rest
      | CC_ls(LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv,bv),offset},rest) =
        LS.HANDLE{default=CC_lss default,handl=(CC_lss handl,handl_lv),handl_return=([],handl_return_lv,bv),offset=offset}::rest
      | CC_ls(LS.HANDLE{default,handl,handl_return,offset},rest) = die "CC_ls: handl_return in HANDLE not empty"
      | CC_ls(LS.SWITCH_I {switch,precision},rest) = LS.SWITCH_I {switch=CC_sw CC_lss switch,
                                                                  precision=precision}::rest
      | CC_ls(LS.SWITCH_W {switch,precision},rest) = LS.SWITCH_W {switch=CC_sw CC_lss switch,
                                                                  precision=precision}::rest
      | CC_ls(LS.SWITCH_S sw,rest) = LS.SWITCH_S(CC_sw CC_lss sw)::rest
      | CC_ls(LS.SWITCH_C sw,rest) = LS.SWITCH_C(CC_sw CC_lss sw)::rest
      | CC_ls(LS.SWITCH_E sw,rest) = LS.SWITCH_E(CC_sw CC_lss sw)::rest
      | CC_ls(LS.CCALL{name,args,rhos_for_result,res},rest) =
        let
          val ({args,rhos_for_result,res},assign_list_args,assign_list_res) =
            CallConv.resolve_ccall RI.args_phreg_ccall RI.res_phreg_ccall LS.PHREG {args=args,rhos_for_result=rhos_for_result,res=res}
        in
          resolve_res(assign_list_args,
                      LS.CCALL{name=name,args=args,rhos_for_result=rhos_for_result,res=res}::
                      resolve_args(assign_list_res,rest))
        end
      | CC_ls(LS.CCALL_AUTO{name,args,res},rest) =
        let
          val ({args,res},assign_list_args,assign_list_res) =
            CallConv.resolve_ccall_auto RI.args_phreg_ccall RI.res_phreg_ccall LS.PHREG {args=args,res=res}
        in
          resolve_res(assign_list_args,
                      LS.CCALL_AUTO{name=name,args=args,res=res}::
                      resolve_args(assign_list_res,rest))
        end
      | CC_ls (ls,rest) = ls::rest

    and CC_lss (lss) = List.foldr (fn (ls,acc) => CC_ls(ls,acc)) [] lss
  in
      fun CC_top_decl(LS.FUN(lab,cc,lss)) =
        let
          val (cc',args,res) = CallConv.resolve_cc RI.args_phreg RI.res_phreg cc
          val args' = map (fn (lv,i) => (LS.VAR lv,i)) args
          val res' = map (fn (lv,i) => (LS.VAR lv,i)) res
          val body_lss = CC_lss(lss)
          val body_args =
             LS.SCOPE{pat=map (mk_sty o #1) args,scope=resolve_args(args',body_lss)}
          val body_res =
            LS.SCOPE{pat=map (mk_sty o #1) res,scope=body_args::resolve_res(res',[])}
        in
          LS.FUN(lab,cc',[body_res])
        end
        | CC_top_decl(LS.FN(lab,cc,lss)) =
        let
          val (cc',args,res) = CallConv.resolve_cc RI.args_phreg RI.res_phreg cc
          val args' = map (fn (lv,i) => (LS.VAR lv,i)) args
          val res' = map (fn (lv,i) => (LS.VAR lv,i)) res
          val body_lss = CC_lss(lss)
          val body_args =
            LS.SCOPE{pat=map (mk_sty o #1) args,scope=resolve_args(args',body_lss)}
          val body_res =
            LS.SCOPE{pat=map (mk_sty o #1) res,scope=body_args::resolve_res(res',[])}
        in
          LS.FN(lab,cc',[body_res])
        end
  end

  fun coalesce_binops lss =
      let fun coalesce_sw f (LS.SWITCH(atom_arg,sels,default)) =
              LS.SWITCH(atom_arg,map (fn (s,lss) => (s,f lss)) sels, f default)
          fun isBinF64 PrimName.Mul_f64 = true
            | isBinF64 PrimName.Plus_f64 = true
            | isBinF64 PrimName.Minus_f64 = true
            | isBinF64 PrimName.Div_f64 = true
            | isBinF64 _ = false
      in case lss of
             nil => nil
           | ls::lss =>
             case ls of
                 LS.PRIM{name=p,args=[x,y],res=[d]} =>   (* treat "d := x op y" as "d := x; d := d op y" *)
                 (if isBinF64 p then
                    (LS.ASSIGN{pat=d,bind=LS.ATOM x} ::
                     LS.PRIM{name=p,args=[d,y],res=[d]} ::
                     coalesce_binops lss)
                  else ls :: coalesce_binops lss)
               | LS.LETREGION{rhos,body} => LS.LETREGION{rhos=rhos,body=coalesce_binops body} :: coalesce_binops lss
               | LS.SCOPE{pat,scope} => LS.SCOPE{pat=pat,scope=coalesce_binops scope} :: coalesce_binops lss
               | LS.HANDLE{default=lss0,
                           handl=(lss1,a),
                           handl_return=(lss2,c,b),
                           offset} => LS.HANDLE{default=coalesce_binops lss0,
                                                handl=(coalesce_binops lss1,a),
                                                handl_return=(coalesce_binops lss2,c,b),
                                                offset=offset} :: coalesce_binops lss
               | LS.SWITCH_I{switch,precision} => LS.SWITCH_I{switch=coalesce_sw coalesce_binops switch,precision=precision} :: coalesce_binops lss
               | LS.SWITCH_W{switch,precision} => LS.SWITCH_W{switch=coalesce_sw coalesce_binops switch,precision=precision} :: coalesce_binops lss
               | LS.SWITCH_S switch => LS.SWITCH_S (coalesce_sw coalesce_binops switch) :: coalesce_binops lss
               | LS.SWITCH_C switch => LS.SWITCH_C (coalesce_sw coalesce_binops switch) :: coalesce_binops lss
               | LS.SWITCH_E switch => LS.SWITCH_E (coalesce_sw coalesce_binops switch) :: coalesce_binops lss
               | _ => ls :: coalesce_binops lss
      end

  (*****************************)
  (*    REGISTER ALLOCATION    *)
  (*****************************)

  (* -----------------------------
   *  Assignment of store types
   *  to lambda variables.
   * ----------------------------- *)

  fun ra_assign (assign : StoreTypeLI -> StoreType) lss =
    let
      fun ra_assign_sw ra_assign_lss (LS.SWITCH(atom_arg,sels,default)) =
        LS.SWITCH(atom_arg,map (fn (s,lss) => (s,ra_assign_lss lss)) sels, ra_assign_lss default)

      fun ra_assign_ls ls =
        case ls
          of LS.ASSIGN a => LS.ASSIGN a
           | LS.FLUSH a => die "ra_dummy_ls: FLUSH not inserted yet."
           | LS.FETCH a => die "ra_dummy_ls: FETCH not inserted yet."
           | LS.FNJMP a => LS.FNJMP a
           | LS.FNCALL a => LS.FNCALL a
           | LS.JMP a => LS.JMP a
           | LS.FUNCALL a => LS.FUNCALL a
           | LS.LETREGION{rhos,body} => LS.LETREGION{rhos=rhos,body=ra_assign_lss body}
           | LS.SCOPE{pat,scope} => LS.SCOPE{pat=map assign pat,scope=ra_assign_lss scope}
           | LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv,bv),offset} =>
            LS.HANDLE{default=ra_assign_lss default,handl=(ra_assign_lss handl,handl_lv),
                      handl_return=([],handl_return_lv,bv),offset=offset}
           | LS.HANDLE{default,handl,handl_return,offset} => die "ra_dummy_ls: handl_return in HANDLE not empty"
           | LS.RAISE{arg,defined_atys} => LS.RAISE{arg=arg,defined_atys=defined_atys}
           | LS.SWITCH_I {switch,precision} => LS.SWITCH_I{switch=ra_assign_sw ra_assign_lss switch,
                                                           precision=precision}
           | LS.SWITCH_W {switch,precision} => LS.SWITCH_W{switch=ra_assign_sw ra_assign_lss switch,
                                                           precision=precision}
           | LS.SWITCH_S sw => LS.SWITCH_S(ra_assign_sw ra_assign_lss sw)
           | LS.SWITCH_C sw => LS.SWITCH_C(ra_assign_sw ra_assign_lss sw)
           | LS.SWITCH_E sw => LS.SWITCH_E(ra_assign_sw ra_assign_lss sw)
           | LS.RESET_REGIONS a => LS.RESET_REGIONS a
           | LS.PRIM a => LS.PRIM a
           | LS.CCALL a => LS.CCALL a
           | LS.CCALL_AUTO a => LS.CCALL_AUTO a
           | LS.EXPORT a => LS.EXPORT a

      and ra_assign_lss lss = List.foldr (fn (ls,acc) => ra_assign_ls ls :: acc) [] lss

    in ra_assign_lss lss
    end

  (* -----------------------------------
   * Dummy Assignment; implement all
   * lambda variables on the stack.
   * ----------------------------------- *)

  fun ra_dummy_prg funcs =
    let
      fun assign(LS.V lv) = STACK_STY lv
        | assign(LS.FV lv) = FV_STY lv
      fun ra_assign_func assign func =
        case func
          of LS.FUN(lab,cc,lss) => LS.FUN(lab,cc,ra_assign assign lss)
           | LS.FN(lab,cc,lss) => LS.FN(lab,cc,ra_assign assign lss)
    in
      foldr (fn (func,acc) => ra_assign_func assign (CC_top_decl func) :: acc) [] funcs
    end

  (* ----------------------------------------
   *  Register allocation with coalescing
   * ---------------------------------------- *)

  (* We record statistics for the entire program module.                     *)
  (* Invariant: no_of_nodes = coalesce + spills + assigned_colors            *)
  (* Invariant: no_of_moves = coalesced_moves+constrained_moves+frozen_moves *)
  (* We do not consider pre-colored variables.                               *)
  local
    val no_of_nodes     = ref 0
    val spills          = ref 0
    val assigned_colors = ref 0

    val no_of_moves       = ref 0
    val coalesced_moves   = ref 0
    val constrained_moves = ref 0
    val frozen_moves      = ref 0

    val no_call = ref 0
    val c_call  = ref 0
    val ml_call = ref 0
  in
    fun fix_int i = StringCvt.padLeft #" " 7 (Int.toString i)

    fun procent (t:int,b:int) =
      if b=0 then "(---)"
      else "("^ StringCvt.padLeft #" " 3 (Int.toString (Real.round((Real.fromInt t) / (Real.fromInt b) * 100.0))) ^ "%)"

    fun pp_stat () =
        if !Flags.chat then
          (chat ("Number of nodes.....: " ^ fix_int(!no_of_nodes) ^ procent(!no_of_nodes,!no_of_nodes));
           chat ("Spilled nodes.......: " ^ fix_int(!spills) ^ procent(!spills,!no_of_nodes));
           chat ("Assigned_colors.....: " ^ fix_int(!assigned_colors) ^ procent(!assigned_colors,!no_of_nodes));
           chat ("Number of moves.....: " ^ fix_int(!no_of_moves) ^ procent(!no_of_moves,!no_of_moves));
           chat ("Coalesced moves.....: " ^ fix_int(!coalesced_moves) ^ procent(!coalesced_moves,!no_of_moves));
           chat ("Constrained moves...: " ^ fix_int(!constrained_moves) ^ procent(!constrained_moves,!no_of_moves));
           chat ("Frozen moves........: " ^ fix_int(!frozen_moves) ^ procent(!frozen_moves,!no_of_moves));
           chat ("Lvar crosses no call: " ^ fix_int(!no_call) ^ procent(!no_call,!no_call+(!c_call)+(!ml_call)));
           chat ("Lvar crosses C call.: " ^ fix_int(!c_call) ^ procent(!c_call,!no_call+(!c_call)+(!ml_call)));
           chat ("Lvar crosses ML call: " ^ fix_int(!ml_call) ^ procent(!ml_call,!no_call+(!c_call)+(!ml_call))))
        else ()

    fun reset_stat () =
        (no_of_nodes := 0;
         spills := 0;
         assigned_colors := 0;
         no_of_moves := 0;
         coalesced_moves := 0;
         constrained_moves := 0;
         frozen_moves := 0;
         no_call := 0;
         c_call := 0;
         ml_call := 0)

    fun inc_initial () = no_of_nodes := !no_of_nodes+1
    fun inc_spills () = spills := !spills+1
    fun inc_assigned_colors () = assigned_colors := !assigned_colors+1
    fun inc_moves () = no_of_moves := !no_of_moves+1
    fun inc_coalesce () = coalesced_moves := !coalesced_moves+1
    fun inc_constrained () = constrained_moves := !constrained_moves+1
    fun inc_frozen () = frozen_moves := !frozen_moves+1
    fun inc_no_call () = no_call := !no_call+1
    fun inc_c_call () = c_call := !c_call+1
    fun inc_ml_call () = ml_call := !ml_call+1
  end

  (* Register allocation is entirely local, thus, we can assume that all ints in the key are unique! *)
  fun key lv = #1 (Lvars.key lv)

  datatype worklist_enum =
    precolored_enum | initial_enum | simplifyWorklist_enum | freezeWorklist_enum |
    spillWorklist_enum | spilledNodes_enum | coalescedNodes_enum | coloredNodes_enum |
    selectStack_enum

  fun pr_worklist wl =
      case wl of
          precolored_enum => "precolored"
        | initial_enum => "initial"
        | simplifyWorklist_enum => "simplify"
        | freezeWorklist_enum => "freeze"
        | spillWorklist_enum => "potential-spill"
        | spilledNodes_enum => "spilled"
        | coalescedNodes_enum => "coalesced"
        | coloredNodes_enum => "colored"
        | selectStack_enum => "select"

  datatype live_range_status =
    no_call | c_call | ml_call (* Does the live range cross a c-call or a ml-call. *)

  fun merge_lrs (no_call,s2) = s2
    | merge_lrs (c_call,ml_call) = ml_call
    | merge_lrs (s1,_) = s1

  type count = int
  type key = int
  type node = {key:key, lv:lvar, degree: count ref, mv_related: count option ref,
               worklist: worklist_enum ref, adjList: key list ref, (* for precolored nodes, adjList is empty  *)
               alias: key option ref, color: key option ref,       (* the key in color represents a register *)
               lrs: live_range_status ref, uses : count ref}

  fun key' (n : node) = #key n

  (* Precolored nodes *)
  val precolored : node list =
      map (fn lv => {key=key lv,lv=lv,degree=ref 0, mv_related=ref NONE,
                     worklist=ref precolored_enum, adjList=ref nil,
                     alias = ref NONE, color=ref (SOME (key lv)),
                     lrs = ref no_call, uses = ref 0})
          RI.caller_save_phregs

  fun reset_precolored () =
      app (fn ({key,lv,degree, mv_related, worklist, adjList, alias, color, lrs, uses} : node) =>
              (degree:=0; mv_related:=NONE; worklist:=precolored_enum;
               adjList:=nil; alias:=NONE; color:=SOME key; lrs:=no_call; uses:=0))
          precolored

  val K = length precolored

  structure S = NatSet

  (* Work lists *)
  val initial          : node list ref = ref []
  val simplifyWorklist : node list ref = ref []
  val freezeWorklist   : node list ref = ref []
  val spillWorklist    : node list ref = ref []
  val spilledNodes     : node list ref = ref []
  val coalescedNodes   : node list ref = ref []
  val coloredNodes     : node list ref = ref []
  val selectStack      : node list ref = ref []

  fun worklistsReset () =
      (initial := nil; simplifyWorklist := nil;
       freezeWorklist := nil; spillWorklist := nil;
       spilledNodes := nil; coalescedNodes := nil;
       coalescedNodes := nil; coloredNodes := nil;
       selectStack := nil)

  fun isEmpty nil = true
    | isEmpty _ = false

  local
    fun get (wle:worklist_enum) (wl:node list ref) : node option =
        case !wl of
            nil => NONE
          | n::ns => ( wl := ns
                     ; if !(#worklist n) = wle then SOME n
                       else get wle wl
                     )
  in
    fun simplifyWorklistGet () = get simplifyWorklist_enum simplifyWorklist
    fun freezeWorklistGet () = get freezeWorklist_enum freezeWorklist
    fun spillWorklistAll () : node list =
        let val nodes = List.filter (fn n => !(#worklist n) = spillWorklist_enum)
                                    (!spillWorklist)
        in spillWorklist := nodes ; nodes
        end
  end

  local
     fun add (wl:node list ref) c (n:node) = (#worklist n := c; wl := n :: !wl)
  in
     fun initialAdd n = add initial initial_enum n
     fun simplifyWorklistAdd n = add simplifyWorklist simplifyWorklist_enum n
     fun freezeWorklistAdd n = (add freezeWorklist freezeWorklist_enum n)
     fun spillWorklistAdd n = add spillWorklist spillWorklist_enum n
     fun spilledNodesAdd n = add spilledNodes spilledNodes_enum n
     fun coalescedNodesAdd n = add coalescedNodes coalescedNodes_enum n
     fun coloredNodesAdd n = add coloredNodes coloredNodes_enum n
  end

  datatype movelist_enum =
           coalescedMoves_enum | constrainedMoves_enum |
           frozenMoves_enum | worklistMoves_enum | activeMoves_enum

  (* why is a move not an instruction? There may be several moves
   * from lv1 into lv2; hmm, no because we use SML *)
  type move = {k1:key, k2:key, movelist:movelist_enum ref}

  (* Move lists *)
  val coalescedMoves   : move list ref = ref []
  val constrainedMoves : move list ref = ref []
  val frozenMoves      : move list ref = ref []
  val worklistMoves    : move list ref = ref []
  val activeMoves      : move list ref = ref []

  fun movelistsReset () =
      (coalescedMoves := nil; constrainedMoves := nil;
       frozenMoves := nil; worklistMoves := nil; activeMoves := nil)

  local
    fun get (mle:movelist_enum) (ml:move list ref) : move option =
        case !ml of
            nil => NONE
          | m :: ms => ( ml := ms
                       ; if !(#movelist m) = mle then SOME m
                         else get mle ml
                       )
  in fun worklistMovesGet () = get worklistMoves_enum worklistMoves
  end

  local fun add (ml:move list ref) c (m:move) = (#movelist m := c; ml := m :: !ml)
  in fun coalescedMovesAdd m = add coalescedMoves coalescedMoves_enum m
     fun constrainedMovesAdd m = add constrainedMoves constrainedMoves_enum m
     fun frozenMovesAdd m = add frozenMoves frozenMoves_enum m
     fun worklistMovesAdd m = add worklistMoves worklistMoves_enum m
     fun activeMovesAdd m = add activeMoves activeMoves_enum m
  end

  (* nTable; table from lvar keys to nodes *)
(*
  local
    structure M = IntFinMap
    val nTableInit : node M.map = M.fromList (map (fn n => (key' n, n)) precolored)
    val nTable : node M.map ref = ref nTableInit
  in
    fun nTableLookup i : node option = M.lookup (!nTable) i
    fun nTableAdd (i:int, n:node) : unit = nTable := M.add(i,n,!nTable)
    fun nTableReset () = nTable := nTableInit
  end
*)

  local
    structure H = Polyhash
    val nTable : (int, node) H.hash_table =
        H.mkTable (fn x => x, op =) (500,Fail "RegAlloc.nTable")
  in
    fun nTableLookup i : node option = H.peek nTable i
    fun nTableAdd (i:int, n:node) : unit = H.insert nTable (i,n)
    fun nTableReset () : unit =
        ( H.clear nTable
        ; app (fn n => nTableAdd (key' n, n)) precolored
        )
    val () = nTableReset ()
  end

  (* moveList; table from lvar keys to moves *)
(*
  local
    structure M = IntFinMap
    val mTable : (move list ref) M.map ref = ref M.empty
  in
    fun moveListLookup i : move list =
        case M.lookup (!mTable) i of
            SOME rl => !rl
          | NONE => nil
    fun moveListAdd (i:int, m:move) : unit =
        case M.lookup (!mTable) i of
            SOME rl => rl := m :: !rl
          | NONE => mTable := M.add(i,ref [m],!mTable)
    fun moveListReset () = mTable := M.empty
  end
*)
  local
    structure H = Polyhash
    val mTable : (int, move list ref) H.hash_table  =
        H.mkTable (fn x => x, op =) (500,Fail "RegAlloc.mTable")
  in
    fun moveListLookup i : move list =
        case H.peek mTable i of
            SOME rl => !rl
          | NONE => nil
    fun moveListAdd (i:int, m:move) : unit =
        case H.peek mTable i of
            SOME rl => rl := m :: !rl
          | NONE => H.insert mTable (i,ref [m])
    fun moveListReset () =
        H.clear mTable
  end

  local
    structure H = Polyhash
    val adjSet : (int, S.Set ref) H.hash_table =
        H.mkTable (fn x => x, op =) (500,Fail "RegAlloc.adjSet")
  in
    fun adjSetMember (i1,i2) : bool =
        if i1 < i2 then (case H.peek adjSet i1 of
                             SOME s => S.member (Word.fromInt i2) (!s)
                           | NONE => false)
        else adjSetMember(i2,i1)
    fun adjSetAdd (i1,i2) : unit =
        if i1 < i2 then (case H.peek adjSet i1 of
                             SOME s => if S.member (Word.fromInt i2) (!s) then ()
                                       else s := S.insert (Word.fromInt i2) (!s)
                           | NONE => H.insert adjSet (i1,ref(S.singleton (Word.fromInt i2))))
        else adjSetAdd(i2,i1)
    fun adjSetReset () =
        H.clear adjSet
  end

  fun raReset () =
      (worklistsReset(); movelistsReset(); nTableReset();
       moveListReset(); adjSetReset(); reset_precolored())

  fun Adjecent (n:node) : node list =
      foldl (fn (k,acc) => case nTableLookup k of
                               SOME n =>
                               (case !(#worklist n) of
                                    selectStack_enum => acc
                                  | coalescedNodes_enum => acc
                                  | _ => n::acc)
                             | NONE => die "Adjecent") nil (!(#adjList n))

  fun NodeMoves (n:node) : move list =
      foldl (fn (m,acc) =>
                case !(#movelist m) of
                    activeMoves_enum => m::acc   (* nodes that have never been potentially *)
                  | worklistMoves_enum => m::acc (* nodes that are already potentially *)
                  | _ => acc)
            nil (moveListLookup (key' n))

  fun MoveRelated (n:node) : bool =
      case NodeMoves n of
          nil => false
        | _ => true

  fun EnableMoves (nodes:node list) : unit =
      app (fn n =>
              app (fn m =>
                      case !(#movelist m) of
                          activeMoves_enum => worklistMovesAdd m
                        | _ => ())
                  (NodeMoves n))
          nodes

  fun GetAliasKey (k : int) : node =
      case nTableLookup k of
          SOME n =>
          if !(#worklist n) = coalescedNodes_enum then
            case !(#alias n) of
                SOME i => GetAliasKey i
              | NONE => die ("GetAliasKey.1: key=" ^ Int.toString k)
          else n
        | NONE => die ("GetAliasKey.2: key=" ^ Int.toString k)

  fun GetAliasNode (n:node) : node =
      GetAliasKey (#key n)

  fun pr_node ({key,lv,degree,mv_related,worklist = ref wl,adjList,
                alias = ref NONE,color = ref (SOME color_key),lrs,uses}:node) =
      "{key: " ^ Int.toString key ^ ",lv:" ^ Lvars.pr_lvar lv ^ ",alias:NONE,color:" ^
      Int.toString color_key ^ ",wl:" ^ pr_worklist wl ^ "}"
    | pr_node {key,lv,degree,mv_related,worklist = ref wl,adjList,alias = ref (SOME a_id),color = ref (SOME color_key),lrs,uses} =
      "{key: " ^ Int.toString key ^ ",lv:" ^ Lvars.pr_lvar lv ^ ",alias:" ^
      Lvars.pr_lvar (#lv(GetAliasKey a_id)) ^ ",color:" ^ Int.toString color_key ^ ",wl:" ^ pr_worklist wl ^ "}"
    | pr_node {key,lv,degree,mv_related,worklist = ref wl,adjList,alias = ref (SOME a_id),color = ref NONE,lrs,uses} =
      "{key: " ^ Int.toString key ^ ",lv:" ^ Lvars.pr_lvar lv ^ ",alias:" ^
      Lvars.pr_lvar (#lv(GetAliasKey a_id)) ^ ",wl:" ^ pr_worklist wl ^ ",color:NONE}"
    | pr_node {key,lv,degree,mv_related,worklist = ref wl,adjList,alias = ref NONE,color = ref NONE,lrs,uses} =
      "{key: " ^ Int.toString key ^ ",lv:" ^ Lvars.pr_lvar lv ^ ",wl:" ^ pr_worklist wl ^ ",alias:NONE,color:NONE}"

  fun pr_precolored () =
      (print "\nPrecolored[";map (print o pr_node) precolored;print "]\n")

  fun DecrementDegree (m:node) : unit =
      let val d = !(#degree m)
      in #degree m := d - 1;
         if d = K then
           (EnableMoves(m :: Adjecent m);
            if MoveRelated m then freezeWorklistAdd m
            else simplifyWorklistAdd m)
         else ()
      end

  local
    fun push (n:node) : unit = (selectStack := n :: (!selectStack);
                                (#worklist n) := selectStack_enum)
  in
    fun simplify () : bool =
        case simplifyWorklistGet() of
            SOME n => (push n; app DecrementDegree (Adjecent n); true)
          | NONE => false
  end

  fun AddEdge (u:key, v:key) : unit =
      if u = v orelse adjSetMember(u,v) then ()
      else ( adjSetAdd(u,v);
             case nTableLookup u of
                 SOME u_node =>
                 (case nTableLookup v of
                      SOME v_node =>
                      (if !(#worklist u_node) <> precolored_enum then
                         ((#adjList u_node) := v :: !(#adjList u_node);
                          (#degree u_node) := !(#degree u_node) + 1)
                       else ();
                       if !(#worklist v_node) <> precolored_enum then
                         ((#adjList v_node) := u :: !(#adjList v_node);
                          (#degree v_node) := !(#degree v_node) + 1)
                       else ())
                    | NONE => ())
               | NONE => ()
           )

  fun AddEdge' (u:word, v:word) : unit =
      AddEdge(Word.toInt u, Word.toInt v)

  fun MakeWorklist () =
      let fun do_n n =
              if !(#degree n) >= K
              then spillWorklistAdd n
              else if MoveRelated n
              then freezeWorklistAdd n
              else simplifyWorklistAdd n
      in app do_n (!initial)
      end

  fun AddWorkList (u : node) : unit =
      if !(#worklist u) <> precolored_enum andalso not(MoveRelated u) andalso !(#degree u) < K then
        simplifyWorklistAdd u
      else ()

  fun OK (t : node, r : node) : bool =
      !(#degree t) < K orelse !(#worklist t) = precolored_enum orelse adjSetMember(key' t, key' r)

  fun Conservative (nodes:S.Set) : bool =
      let val nodes = map (fn k => case nTableLookup (Word.toInt k) of
                                       SOME n => n
                                     | NONE => die "Conservative") (S.list nodes)
      in (foldl (fn (n,k) => if !(#degree n) >= K then k+1 else k) 0 nodes) < K
      end

  fun check_same_kind s (u:node) (v:node) : unit =
      let fun ubf64_of_node (n:node) : bool = Lvars.get_ubf64 (#lv u)
      in if ubf64_of_node u = ubf64_of_node v then ()
         else die ("check_same_kind(" ^ s ^ "): different node kinds")
      end

  fun Combine (u : node, v : node) : unit = (* v is never precolored *)
      (check_same_kind "Combine" u v;
       coalescedNodesAdd v;
       if !(#worklist u) <> precolored_enum
       then (* We only merge lrs for non precolored lvars. 19/03/1999, Niels *)
         #lrs u := merge_lrs(!(#lrs u),!(#lrs v))
       else ();
       #alias v := SOME(key' u);
       let val key_u = key' u
       in app (fn m => moveListAdd (key_u,m)) (moveListLookup (key' v))
       end;
       app (fn t => (AddEdge(key' t,key' u); DecrementDegree t)) (Adjecent v);
       if !(#degree u) >= K andalso !(#worklist u) = freezeWorklist_enum then
         spillWorklistAdd u
       else ())

  fun coalesce () : bool =
      case worklistMovesGet() of
          SOME(m as ({k1,k2,movelist,...}:move)) =>
          let val x = GetAliasKey k1
              val y = GetAliasKey k2
              val (u,v) = if !(#worklist y) = precolored_enum then (y,x)
                          else (x,y)
          in if key' u = key' v then (coalescedMovesAdd m;
                                      inc_coalesce();
                                      AddWorkList u;
                                      true)
             else if !(#worklist v) = precolored_enum orelse adjSetMember(key' u,key' v) then
               (inc_constrained();
                constrainedMovesAdd m;
                AddWorkList u;
                AddWorkList v;
                true)
             else if (!(#worklist u) = precolored_enum andalso
                      (foldl (fn (t, acc) => acc andalso OK(t,u)) true (Adjecent v)))
                     orelse
                     (!(#worklist u) <> precolored_enum andalso
                      Conservative(S.union (S.fromList(map (Word.fromInt o #key) (Adjecent u)))
                                           (S.fromList(map (Word.fromInt o #key) (Adjecent v))))) then
               (coalescedMovesAdd m;
                inc_coalesce();
                Combine(u,v);
                AddWorkList u;
                true)
             else
               (activeMovesAdd m; true)
          end
        | NONE => false

  fun FreezeMoves (u: node) : unit =
      let fun on_move (m as {k1,k2,movelist,...}:move) : unit =
              let val v = if key'(GetAliasKey k2) = key'(GetAliasNode u) then GetAliasKey k1
                          else GetAliasKey k2
              in inc_frozen();
                 frozenMovesAdd m;
                 if !(#worklist v) <> precolored_enum (*18/03/1999, Niels*)
                    andalso isEmpty (NodeMoves v) andalso !(#degree v) < K then
                   simplifyWorklistAdd v
                 else ()
              end
      in app on_move (NodeMoves u)
      end

  fun freeze () : bool =  (* invariant : freezeWorklist is normalised and non-empty *)
      case freezeWorklistGet() of
          SOME u => (simplifyWorklistAdd u; FreezeMoves u; true)
        | NONE => false

  fun selectSpill () : bool =
      let fun pri (n:node) = Real.fromInt(!(#uses n)) / Real.fromInt(!(#degree n))
      in case spillWorklistAll() of        (* use lowest priority: uses/degree*lrs_factor *)
             m :: rest =>
             let val m = #1(foldl (fn (n,(m,mpri)) =>
                                      let val npri = pri n
                                      in if mpri < npri then (m,mpri) else (n,npri)
                                      end) (m,pri m) rest)
             in simplifyWorklistAdd m
              ; FreezeMoves m
              ; true
             end
           | _ => false
      end

  val caller_save_regset =
      S.fromList (map (Word.fromInt o key) RI.caller_save_phregs)

  val callee_save_regs =
      S.empty

  val f64_phregset =
      S.fromList (map (Word.fromInt o key) RI.f64_phregs)

  val callee_save_ccall_phregset =
      S.fromList (map (Word.fromInt o key) RI.callee_save_ccall_phregs)

  fun getOne (s:S.Set) : word option = S.getOne s

  fun AssignColors () : unit =
    let
      fun assign_color (n:node,pri1,pri2,notOkColors) =
          case getOne (S.difference pri1 notOkColors) of
              NONE => (case getOne (S.difference pri2 notOkColors) of
                           NONE => (spilledNodesAdd n; inc_spills())
                         | SOME c => ( coloredNodesAdd n
                                     ; #color n := SOME (Word.toInt c)
                                     ; inc_assigned_colors()
                                     )
                      )
            | SOME c => ( coloredNodesAdd n
                        ; #color n := SOME (Word.toInt c)
                        ; inc_assigned_colors()
                        )

      fun find_color (n:node,notOkColors) =
          if Lvars.get_ubf64(#lv n) then
            (inc_no_call(); assign_color(n,f64_phregset,S.empty,notOkColors))
          else
            case !(#lrs n) of
                c_call =>    (* means: only ccall *)
                (inc_c_call(); assign_color(n,callee_save_regs,S.empty,notOkColors))  (* RI.caller_save_phregset*)
              | ml_call =>   (* means: ml call and/or c call; we have to be carefull that rbx is not assigned as it may be destroyed by an ml-call *)
                (inc_ml_call(); assign_color(n,callee_save_regs,S.empty,notOkColors))
              | no_call =>
                (* prioritise to use caller-save regs so that callee-save regs are
                 * available for those variables with live ranges accross calls *)
                (inc_no_call(); assign_color(n,caller_save_regset,callee_save_ccall_phregset,notOkColors))

      fun pop_loop (ns : node list) =
          case ns of
              nil => app (fn n => if !(#worklist n) = coalescedNodes_enum then
                                    (#color n := !(#color(GetAliasNode n)); inc_assigned_colors())
                                  else ()) (!coalescedNodes)
            | n::ns =>
              let
                val _ = if !(#worklist n) = coalescedNodes_enum
                        then die "assigning color to coalesced node"
                        else ()
                val notOkColors =
                    foldl (fn (k:key,set) =>
                              let val n = GetAliasKey k
                              in if (case !(#worklist n) of
                                         coloredNodes_enum => true
                                       | precolored_enum => true
                                       | _ => false)
                                 then case !(#color(GetAliasKey k)) of
                                          SOME c => S.insert (Word.fromInt c) set
                                        | NONE => die "pop_loop"
                                 else set
                              end) S.empty (!(#adjList n))
              in
                find_color(*_simple*)(n,notOkColors);
                pop_loop ns
              end
    in pop_loop (!selectStack)
    end

  (* args_on_stack_lvs is the set of those lvars that are passed to
   * the function on the stack! *)

  fun MakeInitial lss =
      let fun add_use lv =
              let val i = key lv
              in case nTableLookup i of
                     SOME n => #uses n := !(#uses n) + 1
                   | NONE => ()
              end
          fun add lv =
              let val i = key lv
              in case nTableLookup i of
                     SOME n => () (* Multiple definition in switch *)
                   | NONE =>
                     let val n : node = {key=i,lv=lv,degree=ref 0, mv_related=ref NONE,
                                         worklist=ref initial_enum, adjList=ref nil,
                                         alias = ref NONE, color=ref NONE,
                                         lrs = ref no_call, uses = ref 0}
                     in nTableAdd(i,n); initial := n :: !initial; inc_initial()
                     end
              end
          fun mk_sw mk (LS.SWITCH(a,sels,default)) =
              (app add_use (LS.get_var_atom(a,nil));
               app (fn (_,lss) => app mk lss) sels;
               app mk default)
          fun default ls =
              let val (def,use) = LS.def_use_lvar_ls ls
              in app add def;
                 app add_use use
              end
          fun mk ls =
              case ls of
                  LS.FLUSH _ => die "MakeInitial: FLUSH not inserted yet."
                | LS.FETCH _ => die "MakeInitial: FETCH not inserted yet."
                | LS.LETREGION{rhos,body} => app mk body
                | LS.SCOPE{pat,scope} => app mk scope
                | LS.HANDLE{default,handl=(handl_lss,handl_lv),
                            handl_return=(handl_return_lss,handl_return_lv,bv),offset} =>
                  (app add (LS.get_var_atom (handl_lv,nil));
                   app add (LS.get_var_atom (handl_return_lv,nil));
                   app mk handl_lss;
                   app mk default;
                   app mk handl_return_lss)
                | LS.SWITCH_I {switch,precision} => mk_sw mk switch
                | LS.SWITCH_W {switch,precision} => mk_sw mk switch
                | LS.SWITCH_S sw => mk_sw mk sw
                | LS.SWITCH_C sw => mk_sw mk sw
                | LS.SWITCH_E sw => mk_sw mk sw
                | ls as LS.CCALL _ => (app add RI.args_phreg_ccall; default ls)
                | ls as LS.CCALL_AUTO _ => (app add RI.args_phreg_ccall; default ls)
                | ls => default ls
      in app mk lss
      end

  (* args_on_stack_lvs is the set of those lvars that are passed to
   * the function on the stack! These we do not introduce as nodes in
   * the IG because they are not to be colored. *)

  structure S = NatSet
  fun keyw lv = Word.fromInt (key lv)
  fun delete s e = S.remove e s
  fun add s e = S.insert e s
  fun lvarset_atom a = S.fromList (map keyw (LS.get_var_atom(a,nil)))
  fun lvarsetof lvs = S.fromList (map keyw lvs)

  fun Build (args_on_stack_lvs, lss) =
    let
      val args_on_stack_lvs = lvarsetof args_on_stack_lvs
      fun set_lrs_status new_s k =
          case nTableLookup (Word.toInt k) of
              SOME {lrs = (lrs as ref old_s),...} => lrs := merge_lrs(old_s,new_s)
            | NONE => die "set_lrs_status - nTableLookup failed"
      fun lvarset_app f lvs =
          (*S.apply f lvs*) S.fold (fn e => fn () => f e) () lvs
      fun def_use_var_ls ls =
          let val (def,use) = LS.def_use_var_ls ls
          in (lvarsetof def, S.difference (lvarsetof use) args_on_stack_lvs)
          end
      fun use_var_ls ls =
          S.difference (lvarsetof(LS.use_var_ls ls)) args_on_stack_lvs

      fun ig_sw (ig_lss, LS.SWITCH (a, sel, def), L) =
          let val Ls = map (fn (_, lss) => ig_lss(lss, L)) sel
              val L = foldl (fn (a,b) => S.union a b) (ig_lss(def,L)) Ls
          in S.union L (S.difference (lvarset_atom a) args_on_stack_lvs)
          end
      fun do_non_tail_call (L, ls) =
          let val (def, use) = def_use_var_ls ls  (* def=flv(res) *)
              val lvars_to_flush = S.difference L def
              val _ =
                  case ls of
                      LS.CCALL _ => lvarset_app (set_lrs_status c_call) lvars_to_flush
                    | LS.CCALL_AUTO _ => lvarset_app (set_lrs_status c_call) lvars_to_flush
                    | _ => lvarset_app (set_lrs_status ml_call) lvars_to_flush
              val L = S.union L def  (* We insert edges between def'ed variables *)
              val _ = lvarset_app (fn d => lvarset_app (fn u => AddEdge'(d,u)) L) def
              val L = S.union use lvars_to_flush
          in L
          end
      fun do_tail_call (L, ls) =
          let val (def, use) = def_use_var_ls ls
              val _ = lvarset_app (fn d => lvarset_app (fn u => AddEdge'(d,u)) def) def
              (* We insert edges between def'ed variables *)
              val L = use_var_ls ls
          in L
          end
      fun do_record (L,ls) = (* We must insert edges between def and use! *)
          let val (def,use) = def_use_var_ls ls
              val L' = S.union (S.union L def) use
              val _ = lvarset_app (fn d => lvarset_app (fn l => AddEdge'(l,d)) L') def
              val L = S.union use (S.difference L def)
          in L
          end
      fun do_move (L,lv1,lv2) = (* lv1 <-- lv2 *)
          let val k1 = keyw lv1
              val k2 = keyw lv2
          in
            if S.member k1 args_on_stack_lvs then
              if S.member k2 args_on_stack_lvs then L
              else add L k2
            else if S.member k2 args_on_stack_lvs then
              (lvarset_app (fn l => AddEdge'(l,k1)) L;
               delete L k1)
            else
              let val _ = inc_moves()
                  val move : move = {k1=Word.toInt k1, k2=Word.toInt k2,
                                     movelist=ref worklistMoves_enum}
                  val _ = (moveListAdd(Word.toInt k1, move); moveListAdd(Word.toInt k2, move))
                  val _ = worklistMovesAdd move
                  val _ = lvarset_app (fn l => AddEdge'(l,k1)) (delete L k2)
                  val L = add (delete L k1) k2
              in L
              end
          end
      fun remove_finite_rhos ([]) = []
        | remove_finite_rhos (((place,LS.WORDS i),offset)::rest) = remove_finite_rhos rest
        | remove_finite_rhos (rho::rest) = rho :: remove_finite_rhos rest
      fun ig_ls (ls, L) =
        case ls
          of LS.FLUSH _ => die "ig_ls: FLUSH not inserted yet."
           | LS.FETCH _ => die "ig_ls: FETCH not inserted yet."
           | LS.FNJMP _ => do_tail_call(L,ls)
           | LS.FNCALL _ => do_non_tail_call(L,ls)
           | LS.JMP _ => do_tail_call(L,ls)
           | LS.FUNCALL _ => do_non_tail_call(L,ls)
           | LS.LETREGION{rhos,body} =>
            let
              val L' = ig_lss(body,L)

              (* Infinite letregions involve C calls and so do
               * finite regions when profiling is enabled. C calls
               * are involved both at entrance to the body and at
               * exit of the body, thus, we mark both members of L
               * and L' as crossing C calls. The live range status
               * setting that appears here should be in sync with
               * FetchAndFlush.sml, although the live range status
               * setting is not crucial for soundness of the
               * register allocator. *)

              (* Update live range status for live variables, if C
               * calls are involved. *)
              val _ = if List.null rhos orelse ( not(region_profiling())
                                                 andalso List.null (remove_finite_rhos rhos) ) then ()
                      else (lvarset_app (set_lrs_status c_call) L ;
                            lvarset_app (set_lrs_status c_call) L')
            in L'
            end
           | LS.SCOPE{pat,scope} => ig_lss(scope, L)
           (* File Thesis/handle.ps contains a drawing of liveness wrt. handle. 17/02/1999, Niels *)
           | LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv,bv),offset} =>
           let
             (* We must flush all caller save registers that are live *)
             (* after the handle. We define handl_return_lv in the    *)
             (* handle construct. 19/03/1999, Niels                   *)
             val L' = ig_lss(handl, ig_lss(default, L))
             val handl_return_key = keyw (one_in_list (LS.get_var_atom (handl_return_lv,nil)))
             val _ = lvarset_app (set_lrs_status ml_call) (delete L' handl_return_key)
             val _ = lvarset_app (fn l => AddEdge'(l,handl_return_key)) L'  (* ME 1999-08-14 *)
           in
             L'
           end
           | LS.HANDLE{default,handl,handl_return,offset} => die "ra_ls: handl_return in HANDLE not empty"
           | LS.SWITCH_I {switch,precision} => ig_sw (ig_lss, switch, L)
           | LS.SWITCH_W {switch,precision} => ig_sw (ig_lss, switch, L)
           | LS.SWITCH_S sw => ig_sw (ig_lss, sw, L)
           | LS.SWITCH_C sw => ig_sw (ig_lss, sw, L)
           | LS.SWITCH_E sw => ig_sw (ig_lss, sw, L)
           | LS.CCALL _ => do_non_tail_call(L,ls)
           | LS.CCALL_AUTO _ => do_non_tail_call(L,ls)
           | LS.ASSIGN {pat=LS.VAR lv1, bind=LS.ATOM(LS.VAR lv2)} => do_move(L,lv1,lv2)
           | LS.ASSIGN {pat=LS.VAR lv1, bind=LS.ATOM(LS.PHREG lv2)} => do_move(L,lv1,lv2)
           | LS.ASSIGN {pat=LS.PHREG lv1, bind=LS.ATOM(LS.VAR lv2)} => do_move(L,lv1,lv2)
           | LS.ASSIGN {pat=LS.PHREG lv1, bind=LS.ATOM(LS.PHREG lv2)} => do_move(L,lv1,lv2)
          (* Instead, we should unfold records in LineStmt. 18/02/1999, Niels *)
           | LS.ASSIGN {pat= _, bind=LS.RECORD _} => do_record(L,ls)
           | LS.ASSIGN {pat= _, bind=LS.SCLOS_RECORD _} => do_record(L,ls)
           | LS.ASSIGN {pat= _, bind=LS.REGVEC_RECORD _} => do_record(L,ls)
           | LS.ASSIGN {pat= _, bind=LS.CLOS_RECORD _} => do_record(L,ls)
           | _ => (* general *)
            let val (def, use) = def_use_var_ls ls
                val L = S.union L def   (* to introduce edges between defs *)
                val _ = lvarset_app (fn d => lvarset_app (fn l => AddEdge'(l,d)) L) def
                val L = S.union use (S.difference L def)
            in L
            end

      and ig_lss ([], L) = L
        | ig_lss (ls::lss, L) = let val L = ig_lss(lss, L)
                                in ig_ls(ls, L)
                                end
    in ig_lss (lss, S.empty); ()
    end

  val tm_repeat = Timing.new()
  val tm_assign = Timing.new()
  val tm_assign_cols = Timing.new()
  val tm_mkinitial = Timing.new()
  val tm_mkworklist = Timing.new()
  val tm_build = Timing.new()
  val tm_coalesce = Timing.new()

  val timing = false
  fun wrap t f x =
      if timing then Timing.wrap t f x
      else f x

  fun pp_tm (s,t) = s ^ " : " ^ Timing.pr t
  fun pp_timings () =
      if not timing then ()
      else
        print (String.concatWith "\n"
                                 (map pp_tm
                                      [ ("RA.repeat", tm_repeat)
                                      , ("RA.assign", tm_assign)
                                      , ("RA.assignCols", tm_assign_cols)
                                      , ("RA.mkinitial", tm_mkinitial)
                                      , ("RA.mkworklist", tm_mkworklist)
                                      , ("RA.build", tm_build)
                                      , ("RA.coalesce", tm_coalesce)
                                      ]
                                 ) ^ "\n")

  local
    structure H = Polyhash
  in
  val phregKeyToLv : key -> lvar =
      let val regs = RI.f64_phregs @ RI.all_regs
          val m = H.mkTable (fn x => x, op =) (50,Fail "RegAlloc.phregTable")
          val () = app (fn lv => H.insert m (key lv, lv)) regs
      in fn k => case H.peek m k of
                     SOME lv => lv
                   | NONE => die ("phregKeyToLv: no lv found for key " ^ Int.toString k)
      end
  end

  fun ra_body (fun_name, args_on_stack_lvs, lss) =
      let fun repeat () =
              if simplify() then repeat()
              else if coalesce() then repeat()
              else if freeze() then repeat()
              else if selectSpill() then repeat()
              else ()
          fun assign (LS.V lv) =
              (case nTableLookup (key lv) of
                   SOME n => (case !(#color n) of
                                  SOME c => PHREG_STY (lv,phregKeyToLv c)
                                | NONE => STACK_STY lv)
                 | NONE => die "ra_body.assign: lvar not assigned a color")
            | assign(LS.FV lv) = FV_STY lv

        val lss = wrap tm_coalesce coalesce_binops lss
        val _ = (raReset();
                 wrap tm_mkinitial MakeInitial lss;
                 (*print ("MakeInitial done - " ^ fun_name ^ "\n");*)
                 wrap tm_build Build (args_on_stack_lvs, lss);
                 (*print ("Build done\n");*)
                 wrap tm_mkworklist MakeWorklist ();
                 (*print ("MakeWorklist done\n");*)
                 wrap tm_repeat repeat ();
                 (*print ("repeat done\n");*)
                 wrap tm_assign_cols AssignColors ();
                 (*print ("AssignColors done\n");*)
                 ()
                )

        val res = Timing.wrap tm_assign (ra_assign assign) lss
    in raReset(); res
    end

  fun ra_top_decl f =
      let val f = CC_top_decl f

          fun process (lab,cc,lss) =
              let val args_on_stack_lvs = CallConv.get_spilled_args cc
              in (lab, cc, ra_body (Labels.pr_label lab, args_on_stack_lvs, lss))
              end

      in (* fast_pr (LineStmt.layout_line_prg LineStmt.pr_sty (fn _ => "") pr_atom false [f]);  *)
        case f of
            LS.FUN f => LS.FUN(process f)
          | LS.FN f => LS.FN(process f)
      end

  fun ra_prg funcs =
      foldr (fn (func,acc) => ra_top_decl func :: acc) [] funcs

  (*******************************************************)
  (* Function to invoke the register allocator of choice *)
  (*******************************************************)
  fun ra_main {main_lab:label,
               code=line_prg: (StoreTypeLI,unit,Atom) LinePrg,
               imports:label list * label list,
               exports:label list * label list} ra_prg =
    let val _ = chat "[Register allocation..."
        val _ = reset_stat()
        val line_prg_ra = ra_prg line_prg
        val _ =
            if Flags.is_on "print_register_allocated_program" then
              display("\nReport: AFTER REGISTER ALLOCATION",
                      LS.layout_line_prg pr_sty (fn _ => "()") pr_atom false line_prg_ra)
            else ()
        val _ = pp_stat();
        val () = pp_timings();
        val _ = chat "]\n"
    in
      {main_lab=main_lab,code=line_prg_ra: (StoreType,unit,Atom) LinePrg,imports=imports,exports=exports}
    end

  fun ra_dummy code = ra_main code ra_dummy_prg
  fun ra code = ra_main code ra_prg

end
