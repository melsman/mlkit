functor RegAlloc(structure Con : CON
		 structure Excon : EXCON
		 structure Lvars : LVARS
		 structure Effect : EFFECT
		 structure Lvarset : LVARSET
		   sharing type Lvarset.lvar = Lvars.lvar
		 structure Labels : ADDRESS_LABELS
		 structure CallConv: CALL_CONV
		 structure LineStmt: LINE_STMT
	           sharing type Con.con = LineStmt.con
                   sharing type Excon.excon = LineStmt.excon
                   sharing type Lvars.lvar = LineStmt.lvar = CallConv.lvar
                   sharing type Effect.effect = Effect.place = LineStmt.place
                   sharing type Labels.label = LineStmt.label
                   sharing type CallConv.cc = LineStmt.cc
		 structure BI : BACKEND_INFO
		   sharing type BI.lvar = Lvars.lvar
		   sharing type BI.lvarset = Lvarset.lvarset
		 structure PP : PRETTYPRINT
		   sharing type PP.StringTree = 
                                Effect.StringTree = 
				LineStmt.StringTree
                 structure Flags : FLAGS
		 structure Report : REPORT
		   sharing type Report.Report = Flags.Report
		 structure Crash : CRASH) : REG_ALLOC =
struct

  structure LS = LineStmt

  type lvarset = Lvarset.lvarset
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
    
  fun pr_sty(STACK_STY lv) = Lvars.pr_lvar lv ^ ":stack"
    | pr_sty(PHREG_STY(lv,phreg)) = Lvars.pr_lvar lv ^ ":" ^ LS.pr_phreg phreg
    | pr_sty(FV_STY(lv,l1,l2)) = Lvars.pr_lvar lv ^ ":FV(" ^ Labels.pr_label l1 ^ "," ^ Labels.pr_label l2 ^ ")"

  fun pr_atom atom = LS.pr_atom atom

  (***********)
  (* Logging *)
  (***********)
  fun log s = TextIO.output(!Flags.log,s ^ "\n")
  fun chat(s: string) = if !Flags.chat then print (s ^ "\n") else ()
  fun die s  = Crash.impossible ("RegAlloc." ^ s)
  fun fast_pr stringtree = 
    (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , stringtree, !Flags.colwidth);
     TextIO.output(!Flags.log, "\n"))

  fun display(title, tree) =
    fast_pr(PP.NODE{start=title ^ ": ",
		    finish="",
		    indent=3,
		    children=[tree],
		    childsep=PP.NOSEP
		    })

  fun one_in_list([]) = die "one_in_list: list has zero elements."
    | one_in_list([x]) = x
    | one_in_list _ = die "one_in_list: list has more than one element."

  (****************************************************************)
  (* Add Dynamic Flags                                            *)
  (****************************************************************)

  val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Printing of intermediate forms"],x,y,r))
    [("print_register_allocated_program", "print register allocated program (LineStmt)", ref false)]

  val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Control","Lambda Backend"],x,y,r))
    [("export_ig_graph", "Export IG graph in xvcg file", ref false)]

  val export_ig_flag = Flags.lookup_flag_entry "export_ig_graph"

  (*************************************************)
  (* Make Call Conventions Explicit at Call Points *)
  (*************************************************)
  local
    fun mk_sty lv = LS.V lv (* Flow variables are annotated later *)
    fun resolve_args([],lss) = lss
      | resolve_args((atom,phreg)::args,lss) = 
      resolve_args(args,LS.ASSIGN{pat=atom,bind=LS.ATOM(LS.PHREG phreg)}::lss)

    fun resolve_res([],lss) = lss
      | resolve_res((atom,phreg)::res,lss) = 
      resolve_res(res,LS.ASSIGN{pat=LS.PHREG phreg,bind=LS.ATOM atom}::lss)

    fun CC_sw CC_lss (LS.SWITCH(atom_arg,sels,default)) =
      LS.SWITCH(atom_arg,map (fn (s,lss) => (s,CC_lss lss)) sels, CC_lss default)

    fun CC_ls(LS.FNJMP{opr,args,clos,free,res,bv},rest) =
      let
	val ({clos,args,free,res,...},assign_list_args,assign_list_res) = 
	  CallConv.resolve_app LS.PHREG {clos=clos,free=free,args=args,reg_vec=NONE,reg_args=[],res=res}
	in
	  resolve_res(assign_list_args,
		      LS.FNJMP{opr=opr,args=args,clos=clos,free=free,res=res,bv=bv}::
		      resolve_args(assign_list_res,rest))
	end
	| CC_ls(LS.FNCALL{opr,args,clos,free,res,bv},rest) =
	let
	  val ({clos,args,free,res,...},assign_list_args,assign_list_res) = 
	    CallConv.resolve_app LS.PHREG {clos=clos,free=free,args=args,reg_vec=NONE,reg_args=[],res=res}
	in
	  resolve_res(assign_list_args,
		      LS.FNCALL{opr=opr,args=args,clos=clos,free=free,res=res,bv=bv}::
		      resolve_args(assign_list_res,rest))
	end
	| CC_ls(LS.JMP{opr,args,reg_vec,reg_args,clos,free,res,bv},rest) =
	let
	  val ({clos,args,free,res,reg_vec,reg_args},assign_list_args,assign_list_res) = 
	    CallConv.resolve_app LS.PHREG {clos=clos,free=free,args=args,reg_vec=reg_vec,reg_args=reg_args,res=res}
	in
	  resolve_res(assign_list_args,
		      LS.JMP{opr=opr,args=args,reg_vec=reg_vec,reg_args=reg_args,clos=clos,free=free,res=res,bv=bv}::
		      resolve_args(assign_list_res,rest))
	end
	| CC_ls(LS.FUNCALL{opr,args,reg_vec,reg_args,clos,free,res,bv},rest) =
	let
	  val ({clos,args,free,res,reg_vec,reg_args},assign_list_args,assign_list_res) = 
	    CallConv.resolve_app LS.PHREG {clos=clos,free=free,args=args,reg_vec=reg_vec,reg_args=reg_args,res=res}
	in
	  resolve_res(assign_list_args,
		      LS.FUNCALL{opr=opr,args=args,reg_vec=reg_vec,reg_args=reg_args,clos=clos,free=free,res=res,bv=bv}::
		      resolve_args(assign_list_res,rest))
	end
	| CC_ls(LS.LETREGION{rhos,body},rest) = LS.LETREGION{rhos=rhos,body=CC_lss body}::rest
	| CC_ls(LS.SCOPE{pat,scope},rest) = LS.SCOPE{pat=pat,scope=CC_lss scope}::rest
	| CC_ls(LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv,bv),offset},rest) = 
	LS.HANDLE{default=CC_lss default,handl=(CC_lss handl,handl_lv),handl_return=([],handl_return_lv,bv),offset=offset}::rest
	| CC_ls(LS.HANDLE{default,handl,handl_return,offset},rest) = die "CC_ls: handl_return in HANDLE not empty"
	| CC_ls(LS.SWITCH_I sw,rest) = LS.SWITCH_I(CC_sw CC_lss sw)::rest
	| CC_ls(LS.SWITCH_S sw,rest) = LS.SWITCH_S(CC_sw CC_lss sw)::rest
	| CC_ls(LS.SWITCH_C sw,rest) = LS.SWITCH_C(CC_sw CC_lss sw)::rest
	| CC_ls(LS.SWITCH_E sw,rest) = LS.SWITCH_E(CC_sw CC_lss sw)::rest
	| CC_ls(LS.CCALL{name,args,rhos_for_result,res},rest) = 
	let
	  val ({args,rhos_for_result,res},assign_list_args,assign_list_res) = 
	    CallConv.resolve_ccall LS.PHREG {args=args,rhos_for_result=rhos_for_result,res=res}
	in
	  resolve_res(assign_list_args,
		      LS.CCALL{name=name,args=args,rhos_for_result=rhos_for_result,res=res}::
		      resolve_args(assign_list_res,rest))
	end
	| CC_ls (ls,rest) = ls::rest
	
      and CC_lss(lss) = List.foldr (fn (ls,acc) => CC_ls(ls,acc)) [] lss
  in
      fun CC_top_decl(LS.FUN(lab,cc,lss)) = 
	let
	  val (cc',args,res) = CallConv.resolve_cc(cc)
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
	  val (cc',args,res) = CallConv.resolve_cc(cc)
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
	   | LS.SWITCH_I sw => LS.SWITCH_I(ra_assign_sw ra_assign_lss sw)
	   | LS.SWITCH_S sw => LS.SWITCH_S(ra_assign_sw ra_assign_lss sw)
	   | LS.SWITCH_C sw => LS.SWITCH_C(ra_assign_sw ra_assign_lss sw)
	   | LS.SWITCH_E sw => LS.SWITCH_E(ra_assign_sw ra_assign_lss sw)
	   | LS.RESET_REGIONS a => LS.RESET_REGIONS a
	   | LS.PRIM a => LS.PRIM a
	   | LS.CCALL a => LS.CCALL a
	    
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
    fun procent(t:int,b:int) = 
      if b=0 then
	"(---)"
      else
	"("^ StringCvt.padLeft #" " 3 (Int.toString (Real.round((Real.fromInt t) / (Real.fromInt b) * 100.0))) ^ "%)"
    fun pp_stat() =
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
    fun reset_stat() =
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
    fun inc_initial() = no_of_nodes := !no_of_nodes+1
    fun inc_spills() = spills := !spills+1
    fun inc_assigned_colors() = assigned_colors := !assigned_colors+1
    fun inc_moves() = no_of_moves := !no_of_moves+1
    fun inc_coalesce() = coalesced_moves := !coalesced_moves+1
    fun inc_constrained() = constrained_moves := !constrained_moves+1
    fun inc_frozen() = frozen_moves := !frozen_moves+1
    fun inc_no_call() = no_call := !no_call+1
    fun inc_c_call() = c_call := !c_call+1
    fun inc_ml_call() = ml_call := !ml_call+1
  end

  fun key lv = Lvars.key lv

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

  fun merge_lrs(no_call,s2) = s2
    | merge_lrs(c_call,ml_call) = ml_call
    | merge_lrs(s1,_) = s1

  type count = int
  type key = int
  type node = {lv:lvar, degree: count ref, mv_related: count option ref,
	       worklist: worklist_enum ref, adjList: lvar list ref, (* for precolored nodes, adjList is empty  *)
	       alias: key option ref, color: lvar option ref,       (* the lvar in color represents a register *)
	       lrs: live_range_status ref, uses : count ref}

  fun key' (n : node) = key(#lv n)

  (* Precolored nodes *)
  val precolored : node list = 
    map (fn lv => {lv=lv,degree=ref 0, mv_related=ref NONE,
		   worklist=ref precolored_enum, adjList=ref nil,
		   alias = ref NONE, color=ref (SOME lv),
		   lrs = ref no_call, uses = ref 0})
    (BI.caller_save_phregs @ BI.callee_save_phregs)  
  fun reset_precolored() = 
    app (fn ({lv,degree, mv_related, worklist, adjList, alias, color, lrs, uses} : node) =>
	 (degree:=0; mv_related:=NONE; worklist:=precolored_enum;
	  adjList:=nil; alias:=NONE; color:=SOME lv; lrs:=no_call; uses:=0))
    precolored

  val allColors : lvarset = Lvarset.lvarsetof(map (#lv) precolored)
  val K = length precolored

  (* Work lists *)
  val initial : node list ref = ref []
  val simplifyWorklist : node list ref = ref []
  val freezeWorklist : node list ref = ref []
  val spillWorklist : node list ref = ref [] 
  val spilledNodes : node list ref = ref []
  val coalescedNodes : node list ref = ref []
  val coloredNodes : node list ref = ref []
  val selectStack : node list ref = ref []
    
  fun worklistsReset () =
    (initial := nil; simplifyWorklist := nil;
     freezeWorklist := nil; spillWorklist := nil;
     spilledNodes := nil; coalescedNodes := nil;
     coalescedNodes := nil; coloredNodes := nil;
     selectStack := nil)
    
  fun isEmpty nil = true
    | isEmpty _ = false
    
  local 
    fun norm (wle:worklist_enum) (wl:node list ref) : unit = 
      wl := List.filter (fn n => !(#worklist n) = wle) (!wl)
  in 
    fun norm_simplifyWorklist () = norm simplifyWorklist_enum simplifyWorklist
    fun norm_freezeWorklist() = norm freezeWorklist_enum freezeWorklist
    fun norm_spillWorklist() = norm spillWorklist_enum spillWorklist
    fun isEmpty_simplifyWorklist() =
      (norm_simplifyWorklist(); isEmpty(!simplifyWorklist))
    fun isEmpty_freezeWorklist() =
      (norm_freezeWorklist(); isEmpty(!freezeWorklist))
    fun isEmpty_spillWorklist() =
      (norm_spillWorklist(); isEmpty(!spillWorklist))
  end

  local fun add (wl:node list ref) c (n:node) = (#worklist n := c; wl := n :: !wl)
  in
    fun initialAdd n = add initial initial_enum n
    fun simplifyWorklistAdd n = add simplifyWorklist simplifyWorklist_enum n
    fun freezeWorklistAdd n = (add freezeWorklist freezeWorklist_enum n)
    fun spillWorklistAdd n = add spillWorklist spillWorklist_enum n
    fun spilledNodesAdd n = add spilledNodes spilledNodes_enum n
    fun coalescedNodesAdd n = add coalescedNodes coalescedNodes_enum n
    fun coloredNodesAdd n = add coloredNodes coloredNodes_enum n
  end

  datatype movelist_enum = coalescedMoves_enum | constrainedMoves_enum |
    frozenMoves_enum | worklistMoves_enum | activeMoves_enum
    
  type move = {lv1:lvar, lv2:lvar, movelist:movelist_enum ref} (* why is a move not an instruction? There may be several moves from lv1 into lv2; hmm, no because we use SML *)
    
  (* Move lists *)
  val coalescedMoves : move list ref = ref []
  val constrainedMoves : move list ref = ref []
  val frozenMoves : move list ref = ref []
  val worklistMoves : move list ref = ref []
  val activeMoves : move list ref = ref []
    
  fun movelistsReset() =
    (coalescedMoves := nil; constrainedMoves := nil;
     frozenMoves := nil; worklistMoves := nil; activeMoves := nil)
    
  local
    fun norm (mle:movelist_enum) (ml:move list ref) : unit= 
      ml := List.filter (fn m => !(#movelist m) = mle) (!ml)
  in 
    fun norm_worklistMoves () = norm worklistMoves_enum worklistMoves
    fun isEmpty_worklistMoves() =
      (norm_worklistMoves(); isEmpty(!worklistMoves))
  end

  local fun add (ml:move list ref) c (m:move) = (#movelist m := c; ml := m :: !ml)
  in
    fun coalescedMovesAdd m = add coalescedMoves coalescedMoves_enum m
    fun constrainedMovesAdd m = add constrainedMoves constrainedMoves_enum m
    fun frozenMovesAdd m = add frozenMoves frozenMoves_enum m
    fun worklistMovesAdd m = add worklistMoves worklistMoves_enum m
    fun activeMovesAdd m = add activeMoves activeMoves_enum m
  end

  (* nTable; table from lvar keys to nodes *)
  local
    val nTable : (int*node)list Array.array = Array.array (512,nil)
    fun hash i = Word.toInt(Word.andb(Word.fromInt i, 0w511))
  in
    fun nTableLookup i : node option = 
      let fun find [] = NONE
	    | find ((i',e)::is) = if i=i' then SOME e else find is 
      in find (Array.sub(nTable, hash i))
      end
    fun nTableAdd (i:int,n:node) : unit =
      let val h = hash i
	val l = Array.sub(nTable, h)
	fun add [] = [(i,n)]
	  | add ((p as (i',_)) :: ps) = if i'=i then (i,n)::ps else p :: add ps
      in Array.update(nTable,h,add l)
      end
    fun nTableReset () : unit = (Array.modify (fn _ => nil) nTable;
				 app (fn n => nTableAdd(key' n, n)) precolored)
  end

  (* moveList; table from lvar keys to moves *)
  local
    val mTable : (int*move list)list Array.array = Array.array (512,nil)
    fun hash i = Word.toInt(Word.andb(Word.fromInt i, 0w511))
  in
    fun moveListReset () : unit = Array.modify (fn _ => nil) mTable
    fun moveListLookup i : move list = 
      let fun find [] = nil
	    | find ((i',ms)::is) = if i=i' then ms else find is 
      in find (Array.sub(mTable, hash i))
      end
    fun moveListAdd (i:int,m:move) : unit =
      let val h = hash i
	val l = Array.sub(mTable, h)
	fun add [] = [(i,[m])]
	  | add ((p as (i',ms)) :: ps) = if i'=i then (i,m::ms)::ps else p :: add ps
      in Array.update(mTable,h,add l)
      end
  end

  local 
    val adjSet : (int*int)list Array.array = Array.array (1024,nil)
    fun hash (i1,i2) = Word.toInt(Word.andb(0w65599 * Word.fromInt i1 + Word.fromInt i2, 0w1023))
    fun order (i1:int,i2:int) = if i2 < i1 then (i2,i1) else (i1,i2)
    fun find (p:int*int) [] = false
      | find p ((p':int*int)::ps) = (#1 p' = #1 p andalso #2 p' = #2 p) orelse find p ps
  in 
    fun adjSetReset() : unit = Array.modify (fn _ => nil) adjSet
    fun adjSetMember (p:int*int) : bool = 
      let val p = order p
      in find p (Array.sub(adjSet, hash p))
      end
    fun adjSetAdd (p:int*int) : unit =
      let val p = order p
	val h = hash p
	val l = Array.sub(adjSet, h)
      in if find p l then () else Array.update(adjSet,h,p::l)
      end
  end

  fun raReset () = (worklistsReset(); movelistsReset(); nTableReset(); 
		    moveListReset(); adjSetReset(); reset_precolored())

  fun lvarset_atom a = Lvarset.lvarsetof(LS.get_var_atom(a,nil))

  fun Adjecent (n:node) : node list =
    foldl (fn (lv,acc) => case nTableLookup (key lv)
			    of SOME n => 
			      (case !(#worklist n)
				 of selectStack_enum => acc
				  | coalescedNodes_enum => acc
				  | _ => n::acc)
			     | NONE => die "Adjecent") nil (!(#adjList n))

  fun NodeMoves (n:node) : move list = 
    foldl (fn (m,acc) => case !(#movelist m)
			   of activeMoves_enum => m::acc   (* nodes that have never been potentially *)
			    | worklistMoves_enum => m::acc (* nodes that are already potentially *)
			    | _ => acc) nil (moveListLookup (key' n))

  fun MoveRelated (n:node) : bool = case NodeMoves n
				      of nil => false
				       | _ => true
    
  fun EnableMoves (nodes:node list) : unit =
    app (fn n => app (fn m => case !(#movelist m)
				of activeMoves_enum => worklistMovesAdd m
				 | _ => ()) (NodeMoves n)) nodes

  fun GetAlias (k : int) : node =
    case nTableLookup k
      of SOME n =>
	if !(#worklist n) = coalescedNodes_enum then 
	  case !(#alias n)
	    of SOME i => GetAlias i
	     | NONE => die "GetAlias.1"
	else n
       | NONE => die "GetAlias.2"

  fun pr_node ({lv,degree,mv_related,worklist = ref wl,adjList,alias = ref NONE,color = ref (SOME color_lv),lrs,uses}:node) = 
    "{key: " ^ Int.toString (key lv) ^ ",lv:" ^ Lvars.pr_lvar lv ^ ",alias:NONE,color:" ^ 
    Lvars.pr_lvar color_lv ^ ",wl:" ^ pr_worklist wl ^ "}"
    | pr_node {lv,degree,mv_related,worklist = ref wl,adjList,alias = ref (SOME a_id),color = ref (SOME color_lv),lrs,uses} = 
    "{key: " ^ Int.toString (key lv) ^ ",lv:" ^ Lvars.pr_lvar lv ^ ",alias:" ^ 
    Lvars.pr_lvar (#lv(GetAlias a_id)) ^ ",color:" ^ Lvars.pr_lvar color_lv ^ ",wl:" ^ pr_worklist wl ^ "}"
    | pr_node {lv,degree,mv_related,worklist = ref wl,adjList,alias = ref (SOME a_id),color = ref NONE,lrs,uses} = 
    "{key: " ^ Int.toString (key lv) ^ ",lv:" ^ Lvars.pr_lvar lv ^ ",alias:" ^ 
    Lvars.pr_lvar (#lv(GetAlias a_id)) ^ ",wl:" ^ pr_worklist wl ^ ",color:NONE}"
    | pr_node {lv,degree,mv_related,worklist = ref wl,adjList,alias = ref NONE,color = ref NONE,lrs,uses} =
    "{key: " ^ Int.toString (key lv) ^ ",lv:" ^ Lvars.pr_lvar lv ^ ",wl:" ^ pr_worklist wl ^ ",alias:NONE,color:NONE}"

  fun pr_precolored () = (print "\nPrecolored[";map (print o pr_node) precolored;print "]\n")

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
    fun Simplify() = (* invariant : simplifyWorklist is normalised and non-empty *)
      case !simplifyWorklist
	of n::_ => (push n; app DecrementDegree (Adjecent n))
	 | nil => die "Simplify"
  end

  fun AddEdge (u : lvar ,v : lvar) : unit =
    let
      val (u_key, v_key) = (key u, key v)
    in 
      if u_key = v_key orelse adjSetMember(u_key, v_key) then ()
       else 
	 (adjSetAdd(u_key,v_key); 
	  case nTableLookup u_key
	    of SOME u_node =>
	      (case nTableLookup v_key
		 of SOME v_node => 
		   (if !(#worklist u_node) <> precolored_enum then 
		      ((#adjList u_node) := v :: !(#adjList u_node);
		       (#degree u_node) := !(#degree u_node) + 1)
		    else ();
		      if !(#worklist v_node) <> precolored_enum then 
			((#adjList v_node) := u :: !(#adjList v_node);
			 (#degree v_node) := !(#degree v_node) + 1)
		      else ())
		  | NONE => die ("AddEdge.nTableLookup v_key: " ^ Lvars.pr_lvar v))
	     | NONE => die ("AddEdge.nTableLookup u_key: " ^ Lvars.pr_lvar u))
    end	    

  fun MakeWorklist() =
    let 
      fun do_n n =
	if !(#degree n) >= K then 
	  spillWorklistAdd n
	else 
	  if MoveRelated n then 
	    freezeWorklistAdd n
	  else 
	    simplifyWorklistAdd n
    in 
      app do_n (!initial)
    end

  fun AddWorkList (u : node) : unit =
    if !(#worklist u) <> precolored_enum andalso not(MoveRelated u) andalso !(#degree u) < K then
      simplifyWorklistAdd u
    else ()

  fun OK (t : node, r : node) : bool =
    !(#degree t) < K orelse !(#worklist t) = precolored_enum orelse adjSetMember(key' t, key' r)

  fun Conservative(nodes:lvarset) : bool =
    let val nodes = map (fn lv => case nTableLookup (key lv)
				    of SOME n => n
				     | NONE => die "Conservative") (Lvarset.members nodes)
    in (foldl (fn (n,k) => if !(#degree n) >= K then k+1 else k) 0 nodes) < K
    end

  fun Combine(u : node, v : node) : unit = (* v is never precolored *)
    (coalescedNodesAdd v;
     if !(#worklist u) <> precolored_enum then (* We only merge lrs for non precolored lvars. 19/03/1999, Niels *)
       #lrs u := merge_lrs(!(#lrs u),!(#lrs v))
     else
       ();
     #alias v := SOME(key' u);
     let val key_u = key' u
     in app (fn m => moveListAdd (key_u,m)) (moveListLookup (key' v))
     end;
     app (fn t => (AddEdge(#lv t,#lv u); DecrementDegree t)) (Adjecent v);
     if !(#degree u) >= K andalso !(#worklist u) = freezeWorklist_enum then
       spillWorklistAdd u
     else ())

  fun Coalesce() : unit = (* invariant : worklistMoves is normalised and non-empty *)
    case !worklistMoves
      of (m as {lv1,lv2,movelist}) :: _ =>
	let 
	  val x = GetAlias (key lv1)
	  val y = GetAlias (key lv2)
	  val (u,v) = if !(#worklist y) = precolored_enum then (y,x)
		      else (x,y)
	in if key' u = key' v then (coalescedMovesAdd m;
				    inc_coalesce();
				    AddWorkList u)
	   else if !(#worklist v) = precolored_enum orelse adjSetMember(key' u,key' v) then
	     (inc_constrained();
	      constrainedMovesAdd m;
	      AddWorkList u;
	      AddWorkList v)
	   else if     (!(#worklist u) = precolored_enum andalso 
	               (foldl (fn (t, acc) => acc andalso OK(t,u)) true (Adjecent v)))
	              orelse
		       (!(#worklist u) <> precolored_enum andalso
			Conservative(Lvarset.union(Lvarset.lvarsetof(map (#lv) (Adjecent u)),
						   Lvarset.lvarsetof(map (#lv) (Adjecent v))))) then
		   (coalescedMovesAdd m;
		    inc_coalesce();
		    Combine(u,v);
		    AddWorkList u)
           else activeMovesAdd m
	end
       | _ => die "Coalesce"

  fun FreezeMoves (u: node) : unit =
    let fun on_move (m as {lv1=x,lv2=y,movelist}) : unit =
          let val v = if key'(GetAlias (key y)) = key'(GetAlias(key' u)) then GetAlias(key x)
		      else GetAlias(key y)
	  in
	    inc_frozen();
	    frozenMovesAdd m;
	    if !(#worklist v) <> precolored_enum (*18/03/1999, Niels*) andalso isEmpty (NodeMoves v) andalso !(#degree v) < K then
	      simplifyWorklistAdd v
	    else ()
	  end
    in app on_move (NodeMoves u)
    end

  fun Freeze() : unit =  (* invariant : freezeWorklist is normalised and non-empty *)
    case !freezeWorklist
      of u :: _ => (simplifyWorklistAdd u; FreezeMoves u)
       | _ => die "Freeze"

  fun SelectSpill() : unit = (* invariant : spillWorklist is normalised and non-empty *)
    let
      fun lrs_factor(no_call) = 1.0
	| lrs_factor(c_call) = 1.2
	| lrs_factor(ml_call) = 1.5
      fun pri (n:node) = Real.fromInt(!(#uses n)) / Real.fromInt(!(#degree n))  (**lrs_factor(!(#lrs n))06/04/1999, Niels*)
      fun select_spill() = (* use lowest priority: uses/degree*lrs_factor *)
	case !spillWorklist of
	  m :: rest => #1(foldl (fn (n,(m,mpri)) => 
				 let val npri = pri n
				 in if mpri < npri then (m,mpri) else (n,npri) end) (m,pri m) rest)
	| _ => die "SelectSpill.select_spill"

      fun order_mv_related(n:node,m:node) = 	  
	if List.length(NodeMoves n) < List.length(NodeMoves m) then n else m
      fun select_spill2() = (* spill non-move related nodes *)
	case !spillWorklist of
	  m :: rest => foldl order_mv_related m rest
	| _ => die "SelectSpill.select_spill2"
      val m = select_spill()
    in
      (simplifyWorklistAdd m; FreezeMoves m)
    end

  fun AssignColors() : unit =
    let
      fun assign_color (n:node,pri1,pri2,pri3) =
	(case Lvarset.members pri1 of
	   nil => (case Lvarset.members pri2 of
		     nil => (case Lvarset.members pri3 of
			       nil => (spilledNodesAdd (n:node); inc_spills())
			     | c::_ => (coloredNodesAdd (n:node); #color (n:node) := SOME c; inc_assigned_colors()))
		   | c::_ => (coloredNodesAdd (n:node); #color (n:node) := SOME c; inc_assigned_colors()))
	 | c::_ => (coloredNodesAdd (n:node); #color (n:node) := SOME c; inc_assigned_colors()))

      fun find_color (n:node,notOkColors) =
	let
	  val (callee_save_ccall,callee_save_mlkit,caller_save_mlkit) =
	    (Lvarset.difference(BI.callee_save_ccall_phregset,notOkColors),
	     Lvarset.difference(BI.callee_save_phregset,notOkColors),
	     Lvarset.difference(BI.caller_save_phregset,notOkColors))
	in
	  if !(#lrs n) = ml_call then
	    (inc_ml_call(); assign_color(n,callee_save_mlkit,callee_save_ccall,caller_save_mlkit))
	  else
	    if !(#lrs n) = c_call then 
	      (inc_c_call(); assign_color(n,callee_save_ccall,callee_save_mlkit,caller_save_mlkit))
	    else
	      (inc_no_call(); assign_color(n,caller_save_mlkit,callee_save_ccall,callee_save_mlkit))
	end

      fun find_color_simple (n:node,notOkColors) =
	let
	  val okColors = Lvarset.difference(allColors,notOkColors) 
	in
	  case Lvarset.members okColors
	    of nil => (spilledNodesAdd (n:node); inc_spills())
	     | c::_ => (coloredNodesAdd (n:node); #color (n:node) := SOME c; inc_assigned_colors())
	end
		
      fun pop_loop (ns : node list) =
	case ns
	  of nil => app (fn n => if !(#worklist n) = coalescedNodes_enum then
			 (#color n := !(#color(GetAlias(key' n))); inc_assigned_colors())
				 else ()) (!coalescedNodes)
	   | n::ns =>
	    let 
	      val _ = if !(#worklist n) = coalescedNodes_enum then die "assigning color to coalesced node" else ()
	      val notOkColors = 
		foldl (fn (w:lvar,set) =>
		       let val n = GetAlias(key w)
		       in if (case !(#worklist n)
				of coloredNodes_enum => true
				 | precolored_enum => true
				 | _ => false) then
			 case !(#color(GetAlias(key w)))
			   of SOME c => Lvarset.add(set,c)
			    | NONE => die "pop_loop"
			  else set
		       end) Lvarset.empty (!(#adjList n))
	    in 
	      find_color(*_simple*)(n,notOkColors);
	      pop_loop ns
	    end
    in pop_loop (!selectStack)
    end

  fun MakeInitial lss =
    let 
      fun add_use lv = 
	let 
	  val i = key lv 
	in
	  case nTableLookup i
	    of SOME n => #uses n := !(#uses n) + 1
	     | NONE => die ("MakeInitial.add_use: " ^ Lvars.pr_lvar lv ^ " not in nTableLookup")
	end
      fun add lv = 
	let 
	  val i = key lv 
	in 
	  case nTableLookup i
	    of SOME n => () (* Multiple definition in switch *)
	     | NONE => let 
			 val n : node = {lv=lv,degree=ref 0, mv_related=ref NONE,
					 worklist=ref initial_enum, adjList=ref nil,
					 alias = ref NONE, color=ref NONE,
					 lrs = ref no_call, uses = ref 0}
		       in 
			 nTableAdd(i,n); initial := n :: !initial; inc_initial()
		       end
	end
      fun mk_sw mk (LS.SWITCH(a,sels,default)) =
	(app add_use (LS.get_var_atom(a,nil)); 
	 app (fn (_,lss) => app mk lss) sels; 
	 app mk default)
      fun mk ls =
	case ls
	  of LS.FLUSH _ => die "MakeInitial: FLUSH not inserted yet."
	   | LS.FETCH _ => die "MakeInitial: FETCH not inserted yet."
	   | LS.LETREGION{rhos,body} => app mk body
	   | LS.SCOPE{pat,scope} => app mk scope
	   | LS.HANDLE{default,handl=(handl_lss,handl_lv),handl_return=(handl_return_lss,handl_return_lv,bv),offset} => 
	    (app add (LS.get_var_atom (handl_lv,nil));
	     app add (LS.get_var_atom (handl_return_lv,nil));
	     app mk handl_lss;
	     app mk default;  
	     app mk handl_return_lss)
	   | LS.SWITCH_I sw => mk_sw mk sw
	   | LS.SWITCH_S sw => mk_sw mk sw
	   | LS.SWITCH_C sw => mk_sw mk sw
	   | LS.SWITCH_E sw => mk_sw mk sw
	   | _ => let 
		    val (def,use) = LS.def_use_lvar_ls ls
		  in 
		    app add def; 
		    app add_use use
		  end
    in app mk lss
    end

  fun Build lss =
    let 
      fun set_lrs_status(new_s,lv) = 
	case nTableLookup (key lv)
	  of SOME {lrs = (lrs as ref old_s),...} => lrs := merge_lrs(old_s,new_s)
	   | NONE => die "set_lrs_status - nTableLookup failed"
      fun lvarset_app f lvs = (Lvarset.mapset f lvs; ())
      fun def_use_var_ls ls =
	let val (def,use) = LS.def_use_var_ls ls
	in (Lvarset.lvarsetof def, Lvarset.lvarsetof use)
	end
      fun ig_sw (ig_lss, LS.SWITCH (a, sel, def), L) =
	let val Ls = map (fn (_, lss) => ig_lss(lss, L)) sel 
	    val L = foldl Lvarset.union (ig_lss(def,L)) Ls
	in Lvarset.union (L, lvarset_atom a)
	end
      fun do_non_tail_call (L, ls) =
	let 
	  val (def, use) = def_use_var_ls ls  (* def=flv(res) *)
	  val lvars_to_flush = Lvarset.difference(L,def)
	  val _ = 
	    case ls of
	      LS.CCALL _ => lvarset_app (fn lv => set_lrs_status(c_call,lv)) lvars_to_flush
	    | _ => lvarset_app (fn lv => set_lrs_status(ml_call,lv)) lvars_to_flush
	  val L = Lvarset.union(L,def)  (* We insert edges between def'ed variables *)
	  val _ = lvarset_app (fn d => lvarset_app (fn u => AddEdge(d,u)) L) def 
	  val L = Lvarset.union(use, lvars_to_flush(*Lvarset.difference(L,def)*))
	in L
	end
      fun do_tail_call (L, ls) =
	let val (def, use) = def_use_var_ls ls  (* def=flv(res) *)
	    val _ = lvarset_app (fn d => lvarset_app (fn u => AddEdge(d,u)) def) def (* We insert edges between def'ed variables *)
	    val L = Lvarset.lvarsetof(LS.use_var_ls ls)
	in L
	end
      fun do_record(L,ls) = (* We must insert edges between def and use! *)
	let
	  val (def,use) = def_use_var_ls ls
	  val L' = Lvarset.union(Lvarset.union(L,def),use)
	  val _ = lvarset_app (fn d => lvarset_app (fn l => AddEdge(l,d)) L') def
	  val L = Lvarset.union(use, Lvarset.difference(L,def))
	in
	  L
	end
      fun do_move(L,lv1,lv2) = (* lv1 <-- lv2 *)
	let 
	  val _ = inc_moves()
	  val move : move = {lv1=lv1, lv2=lv2, movelist=ref worklistMoves_enum}
	  val _ = (moveListAdd(key lv1, move); moveListAdd(key lv2, move))
	  val _ = worklistMovesAdd move 
	  val _ = lvarset_app (fn l => AddEdge(l,lv1)) (Lvarset.delete(L,lv2)) 
	  val L = Lvarset.add(Lvarset.delete(L,lv1),lv2) 
	in L
	end
      fun remove_finite_rhos([]) = []
	| remove_finite_rhos(((place,LS.WORDS i),offset)::rest) = remove_finite_rhos rest
	| remove_finite_rhos(rho::rest) = rho :: remove_finite_rhos rest
      fun ig_ls (ls, L) =
	case ls
	  of LS.FLUSH _ => die "ig_ls: FLUSH not inserted yet."
	   | LS.FETCH _ => die "ig_ls: FETCH not inserted yet."
	   | LS.FNJMP _ => do_tail_call(L,ls) 
	   | LS.FNCALL _ => do_non_tail_call(L,ls)
	   | LS.JMP _ => do_tail_call(L,ls) 
	   | LS.FUNCALL _ => do_non_tail_call(L,ls)
	   | LS.LETREGION{rhos,body} => (* Infinite letregions involve ccalls *)
	    let
	      val L' = ig_lss(body,L)
	    in
	      if List.null (remove_finite_rhos rhos) then
		lvarset_app (fn lv => set_lrs_status(c_call,lv)) L'
	      else
		();
	      L'
	    end
	   | LS.SCOPE{pat,scope} => ig_lss(scope, L)
	   (* File Thesis/handle.ps contains a drawing of liveness wrt. handle. 17/02/1999, Niels *)
	   | LS.HANDLE{default,handl=(handl,handl_lv),handl_return=([],handl_return_lv,bv),offset} =>
	   let
	     (* We must flush all caller save registers that are live *)
	     (* after the handle. We define handl_return_lv in the    *)
	     (* handle construct. 19/03/1999, Niels                   *)
	     val L' = ig_lss(handl, ig_lss(default, L))
	     val handl_return_lvar = one_in_list (LS.get_var_atom (handl_return_lv,nil))
	     val _ = lvarset_app (fn lv => set_lrs_status(ml_call,lv)) (Lvarset.delete(L',handl_return_lvar))
	     val _ = lvarset_app (fn l => AddEdge(l,handl_return_lvar)) L'  (* ME 1999-08-14 *)
	   in
	     L'
	   end
	   | LS.HANDLE{default,handl,handl_return,offset} => die "ra_ls: handl_return in HANDLE not empty"
	   | LS.SWITCH_I sw => ig_sw (ig_lss, sw, L)
	   | LS.SWITCH_S sw => ig_sw (ig_lss, sw, L)
	   | LS.SWITCH_C sw => ig_sw (ig_lss, sw, L)
	   | LS.SWITCH_E sw => ig_sw (ig_lss, sw, L)
	   | LS.CCALL _ => do_non_tail_call(L,ls)
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
	        val L = Lvarset.union(L, def)   (* to introduce edges between defs *)
		val _ = lvarset_app (fn d => lvarset_app (fn l => AddEdge(l,d)) L) def
		val L = Lvarset.union(use, Lvarset.difference(L,def))
	    in L
	    end

      and ig_lss ([], L) = L
	| ig_lss (ls::lss, L) = let val L = ig_lss(lss, L)
				in ig_ls(ls, L)
				end
    in ig_lss (lss, Lvarset.empty); ()
    end

  local
    structure EdgeSet =
      OrderSet(structure Order =
		 struct
		   type T = string * string
		   fun lt((s11,s12): T) (s21,s22) = (s11^s12) < (s21^s22)
		 end
	       structure PP =PP
	       structure Report = Report)
    val edge_set = ref EdgeSet.empty
    val move_set = ref EdgeSet.empty
    fun reset_set() = (edge_set := EdgeSet.empty; move_set := EdgeSet.empty)
    val beginGraph = "graph: {\n"
    val attrGraph = "title: \"Interference Graph\"\n" ^
	  "layoutalgorithm: dfs\n" ^
	  "splines: yes\n" ^
	  "finetuning: no\n" ^
	  "orientation: left_to_right\n" ^
	  "ignore_singles: yes\n" ^
	  "display_edge_labels: yes\n" ^
	  "classname 1:\"Graph\"\n"
    val endGraph = "}\n"
    fun pr_node (n : node) = Lvars.pr_lvar (#lv n)
    fun add_edge (s1,s2) =
      if (s1 < s2) then
	edge_set := EdgeSet.insert (s1,s2) (!edge_set)
      else
	edge_set := EdgeSet.insert (s2,s1) (!edge_set)
    fun add_move (s1,s2) =
      if (s1 < s2) then
	move_set := EdgeSet.insert (s1,s2) (!move_set)
      else
	move_set := EdgeSet.insert (s2,s1) (!move_set)
    fun pp_edge (s1,s2,linestyle) = 
      "edge: {arrowstyle: none linestyle: " ^ linestyle ^ " sourcename: \"" ^ s1 ^ "\" targetname: \"" ^ s2 ^ "\" }\n"
    fun export_nodes stream =
      let 
	fun do_n (n : node) = 
	  (TextIO.output(stream, "node: { title:\"" ^ pr_node n ^ "\" label:\"" ^ pr_node n ^ "\" }\n");
	   List.app (fn lv => add_edge(pr_node n, Lvars.pr_lvar lv)) (!(#adjList n));
	   List.app (fn (m:move) => add_move(Lvars.pr_lvar (#lv1 m),Lvars.pr_lvar (#lv2 m))) (NodeMoves n))
	fun export_edges() =
	  (EdgeSet.apply (fn (s1,s2) => TextIO.output(stream, pp_edge(s1,s2,"continuous"))) (!edge_set);
	   EdgeSet.apply (fn (s1,s2) => TextIO.output(stream, pp_edge(s1,s2, "dotted"))) (!move_set))
      in 
	(app do_n (!initial);
	 app do_n precolored;
	 export_edges())
      end
  in
    fun export_ig filename =
      if !export_ig_flag then
	let
	  val _ = reset_set()
	  val stream = TextIO.openOut filename
	in
	  (TextIO.output(stream, beginGraph ^ attrGraph);
	   export_nodes stream;
	   TextIO.output(stream, endGraph);
	   TextIO.closeOut stream;
	   reset_set())
	end
      else
	()
  end
    
  fun ra_body (fun_name,lss) =
    let fun repeat() =
      (if not(isEmpty_simplifyWorklist()) then Simplify()
	      else if not(isEmpty_worklistMoves()) then Coalesce()
		   else if not(isEmpty_freezeWorklist()) then Freeze()
			else if not(isEmpty_spillWorklist()) then SelectSpill()
			     else ();
			       if isEmpty_simplifyWorklist() andalso isEmpty_worklistMoves()
				 andalso isEmpty_freezeWorklist() andalso isEmpty_spillWorklist() then ()
			       else repeat())
	 
	fun assign(LS.V lv) = 
	  (case nTableLookup (Lvars.key lv)
	    of SOME n => 
	      (case !(#color n)
		 of SOME c => PHREG_STY (lv,c)
		  | NONE => STACK_STY lv)
	     | NONE => die "ra_body.assign: lvar not assigned a color")
	  | assign(LS.FV lv) = FV_STY lv 
	
	val _ = (raReset(); 
		 MakeInitial lss;
		 Build lss; 
		 export_ig ("/net/skuld/vol/topps/disk02/MLKIT-afterVersion1/niels/.trash/" ^ fun_name ^ ".vcg");
		 MakeWorklist(); 
		 repeat(); 
		 AssignColors())
	  
	val res = ra_assign assign lss
    in raReset(); res
    end
  
  fun ra_top_decl f =
    case CC_top_decl f 
      of LS.FUN(lab,cc,lss) => LS.FUN(lab,cc,ra_body (Labels.pr_label lab, lss))
       | LS.FN(lab,cc,lss) => LS.FN(lab,cc,ra_body (Labels.pr_label lab, lss))
	
  fun ra_prg funcs = 
    foldr (fn (func,acc) => ra_top_decl func :: acc) [] funcs
    
  (******************************************************)
  (* Funtion to invoke the register allocator of choice *)
  (******************************************************)
  fun ra_main {main_lab:label,
	       code=line_prg: (StoreTypeLI,unit,Atom) LinePrg,
	       imports:label list * label list,
	       exports:label list * label list} ra_prg =
    let
      val _ = chat "[Register allocation..."
      val _ = reset_stat()
      val line_prg_ra = ra_prg line_prg
      val _ = 
	if Flags.is_on "print_register_allocated_program" then
	  display("\nReport: AFTER REGISTER ALLOCATION", 
		  LS.layout_line_prg pr_sty (fn _ => "()") pr_atom false line_prg_ra)
	else
	  ()
      val _ = pp_stat();
      val _ = chat "]\n"
    in
      {main_lab=main_lab,code=line_prg_ra: (StoreType,unit,Atom) LinePrg,imports=imports,exports=exports}
    end

  fun ra_dummy code = ra_main code ra_dummy_prg
  fun ra code = ra_main code ra_prg

end;
