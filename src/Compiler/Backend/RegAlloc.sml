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

  datatype StoreType =
    STACK_STY of lvar
  | PHREG_STY of lvar * lvar
    
  fun pr_sty(STACK_STY lv) = Lvars.pr_lvar lv ^ ":stack"
    | pr_sty(PHREG_STY(lv,phreg)) = Lvars.pr_lvar lv ^ ":" ^ LS.pr_phreg phreg

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

  (****************************************************************)
  (* Add Dynamic Flags                                            *)
  (****************************************************************)

  val _ = List.app (fn (x,y,r) => Flags.add_flag_to_menu (["Printing of intermediate forms"],x,y,r))
    [("print_register_allocated_program", "print register allocated program (LineStmt)", ref false)]


  (*************************************************)
  (* Make Call Conventions Explicit at Call Points *)
  (*************************************************)
  local
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
	     LS.SCOPE{pat=map #1 args,scope=resolve_args(args',body_lss)}
	  val body_res =
	    LS.SCOPE{pat=map #1 res,scope=body_args::resolve_res(res',[])}
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
	    LS.SCOPE{pat=map #1 args,scope=resolve_args(args',body_lss)}
	  val body_res =
	     LS.SCOPE{pat=map #1 res,scope=body_args::resolve_res(res',[])}
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

  fun ra_assign (assign : lvar -> StoreType) lss =
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
    let fun ra_assign_func assign func =
          case func 
	    of LS.FUN(lab,cc,lss) => LS.FUN(lab,cc,ra_assign assign lss)
	     | LS.FN(lab,cc,lss) => LS.FN(lab,cc,ra_assign assign lss)
    in foldr (fn (func,acc) => ra_assign_func STACK_STY (CC_top_decl func) :: acc) [] funcs
    end

  (* ----------------------------------------
   *  Register allocation with coalescing
   * ---------------------------------------- *)

  fun key lv = Lvars.key lv

  datatype worklist_enum =
    precolored_enum | initial_enum | simplifyWorklist_enum | freezeWorklist_enum |
    spillWorklist_enum | spilledNodes_enum | coalescedNodes_enum | coloredNodes_enum |
    selectStack_enum
    
  type count = int
  type key = int
  type node = {lv:lvar, degree: count ref, mv_related: count option ref,
	       worklist: worklist_enum ref, adjList: lvar list ref, (* for precolored nodes, adjList is empty *)
	       alias: key option ref, color: lvar option ref}    (* the lvar in color represents a register *)

  fun key' (n : node) = key(#lv n)
        
  (* Precolored nodes *)
  val precolored : node list = 
    map (fn lv => {lv=lv,degree=ref 0, mv_related=ref NONE,
		   worklist=ref precolored_enum, adjList=ref nil,
		   alias = ref NONE, color=ref (SOME lv)})
    (BI.caller_save_phregs @ BI.callee_save_phregs)
  fun reset_precolored() = 
    app (fn ({lv,degree, mv_related, worklist, adjList, alias, color} : node) =>
	 (degree:=0; mv_related:=NONE; worklist:=precolored_enum;
	  adjList:=nil; alias:=NONE; color:=SOME lv))
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
    fun freezeWorklistAdd n = add freezeWorklist freezeWorklist_enum n
    fun spillWorklistAdd n = add spillWorklist spillWorklist_enum n
    fun spilledNodesAdd n = add spilledNodes spilledNodes_enum n
    fun coalescedNodesAdd n = add coalescedNodes coalescedNodes_enum n
    fun coloredNodesAdd n = add coloredNodes coloredNodes_enum n
  end

  datatype movelist_enum = coalescedMoves_enum | constrainedMoves_enum |
    frozenMoves_enum | worklistMoves_enum | activeMoves_enum
    
  type move = {lv1:lvar, lv2:lvar, movelist:movelist_enum ref}
    
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
			   of activeMoves_enum => m::acc
			    | worklistMoves_enum => m::acc
			    | _ => acc) nil (moveListLookup (key' n))

  fun MoveRelated (n:node) : bool = case NodeMoves n
				      of nil => false
				       | _ => true
    
  fun EnableMoves (nodes:node list) : unit =
    app (fn n => app (fn m => case !(#movelist m)
				of activeMoves_enum => worklistMovesAdd m
				 | _ => ()) (NodeMoves n)) nodes

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
    let val (u_key, v_key) = (key u, key v)
    in if u_key = v_key orelse adjSetMember(u_key, v_key) then ()
       else case nTableLookup u_key
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
	       | NONE => die ("AddEdge.nTableLookup u_key: " ^ Lvars.pr_lvar u)
    end	    

  fun MakeWorklist() =
    let fun do_n n =
          if !(#degree n) >= K then spillWorklistAdd n
	  else if MoveRelated n then freezeWorklistAdd n
	       else simplifyWorklistAdd n
    in app do_n (!initial)
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

  fun GetAlias (k : int) : node =
    case nTableLookup k
      of SOME n =>
	if !(#worklist n) = coalescedNodes_enum then 
	  case !(#alias n)
	    of SOME i => GetAlias i
	     | NONE => die "GetAlias.1"
	else n
       | NONE => die "GetAlias.2"

  fun Combine(u : node, v : node) : unit =
    (coalescedNodesAdd v;
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
	let val x = GetAlias (key lv1)
	    val y = GetAlias (key lv2)
	    val (u,v) = if !(#worklist y) = precolored_enum then (y,x)
			else (x,y)
	in if key' u = key' v then (coalescedMovesAdd m;
				    AddWorkList u)
	   else if !(#worklist v) = precolored_enum orelse adjSetMember(key' u,key' v) then
	     (constrainedMovesAdd m;
	      AddWorkList u;
	      AddWorkList v)
	   else if     (!(#worklist u) = precolored_enum andalso 
	               (foldl (fn (t, acc) => acc andalso OK(t,u)) true (Adjecent v)))
	              orelse
		       (!(#worklist u) <> precolored_enum andalso
			Conservative(Lvarset.union(Lvarset.lvarsetof(map (#lv) (Adjecent u)),
						   Lvarset.lvarsetof(map (#lv) (Adjecent v))))) then
		   (coalescedMovesAdd m;
		    Combine(u,v);
		    AddWorkList u)
           else activeMovesAdd m
	end
       | _ => die "Coalesce"

  fun FreezeMoves (u: node) : unit =
    let fun on_move (m as {lv1=x,lv2=y,movelist}) : unit =
          let val v = if key'(GetAlias (key y)) = key'(GetAlias(key' u)) then GetAlias(key x)
		      else GetAlias(key y)
	  in frozenMovesAdd m;
	    if isEmpty (NodeMoves v) andalso !(#degree v) < K then
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
    case !spillWorklist
      of m :: _ => (simplifyWorklistAdd m; FreezeMoves m)    (* here is my favorite heuristic! *)
       | _ => die "SelectSpill"

  fun AssignColors() : unit =
    let fun pop_loop (ns : node list) =
          case ns
	    of nil => app (fn n => if !(#worklist n) = coalescedNodes_enum then
			              #color n := !(#color(GetAlias(key' n)))
				   else ()) (!coalescedNodes)
	     | n::ns =>
	      let val notOkColors = 
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
		  val okColors = Lvarset.difference(allColors,notOkColors)
	      in case Lvarset.members okColors
		   of nil => spilledNodesAdd (n:node)
		    | c::_ => (coloredNodesAdd (n:node); #color (n:node) := SOME c)

		     ; pop_loop ns
	      end
    in pop_loop (!selectStack)
    end

  fun MakeInitial lss =
    let 
      fun add lv = 
	let val i = key lv
	in case nTableLookup i
	     of SOME n => ()
	      | NONE => let val n : node = {lv=lv,degree=ref 0, mv_related=ref NONE,
					    worklist=ref initial_enum, adjList=ref nil,
					    alias = ref NONE, color=ref NONE}
			in nTableAdd(i,n); initial := n :: !initial
			end
	end
      fun mk_sw mk (LS.SWITCH(a,sels,default)) =
	(app add (LS.get_var_atom(a,nil)); 
	 app (fn (_,lss) => app mk lss) sels; 
	 app mk default)
      fun mk ls =
	case ls
	  of LS.FLUSH _ => die "MakeInitial: FLUSH not inserted yet."
	   | LS.FETCH _ => die "MakeInitial: FETCH not inserted yet."
	   | LS.LETREGION{rhos,body} => app mk body
	   | LS.SCOPE{pat,scope} => app mk scope
	   | LS.HANDLE{default,handl=(handl_lss,handl_lv),handl_return=(handl_return_lss,handl_return_lv,bv),offset} => 
	    (app mk default; app mk handl_lss; app mk handl_return_lss; 
	     app add (LS.get_var_atom (handl_lv,nil)); 
	     app add (LS.get_var_atom (handl_return_lv,nil)))
	   | LS.SWITCH_I sw => mk_sw mk sw
	   | LS.SWITCH_S sw => mk_sw mk sw
	   | LS.SWITCH_C sw => mk_sw mk sw
	   | LS.SWITCH_E sw => mk_sw mk sw
	   | _ => let val (def,use) = LS.def_use_lvar_ls ls
		  in app add def; app add use
		  end
    in app mk lss
    end

  fun Build lss =
    let 
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
	let val (def, use) = def_use_var_ls ls  (* def=flv(res) *)
	    val def = Lvarset.union(def,BI.caller_save_phregset)
	    val _ = lvarset_app (fn d => lvarset_app (fn u => AddEdge(d,u)) L) def
	    val L = Lvarset.union(use, Lvarset.difference(L,def))
	in L
	end
      fun ig_ls (ls, L) =
	case ls
	  of LS.FLUSH _ => die "ig_ls: FLUSH not inserted yet."
	   | LS.FETCH _ => die "ig_ls: FETCH not inserted yet."
	   | LS.FNJMP _ => Lvarset.lvarsetof(LS.use_var_ls ls)
	   | LS.FNCALL _ => do_non_tail_call(L,ls)
	   | LS.JMP _ => Lvarset.lvarsetof(LS.use_var_ls ls)
	   | LS.FUNCALL _ => do_non_tail_call(L,ls)
	   | LS.LETREGION{rhos,body} => ig_lss(body, L)
	   | LS.SCOPE{pat,scope} => ig_lss(scope, L)
	   | LS.HANDLE{default,handl,handl_return,offset} => die "ig_ls.HANDLE"
	   (* a handle defines all caller save registers *) 
	   | LS.SWITCH_I sw => ig_sw (ig_lss, sw, L)
	   | LS.SWITCH_S sw => ig_sw (ig_lss, sw, L)
	   | LS.SWITCH_C sw => ig_sw (ig_lss, sw, L)
	   | LS.SWITCH_E sw => ig_sw (ig_lss, sw, L)
	   | LS.CCALL _ => do_non_tail_call(L,ls)
	   | LS.ASSIGN {pat=LS.VAR lv1, bind=LS.ATOM(LS.VAR lv2)} =>   (* move instruction *)
	    let val move : move = {lv1=lv1, lv2=lv2, movelist=ref worklistMoves_enum}
	        val _ = (moveListAdd(key lv1, move); moveListAdd(key lv2, move))
		val _ = lvarset_app (fn l => AddEdge(l,lv1)) L
		val L = Lvarset.add(Lvarset.delete(L,lv1),lv2) 
	    in L
	    end
	   | _ => (* general *)
	    let val (def, use) = def_use_var_ls ls
	        val L = Lvarset.union(L, def)         (* to introduce edges between defs *)
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
    
  fun ra_body lss =
    let fun repeat() =
      (if not(isEmpty_simplifyWorklist()) then (chat "Simplify.."; Simplify())
	      else if not(isEmpty_worklistMoves()) then (chat "Coalesce.."; Coalesce())
		   else if not(isEmpty_freezeWorklist()) then (chat "Freeze.."; Freeze())
			else if not(isEmpty_spillWorklist()) then (chat "SelectSpill.."; SelectSpill())
			     else ();
			       if isEmpty_simplifyWorklist() andalso isEmpty_worklistMoves()
				 andalso isEmpty_freezeWorklist() andalso isEmpty_spillWorklist() then ()
			       else repeat())
	 
	fun assign lv = 
	  let val n = case nTableLookup (Lvars.key lv)
			of SOME n => n
			 | NONE => die "ra_body.assign"
	  in case !(#color n)
	       of SOME c => PHREG_STY (lv,c)
		| NONE => STACK_STY lv
	  end
	
	val _ = (raReset(); 
		 chat "MakeInitial..";
		 MakeInitial lss;
		 chat "Build..";
		 Build lss; 
		 chat "MakeWorklist..";		 
		 MakeWorklist(); 
		 repeat(); 
		 chat "AssignColors..";
		 AssignColors();
		 chat "ra_assign..")
	  
	val res = ra_assign assign lss
    in raReset(); res
    end
  
  fun ra_top_decl f =
    case CC_top_decl f 
      of LS.FUN(lab,cc,lss) => LS.FUN(lab,cc,ra_body lss)
       | LS.FN(lab,cc,lss) => LS.FN(lab,cc,ra_body lss)
	
  fun ra_prg funcs = 
    foldr (fn (func,acc) => ra_top_decl func :: acc) [] funcs
    
    
  (******************************************************)
  (* Funtion to invoke the register allocator of choice *)
  (******************************************************)
  fun ra_main {main_lab:label,
	       code=line_prg: (lvar,unit,Atom) LinePrg,
	       imports:label list * label list,
	       exports:label list * label list} ra_prg =
    let
      val _ = chat "[Register allocation..."
      val line_prg_ra = ra_prg line_prg
      val _ = 
	if Flags.is_on "print_register_allocated_program" then
	  display("\nReport: AFTER REGISTER ALLOCATION", 
		  LS.layout_line_prg pr_sty (fn _ => "()") pr_atom false line_prg_ra)
	else
	  ()
      val _ = chat "]\n"
    in
      {main_lab=main_lab,code=line_prg_ra: (StoreType,unit,Atom) LinePrg,imports=imports,exports=exports}
    end

  fun ra_dummy code = ra_main code ra_dummy_prg
  fun ra code = ra_main code ra_prg

end;
