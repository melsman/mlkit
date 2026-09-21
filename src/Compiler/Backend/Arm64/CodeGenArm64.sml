(* Generate Target Code *)

functor CodeGenArm64(structure LineStmt: LINE_STMT
                     where type con = Con.con
                     where type excon = Excon.excon
                     where type lvar = Lvars.lvar
                     where type label = AddressLabels.label
                     where type place = Effect.effect
                     where type StringTree = PrettyPrint.StringTree
                     where type cc = CallConv.cc
                   structure SubstAndSimplify: SUBST_AND_SIMPLIFY
                    where type ('a,'b,'c) LinePrg = ('a,'b,'c) LineStmt.LinePrg
                     where type lvar = Lvars.lvar
                     where type place = Effect.effect
                     where type reg = InstsArm64.reg
                     where type label = AddressLabels.label)
    : CODE_GEN =
struct
  structure LS = LineStmt
  structure SS = SubstAndSimplify
  structure A = InstsArm64
  open CodeGenUtilArm64
  type label = AddressLabels.label
  type ('s,'o,'a) LinePrg = ('s,'o,'a) LS.LinePrg
  type offset = int
  type StoreTypeCO = SS.StoreTypeCO
  type AtySS = SS.Aty
  type AsmPrg = A.AsmPrg
  val emit = A.emit
  fun message f = print(f())
  val extra_gc_checks = Flags.add_bool_entry
    {long="extra_gc_checks",short=NONE,item=ref false,neg=false,
     menu=["Compiler","extra GC checks"],desc="Insert extra GC checks (not yet supported on ARM64)."}
  val alloc_protect_always = Flags.add_bool_entry
    {long="alloc_protect_always",short=NONE,item=ref false,neg=false,
     menu=["Compiler","always protect allocation"],desc="Always protect parallel allocation."}
  fun slot fsz off = 8*(fsz-off-1)
  fun read fsz aty dst =
    case aty of
      SS.PHREG_ATY (D _) => unsupported "floating-point operands"
    | SS.PHREG_ATY src => move(src,dst)
    | SS.STACK_ATY off => load(SP,slot fsz off,dst)
    | SS.INTEGER_ATY {value,precision=64} => constant(value,dst)
    | SS.WORD_ATY {value,precision=64} => constant(value,dst)
    | SS.UNIT_ATY => constant(1,dst)
    | _ => unsupported ("operand " ^ SS.pr_aty aty)
  fun write fsz dst src =
    case dst of SS.PHREG_ATY (D _) => unsupported "floating-point result"
              | SS.PHREG_ATY d => move(src,d)
              | SS.STACK_ATY off => store(src,SP,slot fsz off)
              | SS.UNIT_ATY => []
              | _ => unsupported ("result " ^ SS.pr_aty dst)
  fun localFresh () = A.LocalLab(AddressLabels.new_named "arm64")
  (* Stage all arguments before loading their target registers. This handles
   * cycles without destroying input registers and keeps SP aligned. *)
  fun arguments fsz args =
    let val n = length args
        val bytes = 16*((n+1) div 2)
        val () = if n <= 8 then () else unsupported "stack-passed call arguments"
        val save = List.concat(mapi (fn (i,a) =>
          read (fsz+bytes div 8) a (X 16) @ store(X 16,SP,8*i)) args)
        val restore = List.concat(List.tabulate(n,fn i=>load(SP,8*i,X i)))
    in stack(true,bytes) @ save @ restore @ stack(false,bytes) end
  fun results fsz res =
    if length res > 3 then unsupported "stack-passed results"
    else let val bytes = 16*((length res+1) div 2)
         in stack(true,bytes) @
            List.concat(mapi (fn (i,_)=>store(X i,SP,8*i)) res) @
            List.concat(mapi (fn (i,a)=>load(SP,8*i,X 16) @
                                  write (fsz+bytes div 8) a (X 16)) res) @
            stack(false,bytes)
         end
  fun primitive fsz {name,args,res} =
    let open PrimName
        fun binary opn = case (args,res) of
          ([a,b],[d]) => read fsz a (X 16) @ read fsz b (X 17) @
                        [ins opn ["x16","x16","x17"]] @ write fsz d (X 16)
        | _ => unsupported "primitive arity"
        fun compare cc = case (args,res) of
          ([a,b],[SS.FLOW_VAR_ATY(_,t,f)]) =>
            read fsz a (X 16) @ read fsz b (X 17) @
            [ins "cmp" ["x16","x17"],ins ("b." ^ cc) [pr_lab(LocalLab t)],
             ins "b" [pr_lab(LocalLab f)]]
        | _ => unsupported "non-flow comparison"
    in case name of
      Plus_word64ub => binary "add"
    | Minus_word64ub => binary "sub"
    | Mul_word64ub => binary "mul"
    | Andb_word64ub => binary "and"
    | Orb_word64ub => binary "orr"
    | Xorb_word64ub => binary "eor"
    | Equal_word64ub => compare "eq"
    | Less_word64ub => compare "lo"
    | Lesseq_word64ub => compare "ls"
    | Greater_word64ub => compare "hi"
    | Greatereq_word64ub => compare "hs"
    | Equal_int64ub => compare "eq"
    | Less_int64ub => compare "lt"
    | Lesseq_int64ub => compare "le"
    | Greater_int64ub => compare "gt"
    | Greatereq_int64ub => compare "ge"
    | Word64ub_to_int64ub => (case (args,res) of ([a],[d])=>read fsz a (X 16) @ write fsz d (X 16)
                             | _ => unsupported "conversion arity")
    | Int64ub_to_word64ub => (case (args,res) of ([a],[d])=>read fsz a (X 16) @ write fsz d (X 16)
                             | _ => unsupported "conversion arity")
    | _ => unsupported ("primitive " ^ PrimName.pp_prim name)
    end
  val dataLabels : label list ref = ref []
  fun dataLabel l = if List.exists (fn x=>AddressLabels.eq(x,l)) (!dataLabels) then ()
                    else dataLabels := l :: !dataLabels
  fun epilogue fsz = stack(false,8*fsz) @
       [ins "ldp" ["x29","x30","[sp]"],ins "add" ["sp","sp","#16"],ins "ret" []]
  fun stmts fsz statements = List.concat(map (stmt fsz) statements)
  and stmt fsz ls =
    case ls of
      LS.SCOPE {scope,...} => stmts fsz scope
    | LS.LETREGION {rhos=[],body} => stmts fsz body
    | LS.ASSIGN {pat,bind=LS.ATOM{aty}} => read fsz aty (X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.LOAD l} => address(DatLab l,X 16) @ load(X 16,0,X 16) @ write fsz pat (X 16)
    | LS.ASSIGN {pat,bind=LS.STORE(aty,l)} =>
        (dataLabel l; read fsz aty (X 16) @ address(DatLab l,X 17) @ store(X 16,X 17,0) @
         constant(1,X 16) @ write fsz pat (X 16))
    | LS.FLUSH (aty,off) => read fsz aty (X 16) @ store(X 16,SP,slot fsz off)
    | LS.FETCH (aty,off) => load(SP,slot fsz off,X 16) @ write fsz aty (X 16)
    | LS.PRIM p => primitive fsz p
    | LS.CCALL {name,args,rhos_for_result=[],res} =>
        if length res > 1 then unsupported "multiple C results"
        else arguments fsz args @ [ins "bl" [pr_lab(NameLab name)]] @ results fsz res
    | LS.FUNCALL {opr,args,reg_args=[],fargs=[],clos=NONE,res,...} =>
        arguments fsz args @ stack(true,16) @ [ins "bl" [pr_lab(MLFunLab opr)]] @ results fsz res
    | LS.JMP {opr,args,reg_args=[],fargs=[],clos=NONE,res,...} =>
        if length res > 3 then unsupported "tail-call stack results"
        else arguments fsz args @ stack(false,8*fsz) @
             [ins "ldp" ["x29","x30","[sp]"],ins "b" [pr_lab(MLFunLab opr)]]
    | LS.SWITCH_I {switch=LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[(v,yes)],no),...} =>
        if v=IntInf.fromInt BackendInfo.ml_true then flow fsz (t,f,yes,no)
        else flow fsz (f,t,yes,no)
    | LS.SWITCH_C (LS.SWITCH(SS.FLOW_VAR_ATY(_,t,f),[((c,_),yes)],no)) =>
        if Con.eq(c,Con.con_TRUE) then flow fsz (t,f,yes,no)
        else flow fsz (f,t,yes,no)
    | LS.SWITCH_W {switch,precision=64} => switchCode fsz switch
    | LS.SWITCH_I {switch,precision=64} => switchCode fsz switch
    | LS.RESET_REGIONS {regions_for_resetting=[],...} => []
    | _ => unsupported (LS.pr_line_stmt SS.pr_sty SS.pr_offset SS.pr_aty true ls)
  and flow fsz (t,f,yes,no) =
    let val done = localFresh()
    in [Label(LocalLab t)] @ stmts fsz yes @ [ins "b" [pr_lab done],Label(LocalLab f)] @
       stmts fsz no @ [Label done] end
  and switchCode fsz (LS.SWITCH(a,cases,default)) =
    let val done = localFresh()
        val branches = map (fn (v,body)=>(v,localFresh(),body)) cases
    in read fsz a (X 16) @
       List.concat(map (fn (v,l,_) => constant(v,X 17) @
         [ins "cmp" ["x16","x17"],ins "b.eq" [pr_lab l]]) branches) @
       stmts fsz default @ [ins "b" [pr_lab done]] @
       List.concat(map (fn (_,l,body)=>[Label l] @ stmts fsz body @ [ins "b" [pr_lab done]]) branches) @
       [Label done]
    end
  fun top (l,cc,body) =
    let val () = if CallConv.get_ccf_size cc=0 andalso CallConv.get_rcf_size cc=0 then ()
                 else unsupported "spilled function arguments/results"
        val fsz = CallConv.get_frame_size cc
    in function (MLFunLab l) @
       [ins "stp" ["x29","x30","[sp]"],ins "mov" ["x29","sp"]] @
       stack(true,8*fsz) @ stmts fsz body @ epilogue fsz
    end
  fun CG {main_lab,code,imports,exports,safe} =
    let val () = dataLabels := []
        val text = List.concat(map (fn LS.FUN x=>top x | LS.FN x=>top x) code)
        fun data l = [Directive ".data",Directive ".p2align 3",
                      Directive(".globl " ^ pr_lab(DatLab l)),Label(DatLab l),Directive ".quad 0"]
    in text @ List.concat(map data (!dataLabels)) end
  fun generate_link_code (labs,_) =
    let
      (* C-facing entry preserves all C callee-saved registers used by ML. *)
      val gprs = List.tabulate(12,fn i=>X(i+19))
      val fprs = List.tabulate(8,fn i=>D(i+8))
      val regs = gprs@fprs
      val save = List.concat(mapi(fn (i,r)=>store(r,SP,8*i)) regs)
      val restore = List.concat(mapi(fn (i,r)=>load(SP,8*i,r)) regs)
    in function(NameLab "main") @ stack(true,160) @ save @
       [ins "add" ["x29","sp","#80"]] @
       List.concat(map(fn l=>stack(true,16) @ [ins "bl" [pr_lab(MLFunLab l)]]) labs) @
       restore @ stack(false,160) @ constant(0,X 0) @ [ins "ret" []]
    end
  fun generate_repl_init_code () = unsupported "REPL initialization"
  fun generate_repl_link_code _ = unsupported "REPL linking"
end
