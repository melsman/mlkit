(* Exercise the emitted >= pair, including both Boolean and branch results. *)
local
  open InstsX64
  val target = AddressLabels.new_named "equality_probe"
  fun localLab s = LocalLab(AddressLabels.new_named s)
  fun optimiseBody code =
    case #top_decls (optimise {top_decls=[FUN(target,code)],init_code=[],static_data=[]}) of
        [FUN(_,body)] => body
      | _ => raise Fail "equality probe shape"
  fun count code = length(List.filter (fn ucomisd _ => true | _ => false) code)
  fun check (name,n,code) =
    if count(optimiseBody code) = n then () else raise Fail("X64 real equality: " ^ name)
  val no = localLab "eq_no"
  val yes = localLab "eq_yes"
  val join = localLab "eq_join"
  val x = R xmm0
  val y = R xmm1
  val branch = [ucomisd(y,x),jb no,ucomisd(x,y),jae yes,jmp(L no),
                lab yes,movq(I "17",R rax),ret,
                lab no,movq(I "23",R rax),ret]
  val boolean = [ucomisd(y,x),jb no,ucomisd(x,y),
                 movq(I "1",R rax),movq(I "3",R rcx),cmovaeq(R rcx,R rax),jmp(L join),
                 lab no,movq(I "1",R rax),lab join,ret]
  val () = check("branch",1,branch)
  val () = check("Boolean",1,boolean)
  val () = check("live flags",2,
    [ucomisd(y,x),jb no,ucomisd(x,y),jb no,cmovaeq(R rcx,R rax),ret,lab no,ret])
  val () = check("different operands",2,
    [ucomisd(y,x),jb no,ucomisd(x,R xmm2),jb no,ret,lab no,ret])
  val () = check("live Boolean flags",2,
    [ucomisd(y,x),jb no,ucomisd(x,y),movq(I "1",R rax),movq(I "3",R rcx),
     cmovaeq(R rcx,R rax),cmovaeq(R rcx,R rdx),ret,lab no,ret])
  val middle = localLab "eq_middle"
  val () = check("referenced middle label",2,
    leaq(LA middle,R rdx)::ucomisd(y,x)::jb no::lab middle::ucomisd(x,y)::
    jb no::ret::lab no::ret::[])
  (* Separate label sets keep the two exported test functions independent. *)
  fun function name code =
    let val l = NameLab name
    in dot_text::dot_globl(l,FUNC)::lab l::optimiseBody code
    end
  val output = case CommandLine.arguments() of [s] => s | _ => raise Fail "expected output path"
  val boolCode = function "equality_boolean" boolean
  val branchCode = function "equality_branch" branch
  (* Separate assembly files keep the shared local label names independent. *)
  val () = emit({top_decls=[],init_code=boolCode,static_data=[]},output ^ "-boolean.s")
  val () = emit({top_decls=[],init_code=branchCode,static_data=[]},output ^ "-branch.s")
in end
