(* Darwin ARM64 scalar C ABI and the initial ML register contract.
 * No instruction emission or ML representation conversion happens here.
 * See doc/arm64-abi.md; aggregate/vector C types are deliberately absent.
 *)
structure AbiArm64 =
struct
  val frame = FrameLayout.arm64
  val wordBytes = 8
  val stackAlignment = 16
  val contextRegister = 28
  val exceptionRegister = 27
  val framePointer = 29
  val linkRegister = 30
  val reservedGPRs = [16,17,18,27,28,29,30]
  val temporaryGPRs = [16,17]
  val mlArgumentGPRs = [0,1,2,3,4,5,6,7]
  val mlResultGPRs = [0,1,2]
  val mlArgumentFPRs = [0,1,2,3,4,5,6,7]
  val allocatableGPRs = List.tabulate(16,fn i => i) @ List.tabulate(8,fn i => i+19)
  val allocatableFPRs = List.tabulate(28,fn i => i)
  val spillFPRs = [28,29]
  val temporaryFPRs = [30,31]
  val cCalleeSaveGPRs = List.tabulate(10,fn i => i+19)
  val cCalleeSaveFPRs = List.tabulate(8,fn i => i+8) (* low 64 bits only *)
  val gcIntegerSlots = 32   (* bit i maps to word 31-i; see the GC contract *)
  val gcFloatSlots = 8      (* d0..d7; other live f64s are flushed *)
  val gcMetadataWords = 4   (* region/FP count, result count, arg count, arg base *)

  datatype scalar = I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Ptr | F32 | F64
  datatype location = GPR of int | FPR of int | Stack of {offset:int, bytes:int}
  datatype extension = None | SignTo32 | ZeroTo32
  type argument = {source:scalar, passed:scalar, location:location, extension:extension}

  fun bytes t = case t of I8 => 1 | U8 => 1 | I16 => 2 | U16 => 2
                       | I32 => 4 | U32 => 4 | F32 => 4 | _ => 8
  fun floating t = t = F32 orelse t = F64
  fun promotion t = case t of I8 => I32 | U8 => I32 | I16 => I32 | U16 => I32
                           | F32 => F64 | _ => t
  fun extension t = case t of I8 => SignTo32 | I16 => SignTo32
                           | U8 => ZeroTo32 | U16 => ZeroTo32 | _ => None
  fun align (n,a) = n + (a - n mod a) mod a

  (* Independent integer/FP banks for named arguments. Darwin stack arguments
   * occupy their natural sizes. Unnamed arguments are promoted and placed in
   * eight-byte stack slots, even if argument registers remain available.
   *)
  fun arguments {fixed:scalar list, variadic:scalar list} =
    let
      fun place ([],g,f,sp,acc) = (rev acc,sp)
        | place (t::ts,g,f,sp,acc) =
          let val (loc,g',f',sp') =
                if floating t andalso f < 8 then (FPR f,g,f+1,sp)
                else if not(floating t) andalso g < 8 then (GPR g,g+1,f,sp)
                else
                  let val off = align(sp,bytes t)
                  in (Stack{offset = off,bytes = bytes t},g,f,off+bytes t)
                  end
              val arg = {source = t,passed = t,location = loc,
                         extension = case loc of GPR _ => extension t | _ => None}
          in place(ts,g',f',sp',arg::acc)
          end
      val (named,sp) = place(fixed,0,0,0,[])
      fun varargs ([],sp,acc) = (rev acc,sp)
        | varargs (t::ts,sp,acc) =
          let val passed = promotion t
              val off = align(sp,8)
              val arg = {source = t,passed = passed,
                         location = Stack{offset = off,bytes = 8},extension = None}
          in varargs(ts,off+8,arg::acc)
          end
      val (unnamed,finish) = varargs(variadic,sp,[])
    in {arguments = named@unnamed,stackBytes = align(finish,16)}
    end

  (* The same locations describe outgoing C calls and incoming callbacks. *)
  fun result NONE = NONE
    | result (SOME t) = SOME (if floating t then FPR 0 else GPR 0)
end
