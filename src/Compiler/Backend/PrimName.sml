structure PrimName = struct

datatype prim =
         (* flow primitives *)
         Equal_int31 | Equal_int32ub | Equal_int32b |
	 Equal_word31 | Equal_word32ub | Equal_word32b |
	 Less_int31 | Less_int32ub | Less_int32b |
	 Less_word31 | Less_word32ub | Less_word32b |
	 Lesseq_int31 | Lesseq_int32ub | Lesseq_int32b |
	 Lesseq_word31 | Lesseq_word32ub | Lesseq_word32b |
	 Greater_int31 | Greater_int32ub | Greater_int32b |
	 Greater_word31 | Greater_word32ub | Greater_word32b |
	 Greatereq_int31 | Greatereq_int32ub | Greatereq_int32b |
	 Greatereq_word31 | Greatereq_word32ub | Greatereq_word32b |

         (* other primitives *)
         Less_real | Lesseq_real | Greater_real | Greatereq_real |
         Less_f64 | Lesseq_f64 | Greater_f64 | Greatereq_f64 |
	 Plus_int31 | Plus_int32ub | Plus_int32b |
	 Plus_word31 | Plus_word32ub | Plus_word32b | Plus_real |
	 Minus_int31 | Minus_int32ub | Minus_int32b |
	 Minus_word31 | Minus_word32ub | Minus_word32b | Minus_real |
	 Mul_int31 | Mul_int32ub | Mul_int32b |
	 Mul_word31 | Mul_word32ub | Mul_word32b | Mul_real |
	 Div_real |
	 Neg_int31 | Neg_int32ub | Neg_int32b | Neg_real |
	 Abs_int31 | Abs_int32ub | Abs_int32b | Abs_real |
	 Andb_word31 | Andb_word32ub | Andb_word32b |
	 Orb_word31 | Orb_word32ub | Orb_word32b |
	 Xorb_word31 | Xorb_word32ub | Xorb_word32b |
	 Shift_left_word31 | Shift_left_word32ub | Shift_left_word32b |
	 Shift_right_signed_word31 |
	 Shift_right_signed_word32ub | Shift_right_signed_word32b |
	 Shift_right_unsigned_word31 |
	 Shift_right_unsigned_word32ub | Shift_right_unsigned_word32b |
	 Int31_to_int32b | Int31_to_int32ub | Int32b_to_int31 | Int32b_to_word32b | Int32ub_to_int31 |
	 Word31_to_word32b | Word31_to_word32ub | Word32b_to_word31 | Word32ub_to_word31 |
	 Word31_to_word32ub_X | Word31_to_word32b_X |
	 Word32b_to_int32b | Word32b_to_int32b_X | Word32ub_to_int32ub | Word31_to_int31 |
	 Word32b_to_int31 | Int32b_to_word31 | Word32b_to_int31_X |
	 Exn_ptr | Fresh_exname |
         Bytetable_sub | Bytetable_size | Bytetable_update |
	 Word_sub0 | Word_update0 | Table_size |
	 Is_null |
	 ServerGetCtx |
         Plus_f64 | Minus_f64 | Mul_f64 | Div_f64 | Max_f64 | Min_f64 |
         Real_to_f64 | F64_to_real |
         Sqrt_f64 | Neg_f64 | Abs_f64 | Int_to_f64 |
         Blockf64_update_real | Blockf64_sub_real | Blockf64_size | Blockf64_alloc |
         Blockf64_update_f64 | Blockf64_sub_f64

local
  structure M = OrderFinMap(struct type T = string
                                   fun lt (a: T) b = a < b
			    end)

  val flow_pairs =
	[("__equal_int31", Equal_int31), ("__equal_int32ub", Equal_int32ub), ("__equal_int32b", Equal_int32b),
	 ("__equal_word31", Equal_word31), ("__equal_word32ub", Equal_word32ub), ("__equal_word32b", Equal_word32b),
	 ("__less_int31", Less_int31), ("__less_int32ub", Less_int32ub), ("__less_int32b", Less_int32b),
	 ("__less_word31", Less_word31), ("__less_word32ub", Less_word32ub), ("__less_word32b", Less_word32b),
	 ("__lesseq_int31", Lesseq_int31), ("__lesseq_int32ub", Lesseq_int32ub), ("__lesseq_int32b", Lesseq_int32b),
	 ("__lesseq_word31", Lesseq_word31), ("__lesseq_word32ub", Lesseq_word32ub), ("__lesseq_word32b", Lesseq_word32b),
	 ("__greater_int31", Greater_int31), ("__greater_int32ub", Greater_int32ub), ("__greater_int32b", Greater_int32b),
	 ("__greater_word31", Greater_word31), ("__greater_word32ub", Greater_word32ub), ("__greater_word32b", Greater_word32b),
	 ("__greatereq_int31", Greatereq_int31), ("__greatereq_int32ub", Greatereq_int32ub), ("__greatereq_int32b", Greatereq_int32b),
	 ("__greatereq_word31", Greatereq_word31), ("__greatereq_word32ub", Greatereq_word32ub), ("__greatereq_word32b", Greatereq_word32b)
	 ]

  val M_flow = M.fromList flow_pairs

  val pairs =
        [("__less_real", Less_real), ("__lesseq_real", Lesseq_real), ("__greater_real", Greater_real), ("__greatereq_real", Greatereq_real),
         ("__less_f64", Less_f64), ("__lesseq_f64", Lesseq_f64), ("__greater_f64", Greater_f64), ("__greatereq_f64", Greatereq_f64),
	 ("__plus_int31", Plus_int31), ("__plus_int32ub", Plus_int32ub), ("__plus_int32b", Plus_int32b),
	 ("__plus_word31", Plus_word31), ("__plus_word32ub", Plus_word32ub), ("__plus_word32b", Plus_word32b), ("__plus_real", Plus_real),
	 ("__minus_int31", Minus_int31), ("__minus_int32ub", Minus_int32ub), ("__minus_int32b", Minus_int32b),
	 ("__minus_word31", Minus_word31), ("__minus_word32ub", Minus_word32ub), ("__minus_word32b", Minus_word32b), ("__minus_real", Minus_real),
	 ("__mul_int31", Mul_int31), ("__mul_int32ub", Mul_int32ub), ("__mul_int32b", Mul_int32b),
	 ("__mul_word31", Mul_word31), ("__mul_word32ub", Mul_word32ub), ("__mul_word32b", Mul_word32b), ("__mul_real", Mul_real),
	 ("__div_real", Div_real),
	 ("__neg_int31", Neg_int31), ("__neg_int32ub", Neg_int32ub), ("__neg_int32b", Neg_int32b), ("__neg_real", Neg_real),
	 ("__abs_int31", Abs_int31), ("__abs_int32ub", Abs_int32ub), ("__abs_int32b", Abs_int32b), ("__abs_real", Abs_real),
	 ("__andb_word31", Andb_word31), ("__andb_word32ub", Andb_word32ub), ("__andb_word32b", Andb_word32b),
	 ("__orb_word31", Orb_word31), ("__orb_word32ub", Orb_word32ub), ("__orb_word32b", Orb_word32b),
	 ("__xorb_word31", Xorb_word31), ("__xorb_word32ub", Xorb_word32ub), ("__xorb_word32b", Xorb_word32b),
	 ("__shift_left_word31", Shift_left_word31), ("__shift_left_word32ub", Shift_left_word32ub), ("__shift_left_word32b", Shift_left_word32b),
	 ("__shift_right_signed_word31", Shift_right_signed_word31),
	 ("__shift_right_signed_word32ub", Shift_right_signed_word32ub), ("__shift_right_signed_word32b", Shift_right_signed_word32b),
	 ("__shift_right_unsigned_word31", Shift_right_unsigned_word31),
	 ("__shift_right_unsigned_word32ub", Shift_right_unsigned_word32ub), ("__shift_right_unsigned_word32b", Shift_right_unsigned_word32b),
	 ("__int31_to_int32b", Int31_to_int32b), ("__int31_to_int32ub", Int31_to_int32ub), ("__int32b_to_int31", Int32b_to_int31), ("__int32b_to_word32b", Int32b_to_word32b), ("__int32ub_to_int31", Int32ub_to_int31),
	 ("__word31_to_word32b", Word31_to_word32b), ("__word31_to_word32ub", Word31_to_word32ub), ("__word32b_to_word31", Word32b_to_word31), ("__word32ub_to_word31", Word32ub_to_word31),
	 ("__word31_to_word32ub_X", Word31_to_word32ub_X), ("__word31_to_word32b_X", Word31_to_word32b_X),
	 ("__word32b_to_int32b", Word32b_to_int32b), ("__word32b_to_int32b_X", Word32b_to_int32b_X), ("__word32ub_to_int32ub", Word32ub_to_int32ub), ("__word31_to_int31", Word31_to_int31),
	 ("__word32b_to_int31", Word32b_to_int31), ("__int32b_to_word31", Int32b_to_word31), ("__word32b_to_int31_X", Word32b_to_int31_X),
	 ("__exn_ptr", Exn_ptr), ("__fresh_exname", Fresh_exname),
         ("__bytetable_sub", Bytetable_sub), ("__bytetable_size", Bytetable_size), ("__bytetable_update", Bytetable_update),
	 ("word_sub0", Word_sub0), ("word_update0", Word_update0), ("table_size", Table_size),
	 ("__is_null", Is_null),
	 ("__serverGetCtx", ServerGetCtx),
         ("__plus_f64", Plus_f64),
         ("__minus_f64", Minus_f64),
         ("__mul_f64", Mul_f64),
         ("__div_f64", Div_f64),
         ("__max_f64", Max_f64),
         ("__min_f64", Min_f64),
         ("__real_to_f64", Real_to_f64),
         ("__f64_to_real", F64_to_real),
         ("__sqrt_f64", Sqrt_f64),
         ("__neg_f64", Neg_f64),
         ("__abs_f64", Abs_f64),
         ("__int_to_f64", Int_to_f64),
         ("__blockf64_update_real", Blockf64_update_real),
         ("__blockf64_sub_real", Blockf64_sub_real),
         ("__blockf64_size", Blockf64_size),
         ("__blockf64_alloc", Blockf64_alloc),
         ("__blockf64_update_f64", Blockf64_update_f64),
         ("__blockf64_sub_f64", Blockf64_sub_f64)
]

  val M = M.fromList pairs
in
fun lookup_prim (name:string) : prim option =
    case M.lookup M name  of
        NONE => M.lookup M_flow name
      | res => res
fun lookup_flow_prim (name:string) : prim option =
    M.lookup M_flow name

fun is_flow_prim (p:prim) : bool =
    case p of
        Equal_int31 => true
      | Equal_int32ub => true
      | Equal_int32b => true
      | Equal_word31 => true
      | Equal_word32ub => true
      | Equal_word32b => true
      | Less_int31 => true
      | Less_int32ub => true
      | Less_int32b => true
      | Less_word31 => true
      | Less_word32ub => true
      | Less_word32b => true
      | Lesseq_int31 => true
      | Lesseq_int32ub => true
      | Lesseq_int32b => true
      | Lesseq_word31 => true
      | Lesseq_word32ub => true
      | Lesseq_word32b => true
      | Greater_int31 => true
      | Greater_int32ub => true
      | Greater_int32b => true
      | Greater_word31 => true
      | Greater_word32ub => true
      | Greater_word32b => true
      | Greatereq_int31 => true
      | Greatereq_int32ub => true
      | Greatereq_int32b => true
      | Greatereq_word31 => true
      | Greatereq_word32ub => true
      | Greatereq_word32b => true
      | Less_f64 => true
      | Lesseq_f64 => true
      | Greater_f64 => true
      | Greatereq_f64 => true
      | _ => false

fun pp_prim (p:prim) : string =
    case p of
        Equal_int31 => "Equal_int31"
      | Equal_int32ub => "Equal_int32ub"
      | Equal_int32b => "Equal_int32b"
      | Equal_word31 => "Equal_word31"
      | Equal_word32ub => "Equal_word32ub"
      | Equal_word32b => "Equal_word32b"
      | Less_int31 => "Less_int31"
      | Less_int32ub => "Less_int32ub"
      | Less_int32b => "Less_int32b"
      | Less_word31 => "Less_word31"
      | Less_word32ub => "Less_word32ub"
      | Less_word32b => "Less_word32b"
      | Lesseq_int31 => "Lesseq_int31"
      | Lesseq_int32ub => "Lesseq_int32ub"
      | Lesseq_int32b => "Lesseq_int32b"
      | Lesseq_word31 => "Lesseq_word31"
      | Lesseq_word32ub => "Lesseq_word32ub"
      | Lesseq_word32b => "Lesseq_word32b"
      | Greater_int31 => "Greater_int31"
      | Greater_int32ub => "Greater_int32ub"
      | Greater_int32b => "Greater_int32b"
      | Greater_word31 => "Greater_word31"
      | Greater_word32ub => "Greater_word32ub"
      | Greater_word32b => "Greater_word32b"
      | Greatereq_int31 => "Greatereq_int31"
      | Greatereq_int32ub => "Greatereq_int32ub"
      | Greatereq_int32b => "Greatereq_int32b"
      | Greatereq_word31 => "Greatereq_word31"
      | Greatereq_word32ub => "Greatereq_word32ub"
      | Greatereq_word32b => "Greatereq_word32b"
      | Less_real => "Less_real"
      | Lesseq_real => "Lesseq_real"
      | Greater_real => "Greater_real"
      | Greatereq_real => "Greatereq_real"
      | Less_f64 => "Less_f64"
      | Lesseq_f64 => "Lesseq_f64"
      | Greater_f64 => "Greater_f64"
      | Greatereq_f64 => "Greatereq_f64"
      | Plus_int31 => "Plus_int31"
      | Plus_int32ub => "Plus_int32ub"
      | Plus_int32b => "Plus_int32b"
      | Plus_word31 => "Plus_word31"
      | Plus_word32ub => "Plus_word32ub"
      | Plus_word32b => "Plus_word32b"
      | Plus_real => "Plus_real"
      | Minus_int31 => "Minus_int31"
      | Minus_int32ub => "Minus_int32ub"
      | Minus_int32b => "Minus_int32b"
      | Minus_word31 => "Minus_word31"
      | Minus_word32ub => "Minus_word32ub"
      | Minus_word32b => "Minus_word32b"
      | Minus_real => "Minus_real"
      | Mul_int31 => "Mul_int31"
      | Mul_int32ub => "Mul_int32ub"
      | Mul_int32b => "Mul_int32b"
      | Mul_word31 => "Mul_word31"
      | Mul_word32ub => "Mul_word32ub"
      | Mul_word32b => "Mul_word32b"
      | Mul_real => "Mul_real"
      | Div_real => "Div_real"
      | Neg_int31 => "Neg_int31"
      | Neg_int32ub => "Neg_int32ub"
      | Neg_int32b => "Neg_int32b"
      | Neg_real => "Neg_real"
      | Abs_int31 => "Abs_int31"
      | Abs_int32ub => "Abs_int32ub"
      | Abs_int32b => "Abs_int32b"
      | Abs_real => "Abs_real"
      | Andb_word31 => "Andb_word31"
      | Andb_word32ub => "Andb_word32ub"
      | Andb_word32b => "Andb_word32b"
      | Orb_word31 => "Orb_word31"
      | Orb_word32ub => "Orb_word32ub"
      | Orb_word32b => "Orb_word32b"
      | Xorb_word31 => "Xorb_word31"
      | Xorb_word32ub => "Xorb_word32ub"
      | Xorb_word32b => "Xorb_word32b"
      | Shift_left_word31 => "Shift_left_word31"
      | Shift_left_word32ub => "Shift_left_word32ub"
      | Shift_left_word32b => "Shift_left_word32b"
      | Shift_right_signed_word31 => "Shift_right_signed_word31"
      | Shift_right_signed_word32ub => "Shift_right_signed_word32ub"
      | Shift_right_signed_word32b => "Shift_right_signed_word32b"
      | Shift_right_unsigned_word31 => "Shift_right_unsigned_word31"
      | Shift_right_unsigned_word32ub => "Shift_right_unsigned_word32ub"
      | Shift_right_unsigned_word32b => "Shift_right_unsigned_word32b"
      | Int31_to_int32b => "Int31_to_int32b"
      | Int31_to_int32ub => "Int31_to_int32ub"
      | Int32b_to_int31 => "Int32b_to_int31"
      | Int32b_to_word32b => "Int32b_to_word32b"
      | Int32ub_to_int31 => "Int32ub_to_int31"
      | Word31_to_word32b => "Word31_to_word32b"
      | Word31_to_word32ub => "Word31_to_word32ub"
      | Word32b_to_word31 => "Word32b_to_word31"
      | Word32ub_to_word31 => "Word32ub_to_word31"
      | Word31_to_word32ub_X => "Word31_to_word32ub_X"
      | Word31_to_word32b_X => "Word31_to_word32b_X"
      | Word32b_to_int32b => "Word32b_to_int32b"
      | Word32b_to_int32b_X => "Word32b_to_int32b_X"
      | Word32ub_to_int32ub => "Word32ub_to_int32ub"
      | Word31_to_int31 => "Word31_to_int31"
      | Word32b_to_int31 => "Word32b_to_int31"
      | Int32b_to_word31 => "Int32b_to_word31"
      | Word32b_to_int31_X => "Word32b_to_int31_X"
      | Exn_ptr => "Exn_ptr"
      | Fresh_exname => "Fresh_exname"
      | Bytetable_sub => "Bytetable_sub"
      | Bytetable_size => "Bytetable_size"
      | Bytetable_update => "Bytetable_update"
      | Word_sub0 => "Word_sub0"
      | Word_update0 => "Word_update0"
      | Table_size => "Table_size"
      | Is_null => "Is_null"
      | ServerGetCtx => "ServerGetCtx"
      | Plus_f64 => "Plus_f64"
      | Minus_f64 => "Minus_f64"
      | Mul_f64 => "Mul_f64"
      | Div_f64 => "Div_f64"
      | Max_f64 => "Max_f64"
      | Min_f64 => "Min_f64"
      | Real_to_f64 => "Real_to_f64"
      | F64_to_real => "F64_to_real"
      | Sqrt_f64 => "Sqrt_f64"
      | Neg_f64 => "Neg_f64"
      | Abs_f64 => "Abs_f64"
      | Int_to_f64 => "Int_to_f64"
      | Blockf64_update_real => "Blockf64_update_real"
      | Blockf64_sub_real => "Blockf64_sub_real"
      | Blockf64_size => "Blockf64_size"
      | Blockf64_alloc => "Blockf64_alloc"
      | Blockf64_update_f64 => "Blockf64_update_f64"
      | Blockf64_sub_f64 => "Blockf64_sub_f64"

end

end
