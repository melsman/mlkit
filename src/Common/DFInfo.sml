(* Derived form information *)

structure DFInfo: DF_INFO =
  struct
    type InfixBasis = InfixBasis.Basis
    datatype DFInfo = UNITEXP_df | TUPLE_df | CASE_df | IF_df | ORELSE_df
                    | FUN_df | VALIT_df | INFIX_df | INFIX_BASIS of InfixBasis

    type StringTree = PrettyPrint.StringTree
    fun string UNITEXP_df = "UNITEXP"
      | string TUPLE_df   = "TUPLE"
      | string CASE_df    = "CASE"
      | string IF_df      = "IF_df"
      | string ORELSE_df  = "ORELSE_df"
      | string FUN_df     = "FUN_df"
      | string VALIT_df   = "VALIT_df"
      | string INFIX_df   = "INFIX_df"
      | string (INFIX_BASIS _) = "INFIX_BASIS"
    val layout = PrettyPrint.LEAF o string
  end
