(*DFInfo is a part of the ParseInfo.  It gives derived form
 information.  See PARSE_INFO for an overview of the different
 kinds of info.*)

(*$DF_INFO *)

signature DF_INFO =
  sig
    datatype DFInfo = UNITEXP_df | TUPLE_df | CASE_df | IF_df | ORELSE_df
                    | FUN_df | VALIT_df | INFIX_df
    type StringTree
    val string : DFInfo -> string
    val layout : DFInfo -> StringTree
  end;
