(*DFInfo is a part of the ParseInfo.  It gives derived form
 information.  See PARSE_INFO for an overview of the different
 kinds of info.*)

(* It also makes it possible to save an infix basis in a functor
 * binding node. This is to support reelaboration of functor bodies. 
 *)

(*$DF_INFO *)

signature DF_INFO =
  sig
    type InfixBasis
    datatype DFInfo = UNITEXP_df | TUPLE_df | CASE_df | IF_df | ORELSE_df
                    | FUN_df | VALIT_df | INFIX_df | INFIX_BASIS of InfixBasis
    type StringTree
    val string : DFInfo -> string
    val layout : DFInfo -> StringTree
  end;
