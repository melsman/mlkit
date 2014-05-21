(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)
(* This code: (c) Peter Sestoft.                                            *)

signature WSeq =
sig

    datatype wseq = 
	Empty                               (* The empty sequence         *)
      | Nl                                  (* Newline                    *)
      | $ of string                         (* A string                   *)
      | $$ of string list                   (* A sequence of strings      *)
      | && of wseq * wseq;                  (* Concatenation of sequences *)

    (* Manipulating wseqs *)
    val prmap    : ('a -> wseq) -> 'a list -> wseq
    val prsep    : wseq -> ('a -> wseq) -> 'a list -> wseq
    val flatten  : wseq -> string
end

(*
   Efficiently concatenable strings (word sequences).
   
   [prmap f list] constructs a sequence as the concatenation of the
   elements of the list list under f.

   [prsep sep f list] constructs a sequence as the concatenation of
   the elements of the list list under f; elements are seperated by
   sep.

   [flatten seq] returns a string representation of a sequence seq.
*)
