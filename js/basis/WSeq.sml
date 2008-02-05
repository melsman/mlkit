(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)
(* This code: (c) Peter Sestoft.                                            *)

(* EFFICIENTLY CONCATENABLE STRINGS
 * (WORD SEQUENCES)
 *
 * Author: Peter Sestoft
 *)

structure WSeq :> WSeq =
struct

    datatype wseq = 
	Empty                               (* The empty sequence         *)
      | Nl                                  (* Newline                    *)
      | $ of string                         (* A string                   *)
      | $$ of string list                   (* A sequence of strings      *)
      | && of wseq * wseq;                  (* Concatenation of sequences *)

    infix &&

    fun prmap f []       = Empty 
      | prmap f (x1::xr) = 
	let fun loop y1 []       = f y1
	      | loop y1 (y2::yr) = f y1 && loop y2 yr 
	in loop x1 xr end

    fun prsep sep f []       = Empty 
      | prsep sep f (x1::xr) = 
	let fun loop y1 []       = f y1
	      | loop y1 (y2::yr) = f y1 && sep && loop y2 yr 
	in loop x1 xr end

    fun flatten Empty acc      = acc
      | flatten Nl    acc      = "\n" :: acc
      | flatten ($ s) acc      = s :: acc
      | flatten ($$ ss) acc    = List.@(ss, acc)
      | flatten (s1 && s2) acc = flatten s1 (flatten s2 acc)
    val flatten = fn seq => String.concat(flatten seq [])
	
end (* structure WSeq *)
