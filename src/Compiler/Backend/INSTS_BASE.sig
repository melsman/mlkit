(* Base signature for Insts-structure for particular architectures
   (e.g., ARM and X86_64).
 *)

signature INSTS_BASE = sig

  type label = AddressLabels.label

  datatype lab =
        DatLab of label      (* For data to propagate across program units *)
      | LocalLab of label    (* Local label inside a block *)
      | NameLab of string    (* For ml strings, jumps to runtime system,
			        jumps to millicode, code label, finish
			        label, etc. *)
      | MLFunLab of label    (* Labels on ML Functions *)

  val eq_lab    : lab * lab -> bool
  val pr_lab    : lab -> string

  val intToStr  : IntInf.int -> string
  val wordToStr : IntInf.int -> string
  val i2s       : int -> string

  datatype Offset = WORDS of int
                  | BYTES of int

  val isZeroOffset : Offset -> bool
  val offset_bytes : Offset -> string

  structure LabSet : sig type t
                         val insert : t -> lab -> unit
                         val empty  : unit -> t
                         val mem    : t -> lab -> bool
                     end

  val sysname  : unit -> string
  val release  : unit -> string option
  val isDarwin : unit -> bool

end
