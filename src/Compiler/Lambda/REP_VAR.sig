(* Layout variables are monomorphic: type instantiation retains their identity. *)
signature REP_VAR =
sig
  type repvar
  exception Span of int * int
  val fresh : int -> repvar
  (* Temporary access annotation; unify with a receiver before validation. *)
  val freshAccess : unit -> repvar
  val top : repvar
  val isTop : repvar -> bool
  val span : repvar -> int option
  val unify : repvar * repvar -> unit
  val same : repvar * repvar -> bool
  val checkSpan : repvar * int -> unit
  (* Canonical key: stable only while the solved graph is not unified. *)
  val key : repvar -> int option
  val layout : repvar -> string
  (* Each invocation creates an independent copy operation, preserving sharing
   * within the copied graph without retaining mutable links to the original. *)
  val copier : unit -> repvar -> repvar
  val pu : repvar Pickle.pu
end
