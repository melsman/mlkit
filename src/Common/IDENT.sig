(* Identifiers - variables or constructors *)

signature IDENT =
  sig
    eqtype strid

    eqtype longid
    val pr_longid : longid -> string

    eqtype id
    val inventId: unit -> id
    val invent_named_id: string -> id
    and pr_id : id -> string

    val mk_Id: string -> id			(* NEW PARSER *)
    val mk_LongId: string list -> longid	(* NEW PARSER *)
    val inventLongId: unit -> longid		(* NEW PARSER *)
    val idToLongId: id -> longid

    val unqualified: longid -> bool
    val decompose: longid -> strid list * id
    val decompose0: longid -> id
    val implode_LongId : strid list * id -> longid

    val < : id * id -> bool

   (* Identifiers needed for derived form conversion. *)

    val id_NIL: id
    val id_CONS: id
    val id_TRUE: id
    val id_FALSE: id
    val id_REF: id
    val id_PRIM: id
    val id_EXPORT: id
    val id_IT: id

    val id_QUOTE : id
    val id_ANTIQUOTE : id

    val id_INTINF : id

    (* Identifiers needed for initial environment - because of overloading *)
    val id_ABS: id
    val id_NEG: id
    val id_DIV: id
    val id_MOD: id
    val id_PLUS: id
    val id_MINUS: id
    val id_MUL: id
    val id_LESS: id
    val id_GREATER: id
    val id_LESSEQ: id
    val id_GREATEREQ: id

    val resetRegions: id
    val forceResetting: id

    (*exception constructors in the initial basis which are not declared in
     the prelude:*)

    val id_Div : id
    val id_Match : id
    val id_Bind : id
    val id_Overflow : id
    val id_Interrupt : id

    (* Bogus identifier *)
    val bogus: longid

    val reset : unit -> unit   (* resets counter to initial counter. *)
    val pu : id Pickle.pu
  end;
