(* Elaborate a Core Language Declaration, dec *)

signature ELABDEC =
sig
  type Context and Env and Type and TyName

  type PreElabDec and PostElabDec
  type PreElabTy  and PostElabTy

  val elab_dec : Context * PreElabDec -> TyName list * Env  * PostElabDec
  and elab_ty  : Context * PreElabTy  -> Type option * PostElabTy

    (* elab_ty returns `NONE' if an error occurred when elborating the
     * type expression. The reason we do things this way is that
     * errors are dealt with in two different ways depending on the
     * construct the type expression is part of. *)

end;
