(* Elaborate a Core Language Declaration, dec *)

(*$ELABDEC*)

signature ELABDEC =
sig
  type Context and Env and Type and TyName

  type PreElabDec and PostElabDec
  type PreElabTy  and PostElabTy

  val elab_dec : Context * PreElabDec -> TyName list * Env  * PostElabDec
  and elab_ty  : Context * PreElabTy  -> Type * PostElabTy
end;
