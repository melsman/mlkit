(* Main entry point to the core language compiler. compileDec compiles
   a declaration to a function of type LambdaExp->LambdaExp, together
   with the environment *of this declaration only*. Apply
   this function to the scope of the declaration to get the whole lambda
   expression. This suspension makes environment management quite easy:
   call compileDec to find out what environment the declaration builds,
   and *then* build a record of those lvars and build the final lambda. *)

(*$COMPILE_DEC*)
signature COMPILE_DEC =
  sig
    type dec
    type CEnv				(* Compiler env: ident -> lvar/prim. *)
    type LambdaExp
    type LambdaPgm

    val compileDec: CEnv -> dec -> (CEnv * (LambdaExp -> LambdaPgm))

    val reset: unit -> unit
  end;
