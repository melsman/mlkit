(* Main entry point to the compiler. compileStrdec compiles a list of
 * structure declarations containing no functor applications to a
 * lambda program, together with the environment _of this declaration
 * only_. 
 *)

signature COMPILE_DEC =
  sig
    type strdec
    type CEnv
    type LambdaPgm

    val compileStrdecs: CEnv -> strdec list -> CEnv * LambdaPgm

  end
