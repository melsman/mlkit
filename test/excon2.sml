(* Matching an exception constructor at a path already described by a Pos
 * description for another exception constructor.  The excon rules of
 * staticmatch answer Maybe there (two excons may be the same although their
 * longids differ), and the argument descriptions recorded in that Pos belong
 * to the other constructor -- they say nothing about this one and need not
 * even have its arity.  Used to make the pattern match compiler die with
 * "Impossible: CompileDec.succeed". *)

exception A of int
exception B
exception C of string * int
exception A' = A                    (* an alias: the same excon, other longid *)
exception Io of {name: string, cause: exn}

fun f x = x () handle A 1 => "A1"
                    | B => "B"
                    | A n => "A" ^ Int.toString n
                    | C ("x", _) => "Cx"
                    | C (s, n) => "C" ^ s ^ Int.toString n
                    | _ => "other"

fun g x = x () handle A 1 => "A1"
                    | A' n => "A'" ^ Int.toString n
                    | _ => "other"

fun h x = x () handle Io {cause = B, ...} => "Io-B"
                    | B => "B"
                    | Io {name, ...} => "Io-" ^ name
                    | _ => "other"

val () =
  app (fn s => print (s ^ "\n"))
      [f (fn () => raise A 1),
       f (fn () => raise B),
       f (fn () => raise A 7),
       f (fn () => raise C ("x", 0)),
       f (fn () => raise C ("y", 3)),
       f (fn () => raise Empty),
       g (fn () => raise A 1),
       g (fn () => raise A 2),
       g (fn () => raise A' 1),
       g (fn () => raise Empty),
       h (fn () => raise Io {name = "n", cause = B}),
       h (fn () => raise Io {name = "n", cause = A 1}),
       h (fn () => raise B),
       h (fn () => raise Empty)]
