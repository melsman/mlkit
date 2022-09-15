(* Locally live variables: second pass of Storage Mode Analysis *)

signature LOCALLY_LIVE_VARIABLES =
sig
   type liveset  (* sets of locally live lvars and excons *)

   type place and mul and qmularefset
     and ('a,'b,'c)LambdaPgm (*from MulExp*)
     and lvar and excon

   (*llv(p): annotate every binder in p with a set of
             locally live lvars and excons *)

   val llv: (place, place*mul, qmularefset ref)LambdaPgm ->
            (place*liveset, place*mul, qmularefset ref)LambdaPgm

   (* findLvar f liveset returns
         Some(lvar,a), if lvar is in liveset and f(lvar) = Some a
      or None,         if for all lvar in liveset, f(lvar)= None

      Similarly for findExcon.
   *)

   val findLvar: (lvar -> 'a option) -> liveset -> (lvar * 'a) option
   val findExcon: (excon -> 'a option) -> liveset -> (excon * 'a) option

   type StringTree
   val layout_liveset: liveset -> StringTree
end
