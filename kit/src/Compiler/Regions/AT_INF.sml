(*$AT_INF*)

(* Storage Mode Analysis *)

signature AT_INF =
  sig
        type ('a,'b,'c)LambdaPgm and ('a,'b,'c)LambdaExp
         and place
         and mul
         and qmularefset

        datatype 'a at = ATTOP of 'a | ATBOT of 'a | SAT of 'a | IGNORE

        val sma : (place, place*mul, qmularefset ref)LambdaPgm -> (place at, place*mul, unit)LambdaPgm

             (* In the old storage mode analysis an environment was propagated to later
	      * program units. Since we must assign storage mode attop to regions passed
	      * to functions declared outside a program unit, the environment is of no
	      * use. 13/10/96-Martin 
	      *)

        type StringTree
        val layout_pgm : (place at, place*mul, unit)LambdaPgm -> StringTree
        val layout_exp_brief : (place at, place*mul, unit)LambdaExp -> StringTree
        val layout_pgm_brief : (place at, place*mul, unit)LambdaPgm -> StringTree
	val layout_at : ('a -> StringTree) -> 'a at -> StringTree Option
  end

