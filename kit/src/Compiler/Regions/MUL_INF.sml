(*$MUL_INF*)

(* Multiplicity Inference *)

signature MUL_INF =  
  sig
        type ('a,'b)LambdaPgm_phi
         and ('a,'b,'c)LambdaPgm_psi
         and cone and place and mulef and efenv and mul and qmularefset and mularefmap
         and ('a,'b,'c)LambdaExp

        val mulInf: (place,unit)LambdaPgm_phi * mularefmap * cone * efenv  -> 
                    (place,place*mul, qmularefset ref)LambdaPgm_psi * efenv * mularefmap

        val k_normPgm: (place,place*mul, qmularefset ref)LambdaPgm_psi  ->
                       (place,place*mul, qmularefset ref)LambdaPgm_psi 

        type StringTree

        val layoutLambdaPgm: (place,place*mul,qmularefset ref)LambdaPgm_psi -> StringTree
        val layoutExp: (place,place*mul,qmularefset ref)LambdaExp -> StringTree
  end      
