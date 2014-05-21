signature SOBJ_CPS =
  sig
   type sval 
   type sobj
   type sheap
   type sexp

   val Nil    : sexp
   val T      : sexp
   val F      : sexp
   val I      : int -> sexp

   val Cons   : (sexp * sexp) -> sexp
   val Lambda : (sobj -> sexp) -> sexp
   val V      : sobj -> sexp

   val If     : (sexp * sexp * sexp) -> sexp
   val Car    : sexp -> sexp
   val Cdr    : sexp -> sexp
   val Apply  : (sexp * sexp) -> sexp
   
   val NilP    : sexp -> sexp
   val BoolP   : sexp -> sexp
   val IntP    : sexp -> sexp
   val PairP   : sexp -> sexp
   val LambdaP : sexp -> sexp

   val eval    : int -> sexp -> (sheap * sobj)
end
