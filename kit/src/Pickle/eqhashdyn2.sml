signature EQ_HASH_DYN =
    sig
	type dyn
	val new  : ('a -> int -> word) -> ('a * 'a -> bool) 
                   -> ('a -> dyn) * (dyn -> 'a)
	val eq   : dyn * dyn -> bool
	val hash : int -> dyn -> word 
    end

structure EqHashDyn :> EQ_HASH_DYN =
struct

    datatype method = RESET | EQ | SET | HASH of int
    type dyn = method -> bool
    val hash_tmp : word ref = ref 0w0
    fun new h eq = 
       let val r = ref NONE
	   fun eq_opt (NONE,NONE) = true
	     | eq_opt (SOME x, SOME y) = eq (x,y)
	     | eq_opt _ = false
       in  ( fn x => 
                let val new = SOME x
                in  fn RESET => (r := NONE; false)
		     | SET => (r := new; false)
		     | EQ => eq_opt(new, !r)
		     | HASH depth => (hash_tmp := h x depth; false)
                end
           , fn f => ( r := NONE
                     ; f SET
                     ; valOf(!r)
                     )
           )
       end

   fun eq (f1, f2) = (f2 RESET; f1 SET; f2 EQ)

   fun hash depth f = (f (HASH depth); !hash_tmp)
end