structure Data0 =
    struct
	datatype Data = 
	    Int of int
	  | Ref of Data ref
	  | Real of real
	  | Plus of Data * Data

	fun eval t = 
	    let fun eval0 (Int i, acc) = acc + real i
		  | eval0 (Real r, acc) = acc + r
		  | eval0 (Plus (t1,t2), acc) = eval0(t1,eval0(t2,acc))
		  | eval0 (Ref (ref t), acc) = eval0(t,acc)
	    in Real.toString(eval0(t,0.0))
	    end

	val d1 = Plus(Real 2.3,Int 4)
	val v1 = eval d1
	val r = ref d1
	val d2 = Plus(Ref r, Plus(d1,Int 4))
	val _ = r := Int 1000
	val v2 = eval d2
    end