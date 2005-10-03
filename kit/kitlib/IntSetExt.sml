structure IntSetExt 
    :> MONO_SET_EXT where type elt = int =
struct

    local structure IntSetPP = MonoSetPP(IntSetImpl)
	  open IntSetImpl.Impl
    in
	open IntSetPP

	val pu_bal =
	    Pickle.enumGen ("IntSet.bal",[L,B,R])
	    
	fun die s = 
	    let val s = "Impossible: OrderSet." ^ s
	    in print s ; raise Fail s
	    end
	
	val pu = 
	    let fun toInt E = 0
		  | toInt (N _) = 1
		val funE = Pickle.con0 E
		fun funN pu =
		    Pickle.con1 (fn (a,b,c,d) => N(a,d,c,b)) (fn N(a,b,c,d) => (a,d,c,b) 
		  | _ => die "pu.funN")
		    (Pickle.tup4Gen0(Pickle.int,pu_bal,pu,pu))
	    in Pickle.dataGen ("IntSet.Set",toInt,[funE,funN])
	    end
    end
end