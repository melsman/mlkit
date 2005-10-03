functor OrderSetExt(structure Order : ORDER
		    val pu : Order.t Pickle.pu) 
    : MONO_SET_EXT where type elt = Order.t
    =
struct
    local structure SetImpl = OrderSetImpl(Order)
	  structure SetPP = MonoSetPP(SetImpl)
	  open SetImpl.Impl
    in
	fun die s = let val s = "Impossible.OrderSet: " ^ s
		    in print s ; raise Fail s
		    end

	val pu_bal =
	    Pickle.enumGen ("OrderSet.bal",[L,B,R])

	val pu_elt = pu
	val pu = 
	    let fun toInt E = 0
		  | toInt (N _) = 1
		val funE = Pickle.con0 E
		fun funN (pu : Set Pickle.pu) : Set Pickle.pu =
		    Pickle.con1 N (fn N a => a | _ => die "pu.N")
		    (Pickle.tup4Gen0(pu_elt,pu,pu,pu_bal))
	    in Pickle.dataGen ("OrderSet.Set",toInt,[funE,funN])
	    end

	open SetPP
    end
end