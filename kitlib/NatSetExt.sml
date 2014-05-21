structure NatSetExt 
    :> MONO_SET_EXT where type elt = word =
struct
    local structure NatSetPP = MonoSetPP(NatSetImpl)
	  open NatSetImpl.Impl
    in
	fun die s = let val s = "Impossible.NatSet: " ^ s
		    in print s ; raise Fail s
		    end
	val pu =
	    let fun toInt empty = 0
		  | toInt (some _) = 1
		val fun_empty = Pickle.con0 empty
		fun fun_some pu =
		    Pickle.con1 some (fn some a => a | _ => die "pu.some")
		    (Pickle.tup3Gen0(Pickle.word,pu,pu))
	    in Pickle.dataGen ("NatSet",toInt,[fun_empty,fun_some])
	    end

	open NatSetPP	    
    end
end