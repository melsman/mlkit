structure IntFinMapPTExt 
    :> MONO_FINMAP_EXT where type dom = int =
struct

    local structure IntFinMapPP = MonoFinMapPP(IntFinMapPTImpl)
	  open IntFinMapPTImpl.Impl
    in
	open IntFinMapPP

	fun pu (pu_a: 'a Pickle.pu) : 'a map Pickle.pu =
	    let fun toInt Empty = 0
		  | toInt (Lf _) = 1
		  | toInt (Br _) = 2
		
		val pu_Empty = Pickle.con0 Empty
		    
		fun pu_Lf _ =
		    Pickle.con1 Lf (fn Lf a => a | _ => raise Fail "IntFinMapPT.pu_Lf")
		    (Pickle.pairGen0(Pickle.word,pu_a))
		    
		fun pu_Br pu =
		    Pickle.con1 Br (fn Br a => a | _ => raise Fail "IntFinMapPT.pu_Br")
		    (Pickle.tup4Gen0(Pickle.word,Pickle.word,pu,pu))
	    in
		Pickle.dataGen ("IntFinMapPT.map",toInt,[pu_Empty, pu_Lf, pu_Br])
	    end
    end
end