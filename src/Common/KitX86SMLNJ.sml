local
    structure KitX86 = 
	KitCompiler(ExecutionX86)
    val _ = Flags.turn_on "garbage_collection"
in
    structure Main = KitMainSMLNJ(KitX86)
end
	    
