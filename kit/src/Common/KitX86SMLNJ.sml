local
    structure KitX86 = 
	KitCompiler(ExecutionX86)
in
    structure Main = KitMainSMLNJ(KitX86)
end
	    