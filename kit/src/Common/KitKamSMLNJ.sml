local
    structure KitKam = 
	KitCompiler(ExecutionKAM)
in
    structure Main = KitMainSMLNJ(KitKam)
end
	    