
FILE = /home/mael/kit/src/bdys.txt
LINES = /home/mael/kit/src/lines.mael.sml


all:
	(cd Common/PM/NoProf; wc -l *.bdy > $(FILE))
	(cd Common/EfficientElab/PM/NoProf; wc -l *.bdy >> $(FILE)) 
	(cd Parsing/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Lambda/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Regions/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Kam/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Cfg/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/C/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Backend/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Backend/HpPaRisc/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Manager/PM/NoProf; wc -l *.bdy >> $(FILE))	

lines:
	(cd Common; cat *.sml > $(LINES))
	(cd Common/EfficientElab; cat *.sml >> $(LINES)) 
	(cd Parsing; cat *.sml >> $(LINES))	
	(cd Compiler; cat *.sml >> $(LINES))	
	(cd Compiler/Lambda; cat *.sml >> $(LINES))	
	(cd Compiler/Regions; cat *.sml >> $(LINES))	
	(cd Compiler/Kam; cat *.sml >> $(LINES))	
	(cd Compiler/Cfg; cat *.sml >> $(LINES))	
	(cd Compiler/C; cat *.sml >> $(LINES))	
	(cd Compiler/Backend; cat *.sml >> $(LINES))	
	(cd Compiler/Backend/HpPaRisc; cat *.sml >> $(LINES))	
	(cd Manager; cat *.sml >> $(LINES))
	wc -l $(LINES)
	rm -f $(LINES)
