val half : Real16.real = 1.5;
val single : Real32.real = ~2.25;
val nested = ([half,~0.0],{single=single});
val extremes = (Real16.posInf,Real32.negInf);
val small : Real16.real = Real16.minPos;
:quit;
