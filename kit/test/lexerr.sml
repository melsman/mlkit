(*lexerr.sml  20/06/1997 13:19. tho.*)
(*what does the kit do with syntax (lexing) errors?*)
(*
val val v = 0;

gives

 [reading source file:   /usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/lexerr.sml]

/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/lexerr.sml, line 4, column 4:
  val val v = 0;
      ^
syntax error found at VAL
*** Uncaught exception

(someone ought to handle that uncaught exception)

~/arb/kit/src> fsml 'syntax error found' 
                        error("syntax error found at " ^ (showTerminal term),
          | nil => (error("syntax error found at " ^ (showTerminal term),
./Parsing/MyBase.sml
*)


(*
val s = "abra\356kadabra" (*\356 is illegal*)
  
gives

/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/lexerr.sml, line 24, column 15:
  val s = "abra\356kadabra" (*\356 is illegal*)
                 ^
bad ASCII escape: \356
*** Uncaught exception
*)

(*
	     val c = #"uha";
	       
gives

 /usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/lexerr.sml, line 37, column 26:
               val c = #"uha";
                            ^
string must have length 1
*)

	     val c = #"C";
	     val w = 0w1;
	     val w1 = 0wxff;

	     val c2 = #"\u00ff";
(*
	     val c3 = #"\uffff";
	       
gives
 
/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/lexerr.sml, line 54, column 26:
               val c3 = #"\uffff";
                            ^
ASCII escape \uffff must be < 255
*)
(*
	       	     val c4 = #"\u00f";

gives

 /usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/lexerr.sml, line 65, column 34:
                       val c4 = #"\u00f";
                                    ^
illegal string escape
*)
(*
	       	     val c5 = #"\999";

/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/lexerr.sml, line 74, column 34:
                       val c5 = #"\999";
                                    ^
ASCII escape \999 must be < 255
*)
(*by the way, `< 255' has been corrected to `<= 255'*)
(*
	     val c2 = #"";

gives

/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/lexerr.sml, line 83, column 24:
               val c2 = #"";
                          ^
string must have length 1
*)

(*men nu giver
infix = fun op = (x: ''a, y: ''a): bool =           prim(0, (x, y))

	     val b = #"c" = 42;
	       
ikke en typefejl, som det burde.  Rettet. Nu giver det:
/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/lexerr.sml, line 102, column 21:
               val b = #"c" = 42;
                       ^^^^^^^^^
Type clash,
   operand suggests operator type: char * int->int
   but I found operator type:      char * char->bool

   hvilket vel er, som det skal være? Jep, sepp.*)
(*
	     val l = [#"c", 42];

gives

 /usr/local/topps/MLKit/version2_onwards/hojfeld/kit/kitdemo/hojfeld/lexerr.sml, line 108, column 22:
               val l = [#"c", 42];
                        ^^^^
Type clash,
   operand suggests operator type: char * int list->'a
   but I found operator type:      char * char list->char list
*)

	       	     val l = [#"c", #"d"];


