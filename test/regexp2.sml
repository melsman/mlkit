(* Test of RegExp structure *)

fun test_t s true = print (s ^ " : OK\n")
  | test_t s false = print (s ^ " : ERR\n")

fun test_f s b = test_t s (not b)

fun test_e s f = (f(); print (s ^ " : ERR\n"))
  handle _ => print (s ^ " : OK\n")

val _ =
  (
   test_t "test1" (RegExp.match (RegExp.fromString ".*\\.sml") "Mosml.sml");
   test_f "test2" (RegExp.match (RegExp.fromString ".*\\.sml") "Mosml.sig");
   test_t "test3" (RegExp.match (RegExp.fromString "(a|b)*") "abba"); 
   test_f "test4" (RegExp.match (RegExp.fromString "(a | b)*") "abba");
   test_t "test5" (RegExp.match (RegExp.fromString "(a | b)*") "a  b ba ");
   test_t "test6" (RegExp.match (RegExp.fromString "$") "");
   test_f "test7" (RegExp.match (RegExp.fromString "$") "a");
   test_t "test8" (RegExp.match (RegExp.fromString "$|a") "");
   test_t "test9" (RegExp.match (RegExp.fromString "$|a") "a");
   test_f "test10" (RegExp.match (RegExp.fromString "$|a") "b");
   test_f "test11" (RegExp.match (RegExp.fromString "$|a") "$");
   test_t "test12" (RegExp.match (RegExp.fromString "\\$") "$");
   test_t "test13" (RegExp.match (RegExp.fromString "a+") "aaa");
   test_t "test14" (RegExp.match (RegExp.fromString "a+") "a");
   test_f "test15" (RegExp.match (RegExp.fromString "a+") "");
   test_f "test16" (RegExp.match (RegExp.fromString "a+") "ab");
   test_t "test17" (RegExp.match (RegExp.fromString "[a-z]+a") "helloa");
   test_f "test18" (RegExp.match (RegExp.fromString "[a-z]+") "helloW");
   test_f "test19" (RegExp.match (RegExp.fromString "[a-z]+a") "hello");
   test_t "test20" (RegExp.match (RegExp.fromString "(ab)a") "aba");
   test_f "test21" (RegExp.match (RegExp.fromString "(ab)ab") "aba");
   test_t "test22" (RegExp.match (RegExp.fromString "[0-9]+") "10429");
   test_f "test23" (RegExp.match (RegExp.fromString "[0-9]+") "10429a");
   test_f "test24" (RegExp.match (RegExp.fromString "[0-9]+") "a10429");
   test_f "test25" (RegExp.match (RegExp.fromString "[0-9]+") "104s29");
   test_f "test26" (RegExp.match (RegExp.fromString "[0-9]+") "");
   test_f "test27" (RegExp.match (RegExp.fromString "[0-9]+") "a");
   test_t "test28" (RegExp.match (RegExp.fromString "[1-9][0-9]*") "15360");
   test_f "test29" (RegExp.match (RegExp.fromString "[1-9][0-9]*") "01536");
   test_f "test29a" (RegExp.match (RegExp.fromString "[1-9][0-9]*") "15a36");
   test_t "test29b" (RegExp.match (RegExp.fromString "[1-9][0-9]*") "1");
   test_t "test30" (RegExp.match (RegExp.fromString "[a-zA-ZæøåÆØÅ ]+") "æbler smager godt");
   test_f "test31" (RegExp.match (RegExp.fromString "[a-zA-ZæøåÆØÅ ]+") "æbler smager godt..");
   test_e "test32a" (fn()=> RegExp.fromString "[1-9-]");
   test_e "test32b" (fn()=> RegExp.fromString "[-sd]");
   test_e "test32c" (fn()=> RegExp.fromString "[aas");
   test_e "test32d" (fn()=> RegExp.fromString "[");
   test_e "test32e" (fn()=> RegExp.fromString "]");
   test_e "test32f" (fn()=> RegExp.fromString "[df]]");
   test_t "test33a" (RegExp.match (RegExp.fromString "[^a-z ]+") "HELLO-YOU");
   test_f "test33b" (RegExp.match (RegExp.fromString "[^a-z ]+") "HELLO yOU");
   test_t "test33c" (RegExp.match (RegExp.fromString "[^a]") "b");
   test_t "test33d" (RegExp.match (RegExp.fromString "[^ac]+") "bdd");
   test_f "test33e" (RegExp.match (RegExp.fromString "[^ac]+") "bdcd");
   test_t "test33f" (RegExp.match (RegExp.fromString "[^\\^]+") "bdd");
   test_f "test33g" (RegExp.match (RegExp.fromString "[^\\^]+") "s^sd");
   test_t "test33h" (RegExp.match (RegExp.fromString "[a-zA-Z\\-_]+") "this-way-is_grest");
   let val email_pattern = "[a-zA-Z][a-zA-Z0-9\\-_.]*@([a-zA-Z][a-zA-Z0-9\\-_.]*)+"
   in test_t "test33i" (RegExp.match (RegExp.fromString email_pattern) "mael@it.edu");
     test_t "test33j" (RegExp.match (RegExp.fromString email_pattern) "nh@it.edu");
     test_t "test33k" (RegExp.match (RegExp.fromString email_pattern) "nh@it-c.dk");
     test_f "test33l" (RegExp.match (RegExp.fromString email_pattern) "@it-c.dk");
     test_f "test33m" (RegExp.match (RegExp.fromString email_pattern) "nh@-it-c.dk")
   end;
   ())
