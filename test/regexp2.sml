(* Testing of the RegExp module *)

fun test_t s true = print (s ^ " : OK\n")
  | test_t s false = print (s ^ " : ERR\n")

fun test_f s b = test_t s (not b)

fun test_e s f = (f(); print (s ^ " : ERR\n"))
  handle _ => print (s ^ " : OK\n")

val _ =
  let open RegExp
      fun match' r s = match (fromString r) s 
  in
   test_t "test1" (match' ".*\\.sml" "Mosml.sml");
   test_f "test2" (match' ".*\\.sml" "Mosml.sig");
   test_t "test3" (match' "(a|b)*" "abba"); 
   test_f "test4" (match' "(a | b)*" "abba");
   test_t "test5" (match' "(a | b)*" "a  b ba ");
   test_t "test6" (match' "$" "");
   test_f "test7" (match' "$" "a");
   test_t "test8" (match' "$|a" "");
   test_t "test9" (match' "$|a" "a");
   test_f "test10" (match' "$|a" "b");
   test_f "test11" (match' "$|a" "$");
   test_t "test12" (match' "\\$" "$");
   test_t "test13" (match' "a+" "aaa");
   test_t "test14" (match' "a+" "a");
   test_f "test15" (match' "a+" "");
   test_f "test16" (match' "a+" "ab");
   test_t "test17" (match' "[a-z]+a" "helloa");
   test_f "test18" (match' "[a-z]+" "helloW");
   test_f "test19" (match' "[a-z]+a" "hello");
   test_t "test20" (match' "(ab)a" "aba");
   test_f "test21" (match' "(ab)ab" "aba");
   test_t "test22" (match' "[0-9]+" "10429");
   test_f "test23" (match' "[0-9]+" "10429a");
   test_f "test24" (match' "[0-9]+" "a10429");
   test_f "test25" (match' "[0-9]+" "104s29");
   test_f "test26" (match' "[0-9]+" "");
   test_f "test27" (match' "[0-9]+" "a");
   test_t "test28" (match' "[1-9][0-9]*" "15360");
   test_f "test29" (match' "[1-9][0-9]*" "01536");
   test_f "test29a" (match' "[1-9][0-9]*" "15a36");
   test_t "test29b" (match' "[1-9][0-9]*" "1");
   test_t "test30" (match' "[a-zA-ZæøåÆØÅ ]+" "æbler smager godt");
   test_f "test31" (match' "[a-zA-ZæøåÆØÅ ]+" "æbler smager godt..");
   test_e "test32a" (fn()=> fromString "[1-9-]");
   test_e "test32b" (fn()=> fromString "[-sd]");
   test_e "test32c" (fn()=> fromString "[aas");
   test_e "test32d" (fn()=> fromString "[");
   test_e "test32e" (fn()=> fromString "]");
   test_e "test32f" (fn()=> fromString "[df]]");
   test_t "test33a" (match' "[^a-z ]+" "HELLO-YOU");
   test_f "test33b" (match' "[^a-z ]+" "HELLO yOU");
   test_t "test33c" (match' "[^a]" "b");
   test_t "test33d" (match' "[^ac]+" "bdd");
   test_f "test33e" (match' "[^ac]+" "bdcd");
   test_t "test33f" (match' "[^\\^]+" "bdd");
   test_f "test33g" (match' "[^\\^]+" "s^sd");
   test_t "test33h" (match' "[a-zA-Z\\-_]+" "this-is_great");
   let val email_pattern = "[a-zA-Z][a-zA-Z0-9\\-_.]*@([a-zA-Z][a-zA-Z0-9\\-_.]*)+"
   in test_t "test33i" (match' email_pattern "mael@it.edu");
     test_t "test33j" (match' email_pattern "nh@it.edu");
     test_t "test33k" (match' email_pattern "nh@it-c.dk");
     test_f "test33l" (match' email_pattern "@it-c.dk");
     test_f "test33m" (match' email_pattern "nh@-it-c.dk")
   end;
   test_t "test34a" (match' "aa(a*)bb" "aabb");
   test_t "test34b" (match' "aa($)bb" "aabb");
   test_t "test34c" (match' "aa\\\\bb" "aa\\bb");
   test_t "test34d" (match' "aa[\\\\]bb" "aa\\bb");
   ()
  end

val _ = 
  let
    fun extract' r s = RegExp.extract (RegExp.fromString r) s  
  in
    test_t "test50a" (extract' "a(bcd)b" "abcdb" = SOME ["bcd"]);
    test_t "test50b" (extract' "(bcd)b" "bcdb" = SOME ["bcd"]);
    test_t "test50c" (extract' "a(bcd)" "abcd" = SOME ["bcd"]);
    test_t "test50d" (extract' "a(bcd)k(123)dd" "abcdk123dd" = SOME ["bcd","123"]);
    test_t "test50e" (extract' "Name: ([a-zA-Z]+)" "Name: Martin" = SOME ["Martin"]);
    test_t "test50f" (extract' "Name: (([a-zA-Z]+))" "Name: Martin" = SOME ["Martin", "Martin"]);
    test_t "test50g" (extract' "b(ab)*b" "babababb" = SOME ["ab","ab","ab"]);
    test_t "test50h" (extract' "b(ab)*b" "bb" = SOME []);
    test_t "test50i" (extract' "([a-zA-Z][a-zA-Z0-9\\-._]*)@([a-zA-Z][a-zA-Z0-9\\-_]*)(\\.[a-zA-Z][a-zA-Z0-9\\-_]*)*" "mael@dina.kvl.dk" = SOME ["mael", "dina", ".kvl", ".dk"]);
    test_t "test50j" (extract' "a(b?)c" "abc" = SOME ["b"]);
    test_t "test50k" (extract' "a(b?)c" "ac" = SOME []);
    test_t "test50l" (extract' "aa(a*)bb" "aabb" = SOME []);
    ()    
  end

