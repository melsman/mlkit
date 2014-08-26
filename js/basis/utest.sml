structure Utest : UTEST = struct

val wrongCount = ref 0
val okCount = ref 0
val exnCount = ref 0

fun incr r = r := !r + 1

fun count s =
    (incr (case s of
               "OK" => okCount
             | "WRONG" => wrongCount
             | "EXN" => exnCount
             | _ => raise Fail "Utest.impossible");
     s)

fun check b = count(if b then "OK" else "WRONG")
fun check' f = count((if f () then "OK" else "WRONG") handle _ => "EXN")

fun range (from, to) p = 
    let open Int 
    in from > to orelse p from andalso range (from+1, to) p
    end

fun checkrange bounds = check o range bounds

fun tst0 s s' = print (s ^ "    " ^ s' ^ "\n")
fun tst  s b = tst0 s (check  b)
fun tst' s f = tst0 s (check' f)

fun tstrange s bounds = (tst s) o range bounds  

fun qq s = "'" ^ s ^ "'" 

val module = ref "unknown module"
fun tstStart s =
    (print ("Testing " ^ qq s ^ "...\n");
     wrongCount := 0; okCount := 0; exnCount := 0;
     module := s)
fun ppCount s r = " " ^ s ^ ": " ^ Int.toString(!r) ^ "\n"
fun ppCounts () =
    ppCount "OK" okCount ^ ppCount "WRONG" wrongCount ^ ppCount "EXN" exnCount
fun tstEnd () =
    if !wrongCount = 0 andalso !exnCount = 0 then
      print("Succesfully tested " ^ qq (!module) ^ ":\n" ^ ppCounts())
    else
      print("Failed test of " ^ qq (!module) ^ ":\n" ^ ppCounts())
     
end
