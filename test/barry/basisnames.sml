(* Basis operators must be recovered from source spellings, including colons. *)
val cell = ref 0
val () = cell := 2
val joined = String.^ ("a","b") ^ "c"
val values = [1] @ [2]
val () =
    if joined = "abc" andalso joined <> "ab" andalso !cell = 2 andalso
       Int.> (!cell,1) andalso Int.+ (1,2) = 3 andalso
       String.< ("a","b") andalso String.<= ("a","a") andalso
       String.> ("b","a") andalso String.>= ("a","a") andalso
       values = [1,2] andalso abs (9.0 / 2.0 - 4.5) < 0.0001
    then print "Basis names passed\n"
    else raise Fail "Basis names"
