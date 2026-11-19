fun equal (a, b) =
    let val n = String.size a
    in
        if n = String.size b then
            let fun loop i =
                    if i < n then
                        String.sub (a, i) = String.sub (b, i) andalso loop (i + 1)
                    else true
            in loop 0
            end
        else false
    end

(* The failed equality branch must retain its bounds checks. *)
fun unequal (a, b) =
    let val n = String.size a
    in
        if n = String.size b then false
        else
            let fun loop i =
                    if i < n then
                        String.sub (a, i) = String.sub (b, i) andalso loop (i + 1)
                    else true
            in loop 0
            end
    end

fun check true = ()
  | check false = raise Fail "string bounds"
val strings = ["", "a", "b", "ab", "aa", "abc", "abd", "abcdef"]
val () = List.app (fn a => List.app (fn b => check (equal (a,b) = (a = b))) strings) strings
val () = check ((unequal ("aa", "a"); false) handle Subscript => true)
(* Chained equalities must be scoped to both successful tests. *)
fun triple (a, b, c) =
    let val n = String.size a
        val m = String.size b
    in
        if n = m then
            if m = String.size c then
                let fun loop i =
                        if i < n then
                            String.sub (a, i) = String.sub (b, i) andalso
                            String.sub (b, i) = String.sub (c, i) andalso loop (i + 1)
                        else true
                in loop 0
                end
            else false
        else false
    end
val () = List.app (fn a => List.app (fn b => List.app
    (fn c => check (triple (a,b,c) = (a = b andalso b = c))) strings) strings) strings
val () = print "OK\n"
