(* Lifting a captured local loop introduces an internal function name.
 * Barry must emit legal SML for both its binding and recursive calls. *)
fun partition pivot xs =
    let
      fun loop ([], low, high) = (low, high)
        | loop (x :: rest, low, high) =
            if x < pivot then loop (rest, x :: low, high)
            else loop (rest, low, x :: high)
    in
      loop (xs, [], [])
    end

val () =
    if partition 3 [5,1,3,2,4] = ([2,1], [4,3,5]) andalso
       partition 5 [5,1,3,2,4] = ([4,2,3,1], [5])
    then print "Lifted names passed\n"
    else raise Fail "Lifted names"
