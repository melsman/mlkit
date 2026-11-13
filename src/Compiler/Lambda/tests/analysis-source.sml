val () =
  let
    fun inspect 0 (p:int ref * int) = !(#1 p)
      | inspect n p = inspect (n-1) p
    val p = (ref 40,2)
    val () = #1 p := 42
  in if inspect 3 p = 42 then print "analysis source test passed\n"
     else raise Fail "flattening changed behaviour"
  end
