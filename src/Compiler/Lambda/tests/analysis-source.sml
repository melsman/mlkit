val () =
  let
    fun make n = (ref n,n)
    fun increment (p:int ref * int) = #1 p := !(#1 p) + 1
    fun inspect (p:int ref * int) = !(#1 p)
    val p = make 40
  in increment p; increment p;
     if inspect p = 42 then print "analysis source test passed\n"
     else raise Fail "analysis changed behaviour"
  end
