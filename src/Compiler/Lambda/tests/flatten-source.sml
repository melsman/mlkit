(* Recursive consumers keep the relevant records live. Each feature combination
   must preserve the order of effects, aliases and higher-order mutable values. *)
val () =
  let
    fun assert b = if b then () else raise Fail "data flattening semantics"
    fun inspect 0 (p:int * (int * int)) = #1 p + #1 (#2 p) + #2 (#2 p)
      | inspect n p = inspect (n-1) p
    val () = assert (inspect 3 (1,(20,21)) = 42)
    val trace = ref 0
    fun mark n = (trace := !trace * 10 + n; n)
    val p = (ref (mark 1), (mark 2, (mark 3, mark 4)), mark 5)
    val alias = p
    val () = assert (!trace = 12345)
    val () = #1 p := (#1 alias := 40; !(#1 p) + 2)
    val () = assert (!(#1 alias) = 42 andalso #2 (#2 (#2 p)) = 4 andalso #3 p = 5)
    val () = #1 p := (raise Fail "initializer") handle Fail "initializer" => ()
    val () = assert (!(#1 p) = 42)
    fun make x = (ref x, 7)
    fun read 0 (p : 'a ref * int) = !(#1 p)
      | read n p = read (n-1) p
    fun write (p : 'a ref * int) x = #1 p := x
    val functions = make (fn n : int => n + 1)
    val () = write functions (fn n => n + 2)
    val () = assert (read 3 functions 40 = 42)
    val nested = (ref (ref 1), 0)
    val cell = !(#1 nested)
    val () = cell := 42
    val () = assert (!(!(#1 nested)) = 42)
    val strings = (ref "old", 1)
    fun pressure 0 acc = acc
      | pressure n acc = pressure (n-1) (Int.toString n :: acc)
    val () = ignore (pressure 100000 [])
    val young = String.implode [#"n",#"e",#"w"]
    val () = assert (String.size young = 3)
    val () = #1 strings := young
    val () = ignore (pressure 100000 [])
    val () = assert (read 3 strings = "new")
  in print "data flattening semantics passed\n"
  end
