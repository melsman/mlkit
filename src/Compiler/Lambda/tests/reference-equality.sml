(* Exercise reference identity together with a raw payload moved to the prefix. *)
local
  datatype node = N of {next : int ref, prev : int ref, weight : real}
  fun make k = N {prev = ref k, next = ref k, weight = Real.fromInt k}
  fun same (N {next = a, weight = x, ...}, N {next = b, weight = y, ...}) =
    Real.==(x,y) andalso a = b
  fun different (N {next, weight = x, ...}, N {prev, weight = y, ...}) =
    Real.==(x,y) andalso next = prev
  fun sameProjected (N {next = a, ...}, N {next = b, ...}) = a = b
  fun differentProjected (N {next, ...}, N {prev, ...}) = next = prev
  fun set (N {next, ...}) k = next := k
  fun get (N {next, ...}) = !next
  fun check true = ()
    | check false = raise Fail "absorbed reference identity"
  fun makeNodes 0 = []
    | makeNodes n = make 7 :: makeNodes (n - 1)
  val (a, b) = case makeNodes 2 of [a,b] => (a,b)
                | _ => raise Fail "node count"
  val alias = a
  val () = check (same (a, alias))
  val () = check (not (same (a, b)))
  val () = check (not (different (a, a)))
  val () = check (not (different (a, b)))
  val () = check (sameProjected (a, alias))
  val () = check (not (sameProjected (a, b)))
  val () = check (not (differentProjected (a, a)))
  val effects = ref 0
  fun observe n = (effects := !effects + 1; n)
  val () = check (same (observe a, observe alias))
  val () = check (not (different (observe a, observe a)))
  val () = check (!effects = 4)
  val () = set a 9
  val () = check (same (a, alias) andalso not (same (a, b)))
  val () = check (get alias = 9 andalso get b = 7)
in
  val () = print "reference equality flattening passed\n"
end
