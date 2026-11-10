(* Primitive operations make the representation checks independent of Basis
   inlining. Pattern-bound aliases must expose accesses without moving effects. *)
local
  infix 6 + -
  infixr 5 ::
  fun deref (r:'a ref) : 'a = prim("!", r)
  fun assign (r:'a ref, x:'a) : unit = prim(":=", (r,x))
  datatype chain = End | Cell of int ref * chain
  fun make 0 = End | make n = Cell(ref n,make(n-1))
  fun sum End = 0
    | sum (Cell(r,rest)) = deref r + sum rest
  fun bump End = ()
    | bump (Cell(r,rest)) = (assign(r,deref r+1); bump rest)
  datatype tree = Leaf | Node of (int * int) * tree
  fun build 0 = Leaf | build n = Node((n,n+1),build(n-1))
  fun total Leaf = 0 | total (Node(p,rest)) = #1 p + #2 p + total rest
  fun pairs 0 = [] | pairs n = (n,ref n) :: pairs(n-1)
  fun sumPairs [] = 0 | sumPairs ((n,r)::rest) = n + deref r + sumPairs rest
  datatype left = Stop | Left of int ref * right
  and right = Right of left
  fun mutually 0 = Stop | mutually n = Left(ref n,Right(mutually(n-1)))
  fun leftSum Stop = 0 | leftSum (Left(r,rest)) = deref r + rightSum rest
  and rightSum (Right next) = leftSum next
  (* Shared polymorphic traversal code must retain one consistent layout. *)
  fun length__noinline [] = 0 | length__noinline (_::xs) = 1 + length__noinline xs
  val lengths = length__noinline [1,2] + length__noinline [ref 3]
  (* A returned reference must keep its identity and separate allocation. *)
  datatype shared = Shared of int ref * int
  fun extract (Shared(r,_)) = r
  val r = ref 7
  val shared = Shared(r,0)
  fun makeBox__noinline n = BoundaryBox(ref n,0)
  fun readBox__noinline (BoundaryBox(r,_)) = deref r
  fun writeBox__noinline (BoundaryBox(r,_)) n = assign(r,n)
  val privateBox = makeBox__noinline 1
  val ordinaryBox = BoundaryBox(ref 7,0)
  val () = writeBox__noinline privateBox 42
  val xs = make 4
  val () = bump xs
  val () = assign(extract shared,9)
in
  val () = case (sum xs,total(build 3),deref r,
                    readBox__noinline privateBox,consume__noinline ordinaryBox,sumPairs(pairs 3),
                    leftSum(mutually 3),lengths) of
               (14,15,9,42,7,12,6,3) => print "datatype data flattening passed\n"
             | _ => print "datatype data flattening FAILED\n"
end
