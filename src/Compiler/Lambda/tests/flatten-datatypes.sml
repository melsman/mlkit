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
  (* Specialising the list creates a recursive component with the owner.
     Its members can have different boxing conventions. *)
  datatype expr = Lit of int | Branch of (int * int) * expr list
  fun expression 0 = Lit 1
    | expression n = Branch((n,n+1),[expression(n-1),Lit n])
  fun eval (Lit n) = n
    | eval (Branch((x,y),es)) = x + y + evalList es
  and evalList [] = 0
    | evalList (e::es) = eval e + evalList es
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
  fun makeOwner__noinline n = BoundaryOwner(BoundaryBox(ref n,0))
  fun readOwner__noinline (BoundaryOwner(BoundaryBox(r,_))) = deref r
  val privateOwner = makeOwner__noinline 23
  val ordinaryOwner = BoundaryOwner(BoundaryBox(ref 19,0))
  (* Refinement may change typing, but must retain captured state and effects. *)
  datatype 'a packet = Packet of ('a * int) * int
  val packets = ref 0
  fun pack__noinline x =
    (assign(packets,deref packets+1); Packet((x,3),4))
  fun unpack__noinline (Packet(p,_)) = #1 p
  val packetSum = unpack__noinline(pack__noinline 5) + unpack__noinline(pack__noinline 6)
  val xs = make 4
  val () = bump xs
  val () = assign(extract shared,9)
in
  val () = case (sum xs,total(build 3),deref r,
                    readBox__noinline privateBox,consume__noinline ordinaryBox,sumPairs(pairs 3),
                    leftSum(mutually 3),lengths,readOwner__noinline privateOwner,
                    consumeOwner__noinline ordinaryOwner,eval(expression 3),packetSum,deref packets) of
               (14,15,9,42,7,12,6,3,23,19,22,11,2) => print "datatype data flattening passed\n"
             | _ => print "datatype data flattening FAILED\n"
end
