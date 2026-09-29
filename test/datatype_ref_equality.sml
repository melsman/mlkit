fun check (name, b) = print (name ^ ": " ^ (if b then "OK\n" else "FAIL\n"))
datatype t = T of (int -> int) ref
val r = ref (fn x:int => x)
val () = check ("function ref", T r = T r andalso T r <> T (ref (fn x => x)))
datatype a = A of (int -> int) array
val ar = Array.array (1, fn x:int => x)
val () = check ("function array", A ar = A ar andalso A ar <> A (Array.array (1, fn x => x)))
functor Wrap (X:sig type t val x:t end) =
struct
  datatype t = R of X.t ref | A of X.t array
  val r = R (ref X.x)
  val a = A (Array.array (1, X.x))
  val () = check ("abstract ref", r = r andalso r <> R (ref X.x))
  val () = check ("abstract array", a = a andalso a <> A (Array.array (1, X.x)))
end
structure W = Wrap (type t = int -> int val x = fn x:int => x)
datatype recursive = Leaf of (int -> int) ref | Node of recursive list
val v = Node [Leaf r]
val () = check ("recursive", v = Node [Leaf r] andalso v <> Node [])
