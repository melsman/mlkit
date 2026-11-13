(* Primitive wrappers must expose immediate selected-reference accesses even
   when aggressive optimisation is disabled. *)
local
fun deref (r:'a ref) : 'a = prim("!", r)
fun assign (r:'a ref, x:'a) : unit = prim(":=", (r,x))
fun selected false (p:int * int ref) =
      (assign (#2 p, 41); assign (#2 p, (assign (#2 p, 41); 42)); deref (#2 p))
  | selected true p = selected false p
val result = selected true (7, ref 0)

(* Evaluate the reference argument before the RHS changes its source. *)
val original = ref 0
val replacement = ref 0
val holder = ref (original, 1)
val () = assign (#1 (deref holder), (assign (holder, (replacement, 2)); 19))

(* Reversed forwarding must not reverse the original argument evaluation. *)
fun reversed (x, r) = assign (r, x)
val () = reversed ((assign (holder, (original, 3)); 23), #1 (deref holder))

(* A wrapper that reads twice must retain the intervening mutation. *)
fun twice (r, f) = (deref r, f (), deref r)
val observed = twice (original, fn () => assign (original, 31))
in
val () =
  case (result, deref original, deref replacement, observed) of
      (42, 31, 0, (23, (), 31)) => print "selected reference inlining passed\n"
    | _ => print "selected reference inlining FAILED\n"
end
