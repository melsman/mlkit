(* Stack sizes/offsets here are in 64-bit words. CallConv uses a logical
 * header between spilled results and arguments; emitters materialize it.
 * A return address's presence on the stack is not an ISA assumption.
 *)
structure FrameLayout :>
sig
  type t
  val x64 : t
  val arm64 : t
  val headerWords : t -> int
  val returnWord : t -> int
  val returnOffsetFromTop : t -> int
  val handlerWords : t -> int
  val callWords : t -> {args:int, results:int} -> int
  val alignFrame : t -> {locals:int, call:int} -> int
end =
struct
  type t = {header:int, return:int, alignment:int, handler:int}
  val x64 = {header=1, return=0, alignment=2, handler=4}
  val arm64 = {header=2, return=1, alignment=2, handler=6}
  fun headerWords ({header,...}:t) = header
  fun returnWord ({return,...}:t) = return
  fun returnOffsetFromTop (f:t) = returnWord f
  fun handlerWords ({handler,...}:t) = handler
  fun callWords f {args,results} = args + results + headerWords f
  fun alignFrame ({alignment,...}:t) {locals,call} =
      locals + (alignment - (locals + call) mod alignment) mod alignment
end
