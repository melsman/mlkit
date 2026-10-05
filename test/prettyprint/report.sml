(* Isolate the printer from Report's unrelated compiler/pickling dependencies. *)
structure Report =
struct
  type Report = string list
  fun line s = [s]
  val flatten = List.concat
end
