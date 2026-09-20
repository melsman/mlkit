(* Exercise allocation after normal ML execution has returned to the runtime.
 * GC must remain disabled across the C-to-ML exit callback bridge. *)
val () = OS.Process.atExit (fn () =>
  let
    fun loop 0 = print "exit callback completed\n"
      | loop n =
        let val s = String.implode (List.tabulate (10000, fn _ => #"x"))
        in
          if String.size s = 10000 then loop (n-1)
          else raise Fail "unexpected string size"
        end
  in
    loop 100
  end)
