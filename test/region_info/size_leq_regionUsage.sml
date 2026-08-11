(* The computed size of a value should not exceed the
   memory usage of the region it resides in. *)

fun test () =
    let with r
      val name = "size_leq_regionUsage"
      val s : string`r = CharVector.tabulate (100, fn _ => #"x")
      val computed = Size.size Size.string s
      val regionUsage = Regions.memoryUsageOfRegion `[r] ()
      val ok = (computed <= regionUsage)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name
                   ^ " (size=" ^ Int.toString computed
                   ^ ", region=" ^ Int.toString regionUsage ^ ")\n");
      ignore s
    end

val _ = test ()
