(* Mutable objects must survive recursive calls. Their finite regions belong
 * to stack usage, and unwinding must not require a finite-region list. *)
exception Finished of int
fun sample () : unit = prim ("mlkit_rp_sample", ())
fun descend 0 = (sample (); raise Finished 0)
  | descend n =
    let val a = ref n
        val b = ref (n + 1)
        val c = ref (n + 2)
        val d = ref (n + 3)
        val r = descend (n - 1) handle Finished x => x
    in if !a + !b + !c + !d = 4 * n + 6 then r + 1
       else raise Fail "finite stack corruption"
    end
val _ = if descend 200 = 200 then print "finite stack: OK\n"
        else raise Fail "finite stack result"
