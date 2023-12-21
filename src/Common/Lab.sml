(* Labels - Definition v3 page 4 *)

structure Lab :> LAB =
  struct

    type lab = string

   (* Ordering of labels requires a rethink, because we have to be careful
      when printing tuple types and values. `Lab.<' should behave correctly
      for numeric labels (2 < 10) and still give an unambiguous ordering
      for others (a2 > a10). We'd also better be convinced that the
      ordering is transitive, or things could start going horribly wrong. *)

    val op < = fn (str1, str2) =>
      (case (Int.fromString str1, Int.fromString str2)
        of (SOME i1, SOME i2) => i1 < i2
         | _ => str1 < str2)
         handle _ => str1 < str2 (* fromString may raise Overflow *)

    fun is_LabN (str, i) =
      (case Int.fromString str
         of SOME i' => (i = i')
          | _ => false)
         handle _ => false

    fun pr_Lab str = str

    val mk_IdentLab = fn x => x
    val mk_IntegerLab = Int.toString

    val pu = Pickle.string

    (* Notice: Different ordering than string ordering *)
    structure Map = OrderFinMap(struct type t = lab
                                       val lt = op<
                                end)
  end
