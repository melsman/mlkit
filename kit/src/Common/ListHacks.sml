(*$ListHacks: LIST_HACKS*)
functor ListHacks(): LIST_HACKS =
  struct
    fun union(set1, set2) =
      set1 @ List.all (fn x => not(List.member x set1)) set2

    fun intersect(set1, set2) =
      List.all (fn x => List.member x set1) set2

    fun minus(set1, set2) =
      List.all (fn x => not(List.member x set2)) set1

    fun eqSet(set1, set2) =
      case (minus(set1, set2), minus(set2, set1))
	of (nil, nil) => true
	 | _ => false
  end;
