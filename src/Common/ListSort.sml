(*$ListSort: LIST_SORT*)
functor ListSort(): LIST_SORT = 
struct
  (* val sort : ('a * 'a -> bool) -> 'a list -> 'a list*)
  (* sort lt l   sorts l according to the ordering lt *)

  (* We use top-down merge sort from Paulson *)

  exception Take and Drop

  fun take(xs, 0) = []
    | take(x::xs, n) = x :: take(xs, n-1)
    | take _ = raise Take

  fun drop(xs, 0) = xs
    | drop(x::xs, n) = drop(xs, n-1)
    | drop _ = raise Drop

  fun sort lt =
    let fun merge([], ys) = ys
          | merge(xs, []) = xs
          | merge(l1 as x::xs, l2 as y::ys) =
              if lt(x,y) then x::merge(xs, l2)
              else y::merge(l1,ys)
        fun tmergesort[] = []
          | tmergesort[x] = [x]
          | tmergesort xs =
              let val k = List.size xs div 2
              in merge(tmergesort(take(xs, k)),
                       tmergesort(drop(xs, k)))
              end
    in
        tmergesort
    end;
end;