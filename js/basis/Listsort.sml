(** Smooth Applicative Merge Sort, Richard O'Keefe 1982        **)
(** From L.C. Paulson: ML for the Working Programmer, CUP 1991 **)
(** Optimized for Moscow ML                                    **)
(** modified for the MLKit, 2001-06-07                         **)

structure Listsort : LISTSORT =
  struct
    fun sort ordr xs =
      let 
	fun merge []      ys = ys 
	  | merge xs      [] = xs
	  | merge (x::xs) (y::ys) =
	  if ordr(x, y) <> GREATER then x :: merge xs (y::ys)
	  else y :: merge (x::xs) ys
	fun mergepairs l1  []              k = [l1]
	  | mergepairs l1 (ls as (l2::lr)) k =
	  if k mod 2 = 1 then l1::ls
	  else mergepairs (merge l1 l2) lr (k div 2)
        fun nextrun run []      = (run, [])
          | nextrun run (xs as (x::xr)) =
	  if ordr(x, List.hd run) = LESS then (run, xs)
	  else nextrun (x::run) xr
        fun sorting []      ls r = List.hd(mergepairs [] ls 0)
          | sorting (x::xs) ls r =
	  let val (revrun, tail) = nextrun [x] xs
	  in sorting tail (mergepairs (List.rev revrun) ls (r+1)) (r+1) 
	  end
      in sorting xs [] 0 
      end
	
    fun sorted ordr [] = true
      | sorted ordr (y1 :: yr) = 
      let fun h x0 [] = true
	    | h x0 (x1::xr) = ordr(x0, x1) <> GREATER andalso h x1 xr
      in h y1 yr 
      end

  end (* structure ListSort *)
