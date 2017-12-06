(*List.sml*)

structure List : LIST = struct
  type 'a list = 'a list

  exception Empty

  fun null [] = true
    | null _ = false

  fun hd [] = raise Empty
    | hd (x::xr) = x

  fun tl [] = raise Empty
    | tl (x::xr) = xr

  fun last [] = raise Empty
    | last [x] = x
    | last (x::xr) = last xr

  fun nth (xs, n) =
    let fun h [] _ = raise Subscript
	  | h (x::xr) n = if n=0 then x else h xr (n-1)
    in if n<0 then raise Subscript else h xs n
    end

  fun drop (xs, n) =
    let fun h xs 0 = xs
	  | h [] n = raise Subscript
	  | h (x::xr) n = h xr (n-1)
    in if n<0 then raise Subscript else h xs n
    end

  fun take (xs, n) =
    let fun h xs 0 = []
	  | h [] n = raise Subscript
	  | h (x::xr) n = x :: h xr (n-1)
    in if n<0 then raise Subscript else h xs n
    end

  fun length xs =
    let fun acc [] k = k
	  | acc (x::xr) k = acc xr (k+1)
    in acc xs 0
    end

  local
    fun revAcc [] ys = ys
      | revAcc (x::xs) ys = revAcc xs (x::ys)
  in
    fun rev xs = revAcc xs []
    fun revAppend (xs, ys) = revAcc xs ys
  end

  fun xs @ ys =
    let fun loop nil acc = acc
          | loop (x::xs) acc = loop xs (x::acc)
    in loop (rev xs) ys
    end

(*
  fun nil @ ys = ys
    | (x::xs) @ ys = x :: (xs @ ys)
*)

  fun concat [] = []
    | concat (xs::xsr) = xs @ concat xsr

  fun app f [] = ()
    | app f (x::xr) = (f x; app f xr)

  fun map f xs =
    let fun map0 (nil, ys) = ys
          | map0 (x::xs, ys) = map0 (xs, f x :: ys)
    in rev (map0 (xs, nil))
    end

(*
  fun map f [] = []
    | map f (x::xs) = f x :: map f xs
*)

  fun mapPartial f xs =
    let fun aux a [] = a
          | aux a (x::xs) = case f x of
                                SOME v => aux (v::a) xs
                              | NONE => aux a xs
    in rev(aux [] xs)
    end

(*
  fun mapPartial f [] = []
    | mapPartial f (x::xr) =
      case f x of
          NONE => mapPartial f xr
	| SOME r => r :: mapPartial f xr
*)

  fun find p [] = NONE
    | find p (x::xr) = if p x then SOME x else find p xr

  fun filter p xs =
    let fun aux a [] = a
          | aux a (x::xs) = if p x then aux (x::a) xs
                            else aux a xs
    in rev(aux [] xs)
    end

(*
  fun filter p [] = []
    | filter p (x::xr) = if p x then x :: filter p xr else filter p xr
*)

  fun partition p xs =
    let fun h [] are aren't = (rev are, rev aren't)
	  | h (x::xr) are aren't = if p x then h xr (x::are) aren't
				   else h xr are (x::aren't)
    in h xs [] []
    end

  fun foldr f e [] = e
    | foldr f e (x::xr) = f(x, foldr f e xr)

(*
  fun foldr f =
    let fun foldr_loop(e,l) =
              case l of
                [] => e
              | x::xr => f(x,foldr_loop(e,xr))
    in fn e => fn l => foldr_loop(e,l)
    end
*)

  fun foldl f e [] = e
    | foldl f e (x::xr) = foldl f (f(x, e)) xr

(*
  fun foldl f b xs =
    let fun foldl_loop(p as ([], b))= p
          | foldl_loop(x::xs, b) = foldl_loop(xs,f(x, b))
    in
         #2(foldl_loop(xs,b))
    end
*)

  fun exists p [] = false
    | exists p (x::xr) = p x orelse exists p xr

  fun all p [] = true
    | all p (x::xr) = p x andalso all p xr

  fun tabulate (n, f) =
    let fun h (i, acc) =
            if i<n then h (i+1, f i :: acc)
            else acc
    in if n<0 then raise Size else rev(h(0, nil))
    end
(*
  fun tabulate (n, f) =
    let fun h i = if i<n then f i :: h (i+1) else []
    in if n<0 then raise Size else h 0
    end
*)

  fun getItem [] = NONE
    | getItem (x :: xr) = SOME (x, xr)

  fun collate f ([],[]) = EQUAL
    | collate f ([],_) = LESS
    | collate f (_,[]) = GREATER
    | collate f (x::xr, y::yr) =
      case f (x,y) of
          EQUAL => collate f (xr,yr)
        | GREATER => GREATER
        | LESS => LESS

end; (*structure List*)

fun null a = List.null a
val hd = List.hd
val tl = List.tl
fun length a = List.length a
fun rev a = List.rev a
fun op @ a = List.@ a
fun app f l = List.app f l
fun map f l = List.map f l
fun foldr a b c = List.foldr a b c
fun foldl a b c = List.foldl a b c
exception Empty = List.Empty
