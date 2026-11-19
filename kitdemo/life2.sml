(* Game of Life with copying and flattenable coordinate lists.
 * Deconstruct coordinate pairs before using their components and reconstruct
 * them directly at cons allocations. Concrete helper types keep the layout
 * consistent without changing the copying and region-reset discipline. *)

local
  fun length ([] : (int * int) list) = 0
    | length (_::xs) = 1 + length xs

  fun filter pred (xs : (int * int) list) =
    let fun loop [] = []
          | loop ((x,y)::rest) =
              if pred x y then (x,y)::loop rest else loop rest
    in loop xs
    end

  fun member ([] : (int * int) list) _ _ = false
    | member ((x,y)::rest) a b =
        (x=a andalso y=b) orelse member rest a b

  fun revonto (acc : (int * int) list) [] = acc
    | revonto acc ((x,y)::rest) = revonto ((x,y)::acc) rest

  fun append ([] : (int * int) list) ys = ys
    | append ((x,y)::rest) ys = (x,y)::append rest ys

  fun spaces n = CharVector.tabulate (n,fn _ => #" ")

  fun cp_list ([] : (int * int) list) = []
    | cp_list ((x,y)::rest) =
      let val l = cp_list rest
      in (x,y)::l
      end

  local
    fun lexless a2 b2 (a1:int) (b1:int) =
        a2 < a1 orelse (a2=a1 andalso b2<b1)

    fun merge (xs, []) = cp_list xs
      | merge ([], ys) = cp_list ys
      | merge (l1 as (a,b)::xs, l2 as (c,d)::ys) =
          if lexless a b c d then (a,b)::merge(xs, l2)
          else (c,d)::merge(l1, ys)

    fun split ((a,b)::(c,d)::zs, l, r) =
          split(zs, (a,b)::l, (c,d)::r)
      | split ([(a,b)], l, r) = ((a,b)::l, r)
      | split ([], l, r) = (l, r)

    fun msort [] = []
      | msort [(a,b)] = [(a,b)]
      | msort xs =
          let val (l,r) = split(xs, [], [])
          in merge(msort l, msort r)
          end
  in
    fun lexordset x = msort x
  end

  fun collect f (xs : (int * int) list) =
    let fun accumf sofar [] = sofar
          | accumf sofar ((a,b)::rest) =
              accumf (revonto sofar (f a b)) rest
    in accumf [] xs
    end

  fun occurs3 x =
    let fun f q =
          case q of
              (_,_,_,_,[]) => q
            | (xover, x3, x2, x1, (a,b)::rest) =>
                if member xover a b then f(xover, x3, x2, x1, rest) else
                if member x3 a b then f((a,b)::xover, x3, x2, x1, rest) else
                if member x2 a b then f(xover, (a,b)::x3, x2, x1, rest) else
                if member x1 a b then f(xover, x3, (a,b)::x2, x1, rest) else
                f(xover, x3, x2, (a,b)::x1, rest)
        fun diff xs ys = filter (fn a => fn b => not(member ys a b)) xs
        val (xover, x3, _, _, _) = f ([],[],[],[],x)
    in diff x3 xover
    end

  fun neighbours i j = [(i-1,j-1),(i-1,j),(i-1,j+1),
                         (i,j-1),(i,j+1),
                         (i+1,j-1),(i+1,j),(i+1,j+1)]

  abstype generation = GEN of (int * int) list
  with
    fun copy (GEN l) = GEN(cp_list l)
    fun alive (GEN livecoords) = livecoords
    fun mkgen coordlist = GEN (lexordset coordlist)
    fun nextgen gen =
      let val living = alive gen
          fun isalive a b = member living a b
          fun liveneighbours a b = length(filter isalive (neighbours a b))
          fun twoorthree n = n=2 orelse n=3
          val survivors = filter (fn a => fn b => twoorthree(liveneighbours a b)) living
          val newnbrlist = collect
              (fn a => fn b => filter (fn x => fn y => not(isalive x y))
                                     (neighbours a b)) living
          val newborn = occurs3 newnbrlist
      in mkgen (cp_list(append survivors newborn))
      end
  end

  local
    val xstart = 0 and ystart = 0
    fun markafter n string = string ^ spaces n ^ "0"
    fun plotfrom (x,y) (* current position *)
                 str   (* current line being prepared -- a string *)
                 ((x1:int,y1)::more)  (* coordinates to be plotted *)
        = if x=x1
          then (* same line so extend str and continue from y1+1 *)
            plotfrom(x,y1+1)(markafter(y1-y)str)more
          else (* flush current line and start a new line *)
            str :: plotfrom(x+1,ystart)""((x1,y1)::more)
      | plotfrom (x,y) str [] = [str]
    fun good x y = x>=xstart andalso y>=ystart
  in
    fun plot coordlist = plotfrom (xstart,ystart) ""
                                  (filter good coordlist)
  end

  (* the initial generation *)

  fun gun () = mkgen
       [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
        (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
        (6,22),(6,23),(6,28),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
        (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
        (9,29),(9,30),(9,31),(9,32)]

  fun show x = app (fn s => (print s; print "\n")) (plot(alive x));

  local
    fun nthgen (p as (0,g)) = p
      | nthgen (p as (i,g)) =
          nthgen (i-1, let val g' = nextgen g
                       in  show g;
                           resetRegions g;  (* resetRegions g can actually be omitted here, since *)
                           copy g'          (* copy will reset the regions of g!                  *)
                       end)
  in
    fun iter n = #2(nthgen(n,gun()))
  end

  fun testit _ = show(iter 200)

in
  val _ = testit ()
end
