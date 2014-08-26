(* File life.sml: Conway's Game of Life using lists.
 * Copyright (c) 1995-2014, University of Copenhagen, Martin Elsman.
 * MIT License.
 *)

local
  fun error str = raise Fail str
  
  fun accumulate f a [] = a
    | accumulate f a (b::x) = accumulate f (f a b) x
  
  fun accumulate' (f, a, []) = a
    | accumulate' (f, a, b::x) = accumulate'(f, f(a,b), x)

  fun member x a = List.exists (fn y => y = a) x
  
  fun revonto x y = accumulate' ((fn (x,y) => y::x), x, y)

  local
    fun check n = if n<0 then error "repeat<0" else n
  in
    fun repeat f x y = 
      let fun loop(p as (0,x)) = p
            | loop(n,x) = loop(n-1, f x)
      in
          #2(loop(check x, y))
      end
  end
  
  fun spaces 0 = nil
    | spaces n = "<td></td>" :: spaces (n-1)

  fun lexless(a2,b2)(a1:int,b1:int) = 
    if a2<a1 then true else if a2=a1 then b2<b1 else false
  
  local
    fun take(i,l) = 
        case l of 
            [] => []
         |  x::xs=> if i>0 then x::take(i-1,xs) else nil
    fun drop(i,l) = case l of
                        [] => []
                      | x::xs => if i>0 then drop(i-1,xs) else l
    fun merge(lp as (left, right)) =
        case left of 
            [] => right
          | x::xs => case right of
                         [] => left
                       | y::ys => if lexless x y then x::merge(xs, right)
                                  else if lexless y x then y:: merge(left,ys)
                                  else (*x=y*) merge(xs, right)
  in
  fun tmergesort l =
      case l of [] => []
              | x::xs => case xs of
                             [] => l 
                           | _ => let val k = length l div 2
                                  in merge(tmergesort(take(k,l)),
                                           tmergesort(drop(k,l)))
                                  end
  fun lexordset x = tmergesort x
  end

  fun collect f list =
      let fun accumf sofar [] = sofar
            | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
      in accumf [] list
      end
  
  fun occurs3 x = 
      (* finds coords which occur exactly 3 times in coordlist x *)
      let fun f (q) =
              case q of (_,_,_,_,[]) => q
                      | ( xover, x3, x2, x1, (a::x)) =>
                        if member xover a then f( xover, x3, x2, x1, x) else
                        if member x3 a then f ((a::xover), x3, x2, x1, x) else
  		        if member x2 a then f (xover, (a::x3), x2, x1, x) else
                        if member x1 a then f (xover, x3, (a::x2), x1, x) else
  		        f (xover, x3, x2, (a::x1), x)
          fun diff x y = List.filter (fn x => not(member y x)) x  (* unfolded o *)
          val (xover, x3, _, _, _) = f ([],[],[],[],x)
      in diff x3 xover
      end
          
  fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
  			  (i,j-1),(i,j+1),
  			  (i+1,j-1),(i+1,j),(i+1,j+1)]

  abstype generation = GEN of (int*int) list
  with 
    fun alive (GEN livecoords) = livecoords
    and mkgen coordlist = GEN (lexordset coordlist)
    and nextgen gen =
      let
        val living = alive gen
        fun isalive x = member living x
        fun liveneighbours x = length( List.filter isalive ( neighbours x))
        fun twoorthree n = n=2 orelse n=3
        val survivors = List.filter (twoorthree o liveneighbours) living
        val newnbrlist = collect (fn z => List.filter (fn x => not(isalive x)) 
                                                 (neighbours z)
                                 ) living
        val newborn = occurs3 newnbrlist
      in mkgen (survivors @ newborn)
      end
  end
  
  local 
    val xstart = 0 and ystart = 0
    fun markafter n string = string ^ concat(spaces n) ^ "<td>0</td>"
    fun plotfrom (x,y) (* current position *)
                 str   (* current line being prepared -- a string *)
                 ((x1:int,y1)::more)  (* coordinates to be plotted *)
        = if x=x1
          then (* same line so extend str and continue from y1+1 *)
               plotfrom(x,y1+1)(markafter(y1-y)str)more
          else (* flush current line and start a new line *)
               str :: plotfrom(x+1,ystart)""((x1,y1)::more)
      | plotfrom (x,y) str [] = [str]
    fun good (x,y) = x>=xstart andalso y>=ystart
  in  
    fun plot coordlist = plotfrom(xstart,ystart) "" 
                                 (List.filter good coordlist)
  end
  
  (* the initial generation *)
  fun gun() = mkgen         
       [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
        (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
        (6,22),(6,23),(6,26),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
        (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
        (9,29),(9,30),(9,31),(9,32)]
  
  fun pp_gen x = 
      let val board =  "<table>" ::
                       (List.foldl (fn (s,acc) => "<tr>" :: s :: "</tr>" :: acc) ["</table>"]
                                   (plot(alive x)))
      in String.concat board
      end

  open Js.Element infix &
  val boardElem = tag0 "div"
  val statusElem = tag "span" ($"starting")

  fun updStatus n (s:string) (k : unit -> unit) : unit =
      ( Js.innerHTML statusElem s
      ; Js.setTimeout n k ; () )

  fun loop (f : 's -> 's) (p:'s->bool) : 's -> unit =
      let val r : 's option ref = ref NONE
          fun run (s : 's) : unit =
              if p s then 
                ( r := SOME s
                ; updStatus 10 "waiting"
                            (fn () => updStatus 10 "computing"
                                                (fn () => run (f(valOf (!r)))))
                )
              else updStatus 0 "done" (fn () => ())
      in run
      end
  
  fun show g : unit =
      Js.innerHTML boardElem (pp_gen g)

  fun next g =
      let val g' = nextgen g
      in show g'; g'
      end

  fun runit g0 n = 
      loop (fn (i,g)=>(i+1,next g)) (fn (i,_) => i < n) g0

  val e = taga "p" [("style","width:500;height:500;")]
           (tag "h4" ($"Status: " & statusElem) & boardElem)
in
  val () = Dojo.runDialog "Game of Life" e
  val () = runit (0,gun()) 200
end
