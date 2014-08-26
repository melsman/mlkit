(* File war.sml: Simulation of the war card game
 * Copyright (c) 2014, Martin Elsman.
 * MIT License.
 *)

local val a = ref 1
in fun rand n =
     (a := (19 + (!a * 113)) mod 234421;
      !a mod n) (*Random.range (0,n) G*)
end

type player = {hand: int list, table: int list}

fun split 0 (c::cs) a = (a,c,cs)
  | split n (c::cs) a = split (n-1) cs (c::a)
  | split n nil a = raise Fail "split"

fun deal [] (a1,a2) = ({hand=a1,table=[]},{hand=a2,table=[]})
  | deal (c1::c2::cs) (a1, a2) = deal cs (c1::a1, c2::a2)
  | deal [c] (a1,a2) = deal [] (c::a1,a2)

fun shuffle ([],a) = a
  | shuffle (cs,a) =
    let val (cs1,c,cs2) = split (rand (length cs)) cs []
    in shuffle (cs1, shuffle (cs2, c::a))
    end

val color = [1,2,3,4,5,6,7,8,9,10,11,12,13]
val deck = color @ color @ color @ color

fun newdeck() = shuffle(deck,[])

fun pp cs = "[" ^ String.concatWith "," (map Int.toString cs) ^ "]"
fun pr cs = print (pp cs ^ "\n")

val () = pr (newdeck())

fun getCard {hand=[],table=[]} = NONE
  | getCard {hand=c::hand,table} = SOME(c,{hand=hand,table=table})
  | getCard {hand=[],table} = getCard {hand=shuffle(table,[]), table=[]}

fun getCards 0 p = SOME([],p)
  | getCards n p =
   case getCard p of
     SOME(c,p) =>
       (case getCards (n-1) p of
           SOME(cs,p) => SOME(c::cs,p)
         | NONE => NONE)
    | NONE => NONE

fun addCards (p:player) cs : player =
  {hand= #hand p, table= cs @ (#table p)}

fun game (n:int, pool:int list, p1:player, p2:player) : int * int =
   if n >= 1000 then (0,1000) else
   case (getCard p1, getCard p2) of
      (SOME(c1,p1),SOME(c2,p2)) =>
         if c1 > c2 then game (n+1,[],addCards p1 ([c1,c2]@pool),p2)
         else if c2 > c1 then game (n+1,[],p1,addCards p2 ([c1,c2]@pool))
         else (case (getCards 3 p1, getCards 3 p2) of
                  (SOME(cs1,p1),SOME(cs2,p2)) =>
                    game (n+3, cs1@cs2@pool, p1, p2)
                | (SOME _, NONE) => (1,n)
                | (NONE, SOME _) => (2,n)
                | (NONE, NONE) => raise Fail "game")
     | (SOME _, NONE) => (1,n)
     | (NONE, SOME _) => (2,n)
     | (NONE, NONE) => raise Fail "game2"

fun pp_g (p,n) = ("Winner: " ^ Int.toString p ^ " - plays: " ^ Int.toString n)
fun pr_g g = print (pp_g g ^ "\n")

fun runGame() =
  let val (p1,p2) = deal (newdeck()) ([],[])
  in game (0, [], p1, p2)
  end handle Fail s => (print ("Fail " ^ s ^ "\n"); (~1,0))

fun runGames 0 (turns,n) = print ("Average number of turns: " ^ 
                                  Int.toString (turns div n) ^ "\n")
  | runGames n (turns,m) =
  let val g = runGame()
  in pr_g g; runGames (n-1) (turns+ #2 g, m+1) 
  end

val () = runGames 200 (0,0)
