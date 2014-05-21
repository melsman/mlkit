(*
project prof-6 compiled with profiling turned on with the kit
release on april 30 gives a binary (run) that gives a `bus
error' when run on ask or thor.  This may have something to do
with the extreme memory consumption of the program.  God
fornøjelse.
*)

(*------------------------------------------------------------------*
 * The professor game is about placing 16 cards with two professor  *
 * jackets and two professor trousers in different colors on a      *
 * square board.                                                    *
 * This program finds all solutions to a given set of cards.        *
 * 27/12/1995, Niels Hallenberg                                     *
 *------------------------------------------------------------------*)

let

(*---------------------------------------------------*
 * List operations taken from the Edinburgh library. *
 *---------------------------------------------------*)

infix 9 sub
exception ListSubscript of string * int

local
  exception ListSub'
  fun count (x::_,  0) = x
    |   count (_::xs, n) = count (xs, n - 1)
    |   count ([], n) = raise ListSub'
in
  fun l sub n =
    if n < 0 then raise ListSubscript ("sub", n)
    else count (l, n)
      handle ListSub' => raise ListSubscript ("sub", n)
end

local
  fun split (l, 0) = ([], l)
    |   split (x::xs, m) =
          let val (l, l') = split (xs, m - 1)
	  in (x :: l, l')
	  end
    |   split ([], _) = raise ListSub'
in
  fun ListSplitNth n l =
    if n < 0 then raise ListSubscript ("splitNth", n)
    else split (l, n)
      handle ListSub' => raise ListSubscript ("splitNth", n)
end

fun ListNth n l = l sub n
  handle ListSubscript _ => raise ListSubscript ("nth", n)

(*---------------*
 * AUX functions *
 *---------------*)

exception Die
fun pr s = print s
fun die s = (pr ("Professor_game - DIE with message: " ^ s);
	     raise Die)

fun log s = print ("Professor_game - LOG with message: " ^ s ^ "\n")

val debug_flag = false
fun debug s = if debug_flag then log s
	      else ()

(*-------------------------------------------*
 * Datatypes and simple operations.          *
 *-------------------------------------------*)

datatype clothes = RED_JACKET       (*10*)
                 | RED_TROUSERS     (*2*)
                 | GREEN_JACKET     (*26*)
                 | GREEN_TROUSERS   (*18*)
                 | BLUE_JACKET      (*58*)
                 | BLUE_TROUSERS    (*50*)
                 | BROWN_JACKET     (*42*)
                 | BROWN_TROUSERS   (*34*)

type card = {top:clothes,
	     bot:clothes,
	     left:clothes,
	     right:clothes}

fun copy_card {top:clothes, bot:clothes, left:clothes, right:clothes} =
      {top=top, bot=bot, left=left, right=right}

type board = int * int * card list

(*KILL 06/08/1997 15:10. tho.:
fun copy (x::xs) = x::copy xs
  | copy [] = []
*)

fun copy_board (row, col, cards) = (row, col, map copy_card cards)

val emptyBoard = (0, 0, [])
val colNo = 4 (* number of columns on board. *)
val rowNo = 4 (* number of rows on board.    *)

fun findBot ({bot=bot, ...}:card) = bot
fun findTop ({top=top, ...}:card) = top
fun findLeft ({left=left, ...}:card) = left
fun findRight ({right=right, ...}:card) = right

fun findPlaceInList (row, col) = row*colNo+col

fun add n (row, col) =
      let
	val row' = (row*colNo+n+col) div colNo
	val col' = (row*colNo+n+col) mod colNo
      in
	(row', col')
      end

(*--------------------------*
 * PrettyPrinting           *
 *--------------------------*)

fun pp_clothes RED_JACKET     = "RED_JACKET    " 
  | pp_clothes RED_TROUSERS   = "RED_TROUSERS  "
  | pp_clothes GREEN_JACKET   = "GREEN_JACKET  "
  | pp_clothes GREEN_TROUSERS = "GREEN_TROUSERS"
  | pp_clothes BLUE_JACKET    = "BLUE_JACKET   "
  | pp_clothes BLUE_TROUSERS  = "BLUE_TROUSERS "
  | pp_clothes BROWN_JACKET   = "BROWN_JACKET  "
  | pp_clothes BROWN_TROUSERS = "BROWN_TROUSERS"

fun pp_newline () = pr "\n"
fun pp_board_line () = pr
     "+--------------------------------++--------------------------------++--------------------------------++--------------------------------+\n"
fun pp_vertical xs = (map
		      (fn clothe => pr ("|         " ^ (pp_clothes clothe) ^ "         |"))
		       xs;
		      pp_newline ())
fun pp_horizontal xs = (map
			(fn (l, r) => pr ("|" ^ (pp_clothes l) ^ "    " ^ (pp_clothes r) ^ "|"))
			xs;
			pp_newline ())
fun pp_row cards =
      let
	val tops = map findTop cards
	val bots = map findBot cards
	val lefts = map findLeft cards
	val rights = map findRight cards
	fun splice [] [] = []
	  | splice (l::lefts) (r::rights)= (l,r) :: (splice lefts rights)
	  | splice _ _ = die "pp_row.splice"
	val centerRow = splice lefts rights
      in
	(pp_board_line ();
	 pp_vertical tops;
	 pp_horizontal centerRow;
	 pp_vertical bots;
	 pp_board_line ())
      end
  
fun pp_cards [] = ()
  | pp_cards cards = 
      let
	val (left, right) = ListSplitNth (Int.min (colNo, length cards)) cards
      in
	(pp_row left;
	 pp_cards right)
      end
  
fun pp_board (board as (row, col, cards)) = (pr "New board\n";
					     pp_cards cards;
					     pp_newline ())

fun pp_boards boards = map pp_board boards

(*---------------*
 * Solving board *
 *---------------*)

fun matchClothes RED_JACKET     RED_TROUSERS   = true
  | matchClothes RED_TROUSERS   RED_JACKET     = true
  | matchClothes GREEN_JACKET   GREEN_TROUSERS = true
  | matchClothes GREEN_TROUSERS GREEN_JACKET   = true
  | matchClothes BLUE_JACKET    BLUE_TROUSERS  = true
  | matchClothes BLUE_TROUSERS  BLUE_JACKET    = true
  | matchClothes BROWN_JACKET   BROWN_TROUSERS = true
  | matchClothes BROWN_TROUSERS BROWN_JACKET   = true
  | matchClothes _              _              = false

fun matchTop (row, col, cards) card =
  (if row > 0 then
     let
       val topCard = ListNth ((findPlaceInList (row, col))-colNo) cards
     in
       matchClothes (findBot topCard) (findTop card)
     end
   else
     true) handle ListSubscript ("nth", n) => die ("matchTop " ^ (Int.toString n))

fun matchBot (row, col, cards) card =
  (if row < rowNo-1 then
     let
       val botCard = ListNth ((findPlaceInList(row, col))+colNo) cards
     in
       matchClothes (findBot card) (findTop botCard)
     end
   else
     true) handle ListSubscript ("nth", n) => die ("matchBot " ^ (Int.toString n))

fun matchLeft (row, col, cards) card =
  (if col > 0 then
     let
       val leftCard = ListNth ((findPlaceInList(row, col))-1) cards
       (*val _ = debug ("matchLeft with leftcard: " ^ 
		      (pp_clothes (findRight leftCard)) ^ 
		      " and rightcard: " ^
		      (pp_clothes (findLeft card)))*)
     in
       matchClothes (findRight leftCard) (findLeft card)
     end
   else
     true) handle ListSubscript ("nth", n) => 
                    die ("matchLeft with error " ^ (Int.toString n) ^ " and size list " ^ 
			 (Int.toString (length cards)) ^ ",row " ^ (Int.toString row) ^ 
			 ", col " ^ (Int.toString col) ^ " and findPlaceInList " ^
			 (Int.toString (findPlaceInList(row, col))))

fun matchRight (row, col, cards) card =
  (if col < colNo-1 then
     let
       val rightCard = ListNth ((findPlaceInList (row, col))+1) cards
     in
       matchClothes (findRight card) (findLeft rightCard)
     end
   else
     true) handle ListSubscript ("nth", n) => die ("matchRight " ^ (Int.toString n))

fun match (board as (row, col, cards)) card =
  let
    val m = (matchTop board card) andalso (matchLeft board card)
  in
    m
  end


fun findSol ([],[],board,sols) = (log "solution found";
				(board)::sols)
  | findSol ([],_,(board as (row, col, cards)),sols) =
      (debug ("skipping board with (row, col)=(" ^ Int.toString row ^ "," ^ Int.toString col ^ ")");
       sols)
  | findSol ((x::rest),alreadyTried,(row, col, cards),sols) =
      let
	val cards' = map copy_card cards
	val x' = copy_card x
	val _ = resetRegions cards
	val sols' = 
	  if match (row, col, cards') x' then
	    let
	      val _ = debug "findSol got a match"
	      val (row', col') = add 1 (row, col)
	    in
	      findSol((rest@alreadyTried),[],(row', col', cards' @ [x']),sols)
	    end
	  else
	    sols
      in
	findSol (rest,(x::alreadyTried),(row,col, cards'),sols')
      end

(*----------------------------*
 * Generating a board.        *
 *----------------------------*)

val cardSet = [{top= BLUE_TROUSERS, bot=BROWN_JACKET, left= BLUE_JACKET, right=BROWN_TROUSERS},
	       {top=BROWN_TROUSERS, bot=GREEN_JACKET, left=BROWN_JACKET, right=  RED_TROUSERS},
	       {top=GREEN_TROUSERS, bot=GREEN_JACKET, left=  RED_JACKET, right= BLUE_TROUSERS},
	       {top=BROWN_TROUSERS, bot=BROWN_JACKET, left= BLUE_JACKET, right=  RED_TROUSERS},
	       {top=BROWN_TROUSERS, bot=GREEN_JACKET, left= BLUE_JACKET, right=  RED_TROUSERS},
	       {top=GREEN_TROUSERS, bot=BROWN_JACKET, left=  RED_JACKET, right= BLUE_TROUSERS},
	       {top=GREEN_TROUSERS, bot=BROWN_JACKET, left= BLUE_JACKET, right=  RED_TROUSERS},
	       {top=BROWN_TROUSERS, bot= BLUE_JACKET, left=  RED_JACKET, right=GREEN_TROUSERS},
	       {top=GREEN_TROUSERS, bot=BROWN_JACKET, left=  RED_JACKET, right=GREEN_TROUSERS},
	       {top=BROWN_TROUSERS, bot=GREEN_JACKET, left=GREEN_JACKET, right= BLUE_TROUSERS},
	       {top=BROWN_TROUSERS, bot=GREEN_JACKET, left= BLUE_JACKET, right=  RED_TROUSERS},
	       {top= BLUE_TROUSERS, bot=BROWN_JACKET, left=  RED_JACKET, right=GREEN_TROUSERS},
	       {top=BROWN_TROUSERS, bot=  RED_JACKET, left=GREEN_JACKET, right= BLUE_TROUSERS},
	       {top=GREEN_TROUSERS, bot=  RED_JACKET, left= BLUE_JACKET, right=BROWN_TROUSERS},
	       {top=GREEN_TROUSERS, bot= BLUE_JACKET, left=BROWN_JACKET, right=  RED_TROUSERS},
	       {top=BROWN_TROUSERS, bot=GREEN_JACKET, left=  RED_JACKET, right=GREEN_TROUSERS}]
in
  case findSol (cardSet,[],emptyBoard,[]) of
    [] => log "No results\n"
  | res => (pp_boards res; ())
end
