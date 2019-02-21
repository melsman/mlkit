(*------------------------------------------------------------------*
 * The professor game is about placing 16 carts with two professor  *
 * jackets and two professor trousers in different colors on a      *
 * square board.                                                    *
 * This program finds all solution to a givet set of carts.         *
 * 27/12/1995, Niels Hallenberg                                     *
 *------------------------------------------------------------------*)

(*---------------*
 * AUX functions *
 *---------------*)

fun output s = print s
fun die s = (output("Professor_game - DIE with message: " ^ s);
	     raise Fail "die")
fun log s = output("Professor_game - LOG with message: " ^ s ^ "\n")

  fun map f [] = []
    | map f (x::xs) = f x :: map f xs

  fun size xs =
    let fun acc []      k = k
	    | acc (x::xr) k = acc xr (k+1)
    in acc xs 0
    end

  fun digit n = chr(ord #"0" + n)
  fun digits(n,acc) =
    if n >=0 andalso n<=9 then digit n:: acc
    else digits (n div 10, digit(n mod 10) :: acc)

  fun int_to_string(n) = if n >= 0 then implode(digits(n,[]))
			 else "~" ^ int_to_string(~n)

val debug_flag = false
fun debug s = if debug_flag then
                log s
	      else
		()

fun min (n:int) (m:int) =
  if n < m then
    n
  else
    m

(*-------------------------------------------*
 * Datatypes and simple operations.          *
 *-------------------------------------------*)

datatype clothes = RED_JACKET
                 | RED_TROUSERS
                 | GREEN_JACKET
                 | GREEN_TROUSERS
                 | BLUE_JACKET
                 | BLUE_TROUSERS
                 | BROWN_JACKET
                 | BROWN_TROUSERS

type cart = {top:clothes,
	     bot:clothes,
	     left:clothes,
	     right:clothes}

type board = int*int*(cart list)

val emptyBoard = (0, 0, [])
val colNo = 4 (* number of columns on board. *)
val rowNo = 4 (* number of rows on board.    *)

fun findBot ({bot=bot, ...}:cart) = bot
fun findTop ({top=top, ...}:cart) = top
fun findLeft ({left=left, ...}:cart) = left
fun findRight ({right=right, ...}:cart) = right

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

fun pp_newline () = output("\n")
fun pp_board_line () = output(
     "+--------------------------------++--------------------------------++--------------------------------++--------------------------------+\n")
fun pp_vertical xs = (map (fn clothe => output( "|         " ^ (pp_clothes clothe) ^ "         |")) xs;
		      pp_newline())
fun pp_horizontal xs = (map (fn (l, r) => output( "|" ^ (pp_clothes l) ^ "    " ^ (pp_clothes r) ^ "|")) xs;
			pp_newline())
fun pp_row carts =
  let
    val tops = map findTop carts
    val bots = map findBot carts
    val lefts = map findLeft carts
    val rights = map findRight carts
    fun splice [] [] = []
      | splice (l::lefts) (r::rights)= (l,r) :: (splice lefts rights)
      | splice _ _ = die "pp_row.splice"
    val centerRow = splice lefts rights
  in
    (pp_board_line();
     pp_vertical tops;
     pp_horizontal centerRow;
     pp_vertical bots;
     pp_board_line ())
  end


exception Sub'

    fun count (x::_,  0) = x
    |   count (_::xs, n) = count (xs, n - 1)
    |   count ([], n) = raise Sub'
    infix 9 sub
    fun l sub n =
          if n < 0 then raise Subscript
          else count (l, n)
	  handle Sub' => raise Subscript

  fun nth n l = l sub n
		handle Subscript => raise Subscript

    fun split (l, 0) = ([], l)
    |   split (x::xs, m) =
	  let val (l, l') = split (xs, m - 1)
	   in (x :: l, l')
	  end
    |   split ([], _) = raise Sub'

    fun splitNth n l =
	  if n < 0 then raise Subscript
          else split (l, n)
	  handle Sub' => raise Subscript

fun pp_carts [] = ()
  | pp_carts carts =
      let
	val (left, right) = splitNth (min colNo (size carts)) carts
      in
	(pp_row left;
	 pp_carts right)
      end

fun pp_board (board as (row, col, carts)) = (output("New board\n");
					     pp_carts carts;
					     pp_newline())

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

fun matchTop (row, col, carts) cart =
  (if row > 0 then
     let
       val topCart = nth ((findPlaceInList (row, col))-colNo) carts
     in
       matchClothes (findBot topCart) (findTop cart)
     end
   else
     true) handle Subscript => die ("matchTop ")

fun matchBot (row, col, carts) cart =
  (if row < rowNo-1 then
     let
       val botCart = nth ((findPlaceInList(row, col))+colNo) carts
     in
       matchClothes (findBot cart) (findTop botCart)
     end
   else
     true) handle Subscript => die ("matchBot ")

fun matchLeft (row, col, carts) cart =
  (if col > 0 then
     let
       val leftCart = nth ((findPlaceInList(row, col))-1) carts
       val _ = debug ("matchLeft with leftcart: " ^
		      (pp_clothes (findRight leftCart)) ^
		      " and rightcart: " ^
		      (pp_clothes (findLeft cart)))
     in
       matchClothes (findRight leftCart) (findLeft cart)
     end
   else
     true) handle Subscript  =>
                    die ("matchLeft with error "  ^ " and size list " ^
			 (int_to_string (size carts)) ^ ",row " ^ (int_to_string row) ^
			 ", col " ^ (int_to_string col) ^ " and findPlaceInList " ^
			 (int_to_string (findPlaceInList(row, col))))

fun matchRight (row, col, carts) cart =
  (if col < colNo-1 then
     let
       val rightCart = nth ((findPlaceInList (row, col))+1) carts
     in
       matchClothes (findRight cart) (findLeft rightCart)
     end
   else
     true) handle Subscript  => die ("matchRight ")

fun match (board as (row, col, carts)) cart =
  (matchTop board cart) andalso	(matchLeft board cart)

fun findSol [] [] board sols = (log "solution found";
				board::sols)
  | findSol [] _ (board as (row, col, carts)) sols = (debug ("skipping board with (row, col)=(" ^ (int_to_string row) ^ "," ^ (int_to_string col) ^ ")");
						      sols)
  | findSol (x::rest) alreadyTried (board as (row, col, carts)) sols =
      let
	val _ = debug "In findSol"
	val sols' =
	  if match board x then
	    let
	      val _ = debug "findSol got a match"
	      val (row', col') = add 1 (row, col)
	    in
	      findSol(rest@alreadyTried) [] (row', col', carts@[x]) sols
	    end
	  else
	    sols
      in
	findSol rest (x::alreadyTried) board sols'
      end

(*----------------------------*
 * Generating a board.        *
 *----------------------------*)

val cartSet = [{top= BLUE_TROUSERS, bot=BROWN_JACKET, left= BLUE_JACKET, right=BROWN_TROUSERS},
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

fun doit() =
    let val res = findSol cartSet [] emptyBoard []
    in pp_boards res
    end

fun repeatit n = if n <= 0 then ()
                 else (doit(); repeatit(n-1))

val () = repeatit 10
