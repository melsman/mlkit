datatype 'a Option = None | Some of 'a 

fun digit n = chr(ord "0" + n)
fun digits(n,acc) =
      if n >=0 andalso n<=9 then digit n:: acc
      else digits (n div 10, digit(n mod 10) :: acc)

fun toString(n) = implode(digits(n,[]))

fun writeln s = output(std_out, s^"\n")

fun percent(i: int, j: int)(*:int*) = 
     floor((real i * 100.0)/real j) 

(* seek: (string -> bool) -> instream -> string list:
   seek(pred)(is) returns the list of characters obtained
   by reading from up to and including the first character
   which satisfies "pred". (If no such character exists, the
   empty list is returned.
*)

fun seek (pred: string -> bool) (is: instream): string list = 
  let fun readLoop() =
            case input(is, 1) of
              ""  =>  []
            | s => s :: (if pred s then  []
                        else readLoop())
  in readLoop()
  end

fun readLn(is) = seek(fn ch => ch="\n")is

(* dropwhile: ('a -> bool) -> 'a list -> 'a list; endomorphic *)
fun dropwhile pred l =
  let fun drop_loop (l as []) = l
        | drop_loop (l as x::xs) =
           if pred x then drop_loop xs else l
  in drop_loop l
  end

(* takewhile: ('a -> bool) -> 'a list -> 'a list; exomorphic *)
fun takewhile pred l = 
  let fun take_loop [] = []
        | take_loop (l as x::xs) =
           if pred x then x:: take_loop xs else []
  in take_loop l
  end

fun isSpace(ch:string) = 
      ch = " " orelse ch = "\t" orelse ch = "\n" 
      (* orelse ch= "\f" orelse ch = "\r" orelse ch = "\v"*)

fun readWord(is): string Option =
  case
    implode(takewhile(not o isSpace) 
            (dropwhile isSpace (readLn is)))
  of "" => None
   | word => Some word

        