
structure F = Posix.FileSys
type fd = F.file_desc
fun input1 (fd:fd) : string =
    Byte.bytesToString(Posix.IO.readVec(fd,1))

fun writeln s = print(s^"\n")

fun percent (i: int, j: int) :int =
    floor((real i * 100.0)/real j)

local
  (* seek: (char -> bool) -> instream -> string list:
     seek(pred)(is) returns the list of characters obtained
     by reading from up to and including the first character
     which satisfies "pred". (If no such character exists, the
     empty list is returned.
     *)
  exception Impossible
  fun seek (pred: char -> bool) (fd: fd): char list =
    let fun readLoop () =
            case explode (input1 fd) of
                [] => []
	      | [c] => c :: (if pred c then []
			     else readLoop())
	      | _ => (print "lib.seek: impossible"; raise Impossible)
    in readLoop()
    end

  fun readLn fd = seek (fn c => c = #"\n") fd

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

  fun isSpace (ch:char) : bool =
      ch = #" " orelse ch = #"\t" orelse ch = #"\n"
      (* orelse ch= "\f" orelse ch = "\r" orelse ch = "\v"*)
in
  fun readWord (fd:fd) : string option =
      case implode (takewhile (not o isSpace)
		              (dropwhile isSpace (readLn fd)))
       of "" => NONE
        | s => SOME s
end
