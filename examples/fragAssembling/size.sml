structure Size :> SIZE =
struct
  type 'a sz = 'a -> int

  val word_size = 8  (* bytes, 64-bit *)

  (* A heap object has one header word, then its fields.
     Unboxed integers/bools/chars take no heap space.
     real takes two words, one for the header and one for the content, when GC is enabled.
     real takes one word when GC is disabled
     Strings: header + length word + ceil(n/8) data words.
     Cons cell: hd pointer + tl pointer = 2 words.
     Tuple/record of k < 4 fields: k words 
     Tuple/record of k > 3 fields: header + k words. *)

  val int    = fn _ => 0
  val bool   = fn _ => 0
  val char   = fn _ => 0
  val word   = fn _ => 0
  (* val real   = fn _ => 2 * word_size (* with GC enabled *) *)
  val real   = fn _ => word_size (* with GC disabled *)
  val unit   = fn _ => 0

  (* header word + length word + data rounded up to word boundary *)
  fun string s =
    let val n = String.size s
    in  word_size           (* header *)
      + (n + word_size - 1) div word_size * word_size  (* data *)
    end

  (* nil = 0 words; each cons cell = hd + tl = 2 words - with GC disabled *)
  fun list sa xs =
    List.foldl (fn (x, acc) => acc + 2 * word_size + sa x) 0 xs

  (* NONE = one word (tag); SOME = header + one field = 2 words + content *)
  fun option sa opt =
    case opt of
      NONE   => word_size
    | SOME x => 2 * word_size + sa x

  (* tuple: 2 fields = 2 words overhead + content of each field *)
  fun tup2 sa sb (a, b) =
	2 * word_size + sa a + sb b	

  (* triple: 3 fields = 3 words overhead + content of each field *)
  fun tup3 sa sb sc (a, b, c) =
    3 * word_size + sa a + sb b + sc c


  fun size f x = f x
end
