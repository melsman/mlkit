fun upto n =
    let fun loop (n,acc) = if n=0 then acc
			   else loop(n-1, n::acc)
    in loop(n,[])
    end

fun nlength [] = 0
  | nlength (_::xs) = 1 + nlength xs

fun tlength l =
    let fun tlength' (nil, acc) = acc
	  | tlength' (_::xs, acc) = tlength'(xs,acc+1)
    in tlength'(l,0)
    end

fun klength l =
    let fun loop (p as ([], acc)) = p
	  | loop (_::xs, acc) = loop(xs,acc+1)
    in #2(loop(l,0))
    end

local
  fun llength' (p as ([], acc)) = p
    | llength' (_::xs, acc) = llength'(xs,acc+1)
in
  fun llength l = #2(llength'(l, 0))
end

fun global (p as ([], acc)) = p
  | global (_::xs, acc) = global(xs, acc+1)

fun glength l = #2(global(l, 0))

(* command-line arguments *)
fun findNext k xs =
    let fun loop (y::(xs as v::_)) =
            if k=y then SOME v
            else loop xs
          | loop _ = NONE
    in loop xs
    end

fun usage () =
    print ("Usage: length-cmd -n N -f [nlength|tlength|klength|llength|glength|upto|all]\n")

fun die s =
    ( print ("***ERROR: " ^ s ^ "\n")
    ; usage ()
    ; raise Fail "ERROR")

val () =
    let val args = CommandLine.arguments()
        val n = case findNext "-n" args of
                    SOME n_str =>
                    (case Int.fromString n_str of
                         SOME n => n
                       | NONE => die "expecting integer after -n")
                  | NONE => 5000000
        fun all n = nlength(upto n) +
                    tlength(upto n) +
                    klength(upto n) +
                    llength(upto n) +
                    glength(upto n)
        val f : int -> int =
            case findNext "-f" args of
                SOME "nlength" => nlength o upto
              | SOME "tlength" => tlength o upto
              | SOME "klength" => klength o upto
              | SOME "llength" => llength o upto
              | SOME "glength" => glength o upto
              | SOME "upto" => (fn n => (upto n; n))
              | SOME "all" => all
              | SOME _ => die "non-supported argument to -f"
              | NONE => all
    in print ("Result: " ^ Int.toString (f n) ^ "\n")
    end
