(*fft.sml*)

(*by Torben Mogensen (torbenm@diku.dk)*)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

  fun !(x: 'a ref): 'a = prim ("!", "!", x) 
  fun (x: 'a ref) := (y: 'a): unit = prim (":=", ":=", (x, y)) 

  fun op = (x: ''a, y: ''a): bool = prim ("=", "=", (x, y))

  fun not true = false
    | not false = true

  fun a <> b = not (a = b)

  fun print (s:string) : unit = prim("printString", "printString", s)
  fun printReal (r:real) : unit = prim("printReal", "printReal", r)
    fun real (x : int) : real = prim ("realInt", "realInt", x)
    fun floor (x : real) : int = prim ("floorFloat", "floorFloat", x)    (* may raise Overflow *)
    fun ceil (x : real) : int = prim ("ceilFloat", "ceilFloat", x)       (* may raise Overflow *)
    fun trunc (x : real) : int = prim ("truncFloat", "truncFloat", x)    (* may raise Overflow *)

  val op mod : (int * int) -> int = op mod 
fun (a:real) / (b:real) : real = prim ("divFloat", "divFloat", (a,b))
fun implode (chars : char list) : string = prim ("implodeChars", "implodeCharsProfiling", chars)
fun concat (ss : string list) : string = prim ("implodeString", "implodeStringProfiling", ss)
fun (s : string) ^ (s' : string) : string = prim ("concatString", "concatStringProfiling", (s, s'))
fun str (c : char) : string = implode [c]
fun size (s:string): int = prim ("sizeString", "sizeString", s)

  local
    fun append [] ys = ys
	| append (x::xs) ys = x :: append xs ys
  in
    fun xs @ ys = append xs ys
  end

val pi = 3.14159265358979

fun pr (s : string) : unit = print(s)
exception Impossible
fun impossible s = impossible0 (s ^ "\n")
and impossible0 s = (pr ("\nimpossible: " ^ s); raise Impossible)

fun zipWith f ([],[]) = []
  | zipWith f ((a::b),(c::d)) = f (a,c) :: zipWith f (b,d)
  | zipWith f _ = impossible "zipWith"

fun zip ([],[]) = []
  | zip ((a::b),(c::d)) = (a,c) :: zip (b,d)
  | zip _ = impossible "zip"

fun ~+ ((x:real,y:real),(v,w)) = (x+v,y+w)

fun ~- ((x:real,y:real),(v,w)) = (x-v,y-w)

fun ~* ((x:real,y:real),(v,w)) = (x*v-y*w,x*w+y*v)

fun evens [] = []
  | evens (x::y::l) = x :: evens l
  | evens _ = impossible "evens"

fun odds [] = []
  | odds (x::y::l) = y :: odds l
  | odds _ = impossible "odds"

  fun cos (r : real) : real = prim ("cosFloat", "cosFloat", r)
  fun sin (r : real) : real = prim ("sinFloat", "sinFloat", r)
fun fmul (c,pin,[]) = []
  | fmul (c,pin,(a::b))
  =  ~*((cos(c),sin(c)), a) :: fmul (c+pin,pin,b)

fun cp [] = []
  | cp (a::b) = a :: cp b

fun fft ([(a,b)], 1)  = [(a+0.0,b+0.0)]
  | fft (x, n2)
  = let val n = n2 div 2
        val a = fft (evens x, n)
        val cb = fmul (0.0,pi/(real n),fft (odds x, n))
    in
	let val l1 =  zipWith ~+ (a,cb)
	    val l2 =  zipWith ~- (a,cb)
	in resetRegions a; resetRegions cb; l1 @ l2
	end
    end

local val a = 16807.0 and m = 2147483678.0
in
   fun nextrand seed = 
         let val t = a*seed
         in t - m * real(floor (t/m)) end
end


fun mkList(tr as (seed,0,acc)) = tr
  | mkList(seed,n,acc) = mkList(nextrand seed, n-1, seed::acc)

val n = 256 * 256

fun run () = (pr "\nfft by Torben Mogensen (torbenm@diku.dk)\n\nfft'ing... ";
	      let val r = fft (zip (#3(mkList(7.0,n,[])),
				    #3(mkList(8.0,n,[]))), n) in
	      pr " done\n" end);

run ()
