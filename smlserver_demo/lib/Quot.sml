signature QUOT =
  sig
    (* Quotation support. *)
    type quot = string frag list
    val ^^ : quot * quot -> quot
    val fromString : string -> quot
    val toString : quot -> string 
    val size : quot -> int 
    val concat : quot list -> quot 
    val concatFn : (quot -> quot) -> quot list -> quot
    val concatWith : string -> quot list -> quot

    val implode : Char.char list -> quot
    val explode : quot -> Char.char list 
    val map : (Char.char -> Char.char) -> quot -> quot
    val translate : (Char.char -> string) -> quot -> quot
    val isPrefix : quot -> quot -> bool 
    val compare : (quot * quot) -> order 
    val < : (quot * quot) -> bool 
    val <= : (quot * quot) -> bool 
    val == : (quot * quot) -> bool (* Polymorphic equality is not meaningful on quotations! *)
    val > : (quot * quot) -> bool 
    val >= : (quot * quot) -> bool 

    val wrapString : (quot -> quot) -> (string -> string)
  end

structure Quot : QUOT =
  struct
    type quot = string frag list
    val op ^^ : quot * quot -> quot = op @
    fun toString (q : quot) : string =
      concat(map (fn QUOTE s => s | ANTIQUOTE s => s) q)
    fun fromString s = `^s`
    val size = String.size o toString
    fun concat qs = List.foldr (op ^^) `` qs
    fun concatFn f qs = (concat o List.map f) qs
    fun concatWith s qs = fromString (String.concatWith s (List.map toString qs))
    val implode = fromString o String.implode 
    val explode = String.explode o toString
    fun map f q = fromString(String.map f (toString q))
    fun translate f q = fromString(String.translate f (toString q))
    fun isPrefix q1 q2 = String.isPrefix (toString q1) (toString q2)
    local
      fun fixPair (q1,q2) = (toString q1, toString q2)
    in
      val compare = String.compare o fixPair
      val op < = op String.< o fixPair
      val op <= = op String.<= o fixPair
      val op == = op = o fixPair
      val op > = op String.> o fixPair
      val op >= = op String.>= o fixPair

      fun wrapString (f:quot -> quot): (string -> string) = toString o f o fromString 
    end
  end

type quot = Quot.quot
infixr 5 ^^
val op ^^ = Quot.^^

