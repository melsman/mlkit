

signature TYPELET = sig

  type 'a fl

  type 'a cell

  type 'a input
  type 'a group
  type button

  type nil

  type ('a,'b) access
  type ('a,'b) acc = unit -> ('a,'b) access

  val value   : ('a input -> 'b, 'a input) acc
  val enabled : ('a input -> 'b, bool input) acc
  val next    : ('b,'a) access -> ('c->'b,'a) access
  val down    : ('b,'a) access -> ('b group->'c,'a) access

  val getv    : 'a fl -> ('a,'b input) acc -> 'b
  val setv    : 'a fl -> ('a,'b input) acc -> 'b -> unit

  val textbox : string -> string input cell
  val boolbox : string -> bool input cell
  val intbox  : string -> int input cell
  val >>      : 'a cell * 'b fl -> ('a->'b) fl
  val $>      : 'a cell * 'b fl -> ('a->'b) fl
  val group   : string -> 'a fl -> 'a group cell 
  val emp     : nil fl

  val button  : string -> button cell

  type 'a rule
  type ('a,'b) fields
  val &        : ('a,'b)fields * ('a,'c)fields -> ('a,'b*'c)fields
  val hidden   : 'b -> ('a,'b)fields
  val one      : ('a,'b input)acc -> ('a,'b)fields
  val upd_rule : ('a,'b)fields -> ('a,'c)fields -> ('b->'c) -> 'a rule
  val but_rule : ('a,button)acc -> ('a,'b)fields -> ('a,'c)fields -> ('b->'c) -> 'a rule

  type 'a typelet = 'a fl * 'a rule list

end

functor Test(X : TYPELET) = struct

  open X
  infixr 5 >>

  (* Ex 1 *)
  val name = textbox "name"
  val age = intbox "age"
  val male = boolbox "male"

  type t = (string input -> int input -> bool input -> nil) fl

  val form : t = name >> age >> male >> emp

  fun pr (f:t) : string =
      let val nam = getv f ( value )
          val a = getv f ( next o value )
          val m = getv f ( next o next o value )
      in "{name:" ^ nam ^",age:"^Int.toString a^",male:" ^ Bool.toString m ^ "}"
      end

  (* Ex 2 *)
  val f1 = textbox "f1"
  val f2 = intbox "f2"
  val f3 = textbox "f3"

  type fg = (string input -> int input -> string input -> nil) group
  val g1 : fg cell = group "Group1" (f1 >> f2 >> f3 >> emp)

  val t1 = intbox "t1"
  val t2 = textbox "t2"

  type tg = (int input -> string input -> nil) group
  val g2 : tg cell = group "Group2" (t1 >> t2 >> emp)

  type m = (fg -> tg -> nil) group
  val main : (m->nil) fl = group "Main" (g1 >> g2 >> emp) >> emp

  val v1 : int = getv main ( down o next o down o value )
  val e1 : bool = getv main ( down o next o down o enabled )

  val p_f3 = down o next o next o value

  val v2 : string = getv main ( down o p_f3 )

  (* Ex 3 *)

  val t_c = intbox "celcius"
  val t_f = intbox "fahrenheit"
  type temp = (int input -> int input -> nil) group
  val box : (temp -> nil) fl = group "Temperature Conversion" (t_c >> t_f >> emp) >> emp

  val r = upd_rule (one (down o value)) (one(down o next o value)) (fn c => 5 * c div 9 + 32)

  val temp_tl : (temp -> nil) typelet = (box, [r])

end
