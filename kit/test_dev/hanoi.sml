(*
fun digit n = chr(ord "0" + n)
fun digits(n,acc) =
      if n >=0 andalso n<=9 then digit n:: acc
      else digits (n div 10, digit(n mod 10) :: acc)

fun int_to_string(n) = implode(digits(n,[]))
*)

let
  infix +
  infix -
  infix <
  infix >

  fun out_str(s) = prim ("printString","printString",s)
  fun printNum(i:int):unit = prim ("printNum","printNum",i)

  fun neq(x,y) = if x<y then false else if x>y then false else true

  fun show_move(i,j) = 
    (out_str "move ";
     printNum i;
     out_str "to ";
     printNum j;
     out_str "\n ")

  fun hanoi(n, from, to, via)=
    if neq(n,0) then ()
    else (hanoi(n-1, from, via, to); 
	  show_move(from,to);
	  hanoi(n-1, via, to, from))
  val _ = out_str "Hello\n"
  val _ = hanoi(3,1,2,3);
  val _ = out_str "Hello again\n"
in
  ()
end
