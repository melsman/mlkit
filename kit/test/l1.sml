let
  infixr 5  :: @
  fun print (s:string) : unit = prim("printString", "printString", s)
  datatype l = 
    L of string
  | LL of string
  val n = L "It works n gives l"
  val n2 = LL "It works n2 gives ll"
  val n3 = ["hej", " ", "igen"]
  val n4 = LL
in
  let
    val p = (n2,n)
  in
    case #2(p) of
      L s => print s
    | LL s => print s
  end;
  (case n3 of
     (s1::s2::s3::rest) => (print s1; print s2; print s3)
   | _ => print "It does not work!");
(case (LL "Hej") of
   LL s => print s
 | L s => print s)
end
