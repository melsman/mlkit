
let

  fun print (s:string):unit = prim ("printStringML",s)
  fun printNum (s:int):unit = prim ("printNum",s)

  datatype Name =
      MARTIN of string
    | NIELS of string
    | NO_NAME

  infix ::
  val l1 = [1,2,3,4,5]
  val l2 = ["str1","str2","str3","str4","str5"]

  fun print_list p [] = print "[]\n"
    | print_list p (a::rest) = (p a; print "::";print_list p rest)

  val _ = print_list printNum l1
  val _ = print_list print l2
  val _ = print_list printNum l1

  val _ =
    case l1 of
      [] => print "intERROR0\n"
    | [1] => print "intERROR1\n"
    | [1,2] => print "intERROR2\n"
    | [1,2,3] => print "intERROR3\n"
    | [1,2,3,4] => print "intERROR4\n"
    | [1,2,3,4,5] => print "intOK5\n"
    | _ => print "intERROR6\n"

  val _ =
    case l2 of
      [] => print "strERROR0\n"
    | ["str1"] => print "strERROR1\n"
    | ["str1","str2"] => print "strERROR2\n"
    | ["str1","str2","str3"] => print "strERROR3\n"
    | ["str1","str2","str3","str4"] => print "strERROR4\n"
    | ["str1","str2","str3","str4","str5"] => print "strOK5\n"
    | _ => print "strERROR6\n"

  val _ =
    case NIELS "Hallenberg" of
      NIELS "Hallenberg" => print "datatypeOKniels\n"
    | _ => print "datatyepERRORniels"


  val martin = MARTIN "Elsman"
  val _ =
    case martin of
      MARTIN efternavn => (print "Martin ";print efternavn; print " datatypeOKmartin\n")
    | _ => print "datatypeERRORmartin\n"

  val noname = NO_NAME
  val _ =
    case noname of
      MARTIN _ => print "noname ERROR0\n"
    | NIELS _ => print "noname ERROR1\n"
    | NO_NAME => print "noname OK\n"
  val t = true
  val f = false
  val _ =
    case t of
      true => print "true OK\n"
    | false => print "true ERROR\n"
  val _ =
    case f of
      true => print "false ERROR\n"
    | false => print "false OK\n"

in
  ()
end;
