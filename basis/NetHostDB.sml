structure NetHostDB : NET_HOST_DB = struct

fun not_impl s = raise Fail ("not implemented: " ^ s)

fun isNull s = prim("__is_null",s : string) : bool

(* error utilities *)

fun failure s =
    let fun errno () : int = prim("sml_errno",())
        fun errmsg (i : int) : string = prim("sml_errormsg", i)
    in raise Fail (s ^ ": " ^ errmsg(errno()))
    end

type in_addr = int (* IPv4 *)

type addr_family = int

type entry =
     { addrType : addr_family,
       addrs    : in_addr list,
       aliases  : string list,
       name     : string,
       xerr     : int }

fun name (e:entry) = #name e

fun aliases (e:entry) = #aliases e

fun addrType (e:entry) = #addrType e

fun addrs (e:entry) = #addrs e
fun addr (e:entry) =
    case #addrs e of
        a :: _ => a
      | _ => raise Fail "NetHostDb.addr: impossible"

fun repair ({addrType,addrs,aliases,name,xerr}:entry) : entry option =
    if xerr < 0 then NONE
    else SOME {addrType=addrType,
               addrs=List.rev addrs,
               aliases=List.rev aliases,
               name=name,
               xerr=xerr}

fun getByName (n:string) : entry option =
    let val e : entry = prim("sml_gethostbyname", n)
    in repair e
    end

fun getByAddr (a: in_addr) : entry option =
    let val e : entry = prim("sml_gethostbyaddr", a)
    in repair e
    end

fun getHostName () : string =
    let val res = prim("sml_gethostname",())
    in if isNull res then failure "NetHostDb.getHostName"
       else res
    end

fun toString (a:in_addr) : string =
    let val res = prim("sml_inaddr_tostring",a)
    in if isNull res then failure "NetHostDb.toString"
       else res
    end

(* The scan function below is copied from MLton
   https://github.com/MLton/mlton/blob/master/basis-library/net/net-host-db.sml
   together with StringCvtfunctionality from
   https://github.com/MLton/mlton/blob/master/basis-library/text/string-cvt.sml
   (slightly modified)

   MLton is released under an HPND-style license; see
   ../doc/license/MLton-HPND-LICENSE for details.
*)

val radixToInt: StringCvt.radix -> int =
 fn StringCvt.BIN => 2
  | StringCvt.OCT => 8
  | StringCvt.DEC => 10
  | StringCvt.HEX => 16

val radixToWord: StringCvt.radix -> word = Word.fromInt o radixToInt

fun radixFn off l h c =
    if c < l orelse c > h then NONE
    else SOME (Char.ord c - off)

fun charToDigit (radix: StringCvt.radix): char -> int option =
    case radix of
        StringCvt.BIN => radixFn 48 #"0" #"1"
      | StringCvt.OCT => radixFn 48 #"0" #"7"
      | StringCvt.DEC => radixFn 48 #"0" #"9"
      | StringCvt.HEX => fn c => case radixFn 48 #"0" #"9" c of
                                     NONE =>
                                     (case radixFn 65 #"A" #"F" c of
                                          NONE => radixFn 97 #"a" #"f" c
                                        | res => res)
                                   | res => res

fun charToWDigit radix = (Option.map Word.fromInt) o (charToDigit radix)

fun wdigits radix reader state =
    let
      val op + = Word.+
      val op * = Word.*
      val r = radixToWord radix
      fun loop (accum, state) =
          case reader state of
              NONE => SOME (accum, state)
            | SOME (c, state') =>
              case charToWDigit radix c of
                  NONE => SOME (accum, state)
                | SOME n => loop (n + accum * r, state')
    in case reader state of
           NONE => NONE
         | SOME (c, state) =>
           case charToWDigit radix c of
               NONE => NONE
             | SOME n => loop (n, state)
    end

fun scan0 reader state =
    let
      fun scanW state =
          case reader state of
              SOME (#"0", state') =>
              (case reader state' of
                   NONE => SOME (0w0, state')
                 | SOME (c, state'') =>
                   if Char.isDigit c
                   then wdigits StringCvt.OCT reader state'
                   else if c = #"x" orelse c = #"X"
                   then wdigits StringCvt.HEX reader state''
                   else SOME (0w0, state'))
            | _ => wdigits StringCvt.DEC reader state
      fun loop (n, state, acc) =
          if n <= 0
          then List.rev acc
          else let
            fun finish (w, state) =
                case reader state of
                    SOME (#".", state') =>
                    loop (n - 1, state', (w, state)::acc)
                  | _ => List.rev ((w, state)::acc)
          in
            case scanW state of
                SOME (w, state') => finish (w, state')
              | NONE => List.rev acc
          end
      val l = loop (4, state, [])
      fun get1 w =
          (Word8.fromLarge (Word.toLarge (Word.andb (w, 0wxFF))),
           Word.>>(w, 0w8))
      fun get2 w =
          let
            val (a,w) = get1 w
            val (b,w) = get1 w
          in (a,b,w)
          end
      fun get3 w =
          let
            val (a,b,w) = get2 w
            val (c,w) = get1 w
            in (a,b,c,w)
          end
      fun get4 w =
          let
            val (a,b,c,w) = get3 w
            val (d,w) = get1 w
          in (a,b,c,d,w)
          end
      fun try l =
          case l of
              [] => NONE
            | [(w, statew)] =>
              let
                val (d,c,b,a,w) = get4 w
              in
                if w = 0wx0
                then SOME (Vector.fromList [a,b,c,d], statew)
                else NONE
              end
            | [(x, statex), (w, statew)] =>
              let
                val (d,c,b,w) = get3 w
                val (a,x) = get1 x
              in
                if w = 0wx0 andalso x = 0wx0
                then SOME (Vector.fromList [a,b,c,d], statew)
                else try [(x, statex)]
              end
            | [(y, statey), (x, statex), (w, statew)] =>
              let
                val (d,c,w) = get2 w
                val (b,x) = get1 x
                val (a,y) = get1 y
              in
                if w = 0wx0 andalso x = 0wx0 andalso y = 0wx0
                then SOME (Vector.fromList [a,b,c,d], statew)
                else try [(y, statey), (x, statex)]
              end
            | [(z, statez), (y, statey), (x, statex), (w, statew)] =>
              let
                val (d,w) = get1 w
                val (c,x) = get1 x
                val (b,y) = get1 y
                val (a,z) = get1 z
              in
                if w = 0wx0 andalso x = 0wx0 andalso y = 0wx0 andalso z = 0wx0
                then SOME (Vector.fromList [a,b,c,d], statew)
                else try [(z, statez), (y, statey), (x, statex)]
              end
            | _ => NONE
    in
      try l
    end

fun scan (reader: (char,'a)StringCvt.reader) : (in_addr,'a)StringCvt.reader =
    fn (a : 'a) =>
       case scan0 reader a of
           NONE => NONE
         | SOME(v,a) =>
           if Vector.length v > 4 then
             failure "NetHostDb.scan"
           else
             let val toW = Word.fromLarge o Word8.toLarge
                 val w = Vector.foldl (fn (w8,w) => Word.orb(Word.<<(w,0w8),toW w8)) 0w0 v
             in SOME(Word.toInt w, a)
             end

fun fromString s = StringCvt.scanString scan s

end
