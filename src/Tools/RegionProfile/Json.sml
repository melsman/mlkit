(* Small strict JSON reader. Number lexemes stay exact until IntInf validation. *)
structure ProfileJson =
struct
  datatype t = Obj of (string * t) list | Arr of t list | Str of string
             | Num of string | Bool of bool | Null
  fun error s = raise Fail ("JSON: " ^ s)
  fun parse text =
      let val pos = ref 0
          val len = size text
          fun peek () = if !pos < len then String.sub(text,!pos) else #"\000"
          fun get () = if !pos < len then
                         let val c = peek () in pos := !pos+1; c end
                       else error "unexpected end"
          fun space () = if List.exists (fn c => c = peek ()) [#" ",#"\t",#"\n",#"\r"]
                         then (pos := !pos+1; space ()) else ()
          fun expect c = if get () = c then () else error "unexpected character"
          fun hex () =
              let val c = get ()
              in if c >= #"0" andalso c <= #"9" then ord c-ord #"0"
                 else if c >= #"a" andalso c <= #"f" then ord c-ord #"a"+10
                 else if c >= #"A" andalso c <= #"F" then ord c-ord #"A"+10
                 else error "invalid Unicode escape"
              end
          fun four () = let val a = hex ()
                            val b = hex ()
                            val c = hex ()
                            val d = hex ()
                        in ((a*16+b)*16+c)*16+d end
          fun utf8 n =
              if n < 128 then str(chr n)
              else if n < 2048 then implode [chr(192+n div 64),chr(128+n mod 64)]
              else if n < 65536 then implode [chr(224+n div 4096),chr(128+n div 64 mod 64),chr(128+n mod 64)]
              else implode [chr(240+n div 262144),chr(128+n div 4096 mod 64),chr(128+n div 64 mod 64),chr(128+n mod 64)]
          fun unicode () =
              let val n = four ()
              in if n >= 55296 andalso n <= 56319 then
                   let val () = expect #"\\"
                       val () = expect #"u"
                       val low = four ()
                   in if low < 56320 orelse low > 57343 then error "invalid surrogate pair"
                      else utf8 (65536+(n-55296)*1024+low-56320)
                   end
                 else if n >= 56320 andalso n <= 57343 then error "unpaired surrogate"
                 else utf8 n
              end
          fun string () =
              let fun loop acc =
                      case get () of
                          #"\"" => String.concat(rev acc)
                        | #"\\" =>
                          let val s = case get () of
                                  #"\"" => "\"" | #"\\" => "\\" | #"/" => "/"
                                | #"b" => "\008" | #"f" => "\012" | #"n" => "\n"
                                | #"r" => "\r" | #"t" => "\t" | #"u" => unicode ()
                                | _ => error "invalid escape"
                          in loop (s::acc) end
                        | c => if ord c < 32 then error "control character in string"
                               else loop (str c::acc)
              in expect #"\""; loop [] end
          fun digits () = if Char.isDigit(peek ()) then (pos := !pos+1; digits ()) else ()
          fun someDigits () = if Char.isDigit(peek ()) then digits () else error "expected digit"
          fun number () =
              let val start = !pos
                  val () = if peek () = #"-" then pos := !pos+1 else ()
                  val () = if peek () = #"0" then pos := !pos+1 else someDigits ()
                  val () = if peek () = #"." then (pos := !pos+1; someDigits ()) else ()
                  val () = if peek () = #"e" orelse peek () = #"E" then
                             (pos := !pos+1;
                              if peek () = #"+" orelse peek () = #"-" then pos := !pos+1 else ();
                              someDigits ()) else ()
              in Num(String.substring(text,start,!pos-start)) end
          fun literal (s,v) = (app expect (explode s); v)
          fun value depth =
              if depth > 128 then error "nesting too deep"
              else (space ();
                    case peek () of
                        #"\"" => Str(string ())
                      | #"{" => (expect #"{"; space (); Obj(object (depth+1)))
                      | #"[" => (expect #"["; space (); Arr(array (depth+1)))
                      | #"t" => literal("true",Bool true)
                      | #"f" => literal("false",Bool false)
                      | #"n" => literal("null",Null)
                      | c => if c = #"-" orelse Char.isDigit c then number () else error "expected value")
          and object depth =
              if peek () = #"}" then (expect #"}"; [])
              else
                let fun pair () =
                        let val k = string ()
                            val () = space ()
                            val () = expect #":"
                        in (k,value depth) end
                    fun more acc =
                        (space ();
                         case get () of
                             #"}" => rev acc
                           | #"," => (space (); more (pair ()::acc))
                           | _ => error "expected comma or object end")
                    val pairs = more [pair ()]
                    fun unique [] = ()
                      | unique ((k,_)::xs) = if List.exists (fn (n,_) => n = k) xs
                                            then error "duplicate key" else unique xs
                in unique pairs; pairs end
          and array depth =
              if peek () = #"]" then (expect #"]"; [])
              else
                let fun more acc =
                        (space ();
                         case get () of
                             #"]" => rev acc
                           | #"," => more (value depth::acc)
                           | _ => error "expected comma or array end")
                in more [value depth] end
          val result = value 0
          val () = space ()
      in if !pos = len then result else error "trailing input" end

  fun quoteMode html s =
      let fun escape c =
              case c of
                  #"\"" => "\\\"" | #"\\" => "\\\\" | #"\n" => "\\n"
                | #"\r" => "\\r" | #"\t" => "\\t" | #"<" => if html then "\\u003c" else "<"
                | _ => if ord c < 32 then
                         let val h = Int.fmt StringCvt.HEX (ord c)
                         in "\\u00" ^ (if size h = 1 then "0" else "") ^ h end
                       else str c
      in "\"" ^ String.translate escape s ^ "\"" end
  fun encodeMode html exact value =
      case value of
          Obj fields => "{" ^ String.concatWith "," (map (fn (k,v) => quoteMode html k ^ ":" ^ encodeMode html exact v) fields) ^ "}"
        | Arr values => "[" ^ String.concatWith "," (map (encodeMode html exact) values) ^ "]"
        | Str s => quoteMode html s
        | Num s => if exact then quoteMode html s else s
        | Bool b => Bool.toString b
        | Null => "null"
  val quote = quoteMode true
  val encode = encodeMode true
  val encodeJson = encodeMode false false
  fun fields (Obj xs) = xs | fields _ = error "expected object"
  fun find value name = Option.map #2 (List.find (fn (k,_) => k = name) (fields value))
  fun get value name = case find value name of SOME v => v | NONE => error ("missing " ^ name)
  fun string (Str s) = s | string _ = error "expected string"
  fun kind v = string(get v "type")
  fun integer (Num s) =
      if List.all Char.isDigit (explode s) andalso size s > 0 then
        (case IntInf.fromString s of SOME n => n | NONE => error "invalid integer")
      else error "expected nonnegative integer"
    | integer _ = error "expected integer"
  fun uint v k =
      let val n = integer(get v k)
          val max : IntInf.int = 18446744073709551615
      in if n <= max then n else error ("uint64 overflow: " ^ k) end
  fun array (Arr xs) = xs | array _ = error "expected array"
end
