fun reader chunk text =
  let val offset = ref 0
  in fn n =>
       let val count = Int.min(Int.min(n,chunk),size text - !offset)
           val result = String.substring(text,!offset,count)
       in offset := !offset + count; result
       end
  end
fun check (name,actual,expected) =
  if actual = expected then () else raise Fail name
val input = "B abc 123 ~hello !world B\n(* one (* nested *)\n two *)B \"a\\n\\\"b\\\\c\" \255\n"
val lex = TestLex.makeLexer(reader 7 input,17) {tag="ARG",hits=ref 0}
fun collect () =
  let val token = lex()
  in token :: (if String.isPrefix "EOF:" token then [] else collect())
  end
val () = check("actions and positions",collect(),
  ["BOL:B:18:0:ARG","ID:abc:20:0:ARG","DIGIT:1:24:0:ARG",
   "DIGIT:2:25:0:ARG","DIGIT:3:26:0:ARG","AFTER:ID:hello:29:0:ARG",
   "ID:world:36:0:ARG","B:B:42:0:ARG","B:B:71:2:ARG",
   "STRING:a\n\"b\\c:83:2:ARG","HIGH:\255:85:2:ARG","EOF:ARG:11"])
val resumeLex = Escaping.makeLexer(reader 1 "@a !b @ @c abc rej end\n")
fun resume next =
  case next() of
      Escaping.UserDeclarations.Token s =>
        s :: (if s = "EOF" then [] else resume resumeLex)
    | Escaping.UserDeclarations.Resume k => "RESUME" :: resume k
val () = check("escaped and non-tail actions",resume resumeLex,
  ["RESUME","a","AFTER:b","RESUME","RESUME","c","REJECT:abc",
   "RESUME","rej","end","EOF"])
val large = String.implode(List.tabulate(8192,fn _ => #"x"))
val skip = TestLex.makeLexer(reader 1024 ("(*" ^ large ^ "*)ok"),0)
                           {tag="SKIP",hits=ref 0}
val () = check("skip across refills",skip(),"ID:ok:8197:0:SKIP")
val failing = TestLex.makeLexer(reader 1 "?",0) {tag="FAIL",hits=ref 0}
val () = check("action exception",
  ((failing(); false) handle Fail "action failure" => true | _ => false),true)
val () = print "ML-Lex checks passed\n"
