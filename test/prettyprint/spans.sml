val () = (PrettyPrint.raggedRight := true; PrettyPrint.colwidth := 100)
structure P = PrettyPrint
open P
fun assert message b = if b then () else raise Fail message
fun capture f =
  let val chunks = ref []
      val result = f (fn s => chunks := s :: !chunks)
  in (String.concat (rev (!chunks)), result)
  end
fun render width tree =
  capture (fn device => outputTreeWithSpans {device = device, tree = tree, width = width})
fun node start finish indent sep children =
  NODE {start = start, finish = finish, indent = indent,
        children = children, childsep = sep}
fun expect message width tree text spans =
  let val (actual,locations) = render width tree
  in assert (message ^ " text") (actual = text);
     assert (message ^ " spans") (locations = spans)
  end
val _ = raggedRight := false
val _ = expect "UTF-8 bytes" 80
  (node "[" "]" 2 (RIGHT ", ") [MARKED_LEAF (1,"\206\187"),MARKED_LEAF (1,"x")])
  "\n[\206\187, x]"
  [{mark = 1, start = 2, length = 2},{mark = 1, start = 6, length = 1}]
val _ = expect "empty mark" 80 (MARKED_LEAF (7,"")) "\n"
  [{mark = 7, start = 1, length = 0}]
val _ = expect "embedded newline" 80 (MARKED_LEAF (9,"a\nb")) "\na\nb"
  [{mark = 9, start = 1, length = 3}]
val _ = expect "horizontal node" 80
  (HNODE {start = "[", finish = "]", childsep = RIGHT ", ",
          children = [MARKED_LEAF (1,"a"),MARKED_LEAF (2,"b")]})
  "\n[a, b]"
  [{mark = 1, start = 2, length = 1},{mark = 2, start = 5, length = 1}]
val _ = expect "elided leaf" 3 (MARKED_LEAF (1,"long")) "\n..." []
val _ = expect "elided children" 4
  (node "(" ")" 2 NOSEP [MARKED_LEAF (1,"long")]) "\n(\n...\n)" []
val _ = expect "failed flat attempt" 7
  (node "(" ")" 2 (RIGHT ",") [MARKED_LEAF (1,"abc"),MARKED_LEAF (2,"def")])
  "\n( abc,\n  def\n)"
  [{mark = 1, start = 3, length = 3},{mark = 2, start = 10, length = 3}]
val _ = expect "LEFT separator" 8
  (node "" "" 3 (LEFT " | ") [MARKED_LEAF (1,"abc"),MARKED_LEAF (2,"def")])
  "\n   abc\n|  def"
  [{mark = 1, start = 4, length = 3},{mark = 2, start = 11, length = 3}]
val _ = expect "clipped whitespace" 4
  (node "(" ")" 0 NOSEP [MARKED_LEAF (4,"   x")]) "\n(  x\n)"
  [{mark = 4, start = 2, length = 3}]
val _ = expect "overwritten whitespace" 3
  (node "long" "" 0 NOSEP [MARKED_LEAF (4," ")]) "\n..." []
val _ = raggedRight := true
val _ = colwidth := 3
val _ = expect "prefix cannot overwrite content" 3
  (node "long" "" 0 NOSEP [MARKED_LEAF (4,"x")]) "\nlong\nx"
  [{mark = 4, start = 6, length = 1}]
val _ = expect "prefix overwrites all marked whitespace" 3
  (node "long" "" 0 NOSEP [MARKED_LEAF (4," ")]) "\nlong" []
val _ = expect "deep indentation spaces" 3
  (node "" "" 64 NOSEP [MARKED_LEAF (1,"abcd"),MARKED_LEAF (2,"efgh")])
  ("\n" ^ StringCvt.padLeft #" " 64 "" ^ "abcd\n" ^ StringCvt.padLeft #" " 64 "" ^ "efgh")
  [{mark = 1, start = 65, length = 4},{mark = 2, start = 134, length = 4}]

val _ = print "PrettyPrint spans: PASS\n"
