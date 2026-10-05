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
fun erase (MARKED_LEAF (_,s)) = LEAF s
  | erase (LEAF s) = LEAF s
  | erase (NODE {start,finish,indent,children,childsep}) =
      NODE {start = start, finish = finish, indent = indent,
            children = map erase children, childsep = childsep}
  | erase (HNODE {start,finish,children,childsep}) =
      HNODE {start = start, finish = finish,
             children = map erase children, childsep = childsep}
fun node start finish indent sep children =
  NODE {start = start, finish = finish, indent = indent,
        children = children, childsep = sep}
fun hnode start finish sep children =
  HNODE {start = start, finish = finish, children = children, childsep = sep}
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
val _ = expect "deep indentation abbreviation" 3
  (node "" "" 64 NOSEP [MARKED_LEAF (1,"abcd"),MARKED_LEAF (2,"efgh")])
  ("\n" ^ StringCvt.padLeft #" " 64 "" ^ "abcd" ^ "\nb64efgh")
  [{mark = 1, start = 65, length = 4},{mark = 2, start = 73, length = 4}]

(* Exercise both layouts, separator placement, nested flattening and legacy
 * APIs. Each nonempty marker's text is unique in this corpus. *)
val leaves = [MARKED_LEAF (1,"@one@"),MARKED_LEAF (2,"@two@"),MARKED_LEAF (3,"@three@")]
val separators = [NOSEP,LEFT " | ",RIGHT ", "]
val corpus = leaves @ List.concat (map (fn sep =>
  [node "let " " end" 4 sep leaves,
   hnode "[" "]" sep leaves,
   node "(" ")" 2 sep [hnode "[" "]" sep leaves,LEAF "tail"],
   hnode "" "" sep [node "(" ")" 2 sep leaves,LEAF "tail"],
   node "" "" 70 sep leaves,
   node "long prefix" " end" 0 sep leaves,
   node "" "" 0 sep [], hnode "longprefix" "" sep leaves]) separators)
fun check width tree =
  let val (text,spans) = render width tree
      val plain = erase tree
      val (oldText,()) = capture (fn device => outputTree (device,plain,width))
      val (markedText,()) = capture (fn device => outputTree (device,tree,width))
      fun custom t = #1 (capture (fn device =>
        outputTree' (fn n => "<" ^ Int.toString n ^ ">") (device,t,width)))
      fun ordered ((a : span)::(rest as b::_)) = #start a <= #start b andalso ordered rest
        | ordered _ = true
      fun valid ({mark,start,length} : span) =
        String.substring (text,start,length) = List.nth (["@one@","@two@","@three@"],mark-1)
  in
    assert "unchanged output" (text = oldText andalso text = markedText);
    assert "custom indentation API" (custom tree = custom plain);
    assert "flat legacy API" (flatten1 tree = flatten1 plain);
    assert "format legacy API" (flatten (format (width,tree)) = flatten (format (width,plain)));
    assert "report legacy API" (reportStringTree' width tree = reportStringTree' width plain);
    assert "ordered spans" (ordered spans);
    assert "span contents" (List.all valid spans);
    assert "no annotations in plain tree" (null (#2 (render width plain)))
  end
val _ = List.app (fn ragged =>
  (raggedRight := ragged;
   List.app (fn width => (colwidth := width; List.app (check width) corpus))
            [3,4,7,12,40,100])) [false,true]
val _ = print "PrettyPrint spans: PASS\n"
