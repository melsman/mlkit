(* The pretty-printer *)

structure PrettyPrint: PRETTYPRINT =
  struct
    val raggedRight : bool ref = ref true
    val colwidth : int ref = ref 100
    val WIDTH = 75

    fun die s = raise Fail ("PrettyPrint: " ^ s)

    datatype childsep = NOSEP | LEFT of string | RIGHT of string

    datatype StringTree =
        NODE of {start : string, finish: string, indent: int,
                 children: StringTree list, childsep: childsep
                }
      | HNODE of {start : string, finish: string,
                  children: StringTree list, childsep: childsep
                }
      | LEAF of string
      | MARKED_LEAF of int * string

    type span = {mark : int, start : int, length : int}

    (* Layout manipulates text with relative byte spans. No locations are
     * published until the final minipage is emitted. *)
    structure Text =
    struct
      type t = {text : string, spans : span list}
      fun plain s : t = {text = s, spans = []}
      fun marked (mark, s) : t =
        {text = s, spans = [{mark = mark, start = 0, length = size s}]}
      fun string ({text,...} : t) = text
      fun size t = String.size (string t)
      fun shift n ({mark,start,length} : span) =
        {mark = mark, start = start+n, length = length}
      fun concat ts : t =
        let
          fun loop ([], _, acc) = rev acc
            | loop (({text,spans} : t)::rest, offset, acc) =
                loop (rest, offset + String.size text,
                      foldl (fn (span, a) => shift offset span :: a) acc spans)
        in
          {text = String.concat (map string ts), spans = loop (ts, 0, [])}
        end
      fun append (a,b) = concat [a,b]

      (* smash may replace leading whitespace, including whitespace inside a
       * marked leaf. Keep only the surviving part of each nonempty span. *)
      fun drop n ({text,spans} : t) : t =
        let
          fun clip ({mark,start,length} : span) =
            let val first = Int.max (n,start)
                val last = start+length
            in
              if last > first orelse (length = 0 andalso start >= n) then
                SOME {mark = mark, start = first-n, length = last-first}
              else NONE
            end
        in
          {text = String.extract (text,n,NONE), spans = List.mapPartial clip spans}
        end
    end

    (* mk_lines textwidth texts: break texts into lines, each of width textwidth
       (or a bit more) *)

    fun mk_lines textwidth (texts: Text.t list) : Text.t list list =
        let fun loop (w:int ,l: Text.t list ,acc:Text.t list list, []: Text.t list): Text.t list list =
                (case rev l of [] => rev acc | l' => rev (l'::acc))
              | loop (w, l, acc, s::ss) =
                if w <= 0 then (* no space left on line; break it*)
                  loop(textwidth, [], rev l:: acc, s::ss)
                else
                  loop(w-Text.size s, s::l, acc, ss)
        in loop(textwidth, [], [], texts)
        end

    (* intersperce b s l: put the string s between every two strings in l b is
       true iff separator s is to be right adjusted *)

    fun intersperce _ (s: string) ([]: Text.t list) : Text.t list = []
      | intersperce b s [x] = [x]
      | intersperce true s (x:: rest) = Text.append (x,Text.plain s) :: intersperce true s rest
      | intersperce false s (x :: x' ::rest) = x :: intersperce false s (Text.append (Text.plain s,x') :: rest)

    fun intersperceSep NOSEP l = l
      | intersperceSep (RIGHT s) l = intersperce true s l
      | intersperceSep (LEFT s) l =  intersperce false s l;

    fun layoutAtom (f: 'a -> string) (x: 'a) = LEAF(f x)

    fun layout_opt layout (SOME x) = layout x
      | layout_opt layout NONE = LEAF "_|_"

    fun layout_pair layout_x layout_y (x,y) =
        NODE {start = "(", finish = ")", childsep = RIGHT ",", indent=1,
              children = [layout_x x, layout_y y]}

    fun layout_list layout xs =
        NODE {start = "[", finish = "]", indent = 1, childsep = RIGHT ", ",
              children = map layout xs}

    fun layout_set layout xs =
        NODE {start = "{", finish = "}", indent = 1, childsep = RIGHT ", ",
              children = map layout xs}

    fun layout_together children indent =
        NODE {start = "", children = children, childsep = NOSEP,
              indent = indent, finish = ""}

    exception FlatString
    fun consIfEnoughRoom (s,(acc:Text.t list, width:int)) =
        let val n = Text.size s
        in if n <= width then ((s::acc), width-n) else raise FlatString
        end

    local

      fun fold f (LEAF s, acc) = f(Text.plain s, acc)
        | fold f (MARKED_LEAF m, acc) = f(Text.marked m, acc)
        | fold f (NODE{start, finish, indent, children, childsep}, acc) =
            f(Text.plain finish, foldChildren f (children, childsep, f(Text.plain start, acc)))
        | fold f (HNODE{start, finish, children, childsep}, acc) =
            f(Text.plain finish, foldChildren f (children, childsep, f(Text.plain start, acc)))

      and foldChildren f (nil, childsep, acc) = acc
        | foldChildren f ([t], _, acc) = fold f (t, acc)

        | foldChildren f (child :: rest, NOSEP , acc) =
            foldChildren f (rest, NOSEP, fold f (child, acc))

        | foldChildren f (child :: rest, RIGHT s, acc) =
            foldChildren f (rest, RIGHT s, f(Text.plain s, fold f (child, acc)))

        | foldChildren f (child::rest, LEFT s, acc) =
            foldChildren f (rest, LEFT s, f(Text.plain s, fold f (child, acc)))
    in
      val flatten: StringTree -> Text.t list = fn t => rev(fold (op ::) (t, nil))
      val flattenText = Text.concat o flatten
      val flatten1: StringTree -> string = Text.string o flattenText
      fun flattenOrRaiseFlatString (t,width) = Text.concat(rev(#1(fold consIfEnoughRoom (t, (nil,width)))))
    end

    fun oneLiner (f: 'a -> StringTree) (x: 'a) = flatten1(f x)

    datatype minipage = LINES of Text.t list
                      | INDENT of int * minipage
                      | PILE of minipage * minipage

    val pilePages : minipage list -> minipage = foldr PILE (LINES[])

    fun indent (i:int) (m:minipage) =
        case m of
            LINES _ => INDENT(i,m)
         |  INDENT (i',lines) => INDENT(i+i',lines)
         |  PILE(m1,m2) => INDENT(i,m)

    fun blanks n =
        if n <= 0 then "" else " " ^ blanks(n-1)

    fun indent_line (ind:int) text = Text.append (Text.plain (blanks ind), text)

    fun get_first_line (m:minipage) : (Text.t * minipage) option =
        let fun loop (ind,m) =
                case m of
                    LINES [] => NONE
                 |  LINES (hd::tl) => SOME(indent_line ind hd, indent ind (LINES tl))
                 |  INDENT(i, m') => loop(ind+i, m')
                 |  PILE(m1,m2) =>
                    case loop(ind,m1) of
                        NONE => loop(ind,m2)
                      | SOME (string , m1') => SOME(string, PILE(m1', indent ind m2))
        in loop(0,m)
        end

    fun get_last_line (m:minipage) : (Text.t * minipage) option =
        let fun loop (ind,m) =
                case m of
                    LINES list =>
                    (case rev list of
                         last::others_rev =>
                         SOME(indent_line ind last, indent ind (LINES (rev others_rev)))
                       | _ => NONE)
                 |  INDENT(i, m') => loop(ind+i, m')
                 |  PILE(m1,m2) =>
                    case loop(ind,m2) of
                        NONE => loop(ind,m1)
                      | SOME (lastline, m2') => SOME(lastline, PILE(indent ind m1, m2'))
        in loop(0,m)
        end

   (* smash() - tries to prefix "line" (which will probably have some leading
      spaces) with "prefix", by replacing line's leading spaces with the
      prefix. If successful, returns a list of the new line :: rest. If not,
      returns prefix :: line :: rest. *)

    fun smash (prefix, line:Text.t, rest:minipage) : minipage =
        let
          val replaced = Int.min (size prefix,Text.size line)
          fun fits i = i = replaced orelse
            (String.sub (Text.string line,i) = #" " andalso fits (i+1))
        in
          if fits 0 then
            PILE(LINES[Text.append (Text.plain prefix,Text.drop replaced line)], rest)
          else PILE(LINES[Text.plain prefix, line], rest)
        end

    fun topLeftConcat (s: string) (m: minipage): minipage =
        case get_first_line m of
            NONE => LINES([])
          | SOME (thisline, rest:minipage) => smash(s,thisline,rest)

    fun botRightConcat (s: string) (m:minipage): minipage =
        case get_last_line m of
            NONE => LINES[]
          | SOME(lastline, rest) => PILE(rest, LINES[Text.append (lastline,Text.plain s)])

   (* strip() - remove leading spaces from a string. The StringTree might well
      have leading spaces in separators and finish tokens, which is all
      well-and-good for single line printing, but not wanted for multi-line
      printing where LEFT separators and finish tokens appear at the start of
      lines. *)

    fun strip s =
        let fun strip'(#" " :: rest) = strip' rest
              | strip' s = s
        in (implode o strip' o explode) s
        end

    fun print (width: int) (LEAF s): minipage = (* width >= 3 *)
          if size s <= width orelse !raggedRight then LINES[Text.plain s] else LINES[Text.plain "..."]
      | print width (MARKED_LEAF (mark,s)) =
          if size s <= width orelse !raggedRight then
            LINES[Text.marked (mark,s)]
          else LINES[Text.plain "..."]
      | print width (t as HNODE{start, finish,  children, childsep}) =
           (* print children, as many as possible on each line *)
           let val width' = if !raggedRight then !colwidth else width
           in if width'-size start>= 0 then
                botRightConcat finish
                let  val stringLists: Text.t list =
                         intersperceSep childsep (map flattenText children)
                     val stringLists' = (* put "start" at the top left of block *)
                          case (start, mk_lines (width'-size start) stringLists) of
                            ("", lines)  => lines
                          | (_, []) => [[Text.plain start]]
                          | (_, line::lines) => (Text.plain start::line)::
                                    let val ind = blanks(size start)
                                    in map (fn line => Text.plain ind:: line) lines
                                    end
                in  LINES(map Text.concat stringLists')
                end
              else LINES[Text.plain "..."]
           end
      | print (width: int)
              (t as NODE{start, finish, indent, children, childsep}) =
          let                           (* Try to make it go into just one line *)
            val width' = if !raggedRight then !colwidth else width
            val flatString: Text.t = flattenOrRaiseFlatString(t,width')
          in
            LINES[flatString]
          end handle FlatString => (* one line does not hold the whole tree *)
              let
                val startLines = if start = "" then LINES nil else LINES [Text.plain start]
                val finishLines = if finish = "" then LINES nil else LINES[Text.plain (strip finish)]
              in
                if        size start <= width andalso size finish <= width
                   orelse !raggedRight
                then                    (* print children indented *)
                  if         width - indent >= 3
                      orelse !raggedRight
                  then                  (* enough space to attempt printing
                                             of children *)
                    let
                      val childrenLines: minipage =
                        pileChildren(width, indent, childsep, children)

                      val startAndChildren: minipage =
                        case get_first_line childrenLines
                          of NONE =>startLines
                           | SOME(hd, tl:minipage) =>
                                (* `smash' sees if start and hd can be
                                   collapsed into one line *)
                               smash(start, hd, tl)

                        val allLines =  (* add finishing line, if not empty *)
                          PILE(startAndChildren, finishLines)
                    in
                      allLines
                    end
                  else                  (* not enough space to attempt
                                             printing of the children *)
                    case children
                      of nil => PILE(startLines, finishLines)
                       | _ => PILE(startLines,PILE(LINES[Text.plain "..."], finishLines))
                else                    (* start or finish to big: *)
                  LINES[Text.plain "..."]
              end


   (* change to pileChildren. Before, the print function would call pileChildren
      and indent the result. This is wrong for things like "let ... in ... end",
      since the "in" would be a LEFT-separator attached to the children, and
      would be indented from the "let" and "end". So now, pileChildren is
      responsible for indenting its own argument. Slight inconvenience, since
      the caller now has to smash together lines where the opening bracket will
      fit on the first line (e.g. "let val ...."). *)

    and pileChildren(width, ind, childsep, nil): minipage = LINES nil

      | pileChildren(width, ind, childsep, [child]) =
          indent ind (print (width-ind) child)

      | pileChildren(width, ind, NOSEP, children) =
          indent ind (pilePages(map (print (width-ind)) children))

      | pileChildren(width, ind, LEFT s, first :: rest) =
        let val s = strip s   (* If we're printing children multi-line, we
                                 *always* take off leading spaces from
                                 the separator. *)
            val firstWidth: int = if !raggedRight then width else width - ind
            val restWidth = if !raggedRight then width else width - ind - size s
        in if restWidth < 3 andalso not(!raggedRight) then
             indent ind (LINES[Text.plain "..."])
           else
             let val restPages = map ((indent ind) o (print restWidth)) rest
                 val restPages' = map (topLeftConcat s) restPages
                 val firstWidth' = if !raggedRight then firstWidth else firstWidth + size s
             in PILE(indent ind (print firstWidth' first),
                     pilePages restPages')
             end
        end

      | pileChildren(width, ind, RIGHT s, children) =
        let val myWidth = if !raggedRight then width
                          else width - ind (* - size s *)
                                           (* We ignore the right sep's width. *)
        in if myWidth < 3 andalso not(!raggedRight)
           then indent ind (LINES[Text.plain "..."])
           else case rev children of
                    last::firstN_rev =>
                    let val firstNPages = map (print myWidth) (rev firstN_rev)
                        val firstNPages' = map (botRightConcat s) firstNPages
                    in indent ind (PILE(pilePages firstNPages',
                                        print myWidth last))
                    end
                  | _ => die "pileChildren"
        end

    fun prSep NOSEP = "NOSEP"
      | prSep(LEFT s) = "LEFT \"" ^ s ^ "\""
      | prSep(RIGHT s) = "RIGHT \"" ^ s ^ "\""

    fun interpret (indent,m:minipage,acc:Text.t list) =
        case m of
            INDENT(n,m') => interpret(indent+n, m',acc)
          | LINES lines => foldr (fn (t,a) => indent_line indent t :: a) acc lines
          | PILE(m1,m2) => interpret(indent,m1,interpret(indent,m2,acc))

    fun format (width: int, t: StringTree): minipage =
      if width < 3 then die "format: width too small"
      else LINES(interpret(0,print width t,[]))

    (* result with newlines  *)
    fun flatten (LINES strings) : string = String.concatWith "\n" (map Text.string strings)
      | flatten _ = die "flatten"

   (* The string constants below are used for outputting long strings of blanks
      efficiently.
   *)

    val s1 = " "
    val s2 = "  "
    val s4 = "    "
    val s8 = "        "
    val s16= "                "
    val s32= "                                "
    val s64= "                                                                "
    val s128="                                                                                                                                "

    fun outputMarked (blanks: int -> string) collect
                     (device: string -> unit, t: StringTree, width) : span list =
        let val offset = ref 0
            val spans = ref []
            fun emit s = (device s; if collect then offset := !offset + size s else ())
            val jump = 64  (* maximal number of leading blanks on a line *)
            fun output_line indent text =
                let fun loop n =
                        if n>=jump then
                          let val (q,r) = (n div jump, n mod jump)
                          in emit(blanks(q*jump));
                             loop r
                          end
                        else if n>=32 then (emit(s32); loop(n-32))
                        else if n>=16 then (emit(s16); loop(n-16))
                        else if n>= 8 then (emit(s8); loop(n-8))
                        else if n>= 4 then (emit(s4); loop(n-4))
                        else if n>= 2 then (emit(s2); loop(n-2))
                        else if n=1 then emit(" ")
                        else ()
                in emit("\n");
                   loop indent;
                   (if collect then
                      spans := foldl (fn (span, acc) => Text.shift (!offset) span :: acc)
                                     (!spans) (#spans text)
                    else ();
                    emit (Text.string text))
                end

            fun output_minipage (indent:int, m: minipage) =
                case m of
                    LINES l  => List.app (output_line indent) l
                  | INDENT(i,m') => output_minipage(i+indent, m')
                  | PILE(m1,m2) => (output_minipage(indent,m1);
                                    output_minipage(indent,m2))
            val minipage = print width t
        in output_minipage(0,minipage); rev (!spans)
        end

    fun outputTree' blanks args = ignore (outputMarked blanks false args)

    fun outputTreeWithSpans {device,tree,width} =
        outputMarked (fn n => "b" ^ Int.toString n) true (device,tree,width)

    fun outputTree a = outputTree' (fn n => "b" ^ Int.toString n) a

    fun printTree t = outputTree (TextIO.print, t, !colwidth)

    type Report = Report.Report

    fun reportStringTree' width tree =
        case format(width, tree) of
            LINES page =>
            let val lines = map (Report.line o Text.string) page
            in Report.flatten lines
            end
          | _ => die "reportStringTree'"

    fun reportStringTree tree =
        reportStringTree' WIDTH tree

  end
