(* The pretty-printer *)

functor PrettyPrint(structure Report: REPORT
                    structure Crash: CRASH
                    val raggedRight : bool ref
		    val colwidth : int ref
                   ): PRETTYPRINT =
  struct

    structure EdList = Edlib.List

    val WIDTH = 75
    val DEBUG = false
          
    datatype childsep = NOSEP | LEFT of string | RIGHT of string

    datatype StringTree =
        NODE of {start : string, finish: string, indent: int,
                 children: StringTree list, childsep: childsep
                }
      | HNODE of {start : string, finish: string, 
                  children: StringTree list, childsep: childsep
                }
      | LEAF of string
        
    (* mk_lines textwidth texts:  break texts into lines, 
       each of width textwidth (or a bit more) *)
    fun mk_lines textwidth (texts: string list) : string list list =
    let fun loop(w:int ,l: string list ,acc:string list list, []: string list): string list list =
                 (case rev l of   [] => rev acc | l' => rev (l'::acc))
          | loop(w, l, acc, s::ss) = 
               if w<= 0 then (* no space left on line; break it*)
                  loop(textwidth, [], rev l:: acc, s::ss)
               else 
                  loop(w-size s, s::l, acc, ss)
    in 
          loop(textwidth, [], [], texts)
    end;

    (* intersperce b s l: put the string s between every two strings in l
       b is true iff separator s is to be right adjusted *)

    fun intersperce _ (s: string) ([]: string list): string list = []
      | intersperce b s [x] = [x]
      | intersperce true s (x:: rest) = 
           (x ^ s) :: intersperce true s rest
      | intersperce false s (x :: (x' ::rest)) =
           x :: intersperce false s ((s ^ x') ::rest);

    fun intersperceSep NOSEP l = l
      | intersperceSep (RIGHT s) l = intersperce true s l
      | intersperceSep (LEFT s) l =  intersperce false s l;


    fun layoutAtom (f: 'a -> string) (x: 'a) = LEAF(f x)
      
    fun layoutSet f set =
      NODE{start="{", finish="}", indent=1, childsep=RIGHT ", ",
           children=map f (EqSet.list set)
          }

    fun layout_opt layout (SOME x) = layout x
      | layout_opt layout NONE = LEAF "_|_"

    fun layout_pair layout_x layout_y (x,y) =
          NODE {start = "(", finish = ")", childsep = RIGHT ",", indent=1,
		children = [layout_x x, layout_y y]}
    
    fun layout_list layout xs = 
          NODE {start = "[", finish = "]", indent = 1, childsep = RIGHT ", ",
		children = map layout xs}
  
    fun layout_together children indent =
          NODE {start = "", children = children, childsep = NOSEP,
		indent = indent, finish = ""}

    exception FlatString
    fun consIfEnoughRoom(s,(acc: string list, width: int)) = 
      let val n = size s 
      in
         if n<= width then ((s::acc), width-n) else raise FlatString
      end

    local

      fun fold f (LEAF s, acc) = f(s, acc)
        | fold f (NODE{start, finish, indent, children, childsep}, acc) =
            f(finish, foldChildren f (children, childsep, f(start, acc)))
        | fold f (HNODE{start, finish, children, childsep}, acc) = 
            f(finish, foldChildren f (children, childsep, f(start, acc)))

      and foldChildren f (nil, childsep, acc) = acc
        | foldChildren f ([t], _, acc) = fold f (t, acc)

        | foldChildren f (child :: rest, NOSEP , acc) = 
            foldChildren f (rest, NOSEP, fold f (child, acc))

        | foldChildren f (child :: rest, RIGHT s, acc) = 
            foldChildren f (rest, RIGHT s, f(s, fold f (child, acc)))

        | foldChildren f (child::rest, LEFT s, acc) =
            foldChildren f (rest, LEFT s, f(s, fold f (child, acc)))
    in
      val flatten: StringTree -> string list = fn t => rev(fold (op ::) (t, nil))
      val flatten1: StringTree -> string = concat o flatten
      fun flattenOrRaiseFlatString(t,width) = concat(rev(#1(fold consIfEnoughRoom (t, (nil,width)))))
    end
    
    fun oneLiner (f: 'a -> StringTree) (x: 'a) = flatten1(f x)
      
    datatype minipage = LINES of string list
                      | INDENT of int * minipage
                      | PILE of minipage * minipage
      
    val pilePages: minipage list -> minipage =
      foldr PILE (LINES[])

    fun indent (i: int) (m: minipage) =
      case m of
         LINES _ => INDENT(i,m)
      |  INDENT (i',lines) => INDENT(i+i',lines)
      |  PILE(m1,m2) => INDENT(i,m)

    fun blanks n =
      if n <= 0 then "" else " " ^ blanks(n-1)

    fun indent_line (ind: int) (text:string):string =  blanks ind ^ text

    fun get_first_line(m: minipage): (string * minipage) option = 
    let fun loop (ind,m) = 
        case m of
            LINES [] => NONE
         |  LINES (hd::tl) => SOME(indent_line ind hd, indent ind (LINES tl))
         |  INDENT(i, m') => loop(ind+i, m')
         |  PILE(m1,m2) => 
               (case loop(ind,m1) of
                  NONE => loop(ind,m2)
                | SOME (string , m1') => SOME(string, PILE(m1', indent ind m2))
               )
                
    in
         loop(0,m)
    end;

    fun get_last_line(m: minipage): (string * minipage) option = 
    let fun loop (ind,m) = 
        case m of
             LINES [] => NONE
          |  LINES (list) => 
               let val (last,others) = EdList.removeLast list
               in SOME(indent_line ind last, indent ind (LINES others))
               end
          |  INDENT(i, m') => loop(ind+i, m')
          |  PILE(m1,m2) => 
               (case loop(ind,m2) of
                    NONE => loop(ind,m1)
                  | SOME (lastline, m2') => SOME(lastline, PILE(indent ind m1, m2'))
               )
                
    in
         loop(0,m)
    end;

   (* smash() - tries to prefix "line" (which will probably have some leading
      spaces) with "prefix", by replacing line's leading spaces with the
      prefix. If successful, returns a list of the new line :: rest. If
      not, returns prefix :: line :: rest. *)

    fun smash(prefix, line: string, rest:minipage): minipage =
      let
        exception No
        fun try(p :: pRest, #" " :: lRest) = p :: try(pRest, lRest)
          | try(nil, lRest) = lRest
          | try(pRest, nil) = pRest
          | try(_, _) = raise No
      in
        PILE(LINES[implode(try(explode prefix, explode line))], rest)
        handle No => PILE(LINES[prefix, line], rest)
      end

    fun topLeftConcat (s: string) (m: minipage): minipage =
      case get_first_line m of 
          NONE =>LINES([])
        | SOME(thisline, rest:minipage)=> smash(s,thisline,rest)

    fun botRightConcat (s: string) (m: minipage): minipage =
      case get_last_line m of
        NONE => LINES[]
      | SOME(lastline, rest:minipage) => 
          PILE(rest, LINES[lastline ^s])

    

   (* strip() - remove leading spaces from a string. The StringTree might
      well have leading spaces in separators and finish tokens, which is all
      well-and-good for single line printing, but not wanted for multi-line
      printing where LEFT separators and finish tokens appear at the
      start of lines. *)

    fun strip s =
      let
        fun strip'(#" " :: rest) = strip' rest
          | strip' s = s
      in
        (implode o strip' o explode) s
      end;

    fun print (width: int) (LEAF s): minipage = (* width >= 3 *)
          if size s <= width orelse !raggedRight then LINES[s] else LINES["..."]
      | print width (t as HNODE{start, finish,  children, childsep}) =
           (* print children, as many as possible on each line *)
           let val width' = if !raggedRight then !colwidth else width
           in if width'-size start>= 0 then
                botRightConcat finish
                let  val stringLists: string list = 
                         intersperceSep childsep (map flatten1 children)
                     val stringLists' = (* put "start" at the top left of block *)
                          case (start, mk_lines (width'-size start) stringLists) of
                            ("", lines)  => lines
                          | (_, []) => [[start]]
                          | (_, line::lines) => (start::line)::
                                    let val ind = blanks(size start)
                                    in map (fn line=> ind:: line) lines
                                    end
                in  LINES(map concat stringLists')
                end
              else LINES["..."]
           end
      | print (width: int)
              (t as NODE{start, finish, indent, children, childsep}) =
          let                           (* Try to make it go into just one line *)
            val width' = if !raggedRight then !colwidth else width
            val flatString: string = flattenOrRaiseFlatString(t,width')
          in
            LINES[flatString]
          end handle FlatString => (* one line does not hold the whole tree *)
              let
                val startLines = if start = "" then LINES nil else LINES [start]
                val finishLines = if finish = "" then LINES nil else LINES[strip finish]
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
                       | _ => PILE(startLines,PILE(LINES["..."], finishLines))
                else                    (* start or finish to big: *)
                  LINES["..."]
              end


   (* change to pileChildren. Before, the print function would call
      pileChildren and indent the result. This is wrong for things like
      "let ... in ... end", since the "in" would be a LEFT-separator
      attached to the children, and would be indented from the "let"
      and "end". So now, pileChildren is responsible for indenting
      its own argument. Slight inconvenience, since the caller now
      has to smash together lines where the opening bracket will
      fit on the first line (e.g. "let val ...."). *)

    and pileChildren(width, ind, childsep, nil): minipage = LINES nil

      | pileChildren(width, ind, childsep, [child]) =
          indent ind (print (width-ind) child)

      | pileChildren(width, ind, NOSEP, children) =
          indent ind (pilePages(map (print (width-ind)) children))

      | pileChildren(width, ind, LEFT s, first :: rest) =
          let
            val s = strip s     (* If we're printing children multi-line, we
                                   *always* take off leading spaces from
                                   the separator. *)
            val firstWidth: int = if !raggedRight then width else width - ind
            val restWidth = if !raggedRight then width else width - ind - size s
          in
            if restWidth < 3 andalso not(!raggedRight) then 
              indent ind (LINES["..."])
            else
              let
                val restPages = map ((indent ind) o (print restWidth)) rest
                val restPages' =
                  map (topLeftConcat s) restPages
                val firstWidth' = if !raggedRight then firstWidth else firstWidth + size s
              in
                PILE(indent ind (print firstWidth' first),
                     pilePages restPages')
              end
          end

      | pileChildren(width, ind, RIGHT s, children) =
          let
            val myWidth = if !raggedRight then width
                          else width - ind (* - size s *)
                                        (* We ignore the right sep's width. *)
          in
            if myWidth < 3 andalso not(!raggedRight) 
            then
              indent ind (LINES["..."])
            else
              let
                val (last, firstN) = EdList.removeLast children
                val firstNPages = map (print myWidth) firstN
                val firstNPages' = map (botRightConcat s) firstNPages
              in
                indent ind (PILE(pilePages firstNPages',
                                 print myWidth last)
                           )
              end handle EdList.Empty _ => Crash.impossible "PrettyPrint.pileChildren"
          end


    fun prSep NOSEP = "NOSEP"
      | prSep(LEFT s) = "LEFT \"" ^ s ^ "\""
      | prSep(RIGHT s) = "RIGHT \"" ^ s ^ "\""



    fun interpret(indent,m:minipage,acc:string list) =
    case m of
      INDENT(n,m') => interpret(indent+n, m',acc)
    | LINES( lines ) =>
        foldr (fn (text, acc) => (indent_line indent text) :: acc) acc lines
    | PILE(m1,m2) =>
        interpret(indent,m1,interpret(indent,m2,acc))

    fun format(width: int, t: StringTree): minipage =
      if width < 3 then
        Crash.impossible "PrettyPrint.format: width too small"
      else
        LINES(interpret(0,print width t,[]))
             
    (* result with newlines  *)
    and flatten (LINES( string_list)) : string =
      EdList.stringSep "" "" "\n" (fn x => x) string_list
      | flatten _ = Crash.impossible "PrettyPrint.flatten"

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
    val s128="                                                                                                                                ";
        

  fun outputTree(device: string -> unit, t: StringTree, width) : unit =
    let
(*old
       fun output_line indent text =
        let fun loop n =
            if n>=128 then ((*device(s128);*) device"+128";
                            loop(n-128)
                            )
            else if n>=64 then ((*device(s64);*) device"+64";
                                loop(n-64)
                                )
                 else if n>=32 then (device(s32); (*device "+32";*)
                                     loop(n-32)
                                     )
                      else if n>=16 then (device(s16);
                                          loop(n-16)
                                          )
                           else if n>= 8 then (device(s8);
                                               loop(n-8)
                                               )
                                else if n>=4 then (device(s4);
                                                   loop(n-4)
                                                   )
                                     else if n>= 2 then (device(s2);
                                                         loop(n-2)
                                                         )
                                          else if n=1 then device(" ")
                                               else ()
        in
            device("\n");
            loop indent;
            device(text)
        end;
old*)
       val jump = 64  (* maximal number of leading blanks on a line *)
       fun output_line indent text =
        let fun loop (n) =
              if n>=jump then
                  let val (q,r) = (n div jump, n mod jump)
                  in device("b" ^ Int.toString(q*jump));
                     loop(r)
                  end
              else if n>=32 then (device(s32); loop(n-32))
              else if n>=16 then (device(s16); loop(n-16))
              else if n>= 8 then (device(s8);  loop(n-8))
              else if n>= 4 then (device(s4);  loop(n-4))
              else if n>= 2 then (device(s2);  loop(n-2))
              else if n=1 then device(" ")
              else ()
        in
            device("\n");
            loop indent;
            device(text)
        end;

       fun output_minipage (indent:int, m: minipage) = 
           case m of
               LINES [] => ()
             | LINES l  => List.app (output_line indent) l
             | INDENT(i,m') => output_minipage(i+indent, m')
             | PILE(m1,m2) => (output_minipage(indent,m1);
                               output_minipage(indent,m2)
                               )
       val minipage = print width t
        
    in
        output_minipage(0,minipage)
    end

    fun printTree t = outputTree (TextIO.print, t, !colwidth)

    type Report = Report.Report

    fun reportStringTree' width tree = 
      case format(width, tree) of
        LINES page => 
            let
                val lines = map Report.line page
            in
                Report.flatten lines
            end
     | _ => Crash.impossible "PrettyPrint.reportStringTree'"

    fun reportStringTree tree =
      reportStringTree' WIDTH tree

   end;
     
