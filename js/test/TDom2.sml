
structure TDom :> TDOM =
struct

  open RWP
              
  type 'f elm0 = Js.elem list
  type 'f elm = 'f elm0 b
  type Inl = unit
  type Blk = unit
  type 'f Flw = unit
  type Li = unit
  type Dl = unit 
  type Td = unit
  type Tr = unit

  type 'f flw = 'f Flw elm
  type inl = Inl flw
  type blk = Blk flw
  type li = Li elm
  type dl = Dl elm
  type td = Td elm
  type tr = Tr elm

  fun htmlencode s : string =
      let fun enc #"<" = "&lt;"
	    | enc #">" = "&gt;"
	    | enc #"&" = "&amp;"
	    | enc #"\"" = "&quot;"
	    | enc c = String.str c
      in String.translate enc s 
      end
      
  fun quotencode s : string =
      let fun enc #"\"" = "&quot;"
	    | enc c = String.str c
      in String.translate enc s 
      end

  fun removeChildren e =
      case Js.firstChild e of
        SOME c => (Js.removeChild e c; removeChildren e)
      | NONE => ()

  fun single a = [a]

  fun $ (s:string b) : 'f flw = 
      arr (single o Js.createTextNode) s

  (* attributes *)
  datatype attr = A of string * string b  
                | S of string * string b  (* style *)

  type attrs = attr list

  fun elema t (a,e) =
      let val pR = ref NONE   (* parent ref *)
          val aa = List.foldl (fn (A(x,y),a) => pair(const x,y)::a | (S _,a) => a) nil a
          val ss = List.foldl (fn (S(x,y),a) => pair(const x,y)::a | (A _,a) => a) nil a
      in arr (fn (es,(aa,ss)) => 
                 let val p =
                         case !pR of
                           SOME p =>
                           let fun loop NONE nil = ()
                                 | loop NONE (e::es) = (Js.appendChild p e; loop NONE es)
                                 | loop (SOME a) nil = 
                                   let val a' = Js.nextSibling a
                                   in Js.removeChild p a; loop a' nil
                                   end
                                 | loop (SOME a) (all as e::es) = 
                                   if a = e then loop (Js.nextSibling a) es
                                   else let val a' = Js.nextSibling a
                                        in Js.removeChild p a; loop a' all
                                        end
                           in loop (Js.firstChild p) es
                            ; p
                           end
                         | NONE => 
                           let val p = Js.createElement t
                           in List.app (Js.appendChild p) es
                            ; pR := SOME p
                            ; p
                           end
                 in app (fn (x,y) => Js.setAttribute p x y) aa
                  ; app (fn (x,y) => Js.setStyle p (x,y)) ss
                  ; [p]
                 end) (pair(e,pair(list aa,list ss)))
      end

  fun elem t e = elema t (nil,e)

  fun elem0 t : 'f flw = 
      const [Js.createElement t]

  fun elem0a t a : 'f flw = elema t (a, const nil)

  infix &

  fun e1 & e2 = arr (op @) (pair(e1,e2))

  fun br () = elem0 "br" 
  fun em a = elem "em" a
  fun strong a = elem "strong" a
  fun dfn a = elem "dfn" a
  fun code a = elem "code" a
  fun samp a = elem "samp" a
  fun kbd a = elem "kbd" a
  fun var a = elem "var" a
  fun cite a = elem "cite" a
  fun acronym a = elem "acronym" a
  fun sub a = elem "sub" a
  fun sup a = elem "sup" a
  fun tt a = elem "tt" a
  fun i a = elem "i" a
  fun b a = elem "b" a
  fun big a = elem "big" a
  fun small a = elem "small" a
  fun span e = elem "span" e
  fun p a = elem "p" a
  fun h1 a = elem "h1" a
  fun h2 a = elem "h2" a
  fun h3 a = elem "h3" a
  fun h4 a = elem "h4" a
  fun h5 a = elem "h5" a
  fun h6 a = elem "h6" a

  fun op div a = elem "div" a
  fun address a = elem "address" a

  fun blockquote a = elem "blockquote" a
  fun pre a = elem "pre" a

  val hr = elem0 "hr"

  fun li a = elem "li" a
  fun dt a = elem "dt" a
  fun dd a = elem "dd" a
  fun ol a = elem "ol" a
  fun ul a = elem "ul" a
  fun dl a = elem "dl" a

  fun td a = elem "td" a
  fun th a = elem "th" a
  fun tr a = elem "tr" a
  fun table a = elem "table" a

  fun abbr sb eb = elema "abbr" ([A("title",sb)],eb)
  fun imga a = elem0a "img" a
  fun tda a e = elema "td" (a,e)
  fun tha a e = elema "th" (a,e)
  fun tra a e = elema "tr" (a,e)
  fun tablea a e = elema "table" (a,e)
  fun inputa a = elem0a "input" a
  fun textareaa a e = elema "textarea" (a,e)
  fun pa a e = elema "p" (a,e)
  fun lia a e = elema "li" (a,e)
  fun dta a e = elema "dt" (a,e)
  fun dda a e = elema "dd" (a,e)
  fun dla a e = elema "dl" (a,e)
  fun ula a e = elema "ul" (a,e)
  fun ola a e = elema "ol" (a,e)
  fun diva a e = elema "div" (a,e)
  fun spana a e = elema "span" (a,e)

  type Bdy = unit
  type bdy = Bdy elm 
  fun body e = elem "body" e

  fun bodya a e = elema "body" (a,e)

  fun title a = elem "title" a
  fun head a = elem "head" a

  type Htm = unit
  type htm = Htm elm

  fun html(s,e) = elem "html" (head(title($s)) & e)

  fun install (h:Htm elm) : unit =
      let val e = Js.documentElement Js.document
          fun upd vs = (removeChildren e; List.app (Js.appendChild e) vs)
      in upd (current h)
       ; addListener h upd
      end

(*
  fun list (f : ''a * 'b elm -> 'b elm) (h:'b elm) (ssB: ''a list b) : 'b elm =
      let val b : 'b elm = const (current h)
      in
        addListener ssB (fn ss : ''a list => 
                           let val b' : 'b elm = List.foldl f h ss
                           in send b (current b')
                           end)
      ; b
      end
*)
    
(*
  fun insertDOM (id:string) (b:(ast0,B)t) : unit =
      let
        fun idError s id = 
            raise Fail (s ^ ": element with id=" ^ id ^ " not in dom")

        fun toElems (h: ast0) : Js.elem list =
            case h of
              Str s => [Js.createTextNode s]
            | Elem(s,attrs,h) => 
              let val e = Js.createElement s
              in app (fn (a,b) => Js.setAttribute e a b) attrs
               ; app (Js.appendChild e) (toElems h)
               ; [e]
              end
            | Elem0(s,attrs) => 
              let val e = Js.createElement s
              in app (fn (a,b) => Js.setAttribute e a b) attrs
               ; [e]
              end
            | Empty => []
            | Seq(h1,h2) => toElems h1 @ toElems h2

        fun deleteChildren (e:Js.elem) : unit =
            case Js.firstChild e of
              SOME e' => (Js.removeChild e e'; deleteChildren e)
            | NONE => ()
                      
        fun updateElem e ast =
            let val es = toElems ast
            in deleteChildren e; app (Js.appendChild e) es
            end    
      in
        case Js.getElementById Js.document id of
          SOME e => 
          (updateElem e (current b);
           addListener b (updateElem e))
        | NONE => idError "insertDOM" id    
      end
*)
end
