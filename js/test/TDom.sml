
structure TDom :> TDOM =
struct

  open TimeVal
  type 'a b = ('a,B)t          (* behavior *)
              
  type attrs = (string * string) list
  datatype ast0 = Elem of string * attrs * ast0 
                | Elem0 of string * attrs
                | Seq of ast0 * ast0 
                | Empty 
                | Str of string

  type 'f ast = ast0
  type Inl = unit
  type Blk = unit
  type 'f Flw = unit
  type Li = unit
  type Dl = unit 
  type Td = unit
  type Tr = unit

  type 'f flw = 'f Flw ast b
  type inl = Inl flw
  type blk = Blk flw
  type li = Li ast b
  type dl = Dl ast b
  type td = Td ast b
  type tr = Tr ast b

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
      
  fun Attrs nil : attrs b = const nil
    | Attrs ((s,b)::rest) = arr (op ::) (pair(pair(const s,b), Attrs rest))

  fun $ (s:string b) : 'f flw = arr Str s
  fun br() : 'f flw = const (Elem0("br",nil))

  fun elem t e = Elem(t,nil,e)
  fun elema t (a,e) = Elem(t,a,e)
  fun elem0a t a = Elem0(t,a)
  fun elem0 t = Elem0(t,nil)

  fun em a = arr (elem "em") a
  fun strong a = arr (elem "strong") a
  fun dfn a = arr (elem "dfn") a
  fun code a = arr (elem "code") a
  fun samp a = arr (elem "samp") a
  fun kbd a = arr (elem "kbd") a
  fun var a = arr (elem "var") a
  fun cite a = arr (elem "cite") a
  fun abbr sb eb = arr (fn (s,e) => elema "abbr" ([("title",s)],e)) (pair(sb,eb))
  fun acronym a = arr (elem "acronym") a
  fun sub a = arr (elem "sub") a
  fun sup a = arr (elem "sup") a
  fun tt a = arr (elem "tt") a
  fun i a = arr (elem "i") a
  fun b a = arr (elem "b") a
  fun big a = arr (elem "big") a
  fun small a = arr (elem "small") a
  fun p a = arr (elem "p") a
  fun h1 a = arr (elem "h1") a
  fun h2 a = arr (elem "h2") a
  fun h3 a = arr (elem "h3") a
  fun h4 a = arr (elem "h4") a
  fun h5 a = arr (elem "h5") a
  fun h6 a = arr (elem "h6") a

  fun op div a = arr (elem "div") a
  fun address a = arr (elem "address") a

  fun blockquote a = arr (elem "blockquote") a
  fun pre a = arr (elem "pre") a

  val hr = const (elem0 "hr")

  fun op & (e1: 'f ast b,e2: 'f ast b) : 'f ast b =
      arr Seq (pair(e1,e2))

  fun li a = arr (elem "li") a
  fun dt a = arr (elem "dt") a
  fun dd a = arr (elem "dd") a
  fun ol a = arr (elem "ol") a
  fun ul a = arr (elem "ul") a
  fun dl a = arr (elem "dl") a

  fun imga a = arr (elem0a "img") a

  fun td a = arr (elem "td") a
  fun tda a e = arr (elema "td") (pair(a,e))

  fun th a = arr (elem "th") a
  fun tha a e = arr (elema "th") (pair(a,e))

  fun tr a = arr (elem "tr") a
  fun tra a e = arr (elema "tr") (pair(a,e))

  fun table a = arr (elem "table") a
  fun tablea a e = arr (elema "table") (pair(a,e))

  fun inputa a = arr (elem0a "input") a

  fun textareaa a e = arr (elema "textarea") (pair(a,e))

  type Bdy = unit
  type bdy = Bdy ast b 
  fun body a = arr (elem "body") a

  fun title a = arr (elem "title") a
  fun head a = arr (elem "head") a

  type Htm = unit
  type htm = Htm ast b

  fun html(s,e) = op &(head(title($s)), body e)

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
end
