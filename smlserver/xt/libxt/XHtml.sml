(* 
 * Copyright (c) 2003, Martin Elsman
 *
 * SMLserver interface for XHtml that statically guarantees (1)
 * validity of constructed documents and (2) consistent and 
 * typed use of forms. 
 *)

structure XHtmlHidden__ : XHTML_EXTRA =
    struct
	structure A : XHTML_ATTR = XHtmlAttr

	type na = A.na

	type ('a1,'a2) coreattrs = ('a1,'a2,na,na,na,na,na,na,na,na,na,na,na,na) A.attr

	fun html p = p

	type inform = unit 
	type formclosed = unit
	type preclosed = unit
	type inpre = unit
	type aclosed = unit
	type ina = unit

	type li = unit         (* list kinds *)
	type dl = unit
	type td = unit
	type tr = unit
	type 'a flow = unit
	type block = unit
	type inline = unit

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

	fun attr0 t s = [t ^ "=\"" ^ s ^ "\""]

	datatype elem =
	    txt of string 
	  | elem0 of string * string list
	  | elem1 of string * string list * elem
	  | seq of elem * elem

	type ('x,'y,'a,'f,'p,'k,'u) elt = elem
	type ('x,'y,'a,'f,'p,'u,'v) inl2inl = elem -> elem
	type ('x,'y,'a,'f,'p,'u,'v) inl2inlpre = elem -> elem
	type ('x,'y,'a,'f,'p,'u,'v) inl2blk = elem -> elem

	type body = elem

	fun p e          = elem1 ("p",nil,e)
	fun em e         = elem1 ("em",nil,e)
	fun strong e     = elem1 ("strong",nil,e)
	fun small e      = elem1 ("small",nil,e)
	fun big e        = elem1 ("big",nil,e)
	fun sup e        = elem1 ("sup",nil,e)
	fun sub e        = elem1 ("sub",nil,e)
	fun acronym e    = elem1 ("acronym",nil,e)
	fun abbr s e     = elem1 ("abbr",["title=\"" ^ quotencode s ^ "\""],e)
	fun cite e       = elem1 ("cite",nil,e)
	fun var e        = elem1 ("var",nil,e)
	fun kbd e        = elem1 ("kbd",nil,e)
	fun samp e       = elem1 ("samp",nil,e)
	fun code e       = elem1 ("code",nil,e)
	fun dfn e        = elem1 ("dfn",nil,e)
	fun b e          = elem1 ("b",nil,e)
	fun i e          = elem1 ("i",nil,e)
	fun u e          = elem1 ("u",nil,e)
	fun tt e         = elem1 ("tt",nil,e)
	fun address e    = elem1 ("address",nil,e)
	fun blockquote e = elem1 ("blockquote",nil,e)
	fun pre e        = elem1 ("pre",nil,e)
	fun ol e         = elem1 ("ol",nil,e)
	fun ul e         = elem1 ("ul",nil,e)
	fun dl e         = elem1 ("dl",nil,e)
	fun hr()         = elem0 ("hr",nil)
	fun br()         = elem0 ("br",nil)
	fun $ t          = txt (htmlencode t)
	fun h1 e         = elem1("h1",nil,e)
	fun h2 e         = elem1("h2",nil,e)
	fun h3 e         = elem1("h3",nil,e)
	fun h4 e         = elem1("h4",nil,e)
	fun h5 e         = elem1("h5",nil,e)
	fun h6 e         = elem1("h6",nil,e)
	nonfix div
	fun div e        = elem1("div",nil,e)
	fun li e         = elem1("li",nil,e)
	fun dt e         = elem1("dt",nil,e)
	fun dd e         = elem1("dd",nil,e)
	fun td e         = elem1("td",nil,e)
	fun th e         = elem1("th",nil,e)
	fun tr e         = elem1("tr",nil,e)
	fun table e      = elem1("table",nil,e)

	infix &
	val op &  = seq
	fun flatten (x, nil) = x
	  | flatten (x, op :: p) = seq(x,flatten p)

	infix attr
	fun f attr a = fn x => 
	    case f x of
		elem0(t,l) => elem0(t,l@a)
	      | elem1(t,l,e) => elem1(t,l@a,e)
	      | e => e

	fun img {src,alt} = elem0("img", attr0 "src" src @ attr0 "alt" (quotencode alt))

	fun body e = elem1("body",nil,e)

	(* Forms *)
	type ('n, 'typ) fname = {script: string, n: string} 
	type nil = unit
	type 't var = 't Form.var

	type ('x,'y,'a,'p,'k,'u) felt = ('x,'y,'a,inform,'p,'k,'u) elt
	type ('x,'a,'p,'k,'u) form = ('x,nil,'a,'p,'k,'u) felt
	type 'a rad = 'a

	type ('x,'y) num = unit
	fun One () = ()
	fun Succ () = ()
	fun swap () x = x

	fun input (name,it,value) = 
	    let val a = case value of
		    SOME s => attr0 "value" s
		  | NONE => nil
		val a = case name of
		    SOME {n,script} => attr0 "name" n @ a
		  | NONE => a
	    in elem0("input", attr0 "type" it @ a)
	    end

 	fun inputHidden n var = 
	    input (SOME n, "hidden", SOME (SMLserver.Unsafe.Form.toString var))

	fun inputSubmit s = 
	    input (NONE, "submit", SOME s)

	fun inputReset s = 
	    input (NONE, "reset", SOME s)

	fun inputText (n,var) = 
	    input (SOME n, "text", Option.map SMLserver.Unsafe.Form.toString var)

	fun inputPassword (n,var) = 
	    input (SOME n, "password", Option.map SMLserver.Unsafe.Form.toString var)

	fun inputRadio (n,var) = 
	    input (SOME n, "radio", SOME (SMLserver.Unsafe.Form.toString var))

	fun inputRadio' (n,var) = inputRadio (n,var)
	    
	fun radioDrop x = x

	type 'a checkbox = unit
	fun inputCheckbox (n,var) =
	    input (SOME n, "checkbox", SOME (SMLserver.Unsafe.Form.toString var))
	fun inputCheckbox' (n,var) = inputCheckbox (n,var)
	fun checkboxDrop x = x

	fun textarea ({n,script}, {rows,cols}, var) = 
	    let val s = getOpt(Option.map SMLserver.Unsafe.Form.toString var,"")
	    in elem1("textarea", 
		     attr0 "name" n 
		     @ attr0 "rows" (Int.toString rows) 
		     @ attr0 "cols" (Int.toString cols), 
		     txt s)  (* memo: what is to be done with s? *)
	    end

	type 't select_option = {text: string, value: 't var, 
				 selected: bool, disabled: bool}

	fun option (s,var) = {text=s,value=var,selected=false,disabled=false}

	fun select0 a opts =
	    let 
		fun battr s false = nil
		  | battr s true = attr0 s s
		fun opt {text, value, selected, disabled} =
		    elem1("option", (attr0 "value" value 
				     @ battr "selected" selected 
				     @ battr "disabled" disabled), $text)
	    in
		case opts of
		    nil => elem0("select", a)
		  | x::xs => elem1("select", a, 
				   foldl (fn (x,a) => a & opt x) (opt x) xs)
	    end

	fun select ({n,script}, opts) =
	    let val opts = map (fn {text,value,selected,disabled} =>
				{text=text,value=SMLserver.Unsafe.Form.toString value, selected=selected,
				 disabled=disabled}) opts
	    in select0 (attr0 "name" n) opts
	    end

	(* Head elements *)
	type helt = elem
	fun script {typ:string} s : helt =
	    elem1("script", attr0 "type" typ, txt s)

	fun style {typ:string} s : helt =
	    elem1("style", attr0 "type" typ, txt s)

	fun meta {content:string} : helt =
	    elem0("meta", attr0 "content" content)

	fun link {typ:string,rel:string,href:string} : helt =
	    elem0("link", attr0 "type" typ @ attr0 "rel" rel @ attr0 "href" href)

	type head = elem
	fun head (t,h) = 
	    elem1("head",nil,foldl seq (elem1("title",nil,txt (htmlencode t))) h)

	type html = elem * elem

	structure Unsafe =
	    struct
		fun form {action,method} e = 
		    elem1 ("form", (attr0 "action" action
				    @ attr0 "method" method), e)

		fun ahref {src} e = elem1("a", attr0 "href" src, e)
		    		    
		fun toString (h,e) : string =
		    let 
			fun insert_spaces nil = nil
			  | insert_spaces [a] = [a]
			  | insert_spaces (a::xs) = a :: " " :: insert_spaces xs
			fun pp_a nil = ""
			  | pp_a l = concat(" " :: insert_spaces l)
			fun btag t = "<" ^ t ^ ">"
			fun btaga t a = "<" ^ t ^ pp_a a ^ ">"
			fun taga t a = "<" ^ t ^ pp_a a ^ " />"
			fun etag t = "</" ^ t ^ ">"
			fun pe (txt s, c) = s :: c
			  | pe (elem0 (t,a), c) = taga t a :: c
			  | pe (elem1 (t,a,e), c) = btaga t a :: pe (e, etag t :: c)
			  | pe (seq (e1,e2): elem, c) = pe(e1,pe(e2,c))
		    in concat 
			(["<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n\
			 \<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
			 \   \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
			 \<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n"] 
			  @ pe(seq(h,e), ["</html>"]))
		    end
			  
		fun urlencode s : string =
		  let fun enc #" " = "+"
			| enc #"-" = "-"
			| enc #"_" = "_"
			| enc #"." = "."
			| enc c = if Char.isAlphaNum c then String.str c 
				  else "%" ^ StringCvt.padLeft #"0" 2 
				      (Int.fmt StringCvt.HEX (Char.ord c))
		  in String.translate enc s 
		  end

		val htmlencode = htmlencode
	    end
			  
        local
	    open A
	    infix %
	in
	    fun validLink() = 
	       Unsafe.ahref {src="http://validator.w3.org/check/referer"}
	       ((img attr (height (px 31) % width (px 88)))
		{src="http://www.w3.org/Icons/valid-xhtml10",
		 alt="Valid XHTML 1.0!"})
	end			

	val op % = A.%
    end
