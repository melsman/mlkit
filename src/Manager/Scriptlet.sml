signature SCRIPTLET =
    sig
	(* Parsing scriptlet form variable arguments *)
	type result = {funid:string, valspecs: (string * string) list}
	val parseArgsFile : string -> result

	(* Generation of abstract form interfaces *)
	type field = {name:string, typ:string}
	type script = {name:string, fields:field list}
	val genFormInterface : string -> script list -> unit

	(* Generation of scriptlet instantiations - returns a list
	 * of the names of the files that are generated. *)
	val genScriptletInstantiations : script list -> string list
    end

functor Scriptlet(val error : string -> 'a) : SCRIPTLET =
    struct

	(* Parsing scriptlet form variable arguments *)
	type result = {funid:string, valspecs: (string * string) list}

	fun isSymbol c =
	    case c of
		#"=" => true
	      | #"(" => true
	      | #")" => true
	      | #":" => true
	      | _ => false

	fun readSymbol is : string option =
	    case TextIO.lookahead is of
		SOME c => (if isSymbol c then (TextIO.input1 is; SOME (String.str c))
			   else NONE)
	      | NONE => NONE
		    
	fun readId is (acc:char list) : string option =
	    case TextIO.lookahead is of
		SOME c => (if Char.isSpace c orelse isSymbol c then 
			       if acc = nil then NONE
			       else SOME (implode (rev acc))					  
			   else (TextIO.input1 is; readId is (c::acc)))
	      | NONE => SOME (implode (rev acc))

	fun readSpace b is : bool =
	    case TextIO.lookahead is of
		SOME c => if Char.isSpace c then (TextIO.input1 is; readSpace true is)
			  else b
	      | NONE => b
			      
	fun readTokens is (xs:string list) : string list option =
	    case readSymbol is of
		SOME "(" =>
		    (case TextIO.lookahead is of
			 SOME #"*" => (TextIO.input1 is; readComment is xs 1)
		       | _ => readTokens is ("("::xs))
	      | SOME t => readTokens is (t::xs)
	      | NONE => 
		    (case readId is nil of
			 SOME "SCRIPTLET" => SOME (rev xs)
		       | SOME id => readTokens is (id :: xs)
		       | NONE => if readSpace false is then readTokens is xs
				 else NONE)
				     
	and readComment is xs 0 = readTokens is xs
	  | readComment is xs level = 
	     case TextIO.input1 is of
		 SOME #"*" => 
		     readComment is xs (case TextIO.lookahead is of
					    SOME #")" => (TextIO.input1 is; level - 1)
					  | SOME _ => level
					  | NONE => error "nonclosed comment")
	       | SOME #"(" =>
		     readComment is xs (case TextIO.lookahead is of
					    SOME #"*" => (TextIO.input1 is; level + 1)
					  | SOME _ => level
					  | NONE => error "nonclosed comment")
	       | SOME _ => readComment is xs level
	       | NONE => error "nonclosed comment"
		     
	fun isId token : bool = 
	    CharVector.foldli (fn (i,c,b) => 
			       (if i=0 then
				    (Char.isAlpha c 
				     orelse c = #"_") 
				else (b andalso (Char.isAlphaNum c 
						 orelse c = #"_" 
						 orelse c = #"'")))) true (token,0,NONE)

	fun isLongid token : bool = 
	    let val subtokens = String.tokens (fn c => c = #".") token
	    in List.all isId subtokens
	    end

	fun parseId s tokens : string * string list =
	    case tokens of
		t::ts => if isId t then (t,ts)
			 else error ("failed to parse " ^ s ^ " identifier")
	      | _ => error ("failed to parse " ^ s ^ " identifier")

	fun parseToken s tokens =
	    case tokens of
		x :: xs => if s = x then xs
			   else error ("expecting `" ^ s ^ "'")
	      | _ => error ("expecting `" ^ s ^ "'")

	fun parseType (ts,acc) =
	    let
		fun spacify (x::y::ys) = if isLongid x andalso isLongid y then x :: " " :: spacify (y::ys)
					 else x :: spacify (y::ys)
		  | spacify x = x
		fun return acc ts = 
		    case concat(spacify(rev acc)) of
			"" => NONE
		      | s => SOME (s, ts)
	    in
		case ts of
		    t::ts' => if t <> "end" andalso t <> "val" then parseType (ts',t::acc)
			      else return acc ts
		  | nil => return acc ts
	    end
	fun parseSpecs ts : (string * string) list * string list =
	    let fun parseSpec ts : ((string * string) * string list) option =
		  case ts of
		      "val" :: ts => 
			  let val (id,ts) = parseId "value" ts
			      val ts = parseToken ":" ts
			  in
			      case parseType (ts,nil) of
				  SOME (typ,ts) => SOME((id,typ),ts)
				| NONE => NONE
			  end
		    | _ => NONE
		fun parse (ts,acc) =
		    case parseSpec ts of
			SOME (p,ts) => parse (ts,p::acc)
		      | NONE => (rev acc,ts)
	    in parse (ts, nil)
	    end

	fun printss nil = print "\n"
	  | printss (x::xs) = (print x; print "\n"; printss xs)

	fun parseArgs (is : TextIO.instream) : result =
	    case readTokens is nil of
		NONE => error "not able to read all tokens before SCRIPTLET signature"
	      | SOME ts =>
		    let (* val _ = printss ts *)
			val ts = parseToken "functor" ts
			val (funid, ts) = parseId "functor" ts
			val ts = parseToken "(" ts
		    in case ts of
			[")", ":"] => {funid=funid,valspecs=nil}
	               | _ =>
			let				
			    val (strid, ts) = parseId "structure" ts
			    val ts = parseToken ":" ts
			    val ts = parseToken "sig" ts
			    val (specs,ts) = parseSpecs ts
			in {funid=funid,valspecs=specs}
			end
		    end
	fun parseArgsFile (f: string) : result =
	    let val is = TextIO.openIn f
	    in parseArgs is before TextIO.closeIn is
		handle X => (TextIO.closeIn is; raise X)
	    end

	(* Generation of abstract form interfaces *)

	fun stripString s s2 =
	    let fun remove (c::cs,c2::cs2) = if c=c2 then remove(cs,cs2)
					     else NONE
		  | remove (nil,cs2) = SOME cs2
		  | remove _ = NONE
		val (cs,cs2) = (rev (explode s), rev (explode s2))
	    in case remove(cs,cs2) of 
		SOME cs => SOME(implode (rev cs))
	      | NONE => NONE
	    end

	fun stripFormtype typ =
	    case stripString " Obj.obj" typ of
		SOME typ => typ
	      | NONE => error "expecting value specification of type 'a Obj.obj"


	fun typToString "int" = "Int.toString"
	  | typToString "string" = ""
	  | typToString s = 
	    (case rev (String.tokens (fn c => c = #".") s) of
		 x::xs => concat (map (fn x => x ^ ".") (rev xs)) ^ "toString"
	       | _ => error ("Type '" ^ s ^ "' not known!"))

	type field = {name:string, typ:string}
	type script = {name:string, fields:field list}
	fun ind 0 = ""
	  | ind n = "  " ^ ind (n-1)

	fun genFormIface (ss:script list) : string = 
	    let 
		local val buf : string list ref = ref nil
		in 
		    fun bufToString() = concat(rev(!buf))
		    fun outs s = buf := s :: !buf
		    fun outnl() = outs "\n"
		    fun outl s = (outs s; outnl())
		    fun outi i s = (outs (ind i); outl s)
		end

		fun out_header_sig () =
		    (  outl   "signature SCRIPTS ="
		     ; outi 1 "sig"
		     ; outi 2 "include XHTML_EXTRA"
		     ; outi 2 "structure Http : HTTP_EXTRA"
		     ; outi 2 "sharing type Http.html = html")

		fun appl f nil = ()
		  | appl f [x] = f(x,true)
		  | appl f (x::xs) = (f(x,false); appl f xs)

		fun maybe_out_linkargs fields ty =
		    if fields = nil then outl ty
		    else (  outs "{"
			  ; appl (fn ({name,typ},b) => outs (name ^ ":" ^ stripFormtype typ 
							     ^ (if b then "" else ", "))) fields
			  ; outl "}"
			  ; outi 10 ("-> " ^ ty)
			  )

		fun out_script_sig {name,fields} =
		    (  outi 2 ("structure " ^ name ^ " :")
		     ; outi 3 "sig"
		     ; app (fn {name,...} => outi 4 ("type " ^ name)) fields
		     ; app (fn {name,typ} => outi 4 ("val " ^ name ^ " : (" ^ name ^ "," 
						     ^ stripFormtype typ ^ ") fname")) fields
		     ; outs (ind 4)
		     ; outs "val form : ("
		     ; app (fn {name,...} => outs (name ^ "->")) fields
		     ; outl "nil,'a,'p,block) form"
		     ; outi 10 "-> (nil,nil,'a,formclosed,'p,block) elt"
		     ; outs (ind 4)
		     ; outs "val link : "
		     ; maybe_out_linkargs fields "('x,'y,ina,'f,'p,inline) elt"
		     ; outi 10 "-> ('x,'y,aclosed,'f,'p,inline) elt"
		     ; outi 3 "end")

		fun out_header_struct () = 
		    (  outl "structure Scripts :> SCRIPTS ="
		     ; outi 1 "struct"
		     ; outi 2 "open XHtml"
		     ; outi 2 "structure Http = Http"
		     ; outi 2 "fun form0 t s = Unsafe.form {action=s,method=\"post\"} t"
		     ; outnl())

		fun out_link s fields =
		    if fields = nil then 
			outi 4 ("fun link e = Unsafe.ahref {src=\"/" ^ s ^ ".sml\"} e")
		    else 
			(  outs (ind 4)
			 ; outs "fun link {"
			 ; appl (fn ({name,typ},b) => outs(name ^ "=" ^ name ^ "'" ^
							   (if b then "" else ","))) fields
			 ; outl "} e ="
			 ; outi 5 ("Unsafe.ahref {src=concat[\"/" ^ s ^ ".sml\", ")
			 ; (let
				fun outline p {name,typ} =
				    let val typ = stripFormtype typ
				    in outi 7 ("\"" ^ p ^ name ^ "=\", Unsafe.urlencode(" 
					       ^ typToString typ ^ " " ^ name ^ "'),")
				    end
			    in case fields of
				f::fs => (outline "?" f; app (outline "&") fs)
			      | nil => ()
			    end)
			 ; outi 7 "\"\"]} e"	  
			 )

		fun out_script_struct {name=s,fields} = 
		    (  outi 2 ("structure " ^ s ^ " =")
		     ; outi 3 "struct"
		     ; app (fn {name,typ} => outi 4 ("type " ^ name ^ " = unit")) fields
		     ; outi 4 ("val " ^ s ^ " = \"" ^ s ^ ".sml\"")
		     ; app (fn {name,typ} => outi 4 ("val " ^ name ^ " = {script=" ^ s ^ 
						     ", n=\"" ^ name ^ "\"}")) fields
		     ; outi 4 ("fun form t = form0 t " ^ s)
		     ; out_link s fields
		     ; outi 3 "end")
	    in
	        outl "(* This script is auto generated by SMLserver, based on"
	      ; outl " * scriptlet functor arguments - DO NOT EDIT THIS FILE! *)"
	      ; outnl
	      ; out_header_sig()
	      ; app out_script_sig ss
	      ; outi 2 "end"
	      ; outl ""
	      ; out_header_struct()
	      ; app out_script_struct ss
	      ; outi 2 "end"
	      ; outnl()
              ; outl   "signature SCRIPTLET ="
	      ; outi 1 "sig"
	      ; outi 2 "val response : Scripts.Http.response"
	      ; outi 1 "end"
	      ; bufToString()
	    end	

	fun inputAll f : string option = 
	    let val is = TextIO.openIn f
	    in (SOME(TextIO.inputAll is) before TextIO.closeIn is)
		handle X => (TextIO.closeIn is; raise X)
	    end handle _ => NONE

	fun writeIfDifferent {file:string,content:string} : bool =
	    if SOME content = inputAll file then false
	    else let val os = TextIO.openOut file
		 in (  TextIO.output(os,content) 
		     ; TextIO.closeOut os
		     ; true
		    ) handle X => (TextIO.closeOut os; raise X)
		 end

	fun genFormInterface (file:string) a =
	    if writeIfDifferent {file=file,content=genFormIface a} then
		print ("[wrote type safe XHTML form interface: " ^ file ^ "]\n")
	    else 
		print ("[reusing type safe XHTML form interface: " ^ file ^ "]\n")

	fun genScriptletInstance {name,fields} : string = 
	    let fun objFrom typ = 
		  let val typ = stripFormtype typ
		  in case typ of
		      "int" => "Obj.fromInt'"
		    | "string" => "Obj.fromString'"
		    | "bool" => "Obj.fromBool'"
		    | _ => error ("unsupported field type: " ^ typ)
		  end
		fun mk_field {name,typ} = ("    val " ^ name ^ " = case Ns.Conn.formvar \"" ^ name ^ "\" of\n\
					   \        SOME s => " ^ objFrom typ ^ " s\n\
					   \      | NONE => (Ns.Conn.write \"Impossible\";\n\
					   \                 Ns.exit())\n")
	    in
		concat (["structure X = ", name, " (\n",
			 "  struct\n"] @ map mk_field fields @
			["  end)\n",
			 "val _ = Ns.Conn.write (Scripts.Http.Unsafe.toString X.response)\n"])
	    end

	fun genScriptletInstantiations (ss: script list) : string list =
	    map (fn s as {name,...} =>
		 let val i = genScriptletInstance s
		     val file = name ^ ".gen.sml"
		 in 
		     (if writeIfDifferent {file=file, content=i} then
			  print ("[wrote scriptlet instance: " ^ file ^ "]\n")
		      else 
			  print ("[reusing scriptlet instance: " ^ file ^ "]\n")
		     ) ; file
		 end) ss
    end
	