structure Cs : sig
  datatype t = $ of string | & of t * t | CSeq of t list
  val toString : t -> string
  val concatWith : t -> t list -> t
end =
struct
  datatype t = $ of string | & of t * t | CSeq of t list
  infix &
  fun toString t =
      let fun to (t, acc) =
              case t of
                $s => s::acc
              | t1 & t2 => to(t1,to(t2,acc))
              | CSeq ts => tos (ts,acc)
          and tos (nil,acc) = acc
            | tos (t::ts,acc) = to(t,tos(ts,acc))
      in String.concat (to(t,nil))
      end
  fun concatWith (tsep:t) (nil:t list) : t = $""
    | concatWith tsep [t] = t
    | concatWith tsep (t::ts) = t & tsep & concatWith tsep ts
end

structure JsAst : JS_AST = struct

  fun die s = (print("Error: " ^ s ^ "\n"); raise Fail s)

  type id = string  (* labels and identifiers *)

  datatype cnst = Int of IntInf.int | Str of string | Real of string
                | Bool of bool | Word of IntInf.int | Null

  datatype stmt =
         Var of id * exp option
       | Exp of exp
       | Seq of stmt list
       | Sw of exp * (cnst * stmt) list * stmt option
       | Return of exp
       | Continue of id
       | While of id option * exp * stmt   (* the optional id is a label *)
       | IfStmt of exp * stmt * stmt option
       | Break
       | Try of stmt * id * stmt
       | Throw of exp
       | Embed of string

       and exp =
         Prim of string * exp list   (* string determines if it is infix *)
       | Array of exp list
       | IfExp of exp * exp * exp
       | Fun of id list * stmt
       | App of exp * exp list
       | Id of id
       | Cnst of cnst
       | Prop of exp * id
       | New of id * exp list
       | Sub of exp * exp

  fun is_infix p =
      case p of
        "=" => true  (* assign *)
      | "==" => true
      | "<=" => true
      | ">=" => true
      | "<" => true
      | ">" => true
      | "&" => true
      | "|" => true
      | "^" => true
      | "-" => true
      | "+" => true
      | "*" => true
      | "/" => true
      | "%" => true
      | "," => true (* sequence *)
      | ">>" => true
      | ">>>" => true
      | "<<" => true
      | _ => false

  fun mlToJsString s =
    let
      fun digit n = chr(48 + n);
      fun toJSescape (c:char) : string =
	case c of
	    #"\\"   => "\\\\"
	  | #"\""   => "\\\""
	  | _       =>
	    if #"\032" <= c andalso c <= #"\126" then str c
	    else
		(case c of
		     #"\010" => "\\n"			(* LF,  10 *)
		   | #"\013" => "\\r"			(* CR,  13 *)
		   | #"\009" => "\\t"			(* HT,   9 *)
		   | #"\011" => "\\v"			(* VT,  11 *)
		   | #"\008" => "\\b"			(* BS,   8 *)
		   | #"\012" => "\\f"			(* FF,  12 *)
                   | _       => let val n = ord c
				in implode[#"\\", digit(n div 64), digit(n div 8 mod 8),
					   digit(n mod 8)]
				end)

    in "\"" ^ String.translate toJSescape s ^ "\""
    end

  fun mlToJsInt i =
      String.translate (fn #"~" => "-" | c => Char.toString c) i

  fun mlToJsReal s =
      String.translate (fn #"~" => "-" | c => Char.toString c) s

  open Cs infix &

  fun wrap (s1,s2) cs = $s1 & cs & $s2
  fun par b cs = if b then wrap ("(",")") cs else cs

  fun pp_cnst p c =
      case c of
        Int i => par (p andalso i < 0) ($(mlToJsInt (IntInf.toString i)))
      | Str s => $ (mlToJsString s)
      | Real s => par (p andalso String.sub(s,0) = #"~") ($(mlToJsReal s))
      | Bool true => $"true"
      | Bool false => $"false"
      | Word w => $(IntInf.fmt StringCvt.DEC w)
      | Null => $"null"

  fun pr_cnst c = Cs.toString (pp_cnst false c)

  fun pp_list (start,sep,stop) pp es =
      wrap (start,stop) (Cs.concatWith ($sep) (List.map pp es))

  val end_stmt = $";\n"

  fun pp_varid id =
      if CharVector.exists (fn #"." => true | _ => false) id then $id
      else $"var " & $id

  fun pp_exp p e =
      case e of
        Prim ("Infinity",[]) => $"Infinity"
      | Prim ("-Infinity",[]) => $"-Infinity"
      | Prim (",",es) => pp_list ("(",",",")") (pp_exp false) es
      | Prim (n,es) =>
        let fun default() = par p ($n & pp_list ("(",",",")") (pp_exp false) es)
        in case es of
             [e1,e2] =>
             if is_infix n then
               par p (pp_exp true e1 & $" " & $n & $" " & pp_exp true e2)
             else default()
           | _ => default()
        end
      | Array es => pp_list ("[",",","]") (pp_exp false) es
      | IfExp (e0,e1,e2) => par p (pp_exp true e0 & $"?" & pp_exp true e1 & $":" & pp_exp true e2)
      | Fun (ids,s) => par p ($"function" & pp_list ("(",",",")") $ ids & pp_block s)
      | App (e1,es) => par p (pp_exp true e1 & pp_list ("(",",",")") (pp_exp false) es)
      | Id id => $ id
      | Cnst c => pp_cnst p c
      | Prop (e0,id) => pp_exp true e0 & ($".") & ($id)
      | New (id, es) => par p ($"new " & $id & pp_list ("(",",",")") (pp_exp false) es)
      | Sub (e1, e2) => pp_exp true e1 & ($"[") & pp_exp false e2 & ($"]")

  and pp_stmt s =
      case s of
        Var (id, NONE) => pp_varid id & end_stmt
      | Var (id, SOME e) => pp_varid id & $" = " & pp_exp false e & end_stmt
      | Exp e => pp_exp false e & end_stmt
      | Seq ss => CSeq (List.map pp_stmt ss)
      | Sw (e,cases,defopt) =>
        $"switch (" & pp_exp false e & $") { " &
        CSeq (List.map (fn (c,s) => $"case " & pp_cnst false c & $": " & pp_bblock s) cases) &
        (case defopt of SOME def => $"default: " & pp_block def | NONE => $"") &
        $" }" & end_stmt
      | Return e => $"return " & pp_exp false e & end_stmt
      | Continue id => $"continue " & $id & end_stmt
      | While (idopt, e, s) =>
        let val lab =
                case idopt of SOME id => $id & $": "
                            | NONE => CSeq nil
        in lab & $"while (" & pp_exp false e & $") " & pp_block s & end_stmt
        end
      | IfStmt (e,s1,s2opt) =>
        $"if (" & pp_exp false e & $") " & pp_block s1 &
         (case s2opt of
            SOME s2 => $" else " & pp_block s2 & end_stmt
          | NONE => end_stmt)
      | Break => $"break" & end_stmt
      | Try (s0,id,s) => $"try " & pp_block s0 & $" catch(" & $id & $") " & pp_block s & end_stmt
      | Throw e => $"throw " & pp_exp false e & end_stmt
      | Embed s => $s
  and pp_block s = wrap ("{","}") (pp_stmt s)
  and pp_bblock s = wrap ("{"," break; }") (pp_stmt s)

  fun pr_stmt s = Cs.toString (pp_stmt s)
  fun pr_exp e = Cs.toString (pp_exp false e)

end
