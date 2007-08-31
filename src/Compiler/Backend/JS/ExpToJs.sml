structure ExpToJs : EXP_TO_JS =
struct

fun die s = (print (s ^"\n");raise Fail s)

structure L = LambdaExp

type LambdaPgm = L.LambdaPgm
type Exp = L.LambdaExp

datatype Js = $ of string | & of Js * Js | V of (string * Js) list * Js | Par of Js | StToE of Js | returnJs of Js | IfJs of Js * Js * Js

infix & &&

val emp = $""

structure Context :> sig type context
                       val empty : context
                       val add : context -> Lvars.lvar -> context
                       val isIn : context -> Lvars.lvar -> bool
                     end =
struct
  type context = Lvars.lvar list
  val empty = nil
  fun add c s = s::c
  fun isIn c s = List.exists (fn x => Lvars.eq(s,x)) c
end

local
  type lvar = Lvars.lvar
  type excon = Excon.excon
  val frameLvars : lvar list ref = ref nil
  val frameExcons : excon list ref = ref nil

  (* function to replace first occurence of a sub-string in a string *)
  fun replaceString (s0:string,s1:string) (s:string) : string =
      let val ss = Substring.full s
          val (ss1,ss2) = Substring.position s0 ss
      in if Substring.size ss2 > 0 (* there was a match *) then
           let val ss3 = Substring.triml (size s0) ss2
           in Substring.concat[ss1,Substring.full s1,ss3]
           end
         else s (* no match *)
      end

  fun normalizeBase b = 
      let 
(*
        val b = if String.sub(b,size b - 1) = #"1" then
                  Substring.string(Substring.trimr 1 (Substring.full b))
                else b
*)
        val b = replaceString (".mlb-", "$") b
        val b = replaceString (".sml1", "$") b
      in
        String.translate (fn #"." => "$" | c => Char.toString c) b
      end

  val localBase : string option ref = ref NONE

  fun maybeUpdateLocalBase n : bool (* Singleton(true) *) =
      true before
      (case !localBase of 
         SOME _ => ()  (* no need to set it again! *)
       | NONE => localBase := SOME((normalizeBase o #2 o Name.key) n))

  fun isFrameLvar lvar () =
      List.exists (fn lv => Lvars.eq(lv,lvar)) (!frameLvars)
      andalso maybeUpdateLocalBase (Lvars.name lvar)

  fun isFrameExcon excon () =
      List.exists (fn e => Excon.eq(e,excon)) (!frameExcons)
      andalso maybeUpdateLocalBase (Excon.name excon)

  val symbolChars = "!%&$#+-/:<=>?@\\~`^|*"
                    
  fun isSymbol s = 
      Char.contains symbolChars (String.sub(s,0))
      handle _ => die "isSymbol.empty"

  fun idfy_c (c:char) : string =
      case CharVector.findi (fn(_,e) => e = c) symbolChars
       of SOME(i,_) => Char.toString(Char.chr(97+i))
        | NONE => Char.toString c

  fun idfy s =
      if isSymbol s then
        "s$" ^ String.translate idfy_c s
      else String.translate (fn #"'" => "$"             (* Notice: the patching *)
                              | #"." => "$"             (* below  makes the ids *)
                              | c => Char.toString c) s (* unique... *)

  fun patch n f s =
      let val (k,b) = Name.key n
          val s = s ^ "$" ^ Int.toString k
      in if Name.rigid n orelse f() then
           let val b = normalizeBase b
           in b ^ "." ^ s
           end
         else s
      end           
in
  fun setFrameLvars xs = frameLvars := xs
  fun setFrameExcons xs = frameExcons := xs
  fun resetBase() = localBase := NONE

  fun prLvar C lv =
      patch (Lvars.name lv) 
            (fn() => isFrameLvar lv() andalso not(Context.isIn C lv)) 
            (idfy(Lvars.pr_lvar lv))
  fun prLvarExport lv =
      patch (Lvars.name lv) (fn() => true) (idfy(Lvars.pr_lvar lv))
  fun exconName e = 
      patch (Excon.name e) (isFrameExcon e) ("en$" ^ idfy(Excon.pr_excon e))
  fun exconExn e = 
      patch (Excon.name e) (isFrameExcon e) ("exn$" ^ idfy(Excon.pr_excon e))
  fun exconExnExport e = 
      patch (Excon.name e) (fn() => true) ("exn$" ^ idfy(Excon.pr_excon e))
  fun getLocalBase() = !localBase
end

fun toJSString s =
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

fun j1 && j2 =
  j1 & $" " & j2

fun sToS0 s : Js = $(toJSString s)

fun sToS s : Js = 
    $"new String(" & sToS0 s & $")"

fun cToS0 c : Js = 
    $("\"" ^ String.toString (Con.pr_con c) ^ "\"")

fun unPar (e:Js) = 
    case e of
      Par e => e
    | _ => e

fun parJs (e: Js) : Js = 
    case e of
      Par _ => e
    | _ => Par e

fun sqparJs (e:Js) : Js =
    $"[" & unPar e & $"]"

fun seq (jss : Js list) : Js =
  let fun loop xs =
        case xs
         of nil => $""
          | [x] => unPar x
          | x::xs => unPar x & $", " & loop xs 
  in $"(" & loop jss & $")"
  end

fun appi f es = 
  let fun loop (n,nil) = nil
        | loop (n,x::xs) = f(n,x) :: loop(n+1,xs)
  in loop (0, es)
  end

fun stToE (st : Js) : Js = StToE st

val unitValueJs = "0"

(* 

Arithmetic int32 operations check explicitly for overflow and throw
the Overflow exception in this case. Arithmetic word32 operations
truncate the result to make it fit into 32 bits.

int31 values are represented as int32 values (msb of the 32 bits is
the sign-bit); thus, int32 comparisons can be used for int31
comparisons. Moreover, int31 operations (+,-,*,/) can be implemented using
int32 operations, as long as we check for overflow after the operation
(is the result in the desired int31 interval?)

word31 values are represented as 31 bits, which means that bit
operations (&, |) may be implemented using word32 operations, but
arithmentic operations must consider signs explicitly. *)
 
fun wrapWord31 (js: Js) : Js =
    parJs(js & $" & 0x7FFFFFFF")

fun wrapWord32 (js: Js) : Js =
    parJs(js & $" & 0xFFFFFFFF")

fun callPrim1 n e =
    $n & parJs e

fun callPrim2 n e1 e2 =
    $n & seq[e1,e2]

fun chkOvfI32 (e: Js) : Js =
    callPrim1 "SmlPrims.chk_ovf_i32" e

fun chkOvfI31 (e: Js) : Js =
    callPrim1 "SmlPrims.chk_ovf_i31" e

fun pToJs2 name e1 e2 =
 case name of
      "__plus_int32ub" => chkOvfI32(e1 & $"+" & e2)
    | "__plus_int31" => chkOvfI31(e1 & $"+" & e2)
    | "__plus_word32ub" => wrapWord32(e1 & $"+" & e2)
    | "__plus_word31" => wrapWord31(e1 & $"+" & e2)
    | "__plus_real" => parJs(e1 & $"+" & e2)
    | "__minus_int32ub" => chkOvfI32(e1 & $"-" & e2)
    | "__minus_int31" => chkOvfI31(e1 & $"-" & e2)
    | "__minus_word32ub" => wrapWord32(e1 & $"-" & e2)
    | "__minus_word31" => wrapWord31(e1 & $"-" & e2)
    | "__minus_real" => parJs(e1 & $"-" & e2)
    | "__mul_int32ub" => chkOvfI32(e1 & $"*" & e2)
    | "__mul_int31" => chkOvfI31(e1 & $"*" & e2)
    | "__mul_word32ub" => wrapWord32(e1 & $"*" & e2)
    | "__mul_word31" => wrapWord31(e1 & $"*" & e2)
    | "__mul_real" => parJs(e1 & $"*" & e2)

    | "__less_int32ub" => parJs(e1 & $"<" & e2)
    | "__lesseq_int32ub" => parJs(e1 & $"<=" & e2)
    | "__greatereq_int32ub" => parJs(e1 & $">=" & e2)
    | "__greater_int32ub" => parJs(e1 & $">" & e2)
    | "__equal_int32ub" => parJs(e1 & $" == " & e2)
    | "__less_int31" => parJs(e1 & $"<" & e2)
    | "__lesseq_int31" => parJs(e1 & $"<=" & e2)
    | "__greatereq_int31" => parJs(e1 & $">=" & e2)
    | "__greater_int31" => parJs(e1 & $">" & e2)
    | "__equal_int31" => parJs(e1 & $" == " & e2)
    | "__less_word32ub" => parJs(e1 & $"<" & e2)
    | "__lesseq_word32ub" => parJs(e1 & $"<=" & e2)
    | "__greatereq_word32ub" => parJs(e1 & $">=" & e2)
    | "__greater_word32ub" => parJs(e1 & $">" & e2)
    | "__equal_word32ub" => parJs(e1 & $" == " & e2)
    | "__less_word31" => parJs(e1 & $"<" & e2)
    | "__lesseq_word31" => parJs(e1 & $"<=" & e2)
    | "__greatereq_word31" => parJs(e1 & $">=" & e2)
    | "__greater_word31" => parJs(e1 & $">" & e2)
    | "__equal_word31" => parJs(e1 & $" == " & e2)

    | "__less_real" => parJs(e1 & $"<" & e2)
    | "__lesseq_real" => parJs(e1 & $"<=" & e2)
    | "__greatereq_real" => parJs(e1 & $">=" & e2)
    | "__greater_real" => parJs(e1 & $">" & e2)
    | "__bytetable_sub" => parJs e1 & $".charCodeAt" & parJs e2
    | "concatStringML" => parJs(e1 & $"+" & e2)
    | "word_sub0" => parJs e1 & sqparJs e2
    | "word_table_init" => $"SmlPrims.wordTableInit" & seq[e1,e2]
    | "greatereqStringML" => parJs(e1 & $">=" & e2)
    | "greaterStringML" => parJs(e1 & $">" & e2)
    | "lesseqStringML" => parJs(e1 & $"<=" & e2)
    | "lessStringML" => parJs(e1 & $"<" & e2)

    | "__shift_right_unsigned_word32ub" => parJs(e1 & $" >>> " & e2)
    | "__shift_right_unsigned_word31" => parJs(e1 & $" >>> " & e2)
    | "__shift_right_signed_word32ub" => parJs(e1 & $" >> " & e2)
    | "__shift_right_signed_word31" => 
      IfJs(e1 & $" & -0x40000000", 
           parJs(parJs(e1 & $" | 0x80000000") & $" >> " & e2) & $" & 0x7FFFFFFF", 
           e1 & $" >> " & e2)

    | "__shift_left_word31" => wrapWord31(e1 & $" << " & parJs(e2 & $" & 0x1F"))
    | "__shift_left_word32ub" => wrapWord32(e1 & $" << " & parJs(e2 & $" & 0x1F"))

    | "__andb_word32ub" => parJs(e1 & $"&" & e2)
    | "__andb_word31" => parJs(e1 & $"&" & e2)
    | "__andb_word" => parJs(e1 & $"&" & e2)

    | "__orb_word32ub" => parJs(e1 & $"|" & e2)
    | "__orb_word31" => parJs(e1 & $"|" & e2)
    | "__orb_word" => parJs(e1 & $"|" & e2)

    | "__xorb_word32ub" => parJs(e1 & $"^" & e2)
    | "__xorb_word31" => parJs(e1 & $"^" & e2)
    | "__xorb_word" => parJs(e1 & $"^" & e2)
                       
    | "__quot_int31" => chkOvfI31(callPrim2 "SmlPrims.quot" e1 e2)
    | "__rem_int31" => parJs(e1 & $"%" & e2)
    | "__quot_int32ub" => chkOvfI32(callPrim2 "SmlPrims.quot" e1 e2)
    | "__rem_int32ub" => parJs(e1 & $"%" & e2)

    | "divFloat" => parJs(e1 & $"/" & e2)
    | "atan2Float" => $"Math.atan2" & seq[e1,e2]

    | "powFloat" => $"Math.pow" & seq[e1,e2]

    | "stringOfFloatFix" => parJs e2 & $".toFixed" & parJs e1
    | "stringOfFloatSci" => parJs e2 & $".toExponential" & parJs e1
    | "stringOfFloatGen" => parJs e2 & $".toPrecision" & parJs e1

    | _ => die ("pToJs2.unimplemented: " ^ name)

fun pToJs3 name e1 e2 e3 =
    case name 
     of "word_update0" => seq[parJs e1 & sqparJs e2 & $" = " & e3, 
                              $unitValueJs]
      | "__mod_int32ub" => $"SmlPrims.mod_i32" & seq[e1,e2,e3]
      | "__mod_int31" => $"SmlPrims.mod_i31" & seq[e1,e2,e3]
      | "__mod_word32ub" => $"SmlPrims.mod_w32" & seq[e1,e2,e3]
      | "__mod_word31" => $"SmlPrims.mod_w31" & seq[e1,e2,e3]
      | "__div_int32ub" => $"SmlPrims.div_i32" & seq[e1,e2,e3]
      | "__div_int31" => $"SmlPrims.div_i31" & seq[e1,e2,e3]
      | "__div_word32ub" => $"SmlPrims.div_w32" & seq[e1,e2,e3]
      | "__div_word31" => $"SmlPrims.div_w31" & seq[e1,e2,e3]

      | _ => die ("pToJs3.unimplemented: " ^ name)

fun pToJs1 name e =
    case name
     of "__bytetable_size" => parJs e & $".length"
      | "implodeCharsML" => callPrim1 "SmlPrims.implode" e
      | "implodeStringML" => callPrim1 "SmlPrims.concat" e
      | "charsToCharArray" => callPrim1 "SmlPrims.charsToCharArray" e
      | "charArraysConcat" => callPrim1 "SmlPrims.charArraysConcat" e
      | "printStringML" => callPrim1 "document.write" e
      | "exnNameML" => parJs e & $"[0]"
      | "id" => e
      | "word_table0" => $"Array" & parJs e
      | "table_size" => parJs e & $".length"
      | "chararray_to_string" => callPrim1 "SmlPrims.charArrayToString" e

      | "__neg_int32ub" => chkOvfI32($"-" & e)
      | "__neg_int31" => chkOvfI31($"-" & e)
      | "__neg_real" => parJs ($"-" & e)
      | "__abs_int32ub" => chkOvfI32(callPrim1 "Math.abs" e)
      | "__abs_int31" => chkOvfI31(callPrim1 "Math.abs" e)
      | "__abs_real" => callPrim1 "Math.abs" e

      | "__int32ub_to_int" => e
      | "__int_to_int32ub" => e
      | "__int31_to_int32ub" => e
      | "__int31_to_int" => e
      | "__word_to_word32ub" => e
      | "__word_to_word32ub_X" => e
      | "__word31_to_word32ub" => e
      | "__word31_to_word" => e
      | "__word32ub_to_word" => e

      | "__int32ub_to_int31" => chkOvfI31 e
      | "__int_to_int31" => chkOvfI31 e

      | "__word32ub_to_int" => chkOvfI32 e
      | "__word32ub_to_int32ub" => chkOvfI32 e
      | "__word32_to_int_X_JS" => callPrim1 "SmlPrims.w32_to_i32_X" e
      | "__word31_to_int_X_JS" => callPrim1 "SmlPrims.w31_to_i32_X" e
      | "__word32_to_int32_X_JS" => callPrim1 "SmlPrims.w32_to_i32_X" e
      | "__word31_to_word32ub_X" => callPrim1 "SmlPrims.w31_to_w32_X" e
      | "__word31_to_word_X" => callPrim1 "SmlPrims.w31_to_w32_X" e
      | "__int32_to_word32_JS" => callPrim1 "SmlPrims.i32_to_w32" e
      | "__int_to_word32" => callPrim1 "SmlPrims.i32_to_w32" e
      | "__int_to_word31_JS" => callPrim1 "SmlPrims.i32_to_w31" e
      | "__word32ub_to_word31" => wrapWord31 e
      | "__word_to_word31" => wrapWord31 e

      | "isnanFloat" => callPrim1 "isNaN" e
      | "sqrtFloat" => callPrim1 "Math.sqrt" e
      | "sinFloat" => callPrim1 "Math.sin" e
      | "cosFloat" => callPrim1 "Math.cos" e
      | "asinFloat" => callPrim1 "Math.asin" e
      | "acosFloat" => callPrim1 "Math.acos" e
      | "atanFloat" => callPrim1 "Math.atan" e
      | "sinhFloat" => callPrim1 "SmlPrims.sinh" e
      | "coshFloat" => callPrim1 "SmlPrims.cosh" e
      | "tanhFloat" => callPrim1 "SmlPrims.tanh" e
      | "lnFloat" => callPrim1 "Math.log" e
      | "expFloat" => callPrim1 "Math.exp" e

      | "realInt" => e

      | "floorFloat" => chkOvfI32(callPrim1 "Math.floor" e)
      | "ceilFloat" => chkOvfI32(callPrim1 "Math.ceil" e)
      | "truncFloat" => chkOvfI32(callPrim1 "SmlPrims.trunc" e)

      | _ => die ("pToJs1 unimplemented: " ^ name)

fun pToJs0 name =
    case name
     of "posInfFloat" => $"Infinity"
      | "negInfFloat" => parJs($"-Infinity")
      | _ => die ("pToJs0 unimplemented: " ^ name)

fun pToJs name [] = pToJs0 name
  | pToJs name [e] = pToJs1 name e
  | pToJs name [e1,e2] = pToJs2 name e1 e2
  | pToJs name [e1,e2,e3] = pToJs3 name e1 e2 e3
  | pToJs name _ = die ("pToJs unimplemented: " ^ name)

fun varJs (v:string) (js1:Js) (js2:Js) =
    case js2 of
      V(B,js3) => V((v,js1)::B,js3)
    | _ => V([(v,js1)],js2)
      
fun toJsSw (toJs: Exp->Js) (pp:'a->string) (L.SWITCH(e:Exp,bs:('a*Exp)list,eo: Exp option)) =
    let  val default = 
             case eo 
              of SOME e => $"default: " & returnJs (toJs e)
               | NONE => emp
         val cases = foldr(fn ((a,e),acc) => $"case" && $(pp a) && $": " & returnJs(toJs e) && acc) default bs 

    in
      stToE($"switch(" & unPar (toJs e) & $") { " & cases & $" }")
    end

fun booleanBranch bs eo =
    case eo of
      SOME e => 
      (case bs of
         [((c,_),e')] => 
         (if c = Con.con_FALSE then SOME(e,e')
          else (if c = Con.con_TRUE then SOME(e',e)
                else NONE))
       | _ => NONE)
    | NONE => 
      (case bs of
         [((c1,_),e1),((c2,_),e2)] => 
         (if c1 = Con.con_TRUE then SOME(e1,e2)
          else (if c1 = Con.con_FALSE then SOME(e2,e1)
                else NONE))
       | _ => NONE)

fun toJsSw_C (toJs: Exp->Js) (L.SWITCH(e:Exp,bs:((Con.con*Lvars.lvar option)*Exp)list,eo: Exp option)) =
    case booleanBranch bs eo 
     of SOME(e1,e2) => IfJs (toJs e,toJs e1,toJs e2)
      | NONE =>       
        let  
          fun pp (c,lvopt) = cToS0 c
          val default = 
              case eo 
               of SOME e => $"default: " & returnJs(toJs e)
                | NONE => emp
          val cases = foldr(fn ((a,e),acc) => $"case" && pp a && $": " & returnJs(parJs(toJs e)) && acc) default bs                     
        in stToE($"switch(" & parJs(toJs e) & $"[0]" & $") { " & cases & $" }")
        end

fun toJsSw_E (toJs: Exp->Js) (L.SWITCH(e:Exp,bs:((Excon.excon*Lvars.lvar option)*Exp)list,eo: Exp option)) =
    let 
      val cases =
          List.foldr (fn (((excon,_),e),acc) =>
                         $"if (tmp[0] == " & $(exconName excon) & $") { " & returnJs(toJs e) & $" };\n" & acc) emp bs
      val default = 
          case eo 
           of SOME e => returnJs(toJs e)
            | NONE => die "toJsSw_E.no default"
    in
      stToE($"var tmp = " & unPar(toJs e) & $";\n" & cases & default)
    end

fun mlToJsReal s =
    String.translate (fn #"~" => "-" | c => Char.toString c) s

fun mlToJsInt v =
    String.translate (fn #"~" => "-" | c => Char.toString c) (Int32.toString v)

fun toJs (C:Context.context) (e0:Exp) : Js = 
  case e0 of 
    L.VAR {lvar,...} => $(prLvar C lvar)
  | L.INTEGER (value,_) => if value < 0 then parJs($(mlToJsInt value)) else $(mlToJsInt value)
  | L.WORD (value,_) => $(Word32.fmt StringCvt.DEC value)
  | L.STRING s => sToS0 s
  | L.REAL s => if String.sub(s,0) = #"~" then parJs($(mlToJsReal s)) else $(mlToJsReal s)
  | L.PRIM(L.CONprim {con,...},nil) => 
    if con = Con.con_FALSE then $"false"
    else if con = Con.con_TRUE then $"true"
    else $"Array" & seq[cToS0 con]
  | L.PRIM(L.CONprim {con,...},[e]) => $"Array" & seq[cToS0 con, toJs C e]
  | L.PRIM(L.DECONprim _, [e]) => seq [toJs C e] & $"[1]"

  | L.PRIM(L.EXCONprim excon,nil) => (* nullary *)
    $(exconExn excon)
  | L.PRIM(L.EXCONprim excon,[e]) => (* unary *)
    $"Array" & seq[$(exconName excon), toJs C e]

  | L.PRIM(L.DEEXCONprim excon,[e]) => (* unary *)
    parJs (toJs C e) & $"[1]"

  | L.PRIM(L.RECORDprim, []) => $unitValueJs
  | L.PRIM(L.RECORDprim, es) => $"Array" & seq(map (toJs C) es)
  | L.PRIM(L.UB_RECORDprim, _) => die "UB_RECORD unimplemented"
  | L.PRIM(L.SELECTprim i,[e]) => seq [toJs C e] & $("[" ^ Int.toString i ^ "]")

  | L.PRIM(L.DEREFprim _, [e]) => parJs (toJs C e) & $"[0]"
  | L.PRIM(L.REFprim _, [e]) => seq[$"tmp = Array(1)", $"tmp[0] = " & toJs C e, $"tmp"]
  | L.PRIM(L.ASSIGNprim _, [e1,e2]) => seq[parJs (toJs C e1) & $"[0] = " & toJs C e2,
                                           $unitValueJs]
  | L.PRIM(L.DROPprim, [e]) => toJs C e
  | L.PRIM(L.DROPprim, _) => die "DROPprim unimplemented"
                                  
  | L.PRIM(L.EQUALprim _, [e1,e2]) => parJs (toJs C e1 & $"==" & toJs C e2)
                                    
  | L.FN {pat,body} => 
    let val lvs = map ($ o prLvar C o #1) pat
    in $"function" & seq lvs & $"{ " & returnJs(toJs C body) & $" }"
    end
  | L.LET {pat=[p],bind,scope} => 
    let val lv = #1 p
    in varJs (prLvar C lv) 
             (unPar(toJs C bind))
             (toJs C scope)
    end
  | L.LET {pat=[],bind,scope} => (* memo: why not sequence? *)
    varJs ("__dummy") 
          (unPar(toJs C bind))
          (toJs C scope)
  | L.LET {pat,bind,scope} => 
    let val lvs = map #1 pat
        val binds = case bind of L.PRIM(UB_RECORDprim,binds) => binds
                               | _ => die "LET.unimplemented"
        fun loop (nil,nil) = (toJs C scope)
          | loop (lv::lvs,b::bs) =
            varJs (prLvar C lv) 
                  (unPar(toJs C b))
                  (loop(lvs,bs))
          | loop _ = die "LET.mismatch"
    in 
      loop(lvs,binds)
    end
  | L.FIX{functions,scope} => 
    let val C' = foldl(fn(lvar,C) => Context.add C lvar) C (map #lvar functions)
    in foldl (fn ({lvar=f_lv,bind=L.FN{pat,body},...},acc) =>
                 let val lvs = map ($ o prLvar C o #1) pat
                 in varJs (prLvar C f_lv) 
                          ($("function " ^ prLvar C' f_lv) & seq lvs & $"{ " & returnJs(toJs C' body) & $" }")
                          acc
                 end
               | _ => die "toJs.malformed FIX") (toJs C scope) functions
    end
  | L.APP(e1,L.PRIM(L.UB_RECORDprim, es)) => toJs C e1 & seq(map (toJs C) es)
  | L.APP(e1,e2) => toJs C e1 & parJs(toJs C e2)
                    
  | L.SWITCH_I {switch,precision} => toJsSw (toJs C) mlToJsInt switch
  | L.SWITCH_W {switch,precision} => toJsSw (toJs C) (Word32.fmt StringCvt.DEC) switch
  | L.SWITCH_S switch => toJsSw (toJs C) toJSString switch
  | L.SWITCH_C switch => toJsSw_C (toJs C) switch
  | L.SWITCH_E switch => toJsSw_E (toJs C) switch

  | L.PRIM(L.EXPORTprim {name,instance_arg,instance_res},exps) => 
    die "toJs.PRIM(EXPORTprim) unimplemented"
  | L.PRIM(L.CCALLprim {name,...},exps) => 
    (case name of
       "execStmtJS" =>
       (case exps 
         of L.STRING s :: L.STRING argNames :: args =>  (* static code *)
            ($("(function (" ^ argNames ^ ") { " ^ s ^ " })") & seq(map (toJs C) args))
          | s :: argNames :: args => (* dynamic code *)
            parJs(parJs($"new Function" & seq[toJs C argNames, toJs C s])
                       & seq(map (toJs C) args))
          | _ => die "toJs.execStmtJS : string-->string-->args")
     | "callJS" => 
       (case exps 
         of L.STRING f :: args =>  (* static code *)
            ($f & seq(map (toJs C) args))
          | f :: args => (* dynamic code *)
            let val xs = ((String.concatWith ",") o #2)
                         (foldl (fn (_,(i,acc)) => (i+1,"a" ^ Int.toString i::acc)) (0,nil) args)
            in
              parJs(parJs($"new Function" & seq[$("\"" ^ xs ^ "\""), $"\"return \" + " & toJs C f & $(" + \"(" ^ xs ^ ")\"")])
                         & seq(map (toJs C) args))
            end
          | _ => die "toJs.callJS : string-->args")
     | _ => pToJs name (map (toJs C) exps)
    )
  | L.PRIM _ => die "toJs.PRIM unimplemented"
  | L.FRAME {declared_lvars, declared_excons} => $unitValueJs
(*
    let val lvs = map #lvar declared_lvars
    in seq([$"frame = new Object()"] 
           @ map (fn lv => $"frame." & $(prLvar C lv) & $" = " & $(prLvar C lv)) lvs 
           @ [$"frame"])
    end
*)
  | L.HANDLE (e1,e2) => (* memo: avoid capture of variable e! *)
    let val lv = Lvars.newLvar()
        val v = prLvar C lv
    in stToE ($"try { " & returnJs(toJs C e1) & $" } catch(" & $v & $") { " & returnJs(parJs(toJs C e2) & parJs($v)) & $" }")
    end
  | L.EXCEPTION (excon,SOME _,scope) => (* unary *)
    let val s = Excon.pr_excon excon  (* for printing *)
    in varJs (exconName excon) (sToS s) (toJs C scope)
    end
  | L.EXCEPTION (excon,NONE,scope) => (* nullary; precompute exn value and store it in exconExn(excon)... *)
    let val s = Excon.pr_excon excon  (* for printing *)
      val exn_id = exconExn excon
    in varJs (exconName excon) (sToS s) 
             (varJs (exconExn excon) ($"Array" & seq[$(exconName excon)])
                    (toJs C scope))
    end
  | L.RAISE(e,_) => stToE ($"throw" & parJs(toJs C e) & $";\n")

val toJs = fn L.PGM(_,e) => 
              let 
                val (lvars,excons) = LambdaBasics.exports e
                val _ = setFrameLvars lvars
                val _ = setFrameExcons excons
                val _ = resetBase()
                val js = toJs Context.empty e
              in case getLocalBase() of
                   SOME b => $b & $" = {};\n" & js
                 | NONE => js
              end

fun toString (js:Js) : string = 
    let
      fun elim js =
          case js of
            $ _ => js
          | j1 & j2 => elim j1 & elim j2
          | V (B,js_scope) => 
            let fun binds var = 
                    foldr(fn ((s,js),acc) => 
                             let val var = if CharVector.exists (fn #"." => true | _ => false) s then $"" else var
                             in var & $s && $"=" && elim js & $";\n" & acc
                             end) emp B
            in if js_scope = $"" then binds emp
               else stToE(binds ($"var ") & returnJs(elim js_scope))
            end
          | Par js => Par (elim js)
          | StToE js => StToE (elim js)
          | returnJs js => returnJs(elim js)
          | IfJs(e1,e2,e3) => IfJs(elim e1,elim e2,elim e3)
      fun strs b ($s,acc) = s::acc
        | strs b (js1&js2,acc) = strs b (js1,strs b (js2,acc))
        | strs b (Par js,acc) = "("::strs false (js,")"::acc)
        | strs b (returnJs js,acc) = 
          (case unPar js of
             StToE js => strs b (js,acc)
           | IfJs(e,e1,e2) => strs false ($"if " & parJs e & $" { " & returnJs e1 & $" } else { " & returnJs e2 & $" };\n", acc)
           | js => strs false ($"return " & js & $";\n",acc))
        | strs b (IfJs(e,e1,e2),acc) = strs false (parJs(parJs e & $"?" & parJs e1 & $":" & parJs e2),acc)
        | strs b (StToE js,acc) = 
          if b then strs false ($"__dummy = function(){ " & js & $" }()",acc)
          else strs false ($"function(){ " & js & $" }()",acc)
        | strs _ _ = die "toString.strs"
    in String.concat(strs true (elim js,nil))
    end handle ? => (print "Error in toString\n"; raise ?)

fun toFile (f,js) : unit = 
    let val os = TextIO.openOut f
    in 
      ( TextIO.output(os,toString js) ; TextIO.closeOut os )
      handle ? => (TextIO.closeOut os; raise ?)
    end

fun pp_list ss = "[" ^ String.concatWith "," ss ^ "]"

fun exports (L.PGM(_,e)) =
    let val (ls,es) = LambdaBasics.exports e
        val ss = map prLvarExport ls @ map exconExnExport es 
(*        val _ = print ("Exports: " ^ pp_list ss ^ "\n") *)
    in ss
    end

fun imports (L.PGM(_,e)) =
    let val (ls,es) = LambdaBasics.freevars e
        val ss = map (prLvar Context.empty) ls @ map exconExn es 
(*        val _ = print ("Imports: " ^ pp_list ss ^ "\n") *)
    in ss
    end

end
