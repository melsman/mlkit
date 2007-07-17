structure ExpToJs : EXP_TO_JS =
struct

fun die s = (print (s ^"\n");raise Fail s)

structure L = LambdaExp

type LambdaPgm = L.LambdaPgm
type Exp = L.LambdaExp

datatype Js = $ of string | & of Js * Js | V of (string * Js) list * Js

infix & &&

val emp = $""

val symbolChars = "!%&$#+-/:<=>?@\\~`^|*"

fun isSymbol s = 
    Char.contains symbolChars (String.sub(s,0))
    handle _ => die "isSymbol.empty"

fun idfy0 (c:char) : string =
    case CharVector.findi (fn(_,e) => e = c) symbolChars
     of SOME(i,_) => Char.toString(Char.chr(97+i))
      | NONE => Char.toString c

fun idfy s =
    if isSymbol s then
      "__symbol_" ^ String.translate idfy0 s
    else s

fun prLvar lv =
    idfy(Lvars.pr_lvar lv)

fun prCon c =
    idfy(Con.pr_con c)

fun j1 && j2 =
  j1 & $" " & j2

fun sToS0 s : Js =
    $("\"" ^ String.toString s ^ "\"")

fun sToS s : Js = 
    $"new String(" & sToS0 s & $")"

fun cToS0 c : Js = 
    $("\"" ^ String.toString (Con.pr_con c) ^ "\"")

fun seq (jss : Js list) : Js =
  let fun loop xs =
        case xs
         of nil => $""
          | [x] => x
          | x::xs => x & $", " & loop xs 
  in $"(" & loop jss & $")"
  end

fun appi f es = 
  let fun loop (n,nil) = nil
        | loop (n,x::xs) = f(n,x) :: loop(n+1,xs)
  in loop (0, es)
  end

fun stToE (st : Js) : Js =
    $"(function(){ " & st & $" }())"

fun parJs (js: Js) : Js = $"(" & js & $")"

val unitValueJs = "0"

fun pToJs2 name e1 e2 =
 case name of
      "__plus_int32ub" => e1 & $"+" & e2
    | "__plus_int31" => e1 & $"+" & e2
    | "__plus_word32ub" => e1 & $"+" & e2
    | "__plus_word31" => e1 & $"+" & e2
    | "__plus_real" => e1 & $"+" & e2
    | "__minus_int32ub" => e1 & $"-" & e2
    | "__minus_int31" => e1 & $"-" & e2
    | "__minus_word32ub" => e1 & $"-" & e2 
    | "__minus_word31" => e1 & $"-" & e2 
    | "__minus_real" => e1 & $"-" & e2 
    | "__mul_int32ub" => e1 & $"*" & e2
    | "__mul_int31" => e1 & $"*" & e2
    | "__mul_word32ub" => e1 & $"*" & e2
    | "__mul_word31" => e1 & $"*" & e2
    | "__mul_real" => e1 & $"*" & e2

    | "__less_int32ub" => e1 & $"<" & e2 
    | "__lesseq_int32ub" => e1 & $"<=" & e2 
    | "__greatereq_int32ub" => e1 & $">=" & e2 
    | "__greater_int32ub" => e1 & $">" & e2 
    | "__equal_int32ub" => e1 & $" == " & e2 
    | "__less_int31" => e1 & $"<" & e2 
    | "__lesseq_int31" => e1 & $"<=" & e2 
    | "__greatereq_int31" => e1 & $">=" & e2 
    | "__greater_int31" => e1 & $">" & e2 
    | "__equal_int31" => e1 & $" == " & e2 
    | "__less_word32ub" => e1 & $"<" & e2 
    | "__lesseq_word32ub" => e1 & $"<=" & e2 
    | "__greatereq_word32ub" => e1 & $">=" & e2 
    | "__greater_word32ub" => e1 & $">" & e2 
    | "__equal_word32ub" => e1 & $" == " & e2 
    | "__less_word31" => e1 & $"<" & e2 
    | "__lesseq_word31" => e1 & $"<=" & e2 
    | "__greatereq_word31" => e1 & $">=" & e2 
    | "__greater_word31" => e1 & $">" & e2 
    | "__equal_word31" => e1 & $" == " & e2 

    | "__less_real" => e1 & $"<" & e2 
    | "__lesseq_real" => e1 & $"<=" & e2 
    | "__greatereq_real" => e1 & $">=" & e2 
    | "__greater_real" => e1 & $">" & e2 
(*
    | "__equal_real" => e1 & $" == " & e2 
*)
    | "__bytetable_sub" => parJs e1 & $".charCodeAt(" & e2 & $")"
    | "concatStringML" => e1 & $"+" & e2
    | "word_sub0" => parJs e1 & $"[" & e2 & $"]"
    | "word_table_init" => $"SmlPrims.wordTableInit" & seq[e1,e2]
    | "greatereqStringML" => e1 & $">=" & e2 
    | "greaterStringML" => e1 & $">" & e2 
    | "lesseqStringML" => e1 & $"<=" & e2 
    | "lessStringML" => e1 & $"<" & e2 
    | "__shift_right_unsigned_word32ub" => $"SmlPrims.shift_right_unsigned_word32" & seq[e1,e2]
    | "__shift_right_unsigned_word31" => $"SmlPrims.shift_right_unsigned_word31" & seq[e1,e2]
    | "__shift_right_signed_word32ub" => $"SmlPrims.shift_right_signed_word32" & seq[e1,e2]
    | "__shift_right_signed_word31" => $"SmlPrims.shift_right_signed_word31" & seq[e1,e2]

    | "__andb_word32ub" => e1 & $"&" & e2 
    | "__andb_word31" => e1 & $"&" & e2 
    | "__andb_word" => e1 & $"&" & e2 

    | "__orb_word32ub" => e1 & $"|" & e2 
    | "__orb_word31" => e1 & $"|" & e2 
    | "__orb_word" => e1 & $"|" & e2 

    | "__xorb_word32ub" => e1 & $"^" & e2 
    | "__xorb_word31" => e1 & $"^" & e2 
    | "__xorb_word" => e1 & $"^" & e2 
                       
    | "__quot_int31" => $"SmlPrims.quot_int31" & seq[e1,e2]
    | "__rem_int31" => $"SmlPrims.rem_int31" & seq[e1,e2]
    | "__quot_int32ub" => $"SmlPrims.quot_int32" & seq[e1,e2]
    | "__rem_int32ub" => $"SmlPrims.rem_int32" & seq[e1,e2]

    | "__shift_left_word31" => $"SmlPrims.shift_left_word31" & seq[e1,e2]
    | "__shift_left_word32ub" => $"SmlPrims.shift_left_word32" & seq[e1,e2]

    | "divFloat" => e1 & $"/" & e2 
    | "atan2Float" => $"Math.atan2" & seq[e1,e2]

    | "powFloat" => $"Math.pow" & seq[e1,e2]

    | _ => die ("pToJs2.unimplemented: " ^ name)

fun pToJs3 name e1 e2 e3 =
    case name 
     of "word_update0" => seq[parJs e1 & $"[" & e2 & $"] = " & e3, 
                              $unitValueJs]
      | "__mod_int32ub" => $"SmlPrims.mod_int32" & seq[e1,e2,e3]
      | "__mod_int31" => $"SmlPrims.mod_int32" & seq[e1,e2,e3]
      | "__mod_word32ub" => $"SmlPrims.mod_word32" & seq[e1,e2,e3]
      | "__mod_word31" => $"SmlPrims.mod_word31" & seq[e1,e2,e3]
      | "__div_int32ub" => $"SmlPrims.div_int32" & seq[e1,e2,e3]
      | "__div_int31" => $"SmlPrims.div_int32" & seq[e1,e2,e3]
      | "__div_word32ub" => $"SmlPrims.div_word32" & seq[e1,e2,e3]
      | "__div_word31" => $"SmlPrims.div_word31" & seq[e1,e2,e3]

      | "generalStringOfFloat" => $"SmlPrims.generalStringOfFloat" & seq[e1,e2,e3]

      | _ => die ("pToJs3.unimplemented: " ^ name)

fun callPrim1 n e =
    $n & parJs e

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
      | "chararray_to_string" => parJs e & $".join(\"\")"

      | "__neg_int32ub" => callPrim1 "SmlPrims.neg_int32" e
      | "__neg_int31" => callPrim1 "SmlPrims.neg_int31" e
      | "__neg_real" => parJs ($"-" & e)
      | "__abs_int32ub" => callPrim1 "SmlPrims.abs_int32" e
      | "__abs_int31" => callPrim1 "SmlPrims.abs_int31" e
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

      | "__int32ub_to_int31" => callPrim1 "SmlPrims.chk_ovf_int31" e
      | "__int_to_int31" => callPrim1 "SmlPrims.chk_ovf_int31" e

      | "__word32ub_to_int" => callPrim1 "SmlPrims.chk_ovf_int32" e
      | "__word32ub_to_int32ub" => callPrim1 "SmlPrims.chk_ovf_int32" e
      | "__word32ub_to_int_X" => callPrim1 "SmlPrims.word32_to_int32_X" e
      | "__word32ub_to_int32ub_X" => callPrim1 "SmlPrims.word32_to_int32_X" e
      | "__word31_to_word32ub_X" => callPrim1 "SmlPrims.word31_to_word32_X" e
      | "__word31_to_word_X" => callPrim1 "SmlPrims.word31_to_word32_X" e

      | "__word32ub_to_word31" => callPrim1 "SmlPrims.word32_to_word31" e
      | "__word_to_word31" => callPrim1 "SmlPrims.word32_to_word31" e

      | "__int32ub_to_word32ub" => callPrim1 "SmlPrims.int32_to_word32" e

      | "isnanFloat" => parJs (e & $"== NaN") 
      | "sqrtFloat" => callPrim1 "Math.sqrt" e
      | "sinFloat" => callPrim1 "Math.sin" e
      | "cosFloat" => callPrim1 "Math.cos" e
      | "asinFloat" => callPrim1 "Math.asin" e
      | "acosFloat" => callPrim1 "Math.acos" e
      | "atanFloat" => callPrim1 "Math.atan" e
      | "lnFloat" => callPrim1 "Math.log" e
      | "expFloat" => callPrim1 "Math.exp" e

      | "realInt" => e

      | "floorFloat" => callPrim1 "SmlPrims.floorFloat" e
      | "ceilFloat" => callPrim1 "SmlPrims.ceilFloat" e
      | "truncFloat" => callPrim1 "SmlPrims.truncFloat" e

      | "stringOfFloat" => callPrim1 "SmlPrims.stringOfFloat" e


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
      
fun toJsSw (toJs: Exp->Js) (sel:string) (pp:'a->string) (L.SWITCH(e:Exp,bs:('a*Exp)list,eo: Exp option)) =
    let  val default = 
             case eo 
              of SOME e => $"default: return" && toJs e
               | NONE => emp
         val cases = foldl(fn ((a,e),acc) => $"case" && $(pp a) && $": return" && toJs e && $"; break;" && acc) default bs 

    in
      stToE($"switch(" & toJs e & $sel & $") { " & cases & $" }")
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

fun exconName excon = "__exname_" ^ idfy(Excon.pr_excon excon)

fun toJsSw_C (toJs: Exp->Js) (L.SWITCH(e:Exp,bs:((Con.con*Lvars.lvar option)*Exp)list,eo: Exp option)) =
    case booleanBranch bs eo 
     of SOME(e1,e2) => parJs (toJs e) & $"?" & parJs (toJs e1) & $":" & parJs (toJs e2)
      | NONE =>       
        let  
          fun pp (c,lvopt) = cToS0 c
          val default = 
              case eo 
               of SOME e => $"default: return" && toJs e
                | NONE => emp
          val cases = foldl(fn ((a,e),acc) => $"case" && pp a && $": return" && toJs e && $"; break;" && acc) default bs                     
        in stToE($"switch(" & toJs e & $"[0]" & $") { " & cases & $" }")
        end

fun toJsSw_E (toJs: Exp->Js) (L.SWITCH(e:Exp,bs:((Excon.excon*Lvars.lvar option)*Exp)list,eo: Exp option)) =
    let 
      val cases =
          List.foldl (fn (((excon,_),e),acc) =>
                         $"if (tmp[0] = " & $(exconName excon) & $") { return " 
                          & parJs (toJs e) & $" };\n" & acc) emp bs
      val default = 
          case eo 
           of SOME e => $"return" && toJs e & $";\n"
            | NONE => die "toJsSw_E.no default"
    in
      stToE($"var tmp = " & toJs e & $";\n" & cases & default)
    end

fun toJs (e0:Exp) : Js = 
  case e0 of 
    L.VAR {lvar,...} => $(prLvar lvar)
  | L.INTEGER (value,_) => $(Int32.toString value)
  | L.WORD (value,_) => $(Word32.fmt StringCvt.DEC value)
  | L.STRING s => sToS0 s
  | L.REAL s => $s
  | L.PRIM(L.CONprim {con,...},nil) => 
    if con = Con.con_FALSE then $"false"
    else if con = Con.con_TRUE then $"true"
    else $"Array" & seq[cToS0 con]
  | L.PRIM(L.CONprim {con,...},[e]) => $"Array" & seq[cToS0 con, toJs e]
  | L.PRIM(L.DECONprim _, [e]) => seq [toJs e] & $"[1]"

  | L.PRIM(L.EXCONprim excon,nil) => (* nullary *)
    $"Array" & seq[$(exconName excon)]
  | L.PRIM(L.EXCONprim excon,[e]) => (* unary *)
    $"Array" & seq[$(exconName excon), toJs e]

  | L.PRIM(L.DEEXCONprim excon,[e]) => (* unary *)
    parJs (toJs e) & $"[1]"

  | L.PRIM(L.RECORDprim, []) => $unitValueJs
  | L.PRIM(L.RECORDprim, es) => $"Array" & seq(map toJs es)
  | L.PRIM(L.UB_RECORDprim, _) => die "UB_RECORD unimplemented"
  | L.PRIM(L.SELECTprim i,[e]) => seq [toJs e] & $("[" ^ Int.toString i ^ "]")

  | L.PRIM(L.DEREFprim _, [e]) => parJs (toJs e) & $"[0]"
  | L.PRIM(L.REFprim _, [e]) => seq[$"tmp = Array(1)", $"tmp[0] = " & toJs e, $"tmp"]
  | L.PRIM(L.ASSIGNprim _, [e1,e2]) => seq[parJs (toJs e1) & $"[0] = " & toJs e2,
                                           $unitValueJs]
  | L.PRIM(L.DROPprim, [e]) => toJs e
  | L.PRIM(L.DROPprim, _) => die "DROPprim unimplemented"
                                  
  | L.PRIM(L.EQUALprim _, [e1,e2]) => parJs (toJs e1 & $"==" & toJs e2)
                                    
  | L.FN {pat,body} => 
    let val lvs = map ($ o prLvar o #1) pat
    in $"function" & seq lvs & $"{ return(" & toJs body & $"); }"
    end
  | L.LET {pat=[p],bind,scope} => 
    let val lv = #1 p
    in varJs (prLvar lv) 
             (toJs bind)
             (toJs scope)
    end
  | L.LET {pat=[],bind,scope} => 
    varJs ("__dummy") 
          (toJs bind)
          (toJs scope)
  | L.LET {pat,bind,scope} => 
    let val lvs = map #1 pat
        val binds = case bind of L.PRIM(UB_RECORDprim,binds) => binds
                               | _ => die "LET.unimplemented"
        fun loop (nil,nil) = (toJs scope)
          | loop (lv::lvs,b::bs) =
            varJs (prLvar lv) 
                  (toJs b)
                  (loop(lvs,bs))
          | loop _ = die "LET.mismatch"
    in 
      loop(lvs,binds)
    end
  | L.FIX{functions,scope} => 
    foldl (fn ({lvar=f_lv,bind=L.FN{pat,body},...},acc) =>
              let val lvs = map ($ o prLvar o #1) pat
              in varJs (prLvar f_lv) 
                       ($("function " ^ prLvar f_lv) & seq lvs & $"{ return(" & toJs body & $"); }")
                       acc
              end
            | _ => die "toJs.malformed FIX") (toJs scope) functions

  | L.APP(e1,L.PRIM(L.UB_RECORDprim, es)) => toJs e1 & seq(map toJs es)
  | L.APP(e1,e2) => toJs e1 & $"(" & toJs e2 & $")"
                    
  | L.SWITCH_I {switch,precision} => toJsSw toJs "" Int32.toString switch
  | L.SWITCH_W {switch,precision} => toJsSw toJs "" (Word32.fmt StringCvt.DEC) switch
  | L.SWITCH_S switch => toJsSw toJs "" (fn s => "\"" ^ String.toString s ^ "\"") switch
  | L.SWITCH_C switch => toJsSw_C toJs switch
  | L.SWITCH_E switch => toJsSw_E toJs switch
                    
  | L.PRIM(L.CCALLprim {name,...},exps) => pToJs name (map toJs exps)
  | L.PRIM _ => die "toJs.PRIM unimplemented"
  | L.FRAME {declared_lvars, declared_excons} =>  emp
(*
    let val lvs = map #lvar declared_lvars
    in seq([$"frame = new Object()"] 
           @ map (fn lv => $"frame." & $(prLvar lv) & $" = " & $(prLvar lv)) lvs 
           @ [$"frame"])
    end
*)
  | L.HANDLE (e1,e2) => (* memo: avoid capture of variable e! *)
    stToE ($"try { return(" & toJs e1 & $"); } catch(e) { return " && parJs(toJs e2) & $"(e); }")

  | L.EXCEPTION (excon,_,scope) => (* nullary or unary *)
    let val s = Excon.pr_excon excon  (* for printing *)
    in varJs (exconName excon) (sToS s) (toJs scope)
    end
  | L.RAISE(e,_) => stToE ($"throw(" & toJs e & $")")

val toJs = fn (L.PGM(_,e)) => toJs e

fun toString (js:Js) : string = 
    let
      fun elim js =
          case js of
            $ _ => js
          | j1 & j2 => elim j1 & elim j2
          | V (B,js_scope) => 
            let fun binds var = 
                    foldr(fn ((s,js),acc) => var & $s && $"=" && elim js & $";\n" & acc) emp B
            in if js_scope = $"" then binds emp
               else $"(function(){ " & binds ($"var ") & $"; return (" & elim js_scope & $"); })()"                 
            end
      fun strs ($s,acc) = s::acc
        | strs (js1&js2,acc) = strs(js1,strs(js2,acc))
        | strs _ = die "toString.strs"
    in String.concat(strs(elim js,nil))
    end handle ? => (print "Error in toString\n"; raise ?)

fun toFile (f,js) : unit = 
    let val os = TextIO.openOut f
    in 
      ( TextIO.output(os,toString js) ; TextIO.closeOut os )
      handle ? => (TextIO.closeOut os; raise ?)
    end

end
