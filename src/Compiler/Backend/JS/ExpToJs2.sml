structure ExpToJs : EXP_TO_JS = struct

structure L = LambdaExp
structure J = JsAst

fun die s = (print (s ^ "\n"); raise Fail s)

type LambdaPgm = L.LambdaPgm
type Exp = L.LambdaExp
type lvar = Lvars.lvar
type excon = Excon.excon

datatype conRep = (* representation of value constructors for datatypes *)
         BOOL of bool
       | ENUM of int
       | STD of int
       | UNBOXED_NULL
       | UNBOXED_UNARY

(* Replacing lvars in an expression to other lvars *)
type rep = (lvar, lvar) FinMapEq.map

fun replace_lvs (rep:rep) (e as L.VAR{lvar,instances,regvars}) : Exp =
    (case FinMapEq.lookup Lvars.eq rep lvar of
         SOME lv => L.VAR{lvar=lv,instances=instances,regvars=nil}
       | NONE => e)
  | replace_lvs rep (L.FRAME _) = die "rename_lvs: FRAME construct not expected"
  | replace_lvs rep e = LambdaBasics.map_lamb (replace_lvs rep) e


(* Environments map value constructors to information about their
 * representation. *)

structure Env = struct
 structure M = Con.Map
 type t = conRep M.map
 val empty : t = M.empty
 val initial : t =
     let open TyName
     in M.fromList [
        (Con.con_FALSE, BOOL false),
        (Con.con_TRUE, BOOL true),
        (Con.con_NIL, UNBOXED_NULL),
        (Con.con_CONS, UNBOXED_UNARY),
        (Con.con_QUOTE, STD 0),
        (Con.con_ANTIQUOTE, STD 1),
        (Con.con_REF, STD 0),
        (Con.con_INTINF, STD 0)
        ]
     end
 val plus : t * t -> t = M.plus
 fun restrict (e,l) : t = M.restrict (Con.pr_con,e,l)
 val enrich : t * t -> bool = M.enrich (op =)

 val pu_conRep =
     Pickle.dataGen
         ("conRep",
          fn BOOL b => 0
           | ENUM i => 1
           | STD i => 2
           | UNBOXED_NULL => 3
           | UNBOXED_UNARY => 4,
          [fn _ => Pickle.con1 BOOL (fn BOOL b => b | _ => die "pu_conRep.BOOL") Pickle.bool,
           fn _ => Pickle.con1 ENUM (fn ENUM i => i | _ => die "pu_conRep.ENUM") Pickle.int,
           fn _ => Pickle.con1 STD (fn STD i => i | _ => die "pu_conRep.STD") Pickle.int,
           Pickle.con0 UNBOXED_NULL,
           Pickle.con0 UNBOXED_UNARY])

 val pu = M.pu Con.pu pu_conRep

 fun fromDatbinds (L.DATBINDS dbss) : t =
     let
       fun flatten (es:t list) : t =
           List.foldl plus empty es
       val all_nullary =
           List.all (fn (_,NONE) => true | _ => false)
       fun onAll C cs =
           #1(List.foldl(fn ((c,_),(e,i)) => (M.add(c,C i,e),i+1)) (M.empty,0) cs)
       fun unboxable [(c0,NONE),(c1,SOME _)] = SOME(c1,c0)
         | unboxable [(c1,SOME _),(c0,NONE)] = SOME(c1,c0)
         | unboxable _ = NONE
       fun fromDb (tvs,t,cs) =
             if TyName.unboxed t then
               if all_nullary cs then
                 onAll ENUM cs
               else
                 case unboxable cs of
                   SOME (c_unary,c_nullary) =>
                   M.fromList [(c_unary,UNBOXED_UNARY),(c_nullary,UNBOXED_NULL)]
                 | NONE => onAll STD cs
             else  onAll STD cs
     in flatten(map (flatten o map fromDb) dbss)
     end
end

(********
Contexts contain an environment part and information about mutually
recursive bound functions; see below.

Mutually recursive functions are compiled into properties of a
common object:

    var x = {};
    x.f = function(...){... x.g(...) ...};
    x.g = function(...){... x.f(...) ...};
    f = x.f;
    g = x.g;

Here x is a fresh variable. To map f and g to x.f and x.g, we make use
of a map (:lvar->lvar) in the context.
*********)
structure Context :> sig type t
                       val mk : Env.t -> t
                       val empty : t
                       val add : t -> lvar * string -> t
                       val lookup : t -> lvar -> string option
                       val envOf : t -> Env.t
                     end =
struct
  type t = Env.t * string Lvars.Map.map
  fun mk e = (e,Lvars.Map.empty)
  val empty = (Env.empty, Lvars.Map.empty)
  fun add (e,c) (lv,s) = (e,Lvars.Map.add(lv,s,c))
  fun envOf (e,c) = e
  fun lookup (e,c) lv = Lvars.Map.lookup c lv
end

(* Pretty printing of variables *)
local
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
      let val b = replaceString (".mlb-", "$0") b
          val b = replaceString (".sml1", "$1") b
      in
        String.translate (fn #"." => "$2" | #"-" => "$3" | c => Char.toString c) b
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

  (* convert identifier names such as "v343" and "var322" into "v" - the name key
   * is appended later, which will make the identifiers unique... *)

  val idfy =
      let
        fun restDigits i s =
            CharVectorSlice.all Char.isDigit (CharVectorSlice.slice(s,i,NONE))
        fun simplify s =
              (if String.sub(s,0) = #"v" then
                 if restDigits 1 s then "v"
                 else
                   if String.sub(s,1) = #"a"
                      andalso String.sub(s,2) = #"r"
                      andalso restDigits 3 s then "v"
                   else s
               else s) handle _ => s
      in fn s => simplify(idfy s)
      end

  fun patch n f s opt =
      case opt of
        NONE =>
        let val (k,b) = Name.key n
            val s = s ^ "$" ^ Int.toString k
        in if Name.rigid n orelse f() then
             let val b = normalizeBase b
             in b ^ "." ^ s
             end
           else s
        end
      | SOME s0 => s0 ^ ".$" ^ s
in
  fun setFrameLvars xs = frameLvars := xs
  fun setFrameExcons xs = frameExcons := xs
  fun resetBase() = localBase := NONE

  fun pr_lv lv = idfy(Lvars.pr_lvar lv)

  fun prLvar C lv =
      patch (Lvars.name lv)
            (isFrameLvar lv)
            (pr_lv lv)
            (Context.lookup C lv)   (* Fix variables *)
  fun prLvarExport lv =
      patch (Lvars.name lv) (fn() => true) (pr_lv lv) NONE
  fun exconName e =
      patch (Excon.name e) (isFrameExcon e) ("en$" ^ idfy(Excon.pr_excon e)) NONE
  fun exconExn e =
      patch (Excon.name e) (isFrameExcon e) ("exn$" ^ idfy(Excon.pr_excon e)) NONE
  fun exconExnExport e =
      patch (Excon.name e) (fn() => true) ("exn$" ^ idfy(Excon.pr_excon e)) NONE
  fun getLocalBase() = !localBase
  fun fresh_fixvar() =
      prLvar Context.empty (Lvars.new_named_lvar "fix")
  fun fresh_tmpvar() =
      prLvar Context.empty (Lvars.new_named_lvar "t")
  fun pr_label lv = "lab$" ^ pr_lv lv
end

infix &
fun x & y = J.Seq[x,y]

fun sToS s : J.exp = J.New("String",[J.Cnst(J.Str s)])

datatype cont = RetCont of (lvar*J.id list) option   (* lvar is the fix-bound variable and ids are the function parameters *)
              | IdCont of J.id
              | NxtCont

datatype ret = S of cont -> J.stmt | E of J.exp

fun wrapExp (k:cont) (e:J.exp) : J.stmt =
    case k of
      IdCont id => J.Exp(J.Prim("=",[J.Id id,e]))
    | RetCont _ => J.Return e
    | NxtCont => J.Exp e

fun wrapRet (k:cont) (r:ret) : J.stmt =
    case r of
      S f => f k
    | E e => wrapExp k e

fun resolveE (arg: J.stmt option * 'a) (f: 'a -> J.exp) : ret =
    case arg of
      (SOME s,es') => S(fn k => s & wrapExp k (f es'))
    | (NONE,es') => E(f es')

fun resolveS (arg: J.stmt option * 'a) (f: 'a -> cont -> J.stmt) : ret =
    case arg of
      (SOME s,es') => S(fn k => s & f es' k)
    | (NONE,es') => S(fn k => f es' k)

fun jint i = J.Cnst(J.Int(Int32.fromInt i))
val jcnst0 = jint 0
val jcnst1 = jint 1
val junit = jcnst0
val jtrue = J.Cnst(J.Bool true)
val jfalse = J.Cnst(J.Bool false)
val jnull = J.Cnst J.Null

(* Compilation of value constructors *)
fun ppCon C c : J.cnst =
    case Env.M.lookup (Context.envOf C) c of
      SOME(STD i) => J.Int(Int32.fromInt i)
    | SOME(ENUM i) => J.Int(Int32.fromInt i)
    | SOME(BOOL true) => J.Bool true
    | SOME(BOOL false) => J.Bool false
    | SOME UNBOXED_NULL => J.Null
    | _ => die "ppCon"

fun ppConNullary C c : J.exp =
    case Env.M.lookup (Context.envOf C) c of
      SOME(STD i) => J.Array[jint i]
    | SOME(ENUM i) => jint i
    | SOME(BOOL true) => jtrue
    | SOME(BOOL false) => jfalse
    | SOME UNBOXED_NULL => jnull
    | SOME UNBOXED_UNARY => die "ppConNullary: UNBOXED_UNARY applied to argument"
    | NONE => die ("ppConNullary: constructor " ^ Con.pr_con c ^ " not in context")

fun ppConUnary C c e : J.exp =
    case Env.M.lookup (Context.envOf C) c of
      SOME(STD i) => J.Array[jint i,e]
    | SOME(ENUM i) => die "ppConUnary: ENUM"
    | SOME(BOOL _) => die "ppConUnary: BOOL"
    | SOME UNBOXED_NULL => die "ppConUnary: UNBOXED_NULL"
    | SOME UNBOXED_UNARY => e
    | NONE => die ("ppConUnary: constructor " ^ Con.pr_con c ^ " not in context")

(* Compilation of primitives *)

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
arithmentic operations must consider signs explicitly.
*)

fun jandw e w = J.Prim("&",[e,J.Cnst(J.Word w)])
fun jorw e w = J.Prim("|",[e,J.Cnst(J.Word w)])

fun wrapWord31 (e: J.exp) : J.exp = jandw e 0wx7FFFFFFF

fun wrapWord32 (e: J.exp) : J.exp = jandw e 0wxFFFFFFFF

fun callPrim0 n = J.App(J.Id n,[])

fun callPrim1 n e = J.App(J.Id n, [e])

fun callPrim2 n e1 e2 = J.App(J.Id n, [e1,e2])

fun callPrim3 n e1 e2 e3 = J.App(J.Id n, [e1,e2,e3])

fun chkOvfI32 (e: J.exp) : J.exp =
    callPrim1 "SmlPrims.chk_ovf_i32" e

fun chkOvfI31 (e: J.exp) : J.exp =
    callPrim1 "SmlPrims.chk_ovf_i31" e

fun pToJs2 name e1 e2 : J.exp =
    case name of
      "__plus_int32ub" => chkOvfI32(J.Prim("+", [e1,e2]))
    | "__plus_int31" => chkOvfI31(J.Prim("+", [e1,e2]))
    | "__plus_word32ub" => wrapWord32(J.Prim("+", [e1,e2]))
    | "__plus_word31" => wrapWord31(J.Prim("+", [e1,e2]))
    | "__plus_real" => J.Prim("+", [e1,e2])
    | "__plus_f64" => J.Prim("+", [e1,e2])
    | "__minus_int32ub" => chkOvfI32(J.Prim("-", [e1,e2]))
    | "__minus_int31" => chkOvfI31(J.Prim("-", [e1,e2]))
    | "__minus_word32ub" => wrapWord32(J.Prim("-", [e1,e2]))
    | "__minus_word31" => wrapWord31(J.Prim("-", [e1,e2]))
    | "__minus_real" => J.Prim("-", [e1,e2])
    | "__minus_f64" => J.Prim("-", [e1,e2])
    | "__mul_int32ub" => chkOvfI32(J.Prim("*", [e1,e2]))
    | "__mul_int31" => chkOvfI31(J.Prim("*", [e1,e2]))
    | "__mul_word32ub" => wrapWord32(J.Prim("*", [e1,e2]))
    | "__mul_word31" => wrapWord31(J.Prim("*", [e1,e2]))
    | "__mul_real" => J.Prim("*", [e1,e2])
    | "__mul_f64" => J.Prim("*", [e1,e2])

    | "__less_int32ub" => J.Prim("<", [e1,e2])
    | "__lesseq_int32ub" => J.Prim("<=", [e1,e2])
    | "__greatereq_int32ub" => J.Prim(">=", [e1,e2])
    | "__greater_int32ub" => J.Prim(">", [e1,e2])
    | "__equal_int32ub" => J.Prim("==", [e1,e2])
    | "__less_int31" => J.Prim("<", [e1,e2])
    | "__lesseq_int31" => J.Prim("<=", [e1,e2])
    | "__greatereq_int31" => J.Prim(">=", [e1,e2])
    | "__greater_int31" => J.Prim(">", [e1,e2])
    | "__equal_int31" => J.Prim("==", [e1,e2])
    | "__less_word32ub" => J.Prim("<", [e1,e2])
    | "__lesseq_word32ub" => J.Prim("<=", [e1,e2])
    | "__greatereq_word32ub" => J.Prim(">=", [e1,e2])
    | "__greater_word32ub" => J.Prim(">", [e1,e2])
    | "__equal_word32ub" => J.Prim("==", [e1,e2])
    | "__less_word31" => J.Prim("<", [e1,e2])
    | "__lesseq_word31" => J.Prim("<=", [e1,e2])
    | "__greatereq_word31" => J.Prim(">=", [e1,e2])
    | "__greater_word31" => J.Prim(">", [e1,e2])
    | "__equal_word31" => J.Prim("==", [e1,e2])

    | "__less_real" => J.Prim("<", [e1,e2])
    | "__lesseq_real" => J.Prim("<=", [e1,e2])
    | "__greatereq_real" => J.Prim(">=", [e1,e2])
    | "__greater_real" => J.Prim(">", [e1,e2])

    | "__less_f64" => J.Prim("<", [e1,e2])
    | "__lesseq_f64" => J.Prim("<=", [e1,e2])
    | "__greatereq_f64" => J.Prim(">=", [e1,e2])
    | "__greater_f64" => J.Prim(">", [e1,e2])

    | "__bytetable_sub" => J.App(J.Prop(e1,"charCodeAt"),[e2])
    | "concatStringML" => J.Prim("+", [e1,e2])
    | "word_sub0" => J.Sub(e1,e2)
    | "word_table_init" => callPrim2 "SmlPrims.wordTableInit" e1 e2
    | "greatereqStringML" => J.Prim(">=", [e1,e2])
    | "greaterStringML" => J.Prim(">", [e1,e2])
    | "lesseqStringML" => J.Prim("<=", [e1,e2])
    | "lessStringML" => J.Prim("<", [e1,e2])

    | "__shift_right_unsigned_word32ub" => J.Prim(">>>", [e1,e2])
    | "__shift_right_unsigned_word31" => J.Prim(">>>", [e1,e2])
    | "__shift_right_signed_word32ub" => J.Prim(">>", [e1,e2])
    | "__shift_right_signed_word31" =>
      J.IfExp(J.Prim("&", [e1,J.Id "-0x40000000"]),
              jandw (J.Prim(">>", [jorw e1 0wx80000000, e2])) 0wx7FFFFFFF,
              J.Prim(">>", [e1,e2]))

    | "__shift_left_word31" => wrapWord31(J.Prim("<<",[e1,jandw e2 0wx1F]))
    | "__shift_left_word32ub" => wrapWord32(J.Prim("<<",[e1,jandw e2 0wx1F]))

    | "__andb_word32ub" => J.Prim("&",[e1,e2])
    | "__andb_word31" => J.Prim("&",[e1,e2])
    | "__andb_word" => J.Prim("&",[e1,e2])

    | "__orb_word32ub" => J.Prim("|",[e1,e2])
    | "__orb_word31" => J.Prim("|",[e1,e2])
    | "__orb_word" => J.Prim("|",[e1,e2])

    | "__xorb_word32ub" => J.Prim("^",[e1,e2])
    | "__xorb_word31" => J.Prim("^",[e1,e2])
    | "__xorb_word" => J.Prim("^",[e1,e2])

    | "__quot_int31" => chkOvfI31(callPrim2 "SmlPrims.quot" e1 e2)
    | "__rem_int31" => J.Prim("%", [e1,e2])
    | "__quot_int32ub" => chkOvfI32(callPrim2 "SmlPrims.quot" e1 e2)
    | "__rem_int32ub" => J.Prim("%",[e1,e2])

    | "divFloat" => J.Prim("/",[e1,e2])
    | "__div_f64" => J.Prim("/",[e1,e2])
    | "atan2Float" => callPrim2 "Math.atan2" e1 e2

    | "powFloat" => callPrim2 "Math.pow" e1 e2

    | "stringOfFloatFix" => J.App(J.Prop(e2,"toFixed"),[e1])
    | "stringOfFloatSci" => J.App(J.Prop(e2,"toExponential"),[e1])
    | "stringOfFloatGen" => J.App(J.Prop(e2,"toPrecision"),[e1])

    | _ => die ("pToJs2.unimplemented: " ^ name)

fun pToJs3 name e1 e2 e3 : J.exp =
    case name
     of "word_update0" => J.Prim(",",[J.Prim("=",[J.Sub(e1,e2),e3]),junit])
      | "__mod_int32ub" => callPrim3 "SmlPrims.mod_i32" e1 e2 e3
      | "__mod_int31" => callPrim3 "SmlPrims.mod_i31" e1 e2 e3
      | "__mod_word32ub" => callPrim3 "SmlPrims.mod_w32" e1 e2 e3
      | "__mod_word31" => callPrim3 "SmlPrims.mod_w31" e1 e2 e3
      | "__div_int32ub" => callPrim3 "SmlPrims.div_i32" e1 e2 e3
      | "__div_int31" => callPrim3 "SmlPrims.div_i31" e1 e2 e3
      | "__div_word32ub" => callPrim3 "SmlPrims.div_w32" e1 e2 e3
      | "__div_word31" => callPrim3 "SmlPrims.div_w31" e1 e2 e3
      | _ => die ("pToJs3.unimplemented: " ^ name)

fun pToJs1 name e : J.exp =
    case name
     of "__bytetable_size" => J.Prop(e,"length")
      | "implodeCharsML" => callPrim1 "SmlPrims.implode" e
      | "implodeStringML" => callPrim1 "SmlPrims.concat" e
      | "charsToCharArray" => callPrim1 "SmlPrims.charsToCharArray" e
      | "charArraysConcat" => callPrim1 "SmlPrims.charArraysConcat" e
      | "printStringML" => callPrim1 "document.write" e
      | "exnNameML" => J.Sub(e,jcnst0)
      | "id" => e
      | "word_table0" => J.App(J.Id "Array",[e])
      | "table_size" => J.Prop(e,"length")
      | "chararray_to_string" => callPrim1 "SmlPrims.charArrayToString" e
      | "sml_real_to_bytes" => callPrim1 "SmlPrims.real_to_bytes" e
      | "sml_bytes_to_real" => callPrim1 "SmlPrims.bytes_to_real" e

      | "__neg_int32ub" => chkOvfI32(J.Prim("-",[e]))
      | "__neg_int31" => chkOvfI31(J.Prim("-",[e]))
      | "__neg_real" => J.Prim("-",[e])
      | "__neg_f64" => J.Prim("-",[e])
      | "__abs_int32ub" => chkOvfI32(callPrim1 "Math.abs" e)
      | "__abs_int31" => chkOvfI31(callPrim1 "Math.abs" e)
      | "__abs_real" => callPrim1 "Math.abs" e
      | "__abs_f64" => callPrim1 "Math.abs" e

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
      | "sml_localtime" => callPrim1 "SmlPrims.localtime" e
      | "sml_gmtime" => callPrim1 "SmlPrims.gmtime" e
      | "sml_mktime" => callPrim1 "SmlPrims.mktime" e

      | "__real_to_f64" => e
      | "__f64_to_real" => e
      | "__int_to_f64" => e

      | "__sqrt_f64" => callPrim1 "Math.sqrt" e
      | _ => die ("pToJs1 unimplemented: " ^ name)

fun pToJs0 name =
    case name
     of "posInfFloat" => J.Prim("Infinity",[])
      | "negInfFloat" => J.Prim("-Infinity",[])
      | "sml_getrealtime" => callPrim0 "SmlPrims.getrealtime"
      | "sml_localoffset" => callPrim0 "SmlPrims.localoffset"
      | _ => die ("pToJs0 unimplemented: " ^ name)

fun pToJs name [] = pToJs0 name
  | pToJs name [e] = pToJs1 name e
  | pToJs name [e1,e2] = pToJs2 name e1 e2
  | pToJs name [e1,e2,e3] = pToJs3 name e1 e2 e3
  | pToJs name _ = die ("pToJs unimplemented: " ^ name)

(* Compilation of switches *)

fun toJsSw (toj: Exp->ret)
           (toj1: Exp->J.stmt option * J.exp)
           (pp:'a->J.cnst)
           (L.SWITCH(e:Exp,bs:('a*Exp)list,eo: Exp option)) : ret =
    let
      fun cases k =
          foldr(fn ((a,e),acc) => (pp a, wrapRet k (toj e)) :: acc) [] bs
      fun defopt k : J.stmt option =
          case eo of
            SOME e => SOME (wrapRet k (toj e))
          | NONE => NONE
    in
      resolveS (toj1 e) (fn e' => fn k => J.Sw(e',cases k,defopt k))
    end

fun booleanBranch bs eo =
    case eo of
      SOME e =>
      (case bs of
         [((c,_),e')] =>
         (if Con.eq(c, Con.con_FALSE) then SOME(e,e')
          else (if Con.eq(c, Con.con_TRUE) then SOME(e',e)
                else NONE))
       | _ => NONE)
    | NONE =>
      (case bs of
         [((c1,_),e1),((c2,_),e2)] =>
         (if Con.eq(c1, Con.con_TRUE) then SOME(e1,e2)
          else (if Con.eq(c1, Con.con_FALSE) then SOME(e2,e1)
                else NONE))
       | _ => NONE)

fun unboxedBranch C bs eo =
    case eo of
      SOME e' =>
      (case bs of
         [((c,_),e)] =>
         (case Env.M.lookup (Context.envOf C) c of
            SOME UNBOXED_NULL => SOME(e,e')
          | SOME UNBOXED_UNARY => SOME(e',e)
          | _ => NONE)
       | _ => NONE)
    | NONE =>
      (case bs of
         [((c,_),e1),(_,e2)] =>
         (case Env.M.lookup (Context.envOf C) c of
            SOME UNBOXED_NULL => SOME(e1,e2)
          | SOME UNBOXED_UNARY => SOME(e2,e1)
          | _ => NONE)
       | _ => NONE)

fun enumeration C (((c,_),_)::_) =
    (case Env.M.lookup (Context.envOf C) c of
       SOME(ENUM _) => true
     | _ => false)
  | enumeration _ _ = false

fun toJsSw_C C
             (toj: Exp->ret)
             (toj1: Exp-> J.stmt option * J.exp)
             (L.SWITCH(e:Exp,bs:((Con.con*Lvars.lvar option)*Exp)list,eo: Exp option)) : ret =
    let fun compIf condF e1 e2 =
            case (toj e1, toj e2) of
              (E e1', E e2') => resolveE (toj1 e) (fn e' => J.IfExp(condF e',e1',e2'))
            | (r1,r2) => resolveS (toj1 e) (fn e' => fn k => J.IfStmt(condF e',wrapRet k r1,SOME(wrapRet k r2)))
    in
      case booleanBranch bs eo of
        SOME(e1,e2) => compIf (fn c => c) e1 e2
      | NONE =>
        case unboxedBranch C bs eo of
          SOME(e1,e2) => compIf (fn c => J.Prim("==",[c,J.Cnst J.Null])) e1 e2
        | NONE =>
        let
          fun pp (c,lvopt) = ppCon C c
          fun gen unboxed =
              let fun swF e = if unboxed then e else J.Sub(e,jcnst0)
                  fun defopt k =
                      case eo of
                        SOME e => SOME (wrapRet k (toj e))
                      | NONE => NONE
                  fun cases k = foldr(fn ((a,e),acc) => (pp a, wrapRet k (toj e)) :: acc) [] bs
              in resolveS (toj1 e) (fn e' => fn k => J.Sw(swF e',cases k,defopt k))
              end
        in if enumeration C bs then gen true
           else gen false
        end
    end

fun toJsSw_E (toj: Exp->ret) (L.SWITCH(e:Exp,bs:((Excon.excon*Lvars.lvar option)*Exp)list,eo: Exp option)) : ret =
    let
      val tmpvar = fresh_tmpvar()
      val s0 =
          case toj e of
            E e' => J.Var(tmpvar, SOME e')
          | S f => J.Var(tmpvar, NONE) & f (IdCont tmpvar)
      fun default k =
          case eo of
            SOME e => wrapRet k (toj e)
          | NONE => die "toJsSw_E.no default"
      fun cases k =
          List.foldr (fn (((excon,_),e),acc) =>
                         J.IfStmt(J.Prim("==", [J.Sub(J.Id tmpvar,jcnst0), J.Id (exconName excon)]),
                                  wrapRet k (toj e),
                                  SOME acc)) (default k) bs
    in S (fn k => s0 & cases k)
    end

(* Some utility functions *)

local
  exception FOUND
in
  fun lvarInExp lv e =
      let fun f (L.VAR{lvar,...}) =
              if Lvars.eq(lv,lvar) then raise FOUND
              else ()
            | f e = LambdaBasics.app_lamb f e
      in (f e; false) handle FOUND => true
      end

  fun tailCalls lv e =
      let fun f (L.APP(L.VAR{lvar,...},_,SOME true)) = if Lvars.eq(lv,lvar) then raise FOUND else ()
            | f e = LambdaBasics.app_lamb f e
      in (f e; false) handle FOUND => true
      end

  fun unsafeTailCall lv e =
      let fun f underLam (L.APP(L.VAR{lvar,...},e,SOME true)) =
              if underLam andalso Lvars.eq(lv,lvar) then raise FOUND
              else LambdaBasics.app_lamb (f underLam) e
            | f underLam (L.FN {body=e,...}) = LambdaBasics.app_lamb (f true) e
            | f underLam e = LambdaBasics.app_lamb (f underLam) e
      in (f false e; false) handle FOUND => true
      end

  fun noFns e =
      let fun f (L.FN _) = raise FOUND
            | f e = LambdaBasics.app_lamb f e
      in (f e; true) handle FOUND => false
      end
end


(* Monomorphic non recursive functions are compiled into simple
 * function expressions. *)

fun monoNonRec [{lvar,bind=L.FN{pat,body,...},tyvars=_,Type=_,regvars}] =
    if lvarInExp lvar body then NONE
    else SOME(lvar,pat,body)
  | monoNonRec _ = NONE

fun reassignIds (ids:J.id list) (es:J.exp list) : J.stmt =
    let val ids_exps_newids : (J.id * J.exp * J.id) list =
            ListPair.mapEq (fn (id,e) => (id, e, fresh_tmpvar())) (ids, es)
    in J.Seq(map (fn (_,e,id') => J.Var(id',SOME e)) ids_exps_newids) &
       J.Seq(map (fn (id,_,id') => J.Var(id,SOME (J.Id id'))) ids_exps_newids)
    end

(* Main compilation function *)

fun toj C (P:{clos_p:bool}) (e:Exp) : ret =
    case e of
    L.VAR {lvar,...} => E(J.Id(prLvar C lvar))
  | L.INTEGER (v,_) => E(J.Cnst(J.Int v))
  | L.WORD (v,_) => E(J.Cnst(J.Word v))
  | L.STRING (v,_) => E(J.Cnst(J.Str v))
  | L.REAL (v,_) => E(J.Cnst(J.Real v))
  | L.F64 v => E(J.Cnst(J.Real v))
  | L.PRIM(L.CONprim {con,...},nil) => E(ppConNullary C con)
  | L.PRIM(L.CONprim {con,...},[e]) =>
    resolveE (toj1 C P e) (ppConUnary C con)
  | L.PRIM(L.DECONprim {con,...}, [e]) =>
    (case Env.M.lookup (Context.envOf C) con of
       SOME(STD _) => resolveE (toj1 C P e) (fn e' => J.Sub(e',jcnst1))
     | SOME UNBOXED_UNARY => toj C P e
     | SOME _ => die ("toj.PRIM(DECON): constructor " ^ Con.pr_con con
                      ^ " associated with NULLARY constructor info")
     | NONE => die ("toj.PRIM(DECON): constructor " ^ Con.pr_con con
                    ^ " not in context"))
  | L.PRIM(L.EXCONprim excon,nil) => (* nullary *)
    E(J.Id(exconExn excon))
  | L.PRIM(L.EXCONprim excon,[e]) => (* unary *)
    resolveE (toj1 C P e) (fn e' => J.Array [J.Id(exconName excon),e'])
  | L.PRIM(L.DEEXCONprim excon,[e]) => (* unary *)
    resolveE (toj1 C P e) (fn e' => J.Sub(e', jcnst1))
  | L.PRIM(L.RECORDprim _, []) => E junit
  | L.PRIM(L.RECORDprim _, es) => resolveE (tojs C P es) J.Array
  | L.PRIM(L.UB_RECORDprim, [e]) => toj C P e
  | L.PRIM(L.UB_RECORDprim, es) => die ("UB_RECORD unimplemented. size(args) = "
                                        ^ Int.toString (List.length es))
  | L.PRIM(L.SELECTprim i,[e]) =>
    resolveE (toj1 C P e) (fn e' => J.Sub(e',J.Cnst(J.Int(Int32.fromInt i))))
  | L.PRIM(L.DEREFprim _, [e]) =>
    resolveE (toj1 C P e) (fn e' => J.Sub(e', jcnst0))
  | L.PRIM(L.REFprim _, [e]) =>
    resolveE (toj1 C P e) (fn e' => J.Array [e'])
  | L.PRIM(L.ASSIGNprim _, [e1,e2]) =>
    resolveE (toj2 C P (e1,e2))
      (fn (e1',e2') => J.Prim(",",[J.Prim("=",[J.Sub(e1',jcnst0),e2']),junit]))
  | L.PRIM(L.DROPprim, [e]) => toj C P e
  | L.PRIM(L.DROPprim, _) => die "DROPprim unimplemented"
  | L.PRIM(L.EQUALprim _, [e1,e2]) =>
    resolveE (toj2 C P (e1,e2)) (fn (e1',e2') => J.Prim("==",[e1',e2']))
  | L.FN {pat,body} =>
    let val ids = map (prLvar C o #1) pat
    in if #clos_p P then
         let fun fromList nil = FinMapEq.empty
               | fromList ((k,v)::rest) = FinMapEq.add Lvars.eq (k,v,fromList rest)
             val (fvs,_) = LambdaBasics.freevars e (* memo: what about excons? *)
             val lvs_lvs'_idxs =  (* new lvs for free variables *)
                 rev (#1 (foldl (fn (x,(acc,i)) => ((x,Lvars.newLvar(),i)::acc,i+1))
                                (nil,0) fvs))
             val env_id = prLvar C (Lvars.new_named_lvar "env")
             val rep : rep = (fromList o map (fn (lv,lv',_) => (lv,lv'))) lvs_lvs'_idxs
             val body' = replace_lvs rep body
             val binds = map (fn (_,lv',i) => J.Var(prLvar C lv', SOME(J.Sub(J.Id env_id,J.Cnst(J.Int i))))) lvs_lvs'_idxs
             val g = J.Fun(ids, J.Seq (binds@[wrapRet(RetCont NONE) (toj C P body')]))
             val f = J.Fun([env_id], J.Return g)
             val es = map (J.Id o prLvar C) fvs
         in E(J.App(f,[J.Array es]))
         end
       else E(J.Fun(ids, wrapRet (RetCont NONE) (toj C P body)))
    end
  | L.LET {pat=[p],bind,scope} => toj_let C P ([#1 p],[bind],scope)
  | L.LET {pat=[],bind,scope} =>
    S(fn k =>
         let val s_bind =
                 case toj C P bind of
                   S f => f NxtCont
                 | E e => J.Exp e
         in s_bind &
            (case toj C P scope of
               S f' => f' k
             | E e => wrapExp k e)
         end)
  | L.LET {pat,bind,scope} =>
    let val lvs = map #1 pat
        val binds = case bind of L.PRIM(UB_RECORDprim,binds) => binds
                               | _ => die "LET.unimplemented"
    in toj_let C P (lvs, binds, scope)
    end
  | L.LETREGION {scope,...} => toj C P e
  | L.FIX{functions,scope} =>
    (case monoNonRec functions of
       SOME (lv,pat,body) =>
       let val ids = map (prLvar C o #1) pat
           val f = J.Fun(ids, wrapRet (RetCont NONE) (toj C P body))
       in S (fn k => J.Var(prLvar C lv,SOME f) & wrapRet k (toj C P scope))
       end
     | NONE =>
       case functions of
         [{lvar,bind=L.FN{pat,body},...}] =>
         if tailCalls lvar body then
           if noFns body then                           (* ##1## Lambdas inside need not be closed *)
             let val fid = prLvar C lvar
                 val ids = map (prLvar C o #1) pat
                 val fixvar = fresh_fixvar()
                 fun pr_fix_lv lv = fixvar ^ ".$" ^ pr_lv lv   (* needs to be consistent with patch above *)
                 val C' = Context.add C (lvar,fixvar)
                 val funbody = J.While(SOME (pr_label lvar), jtrue,   (* label must be the same as the one for the continue statement... *)
                                       wrapRet (RetCont(SOME(lvar,ids)))
                                               (toj C' P body))
             in S(fn k =>
                     ( J.Var(fixvar,SOME (J.Id "{}")) &
                       J.Var(pr_fix_lv lvar, SOME(J.Fun(ids,funbody))) &
                       J.Var(fid, SOME(J.Id (pr_fix_lv lvar))) &
                       wrapRet k (toj C P scope)))
             end
           else if not (unsafeTailCall lvar body) then  (* ##2## Lambdas inside MUST be closurized *)
             let val fid = prLvar C lvar

                 (*val () = print ("######### New tailrecursive function: " ^ fid ^ "\n")*)

                 val ids = map (prLvar C o #1) pat
                 val fixvar = fresh_fixvar()
                 fun pr_fix_lv lv = fixvar ^ ".$" ^ pr_lv lv   (* needs to be consistent with patch above *)
                 val C' = Context.add C (lvar,fixvar)
                 val funbody = J.While(SOME (pr_label lvar), jtrue,   (* label must be the same as the one for the continue statement... *)
                                       wrapRet (RetCont(SOME(lvar,ids)))
                                               (toj C' {clos_p=true} body))
             in S(fn k =>
                     ( (*print ("fix with tailcalls: " ^ prLvar C lvar ^ "\n");*)
                       J.Var(fixvar,SOME (J.Id "{}")) &
                       J.Var(pr_fix_lv lvar, SOME(J.Fun(ids,funbody))) &
                       J.Var(fid, SOME(J.Id (pr_fix_lv lvar))) &
                       wrapRet k (toj C P scope)))
             end
           else toj_fix C P functions scope
         else toj_fix C P functions scope
       | _ => toj_fix C P functions scope)
  | L.APP(e1 as L.VAR{lvar,...},L.PRIM(L.UB_RECORDprim, es),SOME true) => (* tail call *)
    (S (fn k =>
           let fun notail() =
                   case tojs C P (e1::es) of
                     (SOME s, e1' :: es') =>
                     s & wrapExp k (J.App(e1',es'))
                   | (NONE, e1' :: es') => wrapExp k (J.App(e1',es'))
                   | _ => die "toj.L.APP.tail: impossible"
           in case k of
                RetCont(SOME(lv,ids)) => (* lv should be equal to lvar and ids are the formally bound variables *)
                if not (Lvars.eq(lv,lvar)) then notail()
                else
                  ( (* print (" tail call: " ^ prLvar C lvar ^ "\n"); *)
                   wrapRet NxtCont
                          (resolveS (tojs C P es)
                                    (fn es' => fn _ =>
                                                  reassignIds ids es' & J.Continue (pr_label lvar))))
              | k => notail()
           end
    ))
  | L.APP(e1,L.PRIM(L.UB_RECORDprim, es),_) =>
    (case tojs C P (e1::es) of
       (SOME s, e1' :: es') =>
       S(fn k => s & wrapExp k (J.App(e1',es')))
     | (NONE, e1' :: es') => E(J.App(e1',es'))
     | _ => die "toj.L.APP: impossible")
  | L.APP(e1,e2,i) => toj C P (L.APP(e1,L.PRIM(L.UB_RECORDprim,[e2]),i))

  | L.SWITCH_I {switch,precision} => toJsSw (toj C P) (toj1 C P) J.Int switch
  | L.SWITCH_W {switch,precision} => toJsSw (toj C P) (toj1 C P) J.Word switch
  | L.SWITCH_S switch => toJsSw (toj C P) (toj1 C P) J.Str switch
  | L.SWITCH_C switch => toJsSw_C C (toj C P) (toj1 C P) switch
  | L.SWITCH_E switch => toJsSw_E (toj C P) switch

  (* In EXPORTprim below, we could eta-convert e and add code to check
   * that the type of the argument is compatiple with instance_arg. *)
  | L.PRIM(L.EXPORTprim {name,instance_arg,instance_res},[e]) =>
    resolveE (toj1 C P e) (fn e' => J.Prim(",",[J.Prim("=",[J.Id("SMLtoJs." ^ name),e']),junit]))
  | L.PRIM(L.EXPORTprim {name,instance_arg,instance_res}, _) =>
    die "toj.PRIM(EXPORTprim) should take exactly one argument"
  | L.PRIM(L.CCALLprim {name,...},exps) =>
    (case name of
       "execStmtJS" =>
       (case exps
         of L.STRING (s,_) :: L.STRING (argNames,_) :: args =>  (* static code *)
            resolveE (tojs C P args) (fn es' => J.App(J.Fun([argNames],J.Embed s), es'))   (* hack with argNames pretty printing *)
          | s :: argNames :: args => (* dynamic code *)
            resolveE (tojs C P (s::argNames::args))
                     (fn (s'::argNames'::es') =>
                         J.App(J.New("Function", [argNames',s']),es')
                       | _ => die "toj.execStmtJS : string-->string-->args (2)")
          | _ => die "toj.execStmtJS : string-->string-->args")
     | "callJS" =>
       (case exps
         of L.STRING (f,_) :: args =>  (* static code *)
            resolveE (tojs C P args) (fn es' => J.App(J.Id f,es'))
          | f :: args => (* dynamic code *)
            let val xs = ((String.concatWith ",") o #2)
                         (foldl (fn (_,(i,acc)) => (i+1,"a" ^ Int.toString i::acc)) (0,nil) args)
            in
              resolveE (tojs C P (f::args))
                       (fn (f'::args') =>
                           J.App(J.New("Function",[J.Id("\"" ^ xs ^ "\""),
                                                   J.Prim("+",[J.Prim("+",[J.Id "\"return \"",f']),
                                                               J.Id("\"(" ^ xs ^ ")\"")])]),
                                 args')
                         | _ => die "toj.callJS : string-->args (2)")
            end
          | _ => die "toj.callJS : string-->args")
     | _ => resolveE (tojs C P exps) (pToJs name)
    )
  | L.PRIM _ => die "toj.PRIM unimplemented"
  | L.FRAME {declared_lvars, declared_excons} => E junit
  | L.HANDLE (e1,e2) => (* memo: avoid capture of variable e! *)
    let val lv = Lvars.newLvar()
        val id = prLvar C lv
    in S (fn k => J.Try (wrapRet k (toj C P e1),
                         id,
                         wrapRet k (resolveE (toj1 C P e2) (fn e2' => J.App(e2',[J.Id id])))))
    end
  | L.EXCEPTION (excon,SOME _,scope) => (* unary *)
    let val s = Excon.pr_excon excon  (* for printing *)
    in S (fn k => J.Var(exconName excon, SOME(sToS s)) &
                  wrapRet k (toj C P scope))
    end
  | L.EXCEPTION (excon,NONE,scope) => (* nullary; precompute exn value and store it in exconExn(excon)... *)
    let val s = Excon.pr_excon excon  (* for printing *)
        val exn_id = exconExn excon
    in S (fn k => J.Var(exconName excon, SOME (sToS s)) &
                  J.Var(exconExn excon, SOME (J.Array[J.Id(exconName excon)])) &
                  wrapRet k (toj C P scope))
    end
  | L.RAISE (e,_) => resolveS (toj1 C P e) (fn e' => fn k => J.Throw e')

and toj_let C P (lvs, binds, scope) =
    let
      fun loop (nil,nil) = toj C P scope
        | loop (lv::lvs,b::bs) =
          let val id = prLvar C lv
          in case toj C P b of
               E e' => S(fn k => J.Var(id,SOME e') & wrapRet k (loop(lvs,bs)))
             | S f => S(fn k => J.Var(id,NONE) & f (IdCont id) & wrapRet k (loop(lvs,bs)))
          end
        | loop _ = die "toj_let.mismatch"
    in loop(lvs,binds)
    end

and toj1 C P e =
    case tojs C P [e] of
      (opt,[e']) => (opt,e')
    | _ => die "toj1.impossible"

and toj2 C P (e1, e2) =
    case tojs C P [e1,e2] of
      (sopt,[e1',e2']) => (sopt,(e1',e2'))
    | _ => die "toj2.impossible"

and tojs C P es =
    let fun loop [] = (NONE,[])
          | loop (e::es) =
            case tojs C P es of
              (NONE,es') =>
              (case toj C P e of
                 E e1' => (NONE,e1'::es')
               | S f1 =>
                 let val id = fresh_tmpvar()
                 in (SOME(J.Var(id,NONE) & f1 (IdCont id)),J.Id id :: es')
                 end)
            | (SOME s,es') =>
              case toj C P e of
                E e1' =>
                let val id = fresh_tmpvar()
                in (SOME(J.Var(id,SOME e1') & s), J.Id id :: es')
                end
              | S f1 =>
                let val id = fresh_tmpvar()
                in (SOME(J.Var(id,NONE) & f1 (IdCont id) & s), J.Id id :: es')
                end
    in loop es
    end

and toj_fix C P functions scope =
    let
      val fixvar = fresh_fixvar()
      fun pr_fix_lv lv = fixvar ^ ".$" ^ pr_lv lv   (* needs to be consistent with patch above *)
      val C' = foldl(fn(lvar,C) => Context.add C (lvar,fixvar)) C (map #lvar functions)
      fun js2 k = foldl(fn(lv,js) => J.Var(prLvar C lv, SOME(J.Id (pr_fix_lv lv))) & js) (wrapRet k (toj C P scope)) (map #lvar functions)
      fun fundefs k =
          foldl (fn ({lvar=f_lv,bind=L.FN{pat,body},...},acc) =>
                    let val ids = map (prLvar C o #1) pat
                    in J.Var(pr_fix_lv f_lv, SOME(J.Fun(ids,wrapRet (RetCont NONE) (toj C' P body)))) & acc
                    end
                  | _ => die "toj_fix.malformed FIX")
                (js2 k) functions
    in S(fn k => J.Var(fixvar,SOME (J.Id "{}")) & fundefs k)
    end

fun toJs (env0, L.PGM(dbss,e)) =
    let
      val e = LambdaBasics.annotate_tail_calls e
      val (lvars,excons) = LambdaBasics.exports e
      val _ = setFrameLvars lvars
      val _ = setFrameExcons excons
      val _ = resetBase()
      val env' = Env.fromDatbinds dbss
      val env = Env.plus(env0,env')
      val js = wrapRet (RetCont NONE) (toj (Context.mk env) {clos_p=false} e)
      val js = J.Exp(J.App(J.Fun([],js),[]))
      val js =
          case getLocalBase() of
            SOME b => J.IfStmt(J.Prim("==",[J.App(J.Id"typeof",[J.Id b]),
                                            J.Cnst (J.Str "undefined")]),
                               J.Exp(J.Prim("=",[J.Id b, J.Id "{}"])),
                               NONE) & js
          | NONE => js
    in (js, env')
    end

type Js = J.stmt

fun toString js = JsAst.pr_stmt js

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
