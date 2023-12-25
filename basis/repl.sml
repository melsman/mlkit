local

structure Scan :
sig
  type ('a,'st) reader = ('a,'st) StringCvt.reader
  type ('a,'st) p = (char,'st) reader -> ('a,'st) reader
  val >>> : ('a,'st) p * ('b,'st) p -> ('a * 'b,'st) p
  val >>@ : ('a,'st) p * ('a -> 'b) -> ('b,'st) p
  val ||  : ('a,'st) p * ('a,'st) p -> ('a,'st) p
  val ->> : ('a,'st) p * ('b,'st) p -> ('b,'st) p
  val >>- : ('a,'st) p * ('b,'st) p -> ('a,'st) p
  val >>? : ('a,'st) p * ('b,'st) p -> ('a * 'b -> 'a) -> ('a,'st) p
  val >>* : ('a,'st) p * ('b,'st) p -> ('a * 'b -> 'a) -> ('a,'st) p
  val ign : ('a,'st) p -> (unit,'st) p
  val eos : (unit,'st) p
  val scanChar  : (char -> bool) -> (char,'st) p
  val scanChars : (char -> bool) -> (string,'st) p
  val skipChars : (char -> bool) -> ('a,'st) p -> ('a,'st) p
  val skipWS    : ('a,'st) p -> ('a,'st) p
  val scanId    : (string,'st) p
end =
struct

type ('a,'st) reader = 'st -> ('a * 'st) option
type ('a,'st) p = (char,'st) reader -> ('a,'st) reader

infix >>> ->> >>- >>? || >>@ >>*

fun (p1 >>> p2) get s =
    case p1 get s of
        SOME (a,s) => (case p2 get s of
                           SOME (b,s) => SOME((a,b),s)
                         | NONE => NONE)
      | NONE => NONE

fun (p >>@ f) get s =
    case p get s of
        SOME (a,s) => SOME (f a,s)
      | NONE => NONE

fun (p1 >>- p2) = (p1 >>> p2) >>@ (#1)
fun (p1 ->> p2) = (p1 >>> p2) >>@ (#2)

fun (p1 || p2) get s =
    case p1 get s of
        NONE => p2 get s
      | r => r

fun (p1 >>? p2) f get s =
    case p1 get s of
        NONE => NONE
      | SOME (a,s) => case p2 get s of
                          SOME (b,s) => SOME(f(a,b),s)
                        | NONE => SOME(a,s)

fun (p1 >>* p2) f get s =
    case p1 get s of
        SOME (a,s) =>
        let fun repeat (a,s) =
                case p2 get s of
                    SOME (b,s) => repeat (f(a,b), s)
                  | _ => SOME (a,s)
        in repeat (a,s)
        end
      | NONE => NONE

fun ign scan get s =
    case scan get s of
        SOME (_,s) => SOME((),s)
      | NONE => NONE

fun eos get s =
    case get s of
        NONE => SOME((),s)
      | SOME _ => NONE

fun skipChars P p get s0 =
    case get s0 of
        SOME(c,s) => if P c then skipChars P p get s
                     else p get s0
      | NONE => p get s0

fun scanChar P get s =
    case get s of
        SOME(c,s) => if P c then SOME(c,s)
                     else NONE
      | NONE => NONE

fun scanChars P =
    (scanChar P >>@ (fn c => [c]) >>* scanChar P) (fn (a,b) => b::a) >>@ (implode o rev)

fun skipWS p = skipChars Char.isSpace p

fun scanAnyChar get s = get s

fun scanId get =
    (scanChar Char.isAlpha >>@ String.str >>?
     scanChars (fn c => Char.isAlphaNum c orelse c = #"_")
    ) (op ^) get

end

(* Types and substitutions *)

type tv = string     (* type variable *)
type tn = string     (* type name *)
type cn = string     (* constructor name *)
datatype tau = C of tau list * tn
             | T of tau list
             | R of (string*tau) list
             | A of tau * tau
             | V of tv

datatype boxity = UNB | BOX | UNB_HIGH

type sch = tv list * tau         (* type scheme *)
type cb = cn * sch               (* construtor binding *)
type db = tn * boxity * cb list  (* data binding *)

type subst = (tv * tau) list
fun lookSubst (S:subst) tv =
    case List.find (fn (tv',_) => tv=tv') S of
        SOME (_,t) => SOME t
      | NONE => NONE

fun appSubst (S:subst) t =
    case t of
        C (ts,tn) => C (map (appSubst S) ts,tn)
      | T ts => T (map (appSubst S) ts)
      | R lts => R (map (fn (l,t) => (l,appSubst S t)) lts)
      | A (t1,t2) => A (appSubst S t1, appSubst S t2)
      | V tv => case lookSubst S tv of
                    SOME t' => t'
                  | NONE => t

fun pp (t:tau) : string =
    case t of
        V s => s
      | T ts => "(" ^ String.concatWith "*" (map pp ts) ^ ")"
      | R labtys => "{" ^ String.concatWith "," (map (fn (l,t) => l ^ ":" ^ pp t) labtys) ^ "}"
      | C([],tn) => tn
      | C([t],tn) => pp t ^ " " ^ tn
      | C(ts,tn) => "(" ^ String.concatWith "," (map pp ts) ^ ") " ^ tn
      | A (t1,t2) => pp t1 ^ "->" ^ pp t2

fun mapi (f:int*'a->'b) (xs:'a list) : 'b list =
    List.rev (#2 (List.foldl (fn (x,(i,acc)) => (i+1,f(i,x)::acc)) (0,nil) xs))

type ck = int

fun pp_ck (ck:ck) = "tag" ^ Int.toString ck

fun isArrow (_,A _) = true
  | isArrow _ = false

type cb' = cn * sch * ck
type db' = tn * boxity * cb' list

fun analyse_db (tn:tn, boxity:boxity, cs: cb list) : tn * boxity * cb' list =
    case boxity of
        UNB_HIGH =>
        (tn, boxity, mapi (fn (i,(cn,sch)) => (cn,sch,i)) cs)
      | _ => let val unaries = List.filter (isArrow o #2) cs
                 val nullaries = List.filter (not o isArrow o #2) cs
             in (tn, boxity,
                 (mapi (fn (i,(cn,sch)) => (cn,sch,i)) nullaries @
                  mapi (fn (i,(cn,sch)) => (cn,sch,i)) unaries)
                )
             end

fun lookTn (dbs:db' list) (tn:tn) : (boxity * cb' list) option =
    case List.find (fn (tn',_,_) => tn=tn') dbs of
        SOME (_,b,cs) => SOME (b,cs)
      | NONE => NONE

fun lookUnaryTag (cbs:cb' list) (tag:int) ts =
    case List.find (fn (_,sch,i) => i = tag andalso isArrow sch) cbs of
        SOME (cn,(tvs,t),_) =>
        (let val S = ListPair.zipEq (tvs,ts)
         in case t of
                A(t,_) => SOME (cn,appSubst S t)
              | _ => NONE
         end handle _ => NONE)
      | NONE => NONE

fun lookNullaryTag (cbs:cb' list) (tag:int) =
    case List.find (fn (_,sch,i) => i = tag andalso not(isArrow sch)) cbs of
        SOME (c,_,_) => c
      | NONE => "?"

fun enum (cs:cb' list) =
    List.all (fn (_,sch,_) => not(isArrow sch)) cs

(* Parsing of types and type definitions *)

open Scan

infix >>> ->> >>- >>? || >>@ >>*

type st = CharVectorSlice.slice

val p_name : (string, st) p =
    skipWS scanId

fun p_symb (c:char) : (unit, st) p =
    skipWS (scanChar (fn c' => c=c')) >>@ (fn _ => ())

fun p_symb2 (c1:char) (c2:char) : (unit, st) p =
    skipWS ((scanChar (fn c => c1=c) ->>
                      scanChar (fn c => c2=c)) >>@ (fn _ => ()))

val p_tv : (tv, st) p =
    (skipWS (p_symb #"'" ->> scanId)) >>@ (fn s => "'" ^ s)

(*

Grammar:

    ty ::= ty * .. * ty
         | ty tn
         | ty -> ty
         | (ty,..,ty) tn
         | ( ty )
         | { lab : ty , ... , lab : ty }
         | tn
         | tv

Grammar without left-recursion:

    ty0     ::= aty [ tns ]
    ty1     ::= ty0 [ startys ]
    ty      ::= ty1 [ -> ty ]
    tns     ::= tn [ tns ]
    startys ::= * ty0 [ startys ]
    aty     ::= tv | tn | ( ty ) | tyseq2 tn
              | { lab : ty , ... , lab : ty }  // n >= 0
    tyseq2  ::= ( ty , ... , ty )             // n > 1

*)

fun single t = [t]

fun p_list (sep:char) (p : ('a,st) p) : ('a list,st) p =    (* one or more *)
    fn g => (
      ((p >>@ single) >>? (p_symb sep ->> p_list sep p)) (op @)
    ) g

fun parse (s: string) : db list * tau =
    let val rec p_ty : (tau,st) p =
         fn g => (
           ((p_ty1 >>? (p_symb2 #"-" #">" ->> p_ty)) A)
         ) g
        and p_ty0 : (tau,st) p =
            fn g => (
              (p_aty >>? p_tns) (fn (ty,tns) => foldl (fn (tn,t) => C([t],tn)) ty tns)
            ) g
        and p_ty1 : (tau,st) p =
            fn g => (
              ((p_ty0 >>? p_startys) (T o (op ::)))
            ) g
        and p_tns : (string list,st) p =
            fn g => (
              ((p_name >>@ single) >>? p_tns) (op @)
            ) g
        and p_startys : (tau list,st) p =
            fn g => (
              (((p_symb #"*" ->> p_ty0) >>@ single) >>? p_startys) (op @)
            ) g
        and p_aty : (tau,st) p =
            fn g => (
                 (p_tv >>@ V)
              || (p_name >>@ (fn tn => C([],tn)))
              || (p_symb #"(" ->> p_ty >>- p_symb #")")
              || ((p_symb #"{" ->> p_list #"," (p_name >>- p_symb #":" >>> p_ty) >>- p_symb #"}") >>@ R)
              || ((p_symb #"{" ->> p_symb #"}") >>@ (fn _ => R nil))
              || (((p_symb #"(" ->> p_list #"," p_ty >>- p_symb #")") >>> p_name) >>@ C)
            ) g
        and p_sch : (sch,st) p =
            fn g => (
                 (p_symb #"(" ->> (p_list #"," p_tv) >>- p_symb2 #")" #"." >>> p_ty)
              || (p_ty >>@ (fn t => ([],t)))
            ) g
        and p_cb : (cb,st) p =
            fn g => (
              p_name >>- p_symb #":" >>> p_sch
            ) g
        and p_db : (db,st) p =
            fn g => (
              (p_name >>- p_symb #"(" >>> p_boxity >>- p_symb #")" >>- p_symb #"="
                      >>- p_symb #"[" >>> (p_list #"," p_cb) >>- p_symb #"]")
                  >>@ (fn ((tn,ub),cbs) => (tn,ub,cbs))
            ) g
        and p_boxity : (boxity,st) p =
            fn g => (
                 (p_symb2 #"u" #"a" >>@ (fn _ => UNB_HIGH))
              || (p_symb #"u" >>@ (fn _ => UNB))
              || (p_symb #"b" >>@ (fn _ => BOX))
            ) g
        and p_line : (db list*tau,st) p =
            fn g => (
                 ((p_list #";" p_db) >>- p_symb #";" >>> p_ty)
              || (p_ty >>@ (fn t => ([],t)))
             ) g
        val sl = CharVectorSlice.full s
    in case (p_line >>- skipWS eos) CharVectorSlice.getItem sl of
           SOME((dbs,t),_) => (dbs,t)
         | NONE => raise Fail "parse failure"
    end

(* Operations for manipulation values, given knowledge about their
   types and unboxity
 *)

fun getCtx () : foreignptr = prim("__get_ctx",())

fun selectTuple (v:foreignptr, i:int) : foreignptr =
    prim("selectTuple", (v,i))

fun ubcon_tag (v:foreignptr) : int =
    prim("val_ubcon_tag", v)

fun con_tag (v:foreignptr) : int =
    prim("val_con_tag", v)

fun con1 (v:foreignptr) : bool =
    prim("val_is_con1", v)

fun ubcon1 (v:foreignptr) : bool =
    prim("val_is_ubcon1", v)

fun ubcon1_prj (v:foreignptr) : foreignptr =
    prim("val_ubcon1_prj", v)

fun con1_prj (v:foreignptr) : foreignptr =
    prim("val_con1_prj", v)

fun pretty_depth () : int =
    prim("get_pretty_depth",())

fun pretty_string_size () : int =
    prim("get_pretty_string_size",())

fun ubhcon_tag (v:foreignptr) : int =
    prim("ptr_hitag_get_fun", v)

fun ubhcon1_prj (v:foreignptr) : foreignptr =
    prim("ptr_hitag_clear_fun", v)

(* The pretty printer *)

val z_base = 0        (* precedense values *)
val z_par = 2
val z_weak = 5

fun nopar (s,_) = s
fun par_conarg (s,z) =
    if z = z_base then " " ^ s
    else if z = z_par then s
    else (* z = z_weak *) "(" ^ s ^ ")"

fun pretty_exported (i:int) : int =
    let val ty : string = prim("pretty_ML_GetTy", ())
        (* val () = print ("pretty_exported: " ^ ty ^ "\n") *)
        val v : foreignptr = prim("pretty_ML_GetVal", ())
        val depth = pretty_depth()
        val max_string_size = pretty_string_size()
        val str =
            let val (dbs,t) = parse ty
                val dbs = map analyse_db dbs
                fun pr (d,t,v) =
                    if d < 0 then ("_", z_base) else
                    (case t of
                        T [t] => ("single",z_base)
                      | T ts => ("(" ^ String.concatWith ","
                                                         (mapi (fn (i,t) =>
                                                                   let val v = selectTuple(v,i)
                                                                   in nopar(pr(d,t,v))
                                                                   end) ts
                                                         ) ^ ")",
                                 z_par)
                      | R lts => ("{" ^ String.concatWith ","
                                                          (mapi (fn (i,(l,t)) =>
                                                                    let val v = selectTuple(v,i)
                                                                    in l ^ "=" ^ nopar(pr(d,t,v))
                                                                    end) lts
                                                          ) ^ "}",
                                  z_par)
                      | C ([], "int31") => (Int31.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "int32") => (Int32.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "int63") => (Int63.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "int64") => (Int64.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "int") => (Int.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "intinf") => (IntInf.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "word8") => ("0wx" ^ Word8.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "word31") => ("0wx" ^ Word31.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "word32") => ("0wx" ^ Word32.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "word63") => ("0wx" ^ Word63.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "word64") => ("0wx" ^ Word64.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "word") => ("0wx" ^ Word.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "real") => (Real.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "char") => ("#\"" ^ Char.toString (prim("unsafe_cast", v)) ^ "\"",
                                           z_base)
                      | C ([], "bool") => (Bool.toString (prim("unsafe_cast", v)), z_base)
                      | C ([], "string") =>
                        let val s : string = prim("unsafe_cast", v)
                            val s = if size s <= max_string_size then s
                                    else (String.extract(s, 0, SOME max_string_size) ^ "..")
                        in ("\"" ^  String.toString s ^ "\"",
                            z_base)
                        end
                      | C ([t], "option") =>
                        let val v : foreignptr option = prim("unsafe_cast", v)
                        in case v of
                               SOME v => ("SOME" ^ par_conarg (pr(d-1,t,v)), z_weak)
                             | NONE => ("NONE", z_base)
                        end
                      | C ([t], "list") =>
                        let val v : foreignptr list = prim("unsafe_cast", v)
                            fun loop n acc (v: foreignptr list) =
                                if n < 0 then rev (".."::acc)
                                else case v of
                                         nil => rev acc
                                       | v::vs => loop (n-1) (nopar(pr(d-1,t,v))::acc) vs
                            val ss = loop 10 nil v
                        in ("[" ^ String.concatWith "," ss ^ "]", z_par)
                        end
                      | C([t], "ref") =>
                        let val v : foreignptr ref = prim("unsafe_cast", v)
                        in case v of
                               ref v => ("ref" ^ par_conarg (pr(d-1,t,v)), z_weak)
                        end
                      | C (ts,tn) =>
                        (let val (boxity,cs) =
                                 case lookTn dbs tn of
                                     SOME res => res
                                   | NONE => raise Fail ("type name " ^ tn ^ " not found")
                         in case boxity of
                                UNB =>
                                if enum cs then
                                  let val tag : int = prim("unsafe_cast", v)
                                  in (lookNullaryTag cs tag, z_base)
                                  end
                                else if length cs = 1 then (* unary & single => unboxed & untagged *)
                                  (case lookUnaryTag cs 0 ts of
                                       SOME (cn,t) =>
                                       (cn ^ par_conarg (pr(d-1,t,v)), z_weak)
                                     | NONE => ("?", z_base))
                                else if ubcon1 v then (* unary *)
                                  let val tag = ubcon_tag v
                                  in case lookUnaryTag cs tag ts of
                                         NONE => ("?",z_base)
                                       | SOME (cn, t) =>
                                         (cn ^ par_conarg (pr(d-1,t,ubcon1_prj v)), z_weak)
                                  end
                                else (* nullary *)
                                  let val tag = ubcon_tag v
                                  in (lookNullaryTag cs tag, z_base)
                                  end
                              | UNB_HIGH =>
                                let val tag = ubhcon_tag v
                                in case lookUnaryTag cs tag ts of
                                       SOME (cn, t) =>
                                       (cn ^ par_conarg (pr(d-1,t,ubhcon1_prj v)), z_weak)
                                     | NONE =>
                                       (lookNullaryTag cs tag, z_base)
                                end
                              | BOX =>
                                if con1 v then (* unary *)
                                  let val tag = con_tag v
                                  in case lookUnaryTag cs tag ts of
                                         NONE => ("?",z_base)
                                       | SOME (cn, t) =>
                                         (cn ^ par_conarg (pr(d-1,t,con1_prj v)), z_weak)
                                  end
                                else (* nullary *)
                                  let val tag = con_tag v
                                  in (lookNullaryTag cs tag, z_base)
                                  end
                         end handle Fail s =>
                                    if List.null ts then ("<" ^ tn ^ ">", z_par)
                                    else ("<" ^ tn ^ "," ^ Int.toString (length ts) ^ ">", z_par)
                        )
                      | A _ => ("fn", z_base)
                      | V s => ("tv: " ^ s, z_weak))
            in nopar (pr(depth,t,v))
            end handle _ => "_"
    in prim("pretty_ML_Print", (getCtx(),str,Match)) : unit
     ; size str
    end handle e =>
               (print ("pretty_exported uncaught exception: "
                       ^ General.exnMessage e ^ "\n")
               ; ~1)
in
val () = _export("pretty_exported", pretty_exported)
end
