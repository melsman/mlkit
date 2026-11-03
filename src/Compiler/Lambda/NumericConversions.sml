(* Semantic conversions survive in exported optimizer bodies. Representation
 * operations are selected only for executable code, after composition. *)
structure NumericConversions =
struct
  open LambdaExp
  val eq_Type = LambdaBasics.eq_Type
  datatype scalar = Int of int | Word of int | Inf
  datatype interpretation = Signed | Unsigned
  type conversion = {src: scalar, dst: scalar, interpretation: interpretation}

  fun scalar t =
      if eq_Type(t,int31Type) then SOME(Int 31)
      else if eq_Type(t,int32Type) then SOME(Int 32)
      else if eq_Type(t,int63Type) then SOME(Int 63)
      else if eq_Type(t,int64Type) then SOME(Int 64)
      else if eq_Type(t,word8Type) then SOME(Word 8)
      else if eq_Type(t,word31Type) then SOME(Word 31)
      else if eq_Type(t,word32Type) then SOME(Word 32)
      else if eq_Type(t,word63Type) then SOME(Word 63)
      else if eq_Type(t,word64Type) then SOME(Word 64)
      else if eq_Type(t,intinfType) then SOME Inf else NONE

  fun typ (Int 31) = int31Type
    | typ (Int 32) = int32Type
    | typ (Int 63) = int63Type
    | typ (Int 64) = int64Type
    (* CompileDec erases Word8 to the default word representation. Its logical
     * precision must therefore survive in the semantic primitive name. *)
    | typ (Word 8) = wordDefaultType()
    | typ (Word 31) = word31Type
    | typ (Word 32) = word32Type
    | typ (Word 63) = word63Type
    | typ (Word 64) = word64Type
    | typ Inf = intinfType
    | typ _ = Crash.impossible "NumericConversions.typ"

  fun width (Int n) = n
    | width (Word n) = n
    | width Inf = 0
  fun signed (Int _) = true
    | signed _ = false
  fun spelling (Int n) = "int" ^ Int.toString n
    | spelling (Word n) = "word" ^ Int.toString n
    | spelling Inf = "intinf"

  fun suffix Signed = "_X"
    | suffix Unsigned = ""
  fun name ({src,dst,interpretation}:conversion) =
      "__convert_" ^ spelling src ^ "_to_" ^ spelling dst ^ suffix interpretation

  fun namedScalar s t =
      case s of
          "int31" => SOME(Int 31)
        | "int32" => SOME(Int 32)
        | "int63" => SOME(Int 63)
        | "int64" => SOME(Int 64)
        | "word8" => SOME(Word 8)
        | "word31" => SOME(Word 31)
        | "word32" => SOME(Word 32)
        | "word63" => SOME(Word 63)
        | "word64" => SOME(Word 64)
        | "intinf" => SOME Inf
        | _ => scalar t

  fun decode (CCALLprim{name,Type=ARROWtype([a],_,[b],_),...}) =
      let val tokens = String.tokens (fn c => c = #"_") name
          val numeric = fn s => String.isPrefix "int" s orelse String.isPrefix "word" s
          val names = case tokens of
                          [a,"to",b] => if numeric a andalso numeric b then SOME(a,b) else NONE
                        | [a,"to",b,"X"] => if numeric a andalso numeric b then SOME(a,b) else NONE
                        | ["convert",a,"to",b] => SOME(a,b)
                        | ["convert",a,"to",b,"X"] => SOME(a,b)
                        | _ => if name = "id" then SOME("", "") else NONE
      in case names of
             NONE => NONE
           | SOME(an,bn) =>
             (case (namedScalar an a,namedScalar bn b) of
                  (SOME src,SOME dst) =>
                  if name = "id" andalso (src = Inf orelse dst = Inf) then NONE
                  else SOME {src=src,dst=dst,
                        interpretation=if signed src orelse String.isSuffix "_X" name
                                          orelse (name = "id" andalso signed dst andalso width src = width dst)
                                       then Signed else Unsigned}
                | _ => NONE)
      end
    | decode _ = NONE

  (* Explicit allocation regions are observable storage directives. Do not
   * compose away or move a primitive carrying one. *)
  fun forComposition (p as CCALLprim{regvars=[],...}) = decode p
    | forComposition _ = NONE

  fun primitive (c as {src,dst,...}:conversion) =
      CCALLprim{name=name c,instances=[],regvars=[],tyvars=[],
                Type=ARROWtype([typ src],NONE,[typ dst],NONE)}

  (* Only remove an intermediate when it cannot check or lose information.
   * In particular, never rewrite an IntInf -> finite -> IntInf round trip. *)
  fun compose (inner:conversion,outer:conversion) =
      let val {src,dst=bridge,interpretation=si} = inner
          val {src=bridge',dst,interpretation=so} = outer
          val s = signed src orelse si = Signed
          val preserving =
              case bridge of
                  Inf => true
                | Int n => if s then n >= width src else n > width src
                | Word n => not s andalso n >= width src
          val modulo = case (bridge,dst) of
                           (Word n,Word m) => n >= width src andalso m <= n
                         | _ => false
          val interpretation =
              if s then Signed
              else if so = Signed andalso width bridge = width src then Signed
              else Unsigned
      in if bridge <> bridge' orelse src = Inf orelse
            (not preserving andalso not modulo) then NONE
         else SOME {src=src,dst=dst,interpretation=interpretation}
      end

  fun fallback ({src,dst,interpretation}:conversion) =
      case (src,dst) of
          (Inf,Int n) => SOME("toInt" ^ Int.toString n)
        | (Inf,Word n) => SOME("toWord" ^ Int.toString n)
        | (Int n,Inf) => SOME("fromInt" ^ Int.toString n)
        | (Word n,Inf) => SOME("fromWord" ^ Int.toString n ^ (if interpretation = Signed then "X" else ""))
        | _ => NONE

  (* SMLtoJs has separate Basis conversions and emulated 64-bit types. Keep
   * its existing conversion lowering until it has a native scalar model. *)
  val supported = Flags.is_on0 "values_64bit"
  val tagged = Flags.is_on0 "tag_values"
  fun physical (Int 32) = if tagged() then "int32b" else "int32ub"
    | physical (Word 32) = if tagged() then "word32b" else "word32ub"
    | physical (Int 64) = if tagged() then "int64b" else "int64ub"
    | physical (Word 64) = if tagged() then "word64b" else "word64ub"
    | physical t = spelling t
  fun defaultWord () = scalar (wordDefaultType())
  fun call n args dst es =
      PRIM(CCALLprim{name=n,instances=[],regvars=[],tyvars=[],
                    Type=ARROWtype(args,NONE,[dst],NONE)},es)
  fun cast a b e = call "id" [typ a] (typ b) [e]
  fun machine a b x e =
      if a = b then e
      else if not(tagged()) andalso
              ((a = Int 32 andalso b = Word 32) orelse
               (a = Word 32 andalso b = Int 32 andalso x)) then cast a b e
      else
        let val n = "__" ^ physical a ^ "_to_" ^ physical b ^ (if x then "_X" else "")
            (* These legacy names also select low-unboxed 32-bit operations
             * on native 64-bit backends; there are no separate ub aliases. *)
            val n = case n of
                        "__int32ub_to_int63" => "__int32b_to_int63"
                      | "__int63_to_int32ub" => "__int63_to_int32b"
                      | "__word32ub_to_word63" => "__word32b_to_word63"
                      | "__word63_to_word32ub" => "__word63_to_word32b"
                      | _ => n
        in call n [typ a] (typ b) [e]
        end
  fun pow n = IntInf.<<(1,Word.fromInt n)
  fun wc n = WORD(n,word64Type)
  fun shift opr e n =
      call ("__shift_" ^ opr ^ "_" ^ physical(Word 64))
           [word64Type,wordDefaultType()] word64Type
           [e,WORD(IntInf.fromInt n,wordDefaultType())]
  fun le a b = call ("__lesseq_" ^ physical(Word 64))
                   [word64Type,word64Type] boolType [a,b]
  fun choose p yes no =
      SWITCH_C(SWITCH(p,[((Con.con_TRUE,NONE),yes)],SOME no))
  fun bind t e f =
      let val lv = Lvars.newLvar()
      in LET{pat=[(lv,[],t)],bind=e,scope=f(VAR{lvar=lv,instances=[],regvars=[]})}
      end

  fun unsignedBits src e =
      case src of
          Word 64 => e
        | Word 8 =>
          let val w = case defaultWord() of SOME w => w | NONE => Word 31
          in machine w (Word 64) false (cast src w e)
          end
        | Word _ => machine src (Word 64) false e
        | Int n => unsignedBits (Word n)
                     (if n = 32 orelse n = 64 then machine src (Word n) false e
                      else cast src (Word n) e)
        | Inf => Crash.impossible "NumericConversions.unsignedBits"

  fun truncate dst e =
      case dst of
          Word 64 => e
        | Word 8 =>
          let val w = case defaultWord() of SOME w => w | NONE => Word 31
              val e = machine (Word 64) w false e
              val e = call ("__andb_" ^ physical w) [typ w,typ w] (typ w)
                           [e,WORD(255,typ w)]
          in cast w dst e
          end
        | Word _ => machine (Word 64) dst false e
        | Int n =>
          let val e = truncate (Word n) e
          in if n = 32 orelse n = 64 then machine (Word n) dst true e
             else cast (Word n) dst e
          end
        | Inf => Crash.impossible "NumericConversions.truncate"

  (* Direct operations cover the existing backend instruction families. The
   * remaining mixed cases use a full-width bit carrier, never IntInf lists. *)
  fun fixed (c as {src,dst,interpretation}:conversion) e =
      let val s = signed src orelse interpretation = Signed
          val n = width src
          val m = width dst
          val identity = src = dst orelse
              (src = Word 8 andalso not s andalso
               ((tagged() andalso (m = 31 orelse m = 63)) orelse
                (not(tagged()) andalso (m = 32 orelse m = 64))))
          val direct = case (src,dst) of
              (Int _,Int _) => true
            | (Word a,Word b) => a <> 8 andalso b <> 8
            | (Word a,Int b) => a = b andalso (a = 32 orelse a = 64)
            | (Int a,Word b) => a = b
            | _ => false
          (* Not every integer pair has a named backend operation. *)
          val direct = direct andalso not(src = Int 64 andalso dst = Int 32)
                       andalso not(src = Word 32 andalso dst = Word 63 andalso s)
          fun generic () =
              let val bits = unsignedBits src e
                  val bits = if s andalso n < 64 then
                                 shift "right_signed" (shift "left" bits (64-n)) (64-n)
                             else bits
              in bind word64Type bits (fn v =>
                   let val result = truncate dst v
                       val ov = RAISE(PRIM(EXCONprim Excon.ex_OVERFLOW,[]),Types[typ dst])
                   in case dst of
                          Int _ =>
                          if (s andalso m >= n) orelse (not s andalso m > n) then result
                          else if s then choose (le v (wc(pow(m-1)-1))) result
                                      (choose (le (wc(pow 64-pow(m-1))) v) result ov)
                          else choose (le v (wc(pow(m-1)-1))) result ov
                        | _ => result
                   end)
              end
      in if src = dst then e
         else if identity then cast src dst e
         else if src = Word 8 andalso not s then
             let val w = case defaultWord() of SOME w => w | NONE => Word 31
                 val bits = machine w (Word m) false (cast src w e)
             in case dst of
                    Word _ => bits
                  | Int _ => if m = 31 orelse m = 63 then cast (Word m) dst bits
                             else machine (Word m) dst true bits
                  | Inf => Crash.impossible "NumericConversions.fixed"
             end
         else if direct then
             if (case (src,dst) of (Int _,Word _) => true | _ => false) then
                 if n = 32 orelse n = 64 then machine src dst false e else cast src dst e
             else machine src dst (not(signed src) andalso s andalso
                                   (signed dst orelse m > n)) e
         else generic()
      end

  (* IntInf uses a sign and little-endian base-2^30 digits, also used by
   * CompileDec.buildIntInf. A finite scalar needs at most three digits. Expose
   * their construction to local optimization instead of hiding it in a call. *)
  fun toInf (c as {src,interpretation,...}:conversion) arg =
      let val bits = fixed {src=src,dst=Word 64,interpretation=interpretation} arg
          val nilExp = PRIM(CONprim{con=Con.con_NIL,instances=[int31Type],regvar=NONE},[])
          fun cons d ds = PRIM(CONprim{con=Con.con_CONS,instances=[int31Type],regvar=NONE},
                              [PRIM(RECORDprim{regvar=NONE},[d,ds])])
          fun digits 0 v = nilExp
            | digits n v =
              choose (le v (wc 0)) nilExp
                (let val low = call ("__andb_" ^ physical(Word 64))
                                    [word64Type,word64Type] word64Type [v,wc(pow 30-1)]
                     val d = cast (Word 31) (Int 31) (truncate (Word 31) low)
                 in bind word64Type (shift "right_unsigned" v 30)
                         (fn rest => cons d (digits (n-1) rest))
                 end)
          val falseExp = PRIM(CONprim{con=Con.con_FALSE,instances=[],regvar=NONE},[])
          val trueExp = PRIM(CONprim{con=Con.con_TRUE,instances=[],regvar=NONE},[])
      in bind word64Type bits (fn v =>
           let val negative = if signed src orelse interpretation = Signed then
                                  choose (le v (wc(pow 63-1))) falseExp trueExp
                              else falseExp
           in bind boolType negative (fn negative =>
                let val magnitude = choose negative
                        (call ("__minus_" ^ physical(Word 64))
                              [word64Type,word64Type] word64Type [wc 0,v]) v
                in bind word64Type magnitude (fn magnitude =>
                     PRIM(CONprim{con=Con.con_INTINF,instances=[],regvar=NONE},
                          [PRIM(RECORDprim{regvar=NONE},[digits 3 magnitude,negative])]))
                end)
           end)
      end

  fun needsLowering e =
      LambdaBasics.foldTD (fn found => fn e => found orelse
          (case e of
               PRIM(p as CCALLprim{name,...},[_]) =>
               (case decode p of
                    SOME {src,dst,...} => String.isPrefix "__convert_" name orelse src = Inf orelse dst = Inf
                  | NONE => false)
             | _ => false)) false e

  fun lower lookup e =
      let fun walk e =
              case e of
                  PRIM(p, [arg]) =>
                  (case decode p of
                       SOME c =>
                       if String.isPrefix "__convert_" (case p of CCALLprim{name,...} => name | _ => "")
                          orelse #src c = Inf orelse #dst c = Inf then
                         let val arg = walk arg
                         in if #dst c = Inf then toInf c arg
                            else case fallback c of
                                SOME n =>
                                (case lookup n of
                                     SOME f => APP(f,arg,NONE)
                                   | NONE => Crash.impossible("NumericConversions.lower: " ^ n))
                              | NONE => fixed c arg
                         end
                       else LambdaBasics.map_lamb walk e
                     | NONE => LambdaBasics.map_lamb walk e)
                | _ => LambdaBasics.map_lamb walk e
      in walk e
      end
end
