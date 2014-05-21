
(*
functor TestSpreadExp() =
struct

(*$TestSpreadExp: Crash PrettyPrint Flags DiGraph Effect Lvars 
   LambdaExp TyName 
              Ident Con Excon Report TyCon Timestamp 
              BasicIO  Stack UnionFindPoly StrId RType SpreadDatatype FinMap 
              RegionStatEnv RegionExp SpreadExpression*)


structure BasicIO = BasicIO();
structure Crash = Crash(structure BasicIO = BasicIO);
structure Flags = Flags(structure Crash = Crash);
structure Report = Report(structure BasicIO = BasicIO);
structure PP = PrettyPrint(structure Report = Report
                           structure Crash = Crash
                           structure Flags = Flags);
structure UF = UF_with_path_halving_and_union_by_rank();
structure Stack = Stack();
structure DiGraph = DiGraph(structure UF = UF
                            structure Stack = Stack
                            structure PP = PP
                            structure Flags = Flags
                            structure Crash = Crash)

structure Effect(*:EFFECT*) = Effect(structure G = DiGraph
                          structure PP = PP
                          structure Crash = Crash
                          structure Report = Report);

structure Timestamp = Timestamp()
structure StrId = StrId(structure Timestamp= Timestamp
                        structure Crash= Crash
                          )
structure Ident = Ident(structure StrId = StrId
                        structure Crash = Crash)
structure Con = Con(structure Ident = Ident)
structure Excon = Excon(structure Ident = Ident)

structure TyCon = TyCon(
              structure StrId= StrId
              structure Crash= Crash
             )

structure TyName =TyName(structure TyCon = TyCon
               structure Timestamp = Timestamp
               structure Flags= Flags
              );

structure Lvars = Lvars()

structure LambdaExp = LambdaExp(structure Lvars=Lvars
                       structure Con= Con
                       structure Excon= Excon
                       structure TyName = TyName
                       structure PP= PP
                       structure Crash= Crash
                       structure Flags = Flags
                     )

structure TyNameEnv =
   struct
     type tyname = TyName.TyName
     type tyname_env = (tyname * (int*int*int)) list
     val tyname_env0 = [(TyName.tyName_BOOL, (0,0,0)),
                        (TyName.tyName_INT, (0,0,0)),
                        (TyName.tyName_REAL, (0,0,0)),
                        (TyName.tyName_STRING, (0,0,0)),
                        (TyName.tyName_LIST, (1,1,0)) (* .... more built-ins *)
                       ]

     fun lookup [] tyname = None
       | lookup ((tyname',arity)::rest) tyname = if tyname=tyname' then Some arity else lookup rest tyname

     val lookup = lookup tyname_env0
   end;

structure RType:RTYPE = RType(
              structure Flags = Flags
              structure Crash = Crash
              structure E= Effect
              structure DiGraph = DiGraph
              structure L = LambdaExp
              structure TyName= TyName
              structure TyNameEnv = TyNameEnv
              structure PP = PP
             );

structure FinMap = FinMap(structure Report = Report
                          structure PP = PP
                             );

structure Arg = struct
     structure R:RTYPE = RType 
     structure E:EFFECT = Effect
     structure TyName = TyName
     structure Con = Con
     structure ExCon = Excon
     structure Lvar = Lvars
     structure Crash = Crash
     structure FinMap = FinMap
     structure L: LAMBDA_EXP = LambdaExp
     structure PP = PP
end;

structure RegionStatEnv = RegionStatEnv(open Arg);

structure RegionExp = 
        RegionExp(structure R = RType
                  structure Eff = Effect
(*                  structure RSE = RegionStatEnv*)
                  structure Lam = LambdaExp
                  structure Lvar = Lvars
                  structure Con = Con
                  structure Excon = Excon
                  structure TyName = TyName
                  structure Flags = Flags
                  structure Crash = Crash
                  structure PP = PP);
        

structure SpreadDatatype = SpreadDatatype(
           structure R = RType
           structure Con = Con
           structure ExCon = Excon
           structure Effect = Effect
           structure FinMap = FinMap 
           structure E = LambdaExp
           structure E' = RegionExp 
           structure RSE = RegionStatEnv
           structure TyName = TyName
           structure Crash = Crash
           structure PP = PP
          )


structure SpreadExpression =  SpreadExpression(
  structure Con = Con
  structure ExCon = Excon
  structure E = LambdaExp
  structure E'= RegionExp
  structure FinMap = FinMap
  structure Eff = Effect
  structure R= RType
  structure RSE = RegionStatEnv
  structure SpreadDatatype = SpreadDatatype
  structure Lvars =  Lvars
  structure TyName = TyName
  structure Crash = Crash
  structure PP = PP
)

val dump = ref [] : string list ref
fun pp(t) = PP.flatten1 t
fun say s = output(std_out, s^"\n")
fun say' s= output(std_out, s)
fun etest(label,expected,found) =
say(label ^ (if expected = found then " OK" else (dump:= (label^ " ") :: (!dump);
                                                   " ****** NOT OK *******" ^
     "\n expected: " ^ expected ^ 
     "\n found:    " ^ found)))

fun etest'(label,expected,found) = say (label ^ found);

val _ = Effect.count:= !Effect.count -3;  (* to compensate for changed initial region stat end *)
val new_tyname = TyName.tyName_INSTREAM (* could be any one *)
val alpha = LambdaExp.fresh_tyvar()
val alpha_ty = LambdaExp.TYVARtype alpha;
val alpha_list = LambdaExp.CONStype([alpha_ty], TyName.tyName_LIST);
val string_ty = LambdaExp.CONStype([], TyName.tyName_STRING);
val real_ty = LambdaExp.CONStype([], TyName.tyName_REAL);
val string_list = LambdaExp.CONStype([string_ty], TyName.tyName_LIST);
val int_ty = LambdaExp.CONStype([], TyName.tyName_INT);
val int_x_int = LambdaExp.RECORDtype[int_ty,int_ty]
val int_x_int_ref = LambdaExp.CONStype([int_x_int], TyName.tyName_REF)
val int_to_int = LambdaExp.ARROWtype([int_ty],[int_ty])
val exn_ty = LambdaExp.CONStype([], TyName.tyName_EXN);
val int_list = LambdaExp.CONStype([int_ty], TyName.tyName_LIST);
(*val cons_tau = LambdaExp.ARROWtype(LambdaExp.RECORDtype[alpha_ty,alpha_list],alpha_list);*)
val cons_tau =  LambdaExp.RECORDtype[alpha_ty,alpha_list] ;
val tau' = LambdaExp.RECORDtype[int_list,int_list];

fun dummy _ = None
fun layout_at_rho rho = 
      if !Flags.print_regions
        then Some(PP.LEAF("at " ^ PP.flatten1(Effect.layout_effect rho)))
      else None

fun layout_spread(p) =  RegionExp.layoutLambdaPgm layout_at_rho dummy p;
fun show_spread(p) = 
    let val r: string list ref = ref []
    in 
       PP.outputTree((fn s =>(r:= s:: !r; output(std_out, s))), layout_spread p, 30); 
       output(std_out, "\n"); 
       implode(rev(!r))
    end;
val _ = Flags.print_effects:= true;
val _ = Flags.print_types:= true;
val _ = Flags.print_regions:= true;
val _ = Flags.colwidth:= 80;
val source_db = LambdaExp.DATBINDS([]);


(* case 1 *)
local open LambdaExp 
in
  val x = Lvars.new_named_lvar "x"
  val lam_x_dot_7 = FN{pat = [(x,alpha_ty),(x,alpha_ty),(x,alpha_ty),(x,alpha_ty),(x,alpha_ty),(x,alpha_ty)], 
                       body = INTEGER 7}
  val source_db = DATBINDS([[([alpha],TyName.tyName_LIST,
                              [(Con.con_NIL,None),
                               (Con.con_CONS,Some cons_tau)
                               ])],
                            [([], new_tyname,
                              [(Con.mk_Con "mycon", Some tau')])]
                            ]);
  val p0 = PGM(source_db, lam_x_dot_7)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 1", 
"\nDATATYPE \n\
\   list : \n\
\   {\n\
\   nil : all 'a2 r7 r8.([('a2,r8)],[r7],) list, \n\
\   :: : \n\
\   all \n\
\      'a2 r6 r7 r8 e9.\n\
\      (((('a2,r8)*(([('a2,r8)],[r7],) list,r6)),r7)\n\
\       -e9(put(r6))->\n\
\       (([('a2,r8)],[r7],) list,r6)\n\
\      )\n\
\   }\n\
\; \n\
\DATATYPE \n\
\   instream : \n\
\   {\n\
\   mycon : \n\
\   all \n\
\      r10 r17 r16 r15 r14 r13 r12 r11 e18.\n\
\      ((((([(int,r11)],[r12],) list,r13)*(([(int,r14)],[r15],) list,r16)),r17)\n\
\       -e18(put(r10))->\n\
\       ((,[r11, r12, r13, r14, r15, r16, r17],) instream,r10)\n\
\      )\n\
\   }\n\
\; \n\
\ fn at r27 (x:('a2,r19),x:('a2,r20),x:('a2,r21),x:('a2,r22),x:('a2,r23),x:('a2,r24)) =>\n\
\ 7at r25\n\
\MUS:\n\
\  ((([('a2,r19), ('a2,r20), ('a2,r21), ('a2,r22), ('a2,r23), ('a2,r24)]\n\
\     -e26(put(r25))->\n\
\     (int,r25)\n\
\    ),\n\
\    r27\n\
\   )\n\
\  )\n\
\EFFECT:put(r27)", show_spread p');

(* case 2:  (fn x:int => fn y:int => x) 3 4 *)
local open LambdaExp 
in
  val x = Lvars.new_named_lvar "x"
  val y = Lvars.new_named_lvar "y"
  val lam_y_dot_x = FN{pat = [(y,int_ty)], 
                       body = VAR{lvar = x, 
                                   instances = []}}
  val K = FN{pat = [(x,int_ty)], body = lam_y_dot_x}
  val source_db = DATBINDS([]);
  val p0 = PGM(source_db, APP((APP(K, INTEGER 3), INTEGER 4)))
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 2", 
"\nletregion r31, e30(U), r29 \n\
\in  letregion r33, e32(put(r31)) \n\
\    in (fn e32 at r33 x:(int,r28)=>fn e30 at r31 y:(int,r29)=>x) 3at r28\n\
\    end (*r33, e32(put(r31))*) \n\
\    4at r29\n\
\end (*r31, e30(U), r29*)\n\
\MUS:((int,r28))\n\
\EFFECT:U(put(r28))", show_spread p');

(* case 3:  let id = fn x => x
            in id 3
            end *)

local open LambdaExp 
in
  val x = Lvars.new_named_lvar "x"
  val id = Lvars.new_named_lvar "id"
  val lam_x_dot_x = FN{pat = [(x,alpha_ty)], 
                       body = VAR{lvar = x, 
                                   instances = []}}
  val Let = LET{pat = [(id,[alpha],alpha_ty)], bind = lam_x_dot_x, 
                scope = APP(VAR{lvar =id, instances = [int_ty]},INTEGER 3)}
  val source_db = DATBINDS([]);
  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 3", 
"\nletregion r38 \n\
\in let val id :all 'a2 e37.(('a2,r36)-e37(U)->('a2,r36)) = fn e37 at r38 x:('a2,r36)=>x\n\
\   in  letregion e39 in id([int], [], [e39]) 3at r36end (*e39*)\n\
\   end \n\
\end (*r38*)\n\
\MUS:((int,r36))\n\
\EFFECT:U(put(r36))", show_spread p');
val _ = show_spread p';

(* case 4:  let id ['a] = fn x => x     THIS EXAMPLE ALSO SHOWS THAT VALUES OF DIFFERENT TYPE
            in id['b->'b] id['b]        CAN END UP IN THE SAME REGION, DUE TO LACK OF REGION
            end                         POLYMORPHISM.
*)

val beta = LambdaExp.fresh_tyvar()
val beta_ty = LambdaExp.TYVARtype beta;

local open LambdaExp 
in
  val x = Lvars.new_named_lvar "x"
  val id = Lvars.new_named_lvar "id"
  val lam_x_dot_x = FN{pat = [(x,alpha_ty)], 
                       body = VAR{lvar = x, 
                                   instances = []}}
  val Let = LET{pat = [(id,[alpha],alpha_ty)], bind = lam_x_dot_x, 
                scope = APP(VAR{lvar =id, instances = [ARROWtype([beta_ty],[beta_ty])]},
                            VAR{lvar =id, instances = [beta_ty]})}
  val source_db = DATBINDS([]);
  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 4", 
"\nlet val id :all 'a2 e42.(('a2,r43)-e42(U)->('a2,r43)) = fn e42 at r43 x:('a2,r43)=>x\n\
\in  letregion e47 \n\
\    in id([(('a3,r43)-e44->('a3,r43))], [], [e47]) id(['a3], [], [e44])\n\
\    end (*e47*)\n\
\end \n\
\MUS:(((('a3,r43)-e44->('a3,r43)),r43))\n\
\EFFECT:U(put(r43),get(@r43))", show_spread p');

(* case 5:  letrec id['a]('a ->'a) = fn x:'a => x
            in id[int] 5
            end *)

local open LambdaExp 
in
  val x = Lvars.new_named_lvar "x"
  val id = Lvars.new_named_lvar "id"
  val lam_x_dot_x = FN{pat = [(x,alpha_ty)], 
                       body = VAR{lvar = x, 
                                   instances = []}}
  val fix = FIX{functions = [{lvar = id, tyvars = [alpha], Type = 
                               ARROWtype([alpha_ty],[alpha_ty]), bind = lam_x_dot_x}],
                scope= APP(VAR{lvar = id, instances = [int_ty]}, INTEGER 5)}
  val p0 = PGM(source_db, fix)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 5", 
"\nletregion r55 \n\
\in let fun id at r55:all 'a2 r53 e54.(('a2,r53)-e54(U)->('a2,r53))(x) =x\n\
\   in  letregion r58, e57 in id([int], [r56], [e57]) at r58 5at r56end (*r58, e57*)\n\
\   end \n\
\end (*r55*)\n\
\MUS:((int,r56))\n\
\EFFECT:U(put(r56),get(@r56))", show_spread p');


(* case 6: let K['b,'c] = fn y:'b => fn z: 'c => y
           in letrec id['a]('a ->'a) = fn x:'a => x
              in id[int -> 'e -> int] K[int,'e] 5
              end
           end *)

val beta = LambdaExp.fresh_tyvar()
val beta_ty = LambdaExp.TYVARtype beta;
val gamma = LambdaExp.fresh_tyvar()
val gamma_ty = LambdaExp.TYVARtype gamma;
val d = LambdaExp.fresh_tyvar()
val d_ty = LambdaExp.TYVARtype d;
val e = LambdaExp.fresh_tyvar()
val e_ty = LambdaExp.TYVARtype e;

local open LambdaExp 
in
  val x = Lvars.new_named_lvar "x"
  val y = Lvars.new_named_lvar "y"
  val z = Lvars.new_named_lvar "z"
  val id = Lvars.new_named_lvar "id"
  val k = Lvars.new_named_lvar "K"
  val K = FN{pat = [(y,beta_ty)], 
             body = FN{pat = [(z,gamma_ty)],
                       body = VAR{lvar = y, instances = []}}}
  val lam_x_dot_x = FN{pat = [(x,alpha_ty)], 
                       body = VAR{lvar = x, 
                                   instances = []}}
  val fix = FIX{functions = [{lvar = id, tyvars = [alpha], Type = 
                               ARROWtype([alpha_ty],[alpha_ty]), bind = lam_x_dot_x}],
                scope= APP(APP(VAR{lvar = id, instances = [ARROWtype([int_ty], [ARROWtype([e_ty],[int_ty])])]},
                               VAR{lvar = k, instances = [int_ty,e_ty]}),
                           INTEGER 5)}
  val Let = LET{pat = [(k, [beta,gamma], ARROWtype([beta_ty],[ARROWtype([gamma_ty],[beta_ty])]))],
                bind = K,
                scope = fix}

  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 6", 
"\nletregion r79 \n\
\in let val K : all \n\
\                  'a4 'a5 e62 e64.\n\
\                  (('a4,r84)-e64(put(r78))->((('a5,r76)-e62(U)->('a4,r84)),r78)) = \n\
\           fn e64 at r79 y:('a4,r84)=>fn e62 at r78 z:('a5,r76)=>y\n\
\   in  letregion r72 \n\
\       in let fun id at r72:all 'a2 r70 e71.(('a2,r70)-e71(U)->('a2,r70))(x) =x\n\
\          in  letregion e73(put(r78)) \n\
\              in  letregion r81, e80 \n\
\                  in id(\n\
\                      [((int,r84)-e73(put(r78))->((('a7,r76)-e75->(int,r84)),r78))], \n\
\                      [r79], \n\
\                      [e80]\n\
\                     ) at r81 \n\
\                     K([int,'a7], [], [e75,e73])\n\
\                  end (*r81, e80*) \n\
\                  5at r84\n\
\              end (*e73(put(r78))*)\n\
\          end \n\
\       end (*r72*)\n\
\   end \n\
\end (*r79*)\n\
\MUS:(((('a7,r76)-e75->(int,r84)),r78))\n\
\EFFECT:U(put(r78),put(r84))", show_spread p');


(* case 7: let x = 3
           in let y = 4
              in 
                  5
              end
           end
*)
local open LambdaExp 
in
  val x = Lvars.new_named_lvar "x"
  val y = Lvars.new_named_lvar "y"

  val Let = LET{pat = [(x, [], int_ty)],
                bind = INTEGER 3,
                scope =LET{pat = [(x, [], int_ty)],
                           bind = INTEGER 4,
                           scope =INTEGER 5 } }

  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 7", "\n\
\letregion r85 \n\
\in let val x :int = 3at r85\n\
\   in  letregion r86 in let val x :int = 4at r86 in 5at r87 end end (*r86*)\n\
\   end \n\
\end (*r85*)\n\
\MUS:((int,r87))\n\
\EFFECT:U(put(r87))", show_spread p');

(* case 8: letrec id1 x = id2 x
              and id2 x = x
           in 
              id1   5
           end
*)
local open LambdaExp 
in
  val id1 = Lvars.new_named_lvar "id1"
  val id2= Lvars.new_named_lvar "id2"
  val x = Lvars.new_named_lvar "x"

  val lam_x_dot_id2x = FN{pat = [(x,alpha_ty)], 
                       body = APP(VAR{lvar = id2, instances = []},
                                  VAR{lvar = x, instances = []})}
  val lam_x_dot_x = FN{pat = [(x,alpha_ty)], 
                       body = VAR{lvar = x, instances = []}}
  val fix = FIX{functions = [{lvar = id1, tyvars = [alpha], Type = 
                               ARROWtype([alpha_ty],[alpha_ty]), bind = lam_x_dot_id2x},
                             {lvar = id2, tyvars = [alpha], Type = 
                               ARROWtype([alpha_ty],[alpha_ty]), bind = lam_x_dot_x}],
                scope= APP(VAR{lvar = id1, instances = [int_ty]},
                           INTEGER 5)}

  val p0 = PGM(source_db, fix)
end;

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 8", "\n\
\letregion r104 \n\
\in let fun id1 at r104: all 'a2 r97 r96 e100.(('a2,r96)-e100(U(get(r104),get(r97),get(r96)))->('a2,r97))\n\
\           (x) =\n\
\           letregion r99, e98 in id2([], [r96], [e98]) at r99 xend (*r99, e98*)\n\
\       and id2 at r104:all 'a2 r102 e103.(('a2,r102)-e103(U)->('a2,r102))(x) =x\n\
\   in  letregion r108, e107(get(r104),get(r106),get(r105)), r105 \n\
\       in id1([int], [r106,r105], [e107]) at r108 5at r105\n\
\       end (*r108, e107(get(r104),get(r106),get(r105)), r105*)\n\
\   end \n\
\end (*r104*)\n\
\MUS:((int,r106))\n\
\EFFECT:U(get(r106))", show_spread p');


(* case 9: if 1 > 0 then 5 else 7 *)

local open LambdaExp
in
  fun If(e1,e2,e3) = SWITCH_C(SWITCH(e1,[(Con.con_TRUE,e2),(Con.con_FALSE,e3)],None))
  fun bop(c,e1,e2) = PRIM(c, [e1, e2])
end;

local open LambdaExp
in
  val p0 = LambdaExp.PGM(source_db, If(bop(GREATER_INTprim,INTEGER 1, INTEGER 0), INTEGER 5, INTEGER 7))
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 9", "\n\
\letregion r112 \n\
\in (case letregion r111, r110 in (1at r110 > 0at r111) at r112end (*r111, r110*) \n\
\   of true => 5at r113 | false => 7at r113) (*case*) \n\
\end (*r112*)\n\
\MUS:((int,r113))\n\
\EFFECT:U(put(r113))", show_spread p');

(* case 10:
   letrec fac n = if n <= 0 then 1 else n * fac(n-1)
   in fac 5
   end
*)
local open LambdaExp
in
  val x = Lvars.new_named_lvar "x"
  val fac = Lvars.new_named_lvar "fac"
  val Fac = FN{pat = [(x,int_ty)], 
               body= If(bop(LESSEQ_INTprim, VAR{lvar = x, instances = []}, INTEGER 0),
                        INTEGER 1,
                        bop(MUL_INTprim,VAR{lvar = x, instances = []},
                            APP(VAR{lvar = fac, instances = []},
                                bop(MINUS_INTprim,VAR{lvar = x, instances = []},
                                              INTEGER 1))))}

  val p0 = LambdaExp.PGM(source_db, 
                         FIX{functions = [{lvar = fac, tyvars = [], Type = int_to_int,
                                          bind = Fac}],
                             scope = APP(VAR{lvar = fac, instances = []},
                                         INTEGER 5)})
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 10", "\n\
\letregion r131 \n\
\in let fun fac at r131: all r128 r119 e130.((int,r119)-e130(U(put(r128),get(r119)))->(int,r128))\n\
\           (x) =\n\
\               letregion r121 \n\
\               in (case letregion r120 in (x <= 0at r120) at r121end (*r120*) \n\
\                  of true => 1at r128\n\
\                  |  false => \n\
\                     letregion r123 \n\
\                     in (x*\n\
\                         letregion r125, e124, r122 \n\
\                         in fac([], [r123,r122], [e124]) at r125 \n\
\                            letregion r126 in (x-1at r126) at r122end (*r126*)\n\
\                         end (*r125, e124, r122*)\n\
\                        ) at r128\n\
\                     end (*r123*)\n\
\                  ) (*case*) \n\
\               end (*r121*)\n\
\   in  letregion r135, e134(put(r133),get(r132)), r132 \n\
\       in fac([], [r133,r132], [e134]) at r135 5at r132\n\
\       end (*r135, e134(put(r133),get(r132)), r132*)\n\
\   end \n\
\end (*r131*)\n\
\MUS:((int,r133))\n\
\EFFECT:U(get(r133),put(@r133))", show_spread p');

fun doit n =
  (say "starting";
   let fun loop 0 = say "finished"
         | loop n = (SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
                     loop (n-1)
                     )
   in loop n
   end
)

(* case 11: val x = nil
            val y = [1]
            val z = [1,2]
            val v = [(1,2),(3,4)]
*)

fun cons(e1, e2) = let open LambdaExp 
                   in PRIM(CONprim{con = Con.con_CONS, instances = [int_ty]} , 
                                              [PRIM(RECORDprim,[e1,e2])])
                   end
fun cons'(e1, e2) = let open LambdaExp 
                   in PRIM(CONprim{con = Con.con_CONS, instances = [RECORDtype[int_ty,int_ty]]} , 
                                              [PRIM(RECORDprim,[e1,e2])])
                   end
fun pair(e1, e2) = let open LambdaExp 
                   in PRIM(RECORDprim,[e1,e2])
                   end

local open LambdaExp
  val x = Lvars.new_named_lvar "x"
  val y = Lvars.new_named_lvar "y"
  val z = Lvars.new_named_lvar "z"
  val v = Lvars.new_named_lvar "v"
  val NIL  = PRIM(CONprim{con = Con.con_NIL, instances = [int_ty]},[])
  val NIL'  = PRIM(CONprim{con = Con.con_NIL, instances = [RECORDtype[int_ty,int_ty]]},[])
  val x_exp = NIL
  val y_exp = cons(INTEGER 1, NIL)
  val z_exp = cons(INTEGER 1, cons(INTEGER 2, NIL))
  val v_exp = cons'(pair(INTEGER 1, INTEGER 2), cons'(pair(INTEGER 3, INTEGER 4), NIL'))
  val e = 
  LET{pat = [(x, [], int_list)],
      bind = x_exp,
      scope = LET{pat = [(y, [], int_list)],
                  bind = y_exp,
                  scope = LET{pat = [(z, [], int_list)],
                              bind = z_exp,
                              scope = LET{pat = [(v, [], int_list)],
                                          bind = v_exp,
                                          scope = PRIM(RECORDprim,[VAR{lvar = x, instances = []},
                                                               VAR{lvar = y, instances = []},
                                                               VAR{lvar = z, instances = []},
                                                               VAR{lvar = v, instances = []}])}}}}
           
in
  val p0 = LambdaExp.PGM(source_db, e)
end
val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest'("TestSpreadExp, case 11", "", show_spread p');


(* case 12: 

 let
    exception Hd
    fun hd l = case l of nil => raise Hd
                 | :: => let val p = decon(::, l) 
                          in #1 p
                          end
 in hd nil
 end
*)
local open LambdaExp
  val x = Lvars.new_named_lvar "x"
  val l = Lvars.new_named_lvar "l"
  val p = Lvars.new_named_lvar "p"
  val hd = Lvars.new_named_lvar "hd"
  val HD = Excon.mk_ExCon "Hd"

  val Let = LET{pat = [(p, [], RECORDtype[alpha_ty,alpha_list])],
                bind = PRIM(DECONprim{con = Con.con_CONS, instances = [alpha_ty]},
                            [VAR{lvar = l, instances = []}]) ,
                scope=   PRIM(SELECTprim 0, [VAR{lvar = p, instances = []}])  }
  val sw =  SWITCH_C(SWITCH(VAR{lvar = l, instances = []},
                            [(Con.con_NIL, RAISE(PRIM(EXCONprim HD, []),LambdaExp.Types [alpha_ty])),
                             (Con.con_CONS, Let)
                             ],None))
  val lam = FN{pat = [(l, alpha_list)], body = sw}
  val e = EXCEPTION(HD, None, 
                    FIX{functions = [{lvar = hd, tyvars = [alpha], Type = ARROWtype([alpha_list], [alpha_ty]),
                                      bind = lam}],
                        scope = APP(VAR{lvar = hd, instances = [int_ty]}, 
                                    PRIM(CONprim{con = Con.con_NIL, instances = [int_ty]},[]))})


in
  val p0 = LambdaExp.PGM(source_db, e)
end;
val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 12", "\n\
\let exception Hd : (exn,r195) (* exn value or name at r195 *)\n\
\in  letregion r211 \n\
\    in let fun hd at r211: all \n\
\                              'a2 r204 r203 r209 e210.\n\
\                              ((([('a2,r209)],[r203],) list,r204)-e210(U(get(r203),get(r204)))->('a2,r209))\n\
\               (l) =\n\
\                   (case l \n\
\                   of nil => raise Hd\n\
\                   |  :: => \n\
\                      let val p :(('a2,r209)*(([('a2,r209)],[r203],) list,r204)) = \n\
\                              letregion e208 in decon_::(['a2], [r204,r203,r209], [e208]) lend (*e208*)\n\
\                      in  #0 p\n\
\                      end \n\
\                   ) (*case*) \n\
\       in  letregion r216, e215(get(r213),get(r214)), r214, r213 \n\
\           in hd([int], [r214,r213,r212], [e215]) at r216 nil([int], [r213,r212], []) at r214\n\
\           end (*r216, e215(get(r213),get(r214)), r214, r213*)\n\
\       end \n\
\    end (*r211*)\n\
\end \n\
\MUS:((int,r212))\n\
\EFFECT:U(U(get(r212)),put(r195))", show_spread p');


(* case 13: exception E1 
            exception E2 
            exception E3 of int
              case E3(5) of
                E1 => 8
              | E2 =>  9
              | E3(x) => x+1
              | _ => 10
*)
fun myResetCount() = Effect.count:= 0;
val _ = myResetCount();
local open LambdaExp
in
  val x = Lvars.new_named_lvar "x"
  val x0 = Lvars.new_named_lvar "x0"
  val E1 = Excon.mk_ExCon "E1"
  val E2 = Excon.mk_ExCon "E2"
  val E3 = Excon.mk_ExCon "E3"
  val Case = LET{pat = [(x0, [], exn_ty)],
                 bind = PRIM(EXCONprim E3, [INTEGER 5]),
                 scope = SWITCH_E(SWITCH(VAR{lvar = x0, instances = []},
                                         [(E1,INTEGER 8),
                                          (E2,INTEGER 9)(*,
                                          (E3,LET{pat = [(x,[],int_ty)], 
                                                  bind = PRIM(DEEXCONprim,[VAR{lvar = x0, instances = []}]),
                                                  scope = PRIM(PLUS_INTprim,[VAR{lvar = x0, instances = []},
                                                                             INTEGER 1])})
                                          *)],
                                         Some(INTEGER 10)))}
  val p0 = LambdaExp.PGM(source_db, 
                         EXCEPTION(E1,None,
                                   EXCEPTION(E2, None,
                                             EXCEPTION(E3, Some int_ty, Case))))
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 13", "\n\
\let exception E1 : (exn,r2) (* exn value or name at r2 *); \n\
\    exception E2 : (exn,r2) (* exn value or name at r2 *); \n\
\    exception E3 : (((int,r4)-e3->(exn,r2)),r2) (* exn value or name at r2 *)\n\
\in  let val x0 :exn = E3 at r2 5at r4\n\
\    in  (case x0 of E1 => 8at r8 | E2 => 9at r8 | _ => 10at r8) (*case*) \n\
\    end \n\
\end \n\
\MUS:((int,r8))\n\
\EFFECT:U(U(U(U(put(r2),put(r4),put(r8),get(@r2)),@put),@put),@put)", show_spread p');



(*case 14:  ref 5 *)
val _ = myResetCount();
local open LambdaExp
in
  val ref5 = PRIM(REFprim{instance = int_ty}, [INTEGER 5])
  val p0 = PGM(source_db, ref5)
end
val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 14", 
"\nref  at r2 5at r1MUS:((([(int,r1)],,) ref,r2))EFFECT:U(put(r1),put(r2))", show_spread p');


(*case 15:  ref (5, 6) *)
val _ = myResetCount();
local open LambdaExp
in
  val ref56 = PRIM(REFprim{instance = int_x_int}, [
                  PRIM(RECORDprim, [INTEGER 5, INTEGER 6])])
  val p0 = PGM(source_db, ref56)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 15", "\n\
\ref  at r4 (5at r2, 6at r1) at r3\n\
\MUS:((([(((int,r2)*(int,r1)),r3)],,) ref,r4))\n\
\EFFECT:U(U(put(r1),put(r2),put(r3)),put(r4))", show_spread p');


(*case 16:  !(ref(5.6)) *)
val _ = myResetCount();
local open LambdaExp
in
  val ref56 = PRIM(REFprim{instance = int_x_int}, [
                  PRIM(RECORDprim, [INTEGER 5, INTEGER 6])])
  val dref_ref_56 = PRIM(DEREFprim{instance = int_x_int}, [ref56])
  val p0 = PGM(source_db, dref_ref_56)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 16", "\n\
\letregion r4 in  ! (ref  at r4 (5at r2, 6at r1) at r3)end (*r4*)\n\
\MUS:((((int,r2)*(int,r1)),r3))\n\
\EFFECT:U(put(r3),put(r2),put(r1))", show_spread p');

(*case 17:  let val r = ref (5,6)
            in 
                r:= (7,8)
            end
*)
val _ = myResetCount();
local open LambdaExp
in
  val r = Lvars.new_named_lvar "r"
  val ref56 = PRIM(REFprim{instance = int_x_int}, [
                  PRIM(RECORDprim, [INTEGER 5, INTEGER 6])])
  val Let = LET{pat = [(r, [], int_x_int_ref)],
                bind = ref56 ,
                scope =PRIM(ASSIGNprim{instance = int_x_int},
                            [VAR{lvar = r, instances = []},
                             PRIM(RECORDprim, [INTEGER 7, INTEGER 8])])}
  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 17", "\n\
\letregion r4, r3, r2, r1 \n\
\in let val r :([(((int,r2)*(int,r1)),r3)],,) ref = ref  at r4 (5at r2, 6at r1) at r3\n\
\   in  (r := (7at r2, 8at r1) at r3) at r8\n\
\   end \n\
\end (*r4, r3, r2, r1*)\n\
\MUS:((unit,r8))\n\
\EFFECT:U(put(r8))", show_spread p');

(*case 18:  let val r = ref (5,6)
            in 
                (r:= (7,8), r)
            end
*)
val _ = myResetCount();
local open LambdaExp
in
  val r = Lvars.new_named_lvar "r"
  val ref56 = PRIM(REFprim{instance = int_x_int}, [
                  PRIM(RECORDprim, [INTEGER 5, INTEGER 6])])
  val asg= PRIM(ASSIGNprim{instance = int_x_int},
                            [VAR{lvar = r, instances = []},
                             PRIM(RECORDprim, [INTEGER 7, INTEGER 8])])
  val Let = LET{pat = [(r, [], int_x_int_ref)],
                bind = ref56 ,
                scope = PRIM(RECORDprim,[asg, VAR{lvar = r, instances = []}])}
  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 18", "\n\
\let val r :([(((int,r2)*(int,r1)),r3)],,) ref = ref  at r4 (5at r2, 6at r1) at r3\n\
\in  ((r := (7at r2, 8at r1) at r3) at r8, r) at r9\n\
\end \n\
\MUS:((((unit,r8)*(([(((int,r2)*(int,r1)),r3)],,) ref,r4)),r9))\n\
\EFFECT:U(put(r3),put(r2),put(r1),put(r9),put(r8),put(r4))", show_spread p');


(*case 19:  let val r = ref (5,6)
            in 
                (r:= (7,8),
                 ! r)
            end
*)

val _ = myResetCount();
local open LambdaExp
in
  val r = Lvars.new_named_lvar "r"
  val ref56 = PRIM(REFprim{instance = int_x_int}, [
                  PRIM(RECORDprim, [INTEGER 5, INTEGER 6])])
  val asg= PRIM(ASSIGNprim{instance = int_x_int},
                            [VAR{lvar = r, instances = []},
                             PRIM(RECORDprim, [INTEGER 7, INTEGER 8])])
  val contents_r = PRIM(DEREFprim{instance = int_x_int},[VAR{lvar = r, instances = []}])
  val Let = LET{pat = [(r, [], int_x_int_ref)],
                bind = ref56 ,
                scope = PRIM(RECORDprim,[asg, contents_r])}
  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 19", "\n\
\letregion r4 \n\
\in let val r :([(((int,r2)*(int,r1)),r3)],,) ref = ref  at r4 (5at r2, 6at r1) at r3\n\
\   in  ((r := (7at r2, 8at r1) at r3) at r8,  ! r) at r9\n\
\   end \n\
\end (*r4*)\n\
\MUS:((((unit,r8)*(((int,r2)*(int,r1)),r3)),r9))\n\
\EFFECT:U(put(r3),put(r2),put(r1),put(r9),put(r8))", show_spread p');

(*case 20:  let val r = ref (5,6)
            in 
                <r:= (7,8),
                 ! r>
            end
*)

val _ = myResetCount();
local open LambdaExp
in
  val r = Lvars.new_named_lvar "r"
  val ref56 = PRIM(REFprim{instance = int_x_int}, [
                  PRIM(RECORDprim, [INTEGER 5, INTEGER 6])])
  val asg= PRIM(ASSIGNprim{instance = int_x_int},
                            [VAR{lvar = r, instances = []},
                             PRIM(RECORDprim, [INTEGER 7, INTEGER 8])])
  val contents_r = PRIM(DEREFprim{instance = int_x_int},[VAR{lvar = r, instances = []}])
  val Let = LET{pat = [(r, [], int_x_int_ref)],
                bind = ref56 ,
                scope = PRIM(UB_RECORDprim,[asg, contents_r])}
  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 20", "\n\
\letregion r4 \n\
\in let val r :([(((int,r2)*(int,r1)),r3)],,) ref = ref  at r4 (5at r2, 6at r1) at r3\n\
\   in  < ! r, (r := (7at r2, 8at r1) at r3) at r8>\n\
\   end \n\
\end (*r4*)\n\
\MUS:((unit,r8),(((int,r2)*(int,r1)),r3))\n\
\EFFECT:U(put(r3),put(r2),put(r1),put(r8))", show_spread p');

(* case 21:
   ccall("+", 2,3)
*)

val _ = myResetCount();
local open LambdaExp
in
  val c = PRIM(CCALLprim("+",{instance = int_ty}), [INTEGER 2, INTEGER 3])
  val p0 = PGM(source_db, c)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 21", "\n\
\letregion r3, r2 in ccall(+, 2at r3, 3at r2):(int,r1)[ at r1]end (*r3, r2*)\n\
\MUS:((int,r1))\n\
\EFFECT:U(put(r1))", show_spread p');

(* case 22:
   ccall("explode", "abc")
*)
val _ = myResetCount();
local open LambdaExp
in
  val c = PRIM(CCALLprim("explode",{instance = string_list}), [STRING "abc"])
  val p0 = PGM(source_db, c)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 22", "\n\
\letregion r4 \n\
\in ccall(explode, \"abc\"at r4):(([(string,r1)],[r2],) list,r3)[ at r3, at r2, at r1]\n\
\end (*r4*)\n\
\MUS:((([(string,r1)],[r2],) list,r3))\n\
\EFFECT:U(put(r3),put(r2),put(r1))", show_spread p');

(* case 23:
   ccall("implode", ["a"])
*)

val _ = myResetCount();
local open LambdaExp
in
  val l = PRIM(CONprim{con = Con.con_CONS, instances = [string_ty]},
               [PRIM(RECORDprim,[STRING "a", PRIM(CONprim{con = Con.con_NIL, instances = [string_ty]}, [])])])
  val c = PRIM(CCALLprim("implode",{instance = string_ty}), [l])
  val p0 = PGM(source_db, c)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 23", "\n\
\letregion r6, e5, r9, r11, r7 \n\
\in ccall(implode, \n\
\         ::([string], [r9,r11,r7], [e5]) at r9 \n\
\         (\"a\"at r7, nil([string], [r11,r7], []) at r9) at r11\n\
\   ):(string,r1)[ at r1]\n\
\end (*r6, e5, r9, r11, r7*)\n\
\MUS:((string,r1))\n\
\EFFECT:U(put(r1))", show_spread p');

(* case 24:
   fn x => (2,(3,4.5)) = (7,x)
*)

val _ = myResetCount();
local open LambdaExp
in
  val x = Lvars.new_named_lvar "x"
  val p1 = PRIM(RECORDprim, [INTEGER 2, PRIM(RECORDprim, [INTEGER 3, REAL 4.5])])
  val p2 = PRIM(RECORDprim, [INTEGER 7, VAR{lvar = x, instances = []}])
  val lam = FN{pat = [(x,RECORDtype[int_ty,real_ty])], body = 
               PRIM(EQUALprim{instance = RECORDtype[int_ty,RECORDtype[int_ty,real_ty]]},[p1,p2])}
  val p0 = PGM(source_db, lam)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 24", "\n\
\ fn at r13 x:(((int,r2)*(real,r1)),r3)=>\n\
\ letregion r10, r9, r8, r7, r6, r5, r4 \n\
\ in (2at r7, (3at r5, 4.5at r4) at r6) at r8= at r11(* domain of = is: (((int,r7)*(((int,r5)*(real,r4)),r6)),r8)*(((int,r9)*(((int,r2)*(real,r1)),r3)),r10) *)\n\
\    (7at r9, x) at r10\n\
\ end (*r10, r9, r8, r7, r6, r5, r4*)\n\
\MUS:\n\
\  ((((((int,r2)*(real,r1)),r3)-e12(U(put(r11),get(r2),get(r1),get(r3)))->(bool,r11)),\n\
\    r13\n\
\   )\n\
\  )\n\
\EFFECT:put(r13)", show_spread p');

(* case 25:
   (fn x => resetRegions x)(2,(3,4.5))
*)

val _ = myResetCount();
local open LambdaExp
in
  val x = Lvars.new_named_lvar "x"
  val p1 = PRIM(RECORDprim, [INTEGER 2, PRIM(RECORDprim, [INTEGER 3, REAL 4.5])])
  val lam = FN{pat = [(x,RECORDtype[int_ty,RECORDtype[int_ty,real_ty]])], body = 
               PRIM(RESET_REGIONSprim{instance = RECORDtype[int_ty,RECORDtype[int_ty,real_ty]]},
            [VAR{lvar = x, instances = []}])}
  val app = APP(lam, p1)
  val p0 = PGM(source_db, app)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 25", "\n\
\letregion r8, e7(U(put(r5),put(r3),put(r1),put(r2),put(r4),put(r6))), r5, r4, r3, r2, r1\n\
\\n\
\in  (fn e7 at r8 x:(((int,r4)*(((int,r2)*(real,r1)),r3)),r5)=>\n\
\     (resetRegions x) at r6\n\
\    ) \n\
\    (2at r4, (3at r2, 4.5at r1) at r3) at r5\n\
\end (*r8, e7(U(put(r5),put(r3),put(r1),put(r2),put(r4),put(r6))), r5, r4, r3, r2, r1\n\
\*)\n\
\MUS:((unit,r6))\n\
\EFFECT:U(put(r6))", show_spread p');

(* case 26:
   let x = (2,(3,4.5))
   in resetRegions x
   end
*)

val _ = myResetCount();
local open LambdaExp
in
  val x = Lvars.new_named_lvar "x"
  val p1 = PRIM(RECORDprim, [INTEGER 2, PRIM(RECORDprim, [INTEGER 3, REAL 4.5])])
  val Let = LET{pat = [(x,[],RECORDtype[int_ty,RECORDtype[int_ty,real_ty]])],
                bind = p1,
                scope =   
                   PRIM(RESET_REGIONSprim{instance = RECORDtype[int_ty,RECORDtype[int_ty,real_ty]]},
                        [VAR{lvar = x, instances = []}])}
  val p0 = PGM(source_db, Let)
end

val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);
val _ = etest("TestSpreadExp, case 26", "\n\
\letregion r5, r4, r3, r2, r1 \n\
\in let val x :((int,r4)*(((int,r2)*(real,r1)),r3)) = \n\
\           (2at r4, (3at r2, 4.5at r1) at r3) at r5\n\
\   in  (resetRegions x) at r6\n\
\   end \n\
\end (*r5, r4, r3, r2, r1*)\n\
\MUS:((unit,r6))\n\
\EFFECT:U(put(r6))", show_spread p');

(* case 27: (failing resetRegions)
   let id = fn x=> x
   in resetRegions id
   end
*)

val _ = myResetCount();
local open LambdaExp
in
  val x = Lvars.new_named_lvar "x"
  val id= Lvars.new_named_lvar "id"
  val lam = FN{pat = [(x, int_ty)], body = VAR{lvar = x, instances =[]}}
  val Let = LET{pat = [(id,[],ARROWtype([int_ty],[int_ty]))],
                bind = lam,
                scope =   
                   PRIM(RESET_REGIONSprim{instance = RECORDtype[int_ty,RECORDtype[int_ty,real_ty]]},
                        [VAR{lvar = id, instances = []}])}
  val p0 = PGM(source_db, Let)
end
;
val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, [])
handle _ => (output(std_out, "\n\n case 27: OK\n\n"); (c1,rse',p'));

(* case 28: 

   val [x] = [3]

  DATATYPE [] 
   LET <v27:int list> = 
       PRIM(
          ::, 
          [PRIM(record, [3,PRIM(nil, [])])]
       )
   IN  SWITCH(
          v27:() of 
          ::
          => 
          SWITCH(
             PRIM(select(1), [PRIM(decon(::), [v27:()])]) of 
             nil
             => 
             LET <x:int> = PRIM(select(0), [PRIM(decon(::), [v27:()])])
             IN  FRAME(x: FORALL'a6.'a6)
             END 
             | 
             _ => RAISE(PRIM(Bind, []),'a7)
          )
          | 
          _ => RAISE(PRIM(Bind, []),'a7)
       )
   END 

*)


val _ = myResetCount();
local open LambdaExp
in 
  val x  = Lvars.new_named_lvar "x"
  val v27  = Lvars.new_named_lvar "v27"
  val v27_rhs = PRIM(CONprim{con = Con.con_CONS, instances = [int_ty]},
                    [PRIM(RECORDprim, [INTEGER 3, PRIM(CONprim{con = Con.con_NIL, instances = [int_ty]}, [])])])
  val rule1 = (Con.con_CONS, LET{pat = [(x,[],int_ty)],
                bind = PRIM(SELECTprim(0), [PRIM(DECONprim{con = Con.con_CONS, instances = [int_ty]}, 
                                                [VAR{lvar = v27, instances = []}])]),
                scope =   FRAME{declared_lvars = [{lvar= x , tyvars = [] , Type = int_ty}],
                                declared_excons = []}})
  val rule2 = (Con.con_NIL, RAISE(PRIM(EXCONprim(Excon.ex_BIND), []), RaisedExnBind))
  val sw = SWITCH(VAR{lvar = v27, instances = []},
                  [rule1, rule2],
                  None)
  val biglet = LET{pat = [(v27, [], int_list)],
                   bind = v27_rhs,
                   scope = SWITCH_C sw}
  val p0 = PGM(source_db, biglet)
end
;
val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 28", "\n\
\let val v27 :([(int,r11)],[r12],) list = \n\
\        ::([int], [r13,r12,r11], [e4]) at r13 \n\
\        (3at r11, nil([int], [r12,r11], []) at r13) at r12\n\
\in  (case v27 \n\
\    of :: => \n\
\       let val x :int = \n\
\               #0 letregion e14 in decon_::([int], [r13,r12,r11], [e14]) v27end (*e14*)\n\
\       in  {|x: (int,r11)|}\n\
\       end \n\
\    |  nil => raise Bind\n\
\    ) (*case*) \n\
\end \n\
\MUS:frame\n\
\EFFECT:\n\
\  U\n\
\  (U(U,U(U,U(get(r12),get(r13))),@get,@U),\n\
\   U(e4,U(U(put(@r12),put(@r13)),put(r11),@put))\n\
\  )"
, show_spread p');

(* case 29:  val x: real = sin 3.0 *)

val _ = Effect.resetCount();
local open LambdaExp
in 
  val x  = Lvars.new_named_lvar "x"
  val rhs = PRIM(SINprim, [REAL 3.0])
  val Let = LET{pat = [(x,[],real_ty)],
                bind = rhs,
                scope = FRAME{declared_lvars = [{lvar= x , tyvars = [] , Type = real_ty}],
                                declared_excons = []}}
  val p0 = PGM(source_db, Let)
end
;
val (c1,rse', p') = SpreadExpression.spreadPgm(Effect.initCone,
                                               RegionStatEnv.initial,
                                               p0, []);

val _ = etest("TestSpreadExp, case 29", "\n\
\let val x :real = \n\
\        letregion e4(put(r3),get(r2)), r2 \n\
\        in sin([], [r3,r2], [e4]) <3.0at r2>\n\
\        end (*e4(put(r3),get(r2)), r2*)\n\
\in  {|x: (real,r3)|}\n\
\end \n\
\MUS:frame\n\
\EFFECT:U(U,U(put(r3),get(r1)))"
, show_spread p');


val _ = say ("failed at:" ^ implode(!dump) ^ "\n");

end (*TestSpreadExp*)

*)