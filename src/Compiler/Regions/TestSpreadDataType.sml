
(*
functor TestSpreadDatatype() =
struct

(*$TestSpreadDatatype: Crash PrettyPrint Flags DiGraph Effect Lvars LambdaExp TyName 
              Ident Con Excon Report TyCon Timestamp 
              BasicIO  Stack UnionFindPoly StrId RType SpreadDatatype FinMap 
              RegionStatEnv RegionExp*)


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

structure Effect:EFFECT = Effect(structure G = DiGraph
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

fun pp(t) = PP.flatten1 t
fun say s = output(std_out, s^"\n")
fun say' s= output(std_out, s)
fun etest(label,expected,found) =
 say(label ^ (if expected = found then " OK" else " ****** NOT OK *******" ^
"\n expected: " ^ expected ^ 
"\n found:    " ^ found));
fun etest'(label,expected,found) = say (label ^ found);


val alpha = LambdaExp.fresh_tyvar()
val new_tyname = TyName.tyName_INSTREAM (* could be any one *)
val alpha_ty = LambdaExp.TYVARtype alpha;
val alpha_list = LambdaExp.CONStype([alpha_ty], TyName.tyName_LIST);
val int_ty = LambdaExp.CONStype([], TyName.tyName_INT);
val int_list = LambdaExp.CONStype([int_ty], TyName.tyName_LIST);
(*val cons_tau = LambdaExp.ARROWtype(LambdaExp.RECORDtype[alpha_ty,alpha_list],alpha_list);*)
val cons_tau =  LambdaExp.RECORDtype[alpha_ty,alpha_list] ;
val tau' = LambdaExp.RECORDtype[int_list,int_list];
val source_db = LambdaExp.DATBINDS([[([alpha],TyName.tyName_LIST,
                                      [(Con.con_NIL,None),
                                       (Con.con_CONS,Some cons_tau)
                                      ])],
                                    [([], new_tyname,
                                      [(Con.mk_Con "mycon", Some tau')])]
                                   ]);
val (rse', datbinds') = SpreadDatatype.spreadDatbinds RegionStatEnv.initial source_db Effect.initCone;
fun dummy _ = PP.LEAF"";
val (rho_0,c) = Effect.freshRho(Effect.initCone)
val t0 = RegionExp.TR(RegionExp.INTEGER(5,([],[],[])),
          [](*(RType.TYVAR alpha,rho_0)*),
          Effect.empty);
val st = RegionExp.layoutLambdaPgm dummy dummy 
            (RegionExp.PGM(datbinds',t0));
val _ = etest("test of spreadDatatype (1)", 
"DATATYPE list : {nil : all a2 r7 r8.([(a2,r8)],[r7],) list, :: : all a2 r6 r7 r8 e9.((((a2,r8)*(([(a2,r8)],[r7],) list,r6)),r7)->(([(a2,r8)],[r7],) list,r6))}; DATATYPE instream : {mycon : all r10 r17 r16 r15 r14 r13 r12 r11 e18.((((([(int,r11)],[r12],) list,r13)*(([(int,r14)],[r15],) list,r16)),r17)->((,[r11, r12, r13, r14, r15, r16, r17],) instream,r10))}; RegionExp.layoutExp is not implemented",
PP.flatten1(st))

end (*TestSpreadDatatype *)

*)