
(* Build-file for the compiler. It returns a compile basis structure
   and a compile structure. This version believes in the core language
   only. *)

(*$BuildCompile : TYNAME TOPDEC_GRAMMAR ELAB_INFO STATOBJECT
     ENVIRONMENTS FINMAP FINMAPEQ BASIC_IO REPORT FLAGS PRETTYPRINT
     CRASH TIMING COMPILER_ENV COMPILE_BASIS COMPILE EFFECT RTYPE NAME
     FREE_IDS Lvars Lvarset Con Excon CompilerEnv LambdaExp OptLambda
     LambdaBasics HashTable QuasiEnv CompileBasis LambdaStatSem
     EliminateEq Compile CompileDec MatchCompiler IOStreams RType
     Effect SpreadDatatype SpreadExpression RegionStatEnv DiGraphScc
     DiGraph UnionFindPoly Stack CompLamb CompLambEnv KAMBackend
     RegionExp CConst OrderFinMap OrderSet NatSet RegInf Mul MulExp
     MulInf AtInf DropRegions PhysSizeInf ListSort IntSet IntFinMap
     EqFinMap LocallyLiveVariables RegFlow RegionFlowGraphProfiling*)

functor BuildCompile (structure TyName : TYNAME
		      structure Name : NAME
		      structure TopdecGrammar: TOPDEC_GRAMMAR
			sharing type TopdecGrammar.tycon = TyName.tycon
		      structure FreeIds : FREE_IDS
			sharing type FreeIds.id = TopdecGrammar.id

		      structure ElabInfo : ELAB_INFO
			sharing type ElabInfo.ElabInfo = TopdecGrammar.info
			    and type ElabInfo.TypeInfo.longid = TopdecGrammar.DecGrammar.longid
			    and type ElabInfo.TypeInfo.strid = TopdecGrammar.strid
			    and type ElabInfo.TypeInfo.tycon = TopdecGrammar.tycon
			    and type ElabInfo.TypeInfo.id = TopdecGrammar.id
		      structure StatObject: STATOBJECT
			sharing type StatObject.Type = ElabInfo.TypeInfo.Type
			    and type StatObject.TyName = TyName.TyName
			    and type StatObject.TyVar = ElabInfo.TypeInfo.TyVar
		      structure Environments : ENVIRONMENTS
			sharing type Environments.TyEnv = ElabInfo.TypeInfo.TyEnv
			    and type Environments.Type = StatObject.Type
			    and type Environments.tycon = TopdecGrammar.tycon
			    and type Environments.TypeFcn = StatObject.TypeFcn
			    and type Environments.TypeScheme = StatObject.TypeScheme
			    and type Environments.TyVar = StatObject.TyVar
			    and type Environments.TyName = TyName.TyName
			    and type Environments.id = TopdecGrammar.id
			    and type Environments.strid = TopdecGrammar.strid
			    and type Environments.Env = ElabInfo.TypeInfo.Env
			    and Environments.TyName = TyName
		      structure FinMap: FINMAP
		      structure FinMapEq : FINMAPEQ 
		      structure BasicIO: BASIC_IO
		      structure Report: REPORT
		      structure Flags: FLAGS
		      structure PP: PRETTYPRINT
			sharing type FinMap.StringTree
				     = FinMapEq.StringTree
				     = TopdecGrammar.StringTree
				     = ElabInfo.StringTree
				     = PP.StringTree = TyName.StringTree
			    and type PP.Report = Report.Report
		      structure Crash: CRASH
		      structure Timing: TIMING
                  ) : sig
			structure CompilerEnv : COMPILER_ENV
			structure CompileBasis: COMPILE_BASIS
			structure Compile: COMPILE
		       end  = 
  struct

    structure DecGrammar = TopdecGrammar.DecGrammar
    structure SCon = DecGrammar.SCon
    structure Lab = DecGrammar.Lab
    structure TyVar = DecGrammar.TyVar
    structure Ident = DecGrammar.Ident
    structure StrId = DecGrammar.StrId
    structure TyCon = DecGrammar.TyCon

    structure Lvars = Lvars(structure Name = Name)
    structure Lvarset = Lvarset(structure Lvars = Lvars)
    structure Con = Con(structure Name = Name)
    structure Excon = Excon(structure Name = Name)

    structure LvarEqMap = EqFinMap(structure Report = Report
				   structure PP = PP
				   type dom = Lvars.lvar
				   val eq = Lvars.eq)

    structure ConEqMap = EqFinMap(structure Report = Report
				  structure PP = PP
				  type dom = Con.con
				  val eq = Con.eq)

    structure ExconEqMap = EqFinMap(structure Report = Report
				    structure PP = PP
				    type dom = Excon.excon
				    val eq = Excon.eq)

    structure TyNameEqMap = EqFinMap(structure Report = Report
				     structure PP = PP
				     type dom = TyName.TyName
				     val eq = TyName.eq)

    structure LambdaExp =
      LambdaExp(structure Lvars = Lvars
                structure Con = Con
                structure Excon = Excon
                structure TyName = TyName
                structure PP = PP
                structure Crash = Crash
                structure Flags = Flags
               )

    structure LambdaBasics = 
      LambdaBasics(structure Lvars = Lvars
		   structure TyName = TyName
		   structure TLE = LambdaExp
		   structure FinMap = FinMap
		   structure FinMapEq = FinMapEq
		   structure Crash = Crash
		   structure Flags = Flags)

    structure LambdaStatSem =
      LambdaStatSem(structure Lvars = Lvars
		    structure Con = Con
		    structure Excon = Excon
		    structure TyName = TyName
		    structure FinMap = FinMap
		    structure FinMapEq = FinMapEq
		    structure NatSet = NatSet(structure PP = PP)
		    structure LambdaExp = LambdaExp
		    structure LambdaBasics = LambdaBasics
		    structure Crash = Crash
		    structure PP = PP
		    structure Flags = Flags)

    structure EliminateEq =
      EliminateEq(structure Lvars = Lvars
		  structure Con = Con
		  structure TyName = TyName
		  structure LambdaExp = LambdaExp
		  structure Crash = Crash
		  structure Flags = Flags
		  structure PP = PP
		  structure LvarMap = LvarEqMap
		  structure TyNameMap = TyNameEqMap
		  structure TyVarMap =
		    OrderFinMap(structure Order = struct type T = LambdaExp.tyvar
							 fun lt (a:T) b = LambdaExp.lt_tyvar(a,b)
						  end
				structure PP = PP
				structure Report = Report))

    structure Stack = Stack()

    structure UnionFindPoly = UF_with_path_halving_and_union_by_rank()

    structure DiGraph = DiGraph(structure UF = UnionFindPoly
                                structure Stack = Stack
                                structure PP = PP
                                structure Flags = Flags
                                structure Crash = Crash)

   structure Effect:EFFECT = Effect(structure G = DiGraph
				    structure Flags = Flags
				    structure PP = PP
				    structure Crash = Crash
				    structure Report = Report)

   structure RType:RTYPE = RType(structure Flags = Flags
				 structure Crash = Crash
				 structure E = Effect
				 structure DiGraph = DiGraph
				 structure L = LambdaExp
				 structure TyName = TyName
				 structure PP = PP)

   structure RegionStatEnv = RegionStatEnv(
     structure Flags=Flags
     structure R = RType 
     structure E = Effect
     structure TyName = TyName
     structure Con = Con
     structure Excon = Excon
     structure Lvar = Lvars
     structure Crash = Crash
     structure TyNameMap = TyNameEqMap
     structure LvarMap = LvarEqMap
     structure ConMap = ConEqMap
     structure ExconMap = ExconEqMap
     structure L = LambdaExp
     structure PP = PP)

   structure RegionExp = 
        RegionExp(structure R = RType
                  structure Eff = Effect
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
           structure FinMap = FinMap    (* for tyvars *)
           structure E = LambdaExp
           structure E' = RegionExp 
           structure RSE = RegionStatEnv
           structure TyName = TyName
           structure Crash = Crash
           structure PP = PP
          )


  structure CConst = CConst
    (structure Flags = Flags
     structure Crash = Crash
     structure TyName = TyName)

     structure SpreadExpression =  SpreadExpression(
       structure Con = Con
       structure ExCon = Excon
       structure E = LambdaExp
       structure E'= RegionExp
       structure Eff = Effect
       structure R= RType
       structure RSE = RegionStatEnv
       structure SpreadDatatype = SpreadDatatype
       structure Lvars =  Lvars
       structure TyName = TyName
       structure FinMap = FinMap
       structure Crash = Crash
       structure PP = PP
       structure Flags=Flags
       structure CConst = CConst
	 )

   structure RegInf= RegInf(
      structure Con = Con
      structure ExCon = Excon
      structure TyName = TyName
      structure Exp = RegionExp
      structure RType = RType
      structure Effect = Effect
      structure RSE = RegionStatEnv
      structure Lvar= Lvars
      structure Crash = Crash
      structure PrettyPrint = PP
      structure Flags = Flags)

   structure EffVarEnv=
      OrderFinMap(structure Order =
                  struct
                    type T = Effect.effect
                    fun lt(a: T) b = Effect.lt_eps_or_rho(a,b)
                  end
                  structure PP =PP
                  structure Report = Report
                 )

   structure Sort = ListSort()

   structure HashTable = HashTable(structure PP = PP)

   structure QM_EffVarEnv = QuasiEnv(
      structure OFinMap = EffVarEnv
      val key = Effect.key_of_eps_or_rho
      structure HashTable = HashTable
      structure PP = PP
      structure Crash = Crash)

   structure Mul = Mul(
      structure Lam = LambdaExp
      structure Eff = Effect
      structure LvarMap = LvarEqMap
      structure Crash = Crash
      structure Flags = Flags
      structure Lvar = Lvars
      structure TyName = TyName
      structure RType = RType
      structure RSE = RegionStatEnv
      structure PP = PP
      structure Sort = Sort
      structure UF = UnionFindPoly
      structure QM_EffVarEnv = QM_EffVarEnv
      )

   structure MulExp = MulExp(
      structure Flags = Flags
      structure Con = Con
      structure Excon= Excon
      structure RegionExp = RegionExp
      structure Eff = Effect
      structure Mul = Mul
      structure R = RType
      structure TyName = TyName
      structure Crash = Crash
      structure PP = PP
      structure Lvar = Lvars
      structure Lam = LambdaExp
      structure RSE = RegionStatEnv
      )      

   structure MulInf = MulInf(
      structure Lvar = Lvars
      structure TyName = TyName
      structure RType = RType
      structure MulExp=MulExp
      structure RegionExp = RegionExp
      structure Mul = Mul
      structure Eff = Effect
      structure Flags = Flags
      structure PP = PP
      structure Crash = Crash)
      
    structure KAMBackend = KAMBackend(structure PP = PP
				      structure Report = Report
				      structure Name = Name
				      structure CConst = CConst
				      structure Flags = Flags
				      structure Timing = Timing
				      structure Crash = Crash)

    structure CompilerEnv =
      CompilerEnv(structure Ident = Ident
		  structure StrId = StrId
                  structure Con = Con
                  structure Excon = Excon
		  structure Environments = Environments
		  structure LambdaBasics = LambdaBasics
                  structure TyCon = TyCon
                  structure TyVar = TyVar
                  structure TyName = TyName
                  structure LambdaExp = LambdaExp
                  structure Lvars = Lvars
                  structure FinMap = FinMap
		  structure FinMapEq = FinMapEq
                  structure PP = PP
		  structure Flags = Flags
                  structure Crash = Crash
                 )

    structure LvarDiGraphScc = 
      DiGraphScc(structure InfoDiGraph = 
		   struct
		     type nodeId = Lvars.lvar
		     type info = Lvars.lvar
		     type edgeInfo = unit
		     val lt = fn a => fn b => Lvars.lt(a,b)
		     fun getId lv = lv
		   end
		 structure PP = PP
		 structure Flags = Flags
		 structure Crash = Crash
		 structure Report = Report)


    structure OptLambda = 
      OptLambda(structure Lvars = Lvars
		structure LambdaExp = LambdaExp
		structure LvarDiGraphScc = LvarDiGraphScc
		structure LambdaBasics = LambdaBasics
		structure FinMap = FinMap
		structure LvarMap = LvarEqMap
		structure BasicIO = BasicIO
		structure Con = Con
		structure Excon = Excon
		structure TyName = TyName
		structure Flags = Flags
		structure Crash = Crash
		structure PP = PP
		  )
 
    structure RegFlow = RegFlow(
                structure Lvars = Lvars
		structure Con = Con
		structure Excon = Excon
		structure PrettyPrint = PP
		structure Crash = Crash
                structure Flags = Flags
                structure Eff = Effect
                structure RType = RType
                structure MulExp = MulExp)

    structure LLV = LocallyLiveVariables(
                structure Lvars = Lvars
                structure Lvarset = Lvarset
		structure Con = Con
		structure Excon = Excon
		structure PrettyPrint = PP
		structure Crash = Crash
                structure Flags = Flags
                structure Eff = Effect
                structure RType = RType
                structure MulExp = MulExp
                structure MulInf = MulInf)

    structure IntFinMap = IntFinMap(
      structure PP = PP
      structure Report=Report);


    structure AtInf = AtInf(structure Lvars = Lvars
                            structure Excon = Excon
                            structure MulExp = MulExp
			    structure Mul = Mul
			    structure Eff = Effect
                            structure RType = RType
                            structure LLV = LLV
                            structure RegFlow = RegFlow
                            structure BT = IntFinMap
                            structure RegvarBT = EffVarEnv
			    structure PP = PP
                            structure Flags = Flags
			    structure Crash = Crash
			    structure Report = Report
                            structure Timing = Timing)

    structure DropRegions = DropRegions(structure MulExp = MulExp
					structure AtInf = AtInf
					structure RSE = RegionStatEnv
					structure RType = RType
					structure Lvars = Lvars
					structure Crash = Crash
					structure PP = PP
					structure FinMapEq = FinMapEq
					structure Flags = Flags
					structure Eff = Effect
                                        structure Mul= Mul
                                        structure RegionExp = RegionExp
                                        structure ExCon = Excon)

    structure PhysSizeInf = PhysSizeInf(structure MulExp = MulExp
					structure Effect = Effect
					structure TyName = TyName
					structure AtInf = AtInf
					structure Lvars = Lvars
					structure ExCon = Excon
					structure Mul = Mul
					structure DiGraph = DiGraph
					structure RType = RType
					structure RegvarFinMap = EffVarEnv
					structure LvarMap = LvarEqMap
					structure Flags = Flags
					structure Crash = Crash
					structure PP = PP
					structure CConst = CConst
					  )

    structure CompLambEnv = CompLambEnv(structure Lvars = Lvars
					structure Con = Con
					structure Excon = Excon
					structure Effect = Effect
					structure MulExp = MulExp
					structure KAM =  KAMBackend.KAM
					structure LvarFinMap = LvarEqMap
					structure RegvarFinMap = EffVarEnv
					structure PhysSizeInf = PhysSizeInf
					structure ConFinMap = ConEqMap
					structure ExconFinMap = ExconEqMap
					structure PP = PP
					structure Crash = Crash)

    structure IntSet = IntSet(structure PP = PP)

    structure RegionFlowGraphProfiling =
      RegionFlowGraphProfiling(structure Effect = Effect
			       structure AtInf = AtInf
			       structure PhySizeInf = PhysSizeInf
			       structure PP = PP
			       structure Flags = Flags
			       structure Crash = Crash
			       structure Report = Report)
      
    structure CompLamb = CompLamb(structure Con = Con
				  structure Excon = Excon
				  structure Lvars = Lvars
				  structure TyName = TyName
				  structure KAM = KAMBackend.KAM
				  structure KamVar = KAMBackend.KamVar
				  structure Effect = Effect
				  structure RType = RType
				  structure IntSet = IntSet
				  structure MulExp = MulExp
				  structure AtInf = AtInf
				  structure PhysSizeInf = PhysSizeInf
				  structure CE = CompLambEnv
				  structure RegionFlowGraphProfiling = RegionFlowGraphProfiling
				  structure PP = PP
				  structure Flags = Flags
				  structure Crash = Crash
				  structure CConst = CConst)


    structure CompileBasis =
      CompileBasis(structure CompilerEnv = CompilerEnv
		   structure Con = Con
		   structure Excon = Excon
		   structure TyName = TyName
		   structure LambdaStatSem = LambdaStatSem
		   structure EliminateEq = EliminateEq
		   structure OptLambda = OptLambda
		   structure Effect = Effect
		   structure RegionStatEnv = RegionStatEnv
		   structure DropRegions = DropRegions
		   structure PhysSizeInf = PhysSizeInf
		   structure CompLamb = CompLamb
		   structure FreeIds = FreeIds
		   structure Report = Report
		   structure PP = PP
		   structure Flags = Flags
		   structure Mul = Mul
		     )

    structure MatchCompiler = MatchCompiler(
                            structure SCon = SCon
			    structure Ident = Ident
                            structure Lab = Lab
                            structure Grammar = DecGrammar
                            structure CompilerEnv = CompilerEnv
                            structure ElabInfo = ElabInfo
                            structure Lvars = Lvars
                            structure FinMap = FinMap
                            structure BasicIO = BasicIO
                            structure Report = Report
                            structure Flags = Flags
                            structure PrettyPrint = PP
                            structure Crash = Crash
                          )

     structure CompileDec = CompileDec(
			structure Ident = Ident
                        structure Lab = Lab
                        structure SCon = SCon
                        structure Con = Con
                        structure Excon = Excon
                        structure TyCon = TyCon
                        structure TyName = TyName
                        structure TyVar = TyVar
                        structure Grammar = DecGrammar
			structure TopdecGrammar = TopdecGrammar
                        structure StatObject = StatObject
			structure Environments = Environments
                        structure Lvars = Lvars
                        structure LambdaExp = LambdaExp
                        structure LambdaBasics = LambdaBasics
                        structure CompilerEnv = CompilerEnv

                        structure MatchCompiler = MatchCompiler
			structure ElabInfo = ElabInfo
                        structure FinMap = FinMap
                        structure FinMapEq = FinMapEq
                        structure PrettyPrint = PP
                        structure Report = Report
                        structure Flags = Flags
                        structure Crash = Crash
                      )

    structure Compile =
      Compile(structure RegionExp = RegionExp
	      structure Excon = Excon
	      structure FinMap = FinMap
	      structure Lvars = Lvars
	      structure Name = Name
	      structure LambdaExp = LambdaExp
	      structure LambdaStatSem = LambdaStatSem
	      structure EliminateEq = EliminateEq
	      structure SpreadExp = SpreadExpression
	      structure RegInf= RegInf
	      structure AtInf = AtInf
	      structure DropRegions = DropRegions
	      structure PhysSizeInf = PhysSizeInf
	      structure RegionFlowGraphProfiling = RegionFlowGraphProfiling
	      structure CompLamb = CompLamb
	      structure KAMBackend = KAMBackend
	      structure CompilerEnv = CompilerEnv
	      structure RType = RType
	      structure Effect = Effect
	      structure Mul = Mul
	      structure MulExp = MulExp
	      structure MulInf = MulInf
	      structure CompileDec = CompileDec
	      structure OptLambda = OptLambda
	      structure CompileBasis = CompileBasis
	      structure Report = Report
	      structure Flags = Flags
	      structure PP = PP
	      structure Crash = Crash
	      structure Timing = Timing)

  end
