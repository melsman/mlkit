
signature TOOLS =
  sig
    structure BasicIO: BASIC_IO
    structure FinMap: FINMAP
    structure FinMapEq : FINMAPEQ
    structure SortedFinMap: SORTED_FINMAP
    structure IntFinMap : MONO_FINMAP where type dom = int
    structure WordFinMap : MONO_FINMAP where type dom = word

    structure PrettyPrint: PRETTYPRINT
      sharing type FinMap.StringTree
	           = FinMapEq.StringTree
		   = SortedFinMap.StringTree
		   = PrettyPrint.StringTree
	           = IntFinMap.StringTree
                   = WordFinMap.StringTree

    structure Flags: FLAGS
            
    structure Report: REPORT
      sharing type FinMap.Report
		   = FinMapEq.Report
		   = SortedFinMap.Report
		   = PrettyPrint.Report
	           = Flags.Report
		   = Report.Report

    structure Timestamp: TIMESTAMP
    structure Crash: CRASH
    structure Timing: TIMING
  end;


functor Tools(): TOOLS =
  struct
    structure BasicIO = BasicIO()
    structure Crash = Crash(structure BasicIO = BasicIO)
    structure Report = Report(structure BasicIO = BasicIO)
    structure Timestamp = Timestamp()

    local
      val raggedRight = ref true
      val colwidth = ref 100
    in
      structure PrettyPrint = PrettyPrint(structure Report = Report
					  structure Crash = Crash
					  val raggedRight = raggedRight
					  val colwidth = colwidth)
      structure Flags = Flags(structure Crash = Crash
			      structure Report = Report
			      structure PP = PrettyPrint
			      val raggedRight = raggedRight
			      val colwidth = colwidth)
    end

    structure IntFinMap = IntFinMap(structure Report = Report
				    structure PP = PrettyPrint
				   )

    structure WordFinMap = WordFinMap(structure Report = Report
				      structure PP = PrettyPrint)

    structure FinMap = FinMap(structure Report = Report
			      structure PP = PrettyPrint
			     )

    structure FinMapEq = FinMapEq(structure Report = Report
				  structure PP = PrettyPrint
				    )

    structure SortedFinMap = SortedFinMap(structure Report = Report
					  structure PP = PrettyPrint
					 )

    structure Timing = Timing(structure Flags = Flags
			      structure Crash = Crash)
  end;



signature ALL_INFO =
  sig
    structure SourceInfo      : SOURCE_INFO
    structure DFInfo          : DF_INFO
    structure ParseInfo       : PARSE_INFO
      sharing ParseInfo.SourceInfo = SourceInfo
      sharing ParseInfo.DFInfo = DFInfo
    structure ErrorInfo       : ERROR_INFO
    structure TypeInfo        : TYPE_INFO
    structure OverloadingInfo : OVERLOADING_INFO
    structure ElabInfo : ELAB_INFO
      sharing ElabInfo.ParseInfo = ParseInfo
      sharing ElabInfo.ErrorInfo = ErrorInfo
      sharing ElabInfo.TypeInfo = TypeInfo
      sharing ElabInfo.OverloadingInfo = OverloadingInfo
  end;



signature BASICS =
  sig
    structure Tools : TOOLS 
    structure StrId : STRID

    structure Ident : IDENT
      sharing type Ident.strid = StrId.strid

    structure InfixBasis : INFIX_BASIS
      sharing type InfixBasis.id = Ident.id
      sharing type InfixBasis.Report = Tools.Report.Report
      sharing type InfixBasis.StringTree = Tools.PrettyPrint.StringTree

    structure SCon : SCON

    structure Lab : LAB
    structure TyVar : TYVAR

    structure TyCon : TYCON
      sharing type TyCon.strid = StrId.strid

    structure Name : NAME

    structure TyName : TYNAME
      sharing type TyName.tycon = TyCon.tycon
      sharing type TyName.name = Name.name
      sharing type TyName.StringTree = Tools.PrettyPrint.StringTree

    structure StatObject : STATOBJECT
      sharing StatObject.TyName    = TyName
      sharing type StatObject.ExplicitTyVar = TyVar.SyntaxTyVar
      sharing type StatObject.strid     = StrId.strid
      sharing type StatObject.scon      = SCon.scon
      sharing type StatObject.lab       = Lab.lab

    structure SigId : SIGID
    structure FunId : FUNID

    structure LexBasics: LEX_BASICS
      sharing type LexBasics.Report = Tools.Report.Report
      sharing type LexBasics.StringTree = Tools.PrettyPrint.StringTree

    structure PreElabDecGrammar: DEC_GRAMMAR
      sharing type PreElabDecGrammar.StringTree = Tools.PrettyPrint.StringTree
      sharing PreElabDecGrammar.Ident = Ident
      sharing PreElabDecGrammar.StrId = StrId
      sharing PreElabDecGrammar.TyCon = TyCon
      sharing PreElabDecGrammar.TyVar = TyVar
      sharing PreElabDecGrammar.Lab = Lab
      sharing PreElabDecGrammar.SCon = SCon

    structure Environments : ENVIRONMENTS
      sharing Environments.TyName = StatObject.TyName
      sharing type Environments.Type = StatObject.Type
      sharing type Environments.TyVar = StatObject.TyVar
      sharing type Environments.TypeScheme = StatObject.TypeScheme
      sharing type Environments.TypeFcn = StatObject.TypeFcn
      sharing type Environments.realisation = StatObject.realisation
      sharing type Environments.level = StatObject.level
      sharing type Environments.id = Ident.id
      sharing type Environments.longid = Ident.longid
      sharing type Environments.Substitution = StatObject.Substitution
      sharing type Environments.ty = PreElabDecGrammar.ty
      sharing type Environments.longtycon = TyCon.longtycon
      sharing type Environments.longstrid = StrId.longstrid
      sharing type Environments.ExplicitTyVar = TyVar.SyntaxTyVar
      sharing type Environments.strid = StrId.strid
      sharing type Environments.valbind = PreElabDecGrammar.valbind
      sharing type Environments.pat = PreElabDecGrammar.pat
      sharing type Environments.Report = Tools.Report.Report

    structure ModuleStatObject : MODULE_STATOBJECT
      sharing ModuleStatObject.TyName = TyName
      sharing type ModuleStatObject.Env = Environments.Env
      sharing type ModuleStatObject.realisation = StatObject.realisation
      sharing type ModuleStatObject.strid = StrId.strid
      sharing type ModuleStatObject.longstrid = StrId.longstrid
      sharing type ModuleStatObject.longtycon = TyCon.longtycon
      sharing type ModuleStatObject.Type = StatObject.Type
      sharing type ModuleStatObject.TypeScheme = StatObject.TypeScheme
      sharing type ModuleStatObject.TypeFcn = StatObject.TypeFcn
      sharing type ModuleStatObject.TyVar = StatObject.TyVar
      sharing type ModuleStatObject.id = Ident.id

    structure ModuleEnvironments : MODULE_ENVIRONMENTS
      sharing ModuleEnvironments.TyName = TyName
      sharing type ModuleEnvironments.realisation = StatObject.realisation
      sharing type ModuleEnvironments.longstrid = StrId.longstrid
      sharing type ModuleEnvironments.longtycon = TyCon.longtycon
      sharing type ModuleEnvironments.Context = Environments.Context
      sharing type ModuleEnvironments.FunSig = ModuleStatObject.FunSig
      sharing type ModuleEnvironments.TyStr = Environments.TyStr
      sharing type ModuleEnvironments.TyVar = StatObject.TyVar
      sharing type ModuleEnvironments.id = Ident.id
      sharing type ModuleEnvironments.longid = Ident.longid
      sharing type ModuleEnvironments.strid = StrId.strid
      sharing type ModuleEnvironments.sigid = SigId.sigid
      sharing type ModuleEnvironments.funid = FunId.funid
      sharing type ModuleEnvironments.Env = Environments.Env
      sharing type ModuleEnvironments.Sig = ModuleStatObject.Sig
      sharing type ModuleEnvironments.Report = Tools.Report.Report

    structure OpacityEnv : OPACITY_ENV
      sharing OpacityEnv.TyName = TyName
      sharing type OpacityEnv.funid = FunId.funid
      sharing type OpacityEnv.StringTree = Tools.PrettyPrint.StringTree
      sharing type OpacityEnv.realisation = StatObject.realisation

    structure AllInfo : ALL_INFO
      sharing type AllInfo.TypeInfo.Type = StatObject.Type
      sharing type AllInfo.TypeInfo.TyVar = StatObject.TyVar
      sharing type AllInfo.TypeInfo.TyEnv = Environments.TyEnv
      sharing type AllInfo.TypeInfo.longid = Ident.longid
      sharing type AllInfo.TypeInfo.realisation = StatObject.realisation
      sharing type AllInfo.TypeInfo.Env = Environments.Env
      sharing type AllInfo.TypeInfo.strid = StrId.strid
      sharing type AllInfo.TypeInfo.tycon = TyCon.tycon
      sharing type AllInfo.TypeInfo.id = Ident.id
      sharing type AllInfo.TypeInfo.opaq_env = OpacityEnv.opaq_env
      sharing AllInfo.TypeInfo.TyName = StatObject.TyName
      sharing type AllInfo.TypeInfo.Basis = ModuleEnvironments.Basis
      sharing type AllInfo.ErrorInfo.Type = StatObject.Type
      sharing type AllInfo.ErrorInfo.TypeScheme = StatObject.TypeScheme
      sharing type AllInfo.ErrorInfo.TyVar = StatObject.TyVar
      sharing type AllInfo.ErrorInfo.TyName = TyName.TyName
      sharing type AllInfo.ErrorInfo.TypeFcn = StatObject.TypeFcn
      sharing type AllInfo.ErrorInfo.lab = Lab.lab
      sharing type AllInfo.ErrorInfo.tycon = TyCon.tycon
      sharing type AllInfo.ErrorInfo.longid = Ident.longid
      sharing type AllInfo.ErrorInfo.longtycon = TyCon.longtycon
      sharing type AllInfo.ErrorInfo.strid = StrId.strid
      sharing type AllInfo.ErrorInfo.longstrid = StrId.longstrid
      sharing type AllInfo.ErrorInfo.sigid = SigId.sigid
      sharing type AllInfo.ErrorInfo.funid = FunId.funid
      sharing type AllInfo.ErrorInfo.id = Ident.id
      sharing type AllInfo.ErrorInfo.SigMatchError = ModuleStatObject.SigMatchError
      sharing type AllInfo.SourceInfo.pos = LexBasics.pos
      sharing type AllInfo.SourceInfo.Report = Tools.Report.Report
      sharing type AllInfo.ElabInfo.StringTree = Tools.PrettyPrint.StringTree
      sharing type AllInfo.OverloadingInfo.RecType = StatObject.RecType
      sharing type AllInfo.OverloadingInfo.TyVar = StatObject.TyVar
      sharing type AllInfo.OverloadingInfo.StringTree = Tools.PrettyPrint.StringTree
      sharing type AllInfo.ElabInfo.ParseInfo = PreElabDecGrammar.info
      sharing type AllInfo.ElabInfo.ParseInfo.DFInfo.InfixBasis = InfixBasis.Basis
  end;




functor Basics(structure Tools: TOOLS): BASICS =
  struct
    structure Tools = Tools

    structure StrId = StrId(structure Timestamp = Tools.Timestamp
			    structure Crash = Tools.Crash)

    structure Ident = Ident(structure StrId = StrId
			    structure Crash = Tools.Crash)

    structure InfixBasis = InfixBasis
      (structure Ident = Ident
       structure FinMap = Tools.FinMap
       structure Report = Tools.Report
       structure PP = Tools.PrettyPrint)

    structure SigId = SigId()
          and FunId = FunId()
          and TyVar = TyVar(structure Crash = Tools.Crash)
	  and Lab = Lab()
    	  and SCon = SCon()
    	  and TyCon = TyCon(structure StrId = StrId
			    structure Crash = Tools.Crash)

    structure Name = Name (structure Crash = Tools.Crash)

    structure TyName = TyName(structure TyCon = TyCon
			      structure IntFinMap = Tools.IntFinMap
			      structure Crash = Tools.Crash
			      structure Name = Name
			      structure Flags = Tools.Flags
			      structure PrettyPrint = Tools.PrettyPrint
			      structure Report = Tools.Report)

      structure StatObject : STATOBJECT = 
	StatObject(structure SortedFinMap  = Tools.SortedFinMap
		   structure Name = Name
		   structure IntFinMap = Tools.IntFinMap
		   structure Ident = Ident
		   structure Lab = Lab
		   structure SCon = SCon
		   structure TyName = TyName
		   structure TyCon = TyCon
		   structure ExplicitTyVar = TyVar
		   structure Timestamp = Tools.Timestamp
		   structure Flags = Tools.Flags
		   structure Report = Tools.Report
		   structure FinMap = Tools.FinMap
		   structure FinMapEq = Tools.FinMapEq
		   structure PP = Tools.PrettyPrint
		   structure Crash = Tools.Crash)

   (* LexBasics is needed by SourceInfo, as well as all the parsing
      stuff. *)

    structure LexBasics = LexBasics(structure BasicIO = Tools.BasicIO
				    structure Report = Tools.Report
				    structure PP = Tools.PrettyPrint
				    structure Flags = Tools.Flags
				    structure Crash = Tools.Crash)

    structure DFInfo = DFInfo
      (structure PrettyPrint = Tools.PrettyPrint
       structure InfixBasis = InfixBasis)
      
    structure SourceInfo = SourceInfo
      (structure LexBasics = LexBasics
       structure PrettyPrint = Tools.PrettyPrint
       structure Crash = Tools.Crash)

    structure ParseInfo = ParseInfo
      (structure SourceInfo = SourceInfo
       structure DFInfo = DFInfo
       structure PrettyPrint = Tools.PrettyPrint
       structure Crash = Tools.Crash)

    structure PreElabDecGrammar = DecGrammar
      (structure GrammarInfo =
	 struct
	   type GrammarInfo = ParseInfo.ParseInfo
	   val bogus_info = 
	     ParseInfo.from_SourceInfo(SourceInfo.from_positions LexBasics.DUMMY LexBasics.DUMMY)
	 end
       structure Lab = Lab
       structure SCon = SCon
       structure TyVar = TyVar
       structure TyCon = TyCon
       structure StrId = StrId
       structure Ident = Ident
       structure PrettyPrint = Tools.PrettyPrint)

    structure Environments : ENVIRONMENTS = Environments
      (structure DecGrammar = PreElabDecGrammar
       structure Ident = Ident
       structure TyCon = TyCon
       structure StrId = StrId
       structure StatObject = StatObject
       structure TyName = TyName
       structure PP = Tools.PrettyPrint
       structure SortedFinMap = Tools.SortedFinMap
       structure FinMap = Tools.FinMap
       structure Timestamp = Tools.Timestamp
       structure Report = Tools.Report
       structure Flags = Tools.Flags
       structure Crash = Tools.Crash) 



    structure ModuleStatObject =
      ModuleStatObject(structure StrId        = StrId
		       structure SigId        = SigId
		       structure FunId        = FunId
		       structure TyCon        = TyCon
		       structure TyName       = TyName
		       structure Name         = Name
		       structure StatObject   = StatObject
		       structure Environments = Environments
		       structure FinMap       = Tools.FinMap
		       structure PP           = Tools.PrettyPrint
		       structure Report       = Tools.Report
		       structure Flags        = Tools.Flags
		       structure Crash        = Tools.Crash)


    structure ModuleEnvironments =
      ModuleEnvironments(structure StrId             = StrId
			 structure SigId             = SigId
			 structure FunId             = FunId
			 structure TyCon             = TyCon
			 structure Ident             = Ident
			 structure FinMap            = Tools.FinMap
			 structure FinMapEq          = Tools.FinMapEq
			 structure StatObject        = StatObject
			 structure Environments      = Environments
			 structure ModuleStatObject  = ModuleStatObject
			 structure PP                = Tools.PrettyPrint
			 structure Report	     = Tools.Report
			 structure Flags             = Tools.Flags
			 structure Crash             = Tools.Crash)

    structure OpacityEnv = OpacityEnv(structure FunId = FunId
				      structure Crash = Tools.Crash					
				      structure PP = Tools.PrettyPrint
				      structure Report = Tools.Report
				      structure Environments = Environments)
    structure AllInfo =
      struct
	structure SourceInfo = SourceInfo
	structure DFInfo = DFInfo
	structure ParseInfo = ParseInfo
	structure ErrorInfo = ErrorInfo
	  (structure StatObject = StatObject
	   structure ModuleStatObject = ModuleStatObject
	   structure Ident = Ident
	   structure Lab   = Lab
	   structure TyCon = TyCon
	   structure TyName = TyName
	   structure SigId = SigId
	   structure StrId = StrId
	   structure FunId = FunId
	   structure Report = Tools.Report
	   structure PrettyPrint = Tools.PrettyPrint)
	structure TypeInfo = TypeInfo
	  (structure Crash = Tools.Crash
	   structure Ident = Ident
	   structure ModuleEnvironments = ModuleEnvironments
	   structure StrId = StrId
	   structure TyCon = TyCon
	   structure StatObject=StatObject
	   structure Environments=Environments
	   structure PP = Tools.PrettyPrint
	   structure OpacityEnv = OpacityEnv)
	structure OverloadingInfo = OverloadingInfo
	  (structure StatObject = StatObject
	   structure PrettyPrint = Tools.PrettyPrint
	   structure Flags = Tools.Flags)
	structure ElabInfo = ElabInfo
	  (structure ParseInfo = ParseInfo
	   structure ErrorInfo = ErrorInfo
	   structure TypeInfo = TypeInfo
	   structure OverloadingInfo = OverloadingInfo
	   structure PrettyPrint = Tools.PrettyPrint
	   structure Crash = Tools.Crash)
      end
  end;



signature TOPDEC_PARSING =
  sig
    structure Basics: BASICS

    structure PreElabDecGrammar: DEC_GRAMMAR
      sharing PreElabDecGrammar = Basics.PreElabDecGrammar

    structure PreElabTopdecGrammar: TOPDEC_GRAMMAR
      sharing PreElabTopdecGrammar.DecGrammar = PreElabDecGrammar
      sharing PreElabTopdecGrammar.SigId = Basics.SigId
      sharing PreElabTopdecGrammar.FunId = Basics.FunId

    structure InfixBasis: INFIX_BASIS
      sharing InfixBasis = Basics.InfixBasis

    structure Parse: PARSE
      sharing type Parse.topdec = PreElabTopdecGrammar.topdec
      sharing type Parse.InfixBasis = InfixBasis.Basis
      sharing type Parse.Report = Basics.Tools.Report.Report
  end;



functor TopdecParsing(structure Basics: BASICS): TOPDEC_PARSING =
  struct
    structure Basics = Basics
    structure Tools = Basics.Tools
    structure AllInfo = Basics.AllInfo

    structure PreElabDecGrammar = Basics.PreElabDecGrammar

    structure PreElabTopdecGrammar : TOPDEC_GRAMMAR = TopdecGrammar
      (structure DecGrammar = PreElabDecGrammar
       structure SigId = Basics.SigId
       structure FunId = Basics.FunId
       structure PrettyPrint = Tools.PrettyPrint)

    structure InfixBasis = Basics.InfixBasis

    structure Parse = Parse
      (structure TopdecGrammar = PreElabTopdecGrammar
       structure LexBasics = Basics.LexBasics
       structure ParseInfo = AllInfo.ParseInfo
       structure InfixBasis = InfixBasis
       structure Report = Tools.Report
       structure PrettyPrint = Tools.PrettyPrint
       structure FinMap = Tools.FinMap
       structure BasicIO = Tools.BasicIO
       structure Flags = Tools.Flags
       structure Crash = Tools.Crash)
  end;



signature ELABORATION =
  sig
    structure Basics : BASICS

    structure ElabRepository : ELAB_REPOSITORY
      sharing type ElabRepository.funid = Basics.FunId.funid
      sharing type ElabRepository.name = Basics.Name.name
      sharing type ElabRepository.ElabBasis = Basics.ModuleEnvironments.Basis 
      sharing type ElabRepository.absprjid = Basics.ModuleEnvironments.absprjid
      sharing type ElabRepository.longstrid = Basics.ModuleEnvironments.longstrid
      sharing type ElabRepository.InfixBasis = Basics.InfixBasis.Basis
      sharing type ElabRepository.opaq_env = Basics.OpacityEnv.opaq_env
      sharing ElabRepository.TyName = Basics.TyName

    structure RepositoryFinMap : MONO_FINMAP
      where type dom = Basics.ModuleEnvironments.absprjid * Basics.FunId.funid

    structure ElabTopdec : ELABTOPDEC
      sharing type ElabTopdec.StaticBasis = ElabRepository.ElabBasis
      sharing type ElabTopdec.StringTree = Basics.Tools.PrettyPrint.StringTree
      sharing type ElabTopdec.absprjid = Basics.ModuleEnvironments.absprjid
(*
      sharing type ElabTopdec.PreElabTopdec = Basics.PreElabTopdecGrammar.topdec
*)

    structure PostElabDecGrammar : DEC_GRAMMAR
      sharing type PostElabDecGrammar.lab = Basics.Lab.lab
      sharing type PostElabDecGrammar.scon = Basics.SCon.scon
      sharing type PostElabDecGrammar.tycon = Basics.TyCon.tycon
      sharing type PostElabDecGrammar.longtycon = Basics.TyCon.longtycon
      sharing type PostElabDecGrammar.tyvar = Basics.TyVar.SyntaxTyVar
      sharing type PostElabDecGrammar.id = Basics.Ident.id
      sharing type PostElabDecGrammar.longid = Basics.Ident.longid = Basics.ModuleEnvironments.longid
      sharing type PostElabDecGrammar.info
		   = Basics.AllInfo.ElabInfo.ElabInfo
      sharing type PostElabDecGrammar.StringTree
	           = Basics.Tools.PrettyPrint.StringTree

    structure PostElabTopdecGrammar : TOPDEC_GRAMMAR
      sharing PostElabTopdecGrammar.DecGrammar = PostElabDecGrammar
      sharing PostElabTopdecGrammar.StrId = Basics.StrId
      sharing PostElabTopdecGrammar.SigId = Basics.SigId
      sharing PostElabTopdecGrammar.FunId = Basics.FunId
      sharing type PostElabTopdecGrammar.topdec = ElabTopdec.PostElabTopdec

  end;



functor Elaboration(structure TopdecParsing : TOPDEC_PARSING): ELABORATION =
  struct
    structure Basics = TopdecParsing.Basics

    local
      structure Tools = Basics.Tools
      structure AllInfo = Basics.AllInfo
      structure ElabInfo = AllInfo.ElabInfo
    in
      
      structure PostElabDecGrammar =
	DecGrammar(structure GrammarInfo =
		     struct
		       type GrammarInfo = ElabInfo.ElabInfo
		       val bogus_info = ElabInfo.from_ParseInfo TopdecParsing.PreElabDecGrammar.bogus_info
		     end
		   structure Lab         = Basics.Lab
		   structure SCon        = Basics.SCon
		   structure TyVar       = Basics.TyVar
		   structure TyCon       = Basics.TyCon
		   structure StrId       = Basics.StrId
		   structure Ident       = Basics.Ident
		   structure PrettyPrint = Tools.PrettyPrint
		  )

      structure PostElabTopdecGrammar =
	TopdecGrammar(structure DecGrammar = PostElabDecGrammar
		      structure SigId = Basics.SigId
		      structure FunId = Basics.FunId
		      structure PrettyPrint = Tools.PrettyPrint)

      structure RepositoryFinMap = 
	struct
	  type absprjid = Basics.ModuleEnvironments.absprjid
	  type funid = Basics.FunId.funid

	  fun b_lt (false, true) = true
	    | b_lt _ = false
	  fun bs_lt(_,[]) = false
	    | bs_lt([],_) = true
	    | bs_lt(b::bs,c::cs) = b_lt(b,c) orelse (b = c andalso bs_lt(bs,cs))

	  structure M = OrderFinMap(structure Report = Basics.Tools.Report
				    structure PP = Tools.PrettyPrint
				    structure Order = struct
							type T = absprjid * funid * bool list
							fun lt (a,f,bs) (a',f',bs') = Basics.ModuleEnvironments.lt_absprjid(a,a')
							  orelse (a = a' andalso (Basics.FunId.< (f,f') orelse
										  f = f' andalso bs_lt (bs,bs')))
						      end)

	  val prof_p : unit->bool = Basics.Tools.Flags.is_on0 "region_profiling"
	  val gc_p : unit->bool = Basics.Tools.Flags.is_on0 "garbage_collection"
	    
	  fun Tr (a,f) = (a,f,[prof_p(),gc_p()])
	  fun die s = Basics.Tools.Crash.impossible ("Elaboration.RepositoryFinMap." ^ s)

	  open M
	  type dom = absprjid * funid
	  fun singleton (d,b) = M.singleton(Tr d,b)
	  fun lookup m d = M.lookup m (Tr d)
	  fun add (d, b, m) = M.add (Tr d, b, m)
	  fun remove (d,m) = M.remove(Tr d,m)
	  fun dom m = die "dom"
	  fun list _ = die "list"
	  fun fromList _ = die "fromList"
	  fun ComposeMap _ = die "ComposeMap"
	  fun Fold _ = die "Fold"
	  fun filter _ = die "filter"
	  fun addList _ = die "addList"
	  fun filter _ = die "filter"
	  fun restrict _ = die "restrict"
	  fun layoutMap _ = die "layoutMap"
	  fun reportMap _ = die "reportMap"
	end

      structure ElabRepository = ElabRepository(structure Name = Basics.Name
						structure InfixBasis = TopdecParsing.InfixBasis
						structure TyName = Basics.TyName
						structure OpacityEnv = Basics.OpacityEnv
						structure Flags = Tools.Flags
						type funid = Basics.FunId.funid
						type ElabBasis = Basics.ModuleEnvironments.Basis
						type longstrid = Basics.StrId.longstrid
                                                type absprjid = Basics.ModuleEnvironments.absprjid
						val strip_install_dir' = Basics.ModuleEnvironments.strip_install_dir'
						structure Crash =  Tools.Crash
						structure RepositoryFinMap = RepositoryFinMap)

      structure ElabTopdec =
	ElabTopdec(structure PrettyPrint = Tools.PrettyPrint
		   structure IG = TopdecParsing.PreElabTopdecGrammar
		   structure OG = PostElabTopdecGrammar
		   structure Environments = Basics.Environments
		   structure ModuleEnvironments = Basics.ModuleEnvironments
		   structure StatObject = Basics.StatObject
		   structure ModuleStatObject = Basics.ModuleStatObject
		   structure Name = Basics.Name
		   structure ElabRep = ElabRepository
		   structure ElabDec =
		     ElabDec (structure ParseInfo = AllInfo.ParseInfo
			      structure ElabInfo = AllInfo.ElabInfo
			      structure IG = TopdecParsing.PreElabDecGrammar
			      structure OG = PostElabDecGrammar
			      structure Environments = Basics.Environments
			      structure Ident = Basics.Ident
			      structure Lab = Basics.Lab
			      structure StatObject = Basics.StatObject
			      structure FinMap = Tools.FinMap
			      structure Report = Tools.Report
			      structure PP = Tools.PrettyPrint
			      structure Flags = Tools.Flags
			      structure Crash = Tools.Crash)

		   structure StrId = Basics.StrId
		   structure SigId = Basics.SigId
		   structure ParseInfo = AllInfo.ParseInfo
		   structure ElabInfo = AllInfo.ElabInfo
		   structure BasicIO = Tools.BasicIO
		   structure Report = Tools.Report
		   structure Ident = Basics.Ident
		   structure PP = Tools.PrettyPrint
		   structure FinMap = Tools.FinMap
		   structure Flags = Tools.Flags
		   structure Crash = Tools.Crash)
    end
  end;
