
(* Entire Kit Compiler system, linked together using functors only. The
   top-level modules are functors without arguments. *)

(*$TOOLS: BASIC_IO FINMAP FINMAPEQ SORTED_FINMAP PRETTYPRINT REPORT
	TIMESTAMP FLAGS LIST_HACKS LIST_SORT CRASH TIMING*)

signature TOOLS =
  sig
    structure BasicIO: BASIC_IO
    structure FinMap: FINMAP
    structure FinMapEq : FINMAPEQ
    structure SortedFinMap: SORTED_FINMAP

    structure PrettyPrint: PRETTYPRINT
      sharing type FinMap.StringTree
	           = FinMapEq.StringTree
		   = SortedFinMap.StringTree
		   = PrettyPrint.StringTree
            
    structure Report: REPORT
      sharing type FinMap.Report
		   = FinMapEq.Report
		   = SortedFinMap.Report
		   = PrettyPrint.Report
		   = Report.Report

    structure Timestamp: TIMESTAMP
    structure Flags: FLAGS
    sharing type Flags.Report = Report.Report
    structure ListHacks: LIST_HACKS
    structure ListSort: LIST_SORT
    structure Crash: CRASH
    structure Timing: TIMING
  end;

(*$Tools: BasicIO Crash Flags PrettyPrint FinMapEq FinMap SortedFinMap
	ListHacks ListSort Report Timestamp Timing ORDSET TOOLS *)

functor Tools(): TOOLS =
  struct
    structure BasicIO = BasicIO()
    structure Crash = Crash(structure BasicIO = BasicIO)
    structure Report = Report(structure BasicIO = BasicIO)
    structure Flags = Flags(structure Crash = Crash
			    structure Report = Report)
    structure Timestamp = Timestamp()

    structure PrettyPrint = PrettyPrint(structure Report = Report
					structure Crash = Crash
                                        structure Flags = Flags
				       )

    structure FinMap = FinMap(structure Report = Report
			      structure PP = PrettyPrint
			     )

    structure FinMapEq = FinMapEq(structure Report = Report
				  structure PP = PrettyPrint
				    )

    structure SortedFinMap = SortedFinMap(structure Report = Report
					  structure PP = PrettyPrint
					 )
    structure ListHacks = ListHacks()

    structure ListSort = ListSort()

    structure Timing = Timing(structure Flags = Flags
			      structure Crash = Crash)
  end;

(*$ALL_INFO : DF_INFO SOURCE_INFO PARSE_INFO ERROR_INFO
     TYPE_INFO OVERLOADING_INFO ELAB_INFO*)

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

(*$BASICS: TOOLS SCON LAB NAME TYNAME TYCON STATOBJECT IDENT
        INFIX_BASIS STRID SIGID FUNID DEC_GRAMMAR TYVAR LEX_BASICS
        MODULE_STATOBJECT MODULE_ENVIRONMENTS ENVIRONMENTS ALL_INFO *)

signature BASICS =
  sig
    structure Tools : TOOLS 
    structure StrId : STRID

    structure Ident : IDENT
      sharing type Ident.strid = StrId.strid

    structure InfixBasis : INFIX_BASIS
      sharing type InfixBasis.id = Ident.id
	  and type InfixBasis.Report = Tools.Report.Report
	  and type InfixBasis.StringTree = Tools.PrettyPrint.StringTree

    structure SCon : SCON

    structure Lab : LAB
    structure TyVar : TYVAR

    structure TyCon : TYCON
      sharing type TyCon.strid = StrId.strid

    structure Name : NAME

    structure TyName : TYNAME
      sharing type TyName.tycon = TyCon.tycon
	  and type TyName.name = Name.name

    structure StatObject : STATOBJECT
      sharing type StatObject.ExplicitTyVar = TyVar.SyntaxTyVar
	  and type StatObject.StringTree  = Tools.PrettyPrint.StringTree
          and type StatObject.strid     = StrId.strid
	  and type StatObject.scon      = SCon.scon
	  and type StatObject.lab       = Lab.lab
	  sharing StatObject.TyName    = TyName

    structure SigId : SIGID
    structure FunId : FUNID

    structure LexBasics: LEX_BASICS
      sharing type LexBasics.Report = Tools.Report.Report
      sharing type LexBasics.StringTree = Tools.PrettyPrint.StringTree

    structure PreElabDecGrammar: DEC_GRAMMAR
      sharing PreElabDecGrammar.StrId = StrId
      sharing type PreElabDecGrammar.longid         = Ident.longid
	  and type PreElabDecGrammar.longstrid      = StrId.longstrid
	  and type PreElabDecGrammar.longtycon      = TyCon.longtycon 
	  and type PreElabDecGrammar.tycon          = TyCon.tycon
	  and type PreElabDecGrammar.tyvar          = TyVar.SyntaxTyVar
	  and type PreElabDecGrammar.id             = Ident.id
	  and type PreElabDecGrammar.lab            = Lab.lab
	  and type PreElabDecGrammar.scon           = SCon.scon 
	  and type PreElabDecGrammar.StringTree     = Tools.PrettyPrint.StringTree

      sharing PreElabDecGrammar.Ident = Ident
      and PreElabDecGrammar.StrId = StrId
	  and PreElabDecGrammar.TyCon = TyCon
	  and PreElabDecGrammar.TyVar = TyVar
	  and PreElabDecGrammar.Lab = Lab
	  and PreElabDecGrammar.SCon = SCon

    structure Environments : ENVIRONMENTS
      sharing type Environments.Type         = StatObject.Type
          and type Environments.TyVar        = StatObject.TyVar
	  and type Environments.TypeScheme   = StatObject.TypeScheme
	  and type Environments.TypeFcn      = StatObject.TypeFcn
	  and type Environments.realisation  = StatObject.realisation
	  and type Environments.level        = StatObject.level
	  and type Environments.id           = Ident.id
	  and type Environments.longid       = Ident.longid
	  and type Environments.Substitution = StatObject.Substitution
	  and type Environments.ty           = PreElabDecGrammar.ty
	  and type Environments.tycon        = TyCon.tycon
	  and type Environments.longtycon    = TyCon.longtycon
	  and type Environments.longstrid    = StrId.longstrid
	  and type Environments.ExplicitTyVar  = TyVar.SyntaxTyVar
	  and type Environments.StringTree   = Tools.PrettyPrint.StringTree
	  and type Environments.strid       = StrId.strid
	  and type Environments.valbind = PreElabDecGrammar.valbind
	  and type Environments.pat = PreElabDecGrammar.pat
          sharing Environments.TyName       = StatObject.TyName

    structure ModuleStatObject : MODULE_STATOBJECT
      sharing type ModuleStatObject.Env = Environments.Env
	  and type ModuleStatObject.realisation = StatObject.realisation
	  and type ModuleStatObject.StringTree = Tools.PrettyPrint.StringTree
	  and type ModuleStatObject.strid = StrId.strid
	  and type ModuleStatObject.longstrid = StrId.longstrid
	  and type ModuleStatObject.longtycon = TyCon.longtycon
	  and type ModuleStatObject.Type = StatObject.Type
	  and type ModuleStatObject.TypeScheme = StatObject.TypeScheme
	  and type ModuleStatObject.TypeFcn = StatObject.TypeFcn
	  and type ModuleStatObject.TyVar = StatObject.TyVar
	  and type ModuleStatObject.id = Ident.id
	  sharing ModuleStatObject.TyName = TyName

    structure ModuleEnvironments : MODULE_ENVIRONMENTS
      sharing type ModuleEnvironments.realisation = StatObject.realisation
          and type ModuleEnvironments.longstrid = StrId.longstrid
          and type ModuleEnvironments.longtycon = TyCon.longtycon
	  and type ModuleEnvironments.Context = Environments.Context
	  and type ModuleEnvironments.FunSig = ModuleStatObject.FunSig
	  and type ModuleEnvironments.TyStr = Environments.TyStr
	  and type ModuleEnvironments.TyVar = StatObject.TyVar
	  and type ModuleEnvironments.id = Ident.id
	  and type ModuleEnvironments.strid = StrId.strid
	  and type ModuleEnvironments.sigid = SigId.sigid
	  and type ModuleEnvironments.funid = FunId.funid
	  and type ModuleEnvironments.tycon = TyCon.tycon
	  and type ModuleEnvironments.Env = Environments.Env
	  and type ModuleEnvironments.Sig = ModuleStatObject.Sig
	  and type ModuleEnvironments.StringTree = Tools.PrettyPrint.StringTree
	  sharing ModuleEnvironments.TyName = TyName

    structure AllInfo : ALL_INFO
      sharing type AllInfo.TypeInfo.Type = StatObject.Type
	  and type AllInfo.TypeInfo.TyVar = StatObject.TyVar
	  and type AllInfo.TypeInfo.TyEnv = Environments.TyEnv
	  and type AllInfo.TypeInfo.longid = Ident.longid
	  and type AllInfo.TypeInfo.realisation = StatObject.realisation
	  and type AllInfo.TypeInfo.Env = Environments.Env
	  and type AllInfo.TypeInfo.strid = StrId.strid
	  and type AllInfo.TypeInfo.tycon = TyCon.tycon
	  and type AllInfo.TypeInfo.id = Ident.id
	  and type AllInfo.TypeInfo.TyName = StatObject.TyName
	  and type AllInfo.TypeInfo.Basis = ModuleEnvironments.Basis
	  and type AllInfo.ErrorInfo.Type = StatObject.Type
	  and type AllInfo.ErrorInfo.TypeScheme = StatObject.TypeScheme
	  and type AllInfo.ErrorInfo.TyVar = StatObject.TyVar
	  and type AllInfo.ErrorInfo.TyName = TyName.TyName
	  and type AllInfo.ErrorInfo.TypeFcn = StatObject.TypeFcn
	  and type AllInfo.ErrorInfo.lab = Lab.lab
	  and type AllInfo.ErrorInfo.tycon = TyCon.tycon
	  and type AllInfo.ErrorInfo.longid = Ident.longid
	  and type AllInfo.ErrorInfo.longtycon = TyCon.longtycon
	  and type AllInfo.ErrorInfo.strid = StrId.strid
	  and type AllInfo.ErrorInfo.longstrid = StrId.longstrid
	  and type AllInfo.ErrorInfo.sigid = SigId.sigid
	  and type AllInfo.ErrorInfo.funid = FunId.funid
	  and type AllInfo.ErrorInfo.id = Ident.id
	  and type AllInfo.ErrorInfo.SigMatchError = ModuleStatObject.SigMatchError
	  and type AllInfo.SourceInfo.pos = LexBasics.pos
	  and type AllInfo.SourceInfo.Report = Tools.Report.Report
	  and type AllInfo.ElabInfo.StringTree = Tools.PrettyPrint.StringTree
          and type AllInfo.OverloadingInfo.RecType = StatObject.RecType
          and type AllInfo.OverloadingInfo.TyVar = StatObject.TyVar
	  and type AllInfo.OverloadingInfo.StringTree = Tools.PrettyPrint.StringTree
	  and type AllInfo.ElabInfo.ParseInfo = PreElabDecGrammar.info
	  and type AllInfo.ElabInfo.ParseInfo.DFInfo.InfixBasis = InfixBasis.Basis
  end;

(*$Basics: TOOLS Ident InfixBasis StrId SigId FunId TyVar SCon Lab
	Name TyName TyCon StatObject STATOBJECT LexBasics DFInfo
	Environments ModuleStatObject ModuleEnvironments DecGrammar
	ParseInfo SourceInfo ErrorInfo TypeInfo ElabInfo
	OverloadingInfo BASICS *)

functor Basics(structure Tools: TOOLS): BASICS =
  struct
    structure Tools = Tools

    structure StrId = StrId(structure Timestamp = Tools.Timestamp
			    structure Crash = Tools.Crash
			   )

    structure Ident = Ident(structure StrId = StrId
			    structure Crash = Tools.Crash
			   )

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
			    structure Crash = Tools.Crash
			   )

    structure Name = Name ()

    structure TyName = TyName(structure TyCon = TyCon
			      structure Name = Name
			      structure Flags = Tools.Flags
			      structure PrettyPrint = Tools.PrettyPrint
			      structure Report = Tools.Report)

      structure StatObject : STATOBJECT = 
	StatObject(structure SortedFinMap  = Tools.SortedFinMap
		   structure Ident = Ident
		   structure Lab = Lab
		   structure SCon = SCon
		   structure TyName = TyName
		   structure TyCon = TyCon
		   structure ExplicitTyVar = TyVar
		   structure Timestamp = Tools.Timestamp
		   structure ListHacks = Tools.ListHacks
		   structure Flags = Tools.Flags
		   structure Report = Tools.Report
		   structure FinMap = Tools.FinMap
		   structure FinMapEq = Tools.FinMapEq
		   structure PP = Tools.PrettyPrint
		   structure Crash = Tools.Crash
		  )

   (* LexBasics is needed by SourceInfo, as well as all the parsing
      stuff. *)

    structure LexBasics = LexBasics(structure BasicIO = Tools.BasicIO
				    structure Report = Tools.Report
				    structure PP = Tools.PrettyPrint
				    structure Flags = Tools.Flags
				    structure Crash = Tools.Crash
				   )

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
       structure ListHacks = Tools.ListHacks
       structure ListSort = Tools.ListSort
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
		       structure Crash        = Tools.Crash
		      )


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
			 structure ListHacks	     = Tools.ListHacks
			 structure Crash             = Tools.Crash
			)

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
	  (structure Ident = Ident
	   structure ModuleEnvironments = ModuleEnvironments
	   structure StrId = StrId
	   structure TyCon = TyCon
	   structure StatObject=StatObject
	   structure Environments=Environments
	   structure PP = Tools.PrettyPrint)
	structure OverloadingInfo = OverloadingInfo
	  (structure StatObject = StatObject
	   structure PrettyPrint = Tools.PrettyPrint)
	structure ElabInfo = ElabInfo
	  (structure ParseInfo = ParseInfo
	   structure ErrorInfo = ErrorInfo
	   structure TypeInfo = TypeInfo
	   structure OverloadingInfo = OverloadingInfo
	   structure PrettyPrint = Tools.PrettyPrint
	   structure Crash = Tools.Crash)
      end
  end;

(*$TOPDEC_PARSING: BASICS DEC_GRAMMAR TOPDEC_GRAMMAR 
	 INFIX_BASIS PARSE *)

signature TOPDEC_PARSING =
  sig
    structure Basics: BASICS

    structure PreElabDecGrammar: DEC_GRAMMAR
      sharing PreElabDecGrammar = Basics.PreElabDecGrammar
      sharing type PreElabDecGrammar.longid = Basics.Ident.longid

    structure PreElabTopdecGrammar: TOPDEC_GRAMMAR
      sharing PreElabTopdecGrammar.DecGrammar = PreElabDecGrammar
      sharing type PreElabTopdecGrammar.dec = PreElabDecGrammar.dec
	  and type PreElabTopdecGrammar.tycon = PreElabDecGrammar.tycon
		   = Basics.TyCon.tycon
	  and type PreElabTopdecGrammar.tyvar = PreElabDecGrammar.tyvar
		   = Basics.TyVar.SyntaxTyVar
	  and type PreElabDecGrammar.scon = Basics.SCon.scon
	  and type PreElabDecGrammar.lab = Basics.Lab.lab
	  and type PreElabTopdecGrammar.id = PreElabDecGrammar.id
	    	   = Basics.Ident.id

	  and type PreElabTopdecGrammar.ty = PreElabDecGrammar.ty
	  and type PreElabTopdecGrammar.info = PreElabDecGrammar.info
		   = Basics.AllInfo.ElabInfo.ParseInfo
	  and type PreElabTopdecGrammar.sigid = Basics.SigId.sigid
	  and type PreElabTopdecGrammar.strid = Basics.StrId.strid
	  and type PreElabTopdecGrammar.funid = Basics.FunId.funid
	  and type PreElabTopdecGrammar.longstrid = PreElabDecGrammar.longstrid
	    	   = Basics.StrId.longstrid
	  and type PreElabTopdecGrammar.longtycon = PreElabDecGrammar.longtycon
	    	   = Basics.TyCon.longtycon
          and type PreElabDecGrammar.StringTree
	         = Basics.Tools.PrettyPrint.StringTree
          and type PreElabTopdecGrammar.StringTree
	           = Basics.Tools.PrettyPrint.StringTree

      sharing PreElabTopdecGrammar.DecGrammar = PreElabDecGrammar
	  and PreElabTopdecGrammar.DecGrammar.TyCon = Basics.TyCon
	  and PreElabTopdecGrammar.DecGrammar.TyVar = Basics.TyVar
	  and PreElabDecGrammar.SCon = Basics.SCon
	  and PreElabDecGrammar.Lab = Basics.Lab
	  and PreElabTopdecGrammar.DecGrammar.Ident = Basics.Ident
	  and PreElabTopdecGrammar.SigId = Basics.SigId
	  and PreElabTopdecGrammar.StrId = Basics.StrId
	  and PreElabTopdecGrammar.FunId = Basics.FunId
       sharing type PreElabTopdecGrammar.ty = Basics.Environments.ty
    
    structure InfixBasis: INFIX_BASIS
      sharing InfixBasis = Basics.InfixBasis

    structure Parse: PARSE
      sharing type Parse.topdec = PreElabTopdecGrammar.topdec
	  and type Parse.InfixBasis = InfixBasis.Basis
  end;

(*$TopdecParsing: BASICS DecGrammar TopdecGrammar Parse TOPDEC_PARSING
                  TOPDEC_GRAMMAR *)

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


(*$ELABORATION: BASICS ELABTOPDEC DEC_GRAMMAR TOPDEC_GRAMMAR
	 ELAB_REPOSITORY*)

signature ELABORATION =
  sig
    structure Basics : BASICS

    structure ElabRepository : ELAB_REPOSITORY
      sharing type ElabRepository.funid = Basics.FunId.funid
	  and type ElabRepository.name = Basics.Name.name
	  and type ElabRepository.ElabBasis = Basics.ModuleEnvironments.Basis 
	    
    structure ElabTopdec : ELABTOPDEC
      sharing type ElabTopdec.StaticBasis = ElabRepository.ElabBasis

    structure PostElabDecGrammar : DEC_GRAMMAR
      sharing type PostElabDecGrammar.lab = Basics.Lab.lab
	  and type PostElabDecGrammar.scon = Basics.SCon.scon
	  and type PostElabDecGrammar.tycon = Basics.TyCon.tycon
	  and type PostElabDecGrammar.longtycon = Basics.TyCon.longtycon
	  and type PostElabDecGrammar.tyvar = Basics.TyVar.SyntaxTyVar
 	  and type PostElabDecGrammar.id = Basics.Ident.id
 	  and type PostElabDecGrammar.longid = Basics.Ident.longid
	  and type PostElabDecGrammar.longstrid = Basics.StrId.longstrid
	  and type PostElabDecGrammar.info
		   = Basics.AllInfo.ElabInfo.ElabInfo
	  and type PostElabDecGrammar.StringTree
	           = Basics.Tools.PrettyPrint.StringTree

    structure PostElabTopdecGrammar : TOPDEC_GRAMMAR
      sharing PostElabTopdecGrammar.DecGrammar = PostElabDecGrammar
      sharing type PostElabTopdecGrammar.id = Basics.Ident.id
	  and type PostElabTopdecGrammar.strid = Basics.StrId.strid
	  and type PostElabTopdecGrammar.longstrid = Basics.StrId.longstrid
	  and type PostElabTopdecGrammar.longtycon = Basics.TyCon.longtycon
	  and type PostElabTopdecGrammar.tycon = Basics.TyCon.tycon
	  and type PostElabTopdecGrammar.sigid = Basics.SigId.sigid
	  and type PostElabTopdecGrammar.funid = Basics.FunId.funid
	  and type PostElabTopdecGrammar.dec = PostElabDecGrammar.dec
	  and type PostElabTopdecGrammar.ty = PostElabDecGrammar.ty
	  and type PostElabTopdecGrammar.info
	    	   = Basics.AllInfo.ElabInfo.ElabInfo
	  and type ElabTopdec.PostElabTopdec = PostElabTopdecGrammar.topdec
	  and type PostElabTopdecGrammar.StringTree
	           = Basics.Tools.PrettyPrint.StringTree
  end;

(*$Elaboration: TOPDEC_PARSING DecGrammar TopdecGrammar
	 ElabRepository ElabTopdec ElabDec ELABORATION *)

functor Elaboration(structure TopdecParsing : TOPDEC_PARSING): ELABORATION =
  struct
    structure Basics     = TopdecParsing.Basics

    local
      structure Tools      = Basics.Tools
      structure AllInfo    = Basics.AllInfo
      structure ElabInfo   = AllInfo.ElabInfo
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

      structure ElabRepository = ElabRepository(structure Name = Basics.Name
						structure InfixBasis = TopdecParsing.InfixBasis
						structure TyName = Basics.TyName
						type funid = Basics.FunId.funid
						type ElabBasis = Basics.ModuleEnvironments.Basis
						type realisation = Basics.Environments.realisation
						structure Crash =  Tools.Crash
						structure FinMap = Tools.FinMap)

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
			      structure ListHacks = Tools.ListHacks
			      structure ListSort = Tools.ListSort
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


(*$EXECUTION: ELABORATION COMPILER_ENV COMPILE_BASIS COMPILE FREE_IDS *)
signature EXECUTION =
  sig
    structure Elaboration: ELABORATION
    structure CompilerEnv: COMPILER_ENV
    structure CompileBasis: COMPILE_BASIS
    structure FreeIds : FREE_IDS
      sharing type FreeIds.topdec = Elaboration.ElabTopdec.PostElabTopdec
    structure Compile: COMPILE
      sharing type Compile.CompileBasis = CompileBasis.CompileBasis
	  and type Compile.CEnv = CompilerEnv.CEnv
  end;


(*$Execution: ELABORATION BuildCompile EXECUTION Name FreeIds*)
functor Execution(structure Elaboration : ELABORATION) : EXECUTION =
  struct
    structure Elaboration = Elaboration
    structure Basics      = Elaboration.Basics
    structure Tools       = Basics.Tools
    structure AllInfo     = Basics.AllInfo

    structure FreeIds = FreeIds
      (structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
       structure ElabInfo = AllInfo.ElabInfo
       structure Crash = Tools.Crash
       structure PP = Tools.PrettyPrint)
	 
    structure BuildCompile = BuildCompile
      (structure TyName = Basics.TyName
       structure Name = Basics.Name
       structure FreeIds = FreeIds
       structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
       structure ElabInfo = AllInfo.ElabInfo
       structure StatObject = Basics.StatObject
       structure Environments = Elaboration.Basics.Environments
       structure FinMap = Tools.FinMap
       structure FinMapEq = Tools.FinMapEq
       structure BasicIO = Tools.BasicIO
       structure Report = Tools.Report
       structure Flags = Tools.Flags
       structure PP = Tools.PrettyPrint
       structure Crash = Tools.Crash
       structure Timing = Tools.Timing)

    open BuildCompile
  end;


(* KitCompiler does the functor application to build up the
   hierarchy of structures, and builds the other stuff not directly
   relevant to the build heirarchy. It provides a convenient
   top-level interface. *)

(*$KitCompiler: Tools Basics TopdecParsing Elaboration Execution
        ManagerObjects OpacityElim ParseElab ErrorTraverse
        TopLevelReport TestInfo Manager IntModules MANAGER FLAGS
        TestEnv*)

functor KitCompiler() : sig include MANAGER 
                            val test : unit -> unit
			    structure Flags : FLAGS
			end =
  struct
    structure Tools   = Tools()
    structure Basics  = Basics(structure Tools = Tools)
    structure AllInfo = Basics.AllInfo

    structure TopdecParsing  = TopdecParsing(structure Basics = Basics)      

    structure Elaboration = Elaboration(structure TopdecParsing = TopdecParsing)

    structure Execution = Execution(structure Elaboration = Elaboration)

    structure OpacityElim = OpacityElim(structure Crash = Tools.Crash
					structure ElabInfo = AllInfo.ElabInfo
					structure Environments = Basics.Environments
					structure StatObject = Basics.StatObject
					structure TopdecGrammar = Elaboration.PostElabTopdecGrammar)

    structure Flags = Tools.Flags

    structure ManagerObjects =
      ManagerObjects(structure ModuleEnvironments = Basics.ModuleEnvironments
		     structure OpacityElim = OpacityElim
		     structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
		     structure CompilerEnv = Execution.CompilerEnv
		     structure ElabRep = Elaboration.ElabRepository
		     structure CompileBasis = Execution.CompileBasis
		     structure Compile = Execution.Compile
		     structure InfixBasis = TopdecParsing.InfixBasis
		     structure FinMap = Tools.FinMap
		     structure PP = Tools.PrettyPrint
		     structure Name = Basics.Name
		     structure Flags = Flags
		     structure Crash = Tools.Crash)
      
    structure ParseElab = ParseElab
      (structure Parse = TopdecParsing.Parse
       structure ElabTopdec = Elaboration.ElabTopdec
       structure ModuleEnvironments = Basics.ModuleEnvironments
       structure PreElabTopdecGrammar = TopdecParsing.PreElabTopdecGrammar
       structure PostElabTopdecGrammar = Elaboration.PostElabTopdecGrammar
       structure ErrorTraverse = ErrorTraverse
	 (structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
	  structure ElabInfo = AllInfo.ElabInfo
	  structure Report = Tools.Report
	  structure PrettyPrint = Tools.PrettyPrint
	  structure Crash = Tools.Crash)
       structure InfixBasis = TopdecParsing.InfixBasis
       structure TopLevelReport =
		      TopLevelReport(structure FunId = Basics.FunId
				     structure SigId = Basics.SigId
				     structure StrId = Basics.StrId
				     structure Ident = Basics.Ident
				     structure InfixBasis = TopdecParsing.InfixBasis
				     structure StatObject = Basics.StatObject
				     structure Environments = Basics.Environments
				     structure ModuleStatObject = Basics.ModuleStatObject
				     structure ModuleEnvironments = Basics.ModuleEnvironments
				     structure Report = Tools.Report
				     structure Crash = Tools.Crash)
       structure BasicIO = Tools.BasicIO
       structure Report = Tools.Report
       structure PP = Tools.PrettyPrint
       structure Flags = Tools.Flags
       structure Crash = Tools.Crash)

    structure IntModules = 
      IntModules(structure Name = Basics.Name
		 structure LexBasics = Basics.LexBasics
		 structure ModuleEnvironments = Basics.ModuleEnvironments
		 structure ParseElab = ParseElab
		 structure OpacityElim = OpacityElim
		 structure ManagerObjects = ManagerObjects
		 structure CompilerEnv = Execution.CompilerEnv
		 structure ElabInfo = AllInfo.ElabInfo
		 structure Environments = Basics.Environments
		 structure CompileBasis = Execution.CompileBasis
		 structure FreeIds = Execution.FreeIds
		 structure Compile = Execution.Compile
		 structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
		 structure Crash = Tools.Crash
		 structure Report = Tools.Report
		 structure Flags = Tools.Flags)

    structure Manager =
      Manager(structure ManagerObjects = ManagerObjects
	      structure OpacityElim = OpacityElim
	      structure Name = Basics.Name
	      structure Environments = Basics.Environments
	      structure ModuleEnvironments = Basics.ModuleEnvironments
	      structure ParseElab = ParseElab
	      structure IntModules = IntModules
	      structure FreeIds = Execution.FreeIds
	      structure Timing = Tools.Timing
	      structure Crash = Tools.Crash
	      structure Report = Tools.Report
	      structure PP = Tools.PrettyPrint
	      structure Flags = Tools.Flags)

      structure TestEnv = TestEnv(structure TestInfo = TestInfo (structure Flags = Tools.Flags)
				  structure Flags = Tools.Flags
				  structure Manager = Manager
				  structure Basics = Basics
				  structure Timing = Tools.Timing)

      val test = TestEnv.test
      open Manager
  end;

(*$K: KitCompiler*)
structure K = KitCompiler()
open K
fun i a = (System.cd "/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/src/" ;
	   Flags.lookup_string_entry "path_to_kit_script"
	   := "../bin/ML_to_HPPA_on_HPUX/kit.script" ;
	   (*this path means that you must be in the src/ directory,
	    which is where you will be, if you want Make.again{} etc.
	    to work.  20/06/1997 13:32. tho.*)
	   print ("Script file is " ^ Flags.get_string_entry "path_to_kit_script" ^ "\n\
	    \Reading script file.\n") ;
	   Flags.read_script () ;
	   Flags.interact ()) ;
fun ib a = (System.cd "/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/src/" ;
	   Flags.lookup_string_entry "path_to_kit_script"
	   := "../bin/basislib.script" ;
	   (*this path means that you must be in the src/ directory,
	    which is where you will be, if you want Make.again{} etc.
	    to work.  20/06/1997 13:32. tho.*)
	   print ("Script file is " ^ Flags.get_string_entry "path_to_kit_script" ^ "\n\
	    \Reading script file.\n") ;
	   Flags.read_script () ;
	   Flags.interact ()) ;
