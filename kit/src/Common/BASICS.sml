signature BASICS =
  sig
    structure PreElabDecGrammar: DEC_GRAMMAR
      sharing type PreElabDecGrammar.StringTree = PrettyPrint.StringTree
      sharing PreElabDecGrammar.Ident = Ident
      sharing PreElabDecGrammar.StrId = StrId
      sharing PreElabDecGrammar.TyCon = TyCon
      sharing PreElabDecGrammar.TyVar = SyntaxTyVar
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
      sharing type Environments.Report = Report.Report

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
      sharing type ModuleEnvironments.Report = Report.Report

    structure OpacityEnv : OPACITY_ENV
      sharing OpacityEnv.TyName = TyName
      sharing type OpacityEnv.funid = FunId.funid
      sharing type OpacityEnv.StringTree = PrettyPrint.StringTree
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
      sharing type AllInfo.SourceInfo.Report = Report.Report
      sharing type AllInfo.ElabInfo.StringTree = PrettyPrint.StringTree
      sharing type AllInfo.OverloadingInfo.RecType = StatObject.RecType
      sharing type AllInfo.OverloadingInfo.TyVar = StatObject.TyVar
      sharing type AllInfo.OverloadingInfo.StringTree = PrettyPrint.StringTree
      sharing type AllInfo.ElabInfo.ParseInfo = PreElabDecGrammar.info
      sharing type AllInfo.ElabInfo.ParseInfo.DFInfo.InfixBasis = InfixBasis.Basis
  end;
