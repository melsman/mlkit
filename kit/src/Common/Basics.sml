structure Basics: BASICS =
  struct
    structure StrId = StrId

    structure Ident = Ident

    structure InfixBasis = InfixBasis

    structure SigId = SigId
          and FunId = FunId
          and TyVar = SyntaxTyVar
	  and Lab = Lab
    	  and SCon = SCon
    	  and TyCon = TyCon

    structure Name = Name

    structure TyName = TyName

    structure StatObject = StatObject

   (* LexBasics is needed by SourceInfo, as well as all the parsing
      stuff. *)

    structure LexBasics = LexBasics

    structure DFInfo = DFInfo
      
    structure SourceInfo = SourceInfo

    structure ParseInfo = ParseInfo

    structure PreElabDecGrammar = PreElabDecGrammar

    structure Environments = Environments

    structure ModuleStatObject = ModuleStatObject

    structure ModuleEnvironments = ModuleEnvironments

    structure OpacityEnv = OpacityEnv

    structure AllInfo = AllInfo
  end
