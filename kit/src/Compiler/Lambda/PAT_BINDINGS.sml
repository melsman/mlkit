(*$PAT_BINDINGS*)

signature PAT_BINDINGS =
  sig
    type lab
    type TypeInfo
    type lvar
    type pat
    type (''a, 'b) map
    type CEnv
    type TyVar and SType
    type tyvar and LType

    datatype BindingTree =
        TUPLEbtree of (lab, TypeInfo * lvar * BindingTree) map
      | CONbtree   of {info: TypeInfo,child: BindingTree, childLvar: lvar}
      | EXCONbtree of {info: TypeInfo,child: BindingTree, childLvar: lvar}
      | NILbtree

    val patBindings: (TyVar list * SType -> tyvar list * LType) -> (lvar * pat) -> (BindingTree * CEnv)

    type StringTree
    val layoutPatBindings: BindingTree * CEnv -> StringTree
  end;
