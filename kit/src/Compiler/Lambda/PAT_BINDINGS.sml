(*$PAT_BINDINGS*)

signature PAT_BINDINGS =
  sig
    type lab
    type TypeInfo
    type lvar
    type pat
    type (''a, 'b) map
    type CEnv

    datatype BindingTree =
        TUPLEbtree of (lab, TypeInfo * lvar * BindingTree) map
      | CONbtree   of {info: TypeInfo,child: BindingTree, childLvar: lvar}
      | EXCONbtree of {info: TypeInfo,child: BindingTree, childLvar: lvar}
      | NILbtree

    val patBindings: (lvar * pat) -> (BindingTree * CEnv)

    type StringTree
    val layoutPatBindings: BindingTree * CEnv -> StringTree
  end;
