(* $Id:$ *)
(* Author: Carsten Varming 2006 *)

structure UlFile =
  struct
    type scripts = string
    type location = string
    type uofile = string
    datatype UlSyntax = UlFile of (scripts * location) list
                      | UoFile of uofile list
                      | Script of (uofile * location) list
    fun map ulf uof scf (UlFile l) = UlFile (List.map ulf l)
      | map ulf uof scf (UoFile l) = UoFile (List.map uof l)
      | map ulf uof scf (Script l) = Script (List.map scf l)

    fun mapPartial ulf uof scf (UlFile l) = let val a = List.mapPartial ulf l in if a = [] then NONE else SOME (UlFile a) end
      | mapPartial ulf uof scf (UoFile l) = let val a = List.mapPartial uof l in if a = [] then NONE else SOME (UoFile a) end
      | mapPartial ulf uof scf (Script l) = let val a = List.mapPartial scf l in if a = [] then NONE else SOME (Script a) end
  end
