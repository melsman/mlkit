(* Parts of the Edinburgh Library used by the Kit *)

structure Edlib =
 struct
   structure General = EdlibGeneral
(*   structure Bool = Bool *)
(*   structure Int = Int *)
(*   structure String = String *)
   structure StringType = StringType
   structure List = List
   structure ListSort = ListSort
   structure ListPair = ListPair
(*   structure Pair = Pair *)
   structure Set = Set
(*   structure EqSet = EqSet *)
   structure BoolParse = BoolParse
   structure IntParse = IntParse
   structure StringParse = StringParse
   structure ListParse = ListParse
   structure PairParse = PairParse
 end