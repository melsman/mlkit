signature PERMUTELIST =
sig

type 'a permuteList

val fromList : 'a list -> 'a permuteList
val prj : 'a permuteList -> ('a * 'a permuteList) option
val findSome : ('a -> 'b option) -> 'a permuteList -> 'b option

end
