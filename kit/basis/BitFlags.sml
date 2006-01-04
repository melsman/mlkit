
functor BitFlags(F : sig
                       val all : SysWord.word
                     end) =
  struct
    type flags = SysWord.word
    fun toWord x = x
    fun fromWord x = SysWord.andb(x, F.all)
    fun flags x = List.foldl SysWord.orb 0wx0 x
    fun intersect x = List.foldl SysWord.andb F.all x
    fun clear (f1,f2) = SysWord.andb (SysWord.notb f1,f2)
    fun allSet (f1,f2) = SysWord.andb (f1,f2) = f1
    fun anySet (f1,f2) = SysWord.andb (f1,f2) <> 0wx0
  end
