(* Dummy structure necessary for compiling the MLKit frontend for SMLtoJs online
   - none of the functions will be invoked at runtime by SMLtoJsOnline
 *)

structure Word64 :> WORD = struct

type word = unit
val wordSize = 64

fun unimpl s = raise Fail ("unimplemented: Word64." ^ s)

fun toInt _ = unimpl "toInt"
fun toIntX _ = unimpl "toIntX"
fun fromInt _ = unimpl "fromInt"
fun toLargeWord _ = unimpl "toLargeWord"
val toLarge = toLargeWord
fun toLargeWordX _ = unimpl "toLargeWordX"
val toLargeX = toLargeWordX
fun fromLargeWord _ = unimpl "fromLargeWord"
val fromLarge = fromLargeWord
fun toLargeInt _ = unimpl "toLargeInt"
fun toLargeIntX _ = unimpl "toLargeIntX"
fun fromLargeInt _ = unimpl "fromLargeInt"

fun orb _ = unimpl "orb"
fun andb _ = unimpl "andb"
fun xorb _ = unimpl "xorb"
fun notb _ = unimpl "notb"

fun op << _ = unimpl "<<"
fun op >> _ = unimpl ">>"
fun op ~>> _ = unimpl "~>>"

fun op + _ = unimpl "+"
fun op - _ = unimpl "-"
fun op * _ = unimpl "*"
fun op div _ = unimpl "div"
fun op mod _ = unimpl "mod"

fun ~ _ = unimpl "~"

fun scan _ _ _ = unimpl "scan"

fun fmt _ _ = unimpl "fmt"
fun toString _ = unimpl "toString"
fun fromString _ = unimpl "fromString"

fun min _ = unimpl "min"
fun max _ = unimpl "max"
fun compare _ = unimpl "compare"

fun op > _ = unimpl ">"
fun op >= _ = unimpl ">="
fun op < _ = unimpl "<"
fun op <= _ = unimpl "<="

end
