functor ULLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : UL_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* $Id:$ *)
(* Author: Carsten Varming 2006 *)

structure UF = UlFile
;


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\003\000\025\000\000\000\
\\001\000\004\000\020\000\000\000\
\\001\000\005\000\014\000\000\000\
\\001\000\005\000\023\000\000\000\
\\001\000\006\000\013\000\000\000\
\\001\000\006\000\015\000\000\000\
\\001\000\006\000\017\000\000\000\
\\001\000\009\000\018\000\000\000\
\\001\000\010\000\000\000\000\000\
\\028\000\000\000\
\\029\000\007\000\006\000\008\000\005\000\009\000\004\000\000\000\
\\030\000\000\000\
\\031\000\000\000\
\\032\000\000\000\
\\033\000\001\000\012\000\000\000\
\\034\000\000\000\
\\035\000\002\000\010\000\000\000\
\\036\000\000\000\
\\037\000\002\000\008\000\000\000\
\\038\000\000\000\
\"
val actionRowNumbers =
"\010\000\009\000\018\000\016\000\
\\014\000\004\000\002\000\005\000\
\\016\000\006\000\007\000\010\000\
\\001\000\010\000\017\000\010\000\
\\003\000\013\000\018\000\012\000\
\\011\000\000\000\019\000\014\000\
\\015\000\008\000"
val gotoT =
"\
\\001\000\025\000\002\000\001\000\000\000\
\\000\000\
\\005\000\005\000\000\000\
\\004\000\007\000\000\000\
\\003\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\014\000\000\000\
\\000\000\
\\000\000\
\\002\000\017\000\000\000\
\\000\000\
\\002\000\019\000\000\000\
\\000\000\
\\002\000\020\000\000\000\
\\000\000\
\\000\000\
\\005\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\024\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 26
val numrules = 11
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | SML of unit ->  (string) | LOC of unit ->  (string)
 | UOFILE of unit ->  (string) | ULFILE of unit ->  (string)
 | SmlInclude of unit ->  ( ( UF.uofile * UF.location )  list)
 | UoInclude of unit ->  (UF.uofile list)
 | UlInclude of unit ->  ( ( UF.scripts * UF.location )  list)
 | Fil of unit ->  (UF.UlSyntax list)
 | Result of unit ->  (UF.UlSyntax list)
end
type svalue = MlyValue.svalue
type result = UF.UlSyntax list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 9) => true | _ => false
val showTerminal =
fn (T 0) => "ULFILE"
  | (T 1) => "UOFILE"
  | (T 2) => "LOC"
  | (T 3) => "SML"
  | (T 4) => "AS"
  | (T 5) => "END"
  | (T 6) => "ULFILES"
  | (T 7) => "CODEFILES"
  | (T 8) => "SCRIPTS"
  | (T 9) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4)end
structure Actions =
struct 
type int = Int.int
exception mlyAction of int
local open Header in
val actions = 
fn (i392:int,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Fil Fil1, Fil1left, Fil1right)) :: rest671)
) => let val  result = MlyValue.Result (fn _ => let val  (Fil as Fil1)
 = Fil1 ()
 in ( Fil )
end)
 in ( LrTable.NT 0, ( result, Fil1left, Fil1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.Fil (fn _ => ( [] ))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Fil Fil1, _, Fil1right)) :: _ :: ( _, ( 
MlyValue.UlInclude UlInclude1, _, _)) :: ( _, ( _, ULFILES1left, _))
 :: rest671)) => let val  result = MlyValue.Fil (fn _ => let val  (
UlInclude as UlInclude1) = UlInclude1 ()
 val  (Fil as Fil1) = Fil1 ()
 in ( (UF.UlFile UlInclude)::Fil )
end)
 in ( LrTable.NT 1, ( result, ULFILES1left, Fil1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Fil Fil1, _, Fil1right)) :: _ :: ( _, ( 
MlyValue.UoInclude UoInclude1, _, _)) :: ( _, ( _, CODEFILES1left, _))
 :: rest671)) => let val  result = MlyValue.Fil (fn _ => let val  (
UoInclude as UoInclude1) = UoInclude1 ()
 val  (Fil as Fil1) = Fil1 ()
 in ( (UF.UoFile UoInclude)::Fil )
end)
 in ( LrTable.NT 1, ( result, CODEFILES1left, Fil1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Fil Fil1, _, Fil1right)) :: _ :: ( _, ( 
MlyValue.SmlInclude SmlInclude1, _, _)) :: ( _, ( _, SCRIPTS1left, _))
 :: rest671)) => let val  result = MlyValue.Fil (fn _ => let val  (
SmlInclude as SmlInclude1) = SmlInclude1 ()
 val  (Fil as Fil1) = Fil1 ()
 in ( (UF.Script SmlInclude)::Fil )
end)
 in ( LrTable.NT 1, ( result, SCRIPTS1left, Fil1right), rest671)
end
|  ( 5, ( rest671)) => let val  result = MlyValue.UlInclude (fn _ => (
 [] ))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( MlyValue.UlInclude UlInclude1, _, UlInclude1right))
 :: ( _, ( MlyValue.LOC LOC1, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.ULFILE ULFILE1, ULFILE1left, _)) :: rest671)) => let val  
result = MlyValue.UlInclude (fn _ => let val  (ULFILE as ULFILE1) = 
ULFILE1 ()
 val  (LOC as LOC1) = LOC1 ()
 val  (UlInclude as UlInclude1) = UlInclude1 ()
 in ( (ULFILE,LOC)::UlInclude )
end)
 in ( LrTable.NT 2, ( result, ULFILE1left, UlInclude1right), rest671)

end
|  ( 7, ( rest671)) => let val  result = MlyValue.UoInclude (fn _ => (
 [] ))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( MlyValue.UoInclude UoInclude1, _, UoInclude1right))
 :: ( _, ( MlyValue.UOFILE UOFILE1, UOFILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.UoInclude (fn _ => let val  (UOFILE as 
UOFILE1) = UOFILE1 ()
 val  (UoInclude as UoInclude1) = UoInclude1 ()
 in ( (UOFILE)::UoInclude )
end)
 in ( LrTable.NT 3, ( result, UOFILE1left, UoInclude1right), rest671)

end
|  ( 9, ( rest671)) => let val  result = MlyValue.SmlInclude (fn _ =>
 ( [] ))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.SmlInclude SmlInclude1, _, SmlInclude1right
)) :: ( _, ( MlyValue.SML SML1, _, _)) :: _ :: ( _, ( MlyValue.UOFILE 
UOFILE1, UOFILE1left, _)) :: rest671)) => let val  result = 
MlyValue.SmlInclude (fn _ => let val  (UOFILE as UOFILE1) = UOFILE1 ()
 val  (SML as SML1) = SML1 ()
 val  (SmlInclude as SmlInclude1) = SmlInclude1 ()
 in ( (UOFILE,SML)::SmlInclude )
end)
 in ( LrTable.NT 4, ( result, UOFILE1left, SmlInclude1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Result x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : UL_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ULFILE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ULFILE (fn () => i),p1,p2))
fun UOFILE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.UOFILE (fn () => i),p1,p2))
fun LOC (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.LOC (fn () => i),p1,p2))
fun SML (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.SML (fn () => i),p1,p2))
fun AS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ULFILES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun CODEFILES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun SCRIPTS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
end
end
