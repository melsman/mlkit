(* Initial allows for other modules to be discharged at link time *)
local Initial.sml
in

(* General *)
GENERAL.sml General.sml OPTION.sml Option.sml

(* Lists *)
LIST.sml List.sml LIST_PAIR.sml ListPair.sml LISTSORT.sml
Listsort.sml

(* Maps *)
local SPLAYTREE.sml Splaytree.sml
in SPLAYMAP.sml Splaymap.sml SPLAYSET.sml Splayset.sml
end

BINARYMAP.sml Binarymap.sml INTMAP.sml Intmap.sml

(* Sets *)
BINARYSET.sml Binaryset.sml INTSET.sml Intset.sml

(* Arrays and Vectors *)
inline funapps in 
    local wordtables.sml 
    in VECTOR.sml Vector.sml ARRAY.sml Array.sml ARRAY2.sml Array2.sml
    end

    MONO_VECTOR.sml MONO_ARRAY.sml 
    ByteTable.sml 
end

ARRAYSORT.sml Arraysort.sml

(* Text *)
STRING_CVT.sml StringCvt.sml 

local STR_BASE.sml StrBase.sml 
in Char.sml String.sml CHAR.sml STRING.sml SUBSTRING.sml Substring.sml 
end

BOOL.sml
Bool.sml

(* Integers and Words *)
WORD.sml Word.sml Word32.sml Word31.sml Word8.sml 
BYTE.sml Byte.sml 
INTEGER.sml Int.sml Int32.sml Int31.sml 
PACK_WORD.sml Pack32Little.sml Pack32Big.sml

(* Reals *)
MATH.sml Math.sml REAL.sml Real.sml

(* IntInf -- makes use of Real *)
INT_INF.sml IntInf.sml

(* Hash Tables *)
POLYHASH.sml Polyhash.sml

(* IO *)
IO.sml TEXT_IO-1997.sml TextIO-1997.sml BIN_IO.sml BinIO.sml

(* System *)
TIME.sml Time.sml RANDOM.sml Random.sml OS_PATH-1997.sml Path-1997.sml
OS_FILE_SYS.sml FileSys.sml OS_PROCESS.sml Process.sml OS.sml
COMMAND_LINE.sml CommandLine.sml DATE.sml Date.sml TIMER-1997.sml Timer-1997.sml

(* Suspension *)
SUSP.sml Susp.sml

(* Regular expression support *)
REG_EXP.sml RegExp.sml

(* Misc *)
SML90.sml

end (*Initial*)
