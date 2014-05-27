---
layout: page
title: MLKit Basis Library Implementation
---
{% include JB/setup %}

This page describes the implementation of the Standard ML Basis
Library in the MLKit. The table shows the status of each structure,
signature and functor.

| Module | Reviewed by | Status |
|:-------|:------------|:-------|
| Array structure | Mael |  OK |
| ARRAY signature | Mael |  OK |
| Array2 structure | Mael |  OK |
| ARRAY2 signature | Mael |  OK |
| ArraySlice structure | Mael |  OK |
| ARRAY_SLICE signature | Mael |  OK |
| BinIO structure | Mael |  OK - but with some extra functionality |
| BIN_IO signature | Mael |  OK |
| BIT_FLAGS signature | Varming |  OK |
| Bool structure | Mael |  OK |
| BOOL signature | Varming |  OK |
| Byte structure | Mael |  OK |
| BYTE signature | Mael |  OK |
| Char structure | Mael |  OK |
| CharArray structure | Mael |  OK |
| CharArraySlice structure | Mael |  OK |
| CharVector structure | Mael |  OK |
| CharVectorSlice structure | Mael |  OK |
| CHAR signature | Varming |  OK |
| CommandLine structure | Mael |  OK |
| COMMAND_LINE signature | Varming |  OK |
| Date structure | Mael |  OK |
| DATE signature | Varming |  OK |
| FixedInt structure | Mael |  OK |
| General structure | Mael |  OK |
| GENERAL signature | Mael |  OK |
| GenericSock structure | Mael |  NOT IMPLEMENTED |
| IEEEReal structure | Mael |  NOT IMPLEMENTED |
| IMPERATIVE_IO signature | Mael |  OK |
| ImperativeIO functor | Mael |  OK |
| INetSock structure | Mael |  NOT IMPLEMENTED |
| Int structure | Mael |  OK |
| Int31 structure | Mael |  OK |
| Int32 structure | Mael |  OK |
| INTEGER signature | Mael |  OK |
| INT_INF signature | Mael |  OK |
| IntInf structure | Mael |  More testing needed |
| IO structure | Mael |  OK |
| IO signature | Mael |  OK |
| LargeInt structure | Mael |  OK |
| List structure | Mael |  OK |
| LIST signature | Varming |  OK |
| ListPair structure | Mael |  OK |
| LIST_PAIR signature | Varming |  OK |
| Math structure | Mael |  OK |
| MATH signature | Varming |  OK |
| MONO_ARRAY signature | Mael |  OK |
| MONO_ARRAY2 signature | Mael |  NOT IMPLEMENTED |
| MONO_ARRAY_SLICE signature | Mael |  OK |
| MONO_VECTOR signature | Mael |  OK |
| MONO_VECTOR_SLICE signature | Mael |  OK |
| NetHostDB structure | Mael |  NOT IMPLEMENTED |
| NetProtDB structure | Mael |  NOT IMPLEMENTED |
| NetServDB structure | Mael |  NOT IMPLEMENTED |
| Option structure | Mael |  OK |
| OPTION signature | Mael |  OK |
| OS structure |
| OS_FILE_SYS signature | Varming |  OK |
| OS.FileSys structure | Mael |  OK |
| OS.IO structure |
| OS_PATH signature | Varming |  OK |
| OS.Path structure | Varming |  OK |
| OS_PROCESS signature | Varming |  OK |
| OS.Process structure | Mael |  OK |
| PACK_REAL signature | Varming |  OK |
| PACK_WORD signature | Varming |  OK |
| Posix structure |
| Posix.Error structure |
| Posix.FileSys structure |
| Posix.IO structure |
| Posix.ProcEnv structure |
| Posix.Process structure |
| Posix.Signal structure |
| Posix.SysDB structure |
| Posix.TTY structure |
| PRIM_IO signature | Mael |  OK |
| PrimIO functor | Mael |  OK |
| REAL signature | Mael |  OK - but many missing functions |
| Real structure | Varming |  Does not handle StringCvt.EXACT |
| Socket structure | Mael |  NOT IMPLEMENTED |
| STREAM_IO signature | Mael |  OK |
| StreamIO functor | Mael |  OK |
| String structure | Mael |  OK - but no scan value |
| STRING signature | Mael |  OK - but no scan value |
| StringCvt signature | Varming |  OK |
| StringCvt structure | Mael |  OK |
| Substring structure | Mael |  OK |
| SUBSTRING signature | Mael |  OK |
| TEXT signature | Varming |  OK |
| Text structure | Mael |  OK |
| TEXT_IO signature | Mael |  OK |
| TEXT_STREAM_IO signature | Mael |  OK |
| Time signature | Varming |  toNanoseconds and fromNanoseconds are waiting for support in structure, otherwise OK |
| Time structure | Varming |  Needs representation with nano seconds instead of micro seconds |
| Timer signature | Varming |  Awaiting support for checkCPUTimes and checkGCTime in structure. Otherwise OK |
| Timer structure | Varming |  Lacking implementations of checkCPUTimes and checkGCTime |
| Unix signature | Varming |  OK |
| Unix structure | Varming |  Should be implemented |
| UNIX_SOCK signature | Varming |  Awaiting implementation of UnixSock |
| UnixSock structure | Varming |  Should be implemented |
| Vector structure | Mael |  OK |
| VECTOR signature | Mael |  OK |
| VectorSlice structure | Mael |  OK |
| VECTOR_SLICE signature | Mael |  OK |
| Windows structure | Mael |  NOT IMPLEMENTED |
| Word8Array structure | Mael |  OK |
| Word8ArraySlice structure | Mael |  OK |
| Word8Vector structure | Mael |  OK |
| Word8VectorSlice structure | Mael |  OK |
| Word structure | Mael |  OK |
| Word31 structure | Mael |  OK |
| Word32 structure | Mael |  OK |
| Word structure | Mael |  OK|
| WORD signature| Mael |  OK |
{: class="table" }