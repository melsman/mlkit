(*  TEST FILE

A test file is a file that mentions a set of Standard ML sources and
projects. A test file has extension `.tst'.  Entries in a test file
consists of a file name path (with extension sml, sig, or mlb) followed
by a list of tokens. The following tokens are supported:

 nobasislib     ; do not import basis library
 nooptimiser    ; disable lambda optimiser
 ccl            ; compare compiler logs
 tx             ; time executable
 tc             ; time compiler
 ecte           ; expect compile time error
 ue             ; expect uncaught exception

 nogc 			; disable gc
 parallel		; enable parallel computation

Test files may contain Standard ML like comments.

*)

(* -------------------------------------- *)
(* Testing functionality of region module *)
(* -------------------------------------- *)

resetRegion_doesNotIncreaseMemoryUsage.mlb  nogc
resetRegion_idempotent_memoryUsage.mlb		nogc
resetRegion_keepsAtbot_onEmpty.mlb			nogc
resetRegion_setsAtbot_afterAllocation.mlb	nogc
resetRegion_resets_with_liveRef.mlb			nogc
resetRegion_doesNotIncreaseMemoryUsage.mlb  nogc parallel
resetRegion_idempotent_memoryUsage.mlb		nogc parallel
resetRegion_keepsAtbot_onEmpty.mlb			nogc parallel
resetRegion_setsAtbot_afterAllocation.mlb	nogc parallel
resetRegion_resets_with_liveRef.mlb			nogc parallel


isAtbot_true_initial.mlb					nogc 
isAtbot_false_afterAllocation.mlb			nogc		 
isAtbot_true_initial.mlb					nogc parallel
isAtbot_false_afterAllocation.mlb			nogc parallel

numPages_nonnegative_initial.mlb 			nogc
numPages_increasing_afterAllocation.mlb 	nogc
numPages_nonnegative_initial.mlb 			nogc parallel
numPages_increasing_afterAllocation.mlb 	nogc parallel

memoryUsage_nonnegative_initial.mlb			nogc
memoryUsage_increases_afterAllocation.mlb   nogc
memoryUsage_nonnegative_initial.mlb			nogc parallel
memoryUsage_increases_afterAllocation.mlb   nogc parallel


getPageSizeBytes_stable.mlb					nogc
getPageSizeBytes_positive.mlb				nogc
getPageSizeBytes_stable.mlb					nogc parallel
getPageSizeBytes_positive.mlb				nogc parallel

getNumAllocatedPages_nonnegative.mlb		nogc
getNumAllocatedPages_getFreeList.mlb		nogc
getNumAllocatedPages_nonnegative.mlb		nogc parallel
getNumAllocatedPages_getFreeList.mlb		nogc parallel

getFreeListSize_nonnegative.mlb				nogc
freeList_lAllocatedPages.mlb				nogc
getFreeListSize_nonnegative.mlb				nogc parallel
freeList_lAllocatedPages.mlb				nogc parallel


getThreadFreeListSize_nonnegative.mlb		nogc
threadFree_lTotalAllocated.mlb				nogc
getThreadFreeListSize_nonnegative.mlb		nogc parallel
threadFree_lTotalAllocated.mlb				nogc parallel

giveBack_globalFreeList_nondecreasing.mlb	nogc
giveBack_idempotent_globalFreeList.mlb		nogc
giveBack_idempotent_threadFreeList.mlb		nogc
giveBack_preservesFreeListSum.mlb			nogc
giveBack_preservesTotalAllocatedPages.mlb	nogc
giveBack_threadFreeList_nonincreasing.mlb	nogc
giveBack_globalFreeList_nondecreasing.mlb	nogc parallel
giveBack_idempotent_globalFreeList.mlb		nogc parallel
giveBack_idempotent_threadFreeList.mlb		nogc parallel
giveBack_preservesFreeListSum.mlb			nogc parallel
giveBack_preservesTotalAllocatedPages.mlb	nogc parallel
giveBack_threadFreeList_nonincreasing.mlb	nogc parallel


region_usageWithinAllocatedPages.mlb		nogc
region_usageWithinAllocatedPages.mlb		nogc parallel


(* -------------------------------------- *)
(* Testing Size combinator module         *)
(* -------------------------------------- *)

size_int_zero.mlb							nogc
size_string_positive.mlb					nogc
size_list_nil_is_word.mlb					nogc
size_list_grows.mlb							nogc
size_tup2_positive.mlb						nogc
size_option_some_gt_none.mlb				nogc
size_leq_regionUsage.mlb					nogc

size_int_zero.mlb							nogc parallel
size_string_positive.mlb					nogc parallel
size_list_nil_is_word.mlb					nogc parallel
size_list_grows.mlb							nogc parallel
size_tup2_positive.mlb						nogc parallel
size_option_some_gt_none.mlb				nogc parallel
size_leq_regionUsage.mlb					nogc parallel
