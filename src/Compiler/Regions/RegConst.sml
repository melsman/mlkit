
 (* constants that are used in the runtime system and need to be known to the
    compiler in order to issue warnings about array bounds etc.  If you
    change ALLOCATABLE_WORDS_IN_REGION_PAGE or HEADER_WORDS_IN_REGION_PAGE,
    remember also to change src/Runtime/Version17/Region.h.  If you change
    ALLOCATABLE_WORDS_IN_PRIM_ARRAY, remember also to change
    src/Runtime/Version17/Array.h*)

functor RegConst
  (structure Flags : FLAGS
   structure Crash : CRASH
   structure TyName : TYNAME) : REG_CONST =

struct

  structure TyName = TyName

   val ALLOCATABLE_WORDS_IN_REGION_PAGE = 254
         (*Number of words that can be allocated in each regionpage.
	  Used by CompLamb for determining whether every allocation
	  (alloc(...)) will fit in one page, and by KbpToHpPa to
	  generate code (to allocate etc.).
	  If you change this, remember also to change src/Compiler/Kam/CConst.sml.*)
   val HEADER_WORDS_IN_REGION_PAGE = 2
         (*Number of words in the header part of a region page.
	  If you change this, remember also to change src/Compiler/Kam/CConst.sml.*)
   val MAX_ORIGIN = 10000
         (*number of distinct region variables in program - needed
	  for region profiling *)
   val ALLOCATABLE_WORDS_IN_PRIM_ARRAY = ALLOCATABLE_WORDS_IN_REGION_PAGE - 5
   val initial_closure_offset = 1	(*initial offset for free variables in a closure*)

   val tag_values = Flags.is_on0 "tag_values"

   fun size_of_real () = 
     if tag_values() then 3 else 2

   fun size_of_ref () = 
     if tag_values() then 2 else 1

   fun size_of_record l = 
     if tag_values() then List.length l + 1 else List.length l

   fun size_closure (l1,l2,l3) = 
     if tag_values() then List.length l1 + List.length l2 + List.length l3 + 1 + 1 (* code pointer and tag *)
     else List.length l1 + List.length l2 + List.length l3 + 1

   fun size_fix_closure (l1,l2,l3) = 
     if tag_values() then List.length l1 + List.length l2 + List.length l3 + 1 
     else List.length l1 + List.length l2 + List.length l3

   fun size_region_vector l = 
     if tag_values() then List.length l + 1 
     else List.length l

   fun size_exname() = 
     if tag_values() then 3 else 2

   fun size_excon0() = 
     if tag_values() then 2 else 1

   fun size_excon1() = 
     if tag_values() then 3 else 2

   fun size_con0() = 1 (* boxed CON0 is always 1 word *)
   fun size_con1() = 2 (* boxed CON1 is always 2 words. *)
	     
end;
