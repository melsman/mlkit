(*
 *			
 *	File:     UnionFindPoly.sml
 *	Author:   Lars Birkedal (birkedal@diku.dk)
 *	Created:  Wed May 12 15:26:35 MET DST 1993
 *	Modified: Fri Apr 29 13:37:13 MET DST 1994
 *
 *	Contents: Polymorphic Union Find 
 *	          Tarjan, ``Data Structures and Network Algorithms'', 1983
 *
 *)

(* 24 Feb 1996: modified ElementNode type by replacing   'info ref    by  'info *)

(*$UnionFindPoly: UNION_FIND_POLY *)
functor Union_by_rank() =
struct 
  type rank = int ref    
  datatype 'info ElementNode = EQR of 'info * rank
                             | LINK of 'info Element
  withtype 'info Element = 'info ElementNode ref

  exception UnionFind

  val eq_Elements : 'info Element * 'info Element -> bool  =  (op =)

  fun mkElement (info : '_info) = ref (EQR(info, ref 0))

  fun union info_combine (e,e') =
    (* Union by rank *)
    case (!e, !e') of
      (EQR(i,r),EQR(i',r')) => 
	if !r < !r' then
	  (e' := EQR(info_combine (i,i'),r');
	   e := LINK e';
	   e')
	else 
	  if !r' < !r then
	    (e := EQR(info_combine (i',i),r);
	     e' := LINK e;
	     e)
	  else
	    (r' := !r' + 1;
	     e' := EQR(info_combine (i,i'),r');
	     e  := LINK e';
	     e')
    | _ => raise UnionFind

  fun get_info e = 
    case !e of
      EQR(i,_) => i
    | _ => raise UnionFind

  fun set_info e i  =
    case !e of
      EQR(i',r) => e:= EQR(i,r)
    | _ => raise UnionFind
end;

functor UF_with_path_halving_and_union_by_rank() : UNION_FIND_POLY =
struct
  structure Union = Union_by_rank()
  open Union

  fun find e =         
    (* Path halving *)
    case !e of 
      LINK e' =>
	(case !e' of
	   LINK e'' => (e := LINK e''; find e'')
	 | EQR _ => e')
    | EQR _ => e

  fun find_info e = 
    (* Path halving *)
    case !e of 
      LINK e' =>
	(case !e' of
	   LINK e'' => (e := LINK e''; find_info e'')
	 | EQR(i,_) => i)
    | EQR(i,_) => i

  fun find_rep_and_info e =
    case !e of 
      LINK e' =>
	(case !e' of
	   LINK e'' => (e := LINK e''; find_rep_and_info e'')
	 | EQR(i,_) => (e', i))
    | EQR(i,_) => (e,i)
end;
