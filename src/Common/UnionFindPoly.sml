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

structure UnionFindPoly: UNION_FIND_POLY =
struct

  type rank = int ref
  datatype 'info ElementNode = EQR of 'info * rank
                             | LINK of 'info Element
  withtype 'info Element = 'info ElementNode ref

  fun die s =
      let val s = "UnionFindPoly." ^ s
      in print s; raise Fail s
      end

  fun find e =
    (* Path halving *)
    case !e of
      LINK e' =>
	(case !e' of
	   LINK e'' => (e := LINK e''; find e'')
	 | EQR _ => e')
    | EQR _ => e

  fun find_info e =
      let val e = find e
      in case !e of
	  EQR(i,_) => i
	| _ => die "find_info.impossible"
      end

  fun find_rep_and_info e =
      let val e = find e
      in case !e of
	  EQR(i,_) => (e,i)
	| _ => die "find_rep_and_info.impossible"
      end

  fun set_info e i  =
      let val e = find e
      in case !e of
	  EQR(_,r) => e:= EQR(i,r)
	| _ => die "set_info.impossible"
      end

  fun eq_Elements (e1 : 'info Element, e2 : 'info Element) : bool =
      find e1 = find e2

  fun mkElement (info : '_info) = ref (EQR(info, ref 0))

  fun union info_combine (e,e') =
      (* Union by rank *)
      let val e = find e
	  val e' = find e'
      in
	  case (!e, !e') of
	      (EQR(i,r),EQR(i',r')) =>
		  if !r < !r' then
		      (e' := EQR(info_combine (i,i'),r');
		       e := LINK e';
		       e')
		  else
		      if !r' < !r then
			  (e := EQR(info_combine (i,i'),r);   (* was (i',i)  ; mael 2004-08-21 *)
			   e' := LINK e;
			   e)
		      else
			  (r' := !r' + 1;
			   e' := EQR(info_combine (i,i'),r');
			   e  := LINK e';
			   e')
	    | _ => die "union.impossible"
      end

  (* Pickler *)

  val pu_intref = Pickle.refOneGen Pickle.int  (* Perhaps not really safe as ranks are used
						* as first-class values *)

  fun pu (dummy : 'a) (pu_a : 'a Pickle.pu) : 'a Element Pickle.pu =
      let val dummy : 'a ElementNode = EQR(dummy,ref 0)
	  val pu_Element : 'a ElementNode Pickle.pu -> 'a Element Pickle.pu
	      = Pickle.cache "Element" (fn pu => let val pu = Pickle.refEqGen eq_Elements dummy pu
						 in Pickle.convert (fn a => a, fn a => find a) pu
						 end)
	  fun toInt (EQR _) = 0
	    | toInt (LINK _) = 1
	  fun fun_EQR pu =
	      Pickle.con1 EQR (fn EQR a => a | _ => die "pu.fun_EQR")
	      (Pickle.pairGen0(pu_a,pu_intref))
	  fun fun_LINK pu =
	      Pickle.con1 LINK (fn (* LINK a => a | *) _ => die "pu.fun_LINK")
	      (pu_Element pu)
	  val pu = Pickle.dataGen("UnionFindPoly.ElementNode",toInt,[fun_EQR,fun_LINK])
      in pu_Element pu
      end

  fun find x = x
end
