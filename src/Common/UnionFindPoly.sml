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

  (* Pickler *)

  fun die s = 
      let val s = "UnionFindPoly." ^ s
      in print s; raise Fail s
      end

  val pu_intref = Pickle.ref0Gen Pickle.int

  fun pu (dummy : 'a) (pu_a : 'a Pickle.pu) : 'a Element Pickle.pu =
      let open Pickle
	  val dummy : 'a ElementNode = EQR(dummy,ref 0) 
	  val cache : 'a Element Pickle.pu option ref = ref NONE
	  fun pu_Element (pu_ElementNode : 'a ElementNode Pickle.pu) : 'a Element Pickle.pu =
	      case !cache of
		  SOME pu_e => pu_e
		| NONE => let val pu_e = refGen pu_ElementNode dummy 
			  in cache := SOME pu_e
			   ; pu_e
			  end
	      
	  fun toInt (EQR _) = 0
	    | toInt (LINK _) = 1
	  fun eq (LINK r1, LINK r2) = r1 = r2
	    | eq (EQR(i1,r1),EQR(i2,r2)) = r1 = r2
	      andalso #4 pu_a(i1,i2)
	    | eq _ = false
	  fun fun_EQR pu =
	      (fn EQR(i,r) => (fn spe => let val spe = pickler pu_a i spe
					     val spe = pickler pu_intref r spe
					 in spe
					 end)
	        | _ => die "fun_EQR.pickler",
	       fn supe => let val (i,supe) = unpickler pu_a supe
			      val (r,supe) = unpickler pu_intref supe
			  in (EQR(i,r),supe)
			  end,
	       fn EQR(i,r) => hashCombine(hasher pu_a i, hasher pu_intref r)
		| _ => die "fun_EQR.hasher",
	       eq)
	  fun fun_LINK pu = 
	      (fn LINK e => pickler (pu_Element pu) e
	        | _ => die "fun_LINK.pickler",
	       fn supe => let val (e,supe) = unpickler (pu_Element pu) supe
			  in (LINK e,supe)
			  end,
	       fn LINK e => hasher (pu_Element pu) e
		| _ => die "fun_LINK.hasher",
	       eq)
	  val pu = dataGen(toInt, eq, [fun_EQR,fun_LINK])
      in pu_Element pu
      end

end;
