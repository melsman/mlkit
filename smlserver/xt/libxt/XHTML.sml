(* 
 * Copyright (c) 2003, Martin Elsman
 *
 * SMLserver interface for XHtml that statically guarantees (1)
 * validity of constructed documents and (2) consistent and 
 * typed use of forms. 
 *)

signature XHTML =
    sig
	structure A : XHTML_ATTR           (* Attributes *)
	type na = A.na

	(* In XHTML, pre elements may not contain big, small, sup, or
	 sub elements. This restriction is modelled using a phantom
	 type parameter 'p in the type for elements and requires the
	 abandonned elements to appear in contexts where 'p is
	 instantiated to preclosed. The type of the pre-element
	 constructor restricts arguments to have 'p instantiated to
	 inpre. *)
	type inpre
	type preclosed 

	(* In XHTML, a elements may not contain other a elements. *)
	type ina 
	type aclosed

	(* In XHTML, form elements may not contain other form elements. *)
	type inform 
	type formclosed

	(* The XHTML DTD distinguishes between block, inline, and flow
	 elements (block or inline).  This distinction is modelled
	 using a phantom type parameter 'kind in the type for
	 elements. The 'kind variable is used to model the following
	 partial order:
	 
	     li dl td tr inline flow    block flow
	       \ \ \  |    |               /
	        \ | | |  'a flow ---------'
	         \| | / ___/
	         'kind 
	 *)

	type 'a flow and block and inline    (* 'a flow, inline flow, and block flow *)
	type li and dl and td and tr

	type nil
	type ('n,'t) fname
	type 'a rad
	type 'a var = 'a Form.var

	(* Standard elements *)
	type ('x,'y,'a,'f,'p,'k,'v) elt

	type ('a1,'a2) coreattrs = ('a1,'a2,na,na,na,na,na,na,na,na,na,na,na,na) A.attr

	type ('x,'y,'a,'f,'p,'u,'v) inl2inl = 
	    ('x,'y,'a,'f,'p,inline flow,'u) elt -> ('x,'y,'a,'f,'p,inline flow,'v) elt

	type ('x,'y,'a,'f,'p,'u,'v) inl2inlpre = 
	    ('x,'y,'a,'f,'p,inline flow,'u) elt -> ('x,'y,'a,'f,preclosed,inline flow,'v) elt

	type ('x,'y,'a,'f,'p,'u,'v) inl2blk = 
	    ('x,'y,'a,'f,'p,inline flow,'u) elt -> ('x,'y,'a,'f,'p,block flow,'v) elt

	val em      : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* emphasis *)
	val strong  : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* strong emphasis *)
	val dfn     : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* definitional *)
	val code    : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* program code *)
	val samp    : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* sample *)
	val kbd     : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* user input *)
	val var     : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* variable *)
	val cite    : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* citation *)
	val abbr    : string -> ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl  (* abbreviation *)
	val acronym : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* acronym *)
	val sub     : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inlpre         (* subscript *)
	val sup     : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inlpre         (* superscript *)
	val tt      : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* fixed pitch *)
	val i       : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* italic *)
	val b       : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inl            (* bold *)
	val big     : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inlpre         (* bigger *)
	val small   : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2inlpre         (* smaller *)

	val $       : string -> ('x,'x,'a,'f,'p,inline flow,na) elt
	val br      : unit   -> ('x,'x,'a,'f,'p,inline flow,('a1,'a2)coreattrs) elt

	val p       : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2blk
	val h1      : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2blk            (* most important *)
	val h2      : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2blk
	val h3      : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2blk
	val h4      : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2blk
	val h5      : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2blk
	val h6      : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2blk            (* least important *)

	val div     : ('x,'y,'a,'f,'p,'bi flow,'u) elt -> ('x,'y,'a,'f,'p,block flow,('a1,'a2)coreattrs) elt

	val address : ('x,'y,'a,'f,'p,'u,('a1,'a2)coreattrs) inl2blk
	val blockquote : ('x,'y,'a,'f,'p,block flow,'u) elt -> ('x,'y,'a,'f,'p,block flow,('a1,'a2)coreattrs) elt
	val pre     : ('x,'y,'a,'f,inpre,inline flow,'u) elt       (* disallow big, small, *)
	              -> ('x,'y,'a,'f,preclosed,block flow,('a1,'a2)coreattrs) elt (*   sub, sup, and img. *)
											    
	val hr      : unit -> ('x,'x,'a,'f,'p,block flow,('a1,'a2)coreattrs) elt

	val &       : ('x,'y,'a,'f,'p,'k,'u1) elt * ('y,'z,'a,'f,'p,'k,'u2) elt 
                      -> ('x,'z,'a,'f,'p,'k,na) elt

	val flatten : ('x,'y,'a,'f,'p,'k,'u) elt * ('y,'y,'a,'f,'p,'k,'u) elt list 
                      -> ('x,'y,'a,'f,'p,'k,na) elt

	(* Lists *)
	val li      : ('x,'y,'a,'f,'p,'bi flow,'u) elt -> ('x,'y,'a,'f,'p,li,('a1,'a2)coreattrs) elt
	val dt      : ('x,'y,'a,'f,'p,inline flow,'u) elt -> ('x,'y,'a,'f,'p,dl,('a1,'a2)coreattrs) elt
	val dd      : ('x,'y,'a,'f,'p,'bi flow,'u) elt -> ('x,'y,'a,'f,'p,dl,('a1,'a2)coreattrs) elt
	val ol      : ('x,'y,'a,'f,'p,li,'u) elt -> ('x,'y,'a,'f,'p,block flow,('a1,'a2)coreattrs) elt
	val ul      : ('x,'y,'a,'f,'p,li,'u) elt -> ('x,'y,'a,'f,'p,block flow,('a1,'a2)coreattrs) elt
	val dl      : ('x,'y,'a,'f,'p,dl,'u) elt -> ('x,'y,'a,'f,'p,block flow,('a1,'a2)coreattrs) elt

	(* Images *)
	val img     : {src:string, alt:string} 
	              -> ('x,'x,'a,'f,preclosed,inline flow,
			  ('a1,'a2,A.width,'a3,A.height,'a4,na,na,na,na,na,na,na,na)A.attr) elt

	(* Tables *)
	val td      : ('x,'y,'a,'f,'p,'bi flow,'u) elt 
                      -> ('x,'y,'a,'f,'p,td,
			  ('a1, 'a2, A.align,'a3, A.valign,'a4, A.rowspan,'a5, 
			   A.colspan,'a6, na,na, na,na)A.attr) elt

	val th      : ('x,'y,'a,'f,'p,'bi flow,'u) elt 
                      -> ('x,'y,'a,'f,'p,td,
			  ('a1, 'a2, A.align,'a3, A.valign,'a4, A.rowspan,'a5, 
			   A.colspan,'a6, na,na, na,na)A.attr) elt

	val tr      : ('x,'y,'a,'f,'p,td,'u) elt 
                      -> ('x,'y,'a,'f,'p,tr,
			  ('a1, 'a2, A.align,'a3, A.valign,'a4, na,na, na,na, 
			   na,na, na,na)A.attr) elt

	val table   : ('x,'y,'a,'f,'p,tr,'u) elt 
                      -> ('x,'y,'a,'f,'p,block flow,
			  ('a1, 'a2, A.width,'a3, A.border,'a4, A.cellspacing,'a5, 
			   A.cellpadding,'a6, A.frame,'a7, A.rules,'a8)A.attr) elt

	(* Forms *)
	type ('x,'y,'a,'p,'k,'u) felt = ('x,'y,'a,inform,'p,'k,'u) elt
	type ('x,'a,'p,'k,'u) form = ('x,nil,'a,'p,'k,'u) felt

	(* Functionality for swapping the front name in a name list 
	 * with another name in the list. *)
	type ('old,'new) num
	val One   : unit -> ('n1->'n2->'x,'n2->'n1->'x) num
	val Succ  : ('n1->'x,'n2->'y) num -> ('n1->'n->'x,'n2->'n->'y) num 
	val swap  : ('x,'xx) num -> ('x,'y,'a,'p,'k,'u) felt 
                    -> ('xx,'y,'a,'p,'k,'u) felt

	(* Input elements *)
	val inputText      : ('n,'t)fname * 't var option 
                             -> ('n->'x,'x,'a,'p,inline flow,
				 ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
				  A.size,'a5, na,na, na,na)A.attr) felt

	val inputPassword  : ('n,'t)fname * 't var option 
                             -> ('n->'x,'x,'a,'p,inline flow,
				 ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
				  A.size,'a5, na,na, na,na)A.attr) felt

	val inputRadio     : ('n,'t option)fname * 't var 
                             -> ('n rad->'x,'x,'a,'p,inline flow,
				 ('a1, 'a2, A.checked,'a3, A.disabled,'a4, A.readonly,'a5, 
				  na,na, na,na, na,na)A.attr) felt
	val inputRadio'    : ('n,'t option)fname * 't var 
                             -> ('n rad->'x,'n rad->'x,'a,'p,inline flow,
				 ('a1, 'a2, A.checked,'a3, A.disabled,'a4, A.readonly,'a5, 
				  na,na, na,na, na,na)A.attr) felt

	val radioDrop      : ('n rad->'x,'y,'a,'p,'k,'u) felt -> ('n->'x,'y,'a,'p,'k,'u) felt

	type 'a checkbox

	val inputCheckbox  : ('n,'t list)fname * 't var 
                             -> ('n checkbox->'x,'x,'a,'p,inline flow,
				 ('a1, 'a2, A.checked,'a3, A.disabled,'a4, A.readonly,'a5, 
				  na,na, na,na, na,na)A.attr) felt

	val inputCheckbox' : ('n,'t list)fname * 't var 
                             -> ('n checkbox->'x,'n checkbox->'x,'a,'p,inline flow,
				 ('a1, 'a2, A.checked,'a3, A.disabled,'a4, A.readonly,'a5, 
				  na,na, na,na, na,na)A.attr) felt

	val checkboxDrop   : ('n checkbox->'x,'y,'a,'p,'k,'u) felt -> ('n->'x,'y,'a,'p,'k,'u) felt

 	val inputHidden    : ('n,'t)fname -> 't var 
                             -> ('n->'x,'x,'a,'p,inline flow,
				 ('a1,'a2)coreattrs) felt

 	val inputSubmit    : string 
                             -> ('x,'x,'a,'p,inline flow,
				 ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
				  na,na, na,na, na,na)A.attr) felt
 	val inputReset     : string 
                             -> ('x,'x,'a,'p,inline flow,
				 ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
				  na,na, na,na, na,na)A.attr) felt

	val textarea       : ('n,'t)fname * {rows:int,cols:int} * 't var option 
                             -> ('n->'x,'x,'a,'p,inline flow,
				 ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
				  na,na, na,na, na,na)A.attr) felt

	type 't select_option = {text: string, value: 't var, 
				 selected: bool, disabled: bool}

	val option         : string * 't var -> 't select_option
	val select         : ('n,'t)fname * 't select_option list 
                             -> ('n->'x,'x,'a,'p,inline flow,
				 ('a1, 'a2, na,na, A.disabled,'a3, na,na,
				  A.size,'a4, na,na, na,na)A.attr) felt

	(* Validate link *)
	val validLink : unit -> ('x,'x,'a,'f,'p,inline flow,na) elt

	(* Head elements *)
	type helt
	val script  : {typ:string} -> string -> helt
	val style   : {typ:string} -> string -> helt
	val meta    : {content:string} -> helt
	val link    : {typ:string,rel:string,href:string} -> helt

	type head
	val head    : string * helt list -> head

	(* HTML documents *)	
	type body
	val body    : (nil,nil,aclosed,formclosed,preclosed,block flow,'u) elt -> body
	type html
	val html    : head * body -> html

	val % : ('a1,'a2, 'b1,'b2, 'c,'c1,'c2, 'd,'d1,'d2, 
	      'e,'e1,'e2, 'f,'f1,'f2, 'g,'g1,'g2, 'h,'h1,'h2) A.attr0 *
	    ('a2,'a3, 'b2,'b3, 'c,'c2,'c3, 'd,'d2,'d3, 
	      'e,'e2,'e3, 'f,'f2,'f3, 'g,'g2,'g3, 'h,'h2,'h3) A.attr0 ->
	    ('a1,'a3, 'b1,'b3, 'c,'c1,'c3, 'd,'d1,'d3, 
	      'e,'e1,'e3, 'f,'f1,'f3, 'g,'g1,'g3, 'h,'h1,'h3) A.attr0

	val attr : ('arg -> 
	            ('x,'y,'a,'f,'p,'i,
		     ('a2, 'b2, 'c0,'c2, 'd0,'d2, 
		      'e0,'e2, 'f0,'f2, 'g0,'g2, 'h0,'h2) A.attr) elt)
	         * ('a2,'a3, 'b2,'b3, 'c0,'c2,'c3, 'd0,'d2,'d3, 
		    'e0,'e2,'e3, 'f0,'f2,'f3, 'g0,'g2,'g3, 'h0,'h2,'h3) A.attr0
	        -> ('arg -> 
		    ('x,'y,'a,'f,'p,'i,
		     ('a3, 'b3, 'c0,'c3, 'd0,'d3, 'e0,'e3, 
		      'f0,'f3, 'g0,'g3, 'h0,'h3) A.attr) elt)
	         

    end

signature XHTML_EXTRA =
    sig
	include XHTML
	structure Unsafe :
	    sig
		val form  : {action:string, method:string} 
                    -> ('x,'a,'p,block flow,'u)form 
                    -> (nil,nil,'a,formclosed,'p,block flow,
			('a1,'a2)coreattrs) elt

		val ahref : {src:string}
                    -> ('x,'y,ina,'f,'p,inline flow,'u)elt 
                    -> ('x,'y,aclosed,'f,'p,inline flow,
			('a1,'a2)coreattrs)elt 

		val toString   : html -> string
		val urlencode  : string -> string
		val htmlencode : string -> string
	    end
    end

