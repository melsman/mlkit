(* 
 * Copyright (c) 2003, 2004, Martin Elsman
 *
 * SMLserver interface for XHTML 1.0 that statically guarantees (1)
 * validity of constructed documents and (2) consistent and 
 * typed use of forms. 
 *)

signature XHTML =
    sig
	structure A : XHTML_ATTR           (* Attributes *)
	type na = A.na

	(* In XHTML 1.0, pre elements may not contain big, small, sup,
         or sub elements. This restriction is modelled using a phantom
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
	 using a phantom type parameter 'k as follows:
 
          element type     ML type               description
	    block        flow(block,NOT)      flow-elements not containing inline elements
            inline       flow(NOT,inline)     flow-elements not containing block elements
            flow         flow(block,inline)   combined flow-elements

         Parts of the partial order:
	 
	     li dl td tr (NOT,inline)flow  (block,inline)flow    (block,NOT)flow     
	       \ \ \  |    |               /               \       /
	        \ | | |  ('a,inline)flow -'                 (block,'i)flow
	         \| | / ___/-------------------------------/
	         'kind 
	 *)

	type ('b,'i) flow and block and inline and NOT    (* 'a flow, inline flow, and block flow *)
	type li and dl and td and tr

	type nil
	type ('n,'t) fname
	type 'a rad
	type 'a var = 'a Form.var

	(* Standard elements *)
	type ('x,'y,'a,'f,'p,'k) elt

	type ('x,'y,'a,'f,'p,'b) inl2inl = 
	    ('x,'y,'a,'f,'p,(NOT,inline)flow) elt -> ('x,'y,'a,'f,'p,('b,inline)flow) elt

	type ('x,'y,'a,'f,'p,'b) inl2inlpre = 
	    ('x,'y,'a,'f,'p,(NOT,inline)flow) elt -> ('x,'y,'a,'f,preclosed,('b,inline)flow) elt

	type ('x,'y,'a,'f,'p,'i) inl2blk = 
	    ('x,'y,'a,'f,'p,(NOT,inline)flow) elt -> ('x,'y,'a,'f,'p,(block,'i)flow) elt

	val em      : ('x,'y,'a,'f,'p,'b) inl2inl            (* emphasis *)
	val strong  : ('x,'y,'a,'f,'p,'b) inl2inl            (* strong emphasis *)
	val dfn     : ('x,'y,'a,'f,'p,'b) inl2inl            (* definitional *)
	val code    : ('x,'y,'a,'f,'p,'b) inl2inl            (* program code *)
	val samp    : ('x,'y,'a,'f,'p,'b) inl2inl            (* sample *)
	val kbd     : ('x,'y,'a,'f,'p,'b) inl2inl            (* user input *)
	val var     : ('x,'y,'a,'f,'p,'b) inl2inl            (* variable *)
	val cite    : ('x,'y,'a,'f,'p,'b) inl2inl            (* citation *)
	val abbr    : string -> ('x,'y,'a,'f,'p,'b) inl2inl  (* abbreviation *)
	val acronym : ('x,'y,'a,'f,'p,'b) inl2inl            (* acronym *)
	val sub     : ('x,'y,'a,'f,'p,'b) inl2inlpre         (* subscript *)
	val sup     : ('x,'y,'a,'f,'p,'b) inl2inlpre         (* superscript *)
	val tt      : ('x,'y,'a,'f,'p,'b) inl2inl            (* fixed pitch *)
	val i       : ('x,'y,'a,'f,'p,'b) inl2inl            (* italic *)
	val b       : ('x,'y,'a,'f,'p,'b) inl2inl            (* bold *)
	val big     : ('x,'y,'a,'f,'p,'b) inl2inlpre         (* bigger *)
	val small   : ('x,'y,'a,'f,'p,'b) inl2inlpre         (* smaller *)

	val $       : string -> ('x,'x,'a,'f,'p,('b,inline)flow) elt
	val br      : unit   -> ('x,'x,'a,'f,'p,('b,inline)flow) elt

	val p       : ('x,'y,'a,'f,'p,'i) inl2blk
	val h1      : ('x,'y,'a,'f,'p,'i) inl2blk            (* most important *)
	val h2      : ('x,'y,'a,'f,'p,'i) inl2blk
	val h3      : ('x,'y,'a,'f,'p,'i) inl2blk
	val h4      : ('x,'y,'a,'f,'p,'i) inl2blk
	val h5      : ('x,'y,'a,'f,'p,'i) inl2blk
	val h6      : ('x,'y,'a,'f,'p,'i) inl2blk            (* least important *)

	val div     : ('x,'y,'a,'f,'p,(block,inline)flow) elt 
                      -> ('x,'y,'a,'f,'p,(block,'i)flow) elt

	val address : ('x,'y,'a,'f,'p,'i) inl2blk
	val blockquote : ('x,'y,'a,'f,'p,(block,NOT)flow) elt 
                         -> ('x,'y,'a,'f,'p,(block,'i)flow) elt
	val pre     : ('x,'y,'a,'f,inpre,(NOT,inline)flow) elt       (* disallow big, small, *)
	              -> ('x,'y,'a,'f,preclosed,(block,'i)flow) elt  (*   sub, sup, and img. *)
											    
	val hr      : unit -> ('x,'x,'a,'f,'p,(block,'i)flow) elt

	val &       : ('x,'y,'a,'f,'p,'k) elt * ('y,'z,'a,'f,'p,'k) elt 
                      -> ('x,'z,'a,'f,'p,'k) elt

	val flatten : ('x,'y,'a,'f,'p,'k) elt * ('y,'y,'a,'f,'p,'k) elt list 
                      -> ('x,'y,'a,'f,'p,'k) elt

	(* Lists *)
	val li      : ('x,'y,'a,'f,'p,(block,inline)flow) elt -> ('x,'y,'a,'f,'p,li) elt
	val dt      : ('x,'y,'a,'f,'p,(NOT,inline)flow) elt -> ('x,'y,'a,'f,'p,dl) elt
	val dd      : ('x,'y,'a,'f,'p,(block,inline)flow) elt -> ('x,'y,'a,'f,'p,dl) elt
	val ol      : ('x,'y,'a,'f,'p,li) elt -> ('x,'y,'a,'f,'p,(block,'i)flow) elt
	val ul      : ('x,'y,'a,'f,'p,li) elt -> ('x,'y,'a,'f,'p,(block,'i)flow) elt
	val dl      : ('x,'y,'a,'f,'p,dl) elt -> ('x,'y,'a,'f,'p,(block,'i)flow) elt

	(* Images *)
	val img     : {src:string,alt:string} -> ('x,'x,'a,'f,preclosed,('b,inline)flow) elt
	val imga    : ('aa, 'bb, A.width,'c, A.height,'d, na,na, 
		       na,na, na,na, na,na) A.attr
	              -> {src:string, alt:string} -> ('x,'x,'a,'f,preclosed,('b,inline)flow) elt

	(* Tables *)
	val td      : ('x,'y,'a,'f,'p,(block,inline)flow) elt -> ('x,'y,'a,'f,'p,td) elt
	val tda     : ('aa, 'b, A.align,'c, A.valign,'d, A.rowspan,'e, 
		       A.colspan,'ff, na,na, na,na) A.attr
	              -> ('x,'y,'a,'f,'p,(block,inline)flow) elt -> ('x,'y,'a,'f,'p,td) elt

	val th      : ('x,'y,'a,'f,'p,(block,inline)flow) elt -> ('x,'y,'a,'f,'p,td) elt
	val tha     : ('aa, 'b, A.align,'c, A.valign,'d, A.rowspan,'e, 
		       A.colspan,'ff, na,na, na,na) A.attr
	              -> ('x,'y,'a,'f,'p,(block,inline)flow) elt -> ('x,'y,'a,'f,'p,td) elt

	val tr      : ('x,'y,'a,'f,'p,td) elt -> ('x,'y,'a,'f,'p,tr) elt
	val tra     : ('aa, 'b, A.align,'c, A.valign,'d, na,na, na,na, 
		       na,na, na,na) A.attr  -> ('x,'y,'a,'f,'p,td) elt
                       -> ('x,'y,'a,'f,'p,tr) elt

	val table   : ('x,'y,'a,'f,'p,tr) elt -> ('x,'y,'a,'f,'p,(block,'i)flow) elt
	val tablea  : ('aa, 'b, A.width,'c, A.border,'d, A.cellspacing,'e, 
		       A.cellpadding,'ff, A.frame,'g, A.rules,'h) A.attr
	              -> ('x,'y,'a,'f,'p,tr) elt -> ('x,'y,'a,'f,'p,(block,'i)flow) elt

	(* Forms *)
	type ('x,'y,'a,'p,'k) felt = ('x,'y,'a,inform,'p,'k) elt
	type ('x,'a,'p,'k) form = ('x,nil,'a,'p,'k) felt

	(* Functionality for swapping the front name in a name list 
	 * with another name in the list. *)
	type ('old,'new) num
	val One   : unit -> ('n1->'n2->'x,'n2->'n1->'x) num
	val Succ  : ('n1->'x,'n2->'y) num -> ('n1->'n->'x,'n2->'n->'y) num 
	val swap  : ('x,'xx) num -> ('x,'y,'a,'p,'k) felt 
                    -> ('xx,'y,'a,'p,'k) felt

	(* Input elements *)
	val inputText      : ('n,'t)fname -> 't var option -> ('n->'x,'x,'a,'p,('b,inline)flow) felt
	val inputTexta     : ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
			      A.size,'a5, na,na, na,na) A.attr
	                     -> ('n,'t)fname -> 't var option -> ('n->'x,'x,'a,'p,('b,inline)flow) felt

	val inputPassword  : ('n,'t)fname -> 't var option -> ('n->'x,'x,'a,'p,('b,inline)flow) felt
	val inputPassworda : ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
			      A.size,'a5, na,na, na,na) A.attr
	                     -> ('n,'t)fname -> 't var option -> ('n->'x,'x,'a,'p,('b,inline)flow) felt

	val inputRadio     : ('n,'t option)fname -> 't var -> ('n rad->'x,'x,'a,'p,('b,inline)flow) felt
	val inputRadioa    : ('a1, 'a2, A.checked,'a3, A.disabled,'a4, A.readonly,'a5, 
			      na,na, na,na, na,na)A.attr
	                     -> ('n,'t option)fname -> 't var -> ('n rad->'x,'x,'a,'p,('b,inline)flow) felt

	val inputRadio'    : ('n,'t option)fname -> 't var -> ('n rad->'x,'n rad->'x,'a,'p,('b,inline)flow) felt
	val inputRadioa'   : ('a1, 'a2, A.checked,'a3, A.disabled,'a4, A.readonly,'a5, 
			      na,na, na,na, na,na)A.attr
	                     -> ('n,'t option)fname -> 't var -> ('n rad->'x,'n rad->'x,'a,'p,('b,inline)flow) felt

	val radioDrop      : ('n rad->'x,'y,'a,'p,'k) felt -> ('n->'x,'y,'a,'p,'k) felt

	type 'a checkbox

	val inputCheckbox  : ('n,'t list)fname -> 't var -> ('n checkbox->'x,'x,'a,'p,('b,inline)flow) felt
	val inputCheckboxa : ('a1, 'a2, A.checked,'a3, A.disabled,'a4, A.readonly,'a5, 
			      na,na, na,na, na,na)A.attr
	                     -> ('n,'t list)fname -> 't var -> ('n checkbox->'x,'x,'a,'p,('b,inline)flow) felt

	val inputCheckbox' : ('n,'t list)fname -> 't var 
                             -> ('n checkbox->'x,'n checkbox->'x,'a,'p,('b,inline)flow) felt
	val inputCheckboxa': ('a1, 'a2, A.checked,'a3, A.disabled,'a4, A.readonly,'a5, 
			      na,na, na,na, na,na)A.attr
	                     -> ('n,'t list)fname -> 't var 
                             -> ('n checkbox->'x,'n checkbox->'x,'a,'p,('b,inline)flow) felt

	val checkboxDrop   : ('n checkbox->'x,'y,'a,'p,'k) felt -> ('n->'x,'y,'a,'p,'k) felt

 	val inputHidden    : ('n,'t)fname -> 't var -> ('n->'x,'x,'a,'p,('b,inline) flow) felt
 	val inputHiddena   : ('a1,'a2,na,na,na,na,na,na,na,na,na,na,na,na) A.attr
	                     -> ('n,'t)fname -> 't var -> ('n->'x,'x,'a,'p,('b,inline)flow) felt

 	val inputSubmit    : string -> ('x,'x,'a,'p,('b,inline)flow) felt
 	val inputSubmita   : ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
			      na,na, na,na, na,na)A.attr
	                     -> string -> ('x,'x,'a,'p,('b,inline)flow) felt

 	val inputReset     : string -> ('x,'x,'a,'p,('b,inline)flow) felt
 	val inputReseta    : ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
			      na,na, na,na, na,na)A.attr
	                     -> string -> ('x,'x,'a,'p,('b,inline)flow) felt

	val textarea       : ('n,'t)fname -> {rows:int,cols:int} -> 't var option 
                             -> ('n->'x,'x,'a,'p,('b,inline)flow) felt
	val textareaa      : ('a1, 'a2, na,na, A.disabled,'a3, A.readonly,'a4, 
			      na,na, na,na, na,na)A.attr
	                     -> ('n,'t)fname -> {rows:int,cols:int} -> 't var option 
                             -> ('n->'x,'x,'a,'p,('b,inline)flow) felt

	type 't select_option = {text: string, value: 't var, 
				 selected: bool, disabled: bool}

	val option         : string * 't var -> 't select_option

	val select         : ('n,'t)fname -> 't select_option list 
                             -> ('n->'x,'x,'a,'p,('b,inline)flow) felt
	val selecta        : ('a1, 'a2, na,na, A.disabled,'a3, na,na,
			      A.size,'a4, na,na, na,na)A.attr
	                     -> ('n,'t)fname -> 't select_option list 
                             -> ('n->'x,'x,'a,'p,('b,inline)flow) felt

	(* Validate link *)
	val validLink : unit -> ('x,'x,'a,'f,'p,('b,inline)flow) elt

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
	val body    : (nil,nil,aclosed,formclosed,preclosed,(block,NOT)flow) elt -> body
	type html
	val html    : head * body -> html

	val % : ('a1,'a2, 'b1,'b2, 'c,'c1,'c2, 'd,'d1,'d2, 
	      'e,'e1,'e2, 'f,'f1,'f2, 'g,'g1,'g2, 'h,'h1,'h2) A.attr0 *
	    ('a2,'a3, 'b2,'b3, 'c,'c2,'c3, 'd,'d2,'d3, 
	      'e,'e2,'e3, 'f,'f2,'f3, 'g,'g2,'g3, 'h,'h2,'h3) A.attr0 ->
	    ('a1,'a3, 'b1,'b3, 'c,'c1,'c3, 'd,'d1,'d3, 
	      'e,'e1,'e3, 'f,'f1,'f3, 'g,'g1,'g3, 'h,'h1,'h3) A.attr0
    end

signature XHTML_EXTRA =
    sig
	include XHTML
	structure Unsafe :
	    sig
		val form  : {action:string, method:string} 
                    -> ('x,'a,'p,(block,NOT)flow)form 
                    -> (nil,nil,'a,formclosed,'p,(block,'i)flow) elt

		val ahref : {src:string}
                    -> ('x,'y,ina,'f,'p,(NOT,inline)flow)elt 
                    -> ('x,'y,aclosed,'f,'p,('b,inline)flow)elt 

		val toString   : html -> string
		val urlencode  : string -> string
		val htmlencode : string -> string
	    end
    end

