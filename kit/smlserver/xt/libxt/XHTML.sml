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
	  using a phantom type parameter 'bi in the type for
	  elements. *)
	type inline 
	type block   

	type nil
	type ('n,'t) fname
	type 'a rad
	type 'a obj = 'a Obj.obj

	(* Standard elements *)
	type ('x,'y,'a,'f,'p,'ib) elt

	type ('x,'y,'a,'f,'p) inl2inl = 
	    ('x,'y,'a,'f,'p,inline) elt -> ('x,'y,'a,'f,'p,inline) elt

	type ('x,'y,'a,'f,'p) inl2inlpre = 
	    ('x,'y,'a,'f,'p,inline) elt -> ('x,'y,'a,'f,preclosed,inline) elt

	type ('x,'y,'a,'f,'p) inl2blk = 
	    ('x,'y,'a,'f,'p,inline) elt -> ('x,'y,'a,'f,'p,block) elt

	val em      : ('x,'y,'a,'f,'p) inl2inl            (* emphasis *)
	val strong  : ('x,'y,'a,'f,'p) inl2inl            (* strong emphasis *)
	val dfn     : ('x,'y,'a,'f,'p) inl2inl            (* definitional *)
	val code    : ('x,'y,'a,'f,'p) inl2inl            (* program code *)
	val samp    : ('x,'y,'a,'f,'p) inl2inl            (* sample *)
	val kbd     : ('x,'y,'a,'f,'p) inl2inl            (* user input *)
	val var     : ('x,'y,'a,'f,'p) inl2inl            (* variable *)
	val cite    : ('x,'y,'a,'f,'p) inl2inl            (* citation *)
	val abbr    : string -> ('x,'y,'a,'f,'p) inl2inl  (* abbreviation *)
	val acronym : ('x,'y,'a,'f,'p) inl2inl            (* acronym *)
	val sub     : ('x,'y,'a,'f,'p) inl2inlpre         (* subscript *)
	val sup     : ('x,'y,'a,'f,'p) inl2inlpre         (* superscript *)
	val tt      : ('x,'y,'a,'f,'p) inl2inl            (* fixed pitch *)
	val i       : ('x,'y,'a,'f,'p) inl2inl            (* italic *)
	val b       : ('x,'y,'a,'f,'p) inl2inl            (* bold *)
	val big     : ('x,'y,'a,'f,'p) inl2inlpre         (* bigger *)
	val small   : ('x,'y,'a,'f,'p) inl2inlpre         (* smaller *)

	val $       : string -> ('x,'x,'a,'f,'p,inline) elt
	val br      : unit   -> ('x,'x,'a,'f,'p,inline) elt

	val p       : ('x,'y,'a,'f,'p) inl2blk
	val h1      : ('x,'y,'a,'f,'p) inl2blk            (* most important *)
	val h2      : ('x,'y,'a,'f,'p) inl2blk
	val h3      : ('x,'y,'a,'f,'p) inl2blk
	val h4      : ('x,'y,'a,'f,'p) inl2blk
	val h5      : ('x,'y,'a,'f,'p) inl2blk
	val h6      : ('x,'y,'a,'f,'p) inl2blk            (* least important *)

	val div     : ('x,'y,'a,'f,'p,'bi) elt -> ('x,'y,'a,'f,'p,block) elt

	val address : ('x,'y,'a,'f,'p) inl2blk
	val blockquote : ('x,'y,'a,'f,'p,block) elt -> ('x,'y,'a,'f,'p,block) elt
	val pre     : ('x,'y,'a,'f,inpre,inline) elt       (* disallow big, small, *)
	              -> ('x,'y,'a,'f,preclosed,block) elt (*   sub, sup, and img. *)
											    
	val hr      : unit -> ('x,'x,'a,'f,'p,block) elt
	val &&      : ('x,'y,'a,'f,'p,'bi) elt * ('y,'z,'a,'f,'p,'bi) elt 
                      -> ('x,'z,'a,'f,'p,'bi) elt

	(* Lists *)
	type li and dl       (* list kinds *)
	type ('x,'y,'a,'f,'p,'k) lis  (*'k ranges over list kinds *)
	val li      : ('x,'y,'a,'f,'p,'bi) elt -> ('x,'y,'a,'f,'p,li) lis
	val dt      : ('x,'y,'a,'f,'p,inline) elt -> ('x,'y,'a,'f,'p,dl) lis
	val dd      : ('x,'y,'a,'f,'p,'bi) elt -> ('x,'y,'a,'f,'p,dl) lis
	val ++      : ('x,'y,'a,'f,'p,'k) lis * ('y,'z,'a,'f,'p,'k) lis 
                      -> ('x,'z,'a,'f,'p,'k) lis
	val ol      : ('x,'y,'a,'f,'p,li) lis -> ('x,'y,'a,'f,'p,block) elt
	val ul      : ('x,'y,'a,'f,'p,li) lis -> ('x,'y,'a,'f,'p,block) elt
	val dl      : ('x,'y,'a,'f,'p,dl) lis -> ('x,'y,'a,'f,'p,block) elt

	(* Images *)
	val img     : {src:string,alt:string} -> ('x,'x,'a,'f,preclosed,inline) elt
	val imga    : ('aa, 'b, A.width,'c, A.height,'d, na,na, 
		       na,na, na,na, na,na) A.attr
	              -> {src:string, alt:string} -> ('x,'x,'a,'f,preclosed,inline) elt

	(* Tables *)
	type td and tr  (* additional list kinds *)
	val td      : ('x,'y,'a,'f,'p,'bi) elt -> ('x,'y,'a,'f,'p,td) lis
	val tda     : ('aa, 'b, A.align,'c, A.valign,'d, A.rowspan,'e, 
		       A.colspan,'ff, na,na, na,na) A.attr
	              -> ('x,'y,'a,'f,'p,'bi) elt -> ('x,'y,'a,'f,'p,td) lis

	val th      : ('x,'y,'a,'f,'p,'bi) elt -> ('x,'y,'a,'f,'p,td) lis
	val tha     : ('aa, 'b, A.align,'c, A.valign,'d, A.rowspan,'e, 
		       A.colspan,'ff, na,na, na,na) A.attr
	              -> ('x,'y,'a,'f,'p,'bi) elt -> ('x,'y,'a,'f,'p,td) lis
	val tr      : ('x,'y,'a,'f,'p,td) lis -> ('x,'y,'a,'f,'p,tr) lis

	val tra     : ('aa, 'b, A.align,'c, A.valign,'d, na,na, na,na, 
		       na,na, na,na) A.attr  -> ('x,'y,'a,'f,'p,td) lis 
                       -> ('x,'y,'a,'f,'p,tr) lis
	val table   : ('x,'y,'a,'f,'p,tr) lis -> ('x,'y,'a,'f,'p,block) elt

	val tablea  : ('aa, 'b, A.width,'c, A.border,'d, A.cellspacing,'e, 
		       A.cellpadding,'ff, A.frame,'g, A.rules,'h) A.attr
	              -> ('x,'y,'a,'f,'p,tr) lis -> ('x,'y,'a,'f,'p,block) elt

	(* Forms *)
	type ('x,'y,'a,'p,'bi) felt = ('x,'y,'a,inform,'p,'bi) elt
	type ('x,'a,'p,'bi) form = ('x,nil,'a,'p,'bi) felt

	val swap    : ('n1->'n2->'x,'y,'a,'p,'bi) felt 
                      -> ('n2->'n1->'x,'y,'a,'p,'bi) felt

	(* Input elements *)
	val inputText      : ('n,'t)fname -> 't obj option -> ('n->'x,'x,'a,'p,inline) felt
	val inputPassword  : ('n,'t)fname -> 't obj option -> ('n->'x,'x,'a,'p,inline) felt
	val inputRadio     : ('n,'t)fname -> 't obj -> ('n rad->'x,'x,'a,'p,inline) felt
	val inputRadio'    : ('n,'t)fname -> 't obj -> ('n rad->'x,'n rad->'x,'a,'p,inline) felt
	val radDrop        : ('n rad->'x,'y,'a,'p,inline) felt -> ('n->'x,'y,'a,'p,inline) felt
 	val inputHidden    : ('n,'t)fname -> 't obj -> ('n->'x,'y,'a,'p,inline) felt
 	val inputSubmit    : string -> ('x,'x,'a,'p,inline) felt
 	val inputReset     : string -> ('x,'x,'a,'p,inline) felt

	val textarea       : ('n,'t)fname -> {rows:int,cols:int} -> 't obj option 
                             -> ('n->'x,'x,'a,'p,inline) felt

	type 't select_option = {text: string, value: 't obj, 
				 selected: bool, disabled: bool}

	val option         : string * 't obj -> 't select_option
	val select         : ('n,'t)fname -> 't select_option list 
                             -> ('n->'x,'x,'a,'p,inline) felt

	(* Validate link *)
	val validLink : unit -> ('x,'x,'a,'f,'p,inline) elt

	(* Head elements *)
	type helt
	val script  : {typ:string} -> string -> helt
	val style   : {typ:string} -> string -> helt
	val meta    : {content:string} -> helt
	val link    : {typ:string,rel:string} -> helt

	type head
	val head    : string * helt list -> head

	(* HTML documents *)	
	type body
	val body    : (nil,nil,aclosed,formclosed,preclosed,block) elt -> body
	type html
	val html    : head * body -> html

	val ## : ('a1,'a2, 'b1,'b2, 'c,'c1,'c2, 'd,'d1,'d2, 
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
                    -> ('x,'a,'p,block)form 
                    -> (nil,nil,'a,formclosed,'p,block) elt

		val ahref : {src:string}
                    -> ('x,'y,ina,'f,'p,inline)elt 
                    -> ('x,'y,aclosed,'f,'p,inline)elt 

		val toString   : html -> string
		val urlencode  : string -> string
		val htmlencode : string -> string
	    end
    end

