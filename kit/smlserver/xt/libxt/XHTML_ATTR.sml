
signature XHTML_ATTR =
    sig

	(* The first two type parameter pairs are reserved for id and
	 style attributes, thus no ``attribute identification
	 variables'' ('a and 'b) are used for these. *)

	type ('a1,'a2, 'b1,'b2, 'c,'c1,'c2, 'd,'d1,'d2, 
	      'e,'e1,'e2, 'f,'f1,'f2, 'g,'g1,'g2, 'h,'h1,'h2) attr0

	(* Type representing absence of attribute *)
	type na

	type ('a2, 'b2, 'c,'c2, 'd,'d2, 'e,'e2, 'f,'f2, 
	      'g,'g2, 'h,'h2) attr =
	    (na,'a2, na,'b2, 'c,na,'c2, 'd,na,'d2, 
	     'e,na,'e2, 'f,na,'f2, 'g,na,'g2, 'h,na,'h2) attr0

	(* Concatenation of attributes *)
	val % : ('a1,'a2, 'b1,'b2, 'c,'c1,'c2, 'd,'d1,'d2, 
	      'e,'e1,'e2, 'f,'f1,'f2, 'g,'g1,'g2, 'h,'h1,'h2) attr0 *
	    ('a2,'a3, 'b2,'b3, 'c,'c2,'c3, 'd,'d2,'d3, 
	      'e,'e2,'e3, 'f,'f2,'f3, 'g,'g2,'g3, 'h,'h2,'h3) attr0 ->
	    ('a1,'a3, 'b1,'b3, 'c,'c1,'c3, 'd,'d1,'d3, 
	      'e,'e1,'e3, 'f,'f1,'f3, 'g,'g1,'g3, 'h,'h1,'h3) attr0

	(* Id *)
	type id
	val id : string -> (na,id, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 'e0,'e,'e,
			    'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	(* Style *)
	type style
	val style : string -> ('a,'a, na,style, 'c0,'c,'c, 'd0,'d,'d, 'e0,'e,'e,
			       'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	(* Alignment *)
	type align
	val left    : align
	val center  : align
	val right   : align
	val justify : align
	val char    : Char.char -> align
	val align   : align -> ('a,'a, 'b,'b, align,na,align, 'd0,'d,'d, 
				'e0,'e,'e, 'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	(* Vertical alignment *)
	type valign
	val top      : valign
	val middle   : valign
	val bottom   : valign
	val baseline : valign 
	val valign   : valign -> ('a,'a, 'b,'b, 'c0,'c,'c, valign,na,valign, 
				  'e0,'e,'e, 'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	(* Lengths *)
	type len
	val pct : int -> len
	val px  : int -> len

	(* Width attribute and height *)
	type width
	val width : len -> ('a,'a, 'b,'b, width,na,width, 'd0,'d,'d, 
			    'e0,'e,'e, 'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	type height
	val height : len -> ('a,'a, 'b,'b, 'c0,'c,'c, height,na,height, 
			     'e0,'e,'e, 'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	(* Checked attribute *)
	type checked
	val checked : unit -> ('a,'a, 'b,'b, checked,na,checked, 'd0,'d,'d, 
			       'e0,'e,'e, 'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	(* Disabled attribute *)
	type disabled
	val disabled : unit -> ('a,'a, 'b,'b, 'c0,'c,'c, disabled,na,disabled, 
				'e0,'e,'e, 'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	(* Readonly attribute *)
	type readonly
	val readonly : unit -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 
				readonly,na,readonly, 'f0,'f,'f, 'g0,'g,'g, 
				'h0,'h,'h) attr0

	(* Size attribute *)
	type size
	val size : int -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 'e0,'e,'e, 
			   size,na,size, 'g0,'g,'g, 'h0,'h,'h) attr0

	(* Selected attribute *)
	type selected
	val selected : unit -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 
				selected,na,selected, 'f0,'f,'f, 
				'g0,'g,'g, 'h0,'h,'h) attr0

	(* Tables *)
	type border
	val border : int -> ('a,'a, 'b,'b, 'c0,'c,'c, border,na,border, 
			     'e0,'e,'e, 'f0,'f,'f, 'g0,'g,'g, 'h0,'h,'h) attr0

	type cellspacing
	val cellspacing : int -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 
				  cellspacing,na,cellspacing, 'f0,'f,'f, 
				  'g0,'g,'g, 'h0,'h,'h) attr0

	type cellpadding
	val cellpadding : int -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 'e0,'e,'e, 
				  cellpadding,na,cellpadding, 'g0,'g,'g, 'h0,'h,'h) attr0

	type frame
	val void    : frame
	val above   : frame
	val below   : frame
	val hsides  : frame
	val lhs     : frame
	val rhs     : frame
	val vsides  : frame
	val box     : frame
	val fborder : frame
	val frame : frame -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 'e0,'e,'e, 
			      'f0,'f,'f, frame,na,frame, 'h0,'h,'h) attr0

	type rules
	val none   : rules
	val groups : rules
	val rows   : rules
	val cols   : rules
	val all    : rules
	val rules : rules -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 'e0,'e,'e, 
			      'f0,'f,'f, 'g0,'g,'g, rules,na,rules) attr0

	(* Table content *)
	type rowspan
	val rowspan : int -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 
			      rowspan,na,rowspan, 'f0,'f,'f, 'g0,'g,'g, 
			      'h0,'h,'h) attr0

	type colspan 
	val colspan : int -> ('a,'a, 'b,'b, 'c0,'c,'c, 'd0,'d,'d, 'e0,'e,'e, 
			      colspan,na,colspan, 'g0,'g,'g, 'h0,'h,'h) attr0
    end

(* Supported attributes:
all:
a   id                     ID
b   style                  CDATA

tr, th, td:
c   align                  left|center|right|justify|char
d   valign                 top|middle|bottom|baseline

img, table:
c   width                  %Length

img:           
d   height                 %Length

input:
c   checked                checked

input,select,option,textarea:
d   disabled               disabled

input,textarea:
e   readonly               readonly

input,select:
f   size                   %Number

option:
e   selected               selected

table:
d   border                 %Pixels
e   cellspacing            %Length
f   cellpadding            %Length
g   frame                  void|above|below|hsides|lhs|rhs|vsides|box|border
h   rules                  none|groups|rows|cols|all

th,td:
e   rowspan                %Number
f   colspan                %Number
*)
