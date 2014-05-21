structure XHtmlAttr : XHTML_ATTR  =
    struct
	type ('a1,'a2, 'b1,'b2, 'c,'c1,'c2, 'd,'d1,'d2, 
	      'e,'e1,'e2, 'f,'f1,'f2, 'g,'g1,'g2, 'h,'h1,'h2) attr0 = string list

	type ('a2, 'b2, 'c,'c2, 'd,'d2, 'e,'e2, 'f,'f2, 
	      'g,'g2, 'h,'h2) attr = string list

	fun % (a,b) = a @ b

	(* Type representing absence of attribute *)
	type na = unit

	fun attr a s = [a ^ "=\"" ^ s ^ "\""]

	(* Id *)
	type id = unit
	fun id s = attr "id" s

	(* Style *)
	type style = unit
	fun style s = attr "style" s	    

	(* Alignment *)
	type align = string list
	val left     = attr "align" "left"
	val center   = attr "align" "center"
	val right    = attr "align" "right"
	val justify  = attr "align" "justify"
	fun char c   = attr "align" "char" @ attr "char" (String.str c)
	fun align s = s

	(* Vertical alignment *)
	type valign = string
	val top      = "top"
	val middle   = "middle"
	val bottom   = "bottom"
	val baseline = "baseline"
	fun valign s = attr "valign" s

	(* Char attribute *)
	type char = unit
	fun char c = attr "char" (String.str c)

	(* Charoff attribute *)
	type charoff = unit
	fun charoff i = attr "charoff" (Int.toString i)

	(* Lengths *)
	type len = string
	fun pct i = Int.toString i ^ "%"
	fun px i = Int.toString i

	(* Width attribute and height *)
	type width = unit
	fun width len = attr "width" len

	type height = unit
	fun height len = attr "height" len

	fun mattr s = attr s s

	(* Checked attribute *)
	type checked = unit
	fun checked () = mattr "checked"

	(* Disabled attribute *)
	type disabled  = unit
	fun disabled () = mattr "disabled"
	    
	(* Readonly attribute *)
	type readonly = unit
	fun readonly () = mattr "readonly"
	    
	(* Size attribute *)
	type size = unit
	fun size i = attr "size" (Int.toString i)	    

	(* Selected attribute *)
	type selected = unit
	fun selected () = mattr "selected"
	    
	(* Tables *)
	type border = unit
	fun border i = attr "border" (Int.toString i)

	type cellspacing = unit
	fun cellspacing i = attr "cellspacing" (Int.toString i)

	type cellpadding = unit
	fun cellpadding i = attr "cellpadding" (Int.toString i)

	type frame = string
	val void    = "void"
	val above   = "above"
	val below   = "below"
	val hsides  = "hsides"
	val lhs     = "lhs"
	val rhs     = "rhs"
	val vsides  = "vsides"
	val box     = "box"
	val fborder = "border"
	fun frame s = attr "frame" s

	type rules = string
	val none   = "none"
	val groups = "groups"
	val rows   = "rows"
	val cols   = "cols"
	val all    = "all"
	fun rules s = attr "rules" s
	    
	type rowspan = unit
	fun rowspan i = attr "rowspan" (Int.toString i)

	type colspan = unit
	fun colspan i = attr "colspan" (Int.toString i)
    end

(*
structure XHtmlAttrTest =
    struct
	local
	    structure A :> XHTML_ATTR = XHtmlAttr
	    open A
	    infix %
	in
	    val test1 = fn() => valign middle % width (pct 5)
	    val test2 = fn() => valign middle
	end
    end
*)