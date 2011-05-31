signature TDOM =
sig 
  type B = TimeVal.B
  type ('a,'k)t = ('a,'k)TimeVal.t
  type 'a b = ('a,B)t          (* behavior *)
  type 'f ast and Inl and Blk and 'f Flw and Li and Dl and Td and Tr (* flow, block, inline, li, dl *)
  type 'f flw = 'f Flw ast b
  type inl = Inl flw
  type blk = Blk flw
  type li = Li ast b
  type dl = Dl ast b
  type td = Td ast b
  type tr = Tr ast b

  val insertDOM : string -> 'f ast b -> unit

  (* attributes *)
  type attrs     = (string * string)list
  val Attrs      : (string * string b)list -> attrs b

  val $          : string b -> 'f flw
  val br         : unit -> 'f flw

  val em         : inl -> inl             (* emphasis *)
  val strong     : inl -> inl             (* strong emphasis *)
  val dfn        : inl -> inl             (* definitional *)
  val code       : inl -> inl             (* program code *)
  val samp       : inl -> inl             (* sample *)
  val kbd        : inl -> inl             (* user input *)
  val var        : inl -> inl             (* variable *)
  val cite       : inl -> inl             (* citation *)
  val abbr       : string b -> inl -> inl (* abbreviation *)
  val acronym    : inl -> inl             (* acronym *)
  val sub        : inl -> inl             (* subscript *)
  val sup        : inl -> inl             (* superscript *)
  val tt         : inl -> inl             (* fixed pitch *)
  val i          : inl -> inl             (* italic *)
  val b          : inl -> inl             (* bold *)
  val big        : inl -> inl             (* bigger *)
  val small      : inl -> inl             (* smaller *)

  val p          : inl -> blk
  val h1         : inl -> blk             (* most important *)
  val h2         : inl -> blk
  val h3         : inl -> blk
  val h4         : inl -> blk
  val h5         : inl -> blk
  val h6         : inl -> blk             (* least important *)
                
  val div        : 'f flw -> blk

  val address    : inl -> blk
  val blockquote : blk -> blk
  val pre        : inl -> blk             (* disallow big, small, *)
	                               (*   sub, sup, and img. *)

  val hr         : blk

  val &          : 'f ast b * 'f ast b -> 'f ast b

  (* Lists *)
  val li         : 'f flw -> li
  val dt         : inl -> dl
  val dd         : 'f flw -> dl
  val ol         : li -> blk
  val ul         : li -> blk
  val dl         : dl -> blk

  (* Images *)
  val imga       : attrs b -> inl

  (* Tables *)
  val td         : 'f flw -> td
  val tda        : attrs b -> 'f flw -> td

  val th         : 'f flw -> td
  val tha        : attrs b -> 'f flw -> td

  val tr         : td -> tr
  val tra        : attrs b -> td -> tr

  val table      : tr -> blk
  val tablea     : attrs b -> tr -> blk

  (* Fields *)
  val inputa     : attrs b -> inl
  val textareaa  : attrs b -> inl -> inl

  type Bdy
  type bdy       = Bdy ast b 
  val body       : blk -> bdy 

  type Htm
  type htm       = Htm ast b
  val html       : string b * bdy -> htm

end
