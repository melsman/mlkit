signature TDOM =
sig 
  type 'a b = 'a RWP.b          (* behavior *)
  eqtype 'f elm0
  type 'f elm = 'f elm0 b
  eqtype Inl and Blk and 'f Flw and Li and Dl and Td and Tr and Bdy and Htm
  type 'f flw = 'f Flw elm
  type inl = Inl flw
  type blk = Blk flw
  type li = Li elm
  type dl = Dl elm
  type td = Td elm
  type tr = Tr elm

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
  val acronym    : inl -> inl             (* acronym *)
  val sub        : inl -> inl             (* subscript *)
  val sup        : inl -> inl             (* superscript *)
  val tt         : inl -> inl             (* fixed pitch *)
  val i          : inl -> inl             (* italic *)
  val b          : inl -> inl             (* bold *)
  val big        : inl -> inl             (* bigger *)
  val small      : inl -> inl             (* smaller *)
  val span       : inl -> inl
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
  val pre        : inl -> blk
  val hr         : blk

  val &          : 'f elm * 'f elm -> 'f elm 

  (* Lists *)
  val li         : 'f flw -> li
  val dt         : inl -> dl
  val dd         : 'f flw -> dl
  val ol         : li -> blk
  val ul         : li -> blk
  val dl         : dl -> blk

  (* Tables *)
  val td         : 'f flw -> td
  val th         : 'f flw -> td
  val tr         : td -> tr
  val table      : tr -> blk

  (* attributes *)
  datatype attr  = A of string * string b  
                 | S of string * string b  (* style *)
  type attrs     = attr list

  val pa         : attrs -> inl -> blk

  val lia        : attrs -> 'f flw -> li
  val dta        : attrs -> inl -> dl
  val dda        : attrs -> 'f flw -> dl
  val dla        : attrs -> dl -> blk
  val ula        : attrs -> li -> blk
  val ola        : attrs -> li -> blk

  val diva       : attrs -> 'f flw -> blk
  val spana      : attrs -> inl -> inl

  val abbr       : string b -> inl -> inl (* abbreviation *)
  val imga       : attrs -> inl           (* image *)
  val tda        : attrs -> 'f flw -> td
  val tha        : attrs -> 'f flw -> td
  val tra        : attrs -> td -> tr
  val tablea     : attrs -> tr -> blk

  (* Fields *)
  val inputa     : attrs -> inl
  val textareaa  : attrs -> inl -> inl

  val body       : blk -> Bdy elm
  val bodya      : attrs -> blk -> Bdy elm

  val html       : string b * Bdy elm -> Htm elm

  val install    : Htm elm -> unit

(*
  val list       : (''a * 'b elm -> 'b elm) -> 'b elm -> ''a list b -> 'b elm
  val insertDOM  : string -> 'f elm -> unit
*)
end
