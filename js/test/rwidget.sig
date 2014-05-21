signature RWIDGET = sig  
  type 'a b = 'a RWP.b
  type blk = RHtml.blk 
  type inl = RHtml.inl
  type attrs = RHtml.attrs
  type color = Color.t

  val box : color b -> blk -> blk
  val pad : int b -> blk -> blk
                                  
  val textField : attrs -> string b * inl

  val map : (''a b -> ''b b) -> ''a list b -> ''b list b
  val fold : (''a b -> ''b b) -> (''b b * ''b b -> ''b b) -> ''b b -> ''a list b -> ''b b

  val mouseOver : inl -> bool b * inl
(*                                 
  val selectBox : (string * string) list -> string b * RHtml.blk  (* head is default *)
                                            
  val mouseOver : RHtml.blk * RHtml.blk -> RHtml.blk
                                           
  val mapConcat : (''a b -> ''b elm b) -> ''b elm b -> ''a list b -> ''b elm b
*)
end
