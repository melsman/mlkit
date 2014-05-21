structure RWidget :> RWIDGET = struct
  type 'a b = 'a RWP.b
  type blk = RHtml.blk 
  type inl = RHtml.inl
  type attrs = RHtml.attrs
  type color = Color.t

  open RWP RHtml

  fun toPx i = Int.toString i ^ "px"

  fun box c b =
    diva [S("float", const "left"),S("background", arr Color.toString c)]
    (diva [S("background", const "url(ul.gif) no-repeat top left")]
     (diva [S("background", const "url(ur.gif) no-repeat top right")]
      (diva [S("background", const "url(ll.gif) no-repeat bottom left")] 
       (diva [S("background", const "url(lr.gif) no-repeat bottom right")] 
         b))))

  fun pad (i : int b) b =
      diva [S("padding", arr (fn x => let val p = toPx x
                                      in String.concat[p," ",p," ",p," ",p]
                                      end) i)]
           b

  fun textField (attrs: attrs) : string b * inl =
      let val b = const "" 
          fun handler e = true before send b (Js.value e)
          val attrs = E(Js.onkeyup, handler)::attrs
          val f = inputa attrs
      in (b,f)
      end

  fun map (f: ''a b -> ''b b) (l:''a list b) : ''b list b =
      let fun g (xs : ''a list) : ''b b list = List.map (f o const) xs
      in flatten(arr (list o g) l)
      end

  infix &
  fun fold (f: ''a b -> ''b b) (op & : ''b b * ''b b -> ''b b) (e:''b b) (l: ''a list b) : ''b b =
      let fun g (nil : ''a list) : ''b b = e
            | g (y::xs) = 
              List.foldl (fn (x,a) => f(const x) & a) (f(const y)) xs
      in flatten(arr g l)
      end

  fun mouseOver (e:inl) : bool b * inl =
      let val b : bool b = const false
      in (b, spana [E(Js.onmouseover, fn _ => true before send b true),
                    E(Js.onmouseout,  fn _ => true before send b false)] e)
      end

(*                                 
  val selectBox : (string * string) list -> string b * RHtml.blk  (* head is default *)
                                            
  val mouseOver : RHtml.blk * RHtml.blk -> RHtml.blk
                                           
  val mapConcat : (''a b -> ''b elm b) -> ''b elm b -> ''a list b -> ''b elm b
*)
end
