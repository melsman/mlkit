(* Copyright 2007 Martin Elsman
 *
 * The LIN signature allows for only linear uses of identifiers in
 * XML, e.g. The signature ensures that each div-element (containing
 * an id) receives a unique type, which denotes the "position" of the
 * element in the XML tree. 
 *)

signature LIN =
  sig
    type 'c id
    val newId : unit -> 'c id

    type ('i,'o) x and 'a s and z

    val div   : 'o id -> ('i,'o) x -> ('i,'o s) x
    val &&    : ('i,'t) x * ('t, 'o) x -> ('i,'o) x
    val $     : string -> ('c,'c) x
    val print : (z,'o) x -> unit
  end

functor T (L : LIN) =
  struct 
    open L infix && nonfix div
    val _ = 
        let val id1 = newId()
            val id2 = newId()
        in print(div id1 ($"hello") && ($" world")
                 && div id2 ($" again"))
        end
  end
