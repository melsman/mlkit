signature SCS_BOX =
  sig
    datatype 'a box =
      H of 'a box list
    | V of 'a box list
    | C of 'a
    | BOX of 'a box

    val layout : ('a -> quot) -> 'a box -> quot
    
    val toQuot   : quot box -> quot
    val toString : quot box -> string
  end

structure ScsBox =
  struct
    datatype 'a box =
      H of 'a box list
    | V of 'a box list
    | C of 'a
    | BOX of 'a box

    fun layout pp box =
      let
        fun l box =
          case box of
            H bs => `<table><tr>` ^^ 
                       (List.foldr (fn (b,acc) => `<td>` ^^ (l b) ^^ `</td>` ^^ acc) 
                    `</tr></table>` bs)
          | V bs => `<table>` ^^ 
                       (List.foldr (fn (b,acc) => `<tr><td>` ^^ (l b) ^^ `</td></tr>` ^^ acc) 
                    `</table>` bs)
          | C c => pp c
          | BOX b => `<table border=1><tr><td>` ^^ (l b) ^^ `</td></tr></table>`
      in
        l box
      end

    fun toQuot box = layout (fn id => id) box
    fun toString box = Quot.toString (layout (fn id => id) box)
  end
