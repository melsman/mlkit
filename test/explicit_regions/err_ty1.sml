(* An effect variable (beginning with 'e') cannot be used as a region variable *)

val x =
    let region e
        val a : int #e->string = fn x => "hi"
    in (a 3) : string`e
    end
