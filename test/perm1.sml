fun composePartial (f, g) x = 
    case g x of 
      NONE => NONE
    | SOME y => f y
