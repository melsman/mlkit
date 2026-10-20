fun copy [] = []
  | copy (x::xs) = x :: copy xs
