structure S :> sig type t end =
  struct
      datatype s = S of char
      datatype t = T of s
  end

open S