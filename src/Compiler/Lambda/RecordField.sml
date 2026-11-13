structure RecordField : RECORD_FIELD =
struct
  type mode = {mutable: bool, traversable: bool}
  val ordinary = {mutable = false, traversable = true}
  val mutable = {mutable = true, traversable = true}
  val raw = {mutable = false, traversable = false}
  fun validPrefix fields =
      let fun loop (_, []) = true
            | loop (seen, ({traversable, ...}:mode) :: rest) =
              (traversable orelse not seen) andalso loop (seen orelse traversable, rest)
      in loop (false, fields)
      end
  fun rawPrefix [] = 0
    | rawPrefix (({traversable = true, ...}:mode) :: _) = 0
    | rawPrefix (_ :: rest) = 1 + rawPrefix rest
  fun layout ({mutable, traversable}:mode) =
      (if mutable then "var " else "") ^ (if traversable then "" else "raw ")
  val pu = Pickle.convert
             (fn (m, t) => {mutable = m, traversable = t},
              fn {mutable, traversable} => (mutable, traversable))
             (Pickle.pairGen (Pickle.bool, Pickle.bool))
end
