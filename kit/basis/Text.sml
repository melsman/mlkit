structure Text :> TEXT =
  struct
    structure Char = Char
    structure String = String
    structure Substring = Substring
    structure CharVector = CharVector
    structure CharArray = CharArray
    structure CharVectorSlice = CharVectorSlice
    structure CharArraySlice = CharArraySlice
  end
