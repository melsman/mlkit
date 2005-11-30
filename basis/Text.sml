structure Text :> TEXT
(*  where type Char.char = Char.char
  where type String.string = String.string
  where type Substring.substring = Substring.substring
  where type CharVector.vector = CharVector.vector
  where type CharArray.array = CharArray.array
  where type CharVectorSlice.slice = CharVectorSlice.slice
  where type CharArraySlice.slice = CharArraySlice.slice*) =

  struct
    structure Char = Char
    structure String = String
    structure Substring = Substring
    structure CharVector = CharVector
    structure CharArray = CharArray
    structure CharVectorSlice = CharVectorSlice
    structure CharArraySlice = CharArraySlice
  end
