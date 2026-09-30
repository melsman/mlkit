structure ProfilePage =
struct
  fun replace (text,marker,value) =
      let val (left,right) = Substring.position marker (Substring.full text)
      in if Substring.isEmpty right then text
         else Substring.string left ^ value ^ Substring.string(Substring.triml (size marker) right)
      end
  fun htmlWith options {samples,metadata} =
      let val (left,right) = Substring.position "__DATA__" (Substring.full ProfileHtml.template)
          val data = ProfileJson.encode true (ProfileJson.Arr samples)
          val rest = Substring.string(Substring.triml 8 right)
          val (middle,tail) = Substring.position "__META__" (Substring.full rest)
          val suffix = replace(Substring.string(Substring.triml 8 tail),"__OPTIONS__",ProfileJson.encode false options)
      in Substring.string left ^ data ^ Substring.string middle ^ ProfileJson.encode true metadata ^ suffix
      end
end
