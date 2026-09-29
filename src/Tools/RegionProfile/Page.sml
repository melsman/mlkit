structure ProfilePage =
struct
  fun replace (text,marker,value) =
      let val (left,right) = Substring.position marker (Substring.full text)
      in if Substring.isEmpty right then text
         else Substring.string left ^ value ^ Substring.string(Substring.triml (size marker) right)
      end
  fun html {samples,metadata} =
      let val (left,right) = Substring.position "__DATA__" (Substring.full ProfileHtml.template)
          val data = ProfileJson.encode true (ProfileJson.Arr samples)
          val rest = Substring.string(Substring.triml 8 right)
      in Substring.string left ^ data ^ replace(rest,"__META__",ProfileJson.encode true metadata)
      end
end
