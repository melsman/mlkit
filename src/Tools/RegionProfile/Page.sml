structure ProfilePage =
struct
  fun replace (text,marker,value) =
      let val (left,right) = Substring.position marker (Substring.full text)
      in if Substring.isEmpty right then text
         else Substring.string left ^ value ^ Substring.string(Substring.triml (size marker) right)
      end
  fun html samples =
      replace(ProfileHtml.template,"__DATA__",ProfileJson.encode true (ProfileJson.Arr samples))
end
