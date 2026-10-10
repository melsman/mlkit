structure ProfilePage =
struct
  fun replace (text,marker,value) =
      let val (left,right) = Substring.position marker (Substring.full text)
      in if Substring.isEmpty right then text
         else Substring.string left ^ value ^ Substring.string(Substring.triml (size marker) right)
      end
  fun htmlWith options {samples,metadata} =
      let val (left,right) = Substring.position "__DATA__" (Substring.full ProfileHtml.template)
          val resolve = ProfileCode.resolver metadata
          fun attributed r =
              let
                val state = ProfileJson.uint r "state"
                fun category name = ProfileJson.Obj[("status",ProfileJson.Str name)]
                val raw = resolve {pc=ProfileJson.uint r "pc",buildId=NONE}
                val attribution =
                    if state = 1 then category "recorder"
                    else if state = 3 then category "gc"
                    else if ProfileJson.find raw "status" = SOME(ProfileJson.Str "function") then raw
                    else if state = 2 then resolve {pc=ProfileJson.uint r "origin_pc",buildId=NONE}
                    else raw
              in ProfileJson.Obj(ProfileJson.fields r @ [("attribution",attribution)])
              end
          val metadata = ProfileJson.Obj(map (fn (key,value) =>
              if key = "time_samples" then (key,ProfileJson.Arr(map attributed (ProfileCode.rows metadata key)))
              else (key,value)) (ProfileJson.fields metadata))
          val data = ProfileJson.encode true (ProfileJson.Arr samples)
          val rest = Substring.string(Substring.triml 8 right)
          val (middle,tail) = Substring.position "__META__" (Substring.full rest)
          val suffix = replace(Substring.string(Substring.triml 8 tail),"__OPTIONS__",ProfileJson.encode false options)
      in Substring.string left ^ data ^ Substring.string middle ^ ProfileJson.encode true metadata ^ suffix
      end
end
