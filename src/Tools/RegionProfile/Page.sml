structure ProfilePage =
struct
  fun replace (text,marker,value) =
      let val (left,right) = Substring.position marker (Substring.full text)
      in if Substring.isEmpty right then text
         else Substring.string left ^ value ^ Substring.string(Substring.triml (size marker) right)
      end
  fun timeAttribution metadata =
    let
      val resolve = ProfileCode.resolver metadata
      fun attributed r =
          let
            val state = ProfileJson.uint r "state"
            fun category name = ProfileJson.Obj[("status",ProfileJson.Str name)]
            val raw = resolve {pc=ProfileJson.uint r "pc",buildId=NONE}
            val runtime = ProfileJson.find raw "source" = SOME(ProfileJson.Str "<runtime>")
            val function = case ProfileJson.find raw "function" of
                             SOME(ProfileJson.Str name) => name | _ => ""
            val shared = if String.isPrefix "<gc-stub>" function orelse runtime andalso function = "gc" then SOME "gc"
                         else if runtime andalso List.exists (fn name => name = function)
                           ["mlkit_arm64_allocate_preserving","mlkit_arm64_allocate_untagged_preserving","mlkit_arm64_alloc","__allocate","__allocate_unprotected"] then SOME "allocation"
                         else if runtime andalso List.exists (fn name => name = function)
                           ["mlkit_arm64_reset_preserving","mlkit_arm64_reset","resetRegion","resetRegionProf","__reset_region"] then SOME "reset"
                         else NONE
            val attribution =
                if state = 1 then category "recorder"
                else if state = 3 then category "gc"
                else if Option.isSome shared then category(valOf shared)
                else if ProfileJson.find raw "status" = SOME(ProfileJson.Str "function") then raw
                else if state = 2 then resolve {pc=ProfileJson.uint r "origin_pc",buildId=NONE}
                else raw
          in ProfileJson.Obj(ProfileJson.fields r @ [("attribution",attribution)])
          end
    in attributed
    end
  fun htmlWith options {samples,metadata} =
      let val (left,right) = Substring.position "__DATA__" (Substring.full ProfileHtml.template)
          val attributed = timeAttribution metadata
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
