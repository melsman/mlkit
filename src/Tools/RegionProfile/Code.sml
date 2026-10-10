(* Offline interrupted-PC resolution. All address arithmetic stays in IntInf;
 * JavaScript numbers and host-sized SML integers cannot represent every PC. *)
structure ProfileCode =
struct
  open ProfileJson
  val addressLimit = 18446744073709551616 : IntInf.int
  fun require true _ = ()
    | require false message = raise Fail message
  fun rows metadata key =
    case find metadata key of SOME(Arr xs) => xs | NONE => []
                           | _ => raise Fail ("invalid " ^ key)
  fun resolver metadata =
    let
      val session = case find metadata "code_metadata" of SOME x => x | NONE => Null
      val images = rows metadata "code_images"
      val functions = rows metadata "code_functions"
      val scope = if session = Null then "unavailable" else string(get session "scope")
      val () = if session = Null then () else
        require (uint session "metadata_version" = 1) "unsupported code metadata version"
      val supported = scope = "static-executable"
      val expected = if session = Null then 0 else uint session "function_count"
      val () = require (IntInf.fromInt(length functions) <= expected) "extra function metadata"
      val available = supported andalso expected = IntInf.fromInt(length functions) andalso not(null images)
      val () = require (available orelse not supported orelse find metadata "complete" <> SOME(Bool true))
        "incomplete function metadata in completed profile"
      val () = require (List.exists (fn s => s = scope)
        ["static-executable","unavailable","unsupported-repl","unsupported-platform","missing-image-id"])
        "invalid code metadata scope"
      val () = require (supported orelse (null images andalso null functions andalso expected = 0))
        "code records outside supported metadata session"
      val imageMap = ref (Binarymap.mkDict IntInf.compare)
      fun image r =
        let val id = uint r "image"
            val build = string(get r "build_id")
            val () = require (size build = 32 andalso List.all
              (fn c => Char.isDigit c orelse (c >= #"a" andalso c <= #"f")) (explode build))
              "invalid Mach-O UUID"
            val () = ignore(string(get r "path"))
        in require (uint r "load_address" < addressLimit) "image address overflow";
           require (not(Option.isSome(Binarymap.peek(!imageMap,id)))) "duplicate code image";
           imageMap := Binarymap.insert(!imageMap,id,r)
        end
      val () = app image images
      val ranges = ref (Binarymap.mkDict IntInf.compare)
      fun identityOrder ((u,f),(u',f')) =
        case String.compare(u,u') of EQUAL => String.compare(f,f') | order => order
      val identities = ref (Binarymap.mkDict identityOrder)
      fun function r =
        let val image = case Binarymap.peek(!imageMap,uint r "image") of
                          SOME image => image | NONE => raise Fail "unknown code image"
            val start = uint r "start"
            val finish = uint r "end"
            val base = uint image "load_address"
            val identity = (string(get r "unit"),string(get r "function"))
            val () = app (fn key => ignore(string(get r key))) ["source","ir_identity"]
        in require (start < finish andalso base+finish <= addressLimit) "invalid function range";
           require (not(Option.isSome(Binarymap.peek(!ranges,base+start)))) "duplicate function start";
           require (not(Option.isSome(Binarymap.peek(!identities,identity)))) "duplicate function identity";
           identities := Binarymap.insert(!identities,identity,());
           ranges := Binarymap.insert(!ranges,base+start,(base+start,base+finish,r,image))
        end
      val () = app function functions
      val sorted = map #2 (Binarymap.listItems(!ranges))
      fun disjoint [] = ()
        | disjoint [_] = ()
        | disjoint ((_,finish,_,_) :: (rest as (start,_,_,_)::_)) =
          (require (finish <= start) "overlapping function ranges"; disjoint rest)
      val () = disjoint sorted
      val ranges = Vector.fromList sorted
      fun status value = Obj[("status",Str value)]
      fun resolve {pc,buildId} =
        let
          fun matches image = case buildId of NONE => true
                            | SOME build => build = string(get image "build_id")
          (* Find the last start <= PC. End points belong to a subsequent
           * function only when that function begins at exactly that address. *)
          fun search (low,high) =
            if low >= high then low-1
            else let val middle = low+(high-low) div 2
                     val (start,_,_,_) = Vector.sub(ranges,middle)
                 in if start <= pc then search(middle+1,high) else search(low,middle)
                 end
          val i = search(0,Vector.length ranges)
        in
          if not available then status "metadata-unavailable"
          else if not(List.exists matches images) then status "build-mismatch"
          else if i < 0 then status "unknown-pc"
          else
            let val (_,finish,r,image) = Vector.sub(ranges,i)
            in if pc >= finish then status "unknown-pc"
               else if not(matches image) then status "build-mismatch"
               else Obj(("status",Str "function") :: fields r @
                        [("build_id",get image "build_id"),("image_path",get image "path")])
            end
        end
    in resolve
    end
end
