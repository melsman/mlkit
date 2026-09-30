structure ProfileReader =
struct
  open ProfileJson
  fun read path =
      let val input = BinIO.openIn path
          val bytes = (BinIO.inputAll input handle e => (BinIO.closeIn input; raise e))
          val () = BinIO.closeIn input
          val lines = String.fields (fn c => c = #"\n") (Byte.bytesToString bytes)
          val header = ref NONE
          val definitions = ref (Binarymap.mkDict IntInf.compare)
          val pending = ref NONE
          val regions = ref []
          val stacks = ref []
          val samples = ref []
          val marks = ref []
          val pagePeak = ref NONE
          val collections = ref NONE
          val complete = ref false
          fun noteCollections r =
              let val n = uint r "gc_collections"
              in collections := SOME(case !collections of NONE => n | SOME p => IntInf.max(p,n))
              end
          fun notePeak r =
              let val n = uint r "max_pages"
              in pagePeak := SOME(case !pagePeak of NONE => n | SOME p => IntInf.max(p,n))
              end
          fun require b msg = if b then () else raise Fail msg
          val staticKeys = ["unit","source","name","region_type","kind","binding"]
          fun define r =
              let val id = uint r "definition"
                  val () = require (not(Option.isSome(Binarymap.peek(!definitions,id)))) "duplicate binding definition"
                  val () = app (fn k => ignore(string(get r k))) ["unit","source","name","region_type"]
                  val () = ignore(uint r "binding")
                  val () = require (List.exists (fn k => string(get r "kind") = k) ["finite","infinite"]) "invalid region kind"
                  val metadata = map (fn k => (k,get r k)) staticKeys
              in definitions := Binarymap.insert(!definitions,id,metadata)
              end
          fun resolve r =
              let val id = uint r "definition"
                  val metadata = case Binarymap.peek(!definitions,id) of
                                     SOME m => m
                                   | NONE => raise Fail "unknown binding definition"
                  val () = require (not(List.exists (fn k => Option.isSome(find r k)) staticKeys)) "static metadata in region record"
              in Obj(fields r @ metadata)
              end
          fun checkRegion r =
              let val () = app (fn k => ignore(uint r k))
                      ["pages","unused_tail","page_footprint","large_bytes","finite_bytes","descriptor_bytes","thread","binding"]
                  val h = valOf(!header)
                  val () = require (uint r "page_footprint" = uint r "pages" * uint h "page_bytes" - uint r "unused_tail") "inconsistent page accounting"
                  val () = app (fn k => ignore(string(get r k))) ["unit","source","name","region_type"]
                  val () = require (List.exists (fn k => string(get r "kind") = k) ["finite","infinite"]) "invalid region kind"
              in app (fn k => ignore(uint r k)) ["g0_pages","g1_pages","g0_unused_tail","g1_unused_tail"];
                 require (uint r "pages" = uint r "g0_pages" + uint r "g1_pages" andalso
                          uint r "unused_tail" = uint r "g0_unused_tail" + uint r "g1_unused_tail") "inconsistent generation accounting"
              end
          fun checkStack r =
              (app (fn k => ignore(uint r k)) ["active_bytes","finite_bytes","stack_bytes","thread"];
               require (uint r "active_bytes" = uint r "finite_bytes" + uint r "stack_bytes") "inconsistent stack accounting";
               require (not(List.exists (fn s => uint s "thread" = uint r "thread") (!stacks))) "duplicate thread stack")
          fun add r =
              case !header of
                  NONE =>
                  (require (kind r = "header" andalso string(get r "format") = "mlkit-region-profile") "expected profile header";
                   require (uint r "version" = 4) "unsupported profile version (expected version 4)";
                   require (uint r "page_bytes" > 0) "invalid page size";
                   header := SOME r)
                | SOME h =>
                  (case kind r of
                       "binding" => define r
                     | "sample_begin" =>
                       (require (not(Option.isSome(!pending))) "nested samples";
                        ignore(uint r "sample"); ignore(uint r "time");
                        pending := SOME r; regions := []; stacks := [])
                     | "session_end" => (notePeak r; noteCollections r; complete := true)
                     | "mark" => (ignore(uint r "time"); marks := r :: !marks)
                     | k =>
                       if List.exists (fn t => t = k) ["region","stack","sample_end"] then
                         let val begin = case !pending of SOME s => s | NONE => raise Fail "record outside sample"
                             val () = require (uint r "sample" = uint begin "sample") "record outside its sample"
                         in if k = "region" then let val r = resolve r in checkRegion r; regions := r :: !regions end
                            else if k = "stack" then (checkStack r; stacks := r :: !stacks)
                            else
                              let val () = notePeak r
                                  val () = noteCollections r
                                  val () = require (not(null(!stacks))) "missing stack records"
                                  val () = app (fn key => ignore(uint r key)) ["time","frames","pages_visited"]
                                  val () = ignore(uint r "cache_bytes")
                                  val extra = [("end_time",get r "time"),("frames",get r "frames"),
                                               ("pages_visited",get r "pages_visited"),("cache_bytes",get r "cache_bytes"),
                                               ("page_bytes",get h "page_bytes"),("regions",Arr(rev(!regions))),("stacks",Arr(rev(!stacks)))]
                                  val fields = List.filter (fn (k,_) => not(List.exists (fn (n,_) => n = k) extra)) (fields begin)
                              in samples := Obj(fields @ extra) :: !samples; pending := NONE end
                         end
                       else require (List.exists (fn t => t = k)
                              ["thread_start","thread_end","session_end","sample_skipped"]) ("unknown record: " ^ k))
          (* The final split field is either empty or an uncommitted record. *)
          fun consume [] = ()
            | consume [_] = ()
            | consume (line::rest) = (add(parse line); consume rest)
          val () = consume lines
          val () = require (Option.isSome(!header)) "missing profile header"
          fun partition _ [] acc = (rev acc,[])
            | partition time (m::ms) acc = if uint m "time" <= time then partition time ms (m::acc)
                                          else (rev acc,m::ms)
          fun attach [] _ = []
            | attach [s] ms = [Obj(fields s @ [("marks",Arr ms)])]
            | attach (s::ss) ms =
              let val (here,later) = partition (uint s "time") ms []
              in Obj(fields s @ [("marks",Arr here)]) :: attach ss later end
          val result = attach (rev(!samples)) (rev(!marks))
          val result = case !pagePeak of
                           NONE => result
                         | SOME p => map (fn s => Obj(fields s @ [("max_pages",Num(IntInf.toString p))])) result
          val h = valOf(!header)
          val source = get h "main_source"
          val () = ignore(string source)
          val gc = case get h "gc_enabled" of
                       Bool b => Bool b
                     | _ => raise Fail "invalid GC enabled flag"
          val metadata = Obj[("main_source",source),("gc_enabled",gc),
                             ("gc_collections",case !collections of NONE => Null | SOME n => Num(IntInf.toString n)),
                             ("complete",Bool(!complete))]
      in {samples=result,metadata=metadata}
      end

end
