structure ProfileText =
struct
  open ProfileJson
  fun shortFunction name =
    if size name > 23 andalso String.sub(name,size name-23) = #"_" andalso
       List.all (fn c => String.isSubstring (str c)
         "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789")
         (explode(String.extract(name,size name-22,NONE)))
    then String.substring(name,0,size name-23) else name
  fun render {samples,metadata} =
    let
      val session = get metadata "time_session"
      val () = if session = Null then raise Fail "text overview requires time profiling data" else ()
      val records = ProfileCode.rows metadata "time_samples"
      val attribute = ProfilePage.timeAttribution metadata
      fun field r k = case find r k of SOME (Str s) => s | SOME (Num s) => s | _ => ""
      fun label a =
        case field a "status" of
            "function" => shortFunction(field a "function") ^ " [" ^
              (if field a "source" = "<runtime>" then field a "unit"
               else field a "source") ^ "]"
          | "gc" => "GC"
          | "allocation" => "Allocation"
          | "reset" => "Reset"
          | "recorder" => "Recorder drain"
          | "unknown-pc" => "Unresolved PC / runtime"
          | "metadata-unavailable" => "Code metadata unavailable"
          | other => other
      fun add (r,dict) =
        let
          val a = get (attribute r) "attribution"
          val key = if field a "status" = "function" then
                      encodeJson (Arr [get a "unit",get a "function"]) else field a "status"
          val (name,count) = case Binarymap.peek(dict,key) of
              SOME pair => pair | NONE => (label a,0 : IntInf.int)
        in Binarymap.insert(dict,key,(name,count+1))
        end
      val groups = map #2 (Binarymap.listItems (foldl add (Binarymap.mkDict String.compare) records))
      val groups = ProfileSvg.sort (fn ((a,n),(b,m)) => n > m orelse (n = m andalso a < b)) groups
      val total = IntInf.fromInt (length records)
      fun fixed scale n =
        let val whole = IntInf.toString(n div scale)
            val fraction = IntInf.toString(n mod scale)
            val digits = size(IntInf.toString scale)-1
        in whole ^ "." ^ StringCvt.padLeft #"0" digits fraction
        end
      fun ms n = fixed 1000 (n div 1000) ^ " ms"
      val interval = uint session "interval_ns"
      fun row (name,count) =
        StringCvt.padLeft #" " 12 (IntInf.toString count) ^ "  " ^
        StringCvt.padLeft #" " 8 (fixed 100 ((count*10000+total div 2) div total) ^ "%") ^ "  " ^
        StringCvt.padLeft #" " 16 (ms(count*interval)) ^ "  " ^ name ^ "\n"
      val status = get metadata "time_status"
      val complete = find metadata "complete" = SOME (Bool true) andalso
                     status <> Null andalso uint status "final" = 1
      val elapsed = foldl (fn (r,n) => IntInf.max(uint r "time",n))
                      (if status <> Null then uint status "time" else 0) records
      val threaded = uint session "time_version" = 3
      val loss = if status = Null then "Loss counters unavailable\n" else
                   "Session-wide loss: buffer " ^ field status "dropped" ^
                   ", routing " ^ field status "routing_dropped" ^ "\n"
    in "MLKit time profile\n" ^
       "Samples: " ^ IntInf.toString total ^ " | Interval: " ^ ms interval ^
       " | Elapsed: " ^ ms elapsed ^ " | " ^ (if complete then "complete" else "INCOMPLETE") ^ "\n" ^
       loss ^ "\n     Samples     Share        Time share  Function / category\n" ^
       (if total = 0 then "No recorded time samples.\n" else String.concat(map row groups)) ^
       "\nShares include GC and unresolved samples. Estimates = samples * requested interval;\n" ^
       "includes blocking, excludes machine sleep; not measured CPU time or corrected for loss.\n" ^
       (if threaded then "Aggregated thread wall estimates may exceed elapsed session time.\n" else "") ^
       "Timer coalescing is unobservable; draining blocks delivery.\n"
    end
end
