(* Standalone vector rendering: no browser or external converter is required. *)
structure ProfileSvg =
struct
  open ProfileJson
  val palette = ["#2563eb","#ea580c","#16a34a","#c026d3","#0891b2","#dc2626",
                 "#854d0e","#7c3aed","#65a30d","#db2777","#0d9488","#ca8a04"]
  fun escape s = String.translate (fn #"&" => "&amp;" | #"<" => "&lt;" | #">" => "&gt;"
                                   | #"\"" => "&quot;" | c => str c) s
  fun strField r k = case find r k of SOME (Str s) => s | SOME (Num s) => s | _ => ""
  fun number r k = case find r k of SOME (Num s) => valOf(IntInf.fromString s) | _ => 0
  fun list r k = case find r k of SOME (Arr xs) => xs | _ => []
  fun real n = valOf(Real.fromString(IntInf.toString n))
  fun fmt x = String.translate (fn #"~" => "-" | c => str c) (Real.fmt (StringCvt.FIX (SOME 2)) x)
  fun base s = List.last (""::String.tokens (fn c => c = #"/" orelse c = #"\\") s)
  fun key r = encode false (Arr [get r "unit",Str(strField r "binding")])
  fun sort less xs =
      let fun insert x [] = [x]
            | insert x (y::ys) = if less(x,y) then x::y::ys else y::insert x ys
      in foldl (fn (x,acc) => insert x acc) [] xs
      end
  fun unique xs = foldl (fn (x,acc) => if List.exists (fn y => x = y) acc then acc else x::acc) [] xs
  fun render options {samples,metadata} =
      let
        fun opt k d = case find options k of SOME (Str s) => s | SOME (Num s) => s | _ => d
        fun flag k d = case find options k of SOME (Bool b) => b | _ => d
        val metric = opt "metric" "total"
        val scope = opt "scope" "all"
        val limit = valOf(Int.fromString(opt "limit" "9"))
        val compact = flag "legend-right" true
        fun selected r = case String.fields (fn c => c = #":") scope of
                            ["all"] => true
                          | [field,value] => (case find r field of NONE => value = "-1" | _ => strField r field = value)
                          | _ => false
        fun bytes r = if metric = "total" then number r "page_footprint" + number r "large_bytes" + number r "finite_bytes"
                      else if metric = "pages" then number r "page_footprint" + number r "unused_tail"
                      else number r metric
        val allRegions = List.concat(map (fn s => list s "regions") samples)
        val keys = sort (op <) (unique(map key allRegions))
        fun weight k = foldl (fn (r,n) => if key r = k then n + number r "page_footprint" + number r "large_bytes" + number r "finite_bytes" else n) 0 allRegions
        val weights = map (fn k => (k,weight k)) keys
        val colorKeys = map #1 (sort (fn ((a,w),(b,v)) => w > v orelse (w = v andalso a < b)) weights)
        fun color k =
            if k = "other" then "#a8a29e" else if k = "stack" then "#64748b"
            else
              let fun index [] _ = 0
                    | index (x::xs) i = if x = k then i else index xs (i+1)
              in List.nth(palette,index colorKeys 0 mod length palette)
              end
        fun label r =
            let val name = strField r "name"
                val source = strField r "source"
                val unit = strField r "unit"
                val basename = if unit = "<global>" then "global" else base source
                fun info field fallback = case strField r field of "" => fallback | s => s
                val details = (if flag "show-kind" false then [info "kind" "kind unavailable"] else []) @
                              (if flag "show-type" false then [info "region_type" "type unavailable"] else [])
            in (if compact then (if name = "" then "" else name ^ " · ") ^ "r" ^ strField r "binding"
                else (if name = "" then "Region" else name) ^ " #" ^ strField r "binding") ^
               (if flag "show-base" false then " · " ^ basename else "") ^
               (if null details then "" else " (" ^ String.concatWith ", " details ^ ")")
            end
        fun values k = map (fn s => foldl (fn (r,n) => if selected r andalso key r = k then n + bytes r else n) 0 (list s "regions")) samples
        fun sum xs = foldl (op +) (0:IntInf.int) xs
        val regionKeys = List.filter (fn k => List.exists (fn r => selected r andalso key r = k) allRegions) keys
        val regions = map (fn k => (k,label(valOf(List.find (fn r => key r = k) allRegions)),values k)) regionKeys
        val stack = if metric = "total"
                    then [("stack","ML stack",map (fn s => sum(map (fn r => number r "stack_bytes") (List.filter selected (list s "stacks")))) samples)] else []
        fun less ((k,_,v),(k',_,v')) = sum v < sum v' orelse (sum v = sum v' andalso k < k')
        val ordered = sort less (regions @ stack)
        val ranked = sort less regions
        val hidden = if limit = 0 then [] else List.take(ranked,Int.max(0,length ranked-limit))
        fun omitted k = List.exists (fn (k',_,_) => k = k') hidden
        val zero = map (fn _ => 0:IntInf.int) samples
        fun plus (a,b) = ListPair.mapEq (op +) (a,b)
        val bands = (if null hidden then [] else [("other","Other (" ^ Int.toString(length hidden) ^ " regions)",foldl (fn ((_,_,v),a) => plus(v,a)) zero hidden)]) @
                    List.filter (fn (k,_,_) => not(omitted k)) ordered
        val totals = foldl (fn ((_,_,v),a) => plus(v,a)) zero bands
        val peak = foldl IntInf.max 0 totals
        val firstSample = case samples of [] => raise Fail "no completed snapshots to export" | s::_ => s
        val first = number firstSample "time"
        val last = number (List.last samples) "time"
        val pagePeak = if flag "show-peak" false andalso scope = "all" andalso List.exists (fn m => metric = m) ["total","pages","page_footprint"]
                       then Option.map (fn n => integer n * number firstSample "page_bytes") (find firstSample "max_pages") else NONE
        val maximum = IntInf.max(1,case pagePeak of SOME n => IntInf.max(n,peak) | NONE => peak)
        fun memUnit factor [] = (factor,"EiB")
          | memUnit factor (u::us) = if maximum < factor*1024 orelse null us then (factor,u) else memUnit (factor*1024) us
        val (factor,unit) = memUnit 1 ["bytes","KiB","MiB","GiB","TiB","PiB","EiB"]
        val (timeFactor,timeUnit) = if last >= 1000000000 then (1.0E9,"s") else if last >= 1000000 then (1.0E6,"ms") else if last >= 1000 then (1.0E3,"µs") else (1.0,"ns")
        fun memory n = fmt(real n / real factor) ^ " " ^ unit
        val main = base(string(get metadata "main_source"))
        val gc = if get metadata "gc_enabled" = Bool true then "enabled" else "disabled"
        val caption = opt "caption" ("Region profile for " ^ main ^ " (GC " ^ gc ^ ")")
        val metricName = case metric of "total" => "Regions + ML stack" | "pages" => "Pages" | "page_footprint" => "Page footprint" | "large_bytes" => "Large objects" | "finite_bytes" => "Finite reservations" | _ => "Descriptors (separate)"
        val scopeName = case String.fields (fn c => c = #":") scope of
                            ["thread",n] => "Thread " ^ n | ["worker",n] => "Execution stream " ^ n | ["cpu",n] => "CPU (logical core) " ^ n | _ => "All threads"
        (* Conservative character widths keep labels within the vector canvas,
           including when a converter substitutes a different sans-serif font. *)
        fun chars s = List.filter (fn c => Word8.andb(Word8.fromInt(ord c),0wxC0) <> 0wx80) (explode s)
        fun width size s = size * foldl (fn (c,w) => w + (if c = #"W" orelse c = #"M" orelse ord c >= 128 then 1.0 else 0.65)) 0.0 (chars s)
        val legendWidth = Real.min(360.0,Real.max(100.0,foldl (fn ((_,s,_),w) => Real.max(w,width 16.0 s+24.0)) 0.0 bands))
        val canvasWidth = 1026.0 + legendWidth + 16.0
        val output = ref ([]:string list)
        fun emit s = output := s :: !output
        fun text x y size anchor s = emit("<text x=\"" ^ fmt x ^ "\" y=\"" ^ fmt y ^ "\" font-size=\"" ^ fmt size ^ "\" text-anchor=\"" ^ anchor ^ "\">" ^ escape s ^ "</text>")
        (* Split long tokens only at UTF-8 character boundaries. *)
        fun wrap size available s =
            let
              fun units [] = []
                | units (c::cs) =
                  let fun continuation c = Word8.andb(Word8.fromInt(ord c),0wxC0) = 0wx80
                      fun take (c::cs,acc) = if continuation c then take(cs,c::acc) else (rev acc,c::cs)
                        | take ([],acc) = (rev acc,[])
                      val (tail,rest) = take(cs,[])
                  in implode(c::tail)::units rest
                  end
              fun split [] line acc = rev (if line = "" then acc else line::acc)
                | split (c::cs) line acc = if line <> "" andalso width size (line ^ c) > available then split (c::cs) "" (line::acc) else split cs (line ^ c) acc
            in split (units(explode s)) "" []
            end
        fun paragraph s x y available size = foldl (fn (line,y) => (text x y size "start" line; y+size*1.4)) y (wrap size available s)
        val gcSummary = if gc = "enabled" then
                            " · Garbage collections: " ^ (case find metadata "gc_collections" of SOME (Num n) => n | _ => "unavailable") ^
                            (case find metadata "complete" of SOME (Bool true) => "" | _ => " (recorded so far)")
                        else ""
        val top = paragraph caption 16.0 34.0 (canvasWidth-32.0) 26.0
        val summary = "Metric: " ^ metricName ^ " · View: " ^ scopeName ^ gcSummary ^ " · Samples: " ^ Int.toString(length samples) ^ " · Sampled maximum: " ^ memory peak
        val summarySize = Real.min(16.0,(canvasWidth-32.0)/width 1.0 summary)
        val () = text 16.0 (top+4.0) summarySize "start" summary
        val top = top+4.0+summarySize*1.4+12.0
        fun x sample = 88.0 + 880.0 * real(number sample "time"-first) / real(IntInf.max(1,last-first))
        fun y n = top+593.0-528.0*real n/real maximum
        fun point (s,v) = fmt(x s) ^ "," ^ fmt(y v)
        val bottom = ref zero
        val () = List.app (fn (k,name,v) =>
                     let val upper = plus(!bottom,v)
                         val points = if length samples = 1 then
                                        "88," ^ fmt(y(hd upper)) ^ " 968," ^ fmt(y(hd upper)) ^ " 968," ^ fmt(y(hd(!bottom))) ^ " 88," ^ fmt(y(hd(!bottom)))
                                      else String.concatWith " " (ListPair.mapEq point (samples,upper) @ rev(ListPair.mapEq point (samples,!bottom)))
                     in emit("<polygon data-band=\"" ^ escape k ^ "\" fill=\"" ^ color k ^ "\" points=\"" ^ points ^ "\"><title>" ^ escape name ^ "</title></polygon>"); bottom := upper
                     end) bands
        val () = emit("<path d=\"M88 " ^ fmt(top+65.0) ^ " V" ^ fmt(top+593.0) ^ " H968\" fill=\"none\" stroke=\"#334155\"/>")
        fun ticks lo hi =
            if Real.==(lo,hi) then ([lo],0)
            else let val raw = (hi-lo)/6.0
                     val power = Real.floor(Math.ln raw / Math.ln 10.0)
                     val base = Math.pow(10.0,Real.fromInt power)
                     fun candidate multiple =
                         let val step = multiple*base
                             val start = Real.realCeil(lo/step-1.0E~10)
                             val count = Int.max(0,Real.floor(hi/step-start+1.0E~10)+1)
                             val score = 100*Int.max(0,Int.max(5-count,count-7))+Int.abs(count-6)
                         in (score,multiple,step,start,count)
                         end
                     val choices = map candidate [1.0,1.5,2.0,2.5,3.0,4.0,5.0,6.0,8.0,10.0]
                     val (_,multiple,step,start,count) =
                         foldl (fn (c,best) => if #1 c < #1 best then c else best) (hd choices) (tl choices)
                     val extra = if Real.==(multiple,Real.realFloor multiple) then 0 else 1
                 in (List.tabulate(count,fn i => (start+Real.fromInt i)*step),Int.min(12,Int.max(0,extra-power)))
                 end
        fun tick axis x1 y1 x2 y2 =
            emit("<line data-tick=\"" ^ axis ^ "\" x1=\"" ^ fmt x1 ^ "\" y1=\"" ^ fmt y1 ^
                 "\" x2=\"" ^ fmt x2 ^ "\" y2=\"" ^ fmt y2 ^ "\" stroke=\"#334155\"/>")
        val (memoryTicks,memoryDecimals) = ticks 0.0 (real maximum/real factor)
        val () = List.app (fn v =>
                    let val yy = top+593.0-528.0*v*real factor/real maximum
                    in tick "memory" 82.0 yy 88.0 yy;
                       text 78.0 (yy+4.0) 16.0 "end" (Real.fmt (StringCvt.FIX(SOME memoryDecimals)) v)
                    end) memoryTicks
        val (timeTicks,timeDecimals) = ticks (real first/timeFactor) (real last/timeFactor)
        val () = List.app (fn t =>
                    let val xx = 88.0+880.0*(t*timeFactor-real first)/real(IntInf.max(1,last-first))
                    in tick "time" xx (top+593.0) xx (top+599.0);
                       text xx (top+616.0) 16.0 "middle" (Real.fmt (if length samples = 1 then StringCvt.GEN(SOME 12) else StringCvt.FIX(SOME timeDecimals)) t)
                    end) timeTicks
        val () = text 88.0 (top+43.0) 16.0 "start" ("Memory (" ^ unit ^ ")")
        val () = text 528.0 (top+648.0) 16.0 "middle" ("Elapsed time (" ^ timeUnit ^ ")" ^ (if length samples = 1 then " · single snapshot" else ""))
        val () = case pagePeak of NONE => () | SOME n =>
                   (emit("<line x1=\"88\" x2=\"968\" y1=\"" ^ fmt(y n) ^ "\" y2=\"" ^ fmt(y n) ^ "\" stroke=\"#b91c1c\" stroke-width=\"2\" stroke-dasharray=\"8 4\"/>");
                    text 968.0 (top+43.0) 16.0 "end" ("Peak page capacity: " ^ memory n))
        val legendY = paragraph "Regions" 1026.0 (top+32.0) legendWidth 18.0 + 12.0
        val legendY = foldl (fn ((k,name,_),yy) =>
                        (emit("<rect x=\"1026\" y=\"" ^ fmt(yy-11.0) ^ "\" width=\"12\" height=\"12\" rx=\"2\" fill=\"" ^ color k ^ "\"/>");
                         paragraph name 1048.0 yy (legendWidth-22.0) 16.0+12.0)) legendY (rev bands)
        val foot = Real.max(top+668.0,legendY)+12.0
        val foot = case pagePeak of NONE => foot | SOME _ => paragraph "Peak page capacity excludes cached pages, large objects and stack storage; it includes allocations between snapshots and GC from/to-space overlap." 16.0 foot (canvasWidth-32.0) 16.0+12.0
        val height = foot+12.0
      in "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ^ fmt canvasWidth ^ "\" height=\"" ^ fmt height ^ "\" viewBox=\"0 0 " ^ fmt canvasWidth ^ " " ^ fmt height ^ "\" font-family=\"Arial, sans-serif\" font-size=\"16\" fill=\"#182c39\" role=\"img\"><title>" ^ escape caption ^ "</title><rect width=\"100%\" height=\"100%\" fill=\"white\"/>" ^ String.concat(rev(!output)) ^ "</svg>\n"
      end
end
