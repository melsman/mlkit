f["type"]=="header" { page=f["page_bytes"]; gc=f["gc_enabled"]; source=f["main_source"] }
f["type"]=="sample_begin" {
    n++; need(f["sample"]==n,"sample sequence"); reason[n]=f["reason"]
    if(mode!="runtime") need(f["time"]+0>=lastEnd+0,"time ordering"); active=1
    finite=0; stackFinite=0; footprint=0; large=0; stackCount=0
    for (k in owners) delete owners[k]
    if (reason[n]=="explicit") explicit++
    if (reason[n]=="before_gc") { need(!inGC,"nested GC"); inGC=1; gcKind=f["gc_kind"] }
    if (reason[n]=="after_gc") { need(inGC && gcKind==f["gc_kind"],"GC pairing"); inGC=0; collections++ }
    if (mode=="periodic") need(reason[n]=="periodic","periodic reason")
}
f["type"]=="region" || f["type"]=="stack" {
    need(active && f["sample"]==n,"record outside snapshot")
    need(f["cpu"]>=-1,"CPU identity"); if (f["cpu"]>=0) cpuSeen=1
}
f["type"]=="region" {
    # Native fixture counters are small; large-counter preservation is checked
    # independently with exact output comparisons in check-viewer.sh.
    need(f["page_footprint"]+f["unused_tail"]==f["pages"]*page,"page accounting")
    if ("g0_pages" in f) {
        need(equal(f["pages"],add(f["g0_pages"],f["g1_pages"])),"generation pages")
        need(equal(f["unused_tail"],add(f["g0_unused_tail"],f["g1_unused_tail"])),"generation tails")
    }
    finite=add(finite,f["finite_bytes"]); large=add(large,f["large_bytes"]); footprint=add(footprint,f["page_footprint"])
    names[f["name"]]=1; owners[f["thread"]]=1
    if (f["large_bytes"]>=80000) largeSeen=1
    if (f["g1_pages"]>0 && f["g1_unused_tail"]>0) generationSeen=1
    if (f["source"] ~ /^REPL #/) replSource=1
    if (f["unit"]=="<global>") { global[n]=1; if (n==1) types[f["region_type"]]=1 }
    if (mode=="argobots") { need(f["worker"]>=-1,"worker identity"); if(f["worker"]>=0) workerSeen=1 }
    if (mode=="runtime" && n==1) {
        need(f["source"]=="/fixtures/test.sml","fixture source")
        need((f["binding"]==11 && f["region_type"]=="pair") || (f["binding"]==12 && f["region_type"]=="string") || (f["binding"]==13 && f["region_type"]=="bot"),"fixture type")
    }
    if (mode=="regions") {
        need(f["region_type"]!="unavailable","region type missing")
        need(f["unit"]=="<global>" ? f["source"]=="global" : f["source"] ~ /\/regions.sml$/,"source filename")
        if (n==8 && f["unit"]!="<global>" && f["kind"]=="infinite") {
            locals++; need(f["pages"]==2 && f["page_footprint"]==13016,"local page capacity")
        }
    }
}
f["type"]=="stack" {
    need(equal(f["active_bytes"],add(f["stack_bytes"],f["finite_bytes"])),"stack accounting")
    stackFinite=add(stackFinite,f["finite_bytes"]); stackCount++
    if(mode=="runtime") need(f["active_bytes"]==(n==1 ? 1056 : 512) && f["stack_bytes"]==(n==1 ? 1008 : 488),"runtime stack span")
    if(mode=="graph" && n<=24) { need(n==1 || f["active_bytes"]+0>previousStack+0,"recursive stack growth"); previousStack=f["active_bytes"] }
}
f["type"]=="sample_end" {
    need(active && f["sample"]==n,"snapshot end"); active=0; completed++
    need(stackCount>0 && equal(finite,stackFinite),"finite reservations / stack subtraction")
    lastEnd=f["time"]; maxCollections=f["gc_collections"]+0>maxCollections+0 ? f["gc_collections"] : maxCollections
    ownersCount=0; for(k in owners) ownersCount++; if(ownersCount>2 || (mode=="argobots" && ownersCount>1)) multiOwner=1
    if(mode=="runtime") {
        need(f["max_pages"]==2,"snapshot page maximum")
        need(footprint==(n==1 ? 8264 : 16) && large==(n==1 ? 4096 : 0) && finite==(n==1 ? 48 : 24),"runtime totals")
        if(n==1) need(f["frames"]==2 && f["pages_visited"]==2,"runtime traversal")
    }
    if(mode=="regions") {
        split("32 16 16 16 0 48 32", expected," "); if(n<=7) need(finite==expected[n],"finite totals")
        need(large==(n<=2 ? 24008 : 0),"large objects")
        split("112 112 112 144 96",expected," "); if(n<=5) need(footprint==expected[n],"page footprint")
        if(n==1 || n==9) need(f["frames"]>=3,"frame span")
        if(n==6) need(f["frames"]>=4,"recursive frames")
        if(n==9) need(finite==112,"spilled result reservation")
    }
}
f["type"]=="thread_start" { starts[f["thread"]]++; startCount++ }
f["type"]=="thread_end" { ends[f["thread"]]=1 }
f["type"]=="mark" && mode=="api" { need(f["label"]=="quote\\\" slash\\\\ newline\\n nul\\u0000tail","marker escaping"); markSeen=1 }
f["type"]=="session_end" { session=1; count=f["gc_collections"]; maxPages=f["max_pages"] }
END {
    if(failed) exit 1
    need(completed==n && n>0 && !active,"complete snapshots")
    if(mode=="runtime") need(n==4 && maxPages==7 && reason[1]=="explicit" && reason[2]=="start" && reason[3]=="pause" && reason[4]=="explicit","runtime lifecycle")
    else if(mode=="regions") { need(n==9 && locals==1,"region sample count"); split("top string pair array ref triple",expected," "); for(k in expected) need(expected[k] in types,"global type") }
    else if(mode=="api") need(n==4 && markSeen && reason[1]=="start" && reason[2]=="explicit" && reason[3]=="pause" && reason[4]=="explicit","API lifecycle")
    else if(mode=="graph") need(n==25 && "`alpha" in names && "`beta" in names && "`gamma" in names,"graph bindings")
    else {
        need(session && count+0>=maxCollections+0,"final collection count")
        need(gc=="true" || gc=="false","GC metadata")
        if(gc=="false") need(count==0,"no-GC collections")
        if(mode!="repl") need(source ~ ("/" mode ".sml$") && (gc=="true")== (mode=="gc" || mode=="gengc"),"program metadata")
        if(linux) need(cpuSeen,"Linux CPU capture")
        if(mode=="periodic") need(n>=2,"periodic samples")
        if(mode=="parallel" || mode=="argobots") {
            need(explicit==61 && startCount==13 && multiOwner,"parallel samples")
            for(i=0;i<13;i++) need(starts[i]==1,"thread starts")
            for(i=1;i<13;i++) need(ends[i],"thread exits")
            if(mode=="argobots") need(workerSeen,"Argobots workers")
        }
        if(mode=="gc" || mode=="gengc") { need(!inGC && collections>0 && count>=collections && largeSeen,"GC collections / large objects"); if(mode=="gengc") need(generationSeen,"generation tail") }
        if(mode=="repl") { need(source=="REPL" && replSource && explicit==2,"REPL metadata"); for(i=1;i<=n;i++) need(global[i],"REPL globals") }
    }
    print mode ": accounting and lifecycle checks passed"
}
