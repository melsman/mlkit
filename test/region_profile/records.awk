# Flat JSON records emitted by the runtime. rpview separately validates JSON.
# Keep counters as decimal strings: awk numbers cannot represent all uint64s.
function need(ok, message) {
    if (!ok) { print FILENAME ":" FNR ": " message > "/dev/stderr"; failed=1; exit 1 }
}
function add(a,b,    result,carry,x,y) {
    result=""; carry=0
    while (length(a) || length(b) || carry) {
        x=length(a) ? substr(a,length(a),1)+0 : 0
        y=length(b) ? substr(b,length(b),1)+0 : 0
        x+=y+carry; carry=int(x/10); result=(x%10) result
        a=substr(a,1,length(a)-1); b=substr(b,1,length(b)-1)
    }
    return length(result) ? result : "0"
}
function equal(a,b) { return "x" a == "x" b }
function parse(line,    key,token,k) {
    for (k in f) delete f[k]
    sub(/^[ \t]*\{[ \t]*/,"",line)
    while (match(line,/^"([^"\\]|\\.)*"[ \t]*:/)) {
        key=substr(line,2,RLENGTH-1); sub(/"[ \t]*:$/,"",key)
        line=substr(line,RLENGTH+1); sub(/^[ \t]*/,"",line)
        need(match(line,/^("([^"\\]|\\.)*"|-?[0-9]+|true|false|null)/),"expected scalar")
        token=substr(line,1,RLENGTH); line=substr(line,RLENGTH+1)
        if (substr(token,1,1)=="\"") token=substr(token,2,length(token)-2)
        f[key]=token
        sub(/^[ \t]*,[ \t]*/,"",line)
    }
    need(line ~ /^[ \t]*}[ \t]*$/, "expected flat record")
}
{ parse($0) }
# Resolve definition references independently of rpview for fixture accounting.
f["type"]=="binding" {
    id=f["definition"]
    need(!(id in definitions),"duplicate definition")
    definitions[id]=1
    identity=f["unit"] SUBSEP f["binding"] SUBSEP f["source"] SUBSEP f["name"] SUBSEP f["kind"] SUBSEP f["region_type"]
    need(!(identity in emitted),"static metadata emitted more than once")
    emitted[identity]=1
    split("unit source name region_type kind binding",keys," ")
    for (i in keys) metadata[id,keys[i]]=f[keys[i]]
}
f["type"]=="region" {
    id=f["definition"]
    need(id in definitions,"undefined binding")
    split("unit source name region_type kind binding",keys," ")
    for (i in keys) {
        need(!(keys[i] in f),"repeated static metadata")
        f[keys[i]]=metadata[id,keys[i]]
    }
}
