structure Dropbox :> DROPBOX = struct
    type client = foreignptr
    type datastoremanager = foreignptr
    type datastore = foreignptr
    type table = foreignptr
    structure J = JsCore
    val op ==> = J.==> infix ==>

    fun load f = Js.loadScript "https://www.dropbox.com/static/api/dropbox-datastores-1.0-latest.js" f
    fun client k =
        J.exec1{stmt="return new Dropbox.Client({key:k});",
                arg1=("k", J.string),
                res=J.fptr} k
        
    fun callMethod0 (t:'a J.T) m (obj:foreignptr) : 'a =
        J.exec1{arg1=("obj",J.fptr),
                stmt="return obj." ^ m ^ "();",
                res=t} obj

    fun callMethod1 (a1t: 'a1 J.T, t:'a J.T) m (obj:foreignptr) (a1:'a1) : 'a =
        J.exec2{arg1=("obj",J.fptr),
                arg2=("a1",a1t),
                stmt="return obj." ^ m ^ "(a1);",
                res=t} (obj,a1)

    fun callMethod2 (a1t: 'a1 J.T, a2t: 'a2 J.T, t:'a J.T) m (obj:foreignptr) (a1:'a1,a2:'a2) : 'a =
        J.exec3{arg1=("obj",J.fptr),
                arg2=("a1",a1t),
                arg3=("a2",a2t),
                stmt="return obj." ^ m ^ "(a1,a2);",
                res=t} (obj,a1,a2)
               
    fun authenticate c (onerr: string option -> unit) =
        J.exec2{arg1=("c",J.fptr),
                arg2=("f",(J.option J.string) ==> J.unit),
                stmt="c.authenticate({interactive:false}, f);",
                     res=J.unit} (c,onerr)

    fun dropboxUid c =
        callMethod0 J.string "dropboxUid" c

    fun authenticate0 c =
        callMethod0 J.unit "authenticate" c

    fun isAuthenticated c =
        callMethod0 J.bool "isAuthenticated" c

    fun getDatastoreManager c =
        callMethod0 J.fptr "getDatastoreManager" c

    fun openDefaultDatastore dsm f =
        J.exec2{arg1=("dsm",J.fptr),
                arg2=("thef",J.===>(J.string,J.fptr,J.unit)),
                stmt="return dsm.openDefaultDatastore(function(err,ds) {thef([err,ds]);});",
                res=J.unit} (dsm,f)

    fun deleteDatastore dsm ds =
        callMethod2 (J.fptr,J.==>(J.string,J.unit),J.unit) "deleteDatastore" dsm (ds,fn _ => ())

    fun getTable ds s : table =
        callMethod1 (J.string, J.fptr) "getTable" ds s

    type hash = (string * string) list
    type hash0 = foreignptr

    fun mkHash (h: hash) : hash0 =
        let val hash = JsCore.exec0{stmt="return {};", res=JsCore.fptr} ()
            val () = List.app (fn (k,v) => JsCore.setProperty hash JsCore.string k v) h
        in hash
        end

    fun signOut c f : unit =
        let val h = mkHash []
        in callMethod2 (J.fptr,J.==>(J.fptr,J.unit),J.unit) "signOut" c (h,fn _ => f())
        end

    type record = foreignptr
    fun insert (t:table) (h:hash) : unit =
        (callMethod1 (J.fptr, J.fptr) "insert" t (mkHash h); ())

    fun deleteRecord (r:record) : unit =
        callMethod0 J.unit "deleteRecord" r

    fun idx (t: 'a J.T) (a: foreignptr) i : 'a =
        J.exec2{arg1=("a",J.fptr),arg2=("i",J.int),res=t,
                stmt="return a[i];"}(a,i)

    fun len (a: foreignptr) : int =
        J.getProperty a J.int "length"

    fun query t h : record list =
        let val records = callMethod1 (J.fptr, J.fptr) "query" t (mkHash h)
            fun loop i acc = 
                if i < 0 then acc 
                else loop (i-1) (idx J.fptr records i :: acc)
        in loop (len records - 1) nil
        end

    fun allRecords t = query t []

    fun set r k v =
        callMethod2 (J.string,J.string,J.unit) "set" r (k,v)

    fun get r k =
        callMethod1 (J.string,J.string) "get" r k
  end
