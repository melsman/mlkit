local
    fun print (s:string) : unit =
        prim("printStringML", s)

    fun alloc_unsafe (n:int) : 'a array =
        prim ("word_table0", n)

    fun update_unsafe (a : 'a array, i : int, x : 'a) : unit =
        prim ("word_update0", (a, i, x))

    fun sub_unsafe (a : 'a array, i : int) : 'a =
        prim ("word_sub0", (a, i))

    fun sp__noinline `[e1 e2] (f: unit #e1 -> 'a) (k: 'a #e2 -> 'b) : 'b while e1 ## e2 =
        k (f())

    fun get__noinline x = x
in

    fun spawn `[e1 e2] (f: unit #e1 -> 'a) (k: 'a #e2 -> 'b) : 'b while e1 ## e2 =
        sp__noinline f k

    fun get x = get__noinline x

    (* The below is not ok as there is no guarantee that g also stores values in the pair-region... *)

    fun par `[e1 e2] (f: unit #e1 -> 'a, g: unit #e2 -> 'b) : 'a * 'b while e1 ## e2 =
       spawn g (fn t => (f(), get t))

    val () = print "Done\n"

end
