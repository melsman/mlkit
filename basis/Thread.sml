structure Thread :> THREAD = struct
  type thread = foreignptr
  type 'a t0 = ((unit->'a) * thread) ref
  fun get__noinline ((ref (f,t0)): 'a t0) : 'a = prim("thread_get", t0)
  fun spawn__noinline (f: unit->'a) (k: 'a t0 -> 'b) : 'b =
      let val rf : (unit -> 'a) ref = ref f
          val fp_f : foreignptr = prim("pointer", !rf)

          (* From a region inference perspective, coercing the type of
           * a function to a pointer type is very unsafe, as the
           * pointer type contains no information about which regions
           * need to be kept alive for the pointer value to be
           * valid. By including the type of the function in the type
           * of the thread value, however, we keep all necessary
           * regions alive.
           *
           * Moreover, we encapsulate the function in a ref to avoid
           * that the closure is inlined into the "pointer" prim
           * argument. If it is inlined, the closure will be allocated
           * in a local stack-allocated region inside the "pointer"
           * prim value, which cause the program to segfault.. *)

          val t0 : thread = prim("spawnone", fp_f)
          val t: 'a t0 = ref (f,t0)

          fun force () =
              if !(ref true)          (* make sure the thread has terminated before returning *)
              then get__noinline t    (* and mimic that, from a type perspective, spawn has *)
              else !rf()              (* the effect of calling f *)

          val res = k t handle e => (force(); raise e)
          val _ = force ()

          (* Notice that it is not safe to call `thread_free t0` here
           * as t0 may be live through later calls to `get t`.
           *
           * What is needed is for the ThreadInfo structs to be region
           * allocated and to add finalisers (thread_free) to objects
           * in these regions. Technically, the thread is detached
           * already in the thread_get function. However, the mutex
           * and the ThreadInfo struct is kept live.
           *)
      in res
      end

  (* Now, make it work when a thread raises an exception *)
  datatype 'a res = Ok of 'a | Exn of exn
  type 'a t = 'a res t0
  fun spawn (f: unit->'a) (k: 'a t -> 'b) : 'b =
      let fun f' () = Ok (f ()) handle e => Exn e
      in spawn__noinline f' k
      end
  fun get (x:'a t) : 'a =
      case get__noinline x of
          Ok v => v
        | Exn e => raise e

  fun numCores () : int =
      prim("numCores",())
end
