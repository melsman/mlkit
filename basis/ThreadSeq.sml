structure ThreadSeq :> THREAD = struct
  type 'a t = 'a
  fun spawn f g = g(f())
  fun get x = x
  fun numCores () = 1
end

structure Thread = ThreadSeq
