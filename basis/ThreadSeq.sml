structure ThreadSeq :> THREAD = struct
  type 'a t = 'a
  fun spawn f g = g(f())
  fun get x = x
end

structure Thread = ThreadSeq
