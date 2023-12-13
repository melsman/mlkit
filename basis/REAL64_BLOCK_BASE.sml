signature REAL64_BLOCK_BASE2 = sig
  type t
  val sub0 : t -> real
  val sub1 : t -> real
  val upd0 : t * real -> unit
  val upd1 : t * real -> unit
end

signature REAL64_BLOCK_BASE3 = sig include REAL64_BLOCK_BASE2 val sub2 : t -> real val upd2 : t * real -> unit end
signature REAL64_BLOCK_BASE4 = sig include REAL64_BLOCK_BASE3 val sub3 : t -> real val upd3 : t * real -> unit end
signature REAL64_BLOCK_BASE5 = sig include REAL64_BLOCK_BASE4 val sub4 : t -> real val upd4 : t * real -> unit end
signature REAL64_BLOCK_BASE6 = sig include REAL64_BLOCK_BASE5 val sub5 : t -> real val upd5 : t * real -> unit end
signature REAL64_BLOCK_BASE7 = sig include REAL64_BLOCK_BASE6 val sub6 : t -> real val upd6 : t * real -> unit end
signature REAL64_BLOCK_BASE8 = sig include REAL64_BLOCK_BASE7 val sub7 : t -> real val upd7 : t * real -> unit end
signature REAL64_BLOCK_BASE9 = sig include REAL64_BLOCK_BASE8 val sub8 : t -> real val upd8 : t * real -> unit end
signature REAL64_BLOCK_BASE10 = sig include REAL64_BLOCK_BASE9 val sub9 : t -> real val upd9 : t * real -> unit end
signature REAL64_BLOCK_BASE11 = sig include REAL64_BLOCK_BASE10 val sub10 : t -> real val upd10 : t * real -> unit end

signature REAL64_BLOCK2 = sig include REAL64_BLOCK_BASE2
  val pack : real*real -> t
  val unpack : t -> real*real
end

signature REAL64_BLOCK3 = sig include REAL64_BLOCK_BASE3
  val pack : real*real*real -> t
  val unpack : t -> real*real*real
end

signature REAL64_BLOCK4 = sig include REAL64_BLOCK_BASE4
  val pack : real*real*real*real -> t
  val unpack : t -> real*real*real*real
end

signature REAL64_BLOCK5 = sig include REAL64_BLOCK_BASE5
  val pack : real*real*real*real*real -> t
  val unpack : t -> real*real*real*real*real
end

signature REAL64_BLOCK6 = sig include REAL64_BLOCK_BASE6
  val pack : real*real*real*real*real*real -> t
  val unpack : t -> real*real*real*real*real*real
end

signature REAL64_BLOCK7 = sig include REAL64_BLOCK_BASE7
  val pack : real*real*real*real*real*real*real -> t
  val unpack : t -> real*real*real*real*real*real*real
end

signature REAL64_BLOCK8 = sig include REAL64_BLOCK_BASE8
  val pack : real*real*real*real*real*real*real*real -> t
  val unpack : t -> real*real*real*real*real*real*real*real
end

signature REAL64_BLOCK9 = sig include REAL64_BLOCK_BASE9
  val pack : real*real*real*real*real*real*real*real*real -> t
  val unpack : t -> real*real*real*real*real*real*real*real*real
end

signature REAL64_BLOCK10 = sig include REAL64_BLOCK_BASE10
  val pack : real*real*real*real*real*real*real*real*real*real -> t
  val unpack : t -> real*real*real*real*real*real*real*real*real*real
end

signature REAL64_BLOCK11 = sig include REAL64_BLOCK_BASE11
  val pack : real*real*real*real*real*real*real*real*real*real*real -> t
  val unpack : t -> real*real*real*real*real*real*real*real*real*real*real
end
