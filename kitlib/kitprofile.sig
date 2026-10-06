signature KIT_PROFILE =
sig
  val tellTime: string -> unit   (* tellTime(msg) emits a marker when region profiling is active *)
end