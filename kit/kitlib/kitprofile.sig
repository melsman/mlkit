signature KIT_PROFILE =
sig
  val tellTime: string -> unit   (* tellTime(msg) stores msg for printing at next profile tick *)
end