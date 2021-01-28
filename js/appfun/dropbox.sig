signature DROPBOX = sig
  type client
  type datastoremanager
  type datastore
  type table
  type 'a cont = ('a -> unit) -> unit

  val load                 : unit cont
  val client               : string -> client    (* string is client key *)
  val authorize            : client -> unit
  val dropboxUid           : client -> string cont
  val isAuthorized         : client -> bool
  val signOut              : client -> unit cont

  structure FileStore : sig
    type filestore
    type filename = string and dirname = string
    val filestore   : client -> filestore
    val all_files   : filestore -> filename list cont
    val content     : filestore -> filename -> string cont
    val write_file  : filestore -> filename -> string -> {msg:string} cont
    val mkdir       : filestore -> dirname -> {msg:string} cont
    val delete_file : filestore -> filename -> {msg:string} cont
    val delete_dir  : filestore -> dirname -> {msg:string} cont
    val move_file   : filestore -> filename -> filename -> {msg:string} cont
    val move_dir    : filestore -> dirname -> dirname -> {msg:string} cont
  end
end


(**

[FileStore.write_file fs path s g] writes the content s into path at
filestore fs. The function g is passed a message string.

*)
