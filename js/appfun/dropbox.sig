signature DROPBOX = sig
  type client
  type datastoremanager
  type datastore
  type table
           
  val load                 : (unit -> unit) -> unit
  val client               : string -> client    (* string is client key *)
  val authenticate         : client -> (string option -> unit) -> unit
  val authenticate0        : client -> unit
  val dropboxUid           : client -> string
  val isAuthenticated      : client -> bool
  val signOut              : client -> (unit -> unit) -> unit

  val getDatastoreManager  : client -> datastoremanager
  val openDefaultDatastore : datastoremanager -> (string * datastore -> unit) -> unit
  val deleteDatastore      : datastoremanager -> datastore -> unit

  val getTable             : datastore -> string -> table

  type record
  type hash = (string * string) list
  val insert               : table -> hash -> unit
  val deleteRecord         : record -> unit
  val query                : table -> hash -> record list
  val set                  : record -> string -> string -> unit
  val get                  : record -> string -> string
  val allRecords           : table -> record list
end
