
signature POSIX_FILE_SYS = 
  sig
    eqtype uid
    eqtype gid
    eqtype file_desc

    structure S : sig
      eqtype mode
      include BIT_FLAGS
        where type flags = mode
        val irwxu : mode
        val irusr : mode
        val iwusr : mode
        val ixusr : mode
        val irwxg : mode
        val irgrp : mode
        val iwgrp : mode
        val ixgrp : mode
        val irwxo : mode
        val iroth : mode
        val iwoth : mode
        val ixoth : mode
        val isuid : mode
        val isgid : mode
      end

    structure O : sig
      include BIT_FLAGS

      val append : flags
      val excl : flags
      val noctty : flags
      val nonblock : flags
      val sync : flags
      val trunc : flags
    end

    datatype open_mode = 
        O_RDONLY
      | O_WRONLY
      | O_RDWR

    val stdin : file_desc
    val stdout : file_desc
    val stderr : file_desc

    val openf : string * open_mode * O.flags -> file_desc
    val createf : string * open_mode * O.flags * S.mode -> file_desc
    val creat : string * S.mode -> file_desc
    val umask : S.mode -> S.mode

    val link : {old : string, new : string} -> unit
    val mkdir : string * S.mode -> unit
    val mkfifo : string * S.mode -> unit
    val unlink : string -> unit
    val rmdir : string -> unit
    val rename : {old : string, new : string} -> unit
    val symlink : {old : string, new : string} -> unit
    val readlink : string -> string

    val chmod : string * S.mode -> unit
    val fchmod : file_desc * S.mode -> unit
    val chown : string * uid * gid -> unit
    val fchown : file_desc * uid * gid -> unit
  end
