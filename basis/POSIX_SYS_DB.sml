(** Operations on the POSIX user and group database.

The Posix.SysDB structure implements operations on the user database
and the group database (in POSIX parlance, the password file and the
group file). These are the data and operations described in Section 9
of the POSIX standard 1003.1,1996.

*)

signature POSIX_SYS_DB =
  sig
    eqtype uid
    eqtype gid

    structure Passwd :
      sig
        type passwd
        val name  : passwd -> string
        val uid   : passwd -> uid
        val gid   : passwd -> gid
        val home  : passwd -> string
        val shell : passwd -> string
      end

    structure Group :
      sig
        type group
        val name    : group -> string
        val gid     : group -> gid
        val members : group -> string list
      end

    val getgrgid : gid -> Group.group
    val getgrnam : string -> Group.group
    val getpwuid : uid -> Passwd.passwd
    val getpwnam : string -> Passwd.passwd
  end

(**

[eqtype uid] Type of user identifier; identical to Posix.ProcEnv.uid.

[eqtype gid] Type of group identifier; identical to Posix.ProcEnv.gid.

[structure Passwd] Operations on users.

    [type passwd] Information related to a user.

    [name passwd]
    [uid passwd]
    [gid passwd]
    [home passwd]
    [shell passwd]

    These extract the name, the user ID, the group ID, the path of the
    initial working, or home, directory, and the initial command
    shell, respectively, of the user corresponding to the passwd
    value. The names of the corresponding fields in C are the same,
    but prefixed with "pw_". The one exception is that C uses pw_dir
    for the home directory.

[structure Group] Operations on groups.

    [type group] Information related to a group.

    [name group]
    [gid group]
    [members group]

    These extract the name, the group ID, and the names of users
    belonging to the group, respectively, of the group corresponding
    to the group value. In C, these fields are named gr_name, gr_gid,
    and gr_mem, respectively.

[getgrgid gid]
[getgrnam n]
[getpwuid uid]
[getpwnam n]

These return the group or user database entry associated with the
given group ID or name, or user ID or name. It raises OS.SysErr if
there is no group or user with the given ID or name.

*)
