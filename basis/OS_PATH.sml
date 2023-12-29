(** Path management operations for the operating system. *)

signature OS_PATH =
  sig
    exception Path
    exception InvalidArc

    val parentArc  : string
    val currentArc : string
    val fromString : string -> {isAbs : bool,
                                vol : string,
                                arcs : string list
                               }
    val toString   : {isAbs : bool,
                      vol : string,
                      arcs : string list
                     } -> string

    val validVolume  : {isAbs: bool, vol: string} -> bool

    val getVolume    : string -> string
    val getParent    : string -> string

    val splitDirFile : string -> {dir: string, file: string}
    val joinDirFile  : {dir : string, file : string} -> string
    val dir          : string -> string
    val file         : string -> string

    val splitBaseExt : string -> {base: string, ext: string option}
    val joinBaseExt  : {base: string, ext: string option} -> string
    val base         : string -> string
    val ext          : string -> string option

    val mkCanonical  : string -> string
    val isCanonical  : string -> bool
    val mkAbsolute   : {path: string, relativeTo: string} -> string
    val mkRelative   : {path: string, relativeTo: string} -> string
    val isAbsolute   : string -> bool
    val isRelative   : string -> bool
    val isRoot       : string -> bool

    val concat       : string * string -> string

    val fromUnixPath : string -> string
    val toUnixPath   : string -> string
  end

(**

[parentArc] The string denoting the parent directory (e.g., ".." on
Microsoft Windows and Unix).

[currentArc] The string denoting the current directory (e.g., "." on
Microsoft Windows and Unix).

[fromString path] returns the decomposition {isAbs, vol, arcs} of the
path specified by path.  vol is the volume name and arcs is the list
of (possibly empty) arcs of the path. isAbs is true if the path is
absolute. Under Unix, the volume name is always the empty string;
under Microsoft Windows, in addition it can have the form "A:", "C:",
etc.

Here are some examples for Unix paths:

    path        fromString path
    ""          {isAbs=false, vol="", arcs=[]}
    "/"         {isAbs=true, vol="", arcs=[""]}
    "//"        {isAbs=true, vol="", arcs=["", ""]}
    "a"         {isAbs=false, vol="", arcs=["a"]}
    "/a"        {isAbs=true, vol="", arcs=["a"]}
    "//a"       {isAbs=true, vol="", arcs=["","a"]}
    "a/"        {isAbs=false, vol="", arcs=["a", ""]}
    "a//"       {isAbs=false, vol="", arcs=["a", "", ""]}
    "a/b"       {isAbs=false, vol="", arcs=["a", "b"]}


[toString {isAbs, vol, arcs}] makes a string out of a path represented
as a list of arcs. isAbs specifies whether or not the path is
absolute, and vol provides a corresponding volume. It returns "" when
applied to {isAbs=false, vol="", arcs=[]}. The exception Path is
raised if validVolume{isAbs, vol} is false, or if isAbs is false and
arcs has an initial empty arc. The exception InvalidArc is raised if
any component in arcs is not a valid representation of an arc.  The
exception Size is raised if the resulting string would have size
greater than String.maxSize.

The composition (toString o fromString) is the identity. The
composition (fromString o toString) is also the identity, provided no
exception is raised and none of the strings in arcs contains an
embedded arc separator character. In addition, isRelative(toString
{isAbs=false, vol, arcs}) evaluates to true when defined.

[validVolume {isAbs, vol}] returns true if vol is a valid volume name
for an absolute or relative path, respectively as isAbs is true or
false. Under Unix, the only valid volume name is "". Under Microsoft
Windows, the valid volume names have the form "a:", "A:", "b:", "B:",
etc. and, if isAbs = false, also "". Under MacOS, isAbs can be true if
and only if vol is "".

[getVolume path] returns the volume portion of the path path.

[getParent path] returns a string denoting the parent directory of
path. It holds that getParent path = path if and only if path is a
root. If the last arc is empty or the parent arc, then getParent
appends a parent arc. If the last arc is the current arc, then it is
replaced with the parent arc. Note that if path is canonical, then the
result of getParent will also be canonical.

Here are some examples for Unix paths:

    path        getParent path
    "/"         "/"
    "a"         "."
    "a/"        "a/.."
    "a///"      "a///.."
    "a/b"       "a"
    "a/b/"      "a/b/.."
    ".."        "../.."
    "."         ".."
    ""  ".."

[splitDirFile path] splits the string path path into its directory and
file parts, where the file part is defined to be the last arc. The
file will be "", if the last arc is "".

Here are some examples for Unix paths:

    path        splitDirFile path
    ""          {dir = "", file = ""}
    "."         {dir = "", file = "."}
    "b"         {dir = "", file = "b"}
    "b/"        {dir = "b", file = ""}
    "a/b"       {dir = "a", file = "b"}
    "/a"        {dir = "/", file = "a"}

[joinDirFile {dir, file}] creates a whole path out of a directory and
a file by extending the path dir with the arc file. If the string file
does not correspond to an arc, raises InvalidArc. The exception Size
is raised if the resulting string would have size greater than
String.maxSize.

[dir path]
[file path]

These functions return the directory and file parts of a path,
respectively. They are equivalent to #dir o splitDirFile and #file o
splitDirFile, respectively, although they are probably more efficient.

[splitBaseExt path] splits the path path into its base and extension
parts. The extension is a non-empty sequence of characters following
the right-most, non-initial, occurrence of "." in the last arc; NONE
is returned if the extension is not defined. The base part is
everything to the left of the extension except the final ".". Note
that if there is no extension, a terminating "." is included with the
base part.

Here are some examples for Unix paths:

    path        splitBaseExt path
    ""          {base = "", ext = NONE}
    ".login"    {base = ".login", ext = NONE}
    "/.login"   {base = "/.login", ext = NONE}
    "a"         {base = "a", ext = NONE}
    "a."        {base = "a.", ext = NONE}
    "a.b"       {base = "a", ext = SOME "b"}
    "a.b.c"     {base = "a.b", ext = SOME "c"}
    ".news/comp"        {base = ".news/comp", ext = NONE}

[joinBaseExt {base, ext}] returns an arc composed of the base name and
the extension (if different from NONE). It is a left inverse of
splitBaseExt, i.e., joinBaseExt o splitBaseExt is the identity. The
opposite does not hold, since the extension may be empty, or may
contain extension separators. Note that although splitBaseExt will
never return the extension SOME(""), joinBaseExt treats this as
equivalent to NONE. The exception Size is raised if the resulting
string would have size greater than String.maxSize.

[base path]
[ext path]

These functions return the base and extension parts of a path,
respectively. They are equivalent to #base o splitBaseExt and #ext o
splitBaseExt, respectively, although they are probably more efficient.

[mkCanonical path] returns the canonical path equivalent to
path. Redundant occurrences of the parent arc, the current arc, and
the empty arc are removed. The canonical path will never be the empty
string; the empty path is converted to the current directory path ("."
under Unix and Microsoft Windows).

Note that the syntactic canonicalization provided by mkCanonical may
not preserve file system meaning in the presence of symbolic links
(see concat).

[isCanonical path] returns true if path is a canonical path. It is
equivalent to (path = mkCanonical path).

[mkAbsolute {path, relativeTo}] returns an absolute path that is
equivalent to the path path relative to the absolute path
relativeTo. If path is already absolute, it is returned
unchanged. Otherwise, the function returns the canonical concatenation
of relativeTo with path, i.e., mkCanonical (concat (abs, p)). Thus, if
path and relativeTo are canonical, the result will be canonical. If
relativeTo is not absolute, or if the two paths refer to different
volumes, then the Path exception is raised. The exception Size is
raised if the resulting string would have size greater than
String.maxSize.

[mkRelative {path, relativeTo}] returns a relative path p that, when
taken relative to the canonical form of the absolute path relativeTo,
is equivalent to the path path. If path is relative, it is returned
unchanged. If path is absolute, the procedure for computing the
relative path is to first compute the canonical form abs of
relativeTo. If path and abs are equal, then the current arc is the
result.  Otherwise, the common prefix is stripped from path and abs
giving p' and abs'.  The resulting path is then formed by appending p'
to a path consisting of one parent arc for each arc in abs'. Note that
if both paths are canonical, then the result will be canonical.

If relativeTo is not absolute, or if path and relativeTo are both
absolute but have different roots, the Path exception is raised. The
exception Size is raised if the resulting string would have size
greater than String.maxSize.

Here are some examples for Unix paths:

    path        relativeTo      mkRelative{path, relativeTo}
    "a/b"       "/c/d"  "a/b"
    "/"         "/a/b/c"        "../../.."
    "/a/b/"     "/a/c"  "../b/"
    "/a/b"      "/a/c"  "../b"
    "/a/b/"     "/a/c/"         "../b/"
    "/a/b"      "/a/c/"         "../b"
    "/"         "/"     "."
    "/"         "/."    "."
    "/"         "/.."   "."
    "/a/b/../c"         "/a/d"  "../b/../c"
    "/a/b"      "/c/d"  "../../a/b"
    "/c/a/b"    "/c/d"  "../a/b"
    "/c/d/a/b"  "/c/d"  "a/b"

[isAbsolute path]
[isRelative path]

These functions return true if path is, respectively, absolute or relative.

[isRoot path] returns true if path is a canonical specification of a
root directory.

[concat (path, t)] returns the path consisting of path followed by
t. It raises the exception Path if t is not a relative path or if path
and t refer to different volumes. The exception Size is raised if the
resulting string would have size greater than String.maxSize.

An implementation of concat might be:

     fun concat (p1, p2) = (case (fromString p1, fromString p2)
       of (_, {isAbs=true, ...}) => raise Path
        | ({isAbs, vol=v1, arcs=al1},
           {vol=v2, arcs=al2, ...}
          ) => if ((v2 = "") orelse (v1 = v2))
              then toString{
                  isAbs=isAbs, vol=v1,
                  arcs=concatArcs(al1, al2)
                }
              else raise Path
      (* end case *))

where concatArcs is like List.@, except that a trailing empty arc in
the first argument is dropped. Note that concat should not be confused
with the concatenation of two strings.

The concat function does not preserve canonical paths. For example,
concat("a/b", "../c") returns "a/b/../c". The parent arc is not
removed because "a/b/../c" and "a/c" may not be equivalent in the
presence of symbolic links.

[fromUnixPath s] converts the Unix-style path s to the path syntax of
the host operating system. Slash characters are translated to the
directory separators of the local system, as are parent arcs and
current arcs. This function raises the InvalidArc exception if any arc
in the Unix path is invalid in the host system's path syntax (e.g., an
arc that has a backslash character in it when the host system is
Microsoft Windows).

Note that the syntax of Unix pathnames necessarily limits this
function. It is not possible to specify paths that have a non-empty
volume name or paths that have a slash in one of their arcs using this
function.

[toUnixPath s] converts the path s, which is in the host operating
system's syntax, to a Unix-style path.  If the path s has a non-empty
volume name, then the Path exception is raised. Also, if any arc in
the pathname contains the slash character, then the InvalidArc
exception is raised.

[Discussion]

Syntactically, two paths can be checked for equality by applying
string equality to canonical versions of the paths. Since volumes and
individual arcs are just special classes of paths, an identical test
for equality can be applied to these classes.

*)
