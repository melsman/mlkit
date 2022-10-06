(*Path.sml*)

structure Path : OS_PATH = struct
  exception Path

  (* It would make sense to use substrings for internal versions of
   * fromString and toString, and to allocate new strings only when
   * externalizing the strings.

   * Impossible cases:
     UNIX: {isAbs = false, vol = _, arcs = "" :: _}
     Mac:  {isAbs = true,  vol = _, arcs = "" :: _}
  *)

  local
    val op @ = List.@
    infix 9 sub
    val op sub = String.sub
    val substring = String.extract

  val slash = "/"
  val volslash = "/"
  fun isslash c = c = #"/"
  fun validVol s = s = ""

  fun getVol s =
      if size s >= 1 andalso isslash (s sub 0) then SOME ""
      else NONE

  fun splitabsvolrest s =
      if size s >= 1 andalso isslash (s sub 0) then
	  (true, "", substring(s, 1, NONE))
      else
	  (false, "", s);
  in

  val parentArc  = ".."
  val currentArc = "."

  fun isAbsolute p = #1 (splitabsvolrest p)

  fun isRelative p = not (isAbsolute p);

  fun fromString p =
      case splitabsvolrest p of
	  (false, v,   "") => {isAbs=false, vol = v, arcs = []}

	| (isAbs, v, rest) => {isAbs=isAbs, vol = v,
			       arcs = String.fields isslash rest};

  fun isRoot p =
      case splitabsvolrest p of
	  (true, _, "") => true
	| _             => false;

  fun getVolume p = #2 (splitabsvolrest p);
  fun validVolume{isAbs, vol} = validVol vol;

  fun toString (path as {isAbs, vol, arcs}) =
      let fun h []        res = res
	    | h (a :: ar) res = h ar (a :: slash :: res)
      in
	  if validVolume{isAbs=isAbs, vol=vol} then
	      case (isAbs, arcs) of
		  (false, []         ) => vol
		| (false, "" :: _    ) => raise Path
		| (false, a1 :: arest) =>
		      String.concat (vol :: List.rev (h arest [a1]))

		| (true,  []         ) => vol ^ volslash
		| (true, a1 :: arest ) =>
		      String.concat (List.rev (h arest [a1, volslash, vol]))
	  else
	      raise Path
      end;

  fun concat (p1, p2) =
      let fun stripslash path =
	      if isslash (path sub (size path - 1)) then
		  substring(path, 0, SOME(size path - 1))
	      else path
      in
	  if isAbsolute p2 then raise Path
	  else
	      case splitabsvolrest p1 of
		  (false, "",   "") => p2
		| (false, v,  path) => v ^ stripslash path ^ slash ^ p2
		| (true,  v,  ""  ) => v ^ volslash ^ p2
		| (true,  v,  path) => v ^ volslash ^ stripslash path ^ slash ^ p2
      end;

  fun getParent p =
      let open List
	  val {isAbs, vol, arcs} = fromString p
	  fun getpar xs =
	      rev (case rev xs of
		       []              => [parentArc]
		     | [""]            => if isAbs then [] else [parentArc]
		     | ""   :: revrest => parentArc :: revrest
		     | "."  :: revrest => parentArc :: revrest
		     | ".." :: revrest => parentArc :: parentArc :: revrest
		     | last :: revrest => revrest)
      in
	  case getpar arcs of
	      []   =>
		  if isAbs then toString {isAbs=true, vol=vol, arcs=[""]}
		  else currentArc
	    | arcs => toString {isAbs=isAbs, vol=vol, arcs=arcs}
      end;

  fun mkCanonical p =
      let val {isAbs, vol, arcs} = fromString p
	  fun backup []          = if isAbs then [] else [parentArc]
	    | backup (".."::res) = parentArc :: parentArc :: res
	    | backup ( _ :: res) = res
	  fun reduce arcs =
	      let fun h []         []  = if isAbs then [""] else [currentArc]
		    | h []         res = res
		    | h (""::ar)   res = h ar res
		    | h ("."::ar)  res = h ar res
		    | h (".."::ar) res = h ar (backup res)
		    | h (a1::ar)   res = h ar (a1 :: res)
	      in h arcs [] end
      in
	  toString {isAbs=isAbs, vol=vol, arcs=List.rev (reduce arcs)}
      end;

  fun parentize []      = []
    | parentize (_::ar) = parentArc :: parentize ar;

  fun mkRelative (p1, p2) =
      case (fromString p1, fromString (mkCanonical p2)) of
	  (_ ,                {isAbs=false,...}) => raise Path
	| ({isAbs=false,...}, _                ) => p1
	| ({vol=vol1, arcs=arcs1,...}, {vol=vol2, arcs=arcs2, ...}) =>
	      let fun h [] [] = ["."]
		    | h a1 [] = a1
		    | h [] a2 = parentize a2
		    | h (a1 as (a11::a1r)) (a2 as (a21::a2r)) =
		      if a11=a21 then h a1r a2r
		      else parentize a2 @ (if arcs1 = [""] then [] else a1)
	      in
		  if vol1 <> vol2 then raise Path
		  else toString {isAbs=false, vol="", arcs=h arcs1 arcs2}
	      end;


  fun mkAbsolute (p1, p2) =
      if isRelative p2 then raise Path
      else if isAbsolute p1 then p1
      else mkCanonical(concat(p2, p1));

  fun isCanonical p = mkCanonical p = p;

  fun joinDirFile {dir, file} = concat(dir, file)

  fun splitDirFile p =
      let open List
	  val {isAbs, vol, arcs} = fromString p
      in
	  case rev arcs of
	      []            =>
		  {dir = toString {isAbs=isAbs, vol=vol, arcs=[]}, file = ""  }
	    | arcn :: farcs =>
		  {dir = toString {isAbs=isAbs, vol=vol, arcs=rev farcs},
		   file = arcn}

      end

  fun dir s  = #dir (splitDirFile s);
  fun file s = #file(splitDirFile s);

  fun joinBaseExt {base, ext = NONE}    = base
    | joinBaseExt {base, ext = SOME ""} = base
    | joinBaseExt {base, ext = SOME ex} = base ^ "." ^ ex;

  fun splitBaseExt s =
      let val {dir, file} = splitDirFile s
	  open Substring
	  val (fst, snd) = splitr (fn c => c <> #".") (full file)
      in
	  if isEmpty snd         (* dot at right end     *)
	     orelse isEmpty fst  (* no dot               *)
	     orelse size fst = 1 (* dot at left end only *)
	      then {base = s, ext = NONE}
	  else
	      {base = joinDirFile{dir = dir,
				  file = string (trimr 1 fst)},
	       ext = SOME (string snd)}
      end;

  fun ext s  = #ext  (splitBaseExt s);
  fun base s = #base (splitBaseExt s);

  end
end; (*structure Path*)
