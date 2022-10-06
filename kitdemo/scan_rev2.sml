local
  structure F = Posix.FileSys
  exception NotBalanced

  fun scan fd : int * int =
    let
      fun next () = Byte.bytesToString(Posix.IO.readVec(fd,1))
      fun up (lev, ins) = if lev > 0 then ins + 1
                          else ins

      (* n   : characters read from 'fd'
         ins : characters inside comments
         lev : current number of unmatched (*
         s   : next input character or empty *) *)

      fun count (p as (n,ins,lev,s:string)) =
        case s of
          "" => (* end of stream: *) p
        | "(" => after_lparen(n+1,ins,lev,next())
        | "*" => after_star(n+1,up(lev,ins),lev,next())
        | _  => count(n+1,up(lev,ins), lev,next())
      and after_lparen (p as (n,ins,lev,s)) =
        case s of
          "" => p
        | "*" => count(n+1,ins+2, lev+1,next())
        | "(" => after_lparen(n+1,up(lev,ins),lev,next())
        | _ => count(n+1,up(lev,up(lev,ins)),lev,next())
      and after_star (p as (n,ins,lev,s)) =
        case s of
          "" => p
        | ")" => if lev > 0 then
                    count(n+1,ins+1,lev-1,next())
                 else raise NotBalanced
        | "*" => after_star(n+1,up(lev,ins), lev,next())
        | "(" => after_lparen(n+1,ins,lev,next())
        | _  => count(n+1,up(lev,ins),lev,next())

      val (n,ins,lev,_) = count(0,0,0,next())

    in if lev = 0 then (n,ins) else raise NotBalanced
    end

  fun report_file (filename, n, ins) =
      writeln(concat[filename, ": size = ", Int.toString n,
                     " comments: ", Int.toString ins, " (",
                     (Int.toString(percent(ins, n))
                      handle _ => "-"), "%)"])

  (* scan_file(filename) scans through the file named filename
     returning either SOME(size_in_bytes, size_of_comments) or, in
     case of an error, NONE. In either case a line of information is
     printed. *)

  fun scan_file filename : (int*int) option =
      let val fd = F.openf (filename, F.O_RDONLY, F.O.flags[])
      in let val (n, ins) = scan fd
         in Posix.IO.close fd;
            report_file (filename^"", n, ins);
            SOME (n, ins)
         end handle NotBalanced =>
                    (writeln (filename ^ ": not balanced");
                     Posix.IO.close fd;
                     NONE)
      end handle IO.Io {name,...} =>
                 (writeln (name ^ " failed."); NONE)

  (* main() reads a filename from stdin. The file is scanned using
     scan_file 50 times and each time a summary report is printed *)

  fun main () : unit =
      case readWord F.stdin of
          SOME filename =>
	  let fun do_it 0 = ()
	        | do_it n = (scan_file filename; do_it (n-1))
	  in do_it 50
	  end
        | NONE => ()
in
  val result = main()
end
