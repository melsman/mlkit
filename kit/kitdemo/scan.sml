local
  exception NotBalanced
  fun scan(is: instream) : int*int =
    let
      fun next() = input(is, 1)
      fun up(level,inside) = if level>0 then inside+1 
                             else inside

      (* n: characters read in 'is'
         inside: characters belonging to comments
         level : current number of unmatched (* 
         s     : next input character or empty *)*)

      fun count(p as (n,inside,level,s:string))=
        case s of
          "" => (* end of stream: *) p
        | "(" => after_lparen(n+1,inside,level,next())
        | "*" => after_star(n+1,up(level,inside),level,next())
        | ch  => count(n+1,up(level,inside), level,next())
      and after_lparen(p as (n,inside,level,s))=
        case s of
          "" => p
        | "*" => count(n+1,inside+2, level+1,next())
        | "(" => after_lparen(n+1, up(level,inside), level,next())
        | ch => count(n+1,up(level,up(level,inside)),level,next())
      and after_star(p as (n,inside,level,s)) =
        case s of
          "" => p
        | ")" => if level>0 then
                    count(n+1,inside+1,level-1,next())
                 else raise NotBalanced
        | "*" => after_star(n+1,up(level,inside), level,next())
        | "(" => after_lparen(n+1,inside,level,next())
        | ch  => count(n+1,up(level,inside),level,next())
    
      val (n, inside,level,_) = count(0,0,0,next())
    in
     if level=0 then (n,inside) else raise NotBalanced
    end
  
  fun report_file(filename, n, inside) = 
      writeln(implode[filename , ": size = " , toString n , 
               " comments: ", toString inside, " (",
               toString(percent(inside, n)) handle Quot => "", 
               "%)"]);

  (* scan_file(filename) scans through the file named
     filename returning either Some(size_in_bytes, size_of_comments)
     or, in case of an error, None. In either case a line of
     information is printed. *)

  fun scan_file (filename: string) : (int*int)Option=
   let val is  = open_in filename 
   in let val (n,inside)  = scan is
      in close_in is; 
         report_file(filename, n, inside);
         Some(n,inside)
      end handle NotBalanced => 
          (writeln(filename ^ ": not balanced");
           close_in is;
           None)
   end handle Io msg  => (writeln msg; None)
  
  fun report_totals(n,inside) = 
       writeln(implode["\n\nTotal sizes: ", toString n, 
            " comments: ", toString inside,
            " (", toString(percent(inside,n)) handle Quot => "",
            ")%"])

  (* main(is) reads a sequence of filenames from is,
     one file name pr line (leading spaces are skipped;
     no spaces allowed in file names). Each file is 
     scanned using scan_file after which a summary
     report is printed *)

  fun main(is: instream):unit =
  let 
    fun driver(p as(None,n,inside)) = 
           (report_totals(n, inside); p)
      | driver(p as (Some filename,n:int,inside:int)) =
          driver(case scan_file filename of
                   Some(n',inside') =>
                   (resetRegions p; 
                    (readWord(is), n+n',inside+inside'))
                | None => (resetRegions p;
                           (readWord(is),n,inside)))
  in
    driver(readWord(is),0,0);
    ()
  end

in 
  val result = main(std_in)
end


