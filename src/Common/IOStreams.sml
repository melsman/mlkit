(*$IOStreams: FINMAP CRASH IO_STREAMS*)
functor IOStreams(structure FinMap: FINMAP
		  structure Crash: CRASH
		 ): IO_STREAMS =
  struct
    datatype Streams =
      STREAMS of {inStreams: (int, instream) FinMap.map,
		  outStreams: (int, outstream) FinMap.map,
		  lastInStreamID: int,
		  lastOutStreamID: int
		 }

    val initialStreams =
      STREAMS{inStreams=FinMap.singleton(0, std_in),
	      outStreams=FinMap.singleton(0, std_out),
	      lastInStreamID=0,
	      lastOutStreamID=0
	     }

   (* This scheme isn't perfect; because we can't remove things from FinMaps
      we just use unique numbers and leave dead streams where they are, so
      the stream store actually grows indefinitely. I'll fix it sometime
      when I *haven't* got a live show and a dozen contemporary dancers to
      direct. *)

    fun openIn streams (file, fail) =
      let
	val inStream =
	  open_in file
	  handle Io _ => (fail(); Crash.impossible "IOStreams.unreachable/1")
					(* Keep the polymorphism of `fail'
					   as spec'd in the signature. *)

	val STREAMS{inStreams, outStreams, lastInStreamID, lastOutStreamID} =
	  streams

	val newID = lastInStreamID + 1
      in
	(newID, STREAMS{inStreams=FinMap.add(newID, inStream, inStreams),
			outStreams=outStreams,
			lastInStreamID=newID,
			lastOutStreamID=lastOutStreamID
		       }
	)
      end      

    fun openOut streams (file, fail) =
      let
	val outStream =
	  open_out file
	  handle Io _ => (fail(); Crash.impossible "IOStreams.unreachable/2")
	
	val STREAMS{inStreams, outStreams, lastInStreamID, lastOutStreamID} =
	  streams

	val newID = lastOutStreamID + 1
      in
	(newID, STREAMS{inStreams=inStreams,
			outStreams=FinMap.add(newID, outStream, outStreams),
			lastInStreamID=lastInStreamID,
			lastOutStreamID=newID
		       }
	)
      end

    fun inputStream (STREAMS{inStreams, ...}) id =
      case FinMap.lookup inStreams id
	of Some stream => stream
	 | None => Crash.impossible ("IOStreams.inputStream: cannot find input \
                   \stream number: " ^ Int.string id)

    fun outputStream (STREAMS{outStreams, ...}) id =
      case FinMap.lookup outStreams id
	of Some stream => stream
	 | None => Crash.impossible ("IOStreams.outputStream: cannot find output\
                   \ stream number: " ^ Int.string id)
  end;
