local

  (* Library files; loaded and cached by AOLserver *)

  ../sml/Ns.sml              
  ../sml/MOSML_CGI.sml       
  ../sml/Mosmlcgi.sml
  ../sml/MSP.sml
  ../sml/Msp.sml
in
 [

  (* Leaf-files; may make use of declarations in library files,
   * but cannot refer to other leaf-files. *)

  yellow.sml                 
  hello.sml
  show.sml

  hello.msp
  calendar.msp
  test.msp
  index.msp
  logtofile.msp
  fileindex.msp
  dir.msp
 ]
end
