  (*
  fun returnPage title pic body = Web.return 
    `<html> 
      <head><title>^title</title></head>
      <body bgcolor=white> <center> 
      <h2>^title</h2> <img src=^pic> <p>
       ^(Quot.toString body) <p> <i>Served by <a 
          href=http://www.smlserver.org>SMLserver</a>
      </i> </center> </body>
     </html>`
*)
  fun returnPage title pic body = 
        Page.return title `<center><img src=^pic> <p> ^(Quot.toString body) </p></center>` 

  fun mk_form (n:int) = 
    `<form action=guess.sml method=post>
       <input type=hidden name=n value=^(Int.toString n)>
       <input type=text name=guess>
       <input type=submit value=Guess>
     </form>`

  val _ =
    case FormVar.wrapOpt FormVar.getNatErr "n"
      of NONE => 
         returnPage "Guess a number between 0 and 100"
           "bill_guess.jpg"
           (mk_form (Random.range(0,100) (Random.newgen())))
           
       | SOME n =>
         case FormVar.wrapOpt FormVar.getNatErr "guess"
           of NONE => 
             returnPage "You must type a number - try again"
               "bill_guess.jpg" (mk_form n) 
            | SOME g =>
             if g > n then
               returnPage "Your guess is too big - try again"
                 "bill_large.jpg" (mk_form n) 
             else if g < n then
               returnPage "Your guess is too small - try again"
                 "bill_small.jpg" (mk_form n) 
             else 
               returnPage "Congratulations!" "bill_yes.jpg"
                 `You guessed the number ^(Int.toString n) <p>
                  <a href=guess.sml>Play again?</a>`
