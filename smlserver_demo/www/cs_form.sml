val query = `select id, text from cs`

fun gen_ch_nums s = String.concatWith "," (List.map (Int.toString o Char.ord) (explode s))

fun format (g, s) = `
  <li><form action=cs_upd.sml method=post>
    Id: <b>^(g "id")</b> = <input type=text name=text value="^(g "text")"> (^(gen_ch_nums (g "text")))
    <input type=hidden value="^(g "id")" name="num">
    <input type=submit value="Update Id ^(g "id")">
  </form><p>` ^^ s

val l = Db.fold format `` query

fun ch_row ch 1 = `<td> ^(Int.toString ch) (<b>^(String.str(Char.chr ch))</b>) </td>`
  | ch_row ch n = `<td> ^(Int.toString ch) (<b>^(String.str(Char.chr ch))</b>) </td> ^("\n") ` ^^ (ch_row (ch+1) (n-1))
and ch_tab ch ~1 = ` ` 
  | ch_tab ch n = `<tr> ` ^^ (ch_row ch 8) ^^ ` </tr> ^("\n") ` ^^ (ch_tab (ch+8) (n-8))

val _ = Ns.return (`
  <html>
    <body bgcolor=white>
      <h2>Checking Character Support</h2> 

      Fill out the following input box, and see how it is stored in
      the database.<p>

      <form action=cs_add.sml method=get> 
         <input type=text name=text>
      </form> <p>

      The following lines of text are currently in the database. You
      may update each one of the lines, by clicking the accompanying
      button<p>

      <h2>Content of the Database</h2>

      <ul>` ^^ l ^^ `</ul><p>
     
      <h2>The character set currently used by SMLserver</h2>

      The following table shows the characters (numbered above 31)
      currently used by SMLserver

      <p>
      <blockquote>
      <table> ` ^^ 
      (ch_tab 32 223) ^^ 
      `</table>
      </blockquote>
      <p>

      Back to the <a href="index.sml">example</a> page<p>
      <hr>
      <a href="http://www.smlserver.org/">SMLserver Home Page</a> (<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-07-29

   </body> 
</html>`)

