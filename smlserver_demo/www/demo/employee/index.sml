val _ = Page.return "Search the Employee Database" `
     <center>
       <form action=search.sml method=post>
         Email: <input type=text name=email>
         <input type=submit value=Search>
       </form>
     </center>`

