let
  val s = (Db.fold ((fn (g, s) => s ^^ `
    <li><b>^(g "name")</b>:
    <ul>` ^^ 
    (Db.fold ((fn (g, s) => s ^^ `<li>^(g "name")(^(g "email"))^("\n")`),``,
              Ns.Quot.flatten (`select users.name, users.email from users, user_group 
                                where users.id = user_group.user_id 
                                and user_group.group_id = '^(g "id")'`))) ^^
    `</ul>`),``,"select id, name from groups order by name"))

in
  Ns.Quot.return (`
  <html>
    <body bgcolor=white>
      <h2>User Group Example</h2> 

      This example shows how you can nest an inner fold in an outer
      fold without considering which database pool is in use. The
      system tries to get a database handle from a pool that is
      currently not in use. The exception <code>DbPoolError</code> is
      raised, if no pool is available.<p>

      We have the following pools available: <b>^(Db.Pool.pp())</b>. <p>

      This nested bullet-list is generated with two nested Db.fold's:<p>

      <ul>` ^^ s ^^
      `</ul>
  
      <p>
      Back to the <a href="index.msp">example</a> page<p>
      <hr>
      <a href="http://www.smlserver.org/">SMLserver Home Page</a> (<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-07-29

   </body> 
</html>`)
end
handle _ (*Db.Pool.DbPoolError err*) => 
  Ns.Quot.return `
  <html>
    <body bgcolor=white>
      <h2>User Group Example</h2> 

      We have the following pools available: <b>^(Db.Pool.pp())</b>. <p>

      We got an <code>DbPoolError</code> exception with value : err <p>

      You should check your configuration file: ^(Ns.Info.configFile())<p>
  
      <p>
      Back to the <a href="index.msp">example</a> page<p>
      <hr>
      <a href="http://www.smlserver.org/">SMLserver Home Page</a> (<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-07-29

   </body> 
</html>`



