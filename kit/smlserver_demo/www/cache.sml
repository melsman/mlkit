val _ = Ns.Quot.return `
<html>
  <body bgcolor=white>
  <h1>Caching Demonstration</h1>
  This service demonstrates the caching interface in SMLserver.<p>
  You add login and passwords to the cache. They will live in the cache
  in approximately 20 seconds.<p> You can lookup a login as long as it is in the cache.<p>

  <h2>Lookup Entry</h2>
    <form action=cache_lookup.sml>
      <b>Login:</b> <input type=text name=login>
    </form>
  <hr>
  <h2>Add Entry</h2>
    <form action=cache_add.sml>
    <table><tr>
  <th>Login:</th> <td><input type=text name=login></td></tr>
  <th>Password:</th> <td><input type=password name=passwd></td></tr>
  <td align=center colspan=2> <input type=submit value="Add to Cache"></td>
  </table>
    </form>
  <hr> <i>Served by SMLserver</i>
  </body>
</html>`
