val _ = Page.return "Caching Demonstration" `
  Cache entries map email addresses to names. Entries live in the cache
  in approximately 20 seconds.<p>

  <table width=100% border=1>
  <tr><th>Lookup Entry</th><th>Add Entry</th></tr>
  <tr><td align=center>
  <form action=cache_lookup.sml>
    Email <input type=text name=email><p>
    <input type=submit value=Lookup> 
  </form></td>
  <td align=center>
  <form action=cache_add.sml>
  <table>
  <tr><td align=center>Email</td><td><input type=text name=email></td>
  <tr><td align=center>Name</td><td><input type=text name=name></td>
  <tr><td align=center colspan=2> 
      <input type=submit value="Add to Cache"></td>
  </tr>
  </table></td></tr>
  </form>
  </table>`
