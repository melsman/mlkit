val kind = FormVar.wrapExn (FormVar.getEnumErr ["WhileUsed","ForAWhile","Size"]) "kind"
  handle _ => "Size"

fun pp_kind kind =
  case kind of
    "Size" => `<b>^kind</b> of size 5`
  | _ => `<b>^kind</b>. Entries live in the cache in
    approximately 20 seconds.<p>`

val _ = Page.return "Caching Demonstration V2" 
  (`
  Cache entries map email addresses to pairs of user ids and names.<p>

  Using cache kind: ` ^^ (pp_kind kind) ^^ `<p>

  The cache has ML type: <code>(string,(int,string)) cache</code><p>

  <table width=100% border=1>
  <tr><th>Lookup Entry</th><th>Add Entry</th></tr>
  <tr><td align=center>
  <form action=cache_lookup_v2.sml>
    <input type=hidden name=kind value="^kind">
    Email <input type=text name=email><p>
    <input type=submit value=Lookup> 
  </form></td>
  <td align=center>
  <form action=cache_add_v2.sml>
    <input type=hidden name=kind value="^kind">
  <table>
  <tr><td align=center>Email</td><td><input type=text name=email></td>
  <tr><td align=center>User id</td><td><input type=text name=uid></td>
  <tr><td align=center>Name</td><td><input type=text name=name></td>
  <tr><td align=center colspan=2> 
      <input type=submit value="Add to Cache"></td>
  </tr>
  </table></td></tr>
  </form>
  </table><p>

    You can choose among the following cache kinds: 
    <a href="cache_v2.sml?kind=Size">Size</a>,
    <a href="cache_v2.sml?kind=WhileUsed">WhileUsed</a>,
    <a href="cache_v2.sml?kind=ForAWhile">ForAWhile</a>`)
