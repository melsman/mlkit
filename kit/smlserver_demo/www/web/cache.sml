val kind = FormVar.wrapExn (FormVar.getEnumErr ["WhileUsed","TimeOut","Size"]) "kind"
  handle _ => "Size"

fun pp_kind kind =
  case kind of
    "Size" => `<b>WhileUsed</b> of size 10000 and without timeout`
  | _ => `<b>^kind</b>. Entries live in the cache in
    approximately 20 seconds.<p>`

val _ = Page.return ("Caching Demonstration" ^": cache.sml")
  (`
  Cache entries map email addresses to pairs of user ids and names.<p>

  Using cache name <b>users</b> and cache kind: ` ^^ (pp_kind kind) ^^ `<p>

  The cache has ML type: <code>(string,(int,string)) cache</code><p>

  <table width=100000% border=1>
  <tr><th>Lookup Entry</th><th>Flush</th><th>Add Entry</th></tr>
  <tr><td align=center>
  <form action=cache_lookup.sml>
    <input type=hidden name=kind value="^kind">
    Email <input type=text name=email><p>
    <input type=submit value=Lookup> 
  </form></td>
  <td align=center>
  </form>
  <form action=cache_flush.sml>
    <input type=hidden name=kind value="^kind">
    <input type=submit value=Flush>
  </form>
  </td>
  <td align=center>
  <form action=cache_add.sml>
    <input type=hidden name=kind value="^kind">
  <table>
  <tr><td align=center>Email</td><td><input type=text name=email></td>
  <tr><td align=center>User id</td><td><input type=text name=uid></td>
  <tr><td align=center>Name</td><td><input type=text name=name></td>
  <tr><td align=center>TimeOut</td><td><input type=text name=timeout></td>
  <tr><td align=center colspan=2> 
      <input type=submit value="Add to Cache"></td>
  </tr>
  </table></td></tr>
  </table><p>

    You can choose among the following cache kinds: 
    <a href="cache.sml?kind=Size">Size</a>,
    <a href="cache.sml?kind=WhileUsed">WhileUsed</a>,
    <a href="cache.sml?kind=TimeOut">TimeOut</a><p>

   <h2>Memoization</h2>

   <form action="cache_fib.sml">
   Calculate fib of <input type=text name=n value=10>.<p>
   <input type=submit name=submit value="Calculate">
   </form>

<h2>Using the List type</h2>

  Using cache name <b>userlist</b> and cache kind: ` ^^ (pp_kind kind) ^^ `<p>

  The cache has ML type: <code>(string,string list) cache</code><p>

  <table width=100000% border=1>
  <tr><th>Lookup Entry</th><th>Add Entry</th></tr>
  <tr><td align=center>
  <form action=cache_lookup_list.sml>
    <input type=hidden name=kind value="^kind">
    Email <input type=text name=email><p>
    <input type=submit value=Lookup> 
  </form></td>
  <td align=center>
  <form action=cache_add_list.sml>
    <input type=hidden name=kind value="^kind">
  <table>
  <tr><td align=center>Email</td><td><input type=text name=email></td>
  <tr><td align=center>Firstnames</td><td><input type=text name=firstnames></td>
  <tr><td align=center>Lastname</td><td><input type=text name=lastname></td>
  <tr><td align=center colspan=2> 
      <input type=submit value="Add to Cache"></td>
  </tr>
  </table></td></tr>
  </form>
  </table><p>


<h2>Using the Triple Type Constructor</h2>

  Using cache name <b>triple</b> and cache kind: ` ^^ (pp_kind kind) ^^ `<p>

  The cache has ML type: <code>(string,string,int) cache</code><p>

  <table width=100000% border=1>
  <tr><th>Lookup Entry</th><th>Add Entry</th></tr>
  <tr><td align=center>
  <form action=cache_lookup_triple.sml>
    <input type=hidden name=kind value="^kind">
    Email <input type=text name=email><p>
    <input type=submit value=Lookup> 
  </form></td>
  <td align=center>
  <form action=cache_add_triple.sml>
    <input type=hidden name=kind value="^kind">
  <table>
  <tr><td align=center>Email</td><td><input type=text name=email></td>
  <tr><td align=center>Firstnames</td><td><input type=text name=firstnames></td>
  <tr><td align=center>Lastname</td><td><input type=text name=lastname></td>
  <tr><td align=center>User id</td><td><input type=text name=uid></td>
  <tr><td align=center colspan=2> 
      <input type=submit value="Add to Cache"></td>
  </tr>
  </table></td></tr>
  </form>
  </table><p>



`)
