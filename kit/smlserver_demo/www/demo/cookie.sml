  val cookies = foldl (fn ((n,v),a) => `<li> ^n : ^v ` ^^ a)
		`` (Ns.Cookie.allCookies())

  val _ = Page.return "Cookie Example" 
  (`
  <ul>` ^^ cookies ^^ `</ul>

  Cookies may be added to the list above using the ^`^`Set 
  Cookie'' form. The name and value attributes are 
  mandatory and are sequences of characters. The character 
  sequences are automatically URL-encoded, thus it is 
  legal to include semi-colon, comma, and white space in 
  both name and value. <p>

  A cookie is removed from the browser when the expiration 
  date is reached.  The life time of a cookie with no 
  expiry attribute is the user's session. Life times are 
  given in seconds; the program computes an expiration 
  date based on the current time and the specified life 
  time. A cookie may be removed by specifying a negative 
  life time or by using the ^`^`Delete Cookie'' form. <p>

  A cookie may be specified to be secure, which means that 
  the cookie is transmitted on secure channels only (e.g., 
  HTTPS requests using SSL). A value of "No" means that 
  the cookie is sent in clear text on insecure channels 
  (e.g., HTTP requests).<p>

  <form method=post action=cookie_set.sml>
  <table>
  <tr><td>Name<td>Value<td>Life Time<td>Secure<td>&nbsp
  <tr>
  <td><input type=text value="foo" size=10 name=cookie_name>
  <td><input type=text value="bar" size=10 name=cookie_value>
  <td><input type=text value="60" size=10 name=cookie_lt>
  <td><select name=cookie_secure>
	     <option value="Yes">Yes</option> 
	     <option selected value="No">No</option>
	  </select>
  <td><input type=submit value="Set Cookie">
  </tr>
  </table>
  </form>

  <form method=post action=cookie_delete.sml>
  <table>
  <tr><td>Name<td>&nbsp;</tr>
  <tr>
  <td><input type=text value="foo" name=cookie_name>
  <td><input type=submit value="Delete Cookie">
  </tr>
  </table>
  </form>`)
