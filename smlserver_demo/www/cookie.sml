fun datefmt date = Date.fmt "%a, %d-%b-%Y %H:%M:%S GMT" date

val _ = Ns.return (`
<html>
<body bgcolor=white>

<h1>Cookie Example</h1>

Current GMT date is: <b>^(datefmt(Date.fromTimeUniv(Time.fromSeconds(Time.toSeconds(Time.now())+60))))</b><p>

<form method=post action=cookie_set.sml>
<h3>Adding a Cookie</h3>

The name and value attributes are mandatory and are a sequence of
characters. The character sequences are automatically URL--encoded and
it is therefore legal to include semi--colon, comma and white space in
both the name and value:<p>

Cookie name: <input type=text value="CookieName" name=cookie_name><p>
Cookie value: <input type=text value="Example text" name=cookie_value><p>

A cookie is removed from the browser when the expiration date has been
reached.  The date string format is :

<pre>
    Wdy, DD-Mon-YYYY HH:MM:SS GMT
</pre>

The life time of a cookie with no expiry attribute is the user's
session only. You type in the life time (<i>lt</i>) in seconds, and
the program computes an expiration date being <i>lt</i> seconds in the
future. You remove a cookie by specifying a negative life time.<p>

Cookie Life Time: <input type=text value="60" name=cookie_lt> seconds<p> 

You can set a cookie to be secure, which means that the cookie is
transmitted on secure channels only (e.g., HTTPS requsts using
SSL). If you choose "No", then the cookie is sent in the clear on
unsecured channels (e.g., HTTP requests).<p>

Secure Cookie (<b>Seems to be broken???</b>): 
  <select name=cookie_secure>
    <option value="Yes">Yes</option> 
    <option selected value="No">No</option>
  </select><p> 

<input type=submit value="Set Cookie"> 
</form><p>

<hr>

<form method=post action=cookie_delete.sml>
<h3>Deleting a Cookie</h3>
Cookie name: <input type=text value="CookieName" name=cookie_name><p>

<input type=submit value="Delete Cookie">
</form><p>

<hr>

<h3>The list of all cookies</h3>
<blockquote>
<pre>`
^^ (List.foldl (fn ((n,v),a) => `^n == ^v <br>` ^^ a) `` Ns.Cookie.allCookies) ^^ `
</pre>
</blockquote>
</body>
</html>
`)

