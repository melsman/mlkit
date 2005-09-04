val _ = Page.return "Checking Form Variables" 
`This example serves to demonstrate the extensive 
support for form-variable checking in 
<a href=http://www.smlserver.org>SMLserver</a>.

<form method=post action=formvar_chk.sml>
<table>
<tr><td>Type an integer <td><input type=text name=int>
<tr><td>Type a positive integer <td><input type=text name=nat>
<tr><td>Type a real <td><input type=text name=real><p>
<tr><td>Type a string <td><input type=text name=str><p>
<tr><td>Type a positive integer in the range [2,...,10] <td><input type=text name=range><p>
<tr><td>Type an email <td><input type=text name=email><p>
<tr><td>Type a name <td><input type=text name=name><p>
<tr><td>Type a login <td><input type=text name=login><p>
<tr><td>Type a phone number <td><input type=text name=phone><p>
<tr><td>Type an URL <td><input type=text name=url><p>
<tr><td>Choose sex <td><select name=sex>
<option value="Female">Female</option>
<option value="Male">Male</option>
<option selected value="Unknown">Unknown</option>
</select>
</table>
<input type=submit value="Submit Entry">
</form>`
