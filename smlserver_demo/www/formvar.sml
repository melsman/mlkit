val _ = ScsPage.returnPg "Checking Form Variables" `
<form method=post action=formvar_chk.sml>
Type an integer: <input type=text name=int><p>
Type a positive integer: <input type=text name=nat><p>
Type a real: <input type=text name=real><p>
Type a string: <input type=text name=str><p>
Type a positive integer in the range [2,...,10]: <input type=text name=range><p>
Type an email : <input type=text name=email><p>
Type a name: <input type=text name=name><p>
Type a login: <input type=text name=login><p>
Type a phone number: <input type=text name=phone><p>
Type an URL: <input type=text name=url><p>
Type a CPR number: <input type=text name=cpr><p>
Choose sex: <select name=sex>
<option value="Female">Female</option>
<option value="Male">Male</option>
<option selected value="Unknown">Unknown</option>
</select><p>
Type in a date <input type=text name=date><p>

<input type=submit value="Submit Entry">
</form>`
