val _ = Ns.return `
<html>
<head>
<title>
Authentication
</title>
</head>
<body bgcolor=white>
<h2>Authentication</h2>

This example uses the database to store user id and passwords. You
must either load the datamodel (file <code>/www/auth/auth.sql</code>)
into Oracle or PostgreSQL.<p>

You are currently using the following pools:
<b>^(Db.Pool.pp())</b>. If this is not what you expect, then check the
file <code>/sml/Db.sml</code>.<p>

<h3>Oracle</h3>

In <code>sqlplus</code> you write:

<blockquote>
<pre>
SQL> @auth.sql
</pre>
</blockquote>

<h3>PostgreSQL</h3>

At the command prompt you write:

<blockquote>
<pre>
# psql -f auth.sql
</pre>
</blockquote>



You may view the <a href="auth/admin/show_cookies.sml">password
protected</a> page or the <a href="auth/www/www.sml">open</a> page.<p>

Back to the <a href="index.msp">example</a> page<p>

<hr>
<a href="http://www.smlserver.org/">SMLserver Home Page</a> (<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-07-29
</body>
</html>`

  