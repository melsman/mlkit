<html>
<head>
  <title>SSL Test Page for the nsopenssl module</title>
</head>
<body>

<font face="Verdana, Arial">

<h2>SSL Test Page for the nsopenssl module</h2>


<p>(Copy this ADP page to your pageroot and run.)


<table border=1 cellspacing=0>

<tr><td><font color=red>ns_openssl info</font></td><td>
<%=[ns_openssl info]%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert exists</font></td><td>
<%
  if {[ns_openssl clientcert exists]} {
        ns_puts "Client cert exists"
  } else {
        ns_puts "Client cert does NOT exist"
  }
%>
</td></tr>

<tr><td><font color=red>ns_openssl protocol</font></td><td>
<%=[ns_openssl protocol]%>
</td></tr>

<tr><td><font color=red>ns_openssl cipher name</font></td><td>
<%=[ns_openssl cipher name]%>
</td></tr>

<tr><td><font color=red>ns_openssl cipher strength</font></td><td>
<%=[ns_openssl cipher strength]%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert valid</font></td><td>
<%
  if {[ns_openssl clientcert valid]} {
        ns_puts "Client cert is valid"
  } else {
        ns_puts "Client cert is NOT valid"
  }
%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert version</font></td><td>
<%=[ns_openssl clientcert version]%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert serial</font></td><td>
<%=[ns_openssl clientcert serial]%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert subject</font></td><td>
<%
  set var [ns_openssl clientcert subject]
  ns_puts "$var"
%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert issuer</font></td><td>
<%
  set var [ns_openssl clientcert issuer]
  ns_puts "$var"
%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert notbefore</font></td><td>
<%=[ns_openssl clientcert notbefore]%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert notafter</font></td><td>
<%=[ns_openssl clientcert notafter]%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert signature_algorithm</font></td><td>
<%=[ns_openssl clientcert signature_algorithm]%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert key_algorithm</font></td><td>
<%=[ns_openssl clientcert key_algorithm]%>
</td></tr>

<tr><td><font color=red>ns_openssl clientcert pem</font></td><td>
<%=[ns_openssl clientcert pem]%>
</td></tr>

</table>

<p>Copyright &copy; 2000 by Scott S. Goodwin
<p>Send feedback, bugs and comments to <a href="mailto:scott@scottg.net">me</a>.

</font>

</body>
</html>















