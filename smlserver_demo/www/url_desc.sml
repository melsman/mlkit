val _ = Ns.return `
<html>
<head>
<title>What is an URL</title>
<body bgcolor=white>
<h2>What is an URL?</h2>

A Uniform Resource Locator (URL) is the natural extension to a
<i>filename</i> that extends to the Internet. An URL can reference a
file in a local directory on your desktop PC and it can reference a
file on any PC connected to the Internet. An URL may not only
reference files but also special files like Word dokuments, databases
and programs.<p>

Generally speaking an URL may reference anything that is connected to
the Internet.<p>

Below, we show the most used URL <b>http</b>; it is used to reference
documents through web-servers.

<h2>An HTTP URL</h2>

The HyperText Transport Protocol (HTTP), which is used by web-servers,
are normally used to access hypertext documents (HTML) or programs
that generate HTML documents.<p>

A file named <code>foo.html</code> på a web-server
<code>www.foo.com</code> in the directory <code>/public</code>
is accessed with the following URL:<p>

<pre>
   http://www.foo.com/public/foo.html
</pre>

<h2>Other URL types</h2>

There exists many other URL types and you may find more <a
href="http://www.w3.org/hypertext/WWW/Addressing/Addressing.html">information</a>
on the Internet.

</body>
</html>`

(*
val _ = Ns.return `
<html>
<head>
<title>Hvad er en URL</title>
<body>
<h2>Hvad er en URL?</h2>

URL står for <b>Uniform Resource Locator</b>. En URL kan opfattes som
den naturlige udvidelse af begrebet <i>filnavn</i> til at dække hele
Internettet. Ikke nok med at en URL kan referere til en fil i et
katalog lokalt på din maskine, men den fil kan eksistere på enhver
maskine tilkoblet Internettet. En URL kan også referere til meget
andet en filer. Det kunne f.eks. være dokumenter lagret i en database
eller programmer.<p>

Generelt set, så kan man med en URL referere til alt hvad der findes
på Internettet.<p>

Nedenfor viser vi den mest anvendte URL <b>http</b>, som anvendes til
at referere til dokumenter gennem web-severe.

<h2>En HTTP URL</h2>

HTTP står for HyperText Transport Protocol.  HTTP severe (hvilket
inkluderer web-severe) servicerer normalt hypertekst dokumenter,
dvs. dokumenter med endelsen <tt>htm</tt> eller <tt>html</tt> samt
hypertekst dokumenter som genereres af web-server scripts. <p>

En fil med navnet <code>foo.html</code> på en web-server
<code>www.foo.com</code> i kataloget <code>/offentlig/filer</code>
svarer til følgende URL:<p>

<pre>
   http://www.foo.com/offentlig/filer/foo.html
</pre>

<h2>Andre URL typer</h2>

Der eksisterer mange ander URL typer, og der findes bl.a. denne <a
href="http://www.w3.org/hypertext/WWW/Addressing/Addressing.html">information</a>
på engelsk.
</body>
</html>`
*)