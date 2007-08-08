<?
$title = "SMLtoJs - A Standard ML to JavaScript Compiler"
?>

<html>
<head><title><? echo $title ?></title></head>
<body>
<h2><? echo $title ?></h2>

<h4>By Martin Elsman, 2007-08-08</h4>

<hr>

<p>

</p>

<h3>Introduction</h3>
<p>
SMLtoJs is a compiler from Standard ML to JavaScript, which allows
programmers to enjoy the power of Standard ML static typing,
higher-order functions, pattern matching, and modules for programming
client-side web-applications.
</p>

<p>SMLtoJs compiles all of Standard ML, including most of the Standard ML
Basis Library. It also has support for calling JavaScript functions
and for executing JavaScript statements.
</p>

<h3>Features</h3>

<ul>

<li><b>All of Standard ML.</b> SMLtoJs has support for all of Standard ML,
   including modules, pattern matching, higher-order functions,
   generative exceptions, etc.</li>

<li><b>Standard ML Basis Library support.</b> SMLtoJs has support for most of
   the Standard ML basis library, including the following structures:
<quote>
      <code>Array2 ArraySlice Array Bool Byte Char CharArray CharArraySlice
      CharVector CharVectorSlice General Int Int31 Int32 IntInf
      LargeWord ListPair List Math Option Pack32Big Pack32Little Real
      StringCvt String Substring ArraySlice Text Vector VectorSlice
      Word Word31 Word32 Word8 Word8Array Word8ArraySlice Word8Vector
      Word8VectorSlice</code>
</quote></li>

<li><b>JavaScript integration.</b> SMLtoJs has support for calling JavaScript
   functions and for executing JavaScript statements.</li>

<li><b>Simple DOM access.</b> Simple DOM access and support for installing Standard ML
   functions as DOM event handlers and timer call back functions.</li>

<li><b>Optimization.</b> All Modules language constructs, including functors,
   functor applications, and signature constraints, are eliminated by
   SMLtoJs at compile time. Moreover, SMLtoJs performs a series of
   compile time optimizations, including function inlining and
   specialization of higher-order recursive functions, such as map
   and foldl. Optimizations can be controlled using compile-time
   flags. As a result, SMLtoJs generates fairly efficient JavaScript
   code, although there are rooms for improvements; see below.</li>

</ul>

<h3>Online Demonstration</h3>

<p>
To see SMLtoJs in action, click on the examples below; there are also
links to the Standard ML source code.
</p>

<h3>Getting the Sources</h3>
<p>
SMLtoJs compiles on Debian Linux systems with MLton or MLKit
installed. The SMLtoJS sources are available through an MLKit
sourceforge svn checkout:
<pre>
 $ svn co https://mlkit.svn.sourceforge.net/svnroot/mlkit/tags/smltojs-4.3.3/kit smltojs
</pre>
This command copies the sources to the directory smltojs.
</p>

<h3>Building SMLtoJs</h3>
<p>
To compile SMLtoJs from the sources (MLKit svn checkout;
see above), simply type
<pre>
  $ cd smltojs
  $ ./autobuild
  $ ./configure
  $ make smltojs
</pre>
from within the <code>kit/</code> directory. If compilation succeeds, an executable
file <code>kit/bin/smltojs</code> should now be available. If you wish to install
SMLtoJs on your system, type (as user root)
<pre>
  $ make install_smltojs
</pre>
</p>

<h3>How it Works</h3>
<p>
The SMLtoJs executable <code>kit/bin/smltojs</code> takes as argument an sml-file
(or an mlb-file referencing the sml-files and other mlb-files of the
project) and produces an html file called <code>run.html</code> provided there are
no type errors! The resulting html-file mentions the generated
JavaScript files and a file <code>prims.js</code>, which contains a set of
primitive JavaScript functions used by the generated code.
</p>
<p>
Hint: Adding the flag <code>"-o name"</code> as command-line argument to <code>smltojs</code>
results in the file name.html being generated instead of <code>run.html</code>.
</p>

<h3>Testing that it Works</h3>
<p>
To compile and test the test programs, cd to the <code>kit/js/test</code> directory
and run <code>"make clean all"</code>:
<pre>
 $ cd js/test
 $ make clean all
</pre>
You can now start Firefox on the generated html-files; the file
all.html includes links to all the test files:
<pre>
 $ firefox all.html
</pre>
The examples temp.html, counter.html, and life.html are the most
interesting examples at the moment (more will come).
</p>

<h3>Issues</h3>
<p>
Currently, SMLtoJs does not implement tail-calls properly, as all
calls are translated into JavaScript calls. We are planning in the
near future to implement tail-calls using trampolines.
</p>
<p>
There is a known issue with a bug in the following tests:
<pre>
string
------
 test23: WRONG
 test24: WRONG

intinf
------
 test20e: overflow...

bytechar
--------
 test42: WRONG
 test43: WRONG
</pre>

<h3>Copyright and License</h3>

<p> SMLtoJs is distributed under the GPL license; see the files in
<code>smltojs/doc/license<code> for copyright notices and detailed
license information.  </p>

<hr>
<a href="mailto:mael@itu.dk">mael@itu.dk</a>
</body>
</html>