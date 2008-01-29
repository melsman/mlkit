<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
<head>
<META http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>SMLtoJs - A Standard ML to JavaScript Compiler</title>
<link href="base.css" rel="stylesheet" type="text/css">
</head>
<body>
<table border=0 cellpadding=0 cellspacing=0 width=100%>
<tr><td width=75%>
<img src=smltojs_logo_color.png>
<h2>A Standard ML to JavaScript Compiler</h2>
<h4>By <a href="http://www.itu.dk/people/mael">Martin Elsman</a>, 2008-01-29</h4>

<hr>

<h3>Introduction</h3>
<p>
SMLtoJs (pronounced "SML toys") is a compiler from Standard ML to
JavaScript, which allows programmers to enjoy the power of Standard ML
static typing, higher-order functions, pattern matching, and modules
for programming client-side web applications.
</p>

<p>SMLtoJs compiles all of Standard ML, including most of the Standard ML
Basis Library. It also has support for calling JavaScript functions
and for executing JavaScript statements.
</p>

<h3>Features</h3>

<ul>

<li><b>All of Standard ML</b>. SMLtoJs has support for all of Standard ML,
   including modules, pattern matching, higher-order functions,
   generative exceptions, etc.</li>

<li><b>Standard ML Basis Library support.</b> SMLtoJs has support for most of
   the Standard ML basis library, including the following structures:
      <code>Array2 ArraySlice Array Bool Byte Char CharArray CharArraySlice
      CharVector CharVectorSlice Date General Int Int31 Int32 IntInf
      LargeWord ListPair List Math Option OS.Path Pack32Big
      Pack32Little Random Real StringCvt String Substring Text Time
      Timer Vector VectorSlice Word Word31 Word32 Word8 Word8Array
      Word8ArraySlice Word8Vector Word8VectorSlice</code>.
</li>

<li><b>JavaScript integration</b>. SMLtoJs has support for calling JavaScript
   functions and for executing JavaScript statements.</li>

<li><b>Simple DOM access</b>. Simple DOM access and support for installing Standard ML
   functions as DOM event handlers and timer call back functions.</li>

<li><b>Optimization</b>. All Standard ML module language constructs, including functors,
   functor applications, and signature constraints, are eliminated by
   SMLtoJs at compile time. Moreover, SMLtoJs performs a series of
   compile time optimizations, including function inlining and
   specialization of higher-order recursive functions, such as map
   and foldl. Optimizations can be controlled using compile-time
   flags. As a result, SMLtoJs generates fairly efficient JavaScript
   code, although there are rooms for improvements; see below.</li>

<li><b>Reactive Web Programming</b>. SMLtoJs has library support for
Reactive Web Programming. See the <a href="test/rwp_ex1.html">rwp_ex1
example</a> for an introduction to what you can do with this
library. The <a href="test/rwp.sig">simple library API</a> captures
the basic notions of behaviors and event streams as well as the
xoncepts of behavior transformers and event stream transformers.</li>
</ul>

<h3>Online Demonstration</h3>

<p>
To see SMLtoJs in action, click on the examples in the table to the right; there are also
links to the Standard ML source code.
</p>

<h3>Getting the Sources</h3>
<p>
SMLtoJs compiles on Debian Linux systems with MLton or MLKit
installed. The SMLtoJS sources are available through an MLKit
sourceforge svn checkout:
<pre>
 $ svn co https://mlkit.svn.sourceforge.net/svnroot/mlkit/tags/smltojs-4.3.4/kit smltojs
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
If compilation succeeds, an executable
file <a href="smltojs.man.html"><code>bin/smltojs</code></a> should now be available.</p>

<h3>How it Works</h3>
<p>
The SMLtoJs executable <a href="smltojs.man.html"><code>bin/smltojs</code></a> takes as argument an sml-file
(or an mlb-file referencing the sml-files and other mlb-files of the
project) and produces an html file called <code>run.html</code> provided there are
no type errors! The resulting html-file mentions the generated
JavaScript files and a file <code>prims.js</code>, which contains a set of
primitive JavaScript functions used by the generated code.
</p>
<p>
Hint: Adding the flag <code>"-o name"</code> as command-line argument to <a href="smltojs.man.html"><code>smltojs</code></a>
results in the file name.html being generated instead of <code>run.html</code>.
</p>

<h3>Testing that it Works</h3>
<p>
To compile and test the test programs, cd to the <code>js/test</code> directory
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
There is a known issue with a bug in the following test (in some cases, 
the implementation pretty prints reals slightly different than suggested by the spec):
<pre>
  real
  ----
   test13c: WRONG
</pre>
</p>
<p>
There are plenty of possibilities for further improvements, including:
<ol>

<li>Using numbers to represent constructors (instead of strings).</li>

<li>Unboxing of datatypes; currently, a boxed representation is used
   for lists, for example.</li>

</ol>
</p>

<h3>Copyright and License</h3>

<p>SMLtoJs is distributed under the GPL license; see the files in
<code>smltojs/doc/license<code> for copyright notices and detailed
license information.  
</p>

</td>

<td align=right valign=top width="25%">

   <table border=0 cellpadding=3 cellspacing=0 width=95% align=right>
    <tr><td><table border=0 bgcolor="#1f94b7" cellpadding=1 cellspacing=1 width=100% align=right>
         <tr><td><table border=0 bgcolor=white cellpadding=3 cellspacing=0 width=100%>
                   <tr><td bgcolor="#1f94b7"><font color=white><b>News</b></font></td></tr>
                   <tr><td bgcolor="#eeeeee"><font size=-1>
                       <b>2007-11-27: </b>Martin gave a talk in the DIKU TOPPS group (<a href="slides_diku_2007-11-27.pdf">slides</a>).</td></tr>
                   <tr><td bgcolor="#eeeeee"><font size=-1>
                       <b>2007-09-17: </b>Martin gave a talk on the PLS Monday lunch meeting (<a href="slides_lunch_2007-09-17.pdf">slides</a>).</td></tr>
                   <tr><td bgcolor="#eeeeee"><font size=-1>
                       <b>2007-09-05: </b>SMLtoJs <b>version 4.3.4</b> is available.</td></tr>
                   <tr><td bgcolor="#eeeeee"><font size=-1>
                       <b>2007-08-31: </b>SMLtoJs <b>version 4.3.3</b> is available.</td></tr>
                   <tr><td bgcolor="#eeeeee"><font size=-1>
                       <b>2007-08-08: </b>SMLtoJs <b>version 4.3.2</b> is available.</td></tr>
                 </table>
             </td>
         </tr>
        </table></td></tr><tr><td>&nbsp;</td></tr><tr><td>
<table border=0 bgcolor="#1f94b7" cellpadding=1 cellspacing=1 width=100% align=right>
         <tr><td><table border=0 bgcolor=white cellpadding=3 cellspacing=0 width=100%>
                   <tr><td bgcolor="#1f94b7"><font color=white><b>SMLtoJs Examples</b></font></td></tr>
                   <tr><td bgcolor="#eeeeee"><table width=100%><tr><td><font size=-1><b>program</b></font></td><td>

           <font size=-1><b>sources</b></font></td></tr>
<tr><td>
<a href="test/rwp_ex1.html">Ex: rwp_ex1</a> </td><td><a href="test/rwp_ex1.mlb.html">src</a></td></tr><tr><td>
<a href="test/counter.html">Ex: counter</a> </td><td><a href="test/counter.mlb.html">src</a></td></tr><tr><td>
<a href="test/temp.html">Ex: temp</a> </td><td><a href="test/temp.mlb.html">src</a></td></tr><tr><td>
<a href="test/life.html">Ex: life</a> </td><td><a href="test/life.mlb.html">src</a></td></tr><tr><td>
<a href="test/cal.html">Ex: cal</a> </td><td><a href="test/cal.mlb.html">src</a></td></tr><tr><td>
<a href="test/array2.html">Test: array2</a> </td><td><a href="test/array2.sml.html">src</a></td></tr><tr><td>
<a href="test/int31.html">Test: int31</a> </td><td><a href="test/int31.sml.html">src</a></td></tr><tr><td>
<a href="test/int.html">Test: int</a> </td><td><a href="test/int.sml.html">src</a></td></tr><tr><td>
<a href="test/vectorslice.html">Test: vectorslice</a> </td><td><a href="test/vectorslice.sml.html">src</a></td></tr><tr><td>
<a href="test/word8arrayslice.html">Test: word8arrayslice</a> </td><td><a href="test/word8arrayslice.sml.html">src</a></td></tr><tr><td>
<a href="test/word8vector.html">Test: word8vector</a> </td><td><a href="test/word8vector.sml.html">src</a></td></tr><tr><td>
<a href="test/arrayslice.html">Test: arrayslice</a> </td><td><a href="test/arrayslice.sml.html">src</a></td></tr><tr><td>
<a href="test/general.html">Test: general</a> </td><td><a href="test/general.sml.html">src</a></td></tr><tr><td>
<a href="test/int32_2.html">Test: int32_2</a> </td><td><a href="test/int32_2.sml.html">src</a></td></tr><tr><td>
<a href="test/math.html">Test: math</a> </td><td><a href="test/math.sml.html">src</a></td></tr><tr><td>
<a href="test/stringcvt.html">Test: stringcvt</a> </td><td><a href="test/stringcvt.sml.html">src</a></td></tr><tr><td>
<a href="test/testdyn1.html">Test: testdyn1</a> </td><td><a href="test/testdyn1.sml.html">src</a></td></tr><tr><td>
<a href="test/vector.html">Test: vector</a> </td><td><a href="test/vector.sml.html">src</a></td></tr><tr><td>
<a href="test/word8array.html">Test: word8array</a> </td><td><a href="test/word8array.sml.html">src</a></td></tr><tr><td>
<a href="test/array.html">Test: array</a> </td><td><a href="test/array.sml.html">src</a></td></tr><tr><td>
<a href="test/int_2.html">Test: int_2</a> </td><td><a href="test/int_2.sml.html">src</a></td></tr><tr><td>
<a href="test/int32.html">Test: int32</a> </td><td><a href="test/int32.sml.html">src</a></td></tr><tr><td>
<a href="test/listpair.html">Test: listpair</a> </td><td><a href="test/listpair.sml.html">src</a></td></tr><tr><td>
<a href="test/string.html">Test: string</a> </td><td><a href="test/string.sml.html">src</a></td></tr><tr><td>
<a href="test/testmatc.html">Test: testmatc</a> </td><td><a href="test/testmatc.sml.html">src</a></td></tr><tr><td>
<a href="test/word.html">Test: word</a> </td><td><a href="test/word.sml.html">src</a></td></tr><tr><td>
<a href="test/word8.html">Test: word8</a> </td><td><a href="test/word8.sml.html">src</a></td></tr><tr><td>
<a href="test/word31.html">Test: word31</a> </td><td><a href="test/word31.sml.html">src</a></td></tr><tr><td>
<a href="test/word32.html">Test: word32</a> </td><td><a href="test/word32.sml.html">src</a></td></tr><tr><td>
<a href="test/bytechar.html">Test: bytechar</a> </td><td><a href="test/bytechar.sml.html">src</a></td></tr><tr><td>
<a href="test/int31_2.html">Test: int31_2</a> </td><td><a href="test/int31_2.sml.html">src</a></td></tr><tr><td>
<a href="test/intinf.html">Test: intinf</a> </td><td><a href="test/intinf.sml.html">src</a></td></tr><tr><td>
<a href="test/list.html">Test: list</a> </td><td><a href="test/list.sml.html">src</a></td></tr><tr><td>
<a href="test/real.html">Test: real</a> </td><td><a href="test/real.sml.html">src</a></td></tr><tr><td>
<a href="test/substring.html">Test: substring</a> </td><td><a href="test/substring.sml.html">src</a></td></tr><tr><td>
<a href="test/word8vectorslice.html">Test: word8vectorslice</a> </td><td><a href="test/word8vectorslice.sml.html">src</a></td></tr><tr><td>
<a href="test/time.html">Test: time</a> </td><td><a href="test/time.sml.html">src</a></td></tr><tr><td>
<a href="test/date.html">Test: date</a> </td><td><a href="test/date.sml.html">src</a></td></tr><tr><td>
<a href="test/unixpath.html">Test: unixpath</a> </td><td><a href="test/unixpath.sml.html">src</a></td></tr><tr><td>
<a href="test/exec.html">Test: exec</a> </td><td><a href="test/exec.mlb.html">src</a></td></tr><tr><td>
                 </table>
             </td>
         </tr>
        </table>

</td>
</tr>
</table>
</td></tr></table>
</td>
</tr>
</table>
<hr>
<a href="mailto:mael@itu.dk">mael@itu.dk</a>
</body>
</html>
