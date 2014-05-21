---
layout: page
title: CV
group: navigation
---
{% include JB/setup %}

#### Background Information

<table border="0" width="100%" padding="5%" spacing="0">
<tr valign="top">
<td width="35%"><b>Position:</b></td><td>Associate Professor, Department of Computer Science,
University of Copenhagen, Denmark. <a href="http://www.hiperfit.dk">HIPERFIT</a>
Research Center Manager.
</td></tr>
<tr valign="top">
<td width="35%"><b>Email:</b></td><td>mael at elsman.com</td>
</tr>
<tr valign="top">
<td width="35%"><b>Home page:</b></td><td><a href="http://www.elsman.com">http://www.elsman.com</a></td>
</tr>
<tr valign="top">
<td width="35%"><b>Private address:</b></td><td>Peter Rørdams Vej 24, 2800 Lyngby, Denmark</td>
</tr>
<tr valign="top">
<td width="35%"><b>Mobile phone:</b></td><td>+45 2612 2212</td>
</tr>
<tr valign="top">
<td width="35%"><b>Date of Birth:</b></td><td>1969-12-22</td>
</tr>
</table>
<br/>
__Previous positions:__

<table border="0" width="100%" padding="5%" spacing="0">
<tr valign="top">
<td width="35%">May 2012 -</td><td>Associate Professor, Department of Computer Science, University of Copenhagen, Denmark. HIPERFIT Research Center Manager.</td>
</tr>
<tr valign="top">
<td>May 2008 - April 2012</td><td>Chief System Development Consultant, Team Leader, [SimCorp](http://www.simcorp.com).</td>
</tr>
<tr valign="top">
<td>April 2003 - April 2008</td><td>Associate Professor at The IT University of Copenhagen (ITU). From March 2004 to August 2005, Vice Head of the Department of Innovation, ITU. From August 2005 to January 2006, Head of the Department of Innovation, ITU. From August 2007 - April 2008, Head of the SDT study programme.</td>
</tr>
<tr valign="top">
<td>February 2006 - August 2006</td><td>On leave from ITU: CTO at Zecure.</td>
</tr>
<tr valign="top">
<td>January 2000 - March 2003</td><td>Assistant Professor at The Royal Veterinary and Agricultural University of Denmark (part time at ITU).</td>
</tr>
<tr valign="top">
<td>January 1999 - December 1999</td><td>Postdoctoral research position at University of California, Berkeley.</td>
</tr>
<tr valign="top">
<td>May 1995 - December 1998</td><td>Ph.D. student at the Department of Computer Science, University of Copenhagen (DIKU).</td>
</tr>
</table>

#### Research and Development

I am Associate Professor at the Department of Computer Science,
University of Copenhagen, where I am also Center Manager for the
HIPERFIT Research Center, which, with special attention to the finance
industry, focuses on solving problems within the domains of
high-performance computing and big data.

From May 2008 to April 2012, I worked in SimCorp as Chief System
Development Consultant. From April 2009, I was team leader for the
SimCorp Instrument Modeling Language team. The team focused on
providing support for quick time-to-market financial instruments,
written in a domain specific ML-like programming language and
integrated into a .NET development platform [2014].

I have conducted research in the design and implementation of
high-level programming languages. My research interests have been in
the areas of module systems, platforms and type systems for Web
applications, program optimisation, type systems for static memory
management, transient fault tolerant computing, garbage collection
techniques, and instruction selection for virtual machines and
different microprocessors, including the x86 microprocessor. I have
also done work in the area of constraint-based program analyses.

From February 2006 until August 2006, I was on leave from ITU and
served as CTO at Zecure, a small company focussing on solutions for
Internet payment processing and e-commerce fraud detection.

I am a (co-)developer of the following tools:

* [SMLtoJs](http://www.smlserver.org/smltojs). A compiler from
Standard ML to JavaScript. SMLtoJs allows for building AJAX
applications using a statically typed language. It compiles all of
Standard ML and has support for calling JavaScript functions and for
executing JavaScript statements. It also allows for type safe
integration with SMLserver using, for instance, XML-RPC; see
below. SMLtoJs may compile itself - the result is [a Standard ML
compiler running in a browser](http://www.smlserver.org/ide) [2011].

* [SMLserver](http://www.smlserver.org). An efficient multi-threaded
Web server platform for Standard ML programs
[2004,2003b,2002,2002b]. The aim of the SMLserver project has been to
push advanced programming concepts, such as higher-order functions and
polymorphic type systems, to the context of Web applications. The
development of SMLserver has been driven by practical needs. SMLserver
is used as the basic Web platform for a series of Web services at the
IT University of Copenhagen, including a course evaluation system, an
online course registration system, an alumni database, and the
employee and student database and search facility (more than 250.000
lines of Standard ML). SMLserver has been built as a Web server module
(for Apache or AOLserver) and has support for efficient pooling of
database connections for Oracle (based on OCI), Postgres, and MySQL
databases.

* [MLKit](http://sourceforge.net/apps/mediawiki/mlkit). An optimizing
compiler for the programming language Standard ML [2002c]. In the
context of the MLKit, I have been working on combining dynamic garbage
collection techniques and region-based memory management to gain a
higher degree of control over memory resources
[2004b,2003a,2002a]. Whereas the runtime system of the MLKit is
written in C and assembler, the MLKit compiler itself is written in
Standard ML.

* Carillon. A tool to find Y2K problems in C programs [1999a]. 

I have also participated in a research project on bigraphical
programming languages, which aims at modeling and controling
connectivity and locality for mobile and distributed systems
[2006]. Moreover, I have done work in the area of code generation for
transient fault tolerance [2007].

To ease administration of teaching, I have developed an advanced
generic Web-based system, called
[CourseGrader](https://sourceforge.net/projects/coursegrader/), for
managing course home work assignments and grades. The system is based
on a combination of TCL and Oracle and makes highly use of PL/SQL for
efficient online computing of grades based on a set of
course-configurable rules.

#### Research at Berkeley

From January 1999 until December 1999, I possessed a postdoctoral
research position at University of California, Berkeley, where I
worked with Associate Professor Alex Aiken. The work involved getting
automatic program analyses to scale to large programs. One example
application that we have developed is Carillon, a tool for finding Y2K
problems in C programs [1999a]. Carillon has been used to locate a Y2K
glitch in CVS version 1.9 (fixed in version 1.10), a version-control
system widely used in the open-source software community.

#### Ph.D in Computer Science

I obtained my Ph.D. degree from the Department of Computer Science,
University of Copenhagen (DIKU) in December 1998. My Ph.D. supervisor
was Associate Professor Mads Tofte. Members of the thesis committee
was Xavier Leroy, INRIA Rocquencourt; Greg Morrisett, Cornell
University; and Hanne Riis Nielson, Aahus University (Chair).

My Ph.D. thesis [1999b] is titled ''Program Modules, Separate
Compilation, and Intermodule Optimisation.'' The thesis is about a
series of techniques that allow for compiling program modules in such
a way that little overhead is incurred by dividing a program into
distinct modules. As a part of my Ph.D., I have worked with a group of
highly motivated professors and students in the development and use of
new compiler technology. The techniques developed in my Ph.D. thesis
are used in the MLKit [2002c].

As a part of my Ph.D. program, I have spent seven months (from January
to August 1997) at Cornell University, New York, USA. Here, I worked
with Assistant Professor Greg Morrisett and with people from Carnegie
Mellon University on the development of a system for separate
compilation in the TIL (Typed Intermediate Languages) Standard ML
compiler.

During my Ph.D. program, I have gained experience in teaching courses
for Master's degree students in the areas of type theory, compilation
techniques for module languages, and techniques for type-based program
optimisation.

My Ph.D. work has resulted in a series of papers and reports
[1999,1999b,2002c,1998].

#### Research Programmer

From September 1994 to May 1995, I was employed as a research
programmer to work on the MLKit. During this time, I worked on an
efficient implementation of the type system for the Standard ML
Modules language. Moreover, I implemented--with other people in the
MLKit group--a back-end for the MLKit that generates efficient machine
code for the HP PA-RISC microprocessor [1995]. Since then, I have
ported this back-end to the Intel X86 microprocessor and to an
embeddable virtual machine [2002] for use in a Web server
[2004,2003b,2002b].

#### M.Sc. in Engineering 

I initiated the Master of Science program in engineering at the
Technical University of Denmark in September 1988. I spent the year
1991/92 at Whittier College, Los Angeles, USA, where I took classes in
physics, computer science, mathematics, and philosophy. In the second
part of the Master's program, I specialised in computer science. I
finished the Master's program in August 1994 with a Master's project,
on which Associate Professor Peter Sestoft was my advisor. The title
of my Master's thesis [1994] is ''A Portable Implementation of Standard
ML.'' This thesis is about the implementation of the static semantics
(parsing and elaboration) of Core Standard ML (Standard ML without
Modules) and about implementation of the dynamic semantics (runtime
system and compilation into byte code) of Core Standard ML.

In the last year of my M.Sc. program, I worked as a part-time
programmer in Siemens Nixdorf Information Systems A/S, where I worked
with database technologies (Oracle), for implementing work-flow
applications.

#### Teaching

I have taught the following courses:

* Introduction to Scripting, Databases, and System Architecture. The IT University of Copenhagen. One fourth of course. Course Responsible. Spring 2008.

* Introduction to Scripting, Databases, and System Architecture. The IT University of Copenhagen. One third of course. Course Responsible. Fall 2007.

* Introduction to Scripting, Databases, and System Architecture. The IT University of Copenhagen. Half course. Course Responsible. Spring 2007.

* Advanced Topics in Language-based Security. The IT University of Copenhagen. Full Ph.D. course. Course Responsible. Spring 2005.

* Web Publishing with Databases. The IT University of Copenhagen. Full course. Spring 2005.

* Web Publishing with Databases. The IT University of Copenhagen. One half of the course. Fall 2004.

* Web Publishing with Databases. The IT University of Copenhagen. One half of the course. Spring 2004.

* Advanced Language Implementation and Language Based Security. The IT University of Copenhagen. One third of the course. Fall 2003.

* Web Publishing with Databases. The IT University of Copenhagen. Fall 2003.

* Data Processing. Seven lectures on Web programming with PHP and MySQL. Royal Veterinary and Agricultural University of Denmark. Spring 2003.

* Data Processing. Seven lectures on Web programming with PHP and MySQL. Royal Veterinary and Agricultural University of Denmark. Spring 2002.

* Web Publishing with Databases. The IT University of Copenhagen. Spring 2002.

* Web Publishing with Databases. Royal Veterinary and Agricultural University of Denmark. Summer 2001.

* Web Publishing with Databases. The IT University of Copenhagen. Fall 2000.

* Web Publishing with Databases. The IT University of Copenhagen. Spring 2000.

* Type-based Optimisation Techniques. Department of Computer Science, University of Copenhagen. Spring 1998.

* Aspects of Modules languages. Department of Computer Science, University of Copenhagen. Fall 1996. 

I have supervised eight M.Sc. thesis students and about a dozen
smaller projects at the IT University of Copenhagen. The projects have
covered development, documentation, and testing of Web-based and traditional
GUI-style, database-enabled, systems in Java, C#, PHP, TCL, and
Standard ML. One of the Master's thesis projects has resulted in a
startup company Neducate, specializing on community Web sites for
individual public schools in Denmark.

#### Organisational Work and Training

During my time at [SimCorp](http://www.simcorp.com), I participated in
a series of courses developed by FTP (Financial Training Partners)
covering a number of topics including theoretical pricing of financial
derivatives. From May 2009 to 2012, I worked as Team Leader in the
Instrument Modeling Language team at SimCorp.

August 2007 to May 2008, I served as the head of the international
technical ITU program Software Development and Technology. From
November 2003 to January 2005 I served as Head of the ITU Internet and
Software Technology Study Program. During 2004, I have attended a
management course at SHL Denmark. During the educational year
2001-2002, I have attended a pedagogical education program for
assistant professors at KVL, in addition to a course on project
supervision. I have been a co-organiser of the DIKU International
Summer School on Region-Based Memory Management, August 18-22, 1997,
held at the University of Copenhagen, Denmark. I have reviewed papers
for a series of conferences, workshops, and journals, including
PLDI'96, PLILP'96, IFL'97, POPL'97, SAS'97, PLDI'99, ESOP'01, ESOP'03,
HOSC, JFP, and TOPLAS. I have served on the programme committees for
the 2008 International Conference on Compiler Construction (CC'08),
the 2007 Nordic Workshop on Secure IT Systems (NordSec'07), the 2005
ACM SIGPLAN Workshop on ML (ML'05), held in Tallinn, Estonia,
September 2005. I have also served as programme co-chair for the 2006
ACM Workshop on Semantics, Program Analysis, and Computing
Environments for Memory Management (SPACE'06), held in South Carolina,
January 2006.

#### Publications

* [2014] Martin Elsman and Anders Schack-Nielsen. Typelets - A
Rule-Based Evaluation Model for Dynamic, Statically Typed User
Interfaces. In International Symposium on Practical Aspects of
Declarative Languages (PADL'14). San Diego, USA. January, 2014.

* [2011] Martin Elsman. SMLtoJs: Hosting a Standard ML Compiler in a
Web Browser. In ACM SIGPLAN International Workshop on Programming
Language And Systems Technologies for Internet Clients
(PLASTIC'2011). Portland, Oregon, USA. October, 2011.

* [2007] Martin Elsman. Fault-Tolerant Voting in a Simply-Typed Lambda
Calculus. IT University Technical Report Series. TR-2007-99. June 2007.

* [2006] Arne J. Glenstrup, Troels C. Damgaard, Lars Birkedal, and
Martin Elsman. BDNF-based Matching of Bigraphs. IT University
Technical Report Series. TR-2006-93. October 2006.

* [2005] Martin Elsman. Type-Specialized Serialization with
Sharing. In Sixth Symposium on Trends in Functional Programming
(TFP'05). Tallinn, Estonia. September 2005.

* [2004] Martin Elsman and Ken Friis Larsen. Typing XHTML Web
Applications in ML. In International Symposium on Practical Aspects of
Declarative Languages (PADL'04). Dallas, USA. June 2004.

* [2004a] Martin Elsman. Type-Specialized Serialization with
Sharing. IT University Technical Report Series. TR-2004-43. February 2004.

* [2004b] Mads Tofte, Lars Birkedal, Martin Elsman, and Niels
Hallenberg. A Retrospective on Region-Based Memory Management. In
Higher-Order and Symbolic Computation (HOSC). 17(3): 245-265. 
Copyright © 2004 Kluwer Academic Publishers. September 2004.

* [2003] Martin Elsman and Ken Friis Larsen. Typing XHTML Web
Applications in SMLserver. IT University Technical Report
Series. TR-2003-34. October 2003.

* [2003a] Martin Elsman. Garbage Collection Safety for Region-based
Memory Management. In ACM SIGPLAN Workshop on Types in Language Design
and Implementation (TLDI'03). New Orleans, Louisiana, USA. January 2003.

* [2003b] Martin Elsman and Niels Hallenberg. Web Programming with
SMLserver. In Fifth International Symposium on Practical Aspects of
Declarative Languages (PADL'03). New Orleans, Louisiana, USA. January 2003.

* [2002] Martin Elsman and Niels Hallenberg. A Region-Based Abstract
Machine for the MLKit. IT University Technical Report
Series. TR-2002-18. August 2002.

* [2002a] Niels Hallenberg, Martin Elsman and Mads Tofte. Combining
Region Inference and Garbage Collection. In ACM International
Conference on Programming Language Design and Implementation
(PLDI'02). Berlin, Germany. June 2002.

* [2002b] Martin Elsman and Niels Hallenberg. SMLserver - A Functional
Approach to Web Publishing. Technical Report. Royal Veterinary and
Agricultural University of Denmark and IT University of
Copenhagen. February 2002.

* [1999] Martin Elsman. Static Interpretation of Modules. In Fourth
International Conference on Functional Programming (ICFP'99). Paris,
France. September 1999.

* [1999a] Martin Elsman, Jeffrey S. Foster, and Alexander
Aiken. Carillon - A System to Find Y2K Problems in C Programs. User's
Manual. Computer Science Division, University of California,
Berkeley. July 1999.

* [1999b] Martin Elsman. Program Modules, Separate Compilation, and
Intermodule Optimisation. Ph.D. thesis, revised. Department of
Computer Science, University of Copenhagen. January 1999.

* [2002c] Mads Tofte, Lars Birkedal, Martin Elsman, Niels Hallenberg,
Tommy Højfeld Olesen, Peter Sestoft, and Peter Bertelsen. Programming
with Regions in the MLKit. DIKU Technical Report. Department of
Computer Science, University of Copenhagen. Revised for Version 4, 2002. April 1997.

* [1998] Martin Elsman. Polymorphic Equality - No Tags Required. In
Second International Workshop on Types in Compilation (TIL'98). Kyoto,
Japan. March 1998.

* [1995] Martin Elsman and Niels Hallenberg. An Optimising Back-End
for the MLKit Using a Stack of Regions. Student Project. Department of
Computer Science, University of Copenhagen. July 1995.

* [1994] Martin Elsman. A Portable Standard ML
Implementation. Master's thesis. Department of Computer Science,
Technical University of Denmark. August 1994.

All publications (except Journal papers) are available [online](/papers.html).