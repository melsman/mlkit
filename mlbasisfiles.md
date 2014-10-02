---
layout: page
title: ML Basis Files
---
_By Martin Elsman_

An ML Basis File, in short MLB-file, is a file that lists the SML
source files that make up a project or a library. An MLB-file can also
reference other MLB-files, so one can organise projects in a
hierarchical manner. MLB-files are enforced not to be cyclic.

### Syntax and Semantics of MLB-files

MLB-files have file extension .mlb. The content of an MLB-file is a
basis declaration, for which the grammar is given as follows. We
assume a denumerable infinite set of basis identifiers Bid, ranged
over by bid. We use longbid to range over long basis identifiers, that
is, non-empty lists of basis identifiers separated by a punctuation
letter (.). Basis identifiers can be used for giving a name to a group
of compilation units and allow for expressing source dependencies,
exactly, as a directed acyclic graph, within one MLB-file.

    bdec ::= bdec bdec                 sequential basis declaration
           | (empty)                   empty basis declaration
           | local bdec in bdec end    local declaration
           | basis bid = bexp          basis identifier binding
           | open longbid*             opening of bases
           | atbdec
           | path.mlb                  include
    atbdec ::= path.sml                source file
             | path.sig                source file
    bexp ::= bas bdec end              basis declaration grouping
           | let bdec in bexp end      let expression
           | longbid

In an MLB-file, one can reference source files and other MLB-files
using absolute or relative paths. Relative paths are relative to the
location of the MLB-file. Paths can reference, so-called MLB path
variables using the $(VAR) notation, where VAR is an MLB path
variable. In particular, MLB-files can reference the Basis Library,
using the MLB path variable $(SML_LIB), by including the path
$(SML_LIB)/basis/basis.mlb. An MLB path variable _V_ is resolved
according to the following rules:

1. First, look for an environment variable with name _V_.

1. Then, look for a definition of the variable _V_ in one of the files
provided with an option --mlb_path_maps (see `mlkit -help` for
details).

1. Then, look for a definition of _V_ in the user's local file
$(HOME)/.mlkit/mlb-path-map.

1. Finally, look for a definition of _V_ in the global file /usr/local/etc/mlkit/mlb-path-map.

MLB-files may contain Standard ML style comments. The declared
identifiers of an MLB-file is the union of the identifiers being
declared by source files in the MLB-file, excluding source files that
are included using local. As an example of the use of basis
identifiers and local to limit what identifiers are declared by an
MLB-file, consult the MLB-file basis/basis.mlb.

Every source file must contain a Standard ML top-level declaration;
the scope of the declaration is all the subsequent source files
mentioned in the MLB-file and all other MLB-files that reference this
MLB-file. Thus, a source file may depend on source files mentioned
earlier in the MLB-file and on other referenced MLB-files. The meaning
of an entire MLB-file is the meaning of the top-level declaration that
would arise by expanding all referenced MLB-files and then
concatenating all the source files listed in the MLB-file (with
appropriate renaming of declared identifiers of source files that are
included using local), in the order they are listed, except that each
MLB-file is executed only the first time it is imported. To be
precise, MLB files can be used to hide the definition of signature and
functor declarations, which cannot be accomodated using Stadard ML
toplevel declarations alone.

### Managing Compilation and Recompilation with MLB-files

The MLKit has a system for managing compilation and recompilation of
MLB-files. The system guarantees that the result of first modifying
one or more source files and then using the separate compilation
system to rebuild the executable is the same as if all source files
were recompiled.

Thus, the separate compilation system is a way of avoiding recompiling
parts of a (possibly) long sequence of declarations, while ensuring
that the result is always the same as if one had compiled the entire
program from scratch. As an example, consider the MLB-file
(kitdemo/scan.mlb) for a text scanning example. It contains the
following three lines:

    $(SML_LIB)/basis/basis.mlb
    lib.sml
    scan.sml

The source files for the project are lib.sml and scan.sml, which are
both located in the directory where scan.mlb is located. Whereas each
of the source files lib.sml and scan.sml depends on the Basis Library,
the source file scan.sml also depends on lib.sml.

Compiling an MLB-file is easy; simply give it as an argument to the
MLKit executable. When the MLB-file is first compiled, the MLKit
detects automatically when a source file has been modified (by
checking file modification dates). After a project has been
successfully compiled and linked, it can be executed by running the
command

    run

in the working directory.

The MLKit compiles each source file of an MLB-file one at a time, in
the order mentioned in the project file. A source file is compiled
under a given set of assumptions, which provides, for instance,
region-annotated type schemes with places for free variables of the
source file. Also, compilation of a source file gives rise to exported
information about declared identifiers. Exported information may occur
in assumptions for source files mentioned later in the MLB-file.

There are two rules that govern when a source file is recompiled. A
source file is recompiled if either (1) the user has modified the
source file or (2) the assumptions under which the source file was
previously compiled have changed. To avoid unnecessary recompilation,
assumptions for a source file depend on only its free
identifiers. Moreover, if a source file has been compiled earlier, the
MLKit seeks to match the new exported information to the old exported
information by renaming generated names to names generated when the
source file was first compiled. Matching allows the compiler to use
fresh names (stamps) for implementing generative data types, for
instance, and still achieve that a source file is not necessarily
recompiled even though source files, on which it depends, are
modified.

Let us assume that we modify the source file lib.sml of the text
scanning example, after having compiled the MLB-file kitdemo/scan.mlb
once. When compiling the MLB-file again, the MLKit checks whether the
assumptions under which the source file scan.sml was compiled have
changed, and if so, recompiles scan.sml. Modifying only comments or
string constants inside lib.sml or extending its set of declared
identifiers does not trigger recompilation of scan.sml.

Some of the information a source file depends on is the ML type
schemes of its free variables. It also depends on, for example, the
region-annotated type schemes with places of its free variables. Thus
it can happen that a source file is recompiled even though the ML type
assumptions for free variables are unchanged. For instance, the
region-annotated type scheme with place for a free variable may have
changed, even though the underlying ML type scheme has not.

As an example, consider what happens if we modify the function
readWord in the source file lib.sml so that it puts its result in a
global region. This modification will trigger recompilation of the
source file scan.sml, because the assumptions under which it was
previously compiled have changed. Besides changes in region-annotated
type schemes with places, changes in multiplicities and in physical
sizes of formal region variables of functions may also trigger
recompilation.

For details about the implementation of the recompilation scheme used
for ML Basis Files in the MLKit, see

* Martin Elsman. __A Framework for Cut-Off Incremental Recompilation
and Inter-Module Optimization__. IT University of Copenhagen,
Denmark. IT University Technical Report. April 2008. [pdf]({{BASE_PATH}}/pdf/sepcomp_tr.pdf),
[bibtex]({{BASE_PATH}}/pdf/sepcomp_tr.bibtex.txt).
