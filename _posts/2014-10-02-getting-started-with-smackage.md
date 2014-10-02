---
layout: post
category : lessons
tags : [smackage, libraries, package management, mlton, mlkit]
---
_By Martin Elsman_

{% include JB/setup %}

[Smackage](http://github.com/standardml/smackage) is a package
management tool for Standard ML libraries and source code. It is
useful, in particular, for maintaining a stable collection of Standard
ML libraries on your machine, based on the versioned dependency
specifications, provided by library and application providers.

As an author of many differerent libraries and programs written in
SML, I have often found myself copying utilities - and even entire
libraries - into new repositories. Such copying is often attractive as
it relieves a user of a library from considering what will happen when
the library is updated by its maintainer. It is also unattractive, on
the other hand, as the user does not easily benefit from upstream
library improvements and often starts maintaining the library in
parallel to the maintainance and development of the original
library.

Based on [semantic versioning](http://semver.org),
[Smackage](http://github.com/standardml/smackage) is now here to help
library maintainers and package builders.

To get started with Smackage, you first and foremost need a version of
the `smackage` executable on your computer (we will assume here that
[MLton](http://mlton.org) is installed). You then need to configure
your favorite SML compiler to know about where packages managed by
Smackage reside, and finally, you need to familiarize yourself with
the most simple Smackage commands, such as `smackage refresh`,
`smackage update`, and `smackage get`.

## Install Smackage

Execute the following commands in a shell:

    $ cd
    $ git clone git://github.com/standardml/smackage
    $ cd smackage
    $ make mlton
    $ ./smackage refresh

Smackage stores configuration information, libraries, and versioning
information in your local working directory `$(HOME)/.smackage`, which
was created when `smackage` was run for the first time. Applications and
tools built with Smackage are installed in the `$(HOME)/.smackage/bin`
directory, which you propably want to add to your PATH environment
variable (e.g., alter your `$(HOME)/.bash_profile` file).

Now, in a fresh shell, execute the following commands:

    $ cd smackage
    $ ./smackage get smackage
    $ ./smackage make smackage mlton
    $ ./smackage make smackage install

The get command will fetch the smackage package (and possible
depending packages) from the Smackage github repository. The make
commands will build and install the `smackage` executable according to
the Smackage Makefile targets `mlton` and `install`.

Smackage is now properly installed in `$(HOME)/.smackage/bin` and you
can delete your `$(HOME)/smackage` directory:

    $ rm -rf $(HOME)/smackage

## Make your favorite SML compiler "Smackage aware"

[MLton](http://mlton.org) and [MLKit](http://www.elsman.com/mlkit/)
use [MLB-files](http://www.elsman.com/mlkit/mlbasisfiles.html) for
specifying library dependencies and for managing and controling the
compilation process. For an MLB-file to reference a Smackage-installed
library, the convention is that an MLB-file can reference the special
`$(SMACKAGE)` variable in path names. For this to work, add a line to
the appropriate `mlb-path-map` file:

    SMACKAGE [HOME]/.smackage/lib

Here you need to replace `[HOME]` with your particular path to your
home directory, as found in `$(HOME)`. Different compilers reads the
`mlb-path-map` file from different locations. For instance, MLKit will
try to see if there is a file `$(HOME)/.mlkit/mlb-path-file`. If not,
it will try to find one in `/usr/local/etc/mlkit` or
`/usr/etc/mlkit`. MLton will try similar attempts.

## Mastering `smackage` commands.

Now that `smackage` is installed, you can execute various basic smackage
commands:

 Command           | Action
 ------------------|------
 smackage refresh           | update the local database
 smackage get _package_     | download _package_ and the packages it depends on
 smackage update            | download new versions of fetched packages
 smackage info _package_    | display information about _package_

To learn more about available Smackage commands, just run `smackage`
without arguments.

## Adding your own library

If you want to use Smackage for managing dependencies between your own
git repositories, you can do so by adding an entry to the file
`$(HOME)/.smackage/sources.local` or by using the `smackage source`
command, which effectively adds an entry to the file. Smackage assumes
that you have tagged your library with [semantic version
numbers](http://semver.org). This tagging is possible, by executing
the following two git commands from within a checked out version of
your library:

    $ git tag v1.0.1
    $ git push --tags

If your package is hosted at Github, you may tag the current version
of your package using the Web api - simply click the "releases" link
on the main page of your repository and follow the online guidance.

## Adding a .smackspec-file to your library

You may want to add a description of your package in form of a
.smackspec-file. The description is necessary if your package depends
on other packages

## Related work

For more advanced uses of Smackage, including getting it to work with
SML/NJ and information about platform specific targets, consult the
[Smackage web site](http://github.com/standardml/smackage).

The construction of Smackage was inspired by tools available for other
programming languages, including
[Hackage](http://hackage.haskell.org/) and
[Cabal](http://www.haskell.org/cabal/) for
[Haskell](http://www.haskell.org/haskellwiki/Haskell). Other related
package managers, include [0install](http://0install.net/) and
[Homebrew](http://brew.sh/).

## Conclusions and future work

Whereas Smackage is a great tool for managing source code packages and
their dependencies, Smackage is currently not a build tool - it does
not manage the build-process of a package. As a consequence, there is
no way of specifying that a package have binary dependencies, in the
sense that a specific package needs to be built and installed before
another package can be built.
