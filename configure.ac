AC_INIT(MLKit, [v4.5.11])
AC_CONFIG_HEADERS([src/config.h])
AC_REVISION($Revision$)
AC_CONFIG_FILES([src/Runtime/Makefile
                 src/Makefile
                 Makefile
                 doc/README_BIN
                 man/man1/mlkit-mllex.1
                 man/man1/mlkit-mlyacc.1
                 man/man1/rp2ps.1
                 man/man1/kittester.1
                 src/Tools/MlbMake/Makefile
                 src/Tools/Rp2ps/Makefile
                 src/Tools/ml-lex/Makefile
                 src/Tools/ml-yacc/Makefile
                 src/Tools/Tester/Makefile
                 src/Version.sml])

AC_PROG_CC
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_DIRENT
m4_warn([obsolete],
[The preprocessor macro `STDC_HEADERS' is obsolete.
  Except in unusual embedded environments, you can safely include all
  ISO C90 headers unconditionally.])dnl
# Autoupdate added the next two lines to ensure that your configure
# script's behavior did not change.  They are probably safe to remove.
AC_CHECK_INCLUDES_DEFAULT
AC_PROG_EGREP

AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([fcntl.h fenv.h float.h limits.h malloc.h netdb.h stddef.h stdlib.h string.h strings.h sys/ioctl.h sys/param.h sys/time.h sys/timeb.h termios.h unistd.h utime.h])

AC_ARG_WITH(
     compiler,
     AS_HELP_STRING(--with-compiler[=FILE],[SML compiler to build tools and the initial mlkit compiler]),
     AC_SUBST(mlcomp,${with_compiler})
     ,
     )

AC_MSG_CHECKING(configure date)
DATE=`date -u '+%Y-%m-%dT-%H:%M:%S'`
AC_SUBST(DATE)
AC_MSG_RESULT($DATE)

COMMITDATE=`git log -1 --format=%cI`
AC_SUBST(COMMITDATE)

GITVERSION=`git describe --abbrev --dirty --always --tags`
AC_SUBST(GITVERSION)

AC_OUTPUT
