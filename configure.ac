AC_INIT(MLKit, [v4.7.23])
AC_CONFIG_HEADERS([src/Runtime/config.h])
AC_REVISION($Revision$)
AC_CONFIG_FILES([src/Runtime/Makefile
                 src/Makefile
                 Makefile
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

# macOS uses ARM64; other platforms retain the X64 build.
AC_ARG_VAR([DARWIN_NATIVE], [1 on macOS (default); 0 on other platforms (default)])
AS_IF([test -z "$DARWIN_NATIVE"], [
  AS_CASE([`uname -s`], [Darwin], [DARWIN_NATIVE=1], [DARWIN_NATIVE=0])
])
AS_CASE([$DARWIN_NATIVE], [0|1], [], [AC_MSG_ERROR([DARWIN_NATIVE must be 0 or 1])])
AS_IF([test -z "$CC"], [CC=gcc])
AS_CASE([`uname -s`],
  [Darwin], [
    AS_IF([test "$DARWIN_NATIVE" != 1], [AC_MSG_ERROR([macOS supports only ARM64; omit DARWIN_NATIVE or set it to 1])])
    AS_IF([test "`uname -m`" != arm64], [AC_MSG_ERROR([macOS requires Apple Silicon and a native arm64 shell])])
    MLKIT_ARCH=arm64
    RUNTIME_TARGET=darwin-$MLKIT_ARCH
    CFLAGS="${CFLAGS:--O2 -g} -arch $MLKIT_ARCH"
  ],
  [
    AS_IF([test "$DARWIN_NATIVE" = 1], [AC_MSG_ERROR([DARWIN_NATIVE=1 requires macOS])])
    RUNTIME_TARGET=`uname -s | tr 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 'abcdefghijklmnopqrstuvwxyz'`-`uname -m`
  ])
AC_SUBST([RUNTIME_TARGET])
AC_DEFINE_UNQUOTED([DARWIN_NATIVE], [$DARWIN_NATIVE], [Build the native macOS arm64 runtime])
AC_PROG_CC
AC_MSG_CHECKING([whether the compiler target agrees with DARWIN_NATIVE])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#if defined(__APPLE__)
# if !$DARWIN_NATIVE || (!defined(__aarch64__) && !defined(__arm64__))
#  error macOS requires the ARM64 backend
# endif
#endif
int main(void) { return 0; }
]])], [AC_MSG_RESULT([yes])], [AC_MSG_ERROR([compiler target disagrees with DARWIN_NATIVE; check CC and CFLAGS])])
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_DIRENT

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

AC_ARG_WITH(
     argobots,
     AS_HELP_STRING(--with-argobpts[=ROOTDIR],[ARGOBOTS root-directory]),
     AC_SUBST(ARGOBOTS_ROOT,${with_argobots})
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
