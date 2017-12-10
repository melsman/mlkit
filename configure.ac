AC_INIT(MLKit, [4.3.12])
AC_CONFIG_HEADERS([src/config.h])
AC_REVISION($Revision$)
AC_CONFIG_FILES([src/SMLserver/apache/Makefile
                 src/Runtime/Makefile
                 src/Makefile
                 Makefile
                 doc/README_BIN
                 doc/README_SMLSERVER_BIN
                 man/man1/mlkit-mllex.1
                 man/man1/mlkit-mlyacc.1
                 man/man1/rp2ps.1
                 man/man1/kittester.1
                 smlserver_demo/Makefile
                 src/Tools/MlbMake/Makefile
                 src/Tools/Rp2ps/Makefile
                 src/Tools/GenOpcodes/Makefile
                 src/Tools/ml-lex/Makefile
                 src/Tools/ml-yacc/Makefile
                 src/Tools/UlWrapUp/Makefile
                 src/Tools/Tester/Makefile
                 src/Version.sml])

AC_PROG_CC
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([fcntl.h fenv.h float.h limits.h malloc.h netdb.h stddef.h stdlib.h string.h strings.h sys/ioctl.h sys/param.h sys/time.h sys/timeb.h termios.h unistd.h utime.h])


AC_ARG_ENABLE(SMLserver,AS_HELP_STRING(--enable-SMLserver,compile with SMLserver),
  AC_DEFINE(SMLSERVER,1,[Smlserver requested]),
  [])

AC_ARG_ENABLE(KAM,AS_HELP_STRING(--enable-KAM,compile with KAM backend))

AC_ARG_ENABLE(threaded,
  AS_HELP_STRING(--disable-threaded,Don't use threaded interpreter),[],
  AC_DEFINE(THREADED,1,[Kam backend should be threaded]))

AC_SUBST(odbc,no)
AC_ARG_ENABLE(odbc,
  AS_HELP_STRING(--enable-odbc,Compile ODBC functionality),
  AC_SUBST(odbc,${enable_odbc}),)

AC_ARG_WITH(
     compiler,
     AS_HELP_STRING(--with-compiler[=FILE],[SML compiler to build tools and the initial mlkit compiler]),
     AC_SUBST(mlcomp,${with_compiler})
     ,
     )

if test x"${enable_SMLserver}" != x && test "${enable_SMLserver}" = yes; then
  APACHE_DIR
  ORACLE_DIR
  AC_SUBST(oracle_dir)
fi;

AC_MSG_CHECKING(configure date)
DATE=`date -u '+%Y-%m-%dT-%H:%M:%S'`
AC_SUBST(DATE)
AC_MSG_RESULT($DATE)

AC_OUTPUT
