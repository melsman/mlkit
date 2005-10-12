AC_INIT(MLKit, [4.1.5])
AC_CONFIG_HEADERS([src/config.h])
AC_REVISION($Revision$)
AC_CONFIG_FILES([src/SMLserver/apache/Makefile Makefile version])

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

if test x"${enable_SMLserver}" != x && test "${enable_SMLserver}" = yes; then
  APACHE_DIR
  AC_SUBST(apache_dir)
  ORACLE_DIR
  AC_SUBST(oracle_dir)
fi;

AC_OUTPUT