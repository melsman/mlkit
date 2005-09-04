AC_DEFUN([APACHE_DIR],[

  AC_ARG_WITH(
    apache,
    AS_HELP_STRING(--with-apache[=DIR],Apache server directory),
    ,
    [with_apache="no"]
  )

  AC_MSG_CHECKING(for Apache directory)

  if test "$with_apache" = "no"; then
    AC_MSG_ERROR( Specify the apache using --with-apache)
  else
    # make sure that a well known include file exists
    if test -e $with_apache/include/httpd.h; then
      apache_dir=$with_apache
      AC_DEFINE_UNQUOTED(APACHEDIR,$apache_dir,[Apache location])
      AC_MSG_RESULT(APACHE found!)
    else
      AC_MSG_ERROR(Apache not found in $with_apache.  Check the value you specified with --with-apache)
    fi
  fi

])

AC_DEFUN([ORACLE_DIR],[

  AC_ARG_WITH(
    oracle,
    AS_HELP_STRING(--with-oracle[=DIR],[Oracle home directory (if you would like to compile the Oracle driver)]),
    ,
    [with_oracle="no"]
  )

  AC_MSG_CHECKING(for Oracle home directory)

  if test "$with_oracle" = "no"; then
    AC_MSG_RESULT( Not compiling Oracle driver)
  else
    # make sure that a well known include file exists
    if test -e $with_oracle/rdbms/public/oci.h; then
      oracle_dir=$with_oracle
      AC_DEFINE_UNQUOTED(ORACLEDIR,$oracle_home,[Oracle location])
      AC_MSG_RESULT(Oracle found!)
    else
      AC_MSG_ERROR(Oracle not found in $with_oracle.  Check the value you specified with --with-oracle)
    fi
  fi

])
