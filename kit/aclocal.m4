AC_DEFUN([APACHE_DIR],[

  AC_ARG_WITH(
    apxs,
    AS_HELP_STRING(--with-apxs[=FILE],Apache build tool),
    ,
    [with_apxs="no"]
  )

  AC_MSG_CHECKING('for Apache build tool (apxs)')

  if test "$with_apxs" = "no"; then
    AC_MSG_RESULT(--with-apxs not defined apxs should be in your PATH)
    AC_SUBST(apxs,apxs)
  else
    # make sure that a well known include file exists
    if test -e $with_apxs; then
      apxs=$with_apxs
      AC_SUBST(apxs)
      AC_MSG_RESULT(APACHE found!)
    else
      AC_MSG_ERROR(Apache not found in $with_apxs.  Check the value you specified with --with-apxs)
    fi
  fi

])

AC_DEFUN([ORACLE_DIR],[

  AC_ARG_WITH(
    oracle,
    AS_HELP_STRING(--with-oracle[=DIR],[Oracle instant client sdk directory (if you would like to compile the Oracle driver)]),
    ,
    [with_oracle="no"]
  )

  AC_MSG_CHECKING(for Oracle home directory)

  if test "$with_oracle" = "no"; then
    AC_MSG_RESULT( Not compiling Oracle driver)
  else
    # make sure that a well known include file exists
    if test -e $with_oracle/sdk/include/oci.h; then
      oracle_dir=$with_oracle
      AC_DEFINE_UNQUOTED(ORACLEDIR,$oracle_home,[Oracle location])
      AC_MSG_RESULT(Oracle found!)
    else
      AC_MSG_ERROR(Oracle not found in $with_oracle.  Check the value you specified with --with-oracle)
    fi
  fi

])
