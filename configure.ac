dnl
dnl Configuring Gauche-dbd-mysql
dnl  process this file with autoconf to generate 'configure'.
dnl

AC_PREREQ(2.59)
AC_INIT(Gauche-dbd-mysql, 0.3_pre3, info@kahua.org)
AC_CONFIG_HEADER(acconfig.h)

dnl
dnl Check gauche and its stuff.
dnl
AC_PATH_PROG([GOSH], gosh)
AC_PATH_PROG([GAUCHE_CONFIG], gauche-config)
AC_PATH_PROG([GAUCHE_PACKAGE], gauche-package)
AC_PATH_PROG([GAUCHE_INSTALL], gauche-install)
AC_PATH_PROG([GAUCHE_CESCONV], gauche-cesconv)
SOEXT=`$GAUCHE_CONFIG --so-suffix`
OBJEXT=`$GAUCHE_CONFIG --object-suffix`
EXEEXT=`$GAUCHE_CONFIG --executable-suffix`
AC_SUBST(SOEXT)
AC_SUBST(OBJEXT)
AC_SUBST(EXEEXT)
ac_default_prefix=`$GAUCHE_CONFIG --prefix`
GAUCHE_PKGINCDIR=`$GAUCHE_CONFIG --pkgincdir`
GAUCHE_PKGLIBDIR=`$GAUCHE_CONFIG --pkglibdir`
GAUCHE_PKGARCHDIR=`$GAUCHE_CONFIG --pkgarchdir`
AC_SUBST(GAUCHE_PKGINCDIR)
AC_SUBST(GAUCHE_PKGLIBDIR)
AC_SUBST(GAUCHE_PKGARCHDIR)

dnl
dnl Check MySQL and its stuff.
dnl
AC_ARG_WITH(
  [mysql-config],
  [AC_HELP_STRING([--with-mysql-config=PATH],
		  [path to mysql_config for finding the MySQL client library])])
if test "X${with_mysql_config}" != X; then
   MYSQL_CONFIG="${with_mysql_config}"
else
   AC_PATH_PROGS(MYSQL_CONFIG, mysql_config)
fi
if test X${MYSQL_CONFIG} = X; then
  AC_MSG_RESULT(not available)
else
  MYSQL_CFLAGS=`${MYSQL_CONFIG} --cflags`
  MYSQL_LDFLAGS=`${MYSQL_CONFIG} --libs`
  AC_SUBST(MYSQL_CFLAGS)
  AC_SUBST(MYSQL_LDFLAGS)
  AC_MSG_RESULT(ok)
fi

CFLAGS="$MYSQL_CFLAGS $CFLAGS"
LDFLAGS="$MYSQL_LDFLAGS $LDFLAGS"
AC_CHECK_TYPES([MYSQL_STMT,MYSQL_FIELD,MYSQL_BIND,MYSQL_TIME,MY_CHARSET_INFO],,,[
#include <mysql.h>
#include <errmsg.h>
#include <mysqld_error.h>
])
AC_CHECK_MEMBERS([MYSQL_FIELD.charsetnr,
                  MYSQL_FIELD.name_length,
		  MYSQL_FIELD.org_name,
		  MYSQL_FIELD.org_name_length,
		  MYSQL_FIELD.table_length,
		  MYSQL_FIELD.org_table_length,
		  MYSQL_FIELD.db_length,
		  MYSQL_FIELD.catalog,
		  MYSQL_FIELD.catalog_length,
		  MYSQL_FIELD.def_length],,,[
#include <mysql.h>
#include <errmsg.h>
#include <mysqld_error.h>
])
AC_CHECK_DECLS([mysql_autocommit,
                mysql_commit,
		mysql_get_server_version,
		mysql_rollback,
                mysql_set_character_set,
		mysql_sqlstate,
		mysql_warning_count],,,[
#include <mysql.h>
#include <errmsg.h>
#include <mysqld_error.h>
])
AC_CHECK_DECLS([MYSQL_TYPE_TINY,MYSQL_TYPE_SHORT,MYSQL_TYPE_LONG,MYSQL_TYPE_INT24,
                MYSQL_TYPE_LONGLONG,MYSQL_TYPE_DECIMAL,MYSQL_TYPE_NEWDECIMAL,
		MYSQL_TYPE_FLOAT,MYSQL_TYPE_DOUBLE,MYSQL_TYPE_BIT,MYSQL_TYPE_TIMESTAMP,
		MYSQL_TYPE_DATE,MYSQL_TYPE_TIME,MYSQL_TYPE_DATETIME,MYSQL_TYPE_YEAR,
		MYSQL_TYPE_NEWDATE,MYSQL_TYPE_STRING,MYSQL_TYPE_VAR_STRING,MYSQL_TYPE_BLOB,
		MYSQL_TYPE_TINY_BLOB,MYSQL_TYPE_MEDIUM_BLOB,MYSQL_TYPE_LONG_BLOB,
		MYSQL_TYPE_SET,MYSQL_TYPE_ENUM,MYSQL_TYPE_GEOMETRY,MYSQL_TYPE_NULL],,,[
#include <mysql.h>
#include <errmsg.h>
#include <mysqld_error.h>
])
AC_CHECK_DECLS([SHUTDOWN_DEFAULT],,,[
#include <mysql.h>
#include <errmsg.h>
#include <mysqld_error.h>
])

AC_CHECK_DECLS([MY_CS_AVAILABLE],,,[
#include <my_global.h>
#include <my_sys.h>
])

AC_CHECK_FUNC([get_charset_number],
	      [AC_DEFINE([HAVE_MYSQL_GET_CHARSET_NUMBER],[1],
			 [Define to 1 if you have linkable get_charset_number])])

dnl Creating gpd (gauche package description) file
GAUCHE_PACKAGE_CONFIGURE_ARGS="`echo ""$ac_configure_args"" | sed 's/[\\""\`\$]/\\\&/g'`"
AC_MSG_NOTICE([creating ${PACKAGE_NAME}.gpd])
$GAUCHE_PACKAGE make-gpd "$PACKAGE_NAME" \
  -version "$PACKAGE_VERSION" \
  -configure "./configure $GAUCHE_PACKAGE_CONFIGURE_ARGS"

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile)

