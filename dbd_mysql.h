/*
 *  Copyright (c) 2004-2007 Scheme Arts, L.L.C., All rights reserved.
 *  Copyright (c) 2004-2007 Time Intermedia Corporation, All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * $Id: dbd_mysql.h,v 1.24 2007/03/23 09:43:16 bizenn Exp $
 */

/* Prologue */
#ifndef DBD_MYSQL_H
#define DBD_MYSQL_H

#include <my_global.h>
#include <my_sys.h>
#include <mysql.h>
#include <errmsg.h>
#include <mysqld_error.h>

#include <gauche.h>
#include <gauche/extend.h>
#include <gauche/class.h>

/* To avoid confliction. */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#include "acconfig.h"

SCM_DECL_BEGIN

/*
 * Internal Classes
 */

/* We use foreign pointer to hold mysql handle (MYSQL*) and
   mysql result handle (MYSQL_RES*).  Once a handle is closed
   or freed, we mark the foreing pointer so by setting a
   foreign-pointer-attribute 'closed to #t. */

extern ScmClass *MysqlHandleClass;
typedef ScmForeignPointer MysqlHandle;
#define MYSQL_HANDLE_P(obj)      SCM_XTYPEP(obj, MysqlHandleClass)
#define MYSQL_HANDLE_UNBOX(obj)  SCM_FOREIGN_POINTER_REF(MYSQL*, obj)
#define MYSQL_HANDLE_BOX(handle) Scm_MakeForeignPointer(MysqlHandleClass, handle)

extern ScmClass *MysqlResClass;
typedef ScmForeignPointer ScmMysqlRes;
#define MYSQL_RES_P(obj)     SCM_XTYPEP(obj, MysqlResClass)
#define MYSQL_RES_UNBOX(obj) SCM_FOREIGN_POINTER_REF(MYSQL_RES*, obj)
#define MYSQL_RES_BOX(res)   Scm_MakeForeignPointer(MysqlResClass, res)

extern ScmClass *MysqlRowsClass;
typedef ScmForeignPointer ScmMysqlRows;
#define MYSQL_ROWS_P(obj)     SCM_XTYPEP(obj, MysqlRowsClass)
#define MYSQL_ROWS_UNBOX(obj) SCM_FOREIGN_POINTER_REF(MYSQL_ROWS*, obj)
#define MYSQL_ROWS_BOX(res)   Scm_MakeForeignPointer(MysqlRowsClass, res)

extern int MysqlClosedP(ScmObj obj);
extern void MysqlMarkClosed(ScmObj obj);

extern MYSQL *MysqlRealConnect(const char *host,
			       const char *user,
			       const char *password,
			       const char *db,
			       unsigned int port,
			       const char *unix_socket,
			       unsigned int client_flag);
extern ScmObj MysqlAffectedRows(MYSQL *handle);
extern ScmObj MysqlFetchFieldNames(MYSQL_RES *result);
extern ScmObj MysqlFetchLengths(MYSQL_RES *result);
extern ScmObj MysqlFetchRow(MYSQL_RES *result);

extern void raise_mysql_error(MYSQL *handle, const char *msg);

extern void mysql_cleanup(ScmObj obj);
extern void mysql_res_cleanup(ScmObj obj);

/*
 * Prepared Statement Support
 */
#if HAVE_MYSQL_STMT
   typedef struct {
       MYSQL_STMT *stmt;
       MYSQL_BIND *params;
       unsigned int param_count;
       MYSQL_BIND *fields;
       unsigned int field_count;
       MYSQL_RES  *metares;
   } MYSQL_STMTX;
   extern ScmClass *MysqlStmtxClass;
   typedef ScmForeignPointer ScmMysqlStmtx;
#  define MYSQL_STMTX_P(obj)         SCM_XTYPEP(obj, MysqlStmtxClass)
#  define MYSQL_STMTX_UNBOX(obj)     SCM_FOREIGN_POINTER_REF(MYSQL_STMTX*, obj)
#  define MYSQL_STMTX_BOX(stmtx)     Scm_MakeForeignPointer(MysqlStmtxClass, stmtx)
#  define MYSQL_STMTX_STMT(obj)    ((MYSQL_STMTX_UNBOX(obj))->stmt)
#  define MYSQL_STMTX_PARAMS(obj)  ((MYSQL_STMTX_UNBOX(obj))->params)
#  define MYSQL_STMTX_PARAM_COUNT(obj) ((MYSQL_STMTX_UNBOX(obj))->param_count)
#  define MYSQL_STMTX_FIELDS(obj)  ((MYSQL_STMTX_UNBOX(obj))->fields)
#  define MYSQL_STMTX_FIELD_COUNT(obj) ((MYSQL_STMTX_UNBOX(obj))->field_count)
#  define MYSQL_STMTX_METARES(obj) ((MYSQL_STMTX_UNBOX(obj))->metares)

   extern MYSQL_STMTX *MysqlStmtxPrepare(MYSQL *connection, ScmString *sql);
   extern void MysqlStmtxExecute(MYSQL_STMTX *stmtx, ScmObj args);
   extern ScmObj MysqlStmtxFetch(MYSQL_STMTX *stmtx);
   extern ScmObj MysqlStmtAffectedRows(MYSQL_STMT *stmt);
   extern ScmObj MysqlStmtxFetchFieldNames(MYSQL_STMTX *stmtx);

   extern void raise_mysql_stmt_error(MYSQL_STMT *stmt, const char *msg);

   extern void mysql_stmtx_cleanup(ScmObj obj);
#endif	/* HAVE_MYSQL_STMT */

#if HAVE_MYSQL_FIELD
   typedef struct {
       SCM_HEADER;
       MYSQL_FIELD *field;
   } ScmMysqlField;
   SCM_CLASS_DECL(Scm_MysqlFieldClass);
#  define SCM_CLASS_MYSQL_FIELD     (&Scm_MysqlFieldClass)
#  define SCM_MYSQL_FIELD(obj)      ((ScmMysqlField*)obj)
#  define MYSQL_FIELD_P(obj)        SCM_XTYPEP(obj, SCM_CLASS_MYSQL_FIELD)
#  define MYSQL_FIELD_UNBOX(obj)    (SCM_MYSQL_FIELD(obj)->field)
#  define MYSQL_FIELD_BOX(field)    Scm_MakeMysqlField(SCM_CLASS_MYSQL_FIELD, field)
#  define MYSQL_FIELD_NAME(obj)     (MYSQL_FIELD_UNBOX(obj)->name)
#  define MYSQL_FIELD_NAME_LENGTH(obj) (MYSQL_FIELD_UNBOX(obj)->name_length)
#  define MYSQL_FIELD_ORG_NAME(obj) (MYSQL_FIELD_UNBOX(obj)->org_name)
#  define MYSQL_FIELD_ORG_NAME_LENGTH(obj) (MYSQL_FIELD_UNBOX(obj)->org_name_length)
#  define MYSQL_FIELD_TABLE(obj)    (MYSQL_FIELD_UNBOX(obj)->table)
#  define MYSQL_FIELD_TABLE_LENGTH(obj) (MYSQL_FIELD_UNBOX(obj)->table_length)
#  define MYSQL_FIELD_ORG_TABLE(obj) (MYSQL_FIELD_UNBOX(obj)->org_table)
#  define MYSQL_FIELD_ORG_TABLE_LENGTH(obj) (MYSQL_FIELD_UNBOX(obj)->org_table_length)
#  define MYSQL_FIELD_DB(obj)       (MYSQL_FIELD_UNBOX(obj)->db)
#  define MYSQL_FIELD_DB_LENGTH(obj) (MYSQL_FIELD_UNBOX(obj)->db_length)
#  define MYSQL_FIELD_CATALOG(obj)  (MYSQL_FIELD_UNBOX(obj)->catalog)
#  define MYSQL_FIELD_CATALOG_LENGTH(obj) (MYSQL_FIELD_UNBOX(obj)->catalog_length)
#  define MYSQL_FIELD_DEF(obj)      (MYSQL_FIELD_UNBOX(obj)->def)
#  define MYSQL_FIELD_DEF_LENGTH(obj) (MYSQL_FIELD_UNBOX(obj)->def_length)
#  define MYSQL_FIELD_LENGTH(obj)   (MYSQL_FIELD_UNBOX(obj)->length)
#  define MYSQL_FIELD_MAX_LENGTH(obj) (MYSQL_FIELD_UNBOX(obj)->max_length)
#  define MYSQL_FIELD_NOT_NULL_P(obj) (SCM_MAKE_BOOL(MYSQL_FIELD_UNBOX(obj)->flags&NOT_NULL_FLAG))
#  define MYSQL_FIELD_PRIMARY_KEY_P(obj) (SCM_MAKE_BOOL(MYSQL_FIELD_UNBOX(obj)->flags&PRI_KEY_FLAG))
#  define MYSQL_FIELD_UNIQUE_KEY_P(obj) (SCM_MAKE_BOOL(MYSQL_FIELD_UNBOX(obj)->flags&UNIQUE_KEY_FLAG))
#  define MYSQL_FIELD_MULTIPLE_KEY_P(obj) (SCM_MAKE_BOOL(MYSQL_FIELD_UNBOX(obj)->flags&MULTIPLE_KEY_FLAG))
#  define MYSQL_FIELD_UNSIGNED_P(obj) (SCM_MAKE_BOOL(MYSQL_FIELD_UNBOX(obj)->flags&UNSIGNED_FLAG))
#  define MYSQL_FIELD_ZEROFILL_P(obj) (SCM_MAKE_BOOL(MYSQL_FIELD_UNBOX(obj)->flags&ZEROFILL_FLAG))
#  define MYSQL_FIELD_BINARY_P(obj) (SCM_MAKE_BOOL(MYSQL_FIELD_UNBOX(obj)->flags&BINARY_FLAG))
#  define MYSQL_FIELD_AUTO_INCREMENT_P(obj) (SCM_MAKE_BOOL(MYSQL_FIELD_UNBOX(obj)->flags&AUTO_INCREMENT_FLAG))
#  define MYSQL_FIELD_DECIMALS(obj) (MYSQL_FIELD_UNBOX(obj)->decimals)
#  define MYSQL_FIELD_CHARSETNR(obj) (MYSQL_FIELD_UNBOX(obj)->charsetnr)
#  define MYSQL_FIELD_TYPE(obj) (MYSQL_FIELD_UNBOX(obj)->type)

   extern ScmObj Scm_MakeMysqlField(const MYSQL_FIELD *field);
   extern ScmObj MysqlFetchFields(MYSQL_RES *result);
#endif	/* HAVE_MYSQL_FIELD */

#if HAVE_MY_CHARSET_INFO
   typedef struct {
       SCM_HEADER;
       MY_CHARSET_INFO info;
   } ScmMysqlCharsetInfo;
   SCM_CLASS_DECL(Scm_MysqlCharsetInfoClass);
#  define SCM_CLASS_MYSQL_CHARSET_INFO  (&Scm_MysqlCharsetInfoClass)
#  define SCM_MYSQL_CHARSET_INFO(obj)   ((ScmMysqlCharsetInfo*)obj)
#  define MYSQL_CHARSET_INFO_P(obj) SCM_XTYPEP(obj, SCM_CLASS_MYSQL_CHARSET_INFO)
#  define MYSQL_CHARSET_INFO_UNBOX(obj)  (SCM_MYSQL_CHARSET_INFO(obj)->info)
#  define MYSQL_CHARSET_INFO_NUMBER(obj) (MYSQL_CHARSET_INFO_UNBOX(obj).number)
#  define MYSQL_CHARSET_INFO_STATE(obj)  (MYSQL_CHARSET_INFO_UNBOX(obj).state)
#  define MYSQL_CHARSET_INFO_CSNAME(obj) (MYSQL_CHARSET_INFO_UNBOX(obj).csname)
#  define MYSQL_CHARSET_INFO_NAME(obj)   (MYSQL_CHARSET_INFO_UNBOX(obj).name)
#  define MYSQL_CHARSET_INFO_COMMENT(obj) (MYSQL_CHARSET_INFO_UNBOX(obj).comment)
#  define MYSQL_CHARSET_INFO_DIR(obj)    (MYSQL_CHARSET_INFO_UNBOX(obj).dir)
#  define MYSQL_CHARSET_INFO_MBMINLEN(obj) (MYSQL_CHARSET_INFO_UNBOX(obj).mbminlen)
#  define MYSQL_CHARSET_INFO_MBMAXLEN(obj) (MYSQL_CHARSET_INFO_UNBOX(obj).mbmaxlen)
#endif	/* HAVE_MY_CHARSET_INFO */

#if HAVE_MYSQL_TIME
   typedef struct {
       SCM_HEADER;
       MYSQL_TIME time;
   } ScmMysqlTime;
   SCM_CLASS_DECL(Scm_MysqlTimeClass);
#  define SCM_CLASS_MYSQL_TIME     (&Scm_MysqlTimeClass)
#  define SCM_MYSQL_TIME(obj)      ((ScmMysqlTime*)obj)
#  define MYSQL_TIME_P(obj)        SCM_XTYPEP(obj, SCM_CLASS_MYSQL_TIME)
#  define MYSQL_TIME_UNBOX(obj)    (SCM_MYSQL_TIME(obj)->time)
#  define MYSQL_TIME_YEAR(obj)     (MYSQL_TIME_UNBOX(obj).year)
#  define MYSQL_TIME_MONTH(obj)    (MYSQL_TIME_UNBOX(obj).month)
#  define MYSQL_TIME_DAY(obj)      (MYSQL_TIME_UNBOX(obj).day)
#  define MYSQL_TIME_HOUR(obj)     (MYSQL_TIME_UNBOX(obj).hour)
#  define MYSQL_TIME_MINUTE(obj)   (MYSQL_TIME_UNBOX(obj).minute)
#  define MYSQL_TIME_SECOND(obj)   (MYSQL_TIME_UNBOX(obj).second)
#  define MYSQL_TIME_NEGATIVE_P(obj) (MYSQL_TIME_UNBOX(obj).neg)
#  define MYSQL_TIME_SECOND_PART(obj) (MYSQL_TIME_UNBOX(obj).second_part)

   extern ScmObj mysql_time_allocate(ScmClass *klass, ScmObj initargs);
   extern ScmObj Scm_MakeMysqlTime(MYSQL_TIME *time);
#endif	/* HAVE_MYSQL_TIME */

/* Epilogue */
SCM_DECL_END

#endif  /* DBD_MYSQL_H */
