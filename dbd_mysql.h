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
 * $Id: dbd_mysql.h,v 1.15 2007/02/20 09:07:58 bizenn Exp $
 */

/* Prologue */
#ifndef DBD_MYSQL_H
#define DBD_MYSQL_H

#include <mysql.h>
#include <errmsg.h>
#include <mysqld_error.h>

#include <gauche.h>
#include <gauche/extend.h>

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
#define MYSQL_STMTX_P(obj)         SCM_XTYPEP(obj, MysqlStmtxClass)
#define MYSQL_STMTX_UNBOX(obj)     SCM_FOREIGN_POINTER_REF(MYSQL_STMTX*, obj)
#define MYSQL_STMTX_BOX(stmtx)     Scm_MakeForeignPointer(MysqlStmtxClass, stmtx)
#define MYSQL_STMTX_STMT(obj)    ((MYSQL_STMTX_UNBOX(obj))->stmt)
#define MYSQL_STMTX_PARAMS(obj)  ((MYSQL_STMTX_UNBOX(obj))->params)
#define MYSQL_STMTX_PARAM_COUNT(obj) ((MYSQL_STMTX_UNBOX(obj))->param_count)
#define MYSQL_STMTX_FIELDS(obj)  ((MYSQL_STMTX_UNBOX(obj))->fields)
#define MYSQL_STMTX_FIELD_COUNT(obj) ((MYSQL_STMTX_UNBOX(obj))->field_count)
#define MYSQL_STMTX_METARES(obj) ((MYSQL_STMTX_UNBOX(obj))->metares)

/*
 * API
 */

extern int MysqlClosedP(ScmObj obj);
extern void MysqlMarkClosed(ScmObj obj);

extern ScmObj MysqlAffectedRows(MYSQL *handle);
extern ScmObj MysqlFetchFieldNames(MYSQL_RES *result);
extern ScmObj MysqlFetchRow(MYSQL_RES *result);

extern MYSQL_STMTX *MysqlStmtxPrepare(MYSQL *connection, ScmString *sql);
extern void MysqlStmtxExecute(MYSQL_STMTX *stmtx, ScmObj args);
extern ScmObj MysqlStmtxFetch(MYSQL_STMTX *stmtx);
extern ScmObj MysqlStmtAffectedRows(MYSQL_STMT *stmt);

extern void raise_mysql_error(MYSQL *handle, const char *msg);
extern void raise_mysql_stmt_error(MYSQL_STMT *stmt, const char *msg);

extern void mysql_cleanup(ScmObj obj);
extern void mysql_res_cleanup(ScmObj obj);
extern void mysql_stmtx_cleanup(ScmObj obj);

/* Epilogue */
SCM_DECL_END

#endif  /* DBD_MYSQL_H */
