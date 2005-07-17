/*
 *  Copyright (c) 2004-2005 Scheme Arts, L.L.C., All rights reserved.
 *  Copyright (c) 2004-2005 Time Intermedia Corporation, All rights reserved.
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
 * $Id: gauche_dbd_mysql.h,v 1.1 2005/07/17 06:43:17 shiro Exp $
 */

/* Prologue */
#ifndef GAUCHE_DBD_MYSQL_H
#define GAUCHE_DBD_MYSQL_H

#include <mysql/mysql.h>
#include <mysql/errmsg.h>
#include <mysql/mysqld_error.h>

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

/*
 * Internal Classes
 */

SCM_CLASS_DECL(Scm_MysqlClass);
SCM_CLASS_DECL(Scm_MysqlResClass);

#define SCM_CLASS_MYSQL (&Scm_MysqlClass)
#define SCM_CLASS_MYSQL_RES (&Scm_MysqlResClass)

typedef struct ScmMysqlRec {
  SCM_HEADER;
  MYSQL *handle;
} ScmMysql;

typedef struct ScmMysqlResRec {
  SCM_HEADER;
  MYSQL_RES *res;
} ScmMysqlRes;

#define SCM_MYSQL(obj)		((ScmMysql *)(obj))
#define SCM_MYSQL_RES(obj)	((ScmMysqlRes *)(obj))

#define SCM_MYSQL_P(obj)	(SCM_XTYPEP(obj, SCM_CLASS_MYSQL))
#define SCM_MYSQL_RES_P(obj)	(SCM_XTYPEP(obj, SCM_CLASS_MYSQL_RES))

/*
 * API
 */

extern ScmObj Scm_MysqlRealConnect(ScmString *host,
				   ScmString *user,
				   ScmString *password,
				   ScmString *db,
				   u_int port,
				   ScmString *unix_socket,
				   u_int client_flag,
				   ScmObj driver,
				   ScmObj connection);

extern ScmObj Scm_MysqlQueryUseResult(ScmString *query,
				      ScmObj connection,
				      ScmObj result);

extern ScmObj Scm_MysqlQueryStoreResult(ScmString *query,
					ScmObj connection,
					ScmObj result);

extern ScmObj Scm_MysqlError(ScmObj connection);

extern ScmObj Scm_MysqlErrno(ScmObj connection);

extern ScmObj Scm_MysqlFetchRow(ScmObj result);

extern ScmObj Scm_MysqlFreeResult(ScmObj result);

extern ScmObj Scm_MysqlClose(ScmObj connection);

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_DBD_MYSQL_H */
