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
 * $Id: dbd_mysql.h,v 1.1 2005/08/08 10:17:22 shiro Exp $
 */

/* Prologue */
#ifndef DBD_MYSQL_H
#define DBD_MYSQL_H

#include <mysql/mysql.h>
#include <mysql/errmsg.h>
#include <mysql/mysqld_error.h>

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

/*
 * Internal Classes
 */

extern ScmClass *MysqlHandleClass;
typedef ScmForeignPointer MysqlHandle;
#define MYSQL_HANDLE_P(obj)   SCM_XTYPEP(obj, MysqlHandleClass)
#define MYSQL_HANDLE_UNBOX(obj)  \
    ((MYSQL*)(SCM_FOREIGN_POINTER_REF(obj)))
#define MYSQL_HANDLE_BOX(handle) \
    Scm_MakeForeignPointer(MysqlHandleClass, handle)

extern ScmClass *MysqlResClass;
typedef ScmForeignPointer ScmMysqlRes;
#define MYSQL_RES_P(obj)   SCM_XTYPEP(obj, MysqlResClass)
#define MYSQL_RES_UNBOX(obj) \
    ((MYSQL_RES*)(SCM_FOREIGN_POINTER_REF(obj)))
#define MYSQL_RES_BOX(res) \
    Scm_MakeForeignPointer(MysqlResClass, res)


/*
 * API
 */

extern ScmObj MysqlFetchRow(MYSQL_RES *result);

/* Epilogue */
SCM_DECL_END

#endif  /* DBD_MYSQL_H */
