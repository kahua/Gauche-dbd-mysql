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
 * $Id: gauche_dbd_mysql.c,v 1.1 2005/07/17 06:43:17 shiro Exp $
 */

#include "gauche_dbd_mysql.h"

/*
 * static function prototypes
 */

static ScmObj mysql_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj mysql_res_allocate(ScmClass *klass, ScmObj initargs);

static void mysql_print(ScmObj mysql, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#<mysql %p>", SCM_MYSQL(mysql)->handle);
}

/*
 * class definitions
 */

SCM_DEFINE_BUILTIN_CLASS(Scm_MysqlClass,
			 mysql_print, NULL, NULL,
			 mysql_allocate,
			 NULL);

SCM_DEFINE_BUILTIN_CLASS(Scm_MysqlResClass,
			 NULL, NULL, NULL,
			 mysql_res_allocate,
			 NULL);

/*
 * allocators
 */
static ScmObj mysql_allocate(ScmClass *klass, ScmObj initargs) {
  ScmMysql *h = SCM_NEW(ScmMysql);
  SCM_SET_CLASS(h, SCM_CLASS_MYSQL);
  memset(&h->handle, 0, sizeof(h->handle));
  return SCM_OBJ(h);
}

static ScmObj mysql_res_allocate(ScmClass *klass, ScmObj initargs) {
  ScmMysqlRes *r = SCM_NEW(ScmMysqlRes);
  SCM_SET_CLASS(r, SCM_CLASS_MYSQL_RES);
  memset(&r->res, 0, sizeof(r->res));
  return SCM_OBJ(r);
}

/*
 * cprocs
 */

ScmObj Scm_MysqlRealConnect(ScmString *host,
			    ScmString *user,
			    ScmString *password,
			    ScmString *db,
			    u_int port,
			    ScmString *unix_socket,
			    u_int client_flag,
			    ScmObj driver,
			    ScmObj connection)
{
  ScmMysql *d, *c;
  char *h = NULL, *u = NULL, *p = NULL, *b = NULL, *s = NULL;

  if (SCM_MYSQL_P(driver) && SCM_MYSQL_P(connection)) {
    d = SCM_MYSQL(driver);
    c = SCM_MYSQL(connection);
    if (d->handle == NULL) d->handle = SCM_NEW(MYSQL); // once at 1st time.
    mysql_init(d->handle);
    if (host->length > 0)	 h = Scm_GetString(host); 
    if (user->length > 0)	 u = Scm_GetString(user); 
    if (password->length > 0)	 p = Scm_GetString(password); 
    if (db->length > 0)		 b = Scm_GetString(db); 
    if (unix_socket->length > 0) s = Scm_GetString(unix_socket);
    c->handle =
      mysql_real_connect(d->handle, h, u, p, b, port, s, client_flag);
    if (c->handle == NULL) return SCM_FALSE;
  } else return SCM_FALSE;
 
  return connection;
}

ScmObj Scm_MysqlQueryUseResult(ScmString *query,
			       ScmObj connection,
			       ScmObj result)
{
  ScmMysql *c;
  ScmMysqlRes *r;

  if (SCM_MYSQL_P(connection) && SCM_MYSQL_RES_P(result)) {
    c = SCM_MYSQL(connection);
    r = SCM_MYSQL_RES(result);
    mysql_query(c->handle, Scm_GetString(query));
    r->res = mysql_use_result(c->handle);
    //if (r->res == NULL) return SCM_FALSE;
  }

  return result;
}

ScmObj Scm_MysqlQueryStoreResult(ScmString *query,
				 ScmObj connection,
				 ScmObj result)
{
  ScmMysql *c;
  ScmMysqlRes *r;

  if (SCM_MYSQL_P(connection) && SCM_MYSQL_RES_P(result)) {
    c = SCM_MYSQL(connection);
    r = SCM_MYSQL_RES(result);
    mysql_query(c->handle, Scm_GetString(query));
    r->res = mysql_store_result(c->handle);
    //if (r->res == NULL) return SCM_FALSE;
  }

  return result;
}

ScmObj Scm_MysqlError(ScmObj connection) {
  ScmObj message = SCM_FALSE;
  ScmMysql *c;
  const char *mysql_message;

  if (SCM_MYSQL_P(connection)) {
    c = SCM_MYSQL(connection);
    mysql_message = mysql_error(c->handle);
    if (mysql_message == NULL) return SCM_FALSE;
    message = SCM_MAKE_STR_COPYING(mysql_message);
  }

  return message;
}

ScmObj Scm_MysqlErrno(ScmObj connection) {
  ScmObj error_number = SCM_FALSE;
  ScmMysql *c;
  unsigned int mysql_error_number;

  if (SCM_MYSQL_P(connection)) {
    c = SCM_MYSQL(connection);
    mysql_error_number = mysql_errno(c->handle);
    error_number = SCM_MAKE_INT(mysql_error_number);
  } else return SCM_FALSE;

  return error_number;
}

ScmObj Scm_MysqlFetchRow(ScmObj result) {
  ScmMysqlRes *r;
  ScmObj row = SCM_NIL, t = SCM_NIL;
  MYSQL_ROW mysql_row;
  unsigned int num_fields, i;
  unsigned long *len;

  if (SCM_MYSQL_RES_P(result) && SCM_MYSQL_RES(result)->res) {
    r = SCM_MYSQL_RES(result);
    mysql_row = mysql_fetch_row(r->res);
    if (mysql_row == NULL) return SCM_FALSE;

    num_fields = mysql_num_fields(r->res);
    len = mysql_fetch_lengths(r->res);
    for (i = 0; i < num_fields; i++) {
      SCM_APPEND1(row, t,
		  mysql_row[i] ?
		  Scm_MakeString(mysql_row[i], len[i], -1, SCM_MAKSTR_COPYING) : SCM_FALSE);
    }
  }
  
  return row;
}

ScmObj Scm_MysqlFreeResult(ScmObj result) {
  ScmMysqlRes *r;

  if (SCM_MYSQL_RES_P(result) && SCM_MYSQL_RES(result)->res) {
    r = SCM_MYSQL_RES(result);
    mysql_free_result(r->res);
    r->res = NULL;
  } else return SCM_FALSE;
  return result;
}

ScmObj Scm_MysqlClose(ScmObj connection) {
  ScmMysql *c;

  if (SCM_MYSQL_P(connection)) {
    c = SCM_MYSQL(connection);
    mysql_close(c->handle);
  } else return SCM_FALSE;

  return connection;
}

/*
 * Module initialization function.
 */
extern void Scm_Init_gauche_dbd_mysqllib(ScmModule*);

ScmObj Scm_Init_gauche_dbd_mysql(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(gauche_dbd_mysql);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("dbd.mysql", TRUE));

    /* Register classes */
    Scm_InitStaticClass(&Scm_MysqlClass, "<mysql-handle>",
                        mod, NULL, 0);
    Scm_InitStaticClass(&Scm_MysqlResClass, "<mysql-res>",
                        mod, NULL, 0);

    /* Register stub-generated procedures */
    Scm_Init_gauche_dbd_mysqllib(mod);
}
