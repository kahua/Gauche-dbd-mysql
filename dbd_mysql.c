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
 * $Id: dbd_mysql.c,v 1.1 2005/08/08 10:17:22 shiro Exp $
 */

#include "dbd_mysql.h"

/*
 * Class stuff
 */

/* Class pointers initialized by Scm_Init_gauche_dbd_mysql */
ScmClass *MysqlHandleClass;
ScmClass *MysqlResClass;

static void mysql_cleanup(ScmObj obj)
{
    MYSQL *s = MYSQL_HANDLE_UNBOX(obj);
    mysql_close(s);
}

static void mysql_res_cleanup(ScmObj obj)
{
    MYSQL_RES *r = MYSQL_RES_UNBOX(obj);
    mysql_free_result(r);
}

/*
 * cprocs
 */

ScmObj MysqlFetchRow(MYSQL_RES *result) {
    ScmObj row = SCM_NIL, t = SCM_NIL;
    MYSQL_ROW mysql_row;
    unsigned int num_fields, i;
    unsigned long *len;

    mysql_row = mysql_fetch_row(result);
    if (mysql_row == NULL) return SCM_FALSE;

    num_fields = mysql_num_fields(result);
    len = mysql_fetch_lengths(result);
    for (i = 0; i < num_fields; i++) {
        SCM_APPEND1(row, t,
                    mysql_row[i] ?
                    Scm_MakeString(mysql_row[i], len[i], -1,
                                   SCM_MAKSTR_COPYING) : SCM_FALSE);
    }
    return row;
}

/*
 * Module initialization function.
 */
extern void Scm_Init_dbd_mysqllib(ScmModule*);

ScmObj Scm_Init_dbd_mysql(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(dbd_mysql);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("dbd.mysql", TRUE));

    /* Register classes */
    MysqlHandleClass =
        Scm_MakeForeignPointerClass(mod, "<mysql-handle>",
                                    NULL, mysql_cleanup, 0);
    MysqlResClass =
        Scm_MakeForeignPointerClass(mod, "<mysql-res>",
                                    NULL, mysql_res_cleanup, 0);

    /* Register stub-generated procedures */
    Scm_Init_dbd_mysqllib(mod);
}
