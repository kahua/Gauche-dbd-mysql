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
 * $Id: dbd_mysql.c,v 1.5 2007/02/15 06:04:10 bizenn Exp $
 */

#include "dbd_mysql.h"

/*
 * Class stuff
 */

/* Class pointers initialized by Scm_Init_gauche_dbd_mysql */
ScmClass *MysqlHandleClass;
ScmClass *MysqlResClass;
ScmClass *MysqlStmtClass;

static void mysql_cleanup(ScmObj obj)
{
    if (!MysqlClosedP(obj)) {
        MYSQL *s = MYSQL_HANDLE_UNBOX(obj);
        mysql_close(s);
    }
}

static void mysql_res_cleanup(ScmObj obj)
{
    if (!MysqlClosedP(obj)) {
        MYSQL_RES *r = MYSQL_RES_UNBOX(obj);
        mysql_free_result(r);
    }
}

static void mysql_stmt_cleanup(ScmObj obj)
{
    MYSQL_STMT *stmt = MYSQL_STMT_UNBOX(obj);
    mysql_stmt_close(stmt);
}

/*
 * Auxiliary utilities
 */
/* Symbol 'closed? */
static ScmObj sym_closed;

int MysqlClosedP(ScmObj obj)
{
    SCM_ASSERT(SCM_FOREIGN_POINTER_P(obj));
    return !SCM_FALSEP(Scm_ForeignPointerAttrGet(SCM_FOREIGN_POINTER(obj),
                                                 sym_closed, SCM_FALSE));
}

void MysqlMarkClosed(ScmObj obj)
{
    SCM_ASSERT(SCM_FOREIGN_POINTER_P(obj));
    Scm_ForeignPointerAttrSet(SCM_FOREIGN_POINTER(obj),
                              sym_closed, SCM_TRUE);
}

ScmObj MysqlFetchFieldNames(MYSQL_RES *result)
{
    MYSQL_FIELD *fields;
    int nfields, i;
    ScmObj v;

    if (result == NULL) return SCM_FALSE;
    
    nfields = mysql_num_fields(result);
    fields = mysql_fetch_fields(result);
    v = Scm_MakeVector(nfields, SCM_FALSE);
    for (i=0; i<nfields; i++) {
        SCM_VECTOR_ELEMENTS(v)[i] = SCM_MAKE_STR_COPYING(fields[i].name);
    }
    return v;
}

ScmObj MysqlFetchRow(MYSQL_RES *result)
{
    MYSQL_ROW row;
    unsigned int nfields, i;
    unsigned long *len;
    ScmObj v;

    if (result == NULL) return SCM_FALSE;
    row = mysql_fetch_row(result);
    if (row == NULL) return SCM_FALSE;

    nfields = mysql_num_fields(result);
    len = mysql_fetch_lengths(result);
    v = Scm_MakeVector(nfields, SCM_FALSE);
    for (i = 0; i < nfields; i++) {
        if (row[i] == NULL) continue;
        SCM_VECTOR_ELEMENTS(v)[i] =
            Scm_MakeString(row[i], len[i], -1, SCM_MAKSTR_COPYING);
    }
    return v;
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
    MysqlStmtClass =
	Scm_MakeForeignPointerClass(mod, "<mysql-stmt>",
				    NULL, mysql_stmt_cleanup, 0);

    /* Get handle of the symbol 'closed? */
    sym_closed = SCM_INTERN("closed?");

    /* Register stub-generated procedures */
    Scm_Init_dbd_mysqllib(mod);
}
