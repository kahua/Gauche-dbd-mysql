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
 * $Id: dbd_mysql.c,v 1.29 2007/04/17 15:41:47 bizenn Exp $
 */

#include "dbd_mysql.h"
#include <stdlib.h>

#define MYSQL_ERROR      SCM_SYMBOL_VALUE("dbd.mysql", "<mysql-error>")
#if HAVE_DECL_MYSQL_SQLSTATE
# define MYSQL_SQLSTATE(handle) (SCM_MAKE_STR_IMMUTABLE(mysql_sqlstate(handle)))
#else
# define MYSQL_SQLSTATE(handle) (SCM_MAKE_STR_IMMUTABLE("HY000"))
#endif
void raise_mysql_error(MYSQL *handle, const char *msg)
{
    Scm_RaiseCondition(MYSQL_ERROR,
		       "error-code", SCM_MAKE_INT(mysql_errno(handle)),
		       "sql-code",   MYSQL_SQLSTATE(handle),
		       SCM_RAISE_CONDITION_MESSAGE, "%s: %s", msg, mysql_error(handle));
}

/*
 * Class stuff
 */

/* Class pointers initialized by Scm_Init_gauche_dbd_mysql */
ScmClass *MysqlHandleClass;
ScmClass *MysqlResClass;

void mysql_cleanup(ScmObj obj)
{
    if (!MysqlClosedP(obj)) {
        MYSQL *s = MYSQL_HANDLE_UNBOX(obj);
        mysql_close(s);
	MysqlMarkClosed(obj);
    }
}

void mysql_res_cleanup(ScmObj obj)
{
    if (!MysqlClosedP(obj)) {
        MYSQL_RES *r = MYSQL_RES_UNBOX(obj);
        mysql_free_result(r);
	MysqlMarkClosed(obj);
    }
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

#if   defined(GAUCHE_CHAR_ENCODING_EUC_JP)
static const char *default_character_encoding = "ujis";
static const char setnames[] = "SET NAMES ujis";
#elif defined(GAUCHE_CHAR_ENCODING_UTF_8)
static const char *default_character_encoding = "utf8";
static const char setnames[] = "SET NAMES utf8";
#elif defined(GAUCHE_CHAR_ENCODING_SJIS)
static const char *default_character_encoding = "sjis";
static const char setnames[] = "SET NAMES sjis";
#else  /* NONE */
static const char *default_character_encoding = "binary"; /* FIXME!! */
static const char setnames[] = "SET NAMES binary";
#endif

#if HAVE_DECL_MY_CS_AVAILABLE
# define GET_CHARSET_NUMBER(csname)  get_charset_number((csname), MY_CS_AVAILABLE)
#else
# define GET_CHARSET_NUMBER(csname)  get_charset_number(csname)
#endif

MYSQL *MysqlRealConnect(const char *host,
			const char *user,
			const char *password,
			const char *db,
			unsigned int port,
			const char *unix_socket,
			unsigned int client_flag)
{
    MYSQL *conn, *handle = SCM_NEW(MYSQL);
    if ((handle = mysql_init(handle)) == NULL)
	Scm_SysError("Cannot initialize MYSQL structure.");
#if !HAVE_DECL_MYSQL_SET_CHARACTER_SET
    if (GET_CHARSET_NUMBER(default_character_encoding) > 0)
	if (mysql_options(handle, MYSQL_SET_CHARSET_NAME, default_character_encoding) != 0)
	    raise_mysql_error(handle, "mysql_option w/ MYSQL_SET_CHARSET_NAME");
#endif
    mysql_options(handle, MYSQL_READ_DEFAULT_GROUP, "client");
    if ((conn = mysql_real_connect(handle, host, user, password, db, port, unix_socket, client_flag)) == NULL)
	raise_mysql_error(handle, "mysql_real_connect");
#if HAVE_DECL_MYSQL_SET_CHARACTER_SET
    mysql_set_character_set(conn, default_character_encoding); /* Ignore even if error occured */
#else
    mysql_real_query(handle, setnames, sizeof(setnames)-1);    /* Ignore even if error occured */
#endif	/* HAVE_DECL_MYSQL_SET_CHARACTER_SET */
    return conn;
}

ScmObj MysqlAffectedRows(MYSQL *handle)
{
    my_ulonglong n;

    SCM_ASSERT(handle != NULL);
    n = mysql_affected_rows(handle);
    if (n == (my_ulonglong)~0)
	raise_mysql_error(handle, "mysql_affected_rows");
    return Scm_MakeIntegerU64(n);
}

ScmObj MysqlFetchFieldNames(MYSQL_RES *result)
{
    MYSQL_FIELD *fields = NULL;
    int nfields = 0, i;
    ScmObj v;

    if (result != NULL) {
	nfields = mysql_num_fields(result);
	fields = mysql_fetch_fields(result);
    }
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

    SCM_ASSERT(result != NULL);
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

#if HAVE_MYSQL_FIELD

ScmObj Scm_MakeMysqlField(const MYSQL_FIELD *field)
{
    ScmMysqlField *obj = SCM_NEW_ATOMIC2(ScmMysqlField*, sizeof(ScmMysqlField));

    SCM_ASSERT(field != NULL);
    SCM_SET_CLASS(obj, SCM_CLASS_MYSQL_FIELD);
    MYSQL_FIELD_UNBOX(obj) = (MYSQL_FIELD*)field;
    SCM_RETURN(SCM_OBJ(obj));
}

#endif	/* HAVE_MYSQL_FIELD */

#if HAVE_MYSQL_STMT

ScmClass *MysqlStmtxClass;

void mysql_stmtx_cleanup(ScmObj obj)
{
    if (!MysqlClosedP(obj)) {
	MYSQL_STMT *stmt = MYSQL_STMTX_STMT(obj);
	MYSQL_BIND *params = MYSQL_STMTX_PARAMS(obj);
	unsigned int param_count = MYSQL_STMTX_PARAM_COUNT(obj);
	MYSQL_BIND *fields = MYSQL_STMTX_FIELDS(obj);
	unsigned int field_count = MYSQL_STMTX_FIELD_COUNT(obj);
	MYSQL_RES  *metares = MYSQL_STMTX_METARES(obj);

	if (stmt != NULL) {
	    MYSQL_STMTX_STMT(obj) = NULL;
	    mysql_stmt_free_result(stmt);
	    mysql_stmt_close(stmt);
	}
	if (params != NULL) {
	    int i;
	    MYSQL_STMTX_PARAMS(obj) = NULL;
	    for (i = 0; i <param_count; i++) {
		if (IS_NUM(params[i].buffer_type)) {
		    if (params[i].buffer != NULL) free(params[i].buffer);
		    if (params[i].is_null != NULL) free(params[i].is_null);
		}
	    }
	    free(params);
	}
	if (fields != NULL) {
	    int i;
	    MYSQL_STMTX_FIELDS(obj) = NULL;
	    for (i = 0; i < field_count; i++) {
		if (fields[i].buffer != NULL) free(fields[i].buffer);
		if (fields[i].length != NULL) free(fields[i].length);
		if (fields[i].is_null != NULL) free(fields[i].is_null);
	    }
	    free(fields);
	}
	if (metares != NULL) {
	    MYSQL_STMTX_METARES(obj) = NULL;
	    mysql_free_result(metares);
	}
	MysqlMarkClosed(obj);
    }
}

static MYSQL_STMTX *make_mysql_stmtx(void)
{
    MYSQL_STMTX *stmtx;

    stmtx = SCM_NEW_ATOMIC2(MYSQL_STMTX*, sizeof(MYSQL_STMTX));
    stmtx->stmt = NULL;
    stmtx->params = NULL;
    stmtx->fields = NULL;
    stmtx->metares = NULL;
    return stmtx;
}

MYSQL_STMTX *MysqlStmtxPrepare(MYSQL *connection, ScmString *sql)
{
    MYSQL_STMTX *stmtx = make_mysql_stmtx();
    const ScmStringBody *sql_body = SCM_STRING_BODY(sql);
    const char *sql_p = SCM_STRING_BODY_START(sql_body);
    unsigned int sql_size = SCM_STRING_BODY_SIZE(sql_body);
    MYSQL_STMT *stmt = mysql_stmt_init(connection);
    MYSQL_BIND *params = NULL;
    unsigned int param_count = 0;
    MYSQL_RES  *metares = NULL;

    if (stmt == NULL)
	raise_mysql_error(connection, "mysql_stmt_init");
    if (mysql_stmt_prepare(stmt, sql_p, sql_size) != 0)
	raise_mysql_stmt_error(stmt, "mysql_stmt_prepare");
    stmtx->stmt = stmt;

    if ((param_count = mysql_stmt_param_count(stmt)) > 0)
	if ((params = (MYSQL_BIND*)calloc(param_count, sizeof(MYSQL_BIND))) == NULL)
	    Scm_SysError("Cannot allocate MYSQL_BIND buffers.");
    stmtx->params = params;

    if ((metares = mysql_stmt_result_metadata(stmt)) == NULL)
	if (mysql_stmt_errno(stmt) != 0)
	    raise_mysql_stmt_error(stmt, "mysql_stmt_result_metadata");
    stmtx->metares = metares;

    return stmtx;
}

static void mysql_init_param_string(MYSQL_BIND *param, ScmObj obj)
{
    const ScmStringBody *body = SCM_STRING_BODY(obj);
    param->buffer_type = SCM_STRING_INCOMPLETE_P(obj) ? MYSQL_TYPE_BLOB : MYSQL_TYPE_STRING;
    param->buffer = (void*)SCM_STRING_BODY_START(body);
    param->buffer_length = SCM_STRING_BODY_SIZE(body);
    param->length = &(param->buffer_length);
}

static void mysql_init_param(MYSQL_BIND *param, ScmObj obj)
{
    if (SCM_STRINGP(obj))
	mysql_init_param_string(param, obj);
    else if (SCM_SYMBOLP(obj))
	mysql_init_param_string(param, SCM_OBJ(SCM_SYMBOL_NAME(obj)));
    else if (SCM_INTEGERP(obj)) {
	long long int *p = (long long int*)malloc(sizeof(long long int));
	if (p == NULL)
	    Scm_SysError("Cannot allocate Integer buffer in mysql-stmt-execute");
	*p = (long long int)Scm_GetInteger64(obj);
	param->buffer = p;
	param->buffer_type = MYSQL_TYPE_LONGLONG;
    }
    else if (SCM_FALSEP(obj)) {
	param->buffer_type = MYSQL_TYPE_NULL;
	if ((param->is_null = malloc(sizeof(my_bool))) == NULL)
	    Scm_SysError("Cannot allocate is_null buffer.");
	else
	    *(my_bool*)param->is_null = 1;
    }
    else if (SCM_FLONUMP(obj)) {
	double *p = (double*)malloc(sizeof(double));
	if (p == NULL)
	    Scm_SysError("Cannt allocate Double buffer in mysql-stmt-execute");
	*p = (double)Scm_GetDouble(obj);
	param->buffer = p;
	param->buffer_type = MYSQL_TYPE_DOUBLE;
    }
    else if (MYSQL_TIME_P(obj)) {
	param->buffer_type = MYSQL_TYPE_DATETIME;
	param->buffer = malloc(sizeof(MYSQL_TIME));
	if (param->buffer == NULL)
	    Scm_SysError("Cannot allocate MYSQL_TIME buffer in mysql-stmt-execute");
	*(MYSQL_TIME*)param->buffer = MYSQL_TIME_UNBOX(obj);
    }
    else
	Scm_RaiseCondition(MYSQL_ERROR, "error-code", SCM_MAKE_INT(0), "sql-code", SCM_MAKE_STR_IMMUTABLE(""),
			   SCM_RAISE_CONDITION_MESSAGE, "Parameter is not supported type: %S", obj);
}

#  if HAVE_MYSQL_BIND
#    if HAVE_MYSQL_FIELD
static void mysql_init_field(MYSQL_BIND *bind, MYSQL_FIELD *field)
{
    SCM_ASSERT(bind != NULL);
    SCM_ASSERT(field != NULL);

    switch (field->type) {
	case MYSQL_TYPE_TINY: case MYSQL_TYPE_SHORT: case MYSQL_TYPE_INT24:
	case MYSQL_TYPE_LONG: case MYSQL_TYPE_LONGLONG:
	    bind->buffer_type = MYSQL_TYPE_LONGLONG;
	    bind->buffer_length = sizeof(long long int);
	    if ((bind->buffer = malloc(bind->buffer_length)) == NULL)
		Scm_SysError("Cannot allocate bind buffer.");
	    bind->length = 0;
	    if ((bind->is_null = malloc(sizeof(my_bool))) == NULL)
		Scm_SysError("Cannot allocate is_null buffer.");
	    break;
	case MYSQL_TYPE_FLOAT: case MYSQL_TYPE_DOUBLE:
	    bind->buffer_type = MYSQL_TYPE_DOUBLE;
	    bind->buffer_length = sizeof(double);
	    if ((bind->buffer = malloc(sizeof(double))) == NULL)
		Scm_SysError("Cannot allocate bind buffer.");
	    bind->length = 0;
	    if ((bind->is_null = malloc(sizeof(my_bool))) == NULL)
		Scm_SysError("Cannot allocate is_null buffer.");
	    break;
	case MYSQL_TYPE_STRING: case MYSQL_TYPE_VAR_STRING:
	    bind->buffer_type = field->type;
	    bind->buffer_length = field->length;
	    if ((bind->buffer = malloc(bind->buffer_length)) == NULL)
		Scm_SysError("Cannot allocate bind buffer.");
	    if ((bind->length = malloc(sizeof(unsigned long))) == NULL)
		Scm_SysError("Cannot allocate length buffer.");
	    if ((bind->is_null = malloc(sizeof(my_bool))) == NULL)
		Scm_SysError("Cannot allocate is_null buffer.");
	    break;
	case MYSQL_TYPE_TINY_BLOB: case MYSQL_TYPE_BLOB:
	case MYSQL_TYPE_MEDIUM_BLOB: case MYSQL_TYPE_LONG_BLOB:
	    bind->buffer_type = field->type;
	    bind->buffer_length = field->max_length;
	    if ((bind->buffer = malloc(bind->buffer_length)) == NULL)
		Scm_SysError("Cannot allocate bind buffer.");
	    if ((bind->length = malloc(sizeof(unsigned long))) == NULL)
		Scm_SysError("Cannot allocate length buffer.");
	    if ((bind->is_null = malloc(sizeof(my_bool))) == NULL)
		Scm_SysError("Cannot allocate is_null buffer.");
	    break;
	case MYSQL_TYPE_DATE: case MYSQL_TYPE_TIME:
	case MYSQL_TYPE_DATETIME: case MYSQL_TYPE_TIMESTAMP:
	    bind->buffer_type = field->type;
	    bind->buffer_length = sizeof(MYSQL_TIME);
	    if ((bind->buffer = calloc(1, bind->buffer_length)) == NULL)
		Scm_SysError("Cannot allocate bind buffer.");
	    if ((bind->length = malloc(sizeof(unsigned long))) == NULL)
		Scm_SysError("Cannot allocate bind buffer.");
	    if ((bind->is_null = malloc(sizeof(my_bool))) == NULL)
		Scm_SysError("Cannot allocate bind buffer.");
	    break;
	default:
	    Scm_RaiseCondition(MYSQL_ERROR, "error-code", SCM_MAKE_INT(0), "sql-code", SCM_MAKE_STR_IMMUTABLE(""),
			       SCM_RAISE_CONDITION_MESSAGE, "Unsupported field type: %d", field->type);
    }
}
#    endif  /* HAVE_MYSQL_FIELD */

void MysqlStmtxExecute(MYSQL_STMTX *stmtx, ScmObj args)
{
    MYSQL_STMT *stmt;
    MYSQL_BIND *params, *param;
    MYSQL_BIND *fields, *field;
    unsigned int param_count, field_count, i;
    ScmObj ptr, obj;
    my_bool fix_metadata = 0;

    SCM_ASSERT(stmtx != NULL);
    SCM_ASSERT(args != NULL);
    SCM_ASSERT(SCM_LISTP(args));

    stmt = stmtx->stmt;
    params = stmtx->params;
    stmtx->param_count = param_count = mysql_stmt_param_count(stmt);
    memset(params, 0, sizeof(MYSQL_BIND)*param_count);
    for (ptr = args, i = 0, param = params;
	 !SCM_NULLP(ptr);
	 ptr = Scm_Cdr(ptr), i++, param++) {
	obj = Scm_Car(ptr);
	if (i >= param_count)
	    Scm_RaiseCondition(MYSQL_ERROR, "error-code", SCM_MAKE_INT(0), "sql-code", SCM_MAKE_STR_IMMUTABLE(""),
			       SCM_RAISE_CONDITION_MESSAGE, "mysql-stmt-execute require %d parameters, but got %d parameters",
			       param_count, Scm_Length(args));
	mysql_init_param(param, obj);
    }
    if (i != param_count)
	Scm_RaiseCondition(MYSQL_ERROR, "error-code", SCM_MAKE_INT(0), "sql-code", SCM_MAKE_STR_IMMUTABLE(""),
			   SCM_RAISE_CONDITION_MESSAGE, "mysql-stmt-execute require %d parameters, but got %d parameters",
			   param_count, i);
    if (mysql_stmt_bind_param(stmt, params) != 0)
	raise_mysql_stmt_error(stmt, "mysql_stmt_bind_param");
    stmtx->field_count = field_count = mysql_stmt_field_count(stmt);
    if (field_count > 0) {
	int i;
	MYSQL_FIELD *field = mysql_fetch_fields(stmtx->metares);
	for (i = 0; i < field_count; i++, field++) {
	    if (field->type == MYSQL_TYPE_BLOB) {
		fix_metadata = 1;
		mysql_stmt_attr_set(stmt, STMT_ATTR_UPDATE_MAX_LENGTH, &fix_metadata);
		break;
	    }
	}
    }
    if (mysql_stmt_execute(stmt) != 0)
	raise_mysql_stmt_error(stmt, "mysql_stmt_execute");
    if (mysql_stmt_store_result(stmt) != 0)
	raise_mysql_stmt_error(stmt, "mysql_stmt_store_result");
    if (fix_metadata) {
	MYSQL_RES *metares;
	if ((metares = mysql_stmt_result_metadata(stmt)) == NULL)
	    if (mysql_stmt_errno(stmt) != 0)
		raise_mysql_stmt_error(stmt, "mysql_stmt_result_metadata");
	mysql_free_result(stmtx->metares);
	stmtx->metares = metares;
    }
    if (field_count > 0) {
	int i;
	if ((fields = (MYSQL_BIND*)calloc(field_count, sizeof(MYSQL_BIND))) == NULL)
	    Scm_SysError("Cannot allocate MYSQL_BIND buffers");
	stmtx->fields = fields;
	for (i = 0, field = fields; i < field_count; i++, field++)
	    mysql_init_field(field, mysql_fetch_field_direct(stmtx->metares, i));
	if (mysql_stmt_bind_result(stmt, fields) != 0)
	    raise_mysql_stmt_error(stmt, "mysql_stmt_bind_result");
    }
}

static ScmObj mysql_bind_to_scm_obj(MYSQL_BIND *bind)
{
    switch (bind->buffer_type) {
	case MYSQL_TYPE_STRING: case MYSQL_TYPE_VAR_STRING:
	case MYSQL_TYPE_TINY_BLOB: case MYSQL_TYPE_BLOB:
	case MYSQL_TYPE_MEDIUM_BLOB: case MYSQL_TYPE_LONG_BLOB:
	    return Scm_MakeString(bind->buffer, *bind->length, -1, SCM_MAKSTR_COPYING);
	case MYSQL_TYPE_LONGLONG:
	    return Scm_MakeInteger64(*(long long int*)bind->buffer);
	case MYSQL_TYPE_DOUBLE:
	    return Scm_MakeFlonum(*(double*)bind->buffer);
	case MYSQL_TYPE_DATETIME:
	    return Scm_MakeMysqlTime((MYSQL_TIME*)bind->buffer);
	default:
	    Scm_RaiseCondition(MYSQL_ERROR, "error-code", SCM_MAKE_INT(0), "sql-code", SCM_MAKE_STR_IMMUTABLE(""),
			       SCM_RAISE_CONDITION_MESSAGE, "Unsupported type: %d", bind->buffer_type);
    }
    return SCM_NIL;		/* NOTREACHED */
}

ScmObj MysqlStmtxFetch(MYSQL_STMTX *stmtx)
{
    unsigned int field_count, i;
    MYSQL_BIND *field;
    MYSQL_STMT *stmt;
    ScmObj v;

    SCM_ASSERT(stmtx != NULL);
    stmt = stmtx->stmt;
    field_count = stmtx->field_count;
    switch (mysql_stmt_fetch(stmt)) {
	case 0: break;
	case MYSQL_NO_DATA: return SCM_FALSE;
	case MYSQL_DATA_TRUNCATED: break; /* FIXME */
	default: raise_mysql_stmt_error(stmt, "mysql_stmt_fetch"); /* Maybe 1 */
    }
    v = Scm_MakeVector(field_count, SCM_FALSE);
    for (i = 0, field = stmtx->fields; i < field_count; i++, field++)
	if ((field->is_null == NULL) || (!*field->is_null))
	    SCM_VECTOR_ELEMENTS(v)[i] = mysql_bind_to_scm_obj(field);
    return v;
}
#  endif	  /* HAVE_MYSQL_BIND */

ScmObj MysqlStmtAffectedRows(MYSQL_STMT *stmt)
{
    my_ulonglong n;

    SCM_ASSERT(stmt != NULL);
    n = mysql_stmt_affected_rows(stmt);
    if (n == (my_ulonglong)~0)
	raise_mysql_stmt_error(stmt, "mysql_stmt_affected_rows");
    return Scm_MakeIntegerU64(n);
}

ScmObj MysqlStmtxFetchFieldNames(MYSQL_STMTX *stmtx)
{
    SCM_ASSERT(stmtx != NULL);
    return MysqlFetchFieldNames(stmtx->metares);
}

#  if HAVE_MYSQL_TIME

ScmObj mysql_time_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmMysqlTime *t = SCM_NEW_ATOMIC2(ScmMysqlTime*, sizeof(ScmMysqlTime));
    SCM_SET_CLASS(t, klass);
    return SCM_OBJ(t);
}

ScmObj Scm_MakeMysqlTime(MYSQL_TIME *time)
{
    ScmObj t = mysql_time_allocate(SCM_CLASS_MYSQL_TIME, SCM_NIL);
    MYSQL_TIME_UNBOX(t) = *time;
    SCM_RETURN(t);
}

#  endif  /* HAVE_MYSQL_TIME */

#define MYSQL_STMT_ERROR SCM_SYMBOL_VALUE("dbd.mysql", "<mysql-stmt-error>")
void raise_mysql_stmt_error(MYSQL_STMT *stmt, const char *msg)
{
    Scm_RaiseCondition(MYSQL_STMT_ERROR,
		       "error-code", SCM_MAKE_INT(mysql_stmt_errno(stmt)),
		       "sql-code", SCM_MAKE_STR_IMMUTABLE(mysql_stmt_sqlstate(stmt)),
		       SCM_RAISE_CONDITION_MESSAGE, "%s: %s", msg, mysql_stmt_error(stmt));
}
#endif	/* HAVE_MYSQL_STMT */

/*
 * Module initialization function.
 */
extern void Scm_Init_dbd_mysqllib(ScmModule*);

static const char *load_default_groups[]
= {"mysql", "client", NULL};

static const char *dummy_argv[]
= { "dbd.mysql" };

void Scm_Init_dbd_mysql(void)
{
    ScmModule *mod;
    int   ac = 1;
    char **av = (char**)dummy_argv;

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
#if HAVE_MYSQL_STMT
    MysqlStmtxClass =
	Scm_MakeForeignPointerClass(mod, "<mysql-stmt>",
				    NULL, mysql_stmtx_cleanup, 0);
#endif	/* HAVE_MYSQL_STMT */

    /* Get handle of the symbol 'closed? */
    sym_closed = SCM_INTERN("closed?");

    /* Register stub-generated procedures */
    Scm_Init_dbd_mysqllib(mod);

    load_defaults("my", load_default_groups, &ac, &av);
}
