;;-*- mode: scheme -*-
;; mysqllib.stub - mysql driver stub
;;
;;  Copyright (c) 2003-2009 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2009 Time Intermedia Corporation, All rights reserved.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;


(select-module dbd.mysql)

(inline-stub

(.include "dbd_mysql.h")

(define-cproc mysql-dbd-version () ::<const-cstring>
  (return PACKAGE_VERSION))

(declare-stub-type <mysql-handle> "MYSQL*" "MySQL handle"
  "MYSQL_HANDLE_P" "MYSQL_HANDLE_UNBOX" "MYSQL_HANDLE_BOX")
(declare-stub-type <mysql-res> "MYSQL_RES*" "MySQL result handle"
  "MYSQL_RES_P" "MYSQL_RES_UNBOX" "MYSQL_RES_BOX")
(declare-stub-type <mysql-row-offset> "MYSQL_ROWS*" "MySQL row offset"
  "MYSQL_ROWS_P" "MYSQL_ROWS_UNBOX" "MYSQL_ROWS_BOX")

(define-cproc mysql-handle? (obj) ::<boolean> MYSQL_HANDLE_P)
(define-cproc mysql-res? (obj) ::<boolean> MYSQL_RES_P)
(define-cproc mysql-row-offset? (obj) ::<boolean> MYSQL_ROWS_P)

;; mysql_affected_rows
(define-cproc mysql-affected-rows (connection::<mysql-handle>)
  MysqlAffectedRows)

;; Capture the common pattern
;; Call API with HANDLE and ARGS; if the result isn't zero, calls
;; raise_mysql_error.
(define-cise-stmt mysql-call
  [(_ api handle . args)
   `(when (!= (,api ,handle ,@args) 0)
      (raise_mysql_error ,handle ,(x->string ',api)))])

;; mysql_autocommit
(.if "HAVE_DECL_MYSQL_AUTOCOMMIT"
  (define-cproc mysql-autocommit (handle::<mysql-handle> flag::<boolean>)
    ::<void> (mysql-call mysql_autocommit handle flag)))

;; mysql_change_user
(define-cproc mysql-change-user (handle::<mysql-handle>
                                 user::<const-cstring>?
                                 password::<const-cstring>?
                                 db::<const-cstring>?)
  ::<void> (mysql-call mysql_change_user handle user password db))

;; mysql_character_set_name
(define-cproc mysql-character-set-name (connection::<mysql-handle>)
  ::<const-cstring> mysql_character_set_name)

;; mysql_close
(define-cproc mysql-close (connection) ::<void>
  (unless (MYSQL_HANDLE_P connection)
    (SCM_TYPE_ERROR connection "<mysql-chandle>"))
  (mysql_cleanup connection))

;; mysql_commit
(.when HAVE_DECL_MYSQL_COMMIT
  (define-cproc mysql-commit (connection::<mysql-handle>) ::<void>
    (mysql-call mysql-commit connection)))

;; mysql_connect(): OMITTED(deprecated)
;; mysql_create_db(): OMITTED(deprecated)

;; mysql_data_seek
(define-cproc mysql-data-seek (result::<mysql-res> offset::<integer>) ::<void>
  (mysql_data_seek result (Scm_GetInteger64 offset)))

;; mysql_debug
(define-cproc mysql-debug (debug::<const-cstring>) ::<void> mysql_debug)

;; mysql_drop_db(): OMITTED(deprecated)

;; mysql_dump_debug_info
(define-cproc mysql-dump-debug-info (handle::<mysql-handle>) ::<void>
  (mysql-call mysql_dump_debug_info handle))

;; mysql_eof(): OMITTED(deprecated)

;; mysql_errno
(define-cproc mysql-errno (connection::<mysql-handle>) ::<int> mysql_errno)

;; mysql_error
(define-cproc mysql-error (connection::<mysql-handle>) ::<const-cstring>
  mysql_error)

;; mysql_escape_string(): OMITTED(You should use mysql_real_escape_string().)

;; MYSQL_FIELD
(.if "HAVE_MYSQL_FIELD"
  (begin
    "
#if HAVE_MYSQL_FIELD_NAME_LENGTH
#  define MYSQL_FIELD_NAME_STRING(obj) \\
     {return Scm_MakeString(MYSQL_FIELD_NAME(obj), MYSQL_FIELD_NAME_LENGTH(obj), -1, SCM_STRING_COPYING);}
#else
#  define MYSQL_FIELD_NAME_STRING(obj) \\
     {return SCM_MAKE_STR_COPYING(MYSQL_FIELD_NAME(obj));}
#endif

#if HAVE_MYSQL_FIELD_ORG_NAME
#  if HAVE_MYSQL_FIELD_ORG_NAME_LENGTH
#    define MYSQL_FIELD_ORG_NAME_STRING(obj) \\
       {return Scm_MakeString(MYSQL_FIELD_ORG_NAME(obj), MYSQL_FIELD_ORG_NAME_LENGTH(obj), -1, SCM_STRING_COPYING);}
#  else
#    define MYSQL_FIELD_ORG_NAME_STRING(obj) \\
       {return SCM_MAKE_STR_COPYING(MYSQL_FIELD_ORG_NAME(obj));}
#  endif
#else
#  define MYSQL_FIELD_ORG_NAME_STRING(obj) \\
       {Scm_Error(\"%S doesn't have slot original-name\", obj); return SCM_FALSE;}
#endif

#if HAVE_MYSQL_FIELD_TABLE_LENGTH
#  define MYSQL_FIELD_TABLE_STRING(obj) \\
     {return Scm_MakeString(MYSQL_FIELD_TABLE(obj), MYSQL_FIELD_TABLE_LENGTH(obj), -1, SCM_STRING_COPYING);}
#else
#  define MYSQL_FIELD_TABLE_STRING(obj) \\
     {return SCM_MAKE_STR_COPYING(MYSQL_FIELD_TABLE(obj));}
#endif

#if HAVE_MYSQL_FIELD_ORG_TABLE_LENGTH
#  define MYSQL_FIELD_ORG_TABLE_STRING(obj) \\
     {return Scm_MakeString(MYSQL_FIELD_ORG_TABLE(obj), MYSQL_FIELD_ORG_TABLE_LENGTH(obj), -1, SCM_STRING_COPYING);}
#else
#  define MYSQL_FIELD_ORG_TABLE_STRING(obj) \\
     {return SCM_MAKE_STR_COPYING(MYSQL_FIELD_ORG_TABLE(obj));}
#endif

#if HAVE_MYSQL_FIELD_DB_LENGTH
#  define MYSQL_FIELD_DB_STRING(obj) \\
     {return Scm_MakeString(MYSQL_FIELD_DB(obj), MYSQL_FIELD_DB_LENGTH(obj), -1, SCM_STRING_COPYING);}
#else
#  define MYSQL_FIELD_DB_STRING(obj) \\
     {return MYSQL_FIELD_DB(obj)==NULL?SCM_FALSE:SCM_MAKE_STR_COPYING(MYSQL_FIELD_DB(obj));}
#endif

#if HAVE_MYSQL_FIELD_CATALOG
#  if HAVE_MYSQL_FIELD_CATALOG_LENGTH
#    define MYSQL_FIELD_CATALOG_STRING(obj) \\
       {return Scm_MakeString(MYSQL_FIELD_CATALOG(obj), MYSQL_FIELD_CATALOG_LENGTH(obj), -1, SCM_STRING_COPYING);}
#  else
#    define MYSQL_FIELD_CATALOG_STRING(obj) \\
       {return SCM_MAKE_STR_COPYING(MYSQL_FIELD_CATALOG(obj));}
#  endif
#else
#  define MYSQL_FIELD_CATALOG_STRING(obj) \\
     {Scm_Error(\"%S doesn't have slot catalog\", obj); return SCM_FALSE;}
#endif

#if HAVE_MYSQL_FIELD_DEF_LENGTH
#  define MYSQL_FIELD_DEF_STRING(obj) \\
     {return MYSQL_FIELD_UNBOX(obj)->def==NULL?SCM_FALSE:Scm_MakeString(MYSQL_FIELD_DEF(obj), MYSQL_FIELD_DEF_LENGTH(obj), -1, SCM_STRING_COPYING);}
#else
#  define MYSQL_FIELD_DEF_STRING(obj) \\
     {return MYSQL_FIELD_UNBOX(obj)->def==NULL?SCM_FALSE:SCM_MAKE_STR_COPYING(MYSQL_FIELD_DEF(obj));}
#endif

#if HAVE_MYSQL_FIELD_CHARSETNR
#  define MYSQL_FIELD_CHARSETNR_INTEGERU(obj) \\
     {return Scm_MakeIntegerU(MYSQL_FIELD_CHARSETNR(obj));}
#else
#  define MYSQL_FIELD_CHARSETNR_INTEGERU(obj) \\
     {Scm_Error(\"%S doesn't have slot charset-number\", obj); return SCM_FALSE;}
#endif
"
    (declare-stub-type <mysql-field> "ScmMysqlField*" "MySQL field information"
      "MYSQL_FIELD_P" "SCM_MYSQL_FIELD")
    (define-cclass <mysql-field>
      "ScmMysqlField*" "Scm_MysqlFieldClass"
      ()
      ((name          :type <const-cstring> :setter #f :getter "MYSQL_FIELD_NAME_STRING(obj)")
       (original-name :type <const-cstring> :setter #f :getter "MYSQL_FIELD_ORG_NAME_STRING(obj)")
       (table         :type <const-cstring> :setter #f :getter "MYSQL_FIELD_TABLE_STRING(obj)")
       (original-table :type <const-cstring> :setter #f :getter "MYSQL_FIELD_ORG_TABLE_STRING(obj)")
       (db            :type <const-cstring> :setter #f :getter "MYSQL_FIELD_DB_STRING(obj)")
       (catalog       :type <const-cstring> :setter #f :getter "MYSQL_FIELD_CATALOG_STRING(obj)")
       (default-value :type <const-cstring>? :setter #f :getter "MYSQL_FIELD_DEF_STRING(obj)")
       (length        :type <ulong> :setter #f :getter "return Scm_MakeIntegerU64(MYSQL_FIELD_LENGTH(obj));")
       (max-length    :type <ulong> :setter #f :getter "return Scm_MakeIntegerU64(MYSQL_FIELD_MAX_LENGTH(obj));")
       (not-null?     :type <boolean> :setter #f :getter "return MYSQL_FIELD_NOT_NULL_P(obj);")
       (primary-key?  :type <boolean> :setter #f :getter "return MYSQL_FIELD_PRIMARY_KEY_P(obj);")
       (unique-key?   :type <boolean> :setter #f :getter "return MYSQL_FIELD_UNIQUE_KEY_P(obj);")
       (multiple-key? :type <boolean> :setter #f :getter "return MYSQL_FIELD_MULTIPLE_KEY_P(obj);")
       (unsigned?     :type <boolean> :setter #f :getter "return MYSQL_FIELD_UNSIGNED_P(obj);")
       (zerofill?     :type <boolean> :setter #f :getter "return MYSQL_FIELD_ZEROFILL_P(obj);")
       (binary?       :type <boolean> :setter #f :getter "return MYSQL_FIELD_BINARY_P(obj);")
       (auto-increment? :type <boolean> :setter #f :getter "return MYSQL_FIELD_AUTO_INCREMENT_P(obj);")
       (decimals      :type <uint> :setter #f :getter "return Scm_MakeIntegerU(MYSQL_FIELD_DECIMALS(obj));")
       (charset-number :type <uint> :setter #f :getter "MYSQL_FIELD_CHARSETNR_INTEGERU(obj)")
       (type          :type <int> :setter #f :getter "return Scm_MakeInteger(MYSQL_FIELD_TYPE(obj));")
       ))
    (define-cproc mysql-field? (obj)
      (call <boolean> "MYSQL_FIELD_P"))))

;; Field Types
(.if HAVE_DECL_MYSQL_TYPE_TINY
  (define-enum MYSQL_TYPE_TINY)
  (define-constant MYSQL_TYPE_TINY (c "SCM_MAKE_INT(FIELD_TYPE_TINY)")))
(.if HAVE_DECL_MYSQL_TYPE_SHORT
  (define-enum MYSQL_TYPE_SHORT)
  (define-constant MYSQL_TYPE_SHORT (c "SCM_MAKE_INT(FIELD_TYPE_SHORT)")))
(.if HAVE_DECL_MYSQL_TYPE_LONG
  (define-enum MYSQL_TYPE_LONG)
  (define-constant MYSQL_TYPE_LONG (c "SCM_MAKE_INT(FIELD_TYPE_LONG)")))
(.if HAVE_DECL_MYSQL_TYPE_INT24
  (define-enum MYSQL_TYPE_INT24)
  (define-constant MYSQL_TYPE_INT24 (c "SCM_MAKE_INT(FIELD_TYPE_INT24)")))
(.if HAVE_DECL_MYSQL_TYPE_LONGLONG
  (define-enum MYSQL_TYPE_LONGLONG)
  (define-constant MYSQL_TYPE_LONGLONG (c "SCM_MAKE_INT(FIELD_TYPE_LONGLONG)")))
(.if HAVE_DECL_MYSQL_TYPE_DECIMAL
  (define-enum MYSQL_TYPE_DECIMAL)
  (define-constant MYSQL_TYPE_DECIMAL (c "SCM_MAKE_INT(FIELD_TYPE_DECIMAL)")))
(define-enum-conditionally MYSQL_TYPE_NEWDECIMAL) ; 5.0.3 or later
(.if HAVE_DECL_MYSQL_TYPE_FLOAT
  (define-enum MYSQL_TYPE_FLOAT)
  (define-constant MYSQL_TYPE_FLOAT (c "SCM_MAKE_INT(FIELD_TYPE_FLOAT)")))
(.if HAVE_DECL_MYSQL_TYPE_DOUBLE
  (define-enum MYSQL_TYPE_DOUBLE)
  (define-constant MYSQL_TYPE_DOUBLE (c "SCM_MAKE_INT(FIELD_TYPE_DOUBLE)")))
(define-enum-conditionally MYSQL_TYPE_BIT)	 ; 5.0.3 or later
(.if HAVE_DECL_MYSQL_TYPE_TIMESTAMP
  (define-enum MYSQL_TYPE_TIMESTAMP)
  (define-constant MYSQL_TYPE_TIMESTAMP (c "SCM_MAKE_INT(FIELD_TYPE_TIMESTAMP)")))
(.if HAVE_DECL_MYSQL_TYPE_DATE
  (define-enum MYSQL_TYPE_DATE)
  (define-constant MYSQL_TYPE_DATE (c "SCM_MAKE_INT(FIELD_TYPE_DATE)")))
(.if HAVE_DECL_MYSQL_TYPE_TIME
  (define-enum MYSQL_TYPE_TIME)
  (define-constant MYSQL_TYPE_TIME (c "SCM_MAKE_INT(FIELD_TYPE_TIME)")))
(.if HAVE_DECL_MYSQL_TYPE_DATETIME
  (define-enum MYSQL_TYPE_DATETIME)
  (define-constant MYSQL_TYPE_DATETIME (c "SCM_MAKE_INT(FIELD_TYPE_DATETIME)")))
(.if HAVE_DECL_MYSQL_TYPE_YEAR
  (define-enum MYSQL_TYPE_YEAR)
  (define-constant MYSQL_TYPE_YEAR (c "SCM_MAKE_INT(FIELD_TYPE_YEAR)")))
(.if HAVE_DECL_MYSQL_TYPE_NEWDATE
  (define-enum MYSQL_TYPE_NEWDATE)
  (define-constant MYSQL_TYPE_NEWDATE (c "SCM_MAKE_INT(FIELD_TYPE_NEWDATE)")))
(.if HAVE_DECL_MYSQL_TYPE_STRING
  (define-enum MYSQL_TYPE_STRING)
  (define-constant MYSQL_TYPE_STRING (c "SCM_MAKE_INT(FIELD_TYPE_STRING)")))
(.if HAVE_DECL_MYSQL_TYPE_VAR_STRING
  (define-enum MYSQL_TYPE_VAR_STRING)
  (define-constant MYSQL_TYPE_VAR_STRING (c "SCM_MAKE_INT(FIELD_TYPE_VAR_STRING)")))
(.if HAVE_DECL_MYSQL_TYPE_BLOB
  (define-enum MYSQL_TYPE_BLOB)
  (define-constant MYSQL_TYPE_BLOB (c "SCM_MAKE_INT(FIELD_TYPE_BLOB)")))
(.if HAVE_DECL_MYSQL_TYPE_TINY_BLOB
  (define-enum MYSQL_TYPE_TINY_BLOB)
  (define-constant MYSQL_TYPE_TINY_BLOB (c "SCM_MAKE_INT(FIELD_TYPE_TINY_BLOB)")))
(.if HAVE_DECL_MYSQL_TYPE_MEDIUM_BLOB
  (define-enum MYSQL_TYPE_MEDIUM_BLOB)
  (define-constant MYSQL_TYPE_MEDIUM_BLOB (c "SCM_MAKE_INT(FIELD_TYPE_MEDIUM_BLOB)")))
(.if HAVE_DECL_MYSQL_TYPE_LONG_BLOB
  (define-enum MYSQL_TYPE_LONG_BLOB)
  (define-constant MYSQL_TYPE_LONG_BLOB (c "SCM_MAKE_INT(FIELD_TYPE_LONG_BLOB)")))
(.if HAVE_DECL_MYSQL_TYPE_SET
  (define-enum MYSQL_TYPE_SET)
  (define-constant MYSQL_TYPE_SET (c "SCM_MAKE_INT(FIELD_TYPE_SET)")))
(.if HAVE_DECL_MYSQL_TYPE_ENUM
  (define-enum MYSQL_TYPE_ENUM)
  (define-constant MYSQL_TYPE_ENUM (c "SCM_MAKE_INT(FIELD_TYPE_ENUM)")))
(.if HAVE_DECL_MYSQL_TYPE_GEOMETRY
  (define-enum MYSQL_TYPE_GEOMETRY)
  (define-constant MYSQL_TYPE_GEOMETRY (c "SCM_MAKE_INT(FIELD_TYPE_GEOMETRY)")))
(.if HAVE_DECL_MYSQL_TYPE_NULL
  (define-enum MYSQL_TYPE_NULL)
  (define-constant MYSQL_TYPE_NULL (c "SCM_MAKE_INT(FIELD_TYPE_NULL)")))

(.when HAVE_MYSQL_FIELD
  ;; mysql_fetch_field
  (define-cproc mysql-fetch-field (result::<mysql-res>)
    (let* ([r::MYSQL_FIELD* (mysql_fetch_field result)])
      (result (?: (== r NULL) '#f (Scm_MakeMysqlField r)))))

  ;; mysql_fetch_field_direct
  (define-cproc mysql-fetch-field-direct (result::<mysql-res> offset::<uint>)
    (let* ([r::MYSQL_FIELD* (mysql_fetch_field_direct result offset)])
      (result (?: (== r NULL) '#f (Scm_MakeMysqlField r)))))

  ;; mysql_fetch_fields()
  (define-cproc mysql-fetch-fields (result::<mysql-res>)
    MysqlFetchFields)

  ;; mysql_fetch_lengths()
  (define-cproc mysql-fetch-lengths (result::<mysql-res>)
    MysqlFetchLengths)
  )

;; mysql_fetch_row
(define-cproc mysql-fetch-row (result::<mysql-res>) MysqlFetchRow)

;; mysql_field_count
(define-cproc mysql-field-count (handle::<mysql-handle>) ::<uint>
  mysql_field_count)

;; mysql_field_seek()
(define-cproc mysql-field-seek (result::<mysql-res> offset::<uint>) ::<uint>
  mysql_field_seek)

;; mysql_field_tell()
(define-cproc mysql-field-tell (result::<mysql-res>) ::<uint>
  mysql_field_tell)

;; mysql_free_result
(define-cproc mysql-free-result (result) ::<void>
  (unless (MYSQL_RES_P result) (SCM_TYPE_ERROR result "<mysql-res>"))
  (mysql_res_cleanup result))

(.when HAVE_MY_CHARSET_INFO
  ;; MY_CHARSET_INFO
  (declare-stub-type <mysql-charset> "ScmMysqlCharsetInfo*" "MySQL character set information"
    "MYSQL_CHARSET_INFO_P" "SCM_MYSQL_CHARSET_INFO")
  (define-cclass <mysql-charset>
    "ScmMysqlCharsetInfo*" "Scm_MysqlCharsetInfoClass"
    ()
    ((number :type <uint> :setter #f :getter "return Scm_MakeIntegerU(MYSQL_CHARSET_INFO_NUMBER(obj));")
     (state  :type <uint> :setter #f :getter "return Scm_MakeIntegerU(MYSQL_CHARSET_INFO_STATE(obj));")
     (csname :type <const-cstring> :setter #f :getter "SCM_RETURN(SCM_MAKE_STR_COPYING(MYSQL_CHARSET_INFO_CSNAME(obj)));")
     (name   :type <const-cstring> :setter #f :getter "SCM_RETURN(SCM_MAKE_STR_COPYING(MYSQL_CHARSET_INFO_NAME(obj)));")
     (comment :type <const-cstring>? :setter #f :getter "SCM_RETURN(MYSQL_CHARSET_INFO_COMMENT(obj)?SCM_MAKE_STR_COPYING(MYSQL_CHARSET_INFO_COMMENT(obj)):SCM_FALSE);")
     (dir    :type <const-cstring>? :setter #f :getter
             "SCM_RETURN(MYSQL_CHARSET_INFO_DIR(obj)==NULL?SCM_FALSE:SCM_MAKE_STR_COPYING(MYSQL_CHARSET_INFO_DIR(obj)));")
     (mbminlen :type <uint> :setter #f :getter "return Scm_MakeIntegerU(MYSQL_CHARSET_INFO_MBMINLEN(obj));")
     (mbmaxlen :type <uint> :setter #f :getter "return Scm_MakeIntegerU(MYSQL_CHARSET_INFO_MBMAXLEN(obj));")))
  (define-cproc mysql-charset? (obj) ::<boolean>
    MYSQL_CHARSET_INFO_P)

  ;; mysql_get_character_set_info(5.0.10 or later)
  (define-cproc mysql-get-character-set-info (handle::<mysql-handle>)
    (let* ([r::ScmMysqlCharsetInfo*
            (SCM_NEW_ATOMIC2 (.type ScmMysqlCharsetInfo*)
                             (sizeof ScmMysqlCharsetInfo))])
      (SCM_SET_CLASS r SCM_CLASS_MYSQL_CHARSET_INFO)
      (mysql_get_character_set_info handle (& (MYSQL_CHARSET_INFO_UNBOX r)))
      (return (SCM_OBJ r))))
  )

;; mysql_get_client_info
(define-cproc mysql-get-client-info () ::<const-cstring>
  mysql_get_client_info)

;; mysql_get_client_version
(define-cproc mysql-get-client-version () ::<ulong>
  mysql_get_client_version)

;; mysql_get_host_info
(define-cproc mysql-get-host-info (handle::<mysql-handle>) ::<const-cstring>
  mysql_get_host_info)

;; mysql_get_proto_info
(define-cproc mysql-get-proto-info (handle::<mysql-handle>) ::<uint>
  mysql_get_proto_info)

;; mysql_get_server_info
(define-cproc mysql-get-server-info (handle::<mysql-handle>) ::<const-cstring>
  mysql_get_server_info)

(.when HAVE_DECL_MYSQL_GET_SERVER_VERSION
  ;; mysql_get_server_version
  (define-cproc mysql-get-server-version (handle::<mysql-handle>)
    ::<ulong> mysql_get_server_version))

;; mysql_get_ssl_cipher()
(define-cproc mysql-get-ssl-cipher (handle::<mysql-handle>) ::<const-cstring>
  mysql_get_ssl_cipher)

;; mysql_hex_string()
(define-cproc mysql-hex-string (src::<string>) MysqlHexString)

;; mysql_info
(define-cproc mysql-info (handle::<mysql-handle>) ::<const-cstring>?
  mysql_info)

;; mysql_init(integrated into mysql-real-connect)

;; mysql_insert_id
(define-cproc mysql-insert-id (handle::<mysql-handle>) ::<integer>
  (result (Scm_MakeIntegerU64 (mysql_insert_id handle))))

;; mysql_kill
;; mysql_library_end
;; mysql_library_init

;; mysql_list_dbs
(define-cproc mysql-list-dbs (handle::<mysql-handle> wild::<const-cstring>?)
  ::<mysql-res>
  (let* ([r::MYSQL_RES* (mysql_list_dbs handle wild)])
    (when (== r NULL)
      (raise_mysql_error handle "mysql_list_dbs"))
    (return r)))

;; mysql_list_fields
(define-cproc mysql-list-fields (handle::<mysql-handle>
                                 table::<const-cstring>
                                 wild::<const-cstring>?)
  ::<mysql-res>
  (let* ([r::MYSQL_RES* (mysql_list_fields handle table wild)])
    (when (== r NULL)
      (raise_mysql_error handle "mysql_list_fields"))
    (return r)))

;; mysql_list_processes
(define-cproc mysql-list-processes (handle::<mysql-handle>)
  ::<mysql-res>
  (let* ([r::MYSQL_RES* (mysql_list_processes handle)])
    (when (== r NULL)
      (raise_mysql_error handle "mysql_list_processes"))
    (return r)))

;; mysql_list_tables
(define-cproc mysql-list-tables (handle::<mysql-handle> wild::<const-cstring>?)
  ::<mysql-res>
  (let* ([r::MYSQL_RES* (mysql_list_tables handle wild)])
    (when (== r NULL)
      (raise_mysql_error handle "mysql_list_tables"))
    (return r)))

;; mysql_more_results
;; mysql_next_result

;; mysql_num_fields
(define-cproc mysql-num-fields (result::<mysql-res>) ::<uint> mysql_num_fields)

;; mysql_num_rows
(define-cproc mysql-num-rows (res::<mysql-res>) ::<uint64> mysql_num_rows)

;; mysql_options

;; mysql_ping
(define-cproc mysql-ping (handle::<mysql-handle>) ::<void>
  (mysql-call mysql_ping handle))

;; mysql_query(): OMITTED(You should use mysql_real_query().)

;; mysql_real_connect
(define-cproc mysql-real-connect (host::<const-cstring>?
                                  user::<const-cstring>?
                                  password::<const-cstring>?
                                  db::<const-cstring>?
                                  port::<uint>
                                  unix_socket::<const-cstring>?
                                  client_flag::<uint>)
  ::<mysql-handle> MysqlRealConnect)

;; mysql_real_escape_string
(define-cproc mysql-real-escape-string (connection::<mysql-handle>
                                        string::<string>)
  (let* ([body::(const ScmStringBody *) (SCM_STRING_BODY string)]
         [s::(const char*) (SCM_STRING_BODY_START body)]
         [origsize::u_int (SCM_STRING_BODY_SIZE body)]
         [bufsize::u_int (+ (* origsize 2) 2)]
         [buf::(char*) (SCM_NEW_ATOMIC2 (char*) bufsize)]
         [finalsize::u_long (mysql_real_escape_string connection buf s origsize)])
    ;; buf is new NUL terminated
    (return (Scm_MakeString buf finalsize -1 SCM_MAKSTR_COPYING))))

;; mysql_real_query
(define-cproc mysql-real-query (connection::<mysql-handle> query::<string>)
  ::<void>
  (let* ([body::(const ScmStringBody*) (SCM_STRING_BODY query)]
         [q::(const char *) (SCM_STRING_BODY_START body)]
         [qlen::u_int (SCM_STRING_BODY_SIZE body)])
    (unless (== (mysql_real_query connection q qlen) 0)
      (raise_mysql_error connection "mysql_real_query"))))

;; mysql_refresh
(define-cproc mysql-refresh (handle::<mysql-handle> :rest options) ::<void>
  (let* ([accum::u_int 0])
    (for-each (lambda (flag)
                (logior= accum (Scm_GetIntegerU flag)))
              options)
    (unless (== (mysql_refresh handle accum) 0)
      (raise_mysql_error handle "mysql_refresh"))))

(define-enum-conditionally REFRESH_GRANT)
(define-enum-conditionally REFRESH_LOG)
(define-enum-conditionally REFRESH_TABLES)
(define-enum-conditionally REFRESH_HOSTS)
(define-enum-conditionally REFRESH_STATUS)
(define-enum-conditionally REFRESH_THREADS)
(define-enum-conditionally REFRESH_SLAVE)
(define-enum-conditionally REFRESH_MASTER)

;; mysql_reload(): OMITTED(deprecated)

;; mysql_rollback
(.when HAVE_DECL_MYSQL_ROLLBACK
  (define-cproc mysql-rollback (handle::<mysql-handle>) ::<void>
    (mysql-call mysql_rollback handle)))

;; mysql_row_seek()
(define-cproc mysql-row-seek (result::<mysql-res> offset::<mysql-row-offset>)
  ::<mysql-row-offset> mysql_row_seek)

;; mysql_row_tell()
(define-cproc mysql-row-tell (result::<mysql-res>)
  ::<mysql-row-offset> mysql_row_tell)

;; mysql_select_db
(define-cproc mysql-select-db (handle::<mysql-handle> db::<const-cstring>)
  ::<void> (mysql-call mysql_select_db handle db))

;; mysql_set_character_set
(.when HAVE_DECL_MYSQL_SET_CHARACTER_SET
  (define-cproc mysql-set-character-set (connection::<mysql-handle>
                                         charset::<const-cstring>)
    ::<void> (mysql-call mysql_set_character_set connection charset)))

;; mysql_set_local_infile_default
;; mysql_set_local_infile_handler
;; mysql_set_server_option

;; mysql_shutdown
(.if HAVE_DECL_SHUTDOWN_DEFAULT
     (define-cproc mysql-shutdown (handle::<mysql-handle>
                                   :optional (level::<int>
                                              (SCM_MAKE_INT (c "SHUTDOWN_DEFAULT"))))
       ::<void> (mysql-call mysql_shutdown handle level))
     (define-cproc mysql-shutdown (handle::<mysql-handle> :optional _)
       ::<void> (mysql-call mysql_shutdown handle)))

;; mysql_sqlstate
(.when HAVE_DECL_MYSQL_SQLSTATE
  (define-cproc mysql-sqlstate (handle::<mysql-handle>) ::<const-cstring>
    mysql_sqlstate))

;; mysql_ssl_set()
(define-cproc mysql-ssl-set (handle::<mysql-handle>
                             key::<const-cstring>?
                             cert::<const-cstring>?
                             ca::<const-cstring>?
                             capath::<const-cstring>?
                             cipher::<const-cstring>?)
  ::<boolean> mysql_ssl_set)

;; mysql-stat
(define-cproc mysql-stat (handle::<mysql-handle>) ::<const-cstring>
  (let* ((r::(const char*) (mysql_stat handle)))
    (when (== r NULL)
      (raise_mysql_error handle "mysql_stat"))
    (return r)))

;; mysql_store_result
(define-cproc mysql-store-result (connection::<mysql-handle>) ::<mysql-res>?
  (let* ([r::MYSQL_RES* (mysql_store_result connection)])
    (when (and (== r NULL)
               (!= (mysql_errno connection) 0))
      (raise_mysql_error connection "mysql_store_result"))
    (return r)))

;; mysql_thread_id
(define-cproc mysql-thread-id (handle::<mysql-handle>) ::<ulong>
  mysql_thread_id)

;; mysql_use_result()
(define-cproc mysql-use-result (connection::<mysql-handle>) ::<mysql-res>?
  (let* ([r::MYSQL_RES* (mysql_use_result connection)])
    (when (and (== r NULL)
               (!= (mysql_errno connection) 0))
      (raise_mysql_error connection "mysql_use_result"))
    (return r)))

;; mysql_warning_count
(.when HAVE_DECL_MYSQL_WARNING_COUNT
  (define-cproc mysql-warning-count (handle::<mysql-handle>) ::<uint>
    mysql_warning_count))

(define-cproc mysql-handle-closed? (connection) ::<boolean>
  (unless (MYSQL_HANDLE_P connection)
    (SCM_TYPE_ERROR connection "<mysql-chandle>"))
  (result (MysqlClosedP connection)))

(define-cproc mysql-res-closed? (result) ::<boolean>
  (unless (MYSQL_RES_P result)
    (SCM_TYPE_ERROR result "<mysql-res>"))
  (result (MysqlClosedP result)))

(define-cproc mysql-fetch-field-names (result::<mysql-res>?)
  MysqlFetchFieldNames)

;;
;; Native Prepared Statement API
;;

(.when HAVE_MYSQL_STMT
  (declare-stub-type <mysql-stmt> "MYSQL_STMTX*"
                     "MySQL enhanced statement handle"
                     "MYSQL_STMTX_P" "MYSQL_STMTX_UNBOX" "MYSQL_STMTX_BOX")
  (define-cproc mysql-stmt? (obj) ::<boolean> MYSQL_STMTX_P)

  ;; mysql_stmt_affected_rows
  (define-cproc mysql-stmt-affected-rows (stmtx::<mysql-stmt>)
    (return (MysqlStmtAffectedRows (-> stmtx stmt))))

  ;; mysql_stmt_attr_get
  ;; mysql_stmt_attr_set
  ;; mysql_stmt_bind_param(integrated into mysql-stmt-prepare)
  ;; mysql_stmt_bind_result(integrated into mysql-stmt-execute)

  ;; mysql_stmt_close
  (define-cproc mysql-stmt-close (stmtx) ::<void>
    (unless (MYSQL_STMTX_P stmtx) (SCM_TYPE_ERROR stmtx "<mysql-stmt>"))
    (mysql_stmtx_cleanup stmtx))

  ;; mysql_stmt_data_seek
  (define-cproc mysql-stmt-data-seek (stmtx::<mysql-stmt> offset::<integer>) ::<void>
    (mysql_stmt_data_seek (-> stmtx stmt) (Scm_GetIntegerU64 offset)))

  ;; mysql_stmt_errno
  (define-cproc mysql-stmt-errno (stmtx::<mysql-stmt>) ::<uint>
    (result (mysql_stmt_errno (-> stmtx stmt))))

  ;; mysql_stmt_error
  (define-cproc mysql-stmt-error (stmtx::<mysql-stmt>) ::<const-cstring>
    (result (mysql_stmt_error (-> stmtx stmt))))

  ;; mysql_stmt_execute
  (define-cproc mysql-stmt-execute (stmtx::<mysql-stmt> :rest args) ::<void>
    (MysqlStmtxExecute stmtx args)
    (MysqlResultUnmarkClosed stmtx_scm))

  ;; mysql_stmt_fetch
  (define-cproc mysql-stmt-fetch (stmtx::<mysql-stmt>) MysqlStmtxFetch)

  ;; mysql_stmt_fetch_column

  ;; mysql_stmt_field_count
  (define-cproc mysql-stmt-field-count (stmtx::<mysql-stmt>) ::<uint>
    (return (mysql_stmt_field_count (-> stmtx stmt))))

  ;; mysql_stmt_free_result
  (define-cproc mysql-stmt-free-result (stmtx::<mysql-stmt>) ::<void>
    (when (mysql_stmt_free_result (-> stmtx stmt))
      (raise_mysql_stmt_error (-> stmtx stmt) "mysql_stmt_free_result"))
    (MysqlResultMarkClosed stmtx_scm))

  ;; mysql_stmt_init(integrate into mysql-stmt-prepare)

  ;; mysql_stmt_insert_id
  (define-cproc mysql-stmt-insert-id (stmtx::<mysql-stmt>) ::<uint64>
    (return (mysql_stmt_insert_id (-> stmtx stmt))))

  ;; mysql_stmt_num_rows
  (define-cproc mysql-stmt-num-rows (stmtx::<mysql-stmt>)::<uint64>
    (return (mysql_stmt_num_rows (-> stmtx stmt))))

  ;; mysql_stmt_param_count
  (define-cproc mysql-stmt-param-count (stmtx::<mysql-stmt>) ::<uint>
    (return (mysql_stmt_param_count (-> stmtx stmt))))

  ;; mysql_stmt_param_metadata

  ;; mysql_stmt_prepare
  (define-cproc mysql-stmt-prepare (connection::<mysql-handle> sql::<string>)
    ::<mysql-stmt> MysqlStmtxPrepare)

  ;; mysql_stmt_reset
  (define-cproc mysql-stmt-reset (stmtx::<mysql-stmt>) ::<void>
    (when (!= (mysql_stmt_reset (-> stmtx stmt)) 0)
      (raise_mysql_stmt_error (-> stmtx stmt) "mysql_stmt_reset"))
    (MysqlResultMarkClosed stmtx_scm))

  ;; mysql_stmt_result_metadata
  ;; mysql_stmt_row_seek
  ;; mysql_stmt_row_tell
  ;; mysql_stmt_send_long_data

  (define-cproc mysql-stmt-sqlstate (stmtx::<mysql-stmt>) ::<const-cstring>
    (return (mysql_stmt_sqlstate (-> stmtx stmt))))

  ;; mysql_stmt_store_result(integrated into mysql-stmt-execute)

  (define-cproc mysql-stmt-closed? (stmtx) ::<boolean>
    (unless (MYSQL_STMTX_P stmtx) (SCM_TYPE_ERROR stmtx "<mysql-stmt>"))
    (result (MysqlClosedP stmtx)))

  (define-cproc mysql-stmt-res-closed? (stmtx) ::<boolean>
    (unless (MYSQL_STMTX_P stmtx) (SCM_TYPE_ERROR stmtx "<mysql-stmt>"))
    (result (MysqlResultClosedP stmtx)))

  (define-cproc mysql-stmt-fetch-field-names (stmtx::<mysql-stmt>)
    MysqlStmtxFetchFieldNames)
  )

(.when HAVE_MYSQL_TIME
  (declare-stub-type <mysql-time> "ScmMysqlTime*" "MySQL DATE/TIME/TIMESTAMP structure"
    "MYSQL_TIME_P" "SCM_MYSQL_TIME")
  (define-enum MYSQL_TIMESTAMP_NONE)
  (define-constant MYSQL_TIMESTAMP_NONE (c "SCM_MAKE_INT(MYSQL_TIMESTAMP_NONE)"))
  (define-enum MYSQL_TIMESTAMP_ERROR)
  (define-constant MYSQL_TIMESTAMP_ERROR (c "SCM_MAKE_INT(MYSQL_TIMESTAMP_ERROR)"))
  (define-enum MYSQL_TIMESTAMP_DATE)
  (define-constant MYSQL_TIMESTAMP_DATE (c "SCM_MAKE_INT(MYSQL_TIMESTAMP_DATE)"))
  (define-enum MYSQL_TIMESTAMP_DATETIME)
  (define-constant MYSQL_TIMESTAMP_DATETIME (c "SCM_MAKE_INT(MYSQL_TIMESTAMP_DATETIME)"))
  (define-enum MYSQL_TIMESTAMP_TIME)
  (define-constant MYSQL_TIMESTAMP_TIME (c "SCM_MAKE_INT(MYSQL_TIMESTAMP_TIME)"))
  (define-cclass <mysql-time>
    "ScmMysqlTime*" "Scm_MysqlTimeClass"
    ()
    ((year :type <uint> :c-spec "MYSQL_TIME_YEAR(obj)")
     (month :type <uint> :c-spec "MYSQL_TIME_MONTH(obj)")
     (day :type <uint> :c-spec "MYSQL_TIME_DAY(obj)")
     (hour :type <uint> :c-spec "MYSQL_TIME_HOUR(obj)")
     (minute :type <uint> :c-spec "MYSQL_TIME_MINUTE(obj)")
     (second :type <uint> :c-spec "MYSQL_TIME_SECOND(obj)")
     (second-part :type <uint> :c-spec "MYSQL_TIME_SECOND_PART(obj)") ; microsecond/unused
     (time-type :type <uint> :c-spec "MYSQL_TIME_TIME_TYPE(obj)")
     (nagative? :type <boolean> :c-spec "MYSQL_TIME_NEGATIVE_P(obj)"))
    (allocator (c "mysql_time_allocate")))
  (define-cproc mysql-time? (obj)
    (call <boolean> "MYSQL_TIME_P")))

"
#if HAVE_DECL_MY_CS_AVAILABLE
# define GET_CHARSET_NUMBER(csname)  get_charset_number((csname), MY_CS_AVAILABLE)
#else
# define GET_CHARSET_NUMBER(csname)  get_charset_number(csname)
#endif
"

;; Separate static function to avoid "set but unused" warning.
"
static int get_charset_number_int(const char *csname)
{
#if HAVE_MYSQL_GET_CHARSET_NUMBER
   return GET_CHARSET_NUMBER(csname);
#else
   return 0; /* Zero means no charset found */
#endif
}
"

(define-cproc mysql-get-charset-number (csname::<const-cstring>) ::<int>
  get_charset_number_int)

;;
;; from errmsg.h mysql-5.0.27
;;
(define-enum-conditionally CR_MIN_ERROR)
(define-enum-conditionally CR_MAX_ERROR)
(define-enum-conditionally CLIENT_ERRMAP)
(define-enum-conditionally CR_ERROR_FIRST)
(define-enum-conditionally CR_UNKNOWN_ERROR)
(define-enum-conditionally CR_SOCKET_CREATE_ERROR)
(define-enum-conditionally CR_CONNECTION_ERROR)
(define-enum-conditionally CR_CONN_HOST_ERROR)
(define-enum-conditionally CR_IPSOCK_ERROR)
(define-enum-conditionally CR_UNKNOWN_HOST)
(define-enum-conditionally CR_SERVER_GONE_ERROR)
(define-enum-conditionally CR_VERSION_ERROR)
(define-enum-conditionally CR_OUT_OF_MEMORY)
(define-enum-conditionally CR_WRONG_HOST_INFO)
(define-enum-conditionally CR_LOCALHOST_CONNECTION)
(define-enum-conditionally CR_TCP_CONNECTION)
(define-enum-conditionally CR_SERVER_HANDSHAKE_ERR)
(define-enum-conditionally CR_SERVER_LOST)
(define-enum-conditionally CR_COMMANDS_OUT_OF_SYNC)
(define-enum-conditionally CR_NAMEDPIPE_CONNECTION)
(define-enum-conditionally CR_NAMEDPIPEWAIT_ERROR)
(define-enum-conditionally CR_NAMEDPIPEOPEN_ERROR)
(define-enum-conditionally CR_NAMEDPIPESETSTATE_ERROR)
(define-enum-conditionally CR_CANT_READ_CHARSET)
(define-enum-conditionally CR_NET_PACKET_TOO_LARGE)
(define-enum-conditionally CR_EMBEDDED_CONNECTION)
(define-enum-conditionally CR_PROBE_SLAVE_STATUS)
(define-enum-conditionally CR_PROBE_SLAVE_HOSTS)
(define-enum-conditionally CR_PROBE_SLAVE_CONNECT)
(define-enum-conditionally CR_PROBE_MASTER_CONNECT)
(define-enum-conditionally CR_SSL_CONNECTION_ERROR)
(define-enum-conditionally CR_MALFORMED_PACKET)
(define-enum-conditionally CR_WRONG_LICENSE)
(define-enum-conditionally CR_NULL_POINTER)
(define-enum-conditionally CR_NO_PREPARE_STMT)
(define-enum-conditionally CR_PARAMS_NOT_BOUND)
(define-enum-conditionally CR_DATA_TRUNCATED)
(define-enum-conditionally CR_NO_PARAMETERS_EXISTS)
(define-enum-conditionally CR_INVALID_PARAMETER_NO)
(define-enum-conditionally CR_INVALID_BUFFER_USE)
(define-enum-conditionally CR_UNSUPPORTED_PARAM_TYPE)
(define-enum-conditionally CR_SHARED_MEMORY_CONNECTION)
(define-enum-conditionally CR_SHARED_MEMORY_CONNECT_REQUEST_ERROR)
(define-enum-conditionally CR_SHARED_MEMORY_CONNECT_ANSWER_ERROR)
(define-enum-conditionally CR_SHARED_MEMORY_CONNECT_FILE_MAP_ERROR)
(define-enum-conditionally CR_SHARED_MEMORY_CONNECT_MAP_ERROR)
(define-enum-conditionally CR_SHARED_MEMORY_FILE_MAP_ERROR)
(define-enum-conditionally CR_SHARED_MEMORY_MAP_ERROR)
(define-enum-conditionally CR_SHARED_MEMORY_EVENT_ERROR)
(define-enum-conditionally CR_SHARED_MEMORY_CONNECT_ABANDONED_ERROR)
(define-enum-conditionally CR_SHARED_MEMORY_CONNECT_SET_ERROR)
(define-enum-conditionally CR_CONN_UNKNOW_PROTOCOL)
(define-enum-conditionally CR_INVALID_CONN_HANDLE)
(define-enum-conditionally CR_SECURE_AUTH)
(define-enum-conditionally CR_FETCH_CANCELED)
(define-enum-conditionally CR_NO_DATA)
(define-enum-conditionally CR_NO_STMT_METADATA)
(define-enum-conditionally CR_NO_RESULT_SET)
(define-enum-conditionally CR_NOT_IMPLEMENTED)
(define-enum-conditionally CR_ERROR_LAST)

;;
;; from mysqld_error.h mysql-5.0.27
;;
(define-enum-conditionally ER_ERROR_FIRST)
(define-enum-conditionally ER_HASHCHK)
(define-enum-conditionally ER_NISAMCHK)
(define-enum-conditionally ER_NO)
(define-enum-conditionally ER_YES)
(define-enum-conditionally ER_CANT_CREATE_FILE)
(define-enum-conditionally ER_CANT_CREATE_TABLE)
(define-enum-conditionally ER_CANT_CREATE_DB)
(define-enum-conditionally ER_DB_CREATE_EXISTS)
(define-enum-conditionally ER_DB_DROP_EXISTS)
(define-enum-conditionally ER_DB_DROP_DELETE)
(define-enum-conditionally ER_DB_DROP_RMDIR)
(define-enum-conditionally ER_CANT_DELETE_FILE)
(define-enum-conditionally ER_CANT_FIND_SYSTEM_REC)
(define-enum-conditionally ER_CANT_GET_STAT)
(define-enum-conditionally ER_CANT_GET_WD)
(define-enum-conditionally ER_CANT_LOCK)
(define-enum-conditionally ER_CANT_OPEN_FILE)
(define-enum-conditionally ER_FILE_NOT_FOUND)
(define-enum-conditionally ER_CANT_READ_DIR)
(define-enum-conditionally ER_CANT_SET_WD)
(define-enum-conditionally ER_CHECKREAD)
(define-enum-conditionally ER_DISK_FULL)
(define-enum-conditionally ER_DUP_KEY)
(define-enum-conditionally ER_ERROR_ON_CLOSE)
(define-enum-conditionally ER_ERROR_ON_READ)
(define-enum-conditionally ER_ERROR_ON_RENAME)
(define-enum-conditionally ER_ERROR_ON_WRITE)
(define-enum-conditionally ER_FILE_USED)
(define-enum-conditionally ER_FILSORT_ABORT)
(define-enum-conditionally ER_FORM_NOT_FOUND)
(define-enum-conditionally ER_GET_ERRNO)
(define-enum-conditionally ER_ILLEGAL_HA)
(define-enum-conditionally ER_KEY_NOT_FOUND)
(define-enum-conditionally ER_NOT_FORM_FILE)
(define-enum-conditionally ER_NOT_KEYFILE)
(define-enum-conditionally ER_OLD_KEYFILE)
(define-enum-conditionally ER_OPEN_AS_READONLY)
(define-enum-conditionally ER_OUTOFMEMORY)
(define-enum-conditionally ER_OUT_OF_SORTMEMORY)
(define-enum-conditionally ER_UNEXPECTED_EOF)
(define-enum-conditionally ER_CON_COUNT_ERROR)
(define-enum-conditionally ER_OUT_OF_RESOURCES)
(define-enum-conditionally ER_BAD_HOST_ERROR)
(define-enum-conditionally ER_HANDSHAKE_ERROR)
(define-enum-conditionally ER_DBACCESS_DENIED_ERROR)
(define-enum-conditionally ER_ACCESS_DENIED_ERROR)
(define-enum-conditionally ER_NO_DB_ERROR)
(define-enum-conditionally ER_UNKNOWN_COM_ERROR)
(define-enum-conditionally ER_BAD_NULL_ERROR)
(define-enum-conditionally ER_BAD_DB_ERROR)
(define-enum-conditionally ER_TABLE_EXISTS_ERROR)
(define-enum-conditionally ER_BAD_TABLE_ERROR)
(define-enum-conditionally ER_NON_UNIQ_ERROR)
(define-enum-conditionally ER_SERVER_SHUTDOWN)
(define-enum-conditionally ER_BAD_FIELD_ERROR)
(define-enum-conditionally ER_WRONG_FIELD_WITH_GROUP)
(define-enum-conditionally ER_WRONG_GROUP_FIELD)
(define-enum-conditionally ER_WRONG_SUM_SELECT)
(define-enum-conditionally ER_WRONG_VALUE_COUNT)
(define-enum-conditionally ER_TOO_LONG_IDENT)
(define-enum-conditionally ER_DUP_FIELDNAME)
(define-enum-conditionally ER_DUP_KEYNAME)
(define-enum-conditionally ER_DUP_ENTRY)
(define-enum-conditionally ER_WRONG_FIELD_SPEC)
(define-enum-conditionally ER_PARSE_ERROR)
(define-enum-conditionally ER_EMPTY_QUERY)
(define-enum-conditionally ER_NONUNIQ_TABLE)
(define-enum-conditionally ER_INVALID_DEFAULT)
(define-enum-conditionally ER_MULTIPLE_PRI_KEY)
(define-enum-conditionally ER_TOO_MANY_KEYS)
(define-enum-conditionally ER_TOO_MANY_KEY_PARTS)
(define-enum-conditionally ER_TOO_LONG_KEY)
(define-enum-conditionally ER_KEY_COLUMN_DOES_NOT_EXITS)
(define-enum-conditionally ER_BLOB_USED_AS_KEY)
(define-enum-conditionally ER_TOO_BIG_FIELDLENGTH)
(define-enum-conditionally ER_WRONG_AUTO_KEY)
(define-enum-conditionally ER_READY)
(define-enum-conditionally ER_NORMAL_SHUTDOWN)
(define-enum-conditionally ER_GOT_SIGNAL)
(define-enum-conditionally ER_SHUTDOWN_COMPLETE)
(define-enum-conditionally ER_FORCING_CLOSE)
(define-enum-conditionally ER_IPSOCK_ERROR)
(define-enum-conditionally ER_NO_SUCH_INDEX)
(define-enum-conditionally ER_WRONG_FIELD_TERMINATORS)
(define-enum-conditionally ER_BLOBS_AND_NO_TERMINATED)
(define-enum-conditionally ER_TEXTFILE_NOT_READABLE)
(define-enum-conditionally ER_FILE_EXISTS_ERROR)
(define-enum-conditionally ER_LOAD_INFO)
(define-enum-conditionally ER_ALTER_INFO)
(define-enum-conditionally ER_WRONG_SUB_KEY)
(define-enum-conditionally ER_CANT_REMOVE_ALL_FIELDS)
(define-enum-conditionally ER_CANT_DROP_FIELD_OR_KEY)
(define-enum-conditionally ER_INSERT_INFO)
(define-enum-conditionally ER_UPDATE_TABLE_USED)
(define-enum-conditionally ER_NO_SUCH_THREAD)
(define-enum-conditionally ER_KILL_DENIED_ERROR)
(define-enum-conditionally ER_NO_TABLES_USED)
(define-enum-conditionally ER_TOO_BIG_SET)
(define-enum-conditionally ER_NO_UNIQUE_LOGFILE)
(define-enum-conditionally ER_TABLE_NOT_LOCKED_FOR_WRITE)
(define-enum-conditionally ER_TABLE_NOT_LOCKED)
(define-enum-conditionally ER_BLOB_CANT_HAVE_DEFAULT)
(define-enum-conditionally ER_WRONG_DB_NAME)
(define-enum-conditionally ER_WRONG_TABLE_NAME)
(define-enum-conditionally ER_TOO_BIG_SELECT)
(define-enum-conditionally ER_UNKNOWN_ERROR)
(define-enum-conditionally ER_UNKNOWN_PROCEDURE)
(define-enum-conditionally ER_WRONG_PARAMCOUNT_TO_PROCEDURE)
(define-enum-conditionally ER_WRONG_PARAMETERS_TO_PROCEDURE)
(define-enum-conditionally ER_UNKNOWN_TABLE)
(define-enum-conditionally ER_FIELD_SPECIFIED_TWICE)
(define-enum-conditionally ER_INVALID_GROUP_FUNC_USE)
(define-enum-conditionally ER_UNSUPPORTED_EXTENSION)
(define-enum-conditionally ER_TABLE_MUST_HAVE_COLUMNS)
(define-enum-conditionally ER_RECORD_FILE_FULL)
(define-enum-conditionally ER_UNKNOWN_CHARACTER_SET)
(define-enum-conditionally ER_TOO_MANY_TABLES)
(define-enum-conditionally ER_TOO_MANY_FIELDS)
(define-enum-conditionally ER_TOO_BIG_ROWSIZE)
(define-enum-conditionally ER_STACK_OVERRUN)
(define-enum-conditionally ER_WRONG_OUTER_JOIN)
(define-enum-conditionally ER_NULL_COLUMN_IN_INDEX)
(define-enum-conditionally ER_CANT_FIND_UDF)
(define-enum-conditionally ER_CANT_INITIALIZE_UDF)
(define-enum-conditionally ER_UDF_NO_PATHS)
(define-enum-conditionally ER_UDF_EXISTS)
(define-enum-conditionally ER_CANT_OPEN_LIBRARY)
(define-enum-conditionally ER_CANT_FIND_DL_ENTRY)
(define-enum-conditionally ER_FUNCTION_NOT_DEFINED)
(define-enum-conditionally ER_HOST_IS_BLOCKED)
(define-enum-conditionally ER_HOST_NOT_PRIVILEGED)
(define-enum-conditionally ER_PASSWORD_ANONYMOUS_USER)
(define-enum-conditionally ER_PASSWORD_NOT_ALLOWED)
(define-enum-conditionally ER_PASSWORD_NO_MATCH)
(define-enum-conditionally ER_UPDATE_INFO)
(define-enum-conditionally ER_CANT_CREATE_THREAD)
(define-enum-conditionally ER_WRONG_VALUE_COUNT_ON_ROW)
(define-enum-conditionally ER_CANT_REOPEN_TABLE)
(define-enum-conditionally ER_INVALID_USE_OF_NULL)
(define-enum-conditionally ER_REGEXP_ERROR)
(define-enum-conditionally ER_MIX_OF_GROUP_FUNC_AND_FIELDS)
(define-enum-conditionally ER_NONEXISTING_GRANT)
(define-enum-conditionally ER_TABLEACCESS_DENIED_ERROR)
(define-enum-conditionally ER_COLUMNACCESS_DENIED_ERROR)
(define-enum-conditionally ER_ILLEGAL_GRANT_FOR_TABLE)
(define-enum-conditionally ER_GRANT_WRONG_HOST_OR_USER)
(define-enum-conditionally ER_NO_SUCH_TABLE)
(define-enum-conditionally ER_NONEXISTING_TABLE_GRANT)
(define-enum-conditionally ER_NOT_ALLOWED_COMMAND)
(define-enum-conditionally ER_SYNTAX_ERROR)
(define-enum-conditionally ER_DELAYED_CANT_CHANGE_LOCK)
(define-enum-conditionally ER_TOO_MANY_DELAYED_THREADS)
(define-enum-conditionally ER_ABORTING_CONNECTION)
(define-enum-conditionally ER_NET_PACKET_TOO_LARGE)
(define-enum-conditionally ER_NET_READ_ERROR_FROM_PIPE)
(define-enum-conditionally ER_NET_FCNTL_ERROR)
(define-enum-conditionally ER_NET_PACKETS_OUT_OF_ORDER)
(define-enum-conditionally ER_NET_UNCOMPRESS_ERROR)
(define-enum-conditionally ER_NET_READ_ERROR)
(define-enum-conditionally ER_NET_READ_INTERRUPTED)
(define-enum-conditionally ER_NET_ERROR_ON_WRITE)
(define-enum-conditionally ER_NET_WRITE_INTERRUPTED)
(define-enum-conditionally ER_TOO_LONG_STRING)
(define-enum-conditionally ER_TABLE_CANT_HANDLE_BLOB)
(define-enum-conditionally ER_TABLE_CANT_HANDLE_AUTO_INCREMENT)
(define-enum-conditionally ER_DELAYED_INSERT_TABLE_LOCKED)
(define-enum-conditionally ER_WRONG_COLUMN_NAME)
(define-enum-conditionally ER_WRONG_KEY_COLUMN)
(define-enum-conditionally ER_WRONG_MRG_TABLE)
(define-enum-conditionally ER_DUP_UNIQUE)
(define-enum-conditionally ER_BLOB_KEY_WITHOUT_LENGTH)
(define-enum-conditionally ER_PRIMARY_CANT_HAVE_NULL)
(define-enum-conditionally ER_TOO_MANY_ROWS)
(define-enum-conditionally ER_REQUIRES_PRIMARY_KEY)
(define-enum-conditionally ER_NO_RAID_COMPILED)
(define-enum-conditionally ER_UPDATE_WITHOUT_KEY_IN_SAFE_MODE)
(define-enum-conditionally ER_KEY_DOES_NOT_EXITS)
(define-enum-conditionally ER_CHECK_NO_SUCH_TABLE)
(define-enum-conditionally ER_CHECK_NOT_IMPLEMENTED)
(define-enum-conditionally ER_CANT_DO_THIS_DURING_AN_TRANSACTION)
(define-enum-conditionally ER_ERROR_DURING_COMMIT)
(define-enum-conditionally ER_ERROR_DURING_ROLLBACK)
(define-enum-conditionally ER_ERROR_DURING_FLUSH_LOGS)
(define-enum-conditionally ER_ERROR_DURING_CHECKPOINT)
(define-enum-conditionally ER_NEW_ABORTING_CONNECTION)
(define-enum-conditionally ER_DUMP_NOT_IMPLEMENTED)
(define-enum-conditionally ER_FLUSH_MASTER_BINLOG_CLOSED)
(define-enum-conditionally ER_INDEX_REBUILD)
(define-enum-conditionally ER_MASTER)
(define-enum-conditionally ER_MASTER_NET_READ)
(define-enum-conditionally ER_MASTER_NET_WRITE)
(define-enum-conditionally ER_FT_MATCHING_KEY_NOT_FOUND)
(define-enum-conditionally ER_LOCK_OR_ACTIVE_TRANSACTION)
(define-enum-conditionally ER_UNKNOWN_SYSTEM_VARIABLE)
(define-enum-conditionally ER_CRASHED_ON_USAGE)
(define-enum-conditionally ER_CRASHED_ON_REPAIR)
(define-enum-conditionally ER_WARNING_NOT_COMPLETE_ROLLBACK)
(define-enum-conditionally ER_TRANS_CACHE_FULL)
(define-enum-conditionally ER_SLAVE_MUST_STOP)
(define-enum-conditionally ER_SLAVE_NOT_RUNNING)
(define-enum-conditionally ER_BAD_SLAVE)
(define-enum-conditionally ER_MASTER_INFO)
(define-enum-conditionally ER_SLAVE_THREAD)
(define-enum-conditionally ER_TOO_MANY_USER_CONNECTIONS)
(define-enum-conditionally ER_SET_CONSTANTS_ONLY)
(define-enum-conditionally ER_LOCK_WAIT_TIMEOUT)
(define-enum-conditionally ER_LOCK_TABLE_FULL)
(define-enum-conditionally ER_READ_ONLY_TRANSACTION)
(define-enum-conditionally ER_DROP_DB_WITH_READ_LOCK)
(define-enum-conditionally ER_CREATE_DB_WITH_READ_LOCK)
(define-enum-conditionally ER_WRONG_ARGUMENTS)
(define-enum-conditionally ER_NO_PERMISSION_TO_CREATE_USER)
(define-enum-conditionally ER_UNION_TABLES_IN_DIFFERENT_DIR)
(define-enum-conditionally ER_LOCK_DEADLOCK)
(define-enum-conditionally ER_TABLE_CANT_HANDLE_FT)
(define-enum-conditionally ER_CANNOT_ADD_FOREIGN)
(define-enum-conditionally ER_NO_REFERENCED_ROW)
(define-enum-conditionally ER_ROW_IS_REFERENCED)
(define-enum-conditionally ER_CONNECT_TO_MASTER)
(define-enum-conditionally ER_QUERY_ON_MASTER)
(define-enum-conditionally ER_ERROR_WHEN_EXECUTING_COMMAND)
(define-enum-conditionally ER_WRONG_USAGE)
(define-enum-conditionally ER_WRONG_NUMBER_OF_COLUMNS_IN_SELECT)
(define-enum-conditionally ER_CANT_UPDATE_WITH_READLOCK)
(define-enum-conditionally ER_MIXING_NOT_ALLOWED)
(define-enum-conditionally ER_DUP_ARGUMENT)
(define-enum-conditionally ER_USER_LIMIT_REACHED)
(define-enum-conditionally ER_SPECIFIC_ACCESS_DENIED_ERROR)
(define-enum-conditionally ER_LOCAL_VARIABLE)
(define-enum-conditionally ER_GLOBAL_VARIABLE)
(define-enum-conditionally ER_NO_DEFAULT)
(define-enum-conditionally ER_WRONG_VALUE_FOR_VAR)
(define-enum-conditionally ER_WRONG_TYPE_FOR_VAR)
(define-enum-conditionally ER_VAR_CANT_BE_READ)
(define-enum-conditionally ER_CANT_USE_OPTION_HERE)
(define-enum-conditionally ER_NOT_SUPPORTED_YET)
(define-enum-conditionally ER_MASTER_FATAL_ERROR_READING_BINLOG)
(define-enum-conditionally ER_SLAVE_IGNORED_TABLE)
(define-enum-conditionally ER_INCORRECT_GLOBAL_LOCAL_VAR)
(define-enum-conditionally ER_WRONG_FK_DEF)
(define-enum-conditionally ER_KEY_REF_DO_NOT_MATCH_TABLE_REF)
(define-enum-conditionally ER_OPERAND_COLUMNS)
(define-enum-conditionally ER_SUBQUERY_NO_1_ROW)
(define-enum-conditionally ER_UNKNOWN_STMT_HANDLER)
(define-enum-conditionally ER_CORRUPT_HELP_DB)
(define-enum-conditionally ER_CYCLIC_REFERENCE)
(define-enum-conditionally ER_AUTO_CONVERT)
(define-enum-conditionally ER_ILLEGAL_REFERENCE)
(define-enum-conditionally ER_DERIVED_MUST_HAVE_ALIAS)
(define-enum-conditionally ER_SELECT_REDUCED)
(define-enum-conditionally ER_TABLENAME_NOT_ALLOWED_HERE)
(define-enum-conditionally ER_NOT_SUPPORTED_AUTH_MODE)
(define-enum-conditionally ER_SPATIAL_CANT_HAVE_NULL)
(define-enum-conditionally ER_COLLATION_CHARSET_MISMATCH)
(define-enum-conditionally ER_SLAVE_WAS_RUNNING)
(define-enum-conditionally ER_SLAVE_WAS_NOT_RUNNING)
(define-enum-conditionally ER_TOO_BIG_FOR_UNCOMPRESS)
(define-enum-conditionally ER_ZLIB_Z_MEM_ERROR)
(define-enum-conditionally ER_ZLIB_Z_BUF_ERROR)
(define-enum-conditionally ER_ZLIB_Z_DATA_ERROR)
(define-enum-conditionally ER_CUT_VALUE_GROUP_CONCAT)
(define-enum-conditionally ER_WARN_TOO_FEW_RECORDS)
(define-enum-conditionally ER_WARN_TOO_MANY_RECORDS)
(define-enum-conditionally ER_WARN_NULL_TO_NOTNULL)
(define-enum-conditionally ER_WARN_DATA_OUT_OF_RANGE)
(define-enum-conditionally WARN_DATA_TRUNCATED)
(define-enum-conditionally ER_WARN_USING_OTHER_HANDLER)
(define-enum-conditionally ER_CANT_AGGREGATE_2COLLATIONS)
(define-enum-conditionally ER_DROP_USER)
(define-enum-conditionally ER_REVOKE_GRANTS)
(define-enum-conditionally ER_CANT_AGGREGATE_3COLLATIONS)
(define-enum-conditionally ER_CANT_AGGREGATE_NCOLLATIONS)
(define-enum-conditionally ER_VARIABLE_IS_NOT_STRUCT)
(define-enum-conditionally ER_UNKNOWN_COLLATION)
(define-enum-conditionally ER_SLAVE_IGNORED_SSL_PARAMS)
(define-enum-conditionally ER_SERVER_IS_IN_SECURE_AUTH_MODE)
(define-enum-conditionally ER_WARN_FIELD_RESOLVED)
(define-enum-conditionally ER_BAD_SLAVE_UNTIL_COND)
(define-enum-conditionally ER_MISSING_SKIP_SLAVE)
(define-enum-conditionally ER_UNTIL_COND_IGNORED)
(define-enum-conditionally ER_WRONG_NAME_FOR_INDEX)
(define-enum-conditionally ER_WRONG_NAME_FOR_CATALOG)
(define-enum-conditionally ER_WARN_QC_RESIZE)
(define-enum-conditionally ER_BAD_FT_COLUMN)
(define-enum-conditionally ER_UNKNOWN_KEY_CACHE)
(define-enum-conditionally ER_WARN_HOSTNAME_WONT_WORK)
(define-enum-conditionally ER_UNKNOWN_STORAGE_ENGINE)
(define-enum-conditionally ER_WARN_DEPRECATED_SYNTAX)
(define-enum-conditionally ER_NON_UPDATABLE_TABLE)
(define-enum-conditionally ER_FEATURE_DISABLED)
(define-enum-conditionally ER_OPTION_PREVENTS_STATEMENT)
(define-enum-conditionally ER_DUPLICATED_VALUE_IN_TYPE)
(define-enum-conditionally ER_TRUNCATED_WRONG_VALUE)
(define-enum-conditionally ER_TOO_MUCH_AUTO_TIMESTAMP_COLS)
(define-enum-conditionally ER_INVALID_ON_UPDATE)
(define-enum-conditionally ER_UNSUPPORTED_PS)
(define-enum-conditionally ER_GET_ERRMSG)
(define-enum-conditionally ER_GET_TEMPORARY_ERRMSG)
(define-enum-conditionally ER_UNKNOWN_TIME_ZONE)
(define-enum-conditionally ER_WARN_INVALID_TIMESTAMP)
(define-enum-conditionally ER_INVALID_CHARACTER_STRING)
(define-enum-conditionally ER_WARN_ALLOWED_PACKET_OVERFLOWED)
(define-enum-conditionally ER_CONFLICTING_DECLARATIONS)
(define-enum-conditionally ER_SP_NO_RECURSIVE_CREATE)
(define-enum-conditionally ER_SP_ALREADY_EXISTS)
(define-enum-conditionally ER_SP_DOES_NOT_EXIST)
(define-enum-conditionally ER_SP_DROP_FAILED)
(define-enum-conditionally ER_SP_STORE_FAILED)
(define-enum-conditionally ER_SP_LILABEL_MISMATCH)
(define-enum-conditionally ER_SP_LABEL_REDEFINE)
(define-enum-conditionally ER_SP_LABEL_MISMATCH)
(define-enum-conditionally ER_SP_UNINIT_VAR)
(define-enum-conditionally ER_SP_BADSELECT)
(define-enum-conditionally ER_SP_BADRETURN)
(define-enum-conditionally ER_SP_BADSTATEMENT)
(define-enum-conditionally ER_UPDATE_LOG_DEPRECATED_IGNORED)
(define-enum-conditionally ER_UPDATE_LOG_DEPRECATED_TRANSLATED)
(define-enum-conditionally ER_QUERY_INTERRUPTED)
(define-enum-conditionally ER_SP_WRONG_NO_OF_ARGS)
(define-enum-conditionally ER_SP_COND_MISMATCH)
(define-enum-conditionally ER_SP_NORETURN)
(define-enum-conditionally ER_SP_NORETURNEND)
(define-enum-conditionally ER_SP_BAD_CURSOR_QUERY)
(define-enum-conditionally ER_SP_BAD_CURSOR_SELECT)
(define-enum-conditionally ER_SP_CURSOR_MISMATCH)
(define-enum-conditionally ER_SP_CURSOR_ALREADY_OPEN)
(define-enum-conditionally ER_SP_CURSOR_NOT_OPEN)
(define-enum-conditionally ER_SP_UNDECLARED_VAR)
(define-enum-conditionally ER_SP_WRONG_NO_OF_FETCH_ARGS)
(define-enum-conditionally ER_SP_FETCH_NO_DATA)
(define-enum-conditionally ER_SP_DUP_PARAM)
(define-enum-conditionally ER_SP_DUP_VAR)
(define-enum-conditionally ER_SP_DUP_COND)
(define-enum-conditionally ER_SP_DUP_CURS)
(define-enum-conditionally ER_SP_CANT_ALTER)
(define-enum-conditionally ER_SP_SUBSELECT_NYI)
(define-enum-conditionally ER_STMT_NOT_ALLOWED_IN_SF_OR_TRG)
(define-enum-conditionally ER_SP_VARCOND_AFTER_CURSHNDLR)
(define-enum-conditionally ER_SP_CURSOR_AFTER_HANDLER)
(define-enum-conditionally ER_SP_CASE_NOT_FOUND)
(define-enum-conditionally ER_FPARSER_TOO_BIG_FILE)
(define-enum-conditionally ER_FPARSER_BAD_HEADER)
(define-enum-conditionally ER_FPARSER_EOF_IN_COMMENT)
(define-enum-conditionally ER_FPARSER_ERROR_IN_PARAMETER)
(define-enum-conditionally ER_FPARSER_EOF_IN_UNKNOWN_PARAMETER)
(define-enum-conditionally ER_VIEW_NO_EXPLAIN)
(define-enum-conditionally ER_FRM_UNKNOWN_TYPE)
(define-enum-conditionally ER_WRONG_OBJECT)
(define-enum-conditionally ER_NONUPDATEABLE_COLUMN)
(define-enum-conditionally ER_VIEW_SELECT_DERIVED)
(define-enum-conditionally ER_VIEW_SELECT_CLAUSE)
(define-enum-conditionally ER_VIEW_SELECT_VARIABLE)
(define-enum-conditionally ER_VIEW_SELECT_TMPTABLE)
(define-enum-conditionally ER_VIEW_WRONG_LIST)
(define-enum-conditionally ER_WARN_VIEW_MERGE)
(define-enum-conditionally ER_WARN_VIEW_WITHOUT_KEY)
(define-enum-conditionally ER_VIEW_INVALID)
(define-enum-conditionally ER_SP_NO_DROP_SP)
(define-enum-conditionally ER_SP_GOTO_IN_HNDLR)
(define-enum-conditionally ER_TRG_ALREADY_EXISTS)
(define-enum-conditionally ER_TRG_DOES_NOT_EXIST)
(define-enum-conditionally ER_TRG_ON_VIEW_OR_TEMP_TABLE)
(define-enum-conditionally ER_TRG_CANT_CHANGE_ROW)
(define-enum-conditionally ER_TRG_NO_SUCH_ROW_IN_TRG)
(define-enum-conditionally ER_NO_DEFAULT_FOR_FIELD)
(define-enum-conditionally ER_DIVISION_BY_ZERO)
(define-enum-conditionally ER_TRUNCATED_WRONG_VALUE_FOR_FIELD)
(define-enum-conditionally ER_ILLEGAL_VALUE_FOR_TYPE)
(define-enum-conditionally ER_VIEW_NONUPD_CHECK)
(define-enum-conditionally ER_VIEW_CHECK_FAILED)
(define-enum-conditionally ER_PROCACCESS_DENIED_ERROR)
(define-enum-conditionally ER_RELAY_LOG_FAIL)
(define-enum-conditionally ER_PASSWD_LENGTH)
(define-enum-conditionally ER_UNKNOWN_TARGET_BINLOG)
(define-enum-conditionally ER_IO_ERR_LOG_INDEX_READ)
(define-enum-conditionally ER_BINLOG_PURGE_PROHIBITED)
(define-enum-conditionally ER_FSEEK_FAIL)
(define-enum-conditionally ER_BINLOG_PURGE_FATAL_ERR)
(define-enum-conditionally ER_LOG_IN_USE)
(define-enum-conditionally ER_LOG_PURGE_UNKNOWN_ERR)
(define-enum-conditionally ER_RELAY_LOG_INIT)
(define-enum-conditionally ER_NO_BINARY_LOGGING)
(define-enum-conditionally ER_RESERVED_SYNTAX)
(define-enum-conditionally ER_WSAS_FAILED)
(define-enum-conditionally ER_DIFF_GROUPS_PROC)
(define-enum-conditionally ER_NO_GROUP_FOR_PROC)
(define-enum-conditionally ER_ORDER_WITH_PROC)
(define-enum-conditionally ER_LOGGING_PROHIBIT_CHANGING_OF)
(define-enum-conditionally ER_NO_FILE_MAPPING)
(define-enum-conditionally ER_WRONG_MAGIC)
(define-enum-conditionally ER_PS_MANY_PARAM)
(define-enum-conditionally ER_KEY_PART_0)
(define-enum-conditionally ER_VIEW_CHECKSUM)
(define-enum-conditionally ER_VIEW_MULTIUPDATE)
(define-enum-conditionally ER_VIEW_NO_INSERT_FIELD_LIST)
(define-enum-conditionally ER_VIEW_DELETE_MERGE_VIEW)
(define-enum-conditionally ER_CANNOT_USER)
(define-enum-conditionally ER_XAER_NOTA)
(define-enum-conditionally ER_XAER_INVAL)
(define-enum-conditionally ER_XAER_RMFAIL)
(define-enum-conditionally ER_XAER_OUTSIDE)
(define-enum-conditionally ER_XAER_RMERR)
(define-enum-conditionally ER_XA_RBROLLBACK)
(define-enum-conditionally ER_NONEXISTING_PROC_GRANT)
(define-enum-conditionally ER_PROC_AUTO_GRANT_FAIL)
(define-enum-conditionally ER_PROC_AUTO_REVOKE_FAIL)
(define-enum-conditionally ER_DATA_TOO_LONG)
(define-enum-conditionally ER_SP_BAD_SQLSTATE)
(define-enum-conditionally ER_STARTUP)
(define-enum-conditionally ER_LOAD_FROM_FIXED_SIZE_ROWS_TO_VAR)
(define-enum-conditionally ER_CANT_CREATE_USER_WITH_GRANT)
(define-enum-conditionally ER_WRONG_VALUE_FOR_TYPE)
(define-enum-conditionally ER_TABLE_DEF_CHANGED)
(define-enum-conditionally ER_SP_DUP_HANDLER)
(define-enum-conditionally ER_SP_NOT_VAR_ARG)
(define-enum-conditionally ER_SP_NO_RETSET)
(define-enum-conditionally ER_CANT_CREATE_GEOMETRY_OBJECT)
(define-enum-conditionally ER_FAILED_ROUTINE_BREAK_BINLOG)
(define-enum-conditionally ER_BINLOG_UNSAFE_ROUTINE)
(define-enum-conditionally ER_BINLOG_CREATE_ROUTINE_NEED_SUPER)
(define-enum-conditionally ER_EXEC_STMT_WITH_OPEN_CURSOR)
(define-enum-conditionally ER_STMT_HAS_NO_OPEN_CURSOR)
(define-enum-conditionally ER_COMMIT_NOT_ALLOWED_IN_SF_OR_TRG)
(define-enum-conditionally ER_NO_DEFAULT_FOR_VIEW_FIELD)
(define-enum-conditionally ER_SP_NO_RECURSION)
(define-enum-conditionally ER_TOO_BIG_SCALE)
(define-enum-conditionally ER_TOO_BIG_PRECISION)
(define-enum-conditionally ER_M_BIGGER_THAN_D)
(define-enum-conditionally ER_WRONG_LOCK_OF_SYSTEM_TABLE)
(define-enum-conditionally ER_CONNECT_TO_FOREIGN_DATA_SOURCE)
(define-enum-conditionally ER_QUERY_ON_FOREIGN_DATA_SOURCE)
(define-enum-conditionally ER_FOREIGN_DATA_SOURCE_DOESNT_EXIST)
(define-enum-conditionally ER_FOREIGN_DATA_STRING_INVALID_CANT_CREATE)
(define-enum-conditionally ER_FOREIGN_DATA_STRING_INVALID)
(define-enum-conditionally ER_CANT_CREATE_FEDERATED_TABLE)
(define-enum-conditionally ER_TRG_IN_WRONG_SCHEMA)
(define-enum-conditionally ER_STACK_OVERRUN_NEED_MORE)
(define-enum-conditionally ER_TOO_LONG_BODY)
(define-enum-conditionally ER_WARN_CANT_DROP_DEFAULT_KEYCACHE)
(define-enum-conditionally ER_TOO_BIG_DISPLAYWIDTH)
(define-enum-conditionally ER_XAER_DUPID)
(define-enum-conditionally ER_DATETIME_FUNCTION_OVERFLOW)
(define-enum-conditionally ER_CANT_UPDATE_USED_TABLE_IN_SF_OR_TRG)
(define-enum-conditionally ER_VIEW_PREVENT_UPDATE)
(define-enum-conditionally ER_PS_NO_RECURSION)
(define-enum-conditionally ER_SP_CANT_SET_AUTOCOMMIT)
(define-enum-conditionally ER_MALFORMED_DEFINER)
(define-enum-conditionally ER_VIEW_FRM_NO_USER)
(define-enum-conditionally ER_VIEW_OTHER_USER)
(define-enum-conditionally ER_NO_SUCH_USER)
(define-enum-conditionally ER_FORBID_SCHEMA_CHANGE)
(define-enum-conditionally ER_ROW_IS_REFERENCED_2)
(define-enum-conditionally ER_NO_REFERENCED_ROW_2)
(define-enum-conditionally ER_SP_BAD_VAR_SHADOW)
(define-enum-conditionally ER_TRG_NO_DEFINER)
(define-enum-conditionally ER_OLD_FILE_FORMAT)
(define-enum-conditionally ER_SP_RECURSION_LIMIT)
(define-enum-conditionally ER_SP_PROC_TABLE_CORRUPT)
(define-enum-conditionally ER_SP_WRONG_NAME)
(define-enum-conditionally ER_TABLE_NEEDS_UPGRADE)
(define-enum-conditionally ER_SP_NO_AGGREGATE)
(define-enum-conditionally ER_MAX_PREPARED_STMT_COUNT_REACHED)
(define-enum-conditionally ER_VIEW_RECURSIVE)
(define-enum-conditionally ER_NON_GROUPING_FIELD_USED)
(define-enum-conditionally ER_TABLE_CANT_HANDLE_SPKEYS)
(define-enum-conditionally ER_NO_TRIGGERS_ON_SYSTEM_SCHEMA)
(define-enum-conditionally ER_REMOVED_SPACES)
(define-enum-conditionally ER_AUTOINC_READ_FAILED)
(define-enum-conditionally ER_USERNAME)
(define-enum-conditionally ER_HOSTNAME)
(define-enum-conditionally ER_WRONG_STRING_LENGTH)
(define-enum-conditionally ER_NON_INSERTABLE_TABLE)
(define-enum-conditionally ER_ERROR_LAST)

;;
;; mysql-real-connect client_flags.
;; from mysql_com.h of 5.0.27
;;
"#define ulong uint32_t"
(define-enum-conditionally CLIENT_LONG_PASSWORD)
(define-enum-conditionally CLIENT_FOUND_ROWS)
(define-enum-conditionally CLIENT_LONG_FLAG)
(define-enum-conditionally CLIENT_CONNECT_WITH_DB)
(define-enum-conditionally CLIENT_NO_SCHEMA)
(define-enum-conditionally CLIENT_COMPRESS)
(define-enum-conditionally CLIENT_ODBC)
(define-enum-conditionally CLIENT_LOCAL_FILES)
(define-enum-conditionally CLIENT_IGNORE_SPACE)
(define-enum-conditionally CLIENT_PROTOCOL_41)
(define-enum-conditionally CLIENT_INTERACTIVE)
(define-enum-conditionally CLIENT_SSL)
(define-enum-conditionally CLIENT_IGNORE_SIGPIPE)
(define-enum-conditionally CLIENT_TRANSACTIONS)
(define-enum-conditionally CLIENT_RESERVED)
(define-enum-conditionally CLIENT_SECURE_CONNECTION)
(define-enum-conditionally CLIENT_MULTI_STATEMENTS)
(define-enum-conditionally CLIENT_MULTI_RESULTS)
(define-enum-conditionally CLIENT_SSL_VERIFY_SERVER_CERT)
(define-enum-conditionally CLIENT_REMEMBER_OPTIONS)
)
