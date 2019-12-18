;; -*- mode: scheme; coding: utf-8 -*-
;;
;; Test for dbd.mysql low level API.
;;
;;  Copyright (c) 2003-2009 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2009 Time Intermedia Corporation, All rights reserved.

(use gauche.test)
(use gauche.collection)
(use gauche.sequence)
(use gauche.interactive)
(use srfi-1)
(use srfi-13)
(use util.list)

;; Utilities

(define (is-class? class obj)
  (eq? class (class-of obj)))

(define-syntax set!!
  (syntax-rules ()
    ((_ var val)
     (let1 v val
       (set! var v)
       v))))

(define (info->version str)
  (rxmatch-if (#/^(\d+)\.(\d+)\.(\d+)/ str)
      (#f major minor subm)
    (+ (* 10000 (x->integer major)) (* 100 (x->integer minor)) (x->integer subm))
    0))

(define (with-mysql-connection user password db proc)
  (and-let* ((mysql (guard (_ (else #f))
		      (mysql-real-connect #f user password db 0 #f 0))))
    (unwind-protect
     (proc mysql)
     (mysql-close mysql))))

;; Tests

(test-start "dbd.mysql(low level)")
(use dbd.mysql)
(test-module 'dbd.mysql)

(define-constant *db* "kahua_test")
(define-constant *user* "kahua_test")
(define-constant *password* "kahua_secret")
(define *mysql* #f)
(define *result* #f)
(define *stmt* #f)

(test-section "Client information (requires no connection)")
(let ((cinfo #f)
      (cver  #f))
  (test* "mysql-get-client-info" <string> (set!! cinfo (mysql-get-client-info)) is-class?)
  (test* "mysql-get-client-version" <integer> (set!! cver (mysql-get-client-version)) is-class?)
  (test* "client-info and client-version matching" cver (info->version cinfo) =))

(test-section "Server and connection information (requires connection to a server)")
(with-mysql-connection *user* *password* *db*
  (lambda (mysql)
    (let ((sinfo #f)
	  (sver  #f))
      (test* "mysql-get-server-info" <string> (set!! sinfo (mysql-get-server-info mysql)) is-class?)
      (test* "mysql-get-server-version" <integer> (set!! sver (mysql-get-server-version mysql)) is-class?)
      (test* "server-info and server-version matching" sver (info->version sinfo) =)
      (test* "mysql-get-host-info" "Localhost via UNIX socket" (mysql-get-host-info mysql) string-ci=?)
      (test* "mysql-get-proto-info" 10 (mysql-get-proto-info mysql) <=))))

(let ((mysql #f)
      (sinfo #f)
      (sver  #f))
  (test* "mysql-real-connect w/o db" <mysql-handle>
	 (set!! mysql (mysql-real-connect #f *user* *password* *db* 0 #f 0))
	 is-class?)
  (test* "mysql-handle-closed?/before close" #f (mysql-handle-closed? mysql))
  (test* "mysql-close" (undefined) (mysql-close mysql))
  (test* "mysql-handle-closed?/after close" #t (mysql-handle-closed? mysql))
  )

(test-section "Connection to DB")

(test* "mysql-real-connect/fail" (test-error <mysql-error>)
       (mysql-real-connect #f *user* *password* "nonexistent" 0 #f 0))
(test* "mysql-real-connect/success" <mysql-handle>
       (set!! *mysql* (mysql-real-connect #f *user* *password* *db* 0 #f 0))
       is-class?)

(test-section "Character set handling")
(when (global-variable-bound? (current-module) 'mysql-character-set-name)
  (test* "mysql-character-set-name" "utf8"
	 (mysql-character-set-name *mysql*) string=?))
(when (global-variable-bound? (current-module) 'mysql-get-character-set-info)
  (let1 charset (mysql-get-character-set-info *mysql*)
    (test* "mysql-get-character-set-info: <mysql-charset>" <mysql-charset> (class-of charset))
    (for-each (lambda (args)
		(apply (lambda (sname comp value)
			 (test* (format "mysql-get-character-set-info: ~a" sname)
				value (slot-ref charset sname) comp))
		       args))
	      `((name ,string=? "utf8_general_ci")
		(csname ,string=? "utf8")
		(number ,= 33)
		(comment ,(^[x y] (or (is-class? x y) (not y))) ,<string>)
		(dir ,equal? #f)
		(mbminlen ,= 1)
		(mbmaxlen ,= 3)))))

(test* "mysql-real-escape-string" "\\0a\\rb\\nc\\\\d\\'e\\\"f\\Z"
       (mysql-real-escape-string *mysql* "\0a\rb\nc\\d'e\"f\x1a"))

(define (default-value-compare actual expected)
  (if (not expected)
    (or (not actual) (equal? actual "")) ;MySQL and MariaDB differ
    (equal? expected actual)))

(define-constant *mysql-field-slots*
  `((name            ,string-ci=?)
    (original-name   ,string-ci=?)
    (table           ,string-ci=?)
    (original-table  ,string-ci=?)
    (db              ,string-ci=?)
    (catalog         ,string=?)
    (default-value   ,default-value-compare)
    (length          ,=)
    (max-length      ,=)
    (not-null?       ,eqv?)
    (primary-key?    ,eqv?)
    (unique-key?     ,eqv?)
    (multiple-key?   ,eqv?)
    (unsigned?       ,eqv?)
    (zerofill?       ,eqv?)
    (auto-increment? ,eqv?)
    (decimals        ,=)
    (charset-number  ,=)
    (type            ,=)))

(test-section "Issue MySQL statement(not prepared)")

(define-constant *field-definition*
  `#(((name . "ID") (original-name . "ID")
      (table . "DBD_TEST") (original-table . "DBD_TEST")
      (db . "kahua_test") (catalog . "def")
      (default-value . #f)
      (length . 11) (max-length . 1)
      (not-null? . #t) (primary-key? . #t) (unique-key? . #f)
      (multiple-key? . #f) (unsigned? . #f) (zerofill? . #f)
      (binary? . #f) (auto-increment? . #f) (decimals . 0)
      (charset-number . 63) (type . ,MYSQL_TYPE_LONG))
     ((name . "DATA") (original-name . "DATA")
      (table . "DBD_TEST") (original-table . "DBD_TEST")
      (db . "kahua_test") (catalog . "def")
      (default-value . #f)
      (length . 765) (max-length . 5)
      (not-null? . #f) (primary-key? . #f) (unique-key? . #f)
      (multiple-key? . #f) (unsigned? . #f) (zerofill? . #f)
      (binary? . #f) (auto-increment? . #f) (decimals . 0)
      (charset-number . 33) (type . ,MYSQL_TYPE_VAR_STRING)
      )))

(define (field->alist field)
  (if (is-class? <mysql-field> field)
    (map (^[s] (cons (car s) (slot-ref field (car s))))
         *mysql-field-slots*)
    '()))

(define (check-field info field-alist)
  (define (cmp-slot-value slot-name cmp-func?)
    (cmp-func? (assq-ref field-alist slot-name)
	       (assq-ref info slot-name #f)))
  (let/cc ret
    (for-each (lambda (args)
                (or (apply cmp-slot-value args)
                    (ret #f)))
              *mysql-field-slots*)
    #t))

(test* "mysql-real-query/create table" (undefined)
       (mysql-real-query *mysql* "CREATE TABLE DBD_TEST (id integer, data varchar(255), constraint primary key(id))"))
(dotimes (i 10)
  (test* #`"mysql-real-query/insert record #,|i|" (undefined)
	 (mysql-real-query *mysql* #`"INSERT INTO DBD_TEST (id, data) values (,|i|,, 'DATA,|i|')"))
  (test* "mysql-affected-rows/insert one record" 1 (mysql-affected-rows *mysql*)))
(test* "mysql-store-result/insert" #f (mysql-store-result *mysql*))
(test* "mysql-real-query/select all" (undefined)
       (mysql-real-query *mysql* "SELECT id, data FROM DBD_TEST order by id"))
(test* "mysql-store-result/select" <mysql-res>
       (set!! *result* (mysql-store-result *mysql*))
       is-class?)
(test* "mysql-affected-rows/select 10 records" 10 (mysql-affected-rows *mysql*))
(define *row-offset* #f)
(test* "mysql-row-tell" <mysql-row-offset>
       (set!! *row-offset* (mysql-row-tell *result*))
       is-class?)
(dotimes (i 10)
  (test* #`"mysql-fetch-row record #,|i|" `#(,#`",|i|" ,#`"DATA,|i|") (mysql-fetch-row *result*) equal?))
(test* "mysql-row-tell" <mysql-row-offset> (mysql-row-seek *result* *row-offset*) is-class?)
(test* "mysql-res-closed?/before close" #f (mysql-res-closed? *result*))
(dotimes (i 10)
  (test* #`"mysql-fetch-row record #,|i|" `#(,#`",|i|" ,#`"DATA,|i|") (mysql-fetch-row *result*) equal?))

(for-each-with-index (lambda (i info)
		       (test* "mysql-field-tell" i (mysql-field-tell *result*))
		       (test* "mysql-fetch-field" (vector-ref *field-definition* i)
			      (field->alist (mysql-fetch-field *result*))
                              check-field))
		     *field-definition*)
(test* "mysql-field-tell" 2 (mysql-field-tell *result*) =)
(test* "mysql-fetch-field" #f (mysql-fetch-field *result*) eq?)

(test* "mysql-field-seek" 2 (mysql-field-seek *result* 0) =)
(for-each-with-index (lambda (i info)
		       (test* "mysql-field-tell" i (mysql-field-tell *result*))
		       (test* "mysql-fetch-field" (vector-ref *field-definition* i)
			      (field->alist (mysql-fetch-field *result*))
                              check-field))
		     *field-definition*)
(test* "mysql-field-tell" 2 (mysql-field-tell *result*) =)
(test* "mysql-fetch-field" #f (mysql-fetch-field *result*) eq?)

(for-each (lambda (info field)
	    (test* "mysql-fetch-fields" info (field->alist field) check-field))
	  *field-definition*
	  (mysql-fetch-fields *result*))

(for-each-with-index (lambda (i info)
		       (test* "mysql-fetch-field-direct"
			      info 
                              (field->alist (mysql-fetch-field-direct *result* i))
                              check-field))
		     *field-definition*)

(for-each (lambda (info length)
	    (test* "mysql-fetch-lengths" (assq-ref info 'max-length) length =))
	  *field-definition*
	  (mysql-fetch-lengths *result*))

(test* "mysql-free-result" (undefined) (mysql-free-result *result*))
(test* "mysql-res-closed?/after close" #t (mysql-res-closed? *result*))
(test* "mysql-real-query/drop table" (undefined) (mysql-real-query *mysql* "DROP TABLE DBD_TEST"))

(when (global-variable-bound? (current-module) '<mysql-stmt>)
  (test-section "Issue MySQL Prepared Statement")
  (test* "mysql-stmt-prepare/create table" <mysql-stmt>
	 (set!! *stmt* (mysql-stmt-prepare *mysql* "
                  CREATE TABLE DBD_TEST (
                    id integer,
                    name varchar(20),
                    data varchar(255),
                    constraint primary key (id),
                    constraint unique (name))"))
	 is-class?)
  (test* "mysql-stmt-param-count/create table" 0 (mysql-stmt-param-count *stmt*))
  (test* "mysql-stmt-field-count/create table" 0 (mysql-stmt-field-count *stmt*))
  (test* "mysql-stmt-execute/create table" (undefined) (mysql-stmt-execute *stmt*))
  (test* "mysql-stmt-closed?/before close" #f (mysql-stmt-closed? *stmt*))
  (test* "mysql-stmt-close/create table" (undefined) (mysql-stmt-close *stmt*))
  (test* "mysql-stmt-closed?/after close" #t (mysql-stmt-closed? *stmt*))
  (let1 stmt (mysql-stmt-prepare *mysql* "INSERT INTO DBD_TEST (id, name, data) values (?, ?, ?)")
    (test* "mysql-stmt-param-count/insert" 3 (mysql-stmt-param-count stmt) =)
    (test* "mysql-stmt-field-count/insert" 0 (mysql-stmt-field-count stmt) =)
    (dotimes (i 10)
      (test* #`"mysql-stmt-execute/insert record #,|i| with parameters" (undefined)
	     (mysql-stmt-execute stmt i #`"DATA,|i|" "This is test data."))
      (test* "mysql-stmt-affected-rows" 1 (mysql-stmt-affected-rows stmt) =))
    (mysql-stmt-close stmt))
  (let1 stmt (mysql-stmt-prepare *mysql* "SELECT id, name, data FROM DBD_TEST where ID in (?,?,?,?)")
    (test* "mysql-stmt-param-count/select" 4 (mysql-stmt-param-count stmt) =)
    (test* "mysql-stmt-field-count/select" 3 (mysql-stmt-field-count stmt) =)
    (test* "mysql-stmt-execute/select with parameter" (undefined) (mysql-stmt-execute stmt 2 4 5 9))
    (test* "mysql-stmt-affected-rows/after select 4 records" 4 (mysql-stmt-affected-rows stmt) =)
    (for-each (lambda (r) (test* "mysql-stmt-fetch" r (mysql-stmt-fetch stmt) equal?))
	      '(#(2 "DATA2" "This is test data.")
		#(4 "DATA4" "This is test data.")
		#(5 "DATA5" "This is test data.")
		#(9 "DATA9" "This is test data.")
		#f))
    (mysql-stmt-close stmt))
  (let1 stmt (mysql-stmt-prepare *mysql* "UPDATE DBD_TEST set data=? where ID between ? and ?")
    (test* "mysql-stmt-param-count/update" 3 (mysql-stmt-param-count stmt) =)
    (test* "mysql-stmt-field-count/update" 0 (mysql-stmt-field-count stmt) =)
    (test* "mysql-stmt-execute/update" (undefined) (mysql-stmt-execute stmt #f 5 7))
    (test* "mysql-stmt-affected-rows/update" 3 (mysql-stmt-affected-rows stmt) =)
    (mysql-stmt-close stmt))
  (let1 stmt (mysql-stmt-prepare *mysql* "SELECT DATA, count(*) from DBD_TEST where DATA is NULL group by DATA")
    (test* "mysql-stmt-param-count/select" 0 (mysql-stmt-param-count stmt) =)
    (test* "mysql-stmt-field-count/select" 2 (mysql-stmt-field-count stmt) =)
    (test* "mysql-stmt-execute/select" (undefined) (mysql-stmt-execute stmt))
    (test* "mysql-stmt-affected-rows/select" 1 (mysql-stmt-affected-rows stmt) =)
    (test* "mysql-stmt-fetch/select" '#(#f 3) (mysql-stmt-fetch stmt) equal?)
    (mysql-stmt-close stmt))
  (let1 stmt (mysql-stmt-prepare *mysql* "UPDATE DBD_TEST set data = ? where ID = ?")
    (test* "mysql-stmt-fetch-field-names/update" '#() (mysql-stmt-fetch-field-names stmt) equal?)
    (test* "mysql-stmt-execute/update with Japanese data" (undefined) (mysql-stmt-execute stmt "テストデータ" 1))
    (mysql-stmt-close stmt))
  (let1 stmt (mysql-stmt-prepare *mysql* "SELECT DATA from DBD_TEST where ID = 1")
    (test* "mysql-stmt-fetch-field-names/select" '#("DATA") (mysql-stmt-fetch-field-names stmt) equal?)
    (mysql-stmt-execute stmt)
    (test* "mysql-stmt-fetch/select of Japanese data" #("テストデータ") (mysql-stmt-fetch stmt) equal?)
    (mysql-stmt-close stmt))
  (let1 stmt (mysql-stmt-prepare *mysql* "SELECT ID, NAME from DBD_TEST order by ID")
    (test* "mysql-stmt-fetch-field-names/select" '#("ID" "NAME") (mysql-stmt-fetch-field-names stmt) equal?)
    (mysql-stmt-execute stmt)
    (test* "mysql-stmt-data-seek" (undefined) (mysql-stmt-data-seek stmt 5))
    (for-each (lambda (r)
		(test* "mysql-stmt-fetch/seeked" r (mysql-stmt-fetch stmt) equal?))
	      '(#(5 "DATA5") #(6 "DATA6") #(7 "DATA7") #(8 "DATA8") #(9 "DATA9") #f))
    (test* "mysql-stmt-data-seek" (undefined) (mysql-stmt-data-seek stmt 0))
    (dotimes (i 7)
      (test* #`"mysql-stmt-fetch/record #,|i|" `#(,i ,#`"DATA,|i|") (mysql-stmt-fetch stmt) equal?))
    (test* "mysql-stmt-data-seek/overflow" (undefined) (mysql-stmt-data-seek stmt 15))
    (test* "mysql-stmt-fetch/eor" #f (mysql-stmt-fetch stmt))
    (mysql-stmt-close stmt))
  (let1 stmt (mysql-stmt-prepare *mysql* "DROP TABLE DBD_TEST")
    (mysql-stmt-execute stmt)
    (mysql-stmt-close stmt)))

(test-section "Disconnection")
(test* "mysql-handle-closed?/before close" #f (mysql-handle-closed? *mysql*))
(test* "mysql-close" (undefined) (mysql-close *mysql*))
(test* "mysql-handle-closed?/after close" #t (mysql-handle-closed? *mysql*))

;; epilogue
(test-end)
