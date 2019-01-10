;; -*- mode: scheme; coding: utf-8 -*-
;;
;; Test for dbd.mysql via DBI.
;;
;;  Copyright (c) 2003-2009 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2009 Time Intermedia Corporation, All rights reserved.

(use gauche.test)
(use gauche.collection)
(use gauche.sequence)
(use util.relation)
(use srfi-1)
(use srfi-13)
(use dbi)

(define-syntax set!!
  (syntax-rules ()
    ((_ var val)
     (let1 v val
       (set! var v)
       v))))

(test-start "dbd.mysql(via DBI)")

(define conn #f)
(define result #f)
(define query #f)

;; Test connecting the database.
;; The tester should set up the mysql database so that the current user
;; can connect to the database "test" without password.
(test* "dbi-connect" '<mysql-connection>
       (let1 c (dbi-connect "dbi:mysql:kahua_test" :username "kahua_test" :password "kahua_secret")
         (set! conn c)
         (class-name (class-of c))))

(test* "dbi-open?" #t (dbi-open? conn))

(test* "escape sql" '("abc" "ab\\'c" "ab\\\\c")
       (map (cut dbi-escape-sql conn <>) '("abc" "ab'c" "ab\\c")))

(guard (e (#t #t))
       (begin (dbi-do conn "drop table test") #t))

(test-section "dbi-do")

(test* "create table" #t
       (begin (dbi-do conn "create table test (id integer, name varchar(255))")
              #t))

(test* "insert" #t
       (begin
         (dbi-do conn "insert into test (id, name) values (10, 'yasuyuki')")
         (dbi-do conn "insert into test (id, name) values (20, 'nyama')")
         #t))

(test* "select" '<mysql-result-set>
       (let1 r (dbi-do conn "select * from test")
         (set! result r)
         (class-name (class-of r))))

(test* "dbi-get-value" '(("10" "yasuyuki") ("20" "nyama"))
       (map (lambda (row)
              (list (dbi-get-value row 0) (dbi-get-value row 1)))
            result))

(test-section "dbi-prepare")

(test* "dbi-prepare"
       '<mysql-query>
       (class-name (class-of (set!! query (dbi-prepare conn "select * from test where id=?")))))

(test* "dbi-open?" #t (dbi-open? query))

(for-each (lambda (param expect)
	    (let1 res (dbi-execute query param)
	      (test* "dbi-open?" #t (dbi-open? res))
	      (test* "dbi-execute" '<mysql-result-set> (class-name (class-of res)))
	      (test* "<mysql-result-set> is a <sequence>" #t (is-a? res <sequence>))
	      (test* "<mysql-result-set> is a <relation>" #t (is-a? res <relation>))
	      (for-each (lambda (row)
			  (test* "dbi-get-value" expect
				 (list (dbi-get-value row 0) (dbi-get-value row 1))))
			res)
	      (dbi-close res)
	      (test* "dbi-close & dbi-open?" #f (dbi-open? res))
	      ))
	  '("10" "20" "30")
	  '(("10" "yasuyuki")
	    ("20" "nyama")
	    #f))
(test* "dbi-close" #f
       (begin
	 (dbi-close query)
	 (dbi-open? query)))

(test* "dbi-do drop table test" #t
       (begin (dbi-do conn "drop table test") #t))

;; blob access test

(test-section "blob-access")

(test* "blob insertion and retrieval"
       '((#("abracadabra\x00;foo"))
         (#(#*"abc\x00;def\xff;")))
       (unwind-protect
           (begin
             (dbi-do conn "drop table if exists test_blob")
             (dbi-do conn "create table test_blob (name text, data longblob)")
             (dbi-do conn "insert into test_blob (name, data) values (?, ?)" '()
                     "abc" #*"abracadabra\x00;foo")
             (dbi-do conn "insert into test_blob (name, data) values (?, ?)" '()
                     "def" #u8(97 98 99 0 100 101 102 255))
             (list
              (coerce-to <list>
                         (dbi-do conn "select data from test_blob where name = 'abc'"))
              (coerce-to <list>
                         (dbi-do conn "select data from test_blob where name = 'def'"))
              ))
         (dbi-do conn "drop table test_blob")))

(test* "dbi-close" #f
       (begin (dbi-close conn)
              (dbi-open? conn)))

;; Test for exceptional cases
;; This would fail if there _is_ a database named "nosuchdb", so
;; I don't enable it by default.
'(test* "dbi-connect (non-existent db)" *test-error*
        (dbi-connect "dbi:mysql:nosuchdb"))

;; epilogue
(test-end)
