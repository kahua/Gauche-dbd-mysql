;; -*- mode: scheme; coding: utf-8 -*-
;;
;; Test for dbd.mysql low level API.
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;
;; $Id: dbd.scm,v 1.13 2007/02/21 06:58:32 bizenn Exp $

(use gauche.test)
(use gauche.collection)
(use srfi-1)
(use srfi-13)

(test-start "dbd.mysql(low level)")
(use dbd.mysql)
(test-module 'dbd.mysql)

(define-constant *db* "test")
(define *mysql* #f)
(define *result* #f)
(define *stmt* #f)

(test* "mysql-real-connect/fail" <mysql-error>
       (guard (e (else (class-of e)))
	 (mysql-real-connect #f "" "" "nonexistent" 0 #f 0)))
(test* "mysql-real-connect/success" <mysql-handle>
       (let1 c (mysql-real-connect #f #f #f *db* 0 #f 0)
	 (set! *mysql* c)
	 (class-of c)))
(test* "mysql-character-set-name" "utf8"
       (mysql-character-set-name *mysql*) string=?)

(test* "mysql-real-escape-string" "\\0a\\rb\\nc\\\\d\\'e\\\"f\\Z"
       (mysql-real-escape-string *mysql* "\0a\rb\nc\\d'e\"f\x1a"))

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
       (let1 r (mysql-store-result *mysql*)
	 (set! *result* r)
	 (class-of r)))
(test* "mysql-affected-rows/select 10 records" 10 (mysql-affected-rows *mysql*))
(dotimes (i 10)
  (test* #`"mysql-fetch-row record #,|i|" `#(,#`",|i|" ,#`"DATA,|i|") (mysql-fetch-row *result*) equal?))
(test* "mysql-res-closed?/before close" #f (mysql-res-closed? *result*))
(test* "mysql-free-result" (undefined) (mysql-free-result *result*))
(test* "mysql-res-closed?/after close" #t (mysql-res-closed? *result*))

(test* "mysql-real-query/drop table" (undefined) (mysql-real-query *mysql* "DROP TABLE DBD_TEST"))

(test* "mysql-stmt-prepare/create table" <mysql-stmt>
       (let1 s (mysql-stmt-prepare *mysql* "
                  CREATE TABLE DBD_TEST (
                    id integer,
                    name varchar(20),
                    data varchar(255),
                    constraint primary key (id),
                    constraint unique (name))")
	 (set! *stmt* s)
	 (class-of s)))
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
  (test* "mysql-stmt-execute/update with Japanese data" (undefined) (mysql-stmt-execute stmt "テストデータ" 1))
  (mysql-stmt-close stmt))

(let1 stmt (mysql-stmt-prepare *mysql* "SELECT DATA from DBD_TEST where ID = 1")
  (mysql-stmt-execute stmt)
  (test* "mysql-stmt-fetch/select of Japanese data" #("テストデータ") (mysql-stmt-fetch stmt) equal?)
  (mysql-stmt-close stmt))

(let1 stmt (mysql-stmt-prepare *mysql* "DROP TABLE DBD_TEST")
  (mysql-stmt-execute stmt)
  (mysql-stmt-close stmt))

(test* "mysql-handle-closed?/before close" #f (mysql-handle-closed? *mysql*))
(test* "mysql-close" (undefined) (mysql-close *mysql*))
(test* "mysql-handle-closed?/after close" #t (mysql-handle-closed? *mysql*))

;; epilogue
(test-end)
