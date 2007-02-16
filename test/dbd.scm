;; -*- mode: scheme; coding: utf-8 -*-
;;
;; Test for dbd.mysql low level API.
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;
;; $Id: dbd.scm,v 1.6 2007/02/16 07:26:33 bizenn Exp $

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

(let1 stmt (mysql-stmt-prepare *mysql* "DROP TABLE DBD_TEST")
  (mysql-stmt-execute stmt)
  (mysql-stmt-close stmt))

(test* "mysql-handle-closed?/before close" #f (mysql-handle-closed? *mysql*))
(test* "mysql-close" (undefined) (mysql-close *mysql*))
(test* "mysql-handle-closed?/after close" #t (mysql-handle-closed? *mysql*))

;; epilogue
(test-end)
