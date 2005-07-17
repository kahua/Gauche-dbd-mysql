;;;
;;; Test dbd.mysql
;;;

(use gauche.test)
(use gauche.collection)
(use srfi-1)
(use srfi-13)
(use dbi)

(test-start "dbd.mysql")
(use dbd.mysql)
(test-module 'dbd.mysql)

;; dbi-make-driver のテスト:
;; "mysql" ドライバーをロードして
;; クラス <mysql-driver> のインスタンスだったら合格
(define mysql-driver (dbi-make-driver "mysql"))
(test* "dbi-make-driver mysql"
       #t
       (is-a? mysql-driver <mysql-driver>))

;; dbi-make-connection のテスト:
;; <mysql-driver>型のインスタンスを引数にしたとき
;; dbi-make-connection の戻り値が 
;; <mysql-connection>型のインスタンスだったら合格
;; 注: (sys-getenv "USER")で取得した現在のユーザーがパスワードなしで
;;     MySQLの"test"データベースに接続できる必要がある。
(define current-user (sys-getenv "USER"))
(define mysql-connection
  (dbi-make-connection mysql-driver current-user "" "db=test"))
(test* "dbi-make-connection <mysql-driver>"
       #t
       (is-a? mysql-connection <mysql-connection>))

;; dbi-make-query のテスト:
;; <mysql-connection>型のインスタンスを引数にしたとき
;; dbi-make-queryの戻り値が
;; <mysql-query>型のインスタンスだったら合格
(define mysql-query (dbi-make-query mysql-connection))
(test* "dbi-make-query <mysql-connection>"
       #t
       (is-a? mysql-query <mysql-query>))
;;
;;;; testデータベースをdropしておく
;(dbi-execute-query mysql-query "drop database dbi-mysql-test")
;;;; testデータベースを作成しておく
;(dbi-execute-query mysql-query "create database dbi-mysql-test")
;;;; testデータベースに接続する
;(dbi-execute-query mysql-query "connect test")
;;;; testテーブルをdropしておく
(with-error-handler
  (lambda (e) #t)
 (lambda () (dbi-execute-query mysql-query "drop table test")))
;;;; testテーブルを作成しておく
(dbi-execute-query mysql-query
		   "create table test (id integer, name varchar(255))")
;;;; testテーブルにデータをinsertしておく
(dbi-execute-query mysql-query
		   "insert into test (id, name) values (10, 'yasuyuki')")
(dbi-execute-query mysql-query
		  "insert into test (id, name) values (20, 'nyama')")

;; dbi-execute-query のテスト:
;; <mysql-query>型のインスタンスを引数にしたとき
;; dbi-execute-query の戻り値が
;; <mysql-result-set>型のインスタンスだったら合格
(define mysql-result-set (dbi-execute-query mysql-query "select * from test"))
(test* "dbi-execute-query <mysql-query>"
       #t
       (is-a? mysql-result-set <mysql-result-set>))

;; dbi-get-valueのテスト:
;; map の中で mysql-get-value を使って <mysql-result-set> からすべての行を取得し、
;; あらかじめ insertされた (("10" "yasuyuki") ("20" "nyama")) に等しければ合格
(test* "dbi-get-value with map"
       '(("10" "yasuyuki") ("20" "nyama"))
  (map (lambda (row)
	      (list (dbi-get-value row 0) (dbi-get-value row 1)))
	    mysql-result-set))

;; dbi-close <dbi-result-set> のテスト:
;; <mysql-result-set>型のインスタンスをcloseして再度アクセスし、
;; <dbi-exception>が発生したら合格
(dbi-close mysql-result-set)
(test* "dbi-close <mysql-result-set>" *test-error*
       (dbi-close mysql-result-set))

;; dbi-close <dbi-query> のテスト:
;; <mysql-query>型のインスタンスをcloseして再度アクセスし、
;; <dbi-exception>が発生したら合格
(dbi-close mysql-query)
(test* "dbi-clse <mysql-query>" *test-error*
       (dbi-close mysql-query))

;; dbi-close <dbi-connection> のテスト:
;; <mysql-connection>型のインスタンスをcloseして再度アクセスし、
;; <dbi-exception>が発生したら合格
(dbi-close mysql-connection)
(test* "dbi-close <mysql-connection>" *test-error*
       (dbi-cluse mysql-connection))

;; epilogue
(test-end)





