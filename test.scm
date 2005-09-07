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

(define conn #f)
(define result #f)

;; Test connecting the database.
;; The tester should set up the mysql database so that the current user
;; can connect to the database "test" without password.
(test* "dbi-connect" '<mysql-connection>
       (let1 c (dbi-connect "dbi:mysql:test")
         (set! conn c)
         (class-name (class-of c))))

(test* "escape sql" '("abc" "ab\\'c" "ab\\\\c")
       (map (cut dbi-escape-sql conn <>) '("abc" "ab'c" "ab\\c")))

(test* "dbi-do drop table test" #t
       (begin (dbi-do conn "drop table test") #t))

(test* "dbi-do create table test" #t
       (begin (dbi-do conn "create table test (id integer, name varchar(255))")
              #t))

(test* "dbi-do insert" #t
       (begin
         (dbi-do conn "insert into test (id, name) values (10, 'yasuyuki')")
         (dbi-do conn "insert into test (id, name) values (20, 'nyama')")
         #t))

(test* "dbi-do select" '<mysql-result-set>
       (let1 r (dbi-do conn "select * from test")
         (set! result r)
         (class-name (class-of r))))

(test* "dbi-get-value" '(("10" "yasuyuki") ("20" "nyama"))
       (map (lambda (row)
              (list (dbi-get-value row 0) (dbi-get-value row 1)))
            result))

(test* "dbi-prepare & execute" '(("10" "yasuyuki"))
       (map (lambda (row)
              (list (dbi-get-value row 0) (dbi-get-value row 1)))
            ((dbi-prepare conn "select * from test where id=?")
             "10")))

(test* "dbi-prepare & execute" '(("20" "nyama"))
       (map (lambda (row)
              (list (dbi-get-value row 0) (dbi-get-value row 1)))
            ((dbi-prepare conn "select * from test where id=?")
             "20")))

(test* "dbi-prepare & execute" '()
       (map (lambda (row)
              (list (dbi-get-value row 0) (dbi-get-value row 1)))
            ((dbi-prepare conn "select * from test where id=?")
             "30")))

(test* "dbi-prepare & execute (pass-through)" '(("20" "nyama"))
       (map (lambda (row)
              (list (dbi-get-value row 0) (dbi-get-value row 1)))
            ((dbi-prepare conn "select * from test where id=20"
                          :pass-through #t))))

(test* "dbi-prepare & execute (pass-through)" '<mysql-error>
       (guard (e (else (class-name (class-of e))))
         ((dbi-prepare conn "select * from test where id=?"
                       :pass-through #t))))

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
