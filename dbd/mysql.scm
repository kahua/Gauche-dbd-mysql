;; dbd.mysql - mysql driver
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
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

(define-module dbd.mysql
  (use dbi)
  (use gauche.mop.singleton)
  (use gauche.sequence)
  (use scheme.list)
  (use srfi.13)
  (use srfi.19)
  (use util.match)
  (use util.relation)
  (export <mysql-driver> <mysql-connection> <mysql-result-set>
          <mysql-error>
          mysql-dbd-version
          ))
(select-module dbd.mysql)

;; Loads extension
(dynamic-load "dbd_mysql")

(define-condition-type <mysql-error> <dbi-error> mysql-error?
  (error-code)
  (sql-code))                         ;mysql error code

(define-condition-type <mysql-stmt-error> <mysql-error> mysql-stmt-error?)

(define-class <mysql-driver> (<dbi-driver> <singleton-mixin>)
  ())

(define-class <mysql-connection> (<dbi-connection>)
  ((%handle     :init-keyword :handle :init-value #f)))

(define-method dbi-make-connection ((d <mysql-driver>)
                                    (options <string>)
                                    (option-alist <list>)
                                    . args)
  (let ((user   (get-keyword* :username args
                              (sys-uid->user-name (sys-getuid))))
        (passwd (get-keyword  :password args ""))
        (db     (match option-alist
                  (((maybe-db . #t) . rest) maybe-db)
                  (else (assoc-ref option-alist "db" #f))))
        (host   (assoc-ref option-alist "host" #f))
        (socket (assoc-ref option-alist "unix_socket" #f))
        (port   (x->integer (assoc-ref option-alist "port" 0)))
        (flags  (x->integer (assoc-ref option-alist "client_flag" 0))))
    (let1 handle (mysql-real-connect host user passwd db port socket flags)
      (make <mysql-connection> :driver-name d :open #t :handle handle))))

(define-method dbi-open? ((c <mysql-connection>))
  (not (mysql-handle-closed? (slot-ref c '%handle))))

(define-class <mysql-query> (<dbi-query>)
  ())

(define-class <mysql-result-set> (<relation> <sequence>)
  ((%handle     :init-keyword :handle    :init-value #f)
   (%statement  :init-keyword :statement :init-value #f)
   (%row-count  :init-keyword :row-count)
   (%current-rowid :init-value 0)))

(define-method dbi-prepare ((c <mysql-connection>) (sql <string>) . args)
  (make <mysql-query> :connection c :prepared (mysql-stmt-prepare (slot-ref c '%handle) sql)))

(define-method dbi-open? ((q <mysql-query>))
  (not (mysql-stmt-closed? (slot-ref q 'prepared))))

(define-method dbi-execute-using-connection ((c <mysql-connection>)
                                             (q <dbi-query>) params)
  (let* ((conn (slot-ref c '%handle))
         (prepared  (slot-ref q 'prepared)))
    (unless (dbi-open? q)
      (error <dbi-error> "query already closed: " q))
    (unless (dbi-open? c)
      (error <dbi-error> "connection already closed: " c))
    (apply mysql-stmt-execute prepared params)
    (and (< 0 (mysql-stmt-field-count prepared))
         (make <mysql-result-set>
           :open #t :handle conn :statement prepared :row-count (mysql-stmt-affected-rows prepared)))))

(define-method dbi-escape-sql ((c <mysql-connection>) str)
  (mysql-real-escape-string (slot-ref c '%handle) str))

;; Relation API
(define-method call-with-iterator ((r <mysql-result-set>) proc . maybe-start)
  (let ((stmt (slot-ref r '%statement))
        (start (get-optional maybe-start 0)))
    (letrec ((finish? (lambda ()
                        (>= (slot-ref r '%current-rowid)
                            (slot-ref r '%row-count))))
             (fetch (if (mysql-res? stmt)
                        (lambda ()
                          (and-let* ((row (mysql-fetch-row stmt)))
                            (inc! (slot-ref r '%current-rowid))
                            row))
                        (lambda ()
                          (and-let* ((row (mysql-stmt-fetch stmt)))
                            (inc! (slot-ref r '%current-rowid))
                            (map-to <vector> x->string row)))))
             (seek (if (mysql-res? stmt)
                       mysql-data-seek
                       mysql-stmt-data-seek)))
      (seek stmt start)
      (slot-set! r '%current-rowid start)
      (proc finish? fetch))))

(define-method referencer ((r <mysql-result-set>))
  (let1 stmt (slot-ref r '%statement)
    (receive (seek fetch get)
        (if (mysql-res? stmt)
            (values mysql-data-seek mysql-fetch-row identity)
            (values mysql-stmt-data-seek mysql-stmt-fetch
                    (cut map-to <vector> x->string <>)))
      (lambda (r i . maybe-fallback)
        (if (or (< i 0) (>= i (slot-ref r '%row-count)))
            (get-optional maybe-fallback (error "index is out of range: " i))
            (begin
              (seek stmt i)
              (let1 row (fetch stmt)
                (slot-set! r '%current-rowid (+ i 1))
                (get row))))))))

(define-method relation-rows ((r <mysql-result-set>))
  r)

(define-method relation-column-names ((r <mysql-result-set>))
  (let1 stmt (slot-ref r '%statement)
    (if (mysql-res? stmt)
        (mysql-fetch-field-names stmt)
        (mysql-stmt-fetch-field-names stmt))))

(define-method relation-accessor ((result-set <mysql-result-set>))
  (let1 columns (relation-column-names result-set)
    (lambda (row column . maybe-default)
      (cond
       ((find-index (cut string=? <> column) columns)
        => (cut vector-ref row <>))
       (else (get-optional maybe-default (error "invalud column name:" column)))))))

(define-method relation-coercer ((result-set <mysql-result-set>))
  identity)

(define-method dbi-open? ((result-set <mysql-result-set>))
  (and (not (let1 stmt (slot-ref result-set '%statement)
              (if (mysql-res? stmt)
                  (mysql-res-closed? stmt)
                  (mysql-stmt-res-closed? stmt))))
       (not (mysql-handle-closed? (slot-ref result-set '%handle)))))

(define-method dbi-close ((result-set <mysql-result-set>))
  (let1 stmt (slot-ref result-set '%statement)
    (if (mysql-res? stmt)
        (mysql-free-result stmt)
        (mysql-stmt-free-result stmt))))

(define-method dbi-close ((query <mysql-query>))
  (mysql-stmt-close (slot-ref query 'prepared)))

(define-method dbi-close ((c <mysql-connection>))
  (mysql-close (slot-ref c '%handle)))

;; <mysql-time> handling

(define-macro (when-available sym . definitions)
  (if (global-variable-bound? (current-module) sym)
    `(begin ,@definitions)
    `(begin ,@(filter-map (^[def]
                            (match def
                              [('define x . _) `(define ,x (undefined))]
                              [_ #f]))
                          definitions))))

(when-available
 <mysql-time>
 (define (mysql-time->string t)

   (define fmt-date "~d-~2,'0d-~2,'0d")
   (define fmt-time "~2,'0d:~2,'0d:~2,'0d")
   (define fmt-flacsec ".~6,'0d")
   (define fmt-time/f (string-append fmt-time fmt-flacsec))
   (define fmt-datetime (string-append fmt-date " " fmt-time))
   (define fmt-datetime/f (string-append fmt-datetime fmt-flacsec))

   (define (ts-date->string t)
     (format #f fmt-date
             (slot-ref t 'year) (slot-ref t 'month) (slot-ref t 'day)))
   (define (ts-datetime->string t)
     (if (> (slot-ref t 'second-part) 0)
       (format #f fmt-datetime/f
               (slot-ref t 'year) (slot-ref t 'month) (slot-ref t 'day)
               (slot-ref t 'hour) (slot-ref t 'minute) (slot-ref t 'second)
               (slot-ref t 'second-part))
       (format #f fmt-datetime
               (slot-ref t 'year) (slot-ref t 'month) (slot-ref t 'day)
               (slot-ref t 'hour) (slot-ref t 'minute) (slot-ref t 'second))))
   (define (ts-time->string t)
     (if (> (slot-ref t 'second-part) 0)
       (format #f fmt-time/f
               (slot-ref t 'hour) (slot-ref t 'minute) (slot-ref t 'second)
               (slot-ref t 'second-part))
       (format #f fmt-time
               (slot-ref t 'hour) (slot-ref t 'minute) (slot-ref t 'second))))

   (let1 time-type (slot-ref t 'time-type)
     (cond
      ((eqv? MYSQL_TIMESTAMP_DATE time-type) (ts-date->string t))
      ((eqv? MYSQL_TIMESTAMP_DATETIME time-type) (ts-datetime->string t))
      ((eqv? MYSQL_TIMESTAMP_TIME time-type) (ts-time->string t))
      ((or (eqv? MYSQL_TIMESTAMP_NONE time-type)
           (eqv? MYSQL_TIMESTAMP_ERROR time-type))
       (error "time-type indicates some error on this field:" time-type))
      (else (error "Unknown time-type")))))
 (define-method x->string ((t <mysql-time>))
   (mysql-time->string t))

 (define (sys-tm->mysql-time tm)
   (make <mysql-time>
     :year (+ 1900 (slot-ref tm 'year))
     :month (+ 1 (slot-ref tm 'mon))
     :day (slot-ref tm 'mday)
     :hour (slot-ref tm 'hour)
     :minute (slot-ref tm 'min)
     :second (slot-ref tm 'sec)
     :second-part 0))
 (define (mysql-time->sys-tm mtime)
   (make <sys-tm>
     :year (- (slot-ref mtime 'year) 1900)
     :mon (- (slot-ref mtime 'month) 1)
     :mday (slot-ref mtime 'day)
     :hour (slot-ref mtime 'hour)
     :min (slot-ref mtime 'minute)
     :sec (slot-ref mtime 'second)))

 (define (date->mysql-time d)
   (make <mysql-time>
     :year (slot-ref d 'year)
     :month (slot-ref d 'month)
     :day (slot-ref d 'day)
     :hour (slot-ref d 'hour)
     :minute (slot-ref d 'minute)
     :second (slot-ref d 'second)
     :second-part (quotient (slot-ref d 'nanosecond) 1000)))
 (define (mysql-time->date t)
   (let1 d (current-date)
     (for-each (lambda (sname)
                 (slot-set! d sname (slot-ref t sname)))
               '(year month day hour minute second))
     (slot-set! d 'nanosecond (* 1000 (slot-ref t 'second-part)))
     d))
 )

;; Low-level API
(export-if-defined <mysql-handle> <mysql-res> <mysql-row-offset> <mysql-field> <mysql-charset>
                   mysql-handle? mysql-res? mysql-row-offset? mysql-field? mysql-charset?
                   mysql-affected-rows mysql-autocommit
                   mysql-change-user mysql-character-set-name
                   mysql-close mysql-commit mysql-debug mysql-data-seek
                   mysql-dump-debug-info mysql-error mysql-errno
                   mysql-fetch-field mysql-fetch-field-direct mysql-fetch-fields mysql-fetch-lengths
                   mysql-fetch-field-names mysql-fetch-row mysql-field-count
                   mysql-field-seek mysql-field-tell mysql-row-seek mysql-row-tell
                   mysql-free-result mysql-get-character-set-info mysql-get-client-info
                   mysql-get-client-version mysql-get-host-info mysql-get-proto-info mysql-get-server-info
                   mysql-get-server-version mysql-get-ssl-cipher mysql-info mysql-insert-id mysql-hex-string
                   mysql-list-dbs mysql-list-fields mysql-list-processes
                   mysql-list-tables mysql-num-fields mysql-num-rows
                   mysql-real-connect mysql-real-escape-string mysql-real-query
                   mysql-refresh mysql-rollback mysql-select-db mysql-set-character-set
                   mysql-shutdown mysql-sqlstate mysql-stat mysql-store-result mysql-use-result
                   mysql-thread-id mysql-warning-count
                   mysql-handle-closed? mysql-res-closed? mysql-ssl-set

                   ;; Low-level Prepared Statement API
                   <mysql-stmt> <mysql-time> <mysql-stmt-error> mysql-stmt-error?
                   mysql-stmt? mysql-time? mysql-stmt-affected-rows mysql-stmt-close
                   mysql-stmt-data-seek mysql-stmt-errno mysql-stmt-error
                   mysql-stmt-execute mysql-stmt-fetch mysql-stmt-field-count
                   mysql-stmt-free-result mysql-stmt-insert-id mysql-stmt-num-rows
                   mysql-stmt-param-count mysql-stmt-prepare mysql-stmt-reset
                   mysql-stmt-sqlstate
                   mysql-stmt-closed?
                   mysql-stmt-fetch-field-names
                   mysql-time->string

                   mysql-get-charset-number
                   )

(export-if-defined REFRESH_GRANT REFRESH_LOG REFRESH_TABLES REFRESH_HOSTS
                   REFRESH_STATUS REFRESH_THREADS REFRESH_SLAVE REFRESH_MASTER)

(export-if-defined MYSQL_TYPE_TINY MYSQL_TYPE_SHORT MYSQL_TYPE_LONG MYSQL_TYPE_INT24
                   MYSQL_TYPE_LONGLONG MYSQL_TYPE_DECIMAL MYSQL_TYPE_NEWDECIMAL
                   MYSQL_TYPE_FLOAT MYSQL_TYPE_DOUBLE MYSQL_TYPE_BIT MYSQL_TYPE_TIMESTAMP
                   MYSQL_TYPE_DATE MYSQL_TYPE_TIME MYSQL_TYPE_DATETIME MYSQL_TYPE_YEAR
                   MYSQL_TYPE_STRING MYSQL_TYPE_VAR_STRING MYSQL_TYPE_BLOB MYSQL_TYPE_SET
                   MYSQL_TYPE_ENUM MYSQL_TYPE_GEOMETRY MYSQL_TYPE_NULL)

(export-if-defined MYSQL_TIMESTAMP_NONE MYSQL_TIMESTAMP_ERROR MYSQL_TIMESTAMP_DATE
                   MYSQL_TIMESTAMP_DATETIME MYSQL_TIMESTAMP_TIME)
