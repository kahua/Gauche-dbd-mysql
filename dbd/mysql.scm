;; dbd.mysql - mysql driver
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
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
;; $Id: mysql.scm,v 1.1 2005/07/17 06:43:18 shiro Exp $

(define-module dbd.mysql
  (use dbi)
  (use gauche.collection)
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (use www.cgi)
  (export <mysql-driver>
	  <mysql-connection>
	  <mysql-query>
	  <mysql-result-set>
	  <mysql-handle>	; mysql.so
	  <mysql-res>		; mysql.so
	  mysql-real-connect	; mysql.so
	  mysql-query-use-result ; mysql.so
	  mysql-query-store-result ; mysql.so
	  mysql-error		; mysql.so
	  mysql-errno		; mysql.so
	  mysql-fetch-row	; mysql.so
	  mysql-free-result	; mysql.so
	  mysql-close		; mysql.so
  	  dbi-make-connection
	  dbi-make-query
	  dbi-execute-query
	  dbi-get-value
	  dbi-close))
(select-module dbd.mysql)

;; Loads extension
(dynamic-load "gauche_dbd_mysql")

(define-class <mysql-driver> (<dbi-driver>)
  ((%mysql-handle :init-keyword :mysql-handle :init-value #f)))

(define-class <mysql-connection> (<dbi-connection>)
  ((%connection :init-keyword :connection :init-value #f)))

(define-class <mysql-query> (<dbi-query>)
  ((%connection :init-keyword :connection :init-value #f)
   (%execute-proc :init-keyword :execute-proc :init-value mysql-query-store-result)))

(define-class <mysql-result-set> (<dbi-result-set> <collection>)
  ((%result-set :init-keyword :result-set :init-value #f)
   (%connection :init-keyword :connection :init-value #f)
   (num-cols :init-keyword :num-cols)))

(define-method dbi-make-connection ((d <mysql-driver>) (user <string>)
				    (password <string>) (option <string>))
       (if (not (slot-ref d '%mysql-handle))
	   (slot-set! d `%mysql-handle (make <mysql-handle>)))
       ;; option ¤ÎÊ¬²ò
       (let ((params
	      (cgi-parse-parameters :query-string option)))
	 (let ((h (car (assoc-ref params "host" (list ""))))
	       (b (car (assoc-ref params "db" (list ""))))
	       (s (car (assoc-ref params "unix_socket" (list ""))))
	       (p (or (string->number (car (assoc-ref params "port" (list "")))) 0))
	       (f (or (string->number (car (assoc-ref params "client_flag" (list "")))) 0)))
	   (let ((conn (mysql-real-connect h user password b
					   p s f
					   (slot-ref d '%mysql-handle)
					   (make <mysql-handle>))))
	     (if conn
		 (make <mysql-connection> :driver-name d :open #t
		       :connection conn)
		 (if (slot-ref d '%mysql-handle)
		     (let ((errmsg (mysql-error (slot-ref d '%mysql-handle)))
			   (errno  (mysql-errno (slot-ref d '%mysql-handle)))) 
		       (raise
			(make <dbi-exception>
			  :error-code errno
			  :message errmsg)))
		     (raise (make <dbi-exception> :message "%mysql-handle is null."))))))))


(define-method dbi-make-query ((c <mysql-connection>) (o <string>))
  (if (not (slot-ref c 'open))
      (raise
       (make <dbi-exception> :error-code -1
	     :message "connection has already closed.")))
  (let ((proc mysql-query-store-result))
    (if (equal? o "mysql-query-use-result")
      (set! proc mysql-query-use-result))
    (make <mysql-query> :open #t
	  :connection (slot-ref c '%connection)
	  :execute-proc proc)))

(define-method dbi-make-query ((c <mysql-connection>))
;;  (if (not (slot-ref c 'open))
;;      (raise
;;       (make <dbi-exception> :error-code -1
;;	     :message "connection has already closed.")))
;;  (make <mysql-query> :open #t
;;	:connection (slot-ref c '%connection)))
  (dbi-make-query c "mysql-query-store-result"))

(define (execute-query proc q s)
  (let* ((result-set (proc s
			   (slot-ref q '%connection)
			   (make <mysql-res>)))
	     (errno (mysql-errno (slot-ref q '%connection)))
	     (errmsg (mysql-error (slot-ref q '%connection))))
	(if (not (string-null? errmsg))
	    (raise
	     (make <dbi-exception> :error-code errno
		   :message errmsg))
	    (make <mysql-result-set> :open #t
		  :connection (slot-ref q '%connection)
		  :result-set result-set))))
  
(define-method dbi-execute-query ((q <mysql-query>) (s <string>))
  (if (not (slot-ref q 'open))
      (raise
       (make <dbi-exception> :error-code -2
	     :message "query has already closed."))
	  (execute-query (slot-ref q '%execute-proc) q s)))

(define-method call-with-iterator ((r <mysql-result-set>) proc . option)
  (if (not (slot-ref r 'open))
      (raise
       (make <dbi-exception> :error-code -4 :message "<mysql-result> already closed."))
      (let ((is-end #f))
	(define (fetch-one-row)
	  (if (not is-end)
	      (let ((one-row (mysql-fetch-row (slot-ref r '%result-set))))
		(if (not one-row) (set! is-end #t))
		(if (null? one-row) (set! is-end #t))
		(if (equal? one-row '()) (set! is-end #t))
		one-row)
	      '()))
	(let ((prev-row (fetch-one-row)))
	  (define (end?) is-end)
	  (define (next)
	    (let ((row prev-row))
	      (set! prev-row (fetch-one-row))
	      row))
	  (proc end? next)))))

(define-method dbi-get-value ((l <list>) (n <integer>))
  (cond ((null? l) l)
	((< n 0)
	 (raise
	  (make <dbi-exception> :error-code -5
		:message "column id invalid.")))
	((> n (length l))
	 (raise
	  (make <dbi-exception> :error-code -6
		:message "clumn id out of range.")))
	(else (car (drop l n)))))

(define-method dbi-close ((result-set <mysql-result-set>))
  (if (not (slot-ref result-set 'open))
      (raise
       (make <dbi-exception> :error-code -6 :message "already closed."))
      (begin
	(mysql-free-result (slot-ref result-set '%result-set))
	(let* ((errno (mysql-errno (slot-ref result-set '%connection)))
	       (errmsg (mysql-error (slot-ref result-set '%connection))))
	  (if (not (string-null? errmsg))
	      (raise
	       (make <dbi-exception> :error-code errno
		     :message errmsg))
	      (slot-set! result-set 'open #f))))))

(define-method dbi-close ((query <mysql-query>))
  (if (not (slot-ref query 'open))
      (raise
       (make <dbi-exception> :error-code -7 :message "already closed.")))
  (slot-set! query 'open #f))

(define-method dbi-close ((connection <mysql-connection>))
  (if (not (slot-ref connection 'open))
      (raise
       (make <dbi-exception> :error-code -8 :message "already closed."))
      (begin (mysql-close (slot-ref connection '%connection))
	     (let* ((errno (mysql-errno (slot-ref connection '%connection)))
		    (errmsg (mysql-error (slot-ref connection '%connection))))
	       (if (not (string-null? errmsg))
		   (raise
		    (make <dbi-exception> :error-code errno
			  :message errmsg))
		   (slot-set! connection 'open #f))))))

;; Epilogue
(provide "dbd/mysql")


