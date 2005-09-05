;; dbd.mysql - mysql driver
;;
;;  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
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
;; $Id: mysql.scm,v 1.6 2005/09/05 02:41:51 shiro Exp $

(define-module dbd.mysql
  (use dbi)
  (use gauche.collection)
  (use util.relation)
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (use util.match)
  (export <mysql-driver>
	  <mysql-connection>
	  <mysql-result-set>
	  <mysql-handle>	; mysql.so
	  <mysql-res>		; mysql.so
	  mysql-real-connect	; mysql.so
	  mysql-real-query      ; mysql.so
	  mysql-store-result    ; mysql.so
	  mysql-error		; mysql.so
	  mysql-errno		; mysql.so
	  mysql-fetch-row	; mysql.so
	  mysql-free-result	; mysql.so
	  mysql-close		; mysql.so
          mysql-real-escape-string ; mysql.so
  	  dbd-make-connection
          dbd-execute
	  dbi-close))
(select-module dbd.mysql)

;; Loads extension
(dynamic-load "dbd_mysql")

(define-condition-type <mysql-error> <dbi-error> #f
  (error-code))                         ;mysql error code

(define-class <mysql-driver> (<dbi-driver>)
  ())

(define-class <mysql-connection> (<dbi-connection>)
  ((%handle     :init-keyword :handle :init-value #f)))

(define-class <mysql-result-set> (<dbi-result-set> <relation>)
  ((%handle     :init-keyword :handle :init-value #f)
   (%result-set :init-keyword :result-set :init-value #f)
   (field-names :init-keyword :field-names)
   (num-cols    :init-keyword :num-cols)
   (rows        :init-form #f)))

(define-method dbd-make-connection ((d <mysql-driver>) options option-alist
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
        (flags  (x->integer (assoc-ref option-alist "client_flag" 0)))
        )
    (let1 handle
        (mysql-real-connect host user passwd db port socket flags)
      (make <mysql-connection> :driver-name d :open #t :handle handle))))

(define-method dbd-execute ((c <mysql-connection>) (q <dbi-query>) . params)
  (let* ((h  (slot-ref c '%handle))
         (qr (mysql-real-query h (apply (slot-ref q '%prepared) params))))
    (unless (= qr 0)
      (errorf <mysql-error> :error-code (mysql-errno h)
              "Mysql query failed: ~a" (mysql-error h)))
    (let1 rset (mysql-store-result h)
      (make <mysql-result-set>
        :open #t :connection h :result-set rset
        :field-names (mysql-fetch-field-names rset)))))

(define-method dbi-escape-sql ((c <mysql-connection>) str)
  (mysql-real-escape-string (slot-ref c '%handle) str))

;; Relation API
(define-method call-with-iterator ((r <mysql-result-set>) proc . option)
  (unless (slot-ref r 'open)
    (error <dbi-error> "<mysql-result> already closed:" r))
  (unless (ref r 'rows)
    (set! (ref r 'rows) (get-all-rows (ref r '%result-set))))
  (call-with-iterator (ref r 'rows) proc))

(define (get-all-rows rset)
  (let loop ((rows '()))
    (let1 row (mysql-fetch-row rset)
      (if row
        (loop (cons row rows))
        (reverse! rows)))))

(define-method relation-column-names ((result-set <mysql-result-set>))
  (ref result-set 'field-names))

(define-method relation-accessor ((result-set <mysql-result-set>))
  (lambda (row column . maybe-default)
    (or (and-let* ((i (find-index (cut string=? <> column)
                                  (ref result-set 'field-names))))
          (vector-ref row i))
        (if (pair? maybe-default)
          (car maybe-default)
          (error "invalud column name:" column)))))

(define-method relation-coercer ((result-set <mysql-result-set>))
  identity)

(define-method dbi-close ((result-set <mysql-result-set>))
  (when (dbi-open? c)
    (next-method)
    (mysql-free-result (slot-ref result-set '%result-set))
    (let* ((errno  (mysql-errno (slot-ref result-set '%handle)))
           (errmsg (mysql-error (slot-ref result-set '%handle))))
      (unless (string-null? errmsg)
        (error <mysql-error> :error-code errno :message errmsg)))))

(define-method dbi-close ((c <mysql-connection>))
  (when (dbi-open? c)
    (mysql-close (slot-ref c '%handle))
    (next-method)
    (let* ((errno  (mysql-errno (slot-ref c '%handle)))
           (errmsg (mysql-error (slot-ref c '%handle))))
      (unless (string-null? errmsg)
        (error <mysql-error> :error-code errno :message errmsg)))))

;; Epilogue
(provide "dbd/mysql")


