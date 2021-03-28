# Gauche-dbd-mysql: MySQL (5.0 or later) native driver and DBD interface module.

## PREREQUISITES

* Gauche 0.9.7 or later
* MySQL 5.0 or later
* MySQL client development library & header files

(If you're using Gauche 0.9.6 or before, see 'Using with pre-0.9.7 Gauche'
section below).

## PREPARATION

In order to run the test, you need a test user and test database.
The following mysql commands sets it up.  You can drop those
after the test is done:

    create user 'kahua_test'@'localhost' identified by 'kahua_secret';
    create database kahua_test character set 'utf8';
    grant all on kahua_test.* to 'kahua_test'@'localhost';

## BUILD, TEST and INSTALL

A quick one-liner from tarball:

    gauche-package install -S root Gauche-dbd-mysql-X.X.tgz

From source tree:

    git clone https://github.com/kahua/Gauche-dbd-mysql
    cd Gauche-dbd-mysql
    ./configure
    make
    make -s check
    sudo make install

## API Documentation

The easier way is to use it through dbi interface.
Here's an example session.

```
(use dbi)
(use gauche.sequence) ; get generic 'map' etc

(define conn (dbi-connect "dbi:mysql:DBNAME;host=HOST"
                          :username USERNAME :password PASSWORD))

(dbi-do conn "create table test (id integer, name varchar(255))")

(let1 q (dbi-prepare conn "insert into test (id, name) values (?, ?)")
  (dbi-execute q 10 "Alice")
  (dbi-execute q 20 "Bob"))

(map (lambda (row)
       (list (dbi-get-value row 0) (dbi-get-value row 1)))
     (dbi-do conn "select id, name from test"))
 ; => (("10" "Alice") ("20" "Bob"))

(let1 q (dbi-prepare conn "select name from test where id = ?")
  (coerce-to <list> (dbi-execute q 20)))
 ; => (#("Bob"))
```

The only mysql-specific part is the call to `dbi-connect`; you have to
give the name of the database (DBNAME) and hostname (HOST) as shown.
If you need to pass username and password, give them using keyword arguments.
All the rest are generic dbi operations; see
[dbi manual entry of Gauche document](http://practical-scheme.net/gauche/man/?p=dbi)

The module also provides low-level API that corresponds to
MySQL API.

    Function: mysql-dbd-version    -> string

    Class: <mysql-handle>

    Class: <mysql-res>

    Class: <mysql-row-offset>

    Function: mysql-handler? obj   -> boolean

    Function: mysql-res? obj -> boolean

    Function: mysql-row-offset? obj -> boolean

    Function: mysql-affected-rows handle -> integer

    Function: mysql-autocommit handle flag -> void

    Function: mysql-change-user handler user password db -> void

    Function: mysql-character-set-name handle -> string

    Function: mysql-close handle -> void

    Function: mysql-commit handle -> void

    Function: mysql-data-seek result offset -> void

    Function: mysql-debug stirng -> void

    Function: mysql-dump-debug-info handle -> void

    Function: mysql-errno handle -> integer

    Function: mysql-error handle -> string

    Function: mysql-fetch-row result -> vector

    Function: mysql-field-count handle -> integer

    Function: mysql-field-seek result offset -> integer

    Function: mysql-field-tell result -> integer

    Function: mysql-free-result result -> void

    Function: mysql-get-client-info -> string

    Function: mysql-get-client-version -> integer

    Function: mysql-get-host-info handle -> stirng

    Funciton: mysql-get-proto-info handle -> integer

    Function: mysql-get-server-info handle -> string

    Function: mysql-get-ssl-cipher handle -> string

    Function: mysql-hex-string string -> string

    Function: mysql-info handle -> string or #f

    Function: mysql-insert-id handle -> integer

    Function: mysql-list-dbs handle wild -> result

    Function: mysql-list-fiels handle table wild -> result

    Function: mysql-list-processes handle -> result

    ...

## Using with pre-0.9.7 Gauche

Gauche-dbd-mysql 0.4 requires Gauche-0.9.7.  If you want to
use dbd.mysql module with Gauche 0.9.6 or before (Gauche 0.8.6 or after),
you need to use Gauche-dbd-mysql 0.3.  To build 0.3 from the source repo,
check out the tag release0_3:

    git clone https://github.com/kahua/Gauche-dbd-mysql
    cd Gauche-dbd-mysql
    git checkout release0_3
    ./DIST gen
    ./configure
    make
    make -s check
    sudo make install
