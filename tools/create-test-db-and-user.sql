create user 'kahua_test'@'localhost' identified by 'kahua_secret';
create database kahua_test default character set 'utf8';
grant all on kahua_test.* to 'kahua_test'@'localhost';
