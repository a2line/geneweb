#!/bin/bash

MYSQL="./mysql.sh"

echo "(Re)create table..."
$MYSQL << EOF
drop table if exists INSEE;

create table INSEE (
	Id INTEGER UNSIGNED auto_increment primary key,
	Nom VARCHAR(80) not null,
	Prenom VARCHAR(80) not null,
	Sexe CHAR(1) not null,
	NaissanceY CHAR(4) not null,
	NaissanceM CHAR(2) not null,
	NaissanceD CHAR(2) not null,
	NaissanceCode CHAR(5) not null,
	NaissanceLocalite VARCHAR(30) not null,
	NaissancePays VARCHAR(30) not null,
	DecesY CHAR(4) not null,
	DecesM CHAR(2) not null,
	DecesD CHAR(2) not null,
	DecesCode CHAR(5) not null,
	NumeroActe CHAR(9) not null,
	Fichier VARCHAR(50) not null,
        index I_INSEE_N (Nom),
        index I_INSEE_BY (NaissanceY),
        index I_INSEE_DY (DecesY),
        index I_INSEE_F (Fichier)
);
EOF

for f in $(ls INSEE/deces*txt)
do

echo "Loading $f..."
$MYSQL << EOF
load data
 local infile '$f'
 ignore
 into table INSEE
 (@row)
 set
  Id                = null,
  Nom               = substr(@row, 1, locate('*', @row)-1),
  Prenom            = substr(@row, locate('*', @row)+1, locate('/', @row)-locate('*', @row)-1),
  Sexe              = substr(@row,  81,  1),
  NaissanceY        = substr(@row,  82,  4),
  NaissanceM        = substr(@row,  86,  2),
  NaissanceD        = substr(@row,  88,  2),
  NaissanceCode     = substr(@row,  90,  5),
  NaissanceLocalite = rtrim(substr(@row,  95, 30)),
  NaissancePays     = rtrim(substr(@row, 125, 30)),
  DecesY            = substr(@row, 155,  4),
  DecesM            = substr(@row, 159,  2),
  DecesD            = substr(@row, 161,  2),
  DecesCode         = substr(@row, 163,  5),
  NumeroActe        = substr(@row, 168,  9),
  Fichier           = '$f'
;
EOF

done
