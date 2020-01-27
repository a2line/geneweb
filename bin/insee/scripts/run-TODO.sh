#!/bin/bash

EXE=A_RENSEGNER/geneweb/_build/default/bin/contrib/insee/insee.exe
BDIR=A_RENGEISGNER
BASE=A_RENSEIGNER

MYSQL=./mysql.sh

echo "Get date from GeneWeb..."
cd $BDIR
$EXE $BASE > $OLDPWD/TODO.lst
cd -

echo "(Re)create table..."
$MYSQL << EOF
drop table if exists TODO;

create table TODO (
	Id INTEGER UNSIGNED auto_increment primary key,
	Nom VARCHAR(80) not null,
	Prenom VARCHAR(80) not null,
	Sexe CHAR(1) not null,
	NaissanceY CHAR(4) not null,
	NaissanceM CHAR(2) not null,
	NaissanceD CHAR(2) not null,
	NaissancePlace VARCHAR(500),
	DecesY CHAR(4) not null,
	DecesM CHAR(2) not null,
	DecesD CHAR(2) not null,
	DecesPlace VARCHAR(500),
	Cle VARCHAR(100) not null,
	Etat INTEGER not null default 0,
	NbMatch INTEGER,
	Score INTEGER,
	IdInsee INTEGER UNSIGNED,
	Msg VARCHAR(1000)
);
EOF

echo "Loading TODO..."
$MYSQL << EOF
load data
 local infile 'TODO.lst'
 ignore
 into table TODO
 character set utf8
 fields terminated by '|'
 (Nom, Prenom, Sexe, NaissanceD, NaissanceM, NaissanceY, NaissancePlace, DecesD, DecesM, DecesY, DecesPlace, Cle)
 set
  Id = null
;
EOF

echo "Comparing TODO -> INSEE..."
$MYSQL -N << EOF
call processTodo();
EOF

./rapportFormate.sh > RESULT.txt
echo "Bilan disponisble dans le fichier RESULT.txt"
