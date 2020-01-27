#!/bin/bash

MYSQL="./mysql.sh"
pattern='^INSEE/deces-[[:digit:]]{4}\.txt$'

if [[ "$1" =~ $pattern ]]
then
	f=$1
else
	echo "Usage: $0 INSEE/deces-<annee>.txt"
	exit -1
fi

if [ ! -f "$f" ]
then
	echo "ERROR: File $f not readable."
	exit -1
fi

echo "Remove old data (previous file load)..."
$MYSQL << EOF
delete from INSEE where Fichier = '$f';
EOF

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
