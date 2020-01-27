#!/bin/bash

MYSQL=./mysql.sh

liste=$(sed -e 's/^/"/' -e 's/$/"/' liste_crochets.txt | tr '\n' ',' | sed 's/,$//')

$MYSQL -tvv << EOF
update PlaceNorme
set Libelle = concat( '[',
 substring(Libelle,1,locate(',',Libelle)-1), ']',
 substring(Libelle,locate(',',Libelle)) )
where Code in ($liste)
  and DateFin < '2049-01-01'
  and substring(Libelle,1,1) <> '[';
EOF
