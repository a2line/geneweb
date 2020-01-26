#!/bin/bash

MYSQL=./mysql.sh

$MYSQL -N << EOF
select concat( Cle, '\n', concat_ws( '|',
	Nom, Prenom, Sexe,
	concat( '°', NaissanceD, '/', NaissanceM, '/', NaissanceY),
	NaissancePlace,
	concat( '+', DecesD, '/', DecesM, '/', DecesY),
	DecesPlace ), '\n', Msg, '\nIdInsee(', IdInsee, ') Score ', Score, '\n')
from TODO where Etat = 2
;
EOF
