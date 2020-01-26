#!/bin/bash

DIR=COG

# INSSE COG root : https://www.insee.fr/fr/information/2560452
COG_URL="https://www.insee.fr/fr/statistiques/fichier/3720946/cog_ensemble_2019_csv.zip"

# Autre source pour les pays car les libellés ne sont pas satisfaisant dans le fichier INSEE
PAYS_URL="https://sql.sh/ressources/sql-pays/sql-pays.csv"

mkdir -p ${DIR}

if [ ! -f ${DIR}/COG.zip ]
then
	wget "${COG_URL}" -O ${DIR}/COG.zip
fi
unzip -qc ${DIR}/COG.zip pays2019.csv | sed 's/$//' > ${DIR}/COG-pays.csv
unzip -qc ${DIR}/COG.zip departement2019.csv | sed 's/$//' > ${DIR}/COG-departement.csv
unzip -qc ${DIR}/COG.zip communes-01042019.csv | sed 's/$//' > ${DIR}/COG-commune.csv
unzip -qc ${DIR}/COG.zip mvtcommune-01042019.csv | sed 's/$//' > ${DIR}/COG-mvt.csv

if [ ! -f ${DIR}/sql-pays.csv ]
then
	wget "${PAYS_URL}" -O ${DIR}/sql-pays.csv
fi
