#!/bin/bash

BASE_URL=https://www.data.gouv.fr
INDEX_URL=${BASE_URL}/fr/datasets/fichier-des-personnes-decedees
default_last_sync="2019-12-01"

# Temporary files
SUMMARY=summary.html
URL_LIST=url.txt
JSON=api.json

# Local files
DIR=INSEE
FILE_LAST_SYNC=last_sync.txt

mkdir -p ${DIR}

echo "Get last synchronization date..."
if [ -f ${DIR}/${FILE_LAST_SYNC} ]
then
	last_sync=$(cat ${DIR}/${FILE_LAST_SYNC})
else
	last_sync=${default_last_sync}
fi
echo "Last synchronization : $last_sync"
new_sync=$(date '+%FT%T')

echo "Get URLs list..."
wget -q "${INDEX_URL}" -O ${SUMMARY}
res=$?
if [[ ${res} != 0 ]]
then
	echo " ERROR ${res}."
	exit -1
fi

echo "Get new files..."
grep checkurl ${SUMMARY} | sed -e "s/\s*:checkurl=\"'//" -e "s#/check/'\"##" > ${URL_LIST}
for url in $(cat ${URL_LIST})
do
	wget -q "${BASE_URL}${url}" -O ${JSON}
	res=$?
	if [[ ${res} != 0 ]]
	then
		echo " ERROR ${res} for ${url}."
		exit -1
	fi

	file_latest=$(jq -r '.latest' ${JSON})
	file_last_modified=$(jq -r '.last_modified' ${JSON})
	file_title=$(jq -r '.title' ${JSON})

	if [[ "${file_title}" =~ -(m|t) ]]
	then
		echo "File ${DIR}/${file_title} is useless."
	elif [[ "${last_sync}" < "${file_last_modified}" ]]
	then
		echo "Downloading ${DIR}/${file_title}..."
		wget "${file_latest}" -O "${DIR}/${file_title}"
		res=$?
		if [[ ${res} != 0 ]]
		then
			echo " ERROR ${res} for ${file_latest} / ${file_title}."
			exit -1
		fi
	else
		echo "File ${DIR}/${file_title} is unchanged."
	fi
done

echo "Save synchronization date..."
echo ${new_sync} > ${DIR}/${FILE_LAST_SYNC}

# remove temporary files
rm ${SUMMARY} ${URL_LIST} ${JSON} > /dev/null 2>&1
