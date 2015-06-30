#!/bin/bash -e
set -o pipefail

########################################
# Some common boilerplate for loading conf
########################################
top=`git rev-parse --show-cdup || echo "Assuming $PWD is top." >&2`
schema=${top}schema

getdbconfig () {
	# This isn't perfect but hopefully good enough
	sed -n '/^db\s*{/,/^}/{/^\s*\([a-z]\+\)\s*=\s*/{s//\1 /;s/^\([a-z]\+ \)"\(.*\)"$/\1\2/;s/\$(USER)/'"$USER"'/;p}}' ${top}databrary.conf ${top}local.conf
}


getsolrconfig () {
	# This isn't perfect but hopefully good enough
	sed -n '/^solr\s*{/,/^}/{/^\s*\([a-z]\+\)\s*=\s*/{s//\1 /;s/^\([a-z]\+ \)"\(.*\)"$/\1\2/;s/\$(USER)/'"$USER"'/;p}}' ${top}databrary.conf ${top}local.conf
}

declare -A db=([port]=5432)
while read key val ; do
	db[$key]=$val
done < <(getdbconfig)


declare -A solr=([port]=8983)
while read key val ; do
	solr[$key]=$val
done < <(getsolrconfig)

if [[ -z ${db[db]} || -z ${db[user]} ]] ; then
	echo "Could not read database configuration."
	exit 1
fi

if [[ -n ${db[pass]} && ! -f ~/.pgpass ]] ; then
	um=`umask -p`
	umask 077
	echo "${db[host]:-localhost}:${db[port]}:${db[db]}:${db[user]}:${db[pass]}" >> ~/.pgpass
	$um
fi

declare -a args
if [[ -n ${db[host]} ]] ; then
	args=(-h ${db[host]} -p ${db[port]})
elif [[ -n ${db[sock]} ]] ; then
	args=(-h ${db[sock]%/.s.PGSQL.*})
fi
args=("${args[@]}" -U ${db[user]})
while [[ $1 = -?* ]] ; do
	args=("${args[@]}" $1)
	shift
done


