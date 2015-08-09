########################################
# Some common boilerplate for loading conf
########################################

top=`git rev-parse --show-cdup || echo "Assuming $PWD is top." >&2`

getconfig () {
	# This isn't perfect but hopefully good enough
	sed -n '/^'"$1"'\s*{/,/^}/{/^\s*\([a-z]\+\)\s*=\s*/{s//\1 /;s/^\([a-z]\+ \)"\(.*\)"$/\1\2/;s/\$(USER)/'"$USER"'/;p}}' ${top}databrary.conf
}

declare -A db=([port]=5432)
while read key val ; do
	db[$key]=$val
done < <(getconfig db)

declare -A solr
while read key val ; do
	solr[$key]=$val
done < <(getconfig solr)
