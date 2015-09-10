#!/bin/bash -e
# Control interface for transcode jobs.
# This is run directly from the application, on the webserver.
# It calls transcode, possibly on transcode.host.

escape() {
	for a in "$@" ; do
		echo \'${a//\'/\'\\\'\'}\'
	done
}

cmd=`dirname $0`/transcode

if [[ ! -f $cmd ]] ; then
	echo "$cmd: not found" >&2
	exit 2
fi

while getopts 'i:h:d:v:c:k:s:r:f:t' opt ; do case "$opt" in
	i) id=$OPTARG ;;
	h) host=$OPTARG ;;
	d) dir=$OPTARG ;;
	v) version=$OPTARG ;;

	c) collect=$OPTARG ;;
	k) kill=$OPTARG ;;
	s) src=$OPTARG ;;
	r) url=$OPTARG ;;
	f) fmt=$OPTARG ;;
	t) test=1 ;;

	?) exit 1 ;;
esac ; done

hcmd=./transcode${version:+-$version}

if [[ -n $test ]] ; then
	if [[ -z $dir ]] ; then
		false
	elif [[ -n $host ]] ; then
		ssh "$host" test -d "$dir"
		rsync -p "$cmd" "$host:$hcmd"
	else
		test -d "$dir"
	fi
	exit $?
fi

if [[ -z $id || -z $dir || -z $collect$kill && ( -z $src || -z $url || -z $fmt ) ]] ; then
	echo "$0: usage error: $*" >&2
	exit 1
fi

if [[ -n $collect ]] ; then
	if [[ -n $host ]] ; then
		rsync "$host:$dir/$id.$fmt" "$collect"
		ssh "$host" rm -f "$dir/$id" "$dir/$id.$fmt"
	else
		mv "$dir/$id.$fmt" "$collect"
		rm -f "$dir/$id"
	fi
elif [[ -n $host ]] ; then
	if [[ -z $kill ]] ; then
		rsync "$src" "$host:$dir/$id"
	fi
	ssh "$host" "$hcmd" `escape "$@"` | sed 's/^\([0-9]\+\)\.[.a-z0-9-]*$/\1/'
elif [[ -n $kill ]] ; then
	"$cmd" "$@"
else
	ln -fT "$src" "$dir/$id"
	"$cmd" "$@" &
	echo $!
fi
