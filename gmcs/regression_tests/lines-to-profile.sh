#!/bin/bash

## a shell script to turn raw lines of text into a skeleton TSDB profile on which other operations can run

mkdir -p $1
cp ${LOGONROOT}/lingo/lkb/src/tsdb/skeletons/english/Relations $1/relations
touch $1/item-set
touch $1/run
touch $1/parse
touch $1/result
touch $1/edge
touch $1/decision
touch $1/preference
touch $1/tree

iid=1
{
	while read L ; do
		W=`echo $L | wc -w`
		if [ $W != '0' ]; then
			echo ${iid}@@@@@@$L@@@@@$W@@@
			iid=$((1+$iid))
		fi
	done
} > $1/item
