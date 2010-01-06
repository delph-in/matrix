#!/bin/bash

# Script for running all existing matrix unit tests.
# Be sure that $CUSTOMIZATIONROOT is set appropriately
# (i.e., to point to the matrix/customize directory you
# intend to test... the unit-tests directory with that
# customize/directory is the one that will be active).
# Much copied from logon/parse.

unset DISPLAY
unset LUI

if [ -z "${LOGONROOT}" ]; then
  echo "run-unit-tests: unable to determine \$LOGONROOT directory; exit."
  exit 1
fi

if [ -z "${CUSTOMIZATIONROOT}" ]; then
  echo "run-unit-tests: unable to determine \$CUSTOMIZATIONROOT directory; exit."
  exit 1
fi

# Check if there are any grammars in the way, and if so, exit.

#
# include a shared set of shell functions and global parameters, including the
# architecture identifier .LOGONOS.
#
. ${LOGONROOT}/etc/library.bash

date=$(date "+%Y-%m-%d")
datetime=$(date)
count=1
limit=1000
best=1000

# Main log file to look at tsdb output.

TSDBLOG="${CUSTOMIZATIONROOT}/unit-tests/logs/tsdb.${date}.log"

if [ -e ${TSDBLOG} ]; then
    rm ${TSDBLOG}
fi

# Paraeters which are the same for all unit test:

skeletons="${CUSTOMIZATIONROOT}/unit-tests/skeletons/"
tsdbhome="${CUSTOMIZATIONROOT}/unit-tests/home"
logdir="${CUSTOMIZATIONROOT}/unit-tests/logs"

# Get the list of unit tests from the unit-test-index:
# or from command-line input.

if [ -z $1 ]; then

    lgnames=`python unittestindex.py --lg-names`

    if [ $? != 0 ]; then
	echo "run-unit-tests: Problem with unit-test-index, cannot run unit tests."
	exit
    fi

    echo "Removing old grammars"
    ./cleanup-unit-tests.sh
else 
    lgnames=$1
fi


# Create fresh copy of matrix-core

rm -rf ${CUSTOMIZATIONROOT}/matrix-core
pushd ${CUSTOMIZATIONROOT} >/dev/null
./install -c matrix-core >/dev/null
popd >/dev/null

# Now do essentially the same things as one-unit-test for each one:

for lgname in $lgnames
do 

    printf "%-70s " "$lgname..."

# Set skeleton, grammar, gold-standard for comparison, and
# target directory.

    skeleton="${CUSTOMIZATIONROOT}/unit-tests/skeletons/$lgname"
    gold="gold/$lgname"
    grammardir="${CUSTOMIZATIONROOT}/unit-tests/grammars/$lgname"
    target="current/$lgname"
    log="$logdir/$lgname.$date"

# Invoke customize.py

    ${CUSTOMIZATIONROOT}/unit-tests/call-customize ${CUSTOMIZATIONROOT} ${CUSTOMIZATIONROOT}/unit-tests/choices/$lgname ${CUSTOMIZATIONROOT}/unit-tests/grammars/$lgname

# If you're Scott, you need to uncomment these lines to run the tests
#   rm -rf /tmp/grammar
#   cp -R $grammardir /tmp/grammar
#   rm -rf $grammardir
#   grammardir=/tmp/grammar

# Have to calculate after the grammar is created since the directory is
# no longer always named "matrix")
    pushd $grammardir >/dev/null
    subdir=`echo * | sed 's/choices//g' | sed 's/ //g'`
    popd >/dev/null
    grammar="$grammardir"/"$subdir"/lkb/script

    status=$?

    if [ $status = 17 ]; then
	echo "run-unit-tests: call-customize failed for $lgname... continuining with other unit tests."
	echo "call-customize failed because old grammar was in the way." >> $log
    elif [ $status = 18 ]; then
	echo "run-unit-tests: call-customize failed for $lgname... continuining with other unit tests."
	echo "no choices file at path unit-tests/choices/$lgname." >> $log
    elif [ $status != 0 ]; then

	    echo "run-unit-tests: customization failed for $lgname... continuing with other unit tests."; 
	    echo "Customization failed; no grammar created." >> $log

    else

# Set up a bunch of lisp commands then pipe them to logon/[incr tsdb()]

# I don't see how the following can possibly do anything,
# since nothing has yet invoked [incr tsdb()] or lisp.
# But let's give it a try anyway...

	{
	    options=":error :exit :wait 300"

	    echo "(setf (system:getenv \"DISPLAY\") nil)"

	    echo "(setf tsdb::*process-suppress-duplicates* nil)"
	    echo "(setf tsdb::*process-raw-print-trace-p* t)"

	    echo "(setf tsdb::*tsdb-home* \"$tsdbhome\")"
	    echo "(tsdb:tsdb :skeletons \"$skeletons\")"

	    echo "(lkb::read-script-file-aux \"$grammar\")"

	    echo "(setf target \"$target\")"
	    echo "(tsdb:tsdb :create target :skeleton \"$lgname\")"
	
	    echo "(tsdb:tsdb :process target)"

	    echo "(tsdb::compare-in-detail \"$target\" \"$gold\" :format :ascii :compare '(:readings :mrs) :append \"$log\")"

	} | ${LOGONROOT}/bin/logon ${source} ${cat} \
	    -I base -locale no_NO.UTF-8 -qq 2> ${TSDBLOG} > ${TSDBLOG}

# FIXME: There is probably a more appropriate set of options to
# send to logon, but it seems to work fine as is for now. 

	rm -rf $grammardir

# When the grammar fails to load, [incr tsdb()] is not creating
# the directory.  So use existence of $tsdbhome/$target to check
# for grammar load problems.

	if [ -e $tsdbhome/$target ]; then
	    rm -rf "$tsdbhome/$target"
	else
	    echo "Probable tdl error; grammar failed to load." >> $log
	fi
	   
	
	if [ -s $log ]; then
	    echo "DIFFS!"
	else
	    echo "Success!"
	fi
    fi
done

# Check through tsdb log file for any errors, and report
# whether the results can be considered valid.

# Create one log file with results from all tests, appending on 
# comments.

# 2008-08-22: By request not overwriting the log file
# but appending instead, with time stamps.

masterlog="$logdir/unit-tests.$date"

#if [ -e $masterlog ]; then
#    rm $masterlog
#fi

echo "============ $datetime ============" >> $masterlog

for lgname in $lgnames
do
    log="$logdir/$lgname.$date"
    echo -ne "$lgname" >> $masterlog
    if [ -s $log ]; then
	echo -ne ": " >> $masterlog
	python unittestindex.py --comment $lgname | cat >> $masterlog
	echo "" >> $masterlog
	cat $log >> $masterlog
	echo "" >> $masterlog
    else
	echo "... Success!" >> $masterlog
    fi
    rm -f $log
done

# Notify user of results:

echo "Grepping for 'error' in tsdb log:"
echo ""

grep -i "error" ${TSDBLOG}

echo ""
echo "Results of the unit tests can be seen in"
echo "$masterlog"
