#!/bin/bash

# Script for running all existing matrix unit tests.
# Be sure that $CUSTOMIZATIONROOT is set appropriately
# (i.e., to point to the matrix/customize directory you
# intend to test... the unit-tests directory with that
# customize/directory is the one that will be active).
# Much copied from logon/parse.

unset DISPLAY;
unset LUI;

if [ -z "${LOGONROOT}" ]; then
  echo "one-unit-test: unable to determine \$LOGONROOT directory; exit.";
  exit 1;
fi

if [ -z "${CUSTOMIZATIONROOT}" ]; then
  echo "one-unit-test: unable to determine \$CUSTOMIZATIONROOT directory; exit.";
  exit 1;
fi

# Check if there are any grammars in the way, and if so, exit.

#
# include a shared set of shell functions and global parameters, including the
# architecture identifier .LOGONOS.
#
. ${LOGONROOT}/etc/library.bash;

date=$(date "+%Y-%m-%d");

count=1;
limit=1000;
best=1000;

# Main log file to look at tsdb output.

LOG="${CUSTOMIZATIONROOT}/unit-tests/logs/tsdb.${date}.log";

if [ -e ${LOG} ]; then
    rm ${LOG};
fi

# Paraeters which are the same for all unit test:

skeletons="${CUSTOMIZATIONROOT}/unit-tests/skeletons/"
tsdbhome="${CUSTOMIZATIONROOT}/unit-tests/home/"
logdir="${CUSTOMIZATIONROOT}/unit-tests/logs"

# Get the list of unit tests from the unit-test-index:

lgnames=`python unittestindex.py --lg-names`

# Now do essentially the same things as one-unit-test for each one:

for lgname in $lgnames;
do 

    echo -ne "$lgname...";

# Set skeleton, grammar, gold-standard for comparison, and
# target directory.

    skeleton="${CUSTOMIZATIONROOT}/unit-tests/skeletons/$lgname"
    gold="gold/$lgname"
    grammardir="${CUSTOMIZATIONROOT}/unit-tests/grammars/$lgname"
    grammar="$grammardir/matrix/lkb/script"
    target="current/$lgname"
    log="$logdir/$lgname.$date"

# Invoke customize.py

    ${CUSTOMIZATIONROOT}/unit-tests/call-customize ${CUSTOMIZATIONROOT} ${CUSTOMIZATIONROOT}/unit-tests/choices/$lgname ${CUSTOMIZATIONROOT}/unit-tests/grammars/$lgname


# Set up a bunch of lisp commands then pipe them to logon/[incr tsdb()]

# I don't see how the following can possibly do anything,
# since nothing has yet invoked [incr tsdb()] or lisp.
# But let's give it a try anyway...

    {
	options=":error :exit :wait 300";

	echo "(setf (system:getenv \"DISPLAY\") nil)";

	echo "(setf tsdb::*process-suppress-duplicates* nil)";
	echo "(setf tsdb::*process-raw-print-trace-p* t)";

	echo "(setf tsdb::*tsdb-home* \"$tsdbhome\")";
	echo "(tsdb:tsdb :skeletons \"$skeletons\")";

	echo "(lkb::read-script-file-aux \"$grammar\")";

	echo "(setf target \"$target\")";
	echo "(tsdb:tsdb :create target :skeleton \"$lgname\")";
	
	echo "(tsdb:tsdb :process target)";

	echo "(tsdb::compare-in-detail \"$target\" \"$gold\" :format :ascii :compare '(:readings :mrs) :append \"$log\")";

    } | ${LOGONROOT}/bin/logon ${source} ${cat} \
	-I base -locale no_NO.UTF-8 -qq 2> ${LOG} > ${LOG}

# FIXME: There is probably a more appropriate set of options to
# send to logon, but it seems to work fine as is for now. 

    rm -r $grammardir
    rm -r "$tsdbhome/$target"

    if [ -s $log ]; then
	echo " DIFFS! See log file";
    else
	echo " Success!";
    fi

done

# Check through tsdb log file for any errors, and report
# whether the results can be considered valid.

# Create one log file with results from all tests, appending on 
# comments.

masterlog="$logdir/unit-tests.$date"

if [ -e $masterlog ]; then
    rm $masterlog;
fi

for lgname in $lgnames;
do
    log="$logdir/$lgname.$date"
    if [ -e $log ]; then
	echo "$lgname" >> $masterlog;
	python unittestindex.py --comment $lgname | cat >> $masterlog;
	cat $log >> $masterlog;
	rm $log
    fi
done

# Notify user of results:

echo "Now we check for errors in the [incr tsdb()] log.";
echo "If you see any lines with 'error' here, the test run was invalid.";
echo "";

grep -i "error" ${LOG};

echo "";
echo "Results of the unit tests can be seen in";
echo "$masterlog";



