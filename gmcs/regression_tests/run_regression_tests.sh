#!/bin/bash

# Script for running all existing matrix regression tests.
# Be sure that $CUSTOMIZATIONROOT is set appropriately
# (i.e., to point to the matrix/gmcs directory you
# intend to test... the regression_tests directory with that
# gmcs/directory is the one that will be active).
# Much copied from logon/parse.

unset DISPLAY
unset LUI

###
### INITIALIZATION CHECKS
###

if [ -z "${LOGONROOT}" ]; then
  echo "run-regression-tests: unable to determine \$LOGONROOT directory; exit."
  exit 1
fi

if [ -z "${CUSTOMIZATIONROOT}" ]; then
  echo "run-regression-tests: unable to determine \$CUSTOMIZATIONROOT directory; exit."
  exit 1
fi

if ! hash ace 2>/dev/null; then
  echo "run-regression-tests: no ace command in \$PATH, checking \$ACEROOT..."
  if [ -z "${ACEROOT}" ]; then
    echo "run-regression-tests: unable to determine \$ACEROOT directory; exit."
    exit 1
  else
    ACECMD="$ACEROOT/ace"
  fi
else
  ACECMD="ace"
fi

logon32=''
if [ ! -z "${LOGON32}" ]; then
  echo "The regression test with ACE is not currently possible on a 32-bit machine; exit."
  exit 1
fi

# set the appropriate Python version
python_cmd='python'
if ! echo $( $python_cmd -V 2>&1 ) | grep -q "Python 2\.[5,6,7]"; then
  echo "Default Python version incompatible. Attempting to find another..." >&2
  if which python2.5 >/dev/null; then
    python_cmd='python2.5'
  elif which python2.6 >/dev/null; then
    python_cmd='python2.6'
  elif which python2.7 >/dev/null; then
    python_cmd='python2.7'
  else
    echo "No compatible Python version found. Exiting."
    exit 1
  fi
  echo "  Found $( $python_cmd -V 2>&1 ). Continuing." >&2
fi

# for convenience
matrix_cmd="$python_cmd ${CUSTOMIZATIONROOT}/../matrix.py"

###
### COMMON VARIABLES AND SETTINGS
###

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

# Parameters which are the same for all regression test:
rtestdir="${CUSTOMIZATIONROOT}/regression_tests/"
skeletons="$rtestdir/skeletons/"
choices="$rtestdir/choices/"
grammars="$rtestdir/grammars"
tsdbhome="$rtestdir/home/"
logs="$rtestdir/logs/"

### LOG FILES

# Log file to look at tsdb output.
TSDBLOG="$logs/tsdb.${date}.log"
if [ -e ${TSDBLOG} ]; then
    rm ${TSDBLOG}
fi
# We need to concatenate all TSDBLOGs together because they get overwritten
ALLTSDBLOG="$logs/alltsdb.${date}.log"
if [ -e ${ALLTSDBLOG} ]; then
    rm ${ALLTSDBLOG}
fi

# Create one log file with results from all tests, appending on 
# comments.
# 2008-08-22: By request not overwriting the log file
# but appending instead, with time stamps.
masterlog="$logs/regression-tests.$date"
echo "============ $datetime ============" >> $masterlog

###
### Tasks
###

while getopts "vcp" Option
do
  case $Option in
    v ) validate=true;;
    c ) customize=true;;
    p ) performance=true;;
  esac
done
# if none were set, do all tasks
if ! [[ $validate || $customize || $performance ]]
then
  validate=true
  customize=true
  performance=true
fi
# Now move the argument pointer to the first argument
shift $((OPTIND-1))

###
### TEST PREPARATION
###

# Get the list of regression tests from the regression-test-index:
# or from command-line input.
if [ -z $1 ]; then
    lgnames=`$python_cmd ${CUSTOMIZATIONROOT}/regression_tests/regressiontestindex.py --lg-names`
    if [ $? != 0 ]; then
    echo "run-regression-tests: Problem with regression-test-index, cannot run regression tests."
    exit 1
    fi
else 
    lgnames=$@
fi

# Clear any existing regression test files that can cause conflicts
for lgname in $lgnames
do
    rm -rf $grammars/$lgname
    rm -f $logs/$lgname.$date
    rm -rf $tsdbhome/current/$lgname
done

###
### TSDB GOLD STANDARD COMPARISONS
###

# Now do essentially the same things as one-regression-test for each one:

# comparison without restarting tsdb++
profiles=""
comparison=""

echo "== All grammars are being created. =="      

for lgname in $lgnames
do
    printf "%-70s " "$lgname..."

    # Set skeleton, grammar, gold-standard for comparison, and
    # target directory.
    skeleton="$skeletons/$lgname"
    gold="gold/$lgname"
    choicesfile="$choices/$lgname"
    grammardir="$grammars/$lgname"
    target="current/$lgname"
    log="$logs/$lgname.$date"

    # Check for existence of choices file
    if [ ! -e $choicesfile ]; then
      echo "ERROR!"
      echo "$lgname choices file does not exist: $choicesfile" >> $log
      continue
    fi

    # Validate
    if [[ $validate ]]; then
      $matrix_cmd v $choicesfile >> $log
      if [ $? != 0 ]; then
        echo "INVALID!"
        echo "$lgname choices file did not pass validation." >> $log
        continue
      fi
    fi

    # Customize (Performance needs a grammar, too, though)
    if [[ $customize || $performance ]]; then
      $matrix_cmd --cheap-hack cf $choicesfile $grammardir >> $log
      if [[ $customize && $? != 0 ]]; then
        echo "FAIL!"
        echo "There was an error during the customization of the grammar." >> $log
        continue
      fi
    fi

    # Parsing Performance
    if [[ $performance ]]; then
      # Check for existence of gold profile
      if [ ! -e $tsdbhome/$gold ]; then
        echo "ERROR!"
        echo "Gold profile does not exist: $tsdbhome/$gold" >> $log
        continue
      fi
    fi
    
    subdir=`ls -d $grammardir/*/`
    dat_file=$subdir/${lgname}.dat
    config_file=$subdir/ace/config.tdl
    $ACECMD -G $dat_file -g $config_file 1>/dev/null 2>/dev/null

    #echo "Running ACE with $lgname..."

    mkdir -p $tsdbhome/$target
    cp ${LOGONROOT}/lingo/lkb/src/tsdb/skeletons/english/Relations $tsdbhome/$target/relations
    touch $tsdbhome/$target/item-set
    touch $tsdbhome/$target/run
    touch $tsdbhome/$target/parse
    touch $tsdbhome/$target/result
    touch $tsdbhome/$target/edge
    touch $tsdbhome/$target/decision
    touch $tsdbhome/$target/preference
    touch $tsdbhome/$target/tree
    cp $skeleton/item $tsdbhome/$target/item
    cut -d@ -f7 $skeleton/item | ${CUSTOMIZATIONROOT}/regression_tests/art-static-prerelease -a "$ACECMD -g $dat_file 2>/dev/null" $tsdbhome/$target 1>/dev/null
    echo "DONE"	

    #echo "Working on ACE is done!!!"
    sed "s;@@;@0@;g" $tsdbhome/$target/parse > $tsdbhome/$target/tmp
    mv -f $tsdbhome/$target/tmp $tsdbhome/$target/parse
    sed "s;@@;@0@;g" $tsdbhome/$target/parse > $tsdbhome/$target/tmp
    mv -f $tsdbhome/$target/tmp $tsdbhome/$target/parse
    sed "s;@@;@0@;g" $tsdbhome/$target/result > $tsdbhome/$target/tmp
    mv -f $tsdbhome/$target/tmp $tsdbhome/$target/result
    sed "s;@@;@0@;g" $tsdbhome/$target/result > $tsdbhome/$target/tmp
    mv -f $tsdbhome/$target/tmp $tsdbhome/$target/result

    #$python_cmd ${CUSTOMIZATIONROOT}/regression_tests/cleanup_parse.py < $tsdbhome/$target/parse > $tsdbhome/$target/tmp
    #mv -f $tsdbhome/$target/tmp $tsdbhome/$target/parse

    echo "=== Readings-Compare ===" >> $log
    profiles+="(tsdb:tsdb :create \"$target\" :skeleton \"$lgname\")"
    comparison+="(tsdb::compare-in-detail \"$target\" \"$gold\" :format :ascii :compare '(:readings) :append \"$log\")"
done


echo "== All profiles are being compared to the gold standards. =="      

{
  options=":error :exit :wait 300"
  echo "(setf (system:getenv \"DISPLAY\") nil)"

  echo "(setf tsdb::*process-suppress-duplicates* nil)"
  echo "(setf tsdb::*process-raw-print-trace-p* t)"

  echo "(setf tsdb::*tsdb-home* \"$tsdbhome\")"
  echo "(tsdb:tsdb :skeletons \"$skeletons\")"

  echo "$profiles"
  echo "$comparison"

} | ${LOGONROOT}/bin/logon -I base -locale no_NO.UTF-8 -qq 2> ${TSDBLOG} > ${TSDBLOG}

# The $TSDBLOG is overwritten each time, so copy it to $ALLTSDBLOG
echo "== BEGIN TSDB LOG for $lgname ==" >> $ALLTSDBLOG
cat $TSDBLOG >> $ALLTSDBLOG


# checking out the results of comparion
for lgname in $lgnames
do
# When the grammar fails to load, [incr tsdb()] is not creating
# the directory.  So use existence of $tsdbhome/$target to check
# for grammar load problems.
    printf "%-70s " "$lgname..."
    log="$logs/$lgname.$date"

    gold="${CUSTOMIZATIONROOT}/regression_tests/home/gold/$lgname"
    target="${CUSTOMIZATIONROOT}/regression_tests/home/current/$lgname"
    echo "" >> $log
    echo "" >> $log
    echo "=== MRS-Compare ===" >> $log
    echo "" >> $log
    cp $gold/result $gold/result.tmp
    cp $gold/parse $gold/parse.tmp 
    cp $gold/item $gold/item.tmp  
    cp $target/result $target/result.tmp
    cp $target/parse $target/parse.tmp 
    cp $target/item $target/item.tmp  
    $python_cmd ${CUSTOMIZATIONROOT}/regression_tests/multiple-mrs.py $gold
    $python_cmd ${CUSTOMIZATIONROOT}/regression_tests/multiple-mrs.py $target
    ${CUSTOMIZATIONROOT}/regression_tests/mrs-compare $target $gold >> $log
    mv $gold/result.tmp $gold/result
    mv $gold/parse.tmp $gold/parse
    mv $gold/item.tmp $gold/item
    mv $target/result.tmp $target/result
    mv $target/parse.tmp $target/parse
    mv $target/item.tmp $target/item

    target="current/$lgname"
    
    if [ ! -e $tsdbhome/$target ]
    then
      echo "ERROR!"
      echo "Probable tdl error; grammar failed to load." >> $log
      continue
    # newer versions of [incr tsdb()] write that there were 0 diffs, so
    # the file is no longer empty for success
    elif [ -n "$(grep -i "error" ${TSDBLOG} | grep -v "^([0-9]\+) \`\*")" ]
    then
      echo "ERROR!"
      echo "TSDB error; check ${TSDBLOG}" >> $log
      continue
    elif [ -z "$(grep "compare-in-detail(): 0 differences" $log)" ]
    then
      echo "DIFFS!"
      echo "Diffs were found in the current and gold profiles." >> $log
      continue
    elif [ "$(grep "^item [0-9]*:" $log)" ]
    then
      echo "DIFFS!"
      echo "Diffs were found in the current and gold profiles." >> $log
      continue
    fi

    # if we made it here, it's probably a success
    echo "Success!"
done

# Check through tsdb log file for any errors, and report
# whether the results can be considered valid.

for lgname in $lgnames
do
    log="$logs/$lgname.$date"
    echo -ne "$lgname" >> $masterlog
    if [ -s $log ]; then
        echo -ne ": " >> $masterlog
        $python_cmd ${CUSTOMIZATIONROOT}/regression_tests/regressiontestindex.py --comment $lgname | cat >> $masterlog
        echo "" >> $masterlog
        cat $log >> $masterlog
        echo "" >> $masterlog
    else
        echo "... Success!" >> $masterlog
    fi
done

# Notify user of results:
#if [[ $performance ]]; then
#  echo "Grepping for 'error' in tsdb log:"
#  echo ""

  # Don't report errors for starred items
#  grep -i "error" ${ALLTSDBLOG} | grep -v "^([0-9]\+) \`\*"
#fi

echo ""
echo "Results of the regression tests can be seen in"
echo "$masterlog"
