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

logon32=''
if [ ! -z "${LOGON32}" ]; then
  echo "running regression tests with --32 flag for logon"
  logon32='--32'
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

      subdir=`ls -d $grammardir/*/`
      grammar=$subdir/lkb/script
      grm_file=`ls $subdir/*-pet.grm`
      # Do the following if parsing with PET and without TSDB
      #mkdir -p $tsdbhome/$target
      #cut -d@ -f7 $skeleton/item | cheap -mrs -tsdbdump $tsdbhome/$target $grm_file 2>${TSDBLOG} >${TSDBLOG}
      # cheap makes a bad item file, so just copy over the original one
      #cp $skeleton/item $tsdbhome/$target/item
      #$matrix_cmd a $tsdbhome/$target $tsdbhome/$gold > $log

      {
          options=":error :exit :wait 300"
  
          echo "(setf (system:getenv \"DISPLAY\") nil)"
  
          # Use the following for PET parsing
          # echo "(setf *tsdb-cache-connections-p* t)"
          # echo "(setf *pvm-encoding* :utf-8)"
          # echo "(setf *pvm-cpus* (list (make-cpu"
          # echo "  :host (short-site-name)"
          # echo "  :spawn \"${LOGONROOT}/bin/cheap\""
          # echo "  :options (list \"-tsdb\" \"-packing\" \"-mrs\" \"$grm_file\")"
          # echo "  :class :$lgname :name \"$lgname\""
          # echo "  :grammar \"$lgname (current)\""
          # echo "  :encoding :utf-8"
          # echo "  :task '(:parse) :wait 300 :quantum 180)))"
          # echo "(tsdb:tsdb :cpu :$lgname :task :parse :file t)"
  
          # Use the following for LKB parsing
          echo "(lkb::read-script-file-aux \"$grammar\")"
  
          # And the following is necessary for TSDB
          echo "(setf tsdb::*process-suppress-duplicates* nil)"
          echo "(setf tsdb::*process-raw-print-trace-p* t)"
  
          echo "(setf tsdb::*tsdb-home* \"$tsdbhome\")"
          echo "(tsdb:tsdb :skeletons \"$skeletons\")"
  
          echo "(setf target \"$target\")"
          echo "(tsdb:tsdb :create target :skeleton \"$lgname\")"
          echo "(tsdb:tsdb :process target)"
  
          echo "(tsdb::compare-in-detail \"$target\" \"$gold\" :format :ascii :compare '(:readings :mrs) :append \"$log\")"
  
      } | ${LOGONROOT}/bin/logon $logon32 \
          -I base -locale no_NO.UTF-8 -qq 2> ${TSDBLOG} > ${TSDBLOG}
      
    # The $TSDBLOG is overwritten each time, so copy it to $ALLTSDBLOG
    echo "== BEGIN TSDB LOG for $lgname ==" >> $ALLTSDBLOG
    cat $TSDBLOG >> $ALLTSDBLOG

# When the grammar fails to load, [incr tsdb()] is not creating
# the directory.  So use existence of $tsdbhome/$target to check
# for grammar load problems.

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
      fi
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
if [[ $performance ]]; then
  echo "Grepping for 'error' in tsdb log:"
  echo ""

  # Don't report errors for starred items
  grep -i "error" ${ALLTSDBLOG} | grep -v "^([0-9]\+) \`\*"
fi

echo ""
echo "Results of the regression tests can be seen in"
echo "$masterlog"
