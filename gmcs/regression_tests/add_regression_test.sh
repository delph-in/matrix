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
  echo "add-regression-tests: unable to determine \$LOGONROOT directory; exit."
  exit 1
fi

if [ -z "${CUSTOMIZATIONROOT}" ]; then
  echo "add-regression-tests: unable to determine \$CUSTOMIZATIONROOT directory; exit."
  exit 1
fi

if [ -z "${ACEROOT}" ]; then
  echo "add-regression-tests: unable to determine \$ACEROOT directory; exit."
  exit 1
fi

if [ ! -d "${ACEROOT}" ]; then
  echo "add-regression-tests: the \$ACEROOT does not point to the directory; exit."
  exit 1
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

matrix_cmd="$python_cmd ${CUSTOMIZATIONROOT}/../matrix.py"

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


# Parameters which are the same for all regression test:
rtestdir="${CUSTOMIZATIONROOT}/regression_tests/"
gold="$rtestdir/home/gold/"
scratch="$rtestdir/scratch/"
log="$rtestdir/logs/ace.$(date "+%Y-%m-%d")" # TJT 2014-09-12: Specify the log
grammar="$scratch/grammars"
lgname=$3

# Make grammar directories # TJT 2014-09-12: Make sure the grammar directory exists
mkdir -p $grammar

# Check for existence of choices file
if [ ! -e $1 ]; then
  echo "ERROR!"
  echo "$lgname choices file does not exist: $choicesfile" >> $log
  exit 1
fi

# Validate
if [[ $validate ]]; then
  $matrix_cmd v $1
  if [ $? != 0 ]; then
    echo "INVALID!"
    echo "$lgname choices file did not pass validation." >> $log
    exit 1
  fi
fi

# Customize (Performance needs a grammar, too, though)
if [[ $customize || $performance ]]; then
  $matrix_cmd --cheap-hack cf $1 $grammar/${lgname}
  if [[ $customize && $? != 0 ]]; then
    echo "FAIL!"
    echo "There was an error during the customization of the grammar." >> $log
    exit 1
  fi
fi

dat_file=$grammar/${lgname}/${lgname}.dat
config_file=$grammar/${lgname}/ace/config.tdl

$ACEROOT/ace -G $dat_file -g $config_file 1>/dev/null 2>/dev/null
# TJT 2014-09-12: Check to see if ACE succeeded in making the grammar
if [[ ! -f $dat_file ]]; then
  echo "FAIL!"
  echo "There was an error creating the grammar with ACE." >> $log
  exit 1
fi

mkdir -p $gold/$lgname
cp ${LOGONROOT}/lingo/lkb/src/tsdb/skeletons/english/Relations $gold/$lgname/relations
touch $gold/$lgname/item-set
touch $gold/$lgname/run
touch $gold/$lgname/parse
touch $gold/$lgname/result
touch $gold/$lgname/edge
touch $gold/$lgname/decision
touch $gold/$lgname/preference
touch $gold/$lgname/tree

#_FIX_ME_: the format of items could be wrong.
cnt=0
while read line           
do
  let "cnt += 1"
  if [ "${line:0:1}" = "*" ]; then
    sen=`echo $line| cut -d* -f2`
    echo "$cnt@unknown@formal@none@1@@$sen@@@@@@@$(whoami)@$(date "+%Y-%m-%d")"
  else
    echo "$cnt@unknown@formal@none@1@S@$line@@@@@@@$(whoami)@$(date "+%Y-%m-%d")"
  fi
done < $2 > $gold/$lgname/item

cut -d@ -f7 $gold/$lgname/item | ${CUSTOMIZATIONROOT}/regression_tests/art-static-prerelease -a "$ACEROOT/ace -g $dat_file 2>/dev/null" $gold/$lgname 1>/dev/null

sed "s;@@;@0@;g" $gold/$lgname/parse > $gold/$lgname/tmp
mv -f $gold/$lgname/tmp $gold/$lgname/parse
sed "s;@@;@0@;g" $gold/$lgname/parse > $gold/$lgname/tmp
mv -f $gold/$lgname/tmp $gold/$lgname/parse
sed "s;@@;@0@;g" $gold/$lgname/result > $gold/$lgname/tmp
mv -f $gold/$lgname/tmp $gold/$lgname/result
sed "s;@@;@0@;g" $gold/$lgname/result > $gold/$lgname/tmp
mv -f $gold/$lgname/tmp $gold/$lgname/result

#cp  $gold/$lgname/item $skeletons/$lgname/item
#cp  $gold/$lgname/relations $skeletons/$lgname/relations

rm -rf $grammar
