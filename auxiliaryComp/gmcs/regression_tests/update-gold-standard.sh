#!/bin/bash

#Script for updating gold standard when the diffs to
#the current represent progress rather than regression.

# update-gold-standard.sh lg-name

#Assumes that you've been looking at it interactively,
#so that there is something in home/current/lg-name.

lgname=$1;

if [ -z "${CUSTOMIZATIONROOT}" ]; then
  echo "update-gold-standard: unable to determine \$CUSTOMIZATIONROOT directory; exit.";
  exit 1;
fi

current="${CUSTOMIZATIONROOT}/regression_tests/home/current/$lgname";

echo $current

if [ ! -e "$current" ]; then
  echo "update-gold-standard: no current profile; exit.";
  exit 1;
fi

gold="${CUSTOMIZATIONROOT}/regression_tests/home/gold/$lgname";

if [ ! -e "$gold" ]; then
  echo "update-gold-standard: no gold profile; exit.";
  exit 1;
fi

cp $current/item $gold/item;
cp $current/parse $gold/parse;
cp $current/relations $gold/relations;
cp $current/result $gold/result;
cp $current/run $gold/run;

echo "Copy successful.  Be sure to run svn commit in $gold."

