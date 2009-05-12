#!/bin/bash

if [ $# -ne 1 ]
then
  echo "Usage: $0 <lg-name>"
  exit 0
fi

svn -q add home/gold/"$1" skeletons/"$1"
svn -q add home/gold/"$1"/[a-z]* skeletons/"$1"/[a-z]* choices/"$1" txt-suites/"$1"
