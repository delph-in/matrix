#!/bin/bash

if [ $# -ne 1 ]
then
  echo "Usage: $0 <lg-name>"
  exit 0
fi

svn -q add regression_tests/home/gold/"$1" regression_tests/skeletons/"$1"
svn -q add regression_tests/home/gold/"$1"/[a-z]* regression_tests/skeletons/"$1"/[a-z]* regression_tests/choices/"$1" regression_tests/txt-suites/"$1"
