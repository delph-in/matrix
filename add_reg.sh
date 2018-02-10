#!/bin/bash

dir_name=$1

path_to_choices="$dir_name/choices3"
path_to_txtsuite="$dir_name/txt-suite"

cp $path_to_choices gmcs/regression_tests/scratch/choices3
cp $path_to_txtsuite gmcs/regression_tests/scratch/txt-suite
python matrix.py regression-test-add choices3 txt-suite
rm gmcs/regression_tests/scratch/choices3*
rm gmcs/regression_tests/scratch/txt-suite*
