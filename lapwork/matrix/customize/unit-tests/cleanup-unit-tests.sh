#!/bin/bash

# Script for cleaning up after the unit tests
# Be sure that $CUSTOMIZATIONROOT is set appropriately
# (i.e., to point to the matrix/customize directory you
# intend to test... the unit-tests directory with that
# customize/directory is the one that will be active).

for d in "grammars logs home/current"
do
    pushd $d >/dev/null
    rm -rf */*
    rmdir * 2>&1 | grep -v README
    popd >/dev/null
done
