#!/bin/bash

# Script for cleaning up after the regression tests
# Be sure that $CUSTOMIZATIONROOT is set appropriately
# (i.e., to point to the matrix/customize directory you
# intend to test... the regression_tests directory with that
# customize/directory is the one that will be active).

rm -rf ${CUSTOMIZATIONROOT}/regression_tests/grammars/*
#rmdir ${CUSTOMIZATIONROOT}/regression_tests/grammars/* 2>&1 | grep -v README
rm -rf ${CUSTOMIZATIONROOT}/regression_tests/logs/*
#rmdir ${CUSTOMIZATIONROOT}/regression_tests/logs/* 2>&1 | grep -v README
rm -rf ${CUSTOMIZATIONROOT}/regression_tests/home/current/*
#rmdir ${CUSTOMIZATIONROOT}/regression_tests/home/current/* 2>&1 | grep -v README
