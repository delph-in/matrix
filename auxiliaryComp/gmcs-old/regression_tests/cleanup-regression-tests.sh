#!/bin/bash

# Script for cleaning up after the regression tests
# Be sure that $CUSTOMIZATIONROOT is set appropriately
# (i.e., to point to the matrix/customize directory you
# intend to test... the regression_tests directory with that
# customize/directory is the one that will be active).

gram_dir=${CUSTOMIZATIONROOT}/regression_tests/grammars
ls $gram_dir | grep -v README | awk '{printf("rm -rf %s/%s\n",$gram_dir,$1);}' | sh

logs_dir=${CUSTOMIZATIONROOT}/regression_tests/logs
ls $logs_dir | grep -v README | awk '{printf("rm -rf %s/%s\n",$logs_dir,$1);}' | sh

home_cur_dir=${CUSTOMIZATIONROOT}/regression_tests/home/current
ls $home_cur_dir | grep -v README | awk '{printf("rm -rf %s/%s\n",$home_cur_dir,$1);}' | sh
