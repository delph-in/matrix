#!/bin/bash

# Script for cleaning up after the regression tests
# Be sure that $CUSTOMIZATIONROOT is set appropriately
# (i.e., to point to the matrix/customize directory you
# intend to test... the regression_tests directory with that
# customize/directory is the one that will be active).

# TJT 2014-09-09: Adding keyword for setting customization root
# from command line (see http://stackoverflow.com/questions/7126580/expand-a-possible-relative-path-in-bash)

# Doesn't seem to work... fix later
while getopts ":C:" opt; do
    case "$opt" in
    C)
	CUSTOMIZATIONROOT=$(readlink -f $OPTARG)
        ;;
    :)
	echo "Option -$OPTARG requires an argument." >&2
    esac
done

gram_dir=${CUSTOMIZATIONROOT}/regression_tests/grammars
ls $gram_dir | grep -v README | awk '{printf("rm -rf %s/%s\n",$gram_dir,$1);}' | sh

logs_dir=${CUSTOMIZATIONROOT}/regression_tests/logs
ls $logs_dir | grep -v README | awk '{printf("rm -rf %s/%s\n",$logs_dir,$1);}' | sh

home_cur_dir=${CUSTOMIZATIONROOT}/regression_tests/home/current
ls $home_cur_dir | grep -v README | awk '{printf("rm -rf %s/%s\n",$home_cur_dir,$1);}' | sh
