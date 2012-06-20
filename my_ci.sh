#!/bin/sh

svn update
rm -rf gmcs/regression_tests/grammars/*
rm -rf gmcs/regression_tests/home/current/*
rm -rf gmcs/regression_tests/logs/*
find . -name "*.pyc" -exec rm -rf {} \;
find . -name "*~" -exec rm -rf {} \;
svn add --force *
svn ci -m $1


