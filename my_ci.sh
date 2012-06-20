#!/bin/sh

svn update
find . -name "*.pyc" -exec rm -rf {} \;
find . -name "*~" -exec rm -rf {} \;
svn add --force *
svn ci -m $1


