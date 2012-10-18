rm -rf sessions
rm -rf *.pyc
svn up
svn add --force *
svn ci -m $1


