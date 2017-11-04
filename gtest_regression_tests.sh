#!/bin/bash

# Clean-up
mv gmcs/regression_tests/grammars/README .
rm -rf regression_tests/grammars/*

for file in gmcs/regression_tests/choices/*; do
    CHOICES=$(basename "$file")
    echo "Customizing $CHOICES" 
    python matrix.py cd $file gmcs/regression_tests/grammars/$CHOICES
    ~/LING_TOOLS/gtest/gTest.py -vv -G gmcs/regression_tests/grammars/$CHOICES R --skel-dir gmcs/regression_tests/skeletons/ --gold-dir gmcs/regression_tests/home/gold :$CHOICES
done

mv README gmcs/regression_tests/grammars/

