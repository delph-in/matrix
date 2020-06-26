
# Regression Test Txt-Suites

This directory contains the txt-suite files used in the regression
tests. The txt-suites are just text files containing one sentence per
line, with ungrammatical lines having `*` as the first character of
each line. The txt-suites are used to create skeletons for
testing. You should not move txt-suite files here by hand, but instead
use the `rtest.py` script with the `--add` option.
