### This script takes the results of invoking create-matrix-regression-test()
### from within lisp (lkb package) and copies things into the right
### directories.

### Usage:

### python add-regression-test.py choices-file txt-suite

### The program assumes that the environment variable CUSTOMIZATIONROOT
### is set appropriately.

import sys
import os
import shutil
import re
import subprocess
from gmcs.regression_tests.regressiontestindex import RegressionTestIndex
from gmcs.choices import ChoicesFile

# CMC 2017-04-08: Changes throughout to consistently use os.path.join instead of
# manually building paths. This gives more robust path handling, including
# support for relative paths in choices_file and txt_suite.

def add(choices_file, txt_suite):
    cust_root = os.environ.get("CUSTOMIZATIONROOT")

    # Check whether arguments correspond to existing files
    rt_root = os.path.join(cust_root,'regression_tests')

    # CMC 2017-04-08: use expansion to handle relative paths (can't deal with slashes)
    choices_file = os.path.join(rt_root, 'scratch', *choices_file.split('/'))
    txt_suite = os.path.join(rt_root, 'scratch', *txt_suite.split('/'))

    if not os.path.exists(cust_root):
        raise ValueError, "Invalid path name for customization root."

    if not os.path.exists(choices_file):
        raise ValueError, "Invalid path name for choices file."

    if not os.path.exists(txt_suite):
        raise ValueError, "Invalid path name for txt-suite file."

    # CMC 2017-04-08: don't do path joins by hand
    if not os.path.exists(os.path.join(rt_root, "regression-test-index")):
        raise ValueError, "No file regression_tests/regression-test-index in customization root.  Invalid customization root or missing file."

    # Get the language name

    ch = ChoicesFile(choices_file)
    lg_name = ch.get('language')

    # Make sure illegal characters (for lisp strings) are not in the lg_name.
    # For now this is just double-quotes "
    if any(c in ('"',) for c in lg_name):
        raise ValueError, 'Double-quotes are not allowed in the language name.'

    # Load up the current regression test index
    index = RegressionTestIndex(os.path.join(rt_root, "regression-test-index"))

    # Check whether we already have a regression-test with that
    # name, and if not, if there are any files in the way anyway.

    if index.exists(lg_name):
        # TJT 2014-09-11: Updating this error to suggest using matrix.py to remove test
        #raise ValueError, "A regression test with that language name already exists.  If you must remove it, be sure to edit regression-test-index."
        raise ValueError, "A regression test with that language name already exists.  If you need to remove the existing test, use matrix.py rr TEST"

    if os.path.exists(os.path.join(rt_root, "choices", lg_name)):
        raise ValueError, "Move regression_tests/choices/" + lg_name +", it is in the way."

    if os.path.exists(os.path.join(rt_root, "txt-suites", lg_name)):
        raise ValueError, "Move regression_tests/txt-suites/" + lg_name +", it is in the way."

    if os.path.exists(os.path.join(rt_root, "home", "gold", lg_name)):
        raise ValueError, "Move regression_tests/home/gold/" + lg_name +", it is in the way."

    # CMC 2018-04-09: this path was wrong (skeletons dir is rt_root, not rt_root/home)
    if os.path.exists(os.path.join(rt_root, "skeletons", lg_name)):
        raise ValueError, "Move regression_tests/skeletons/" + lg_name +", it is in the way. You might also need to edit regression_tests/skeletons/Index.lisp."

    # Prompt user for comment on test.
    comment = raw_input("Enter a short comment describing this regression test: ")

    # Make sure illegal characters (for lisp strings) are not in the comment.
    # For now this is just double-quotes "
    if any(c in ('"',) for c in comment):
        raise ValueError, 'Double-quotes are not allowed in the comment.'

    # Make the profile
    cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'], 'regression_tests', 'add_regression_test.sh')
    retval = subprocess.call([cmd, choices_file, txt_suite, lg_name], env=os.environ);
    #if retval != 0:
    #  print "Error creating regression test... (possible missing gold results?)"

    # Add line to regression-test-index
    index_file = open(os.path.join(rt_root, "regression-test-index"), 'a')
    index_file.write(lg_name + "=" + comment + "\n")
    index_file.close()

    # Copy choices file, txt-suite and profile to the appropriate places.
    shutil.copy(choices_file, os.path.join(rt_root, "choices", lg_name))
    shutil.copy(txt_suite, os.path.join(rt_root, "txt-suites", lg_name))

    # Create skeleton
    os.mkdir(os.path.join(rt_root, "skeletons", lg_name))
    shutil.copy(os.path.join(rt_root, "home", "gold", lg_name, "item"), os.path.join(rt_root, "skeletons", lg_name))
    shutil.copy(os.path.join(rt_root, "skeletons", "Relations"), os.path.join(rt_root, "skeletons", lg_name, "relations"))

    # Update Index.lisp
    f = open(os.path.join(rt_root, "skeletons", "Index.lisp"),'r')
    lines = f.readlines()
    f.close

    f = open(os.path.join(rt_root, "skeletons", "Index.lisp"),'w')
    for l in lines:
        f.write(l)
        if (re.search("new-regression-test-here",l)):
            f.write("((:path . \"")
            f.write(lg_name)
            f.write("\") (:content . \"")
            f.write(lg_name + ": " + comment)
            f.write("\"))\n")
    f.close()
    return lg_name
