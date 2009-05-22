### This script takes the results of invoking create-matrix-unit-test()
### from within lisp (lkb package) and copies things into the right
### directories.

### Usage:

### python add-unit-test.py choices-file txt-suite

### The program assumes that the environment variable CUSTOMIZATIONROOT
### is set appropriately.

import sys
import os
import shutil
import re
from unittestindex import UnitTestIndex

sys.path.append("..")
from choices import ChoicesFile

cust_root = os.environ.get("CUSTOMIZATIONROOT")


# Check whether arguments correspond to existing files

choices_file = sys.argv[1]
txt_suite = sys.argv[2]

ut_root = cust_root + "/unit-tests/"

if not os.path.exists(cust_root):
    raise ValueError, "Invalid path name for customization root."

if not os.path.exists(choices_file):
    raise ValueError, "Invalid path name for choices file."

if not os.path.exists(txt_suite):
    raise ValueError, "Invalid path name for txt-suite file."

if not os.path.exists(ut_root + "unit-test-index"):
    raise ValueError, "No file unit-tests/unit-test-index in customization root.  Invalid customization root or missing file."

# Get the language name

ch = ChoicesFile(choices_file)
lg_name = ch.get('language')

# Load up the current unit test index

index = UnitTestIndex(ut_root + "unit-test-index")

# Check whether we already have a unit-test with that
# name, and if not, if there are any files in the way anyway.

if index.exists(lg_name):
    raise ValueError, "A unit test with that language name already exists.  If you must remove it, be sure to edit unit-test-index."

if os.path.exists(ut_root + "choices/" + lg_name):
    raise ValueError, "Move unit-tests/choices/" + lg_name +", it is in the way."

if os.path.exists(ut_root + "txt-suites/" + lg_name):
    raise ValueError, "Move unit-tests/txt-suites/" + lg_name +", it is in the way."

if os.path.exists(ut_root + "home/gold/" + lg_name):
    raise ValueError, "Move unit-tests/home/gold/" + lg_name +", it is in the way."

if os.path.exists(ut_root + "home/skeletons/" + lg_name):
    raise ValueError, "Move unit-tests/home/skeletons/" + lg_name +", it is in the way. You might also need to edit unit-tests/home/skeletons/Index.lisp."

# Check whether there is an appropriate profile in home/current
# _FIX_ME_: Ideally, this should also check whether the profile corresponds
# to the txt-suite, at least.

if not os.path.exists(ut_root + "home/current/" + lg_name):
    raise ValueError, "No [incr tsdb()] profile in home/current for this unit test.  Invoke create-matrix-unit-test() from within lisp first, and then come back."

# Prompt user for comment on test.

comment = raw_input("Enter a short comment describing this unit test: ")

# Add line to unit-test-index

index_file = open(ut_root + "unit-test-index", 'a')
index_file.write(lg_name + "=" + comment + "\n")
index_file.close()

# Copy choices file, txt-suite and profile to the appropriate places.

shutil.copy(choices_file, ut_root + "choices/" + lg_name)
shutil.copy(txt_suite, ut_root + "txt-suites/" + lg_name)
shutil.copytree(ut_root + "home/current/" + lg_name, ut_root + "home/gold/" + lg_name)

# Create skeleton

os.mkdir(ut_root + "skeletons/" + lg_name)
shutil.copy(ut_root + "home/gold/" + lg_name + "/item", ut_root + "skeletons/" + lg_name)
shutil.copy(ut_root + "skeletons/Relations", ut_root + "skeletons/" + lg_name + "/relations")

# Update Index.lisp

f = open(ut_root + "skeletons/Index.lisp",'r')
lines = f.readlines()
f.close

f = open(ut_root + "skeletons/Index.lisp",'w')
for l in lines:
    f.write(l)
    if (re.search("new-unit-test-here",l)):
        f.write("((:path . \"")
        f.write(lg_name)
        f.write("\") (:content . \"")
        f.write(lg_name + ": " + comment)
        f.write("\"))\n")
f.close()

# Reminders about svn

print "Success!\n Use svn add to add choices/" + lg_name + ", txt-suites/" + lg_name + ",\n skeletons." + lg_name + ", and home/gold/" + lg_name + ".\n Then do svn commit in unit-tests."
