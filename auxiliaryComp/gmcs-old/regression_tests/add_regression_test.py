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
from gmcs.regression_tests.regressiontestindex import RegressionTestIndex
from gmcs.choices import ChoicesFile

def add(choices_file, txt_suite):
  cust_root = os.environ.get("CUSTOMIZATIONROOT")

  # Check whether arguments correspond to existing files

  rt_root = cust_root + "/regression_tests/"

  if not os.path.exists(cust_root):
      raise ValueError, "Invalid path name for customization root."

  if not os.path.exists(choices_file):
      raise ValueError, "Invalid path name for choices file."

  if not os.path.exists(txt_suite):
      raise ValueError, "Invalid path name for txt-suite file."

  if not os.path.exists(rt_root + "regression-test-index"):
      raise ValueError, "No file regression_tests/regression-test-index in customization root.  Invalid customization root or missing file."

  # Get the language name

  ch = ChoicesFile(choices_file)
  lg_name = ch.get('language')

  # Load up the current regression test index

  index = RegressionTestIndex(rt_root + "regression-test-index")

  # Check whether we already have a regression-test with that
  # name, and if not, if there are any files in the way anyway.

  if index.exists(lg_name):
      raise ValueError, "A regression test with that language name already exists.  If you must remove it, be sure to edit regression-test-index."

  if os.path.exists(rt_root + "choices/" + lg_name):
      raise ValueError, "Move regression_tests/choices/" + lg_name +", it is in the way."

  if os.path.exists(rt_root + "txt-suites/" + lg_name):
      raise ValueError, "Move regression_tests/txt-suites/" + lg_name +", it is in the way."

  if os.path.exists(rt_root + "home/gold/" + lg_name):
      raise ValueError, "Move regression_tests/home/gold/" + lg_name +", it is in the way."

  if os.path.exists(rt_root + "home/skeletons/" + lg_name):
      raise ValueError, "Move regression_tests/home/skeletons/" + lg_name +", it is in the way. You might also need to edit regression_tests/home/skeletons/Index.lisp."

  # Check whether there is an appropriate profile in home/current
  # _FIX_ME_: Ideally, this should also check whether the profile corresponds
  # to the txt-suite, at least.

  if not os.path.exists(rt_root + "home/current/" + lg_name):
      raise ValueError, "No [incr tsdb()] profile in home/current for this regression test.  Invoke create-matrix-regression-test() from within lisp first, and then come back."

  # Prompt user for comment on test.

  comment = raw_input("Enter a short comment describing this regression test: ")

  # Add line to regression-test-index

  index_file = open(rt_root + "regression-test-index", 'a')
  index_file.write(lg_name + "=" + comment + "\n")
  index_file.close()

  # Copy choices file, txt-suite and profile to the appropriate places.

  shutil.copy(choices_file, rt_root + "choices/" + lg_name)
  shutil.copy(txt_suite, rt_root + "txt-suites/" + lg_name)
  shutil.copytree(rt_root + "home/current/" + lg_name, rt_root + "home/gold/" + lg_name)

  # Create skeleton

  os.mkdir(rt_root + "skeletons/" + lg_name)
  shutil.copy(rt_root + "home/gold/" + lg_name + "/item", rt_root + "skeletons/" + lg_name)
  shutil.copy(rt_root + "skeletons/Relations", rt_root + "skeletons/" + lg_name + "/relations")

  # Update Index.lisp

  f = open(rt_root + "skeletons/Index.lisp",'r')
  lines = f.readlines()
  f.close

  f = open(rt_root + "skeletons/Index.lisp",'w')
  for l in lines:
      f.write(l)
      if (re.search("new-regression-test-here",l)):
          f.write("((:path . \"")
          f.write(lg_name)
          f.write("\") (:content . \"")
          f.write(lg_name + ": " + comment)
          f.write("\"))\n")
  f.close()

  # Reminders about svn

  print "Success!\n Use svn add to add choices/" + lg_name + ", txt-suites/" + lg_name + ",\n skeletons/" + lg_name + ", and home/gold/" + lg_name + ".\n Then do svn commit in regression_tests."
