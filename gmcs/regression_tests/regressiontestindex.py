import sys
import os
import shutil
import re

class RegressionTestIndex:
    # initialize by passing either a file name or ???file handle
    def __init__(self, index_file):
        self.file_name = index_file
        self.tests = {}

        f = open(index_file,'r')
        lines = f.readlines()

        for l in lines:
            l = l.strip()
            if l:
                (testname, comment) = l.split('=')
                self.set(testname, comment)

    # Set the value of 'key' to 'value'
    def set(self, testname, comment):
        self.tests[testname] = comment

    # Check whether a testname is there
    def exists(self, testname):
        return testname in self.tests

if __name__ == "__main__":
    cust_root = os.environ.get("CUSTOMIZATIONROOT")
    if not os.path.exists(cust_root + "/regression_tests/regression-test-index"):
        raise ValueError, "Invalid path name for customization root or missing regression test index."

    index = RegressionTestIndex(cust_root + "/regression_tests/regression-test-index")

    keys = ""

    if sys.argv[1] == "--lg-names":
        print " ".join(sorted(index.tests.keys()))

    if sys.argv[1] == "--comment":
        print index.tests[sys.argv[2]]
