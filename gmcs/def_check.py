### $Id: def_check.py,v 1.4 2008-05-28 21:08:12 sfd Exp $

# ERB I've found too many bugs that come from using
# the wrong variable name.  This script is intended
# to check all of the variables we are using and check
# that they are at least valid variables declared in MatrixDef.

# As a possible extension, we could try to check for
# the values from MatrixDef as well.

import re
import sys
import utils
tokenize_def = utils.tokenize_def


# Read in MatrixDef and create a list of the variables
# that are declared.

var_list = {}

f = open('matrixdef', 'r')
line = f.readlines()
f.close()
  
for l in line:
    l = l.strip()
    if len(l):
        w = tokenize_def(l)
        if len(w) >= 3:
          ty = w[0]
          vn = w[1]
          fn = w[2]
          if ty == 'Text' or ty == 'Radio' or ty == '.' or ty == 'Check':
              var_list[vn] = True

# Read through arg[0] for all instances of ch(), and
# check that the variable being looked up is in the list.

codefile = open(sys.argv[1], 'r')
line = codefile.readlines()
codefile.close()

for l in line:
    l = l.strip()
    if re.search('ch\(',l):
        words = l.split(None)
        for w in words:
            if re.search(r'^ch\(',w):
                repl1 = re.compile('ch\(\'')
                w = repl1.sub('',w,1)
                repl2 = re.compile('\'\).*')
                w = repl2.sub('',w,1)

                if not var_list.has_key(w):
                    print 'Error: You are trying to look up a variable\n that is not defined in matrixdef: ' + w + '\n'


