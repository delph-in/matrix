#!/usr/bin/python
import sys
import gmcs.validate

v = gmcs.validate.validate_choices(sys.argv[1] + '/choices')

for x in v.errors:
  print x
  print '  ', v.errors[x]
