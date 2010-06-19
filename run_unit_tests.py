#!/usr/bin/python

import unittest

import gmcs.tests.testChoices
import gmcs.tests.testValidate

def print_line():
  print 75 * '='

print_line()

print 'Choices tests:'
try:
  unittest.main(gmcs.tests.testChoices)
except:
  pass

print_line()

print 'Validate tests:'
try:
  unittest.main(gmcs.tests.testValidate)
except:
  pass

print_line()

print 'Linglib/Morphotactics tests:'
try:
  unittest.main(gmcs.linglib.tests.testMorphotactics)
except:
  pass

print_line()
