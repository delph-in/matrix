#!/usr/bin/python

import unittest

import customize.tests.testChoices
import customize.tests.testValidate

def print_line():
  print 75 * '='

print_line()

print 'Choices tests:'
try:
  unittest.main(customize.tests.testChoices)
except:
  pass

print_line()

print 'Validate tests:'
try:
  unittest.main(customize.tests.testValidate)
except:
  pass

print_line()
