#!/usr/bin/python
import sys
import os
import getopt
import subprocess

def main():
  # The force flag is used to skip checks in some commands
  force = False

  try:
    opts, args = getopt.getopt(sys.argv[1:], 'C:Fh',
                               ['cr=', 'customizationroot=',
                                'CUSTOMIZATIONROOT=','force', 'help'])
  except getopt.GetoptError, err:
    print str(err)
    usage()
  for o, a in opts:
    if o in ('-C', '--cr', '--customizationroot', '--CUSTOMIZATIONROOT'):
      os.environ['CUSTOMIZATIONROOT'] = os.path.abspath(a)
    elif o in ('-F', '--force'):
      force = True
    elif o in ('-h', '--help'):
      printhelp()
  try:
    if args[0] in ('c', 'customize'):
      import gmcs.customize
      gmcs.customize.customize_matrix(args[1], 'tgz')

    elif args[0] in ('v', 'validate'):
      import gmcs.validate
      v = gmcs.validate.validate_choices(args[1] + '/choices')
      for x in v.errors:
        print x
        print '  ', v.errors[x]

    elif args[0] in ('u', 'utest', 'unit-test'):
      run_unit_tests()

    elif args[0] in ('r', 'rtest', 'regression-test'):
      cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'],
                         'regression_tests/run_regression_tests.sh')
      os.execve(cmd, [cmd] + args[1:], os.environ)

    elif args[0] in ('a', 'rtest-add', 'regression-test-add'):
      choices = args[1]
      txtsuite = args[2]
      import gmcs.regression_tests.add_regression_test
      gmcs.regression_tests.add_regression_test.add(choices, txtsuite)


    elif args[0] in ('i', 'install'):
      cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'], '../install')
      location = args[1]
      # Installations to the live site require validations, so abort.
      if location.strip('/') == 'matrix/customize':
        print "Error: For installation to the live site, please use:"
        print "  matrix.py vivify"
        sys.exit(2)
      os.execve(cmd, [cmd, '-r', '-m', location], os.environ)

    elif args[0] == 'vivify':
      # pass the force flag in case the user wants to avoid checks
      vivify(force)

    else:
      usage()

  except IndexError:
    usage()


def usage():
  print "Usage: matrix.py [OPTION] COMMAND [ARGS...]"
  print "Try `matrix.py --help' for more information."
  sys.exit(2)

def printhelp():
  print """Usage: matrix.py [OPTION] COMMAND [ARGS...]

OPTIONS:
    --cr (-C) PATH                 : Set CUSTOMIZATIONROOT to PATH.
    --help (-h)                    : Print this help message.

COMMANDS:
    customize (c) PATH             : Customize the choices file at PATH.
    validate (v) PATH              : Validate the choices file at PATH.
    rtest (r) [TEST]               : Run regression TEST (runs all tests
                                     TEST is not specified).
    rtest-add (a) CHOICES TXTSUITE : Add CHOICES and TXTSUITE as a new
                                     regression test.
    utest (u)                      : Run all unit tests.
    install (i) PATH               : Install a custom instance of the Grammar
                                     Matrix Customization System at the PATH
                                     specified on the default server.
    vivify                         : Install a new version of the Grammar
                                     Matrix Customization System to the live
                                     site after verifying the code has been
                                     tested and committed to SVN.

EXAMPLES:
    matrix.py customize ../choices/Finnish
    matrix.py v ../choices/Finnish
    matrix.py --cr ./gmcs r
    matrix.py rtest-add Cree_choices Cree_test_suite
    matrix.py install my_matrix
    matrix.py vivify

"""
  sys.exit()

def verify_force():
  print "   You have selected to skip safety checks. Please only do this in"
  print "   rare circumstances when a change is minor and urgently needed. Do"
  print "   not use this option out of impatience or laziness!"
  if raw_input("   Do you want to continue? (y/n): ").lower() in ('y','yes'):
    return True
  print "   Aborted."
  sys.exit(1)

def run_unit_tests():
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

def vivify(force):
  # Before vivifying, make sure the following have occurred:
  #  1. Regression tests have been run if any code has been altered
  #     since the last vivification.
  #  2. There are no remaining modifications not checked into SVN.
  cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'], '../install')
  os.execve(cmd, [cmd, '-r', '-m', 'matrix/customize'], os.environ)

if __name__ == '__main__':
  main()
