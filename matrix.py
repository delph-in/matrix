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
                               ['customizationroot=', 'CUSTOMIZATIONROOT=',
                                'force', 'help'])
  except getopt.GetoptError, err:
    print str(err)
    usage()
  for o, a in opts:
    if o in ('-C', '--customizationroot', '--CUSTOMIZATIONROOT'):
      os.environ['CUSTOMIZATIONROOT'] = os.path.abspath(a)
    elif o in ('-F', '--force'):
      force = True
    elif o in ('-h', '--help'):
      printhelp()

  validate_args(args)

  if args[0] in ('c', 'customize'):
    dest = args[2] if len(args) > 2 else None
    customize_grammar(args[1], destination=dest)

  elif args[0] in ('cf', 'customize-and-flop'):
    dest = args[2] if len(args) > 2 else None
    customize_grammar(args[1], destination=dest, flop=True)

  elif args[0] in ('v', 'validate'):
    choices_file = args[1]
    if os.path.isdir(choices_file):
      choices_file = os.path.join(choices_file, 'choices')
    if not os.path.exists(choices_file):
      sys.exit("Error: Choices file not found at " + choices_file)
    import gmcs.validate
    v = gmcs.validate.validate_choices(choices_file)
    for x in v.errors:
      print x
      print '  ', v.errors[x]

  elif args[0] in ('u', 'unit-test'):
    run_unit_tests()

  elif args[0] in ('r', 'regression-test'):
    cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'],
                       'regression_tests/run_regression_tests.sh')
    #Using subprocess makes it difficult to kill the process
    # (e.g. with Ctrl-C), so we need to handle KeyboardInterrupts
    # (or alternatively use a os.exec* function)
    try:
      p = subprocess.Popen([cmd] + args[1:], env=os.environ)
      p.wait()
    except KeyboardInterrupt:
      print "\nProcess interrupted. Aborting regression tests.\n"
      import signal
      os.kill(p.pid, signal.SIGKILL)

  elif args[0] in ('ra', 'regression-test-add'):
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
    subprocess.call([cmd, '-r', '-m', location], env=os.environ)

  elif args[0] == 'vivify':
    # pass the force flag in case the user wants to avoid checks
    vivify(force)

  else:
    usage()

def validate_python_version():
  """
  Make sure the user is running Python 2.5 or greater.
  """
  if sys.version_info[0] != 2 or sys.version_info[1] < 5:
    version = '.'.join(str(x) for x in sys.version_info[0:2])
    print "Operation aborted: incompatible Python version."
    print "  You are running Python " + version + ", but the Grammar Matrix"
    print "  Customization System requires Python 2.5, 2.6, or 2.7."
    sys.exit(2)

def validate_args(args):
  """
  Run some quick tests to make sure we have the right number of arguments.
  """
  if len(args) == 0: usage()
  elif args[0] in ('c', 'customize'):
    if len(args) < 2: usage('customize')
  elif args[0] in ('cf', 'customize-and-flop'):
    if len(args) < 2: usage('customize-and-flop')
  elif args[0] in ('v', 'validate'):
    if len(args) < 2: usage('validate')
  elif args[0] in ('u', 'unit-test'):
    pass # no other arguments needed
  elif args[0] in ('r', 'regression-test'):
    pass # other arguments are optional
  elif args[0] in ('ra', 'regression-test-add'):
    if len(args) < 3: usage('regression-test-add')
  elif args[0] in ('i', 'install'):
    if len(args) < 2: usage('install')
  elif args[0] == 'vivify':
    pass # no other arguments needed

def usage(command=None):
  if not command:
    print "Usage: matrix.py [OPTION] COMMAND [ARGS...]"
  elif command in ('customize', 'customize-and-flop'):
    print "Usage: matrix.py [OPTION] " + command + " PATH [DEST]"
    print "       Where PATH is the path to a choices file or a directory"
    print "       containing a choices file, and the optional argument DEST"
    print "       points to the output directory."
  elif command == 'validate':
    print "Usage: matrix.py [OPTION] validate PATH"
    print "       Where PATH is the path to a choices file or a directory"
    print "       containing a choices file."
  elif command == 'regression-test-add':
    print "Usage: matrix.py [OPTION] regression-test-add CHOICES TXTSUITE"
    print "       Where CHOICES is the path to a choices file and TXTSUITE"
    print "       is the path to a text file containing test sentences."
  elif command == 'install':
    print "Usage: matrix.py [OPTION] install PATH"
    print "       Where PATH is the path where the Matrix Customization"
    print "       System should be installed."

  print "Try `matrix.py --help' for more information."
  sys.exit(2)

def printhelp():
  print """Usage: matrix.py [OPTION] COMMAND [ARGS...]

OPTIONS:
    --cr (-C) PATH                 : Set CUSTOMIZATIONROOT to PATH.
    --help (-h)                    : Print this help message.

COMMANDS:
    customize (c) PATH [DEST]      : Customize the choices file at PATH,
                                     with the output going to DEST (if
                                     specified) or PATH. PATH is either a
                                     directory or a choices file.
    customize-and-flop (cf) PATH [DEST]
                                   : Customize the choices file at PATH,
                                     then flop the resulting grammar.
    validate (v) PATH              : Validate the choices file at PATH.
    regression-test (r) [TEST]     : Run regression TEST (runs all tests if
                                     TEST is not specified).
    regression-test-add (ra) CHOICES TXTSUITE
                                   : Add CHOICES and TXTSUITE as a new
                                     regression test.
    regression-test-update (ru) TEST
                                   : Update the gold standard of TEST to use
                                     the results of the current system.
    unit-test (u)                  : Run all unit tests.
    install (i) PATH               : Install a custom instance of the Grammar
                                     Matrix Customization System at the PATH
                                     specified on the default server.
    vivify                         : Install a new version of the Grammar
                                     Matrix Customization System to the live
                                     site after verifying the code has been
                                     tested and committed to SVN.

EXAMPLES:
    matrix.py customize ../choices/Finnish
    matrix.py cf ../choices/Finnish
    matrix.py v ../choices/Finnish
    matrix.py -C gmcs/ r
    matrix.py ra Cree_choices Cree_test_suite
    matrix.py install my_matrix
    matrix.py vivify

"""
  sys.exit()

def verify_force():
  print "   You have selected to skip safety checks. Please only do this in"
  print "   rare circumstances when a change is minor and urgently needed. Do"
  print "   not use this option out of impatience or laziness!"
  if utils.verify():
    return True
  print "   Aborted."
  sys.exit(1)

def customize_grammar(path, destination=None, flop=False):
  """
  Customize a grammar for the choices file at directory, and if flop
  is True, run flop on the resulting lang-pet.tdl file in the grammar
  directory.
  """
  # run this check before customizing
  if flop and 'LOGONROOT' not in os.environ:
    sys.exit('Error: Cannot flop grammar if LOGONROOT is not set.')
  # customize if choices file found
  import gmcs.customize
  if os.path.isdir(path):
    path = os.path.join(path, 'choices')
  if not os.path.exists(path):
    sys.exit("Error: No choices file found at " + path)
  grammar_dir = gmcs.customize.customize_matrix(path, 'tgz', destination)
  # Now a grammar has been created, so we can flop it
  if flop:
    import gmcs.choices
    lang = gmcs.choices.get_choice('language', path)
    pet_file = lang.lower() + '-pet.tdl'
    if not os.path.exists(os.path.join(grammar_dir, pet_file)):
      sys.exit("Error: " + pet_file + " not found.")
    cmd = os.path.join(os.environ['LOGONROOT'], 'bin/flop')
    devnull = open('/dev/null', 'w')
    subprocess.call([cmd, pet_file], cwd=grammar_dir, env=os.environ, stderr=devnull)


def run_unit_tests():
  import unittest

  def print_line():
    print 75 * '='

  loader = unittest.defaultTestLoader
  runner = unittest.TextTestRunner(verbosity=1)

  print_line()
  print 'Choices tests:'
  import gmcs.tests.testChoices
  runner.run(loader.loadTestsFromModule(gmcs.tests.testChoices))

  print_line()
  print 'Validate tests:'
  import gmcs.tests.testValidate
  runner.run(loader.loadTestsFromModule(gmcs.tests.testValidate))

  #print_line()
  #print 'Linglib/Morphotactics tests:'
  #import gmcs.linglib.tests.testMorphotactics
  #runner.run(loader.loadTestsFromModule(gmcs.linglib.tests.testMorphotactics))

  print_line()

def vivify(force):
  # Before vivifying, make sure the following have occurred:
  #  1. Regression tests have been run if any code has been altered
  #     since the last vivification.
  #  2. There are no remaining modifications not checked into SVN.
  cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'], '../install')
  subprocess.call([cmd, '-r', '-m', 'matrix/customize'], env=os.environ)

if __name__ == '__main__':
  validate_python_version()
  main()
