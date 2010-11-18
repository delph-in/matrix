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
      if len(args) > 2:
        dest = args[2]
      else:
        dest = None
      customize_grammar(args[1], destination=dest)

    elif args[0] in ('cf', 'customize-and-flop'):
      if len(args) > 2:
        dest = args[2]
      else:
        dest = None
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

    elif args[0] in ('u', 'utest', 'unit-test'):
      run_unit_tests()

    elif args[0] in ('r', 'rtest', 'regression-test'):
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
      subprocess.call([cmd, '-r', '-m', location], env=os.environ)

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
    customize (c) PATH [DEST]      : Customize the choices file at PATH,
                                     with the output going to DEST (if
                                     specified) or PATH. PATH is either a
                                     directory or a choices file.
    customize-and-flop (cf) PATH [DEST]
                                   : Customize the choices file at PATH,
                                     then flop the resulting grammar.
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
  main()
