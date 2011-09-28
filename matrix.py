4#!/usr/bin/python
import sys
import os
import getopt
import subprocess

### matrix.py
### A general-purpose script for running Matrix code.
### The intention of this script is to define options, commands, and
### arguments for running Matrix code, validates the user's input, and
### offers helpful messages for improper input, missing files, etc. It
### should not have a general try-except wrapper unless it re-raises
### the errors (otherwise they will be hidden from the user). Further,
### if possible it should check to make sure the developer is following
### best-practices for Matrix development (e.g. running regression tests
### and checking in code before vivifying, etc.).

# NOTE TO DEVELOPERS
# Because we are unsure of the Python version being used (and some systems
# we rely on use 2.4), this module should check the version and fail
# gracefully if it's less than our 'officially supported' version (currently
# this is 2.5). In order to fail gracefully, this script must not have any
# syntax from later Python versions (otherwise it will throw a SyntaxError
# before it does anything).

def main():
  # The force flag is used to skip checks in some commands
  force = False
  # Cheap fails to parse if there are no morphological rules. This
  # hack adds a blank rule (can cause spinning on generation!)
  cheaphack = False
  # show_warnings, if True, allows printing of warnings from validation
  show_warnings = False

  # Extract the options and arguments, act on the options, and validate
  # the commands.
  try:
    opts, args = getopt.getopt(sys.argv[1:], 'C:Fhw',
                               ['customizationroot=', 'CUSTOMIZATIONROOT=',
                                'force', 'help', 'warning', 'cheap-hack'])
  except getopt.GetoptError, err:
    print str(err)
    usage()
  for o, a in opts:
    if o in ('-C', '--customizationroot', '--CUSTOMIZATIONROOT'):
      os.environ['CUSTOMIZATIONROOT'] = os.path.abspath(a)
    elif o in ('-F', '--force'):
      force = True
    elif o in ('-h', '--help'):
      cmd = 'all'
      if len(args) > 0:
        cmd = args[0]
      usage(command=cmd, exitcode=0)
    elif o in ('-w', '--warning'):
      show_warnings = True
    elif o == '--cheap-hack':
      cheaphack = True

  # if CUSTOMIZATIONROOT is not set externally or through an option, try
  # to find an appropriate default directory
  ensure_customization_root_set()
  # make sure the argument have the correct number of parameters
  validate_args(args)

  if args[0] in ('c', 'customize'):
    dest = None
    if len(args) > 2:
      dest = args[2]
    customize_grammar(args[1], destination=dest, cheaphack=cheaphack)

  elif args[0] in ('cf', 'customize-and-flop'):
    dest = None
    if len(args) > 2:
      dest = args[2]
    customize_grammar(args[1], destination=dest,
                      flop=True, cheaphack=cheaphack)

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
      print '  ', v.errors[x].message
    if show_warnings:
      for x in v.warnings:
        print x
        print '  ', v.warnings[x].message
    # If there are errors, exit with a return code of 1, otherwise 0
    sys.exit(len(v.errors) > 0)

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
    try:
      lg = gmcs.regression_tests.add_regression_test.add(choices, txtsuite)
      print 'Succeeded copying files for %s.' % lg
      rpath = os.path.join(os.environ['CUSTOMIZATIONROOT'], 'regression_tests')
      subprocess.call(['svn', '-q', 'add'] +\
                      [os.path.join(rpath, 'home/gold/' + lg),
                       os.path.join(rpath, 'skeletons/' + lg)])
      subprocess.call(['svn', '-q', 'add'] +\
                      [os.path.join(rpath, 'home/gold/' + lg + '/[a-z]*'),
                       os.path.join(rpath, 'skeletons/' + lg + '/[a-z]*'),
                       os.path.join(rpath, 'choices/' + lg),
                       os.path.join(rpath, 'txt-suites/' + lg)])
      print 'Succeeded adding files to Subversion. Be sure to commit!'
    except ValueError, er:
      print "Error adding regression test."
      print er.message

  elif args[0] in ('regression-test-update', 'ru'):
    from gmcs import utils
    test = args[1]
    cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'],
                       'regression_tests/update-gold-standard.sh')
    print "Updating regression test gold standards assumes you have manually"
    print "compared results with TSDB and have determined the current set is"
    print "better than the gold standard. Only continue if you have done this!"
    if utils.verify():
      subprocess.call([cmd, test], env=os.environ)
    else:
      print "Aborted."
    sys.exit(1)

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
  elif args[0] in ('w', 'web-test'):
    run_web_tests();
  elif args[0] in ('wa', 'web-test-add'):
    comment = None
    if len(args) > 2:
      comment = args[2]
    add_web_test(args[1], comment);
  elif args[0] in ('wr', 'web-test-remove'):
    remove_web_test(args[1]);
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

def ensure_customization_root_set():
  """
  Set CUSTOMIZATIONROOT if the appropriate files are found in the
  current working directory.
  """
  if 'CUSTOMIZATIONROOT' in os.environ:
    return
  cwd = os.getcwd()
  if os.path.exists(os.path.join(cwd, 'customize.py')):
    os.environ['CUSTOMIZATIONROOT'] = cwd
  elif os.path.exists(os.path.join(cwd, 'gmcs/customize.py')):
    os.environ['CUSTOMIZATIONROOT'] = os.path.join(cwd, 'gmcs')
  else:
    print "CUSTOMIZATIONROOT is not set and cannot be found."
    sys.exit(2)

def validate_args(args):
  """
  Run some quick tests to make sure we have the right number of arguments.
  """
  if len(args) == 0: usage()
  elif args[0] in ('c', 'customize'):
    if len(args) < 2: usage(command='customize')
  elif args[0] in ('cf', 'customize-and-flop'):
    if len(args) < 2: usage(command='customize-and-flop')
  elif args[0] in ('v', 'validate'):
    if len(args) < 2: usage(command='validate')
  elif args[0] in ('u', 'unit-test'):
    pass # no other arguments needed
  elif args[0] in ('r', 'regression-test'):
    pass # other arguments are optional
  elif args[0] in ('ra', 'regression-test-add'):
    if len(args) < 3: usage(command='regression-test-add')
  elif args[0] in ('regression-test-update', 'ru'):
    if len(args) < 2: usage(command='regression-test-update')
  elif args[0] in ('w', 'web-test'):
    pass #no other arguments needed
  elif args[0] in ('wa', 'web-test-add'):
    if len(args) < 2: usage(command='web-test-add')
  elif args[0] in ('wr', 'web-test-remove'):
    if len(args) < 2: usage(command='web-test-remove')
  elif args[0] in ('i', 'install'):
    if len(args) < 2: usage(command='install')
  elif args[0] == 'vivify':
    pass # no other arguments needed

def usage(command=None, exitcode=2):
  """
  Print an appropriate usage message and exit.
  """
  indent = 0
  # if the user asks for help for an invalid command, nothing will be printed,
  # so we catch this with a flag.
  something_printed = False
  def p(msg, nobreak=False):
    """ Print the message with necessary indentation and linebreaks. """
    if nobreak:
      print " " * indent + msg,
    else:
      print " " * indent + msg

  p("Usage: matrix.py [OPTION]", nobreak=True)
  if not command or command=='all':
    p("COMMAND [ARGS...]\n")
    something_printed = True
    if command == 'all':
      p("OPTIONS:")
      indent = 4
      p("--customizationroot (-C) PATH")
      p("            Set CUSTOMIZATIONROOT to PATH.")
      p("--cheap-hack")
      p("            Add a blank morphological rule to irules.tdl (if it is")
      p("            empty) to workaround a bug in Cheap.")
      p("--warning (-w)")
      p("            Print warnings when running validate.")
      p("--help (-h) [COMMAND]")
      p("            Print a usage message about COMMAND (if specified) or")
      p("            else all commands and examples.")
      p("")
      indent = 0
      p("COMMANDS:")
      indent = 4
  if command in ('customize', 'c', 'all'):
    p("customize (c) PATH [DEST]")
    p("            Customize the grammar at PATH, with the output written to")
    p("            DEST or the directory at PATH. PATH points to a choices")
    p("            file or a directory that contains a choices file.")
    something_printed = True
  if command in ('customize-and-flop', 'cf', 'all'):
    p("customize-and-flop (cf) PATH [DEST]")
    p("            Customize and flop the grammar at PATH, with the output")
    p("            written to DEST or the directory at PATH. PATH points to a")
    p("            choices file or a directory that contains a choices file.")
    something_printed = True
  if command in ('validate', 'v', 'all'):
    p("validate (v) PATH")
    p("            Validate the choices file at PATH.")
    something_printed = True
  if command in ('regression-test', 'r', 'all'):
    p("regression-test (r) [TEST]")
    p("            Run regression test TEST (if specified) or else all tests.")
    something_printed = True
  if command in ('regression-test-add', 'ra', 'all'):
    p("regression-test-add (ra) CHOICES TXTSUITE")
    p("            Add CHOICES (a choices file) and TXTSUITE (a text file")
    p("            containing test sentences) as a new regression test. Both")
    p("            CHOICES and TXTSUITE are filenames, not paths, and the")
    p("            respective files should exist in the scratch directory")
    p("            (gmcs/regression_tests/scratch/).")
    something_printed = True
  if command in ('regression-test-update', 'ru', 'all'):
    p("regression-test-update (ru) TEST")
    p("            Update the gold standard of TEST to use the results of the")
    p("            current system.")
    something_printed = True
  if command in ('unit-test', 'u', 'all'):
    p("unit-test (u)")
    p("            Run all unit tests.")
    something_printed = True
  if command in ('web-test', 'w', 'all'):
    p("web-test (w)")
    p("            Run all web tests.")
    something_printed = True
  if command in ('web-test-add', 'wa', 'all'):
    p("web-test-add (wa) PATH [comment]")
    p("            Add a new Selenium test with an optional comment.")
    something_printed = True
  if command in ('web-test-remove', 'wr', 'all'):
    p("web-test-remove (wr) TEST")
    p("            Remove a Selenium test.")
    something_printed = True
  if command in ('install', 'i', 'all'):
    p("install (i) PATH")
    p("            Install a custom instance of the Grammar Matrix")
    p("            Customization System and Questionnaire at the PATH")
    p("            specified on the default server (Homer).")
    something_printed = True
  if command in ('vivify', 'v', 'all'):
    p("vivify (v)")
    p("            Install a new version of the Grammar Matrix Customization")
    p("            System and Questionnaire to the live site after verifying")
    p("            the code has been tested and committed to SVN.")
    something_printed = True
  indent = 0
  # Just print the generic usage message if an invalid command was provided
  if not something_printed:
    p("COMMAND [ARGS...]\n")
  if command != 'all':
    p("Try `matrix.py --help' for more information.")
  else:
    p("\nEXAMPLES:")
    p("  matrix.py customize ../choices/Finnish")
    p("  matrix.py cf ../choices/Finnish")
    p("  matrix.py v ../choices/Finnish")
    p("  matrix.py --customizationroot=gmcs/ r")
    p("  matrix.py -C gmcs/ ra Cree_choices Cree_test_suite")
    p("  matrix.py -C gmcs/ install my_matrix")
    p("  matrix.py -C gmcs/ vivify")
  sys.exit(exitcode)

def verify_force():
  print "   You have selected to skip safety checks. Please only do this in"
  print "   rare circumstances when a change is minor and urgently needed. Do"
  print "   not use this option out of impatience or laziness!"
  if utils.verify():
    return True
  print "   Aborted."
  sys.exit(1)

def customize_grammar(path, destination=None, flop=False, cheaphack=False):
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
  # To work around a bug in cheap, we can add a blank morphological rule
  if cheaphack:
    irules_path = os.path.join(grammar_dir, 'irules.tdl')
    if os.path.getsize(irules_path) == 0:
      irules = open(irules_path, 'w')
      print >>irules, 'CHEAP-HACK-RULE :='
      print >>irules, '%suffix (ZZZ_ ZZZ)'
      print >>irules, 'lex-rule.'
      irules.close()
  # Now a grammar has been created, so we can flop it
  if flop:
    import gmcs.choices
    lang = gmcs.choices.get_choice('language', path)
    pet_file = lang.lower() + '-pet.tdl'
    if not os.path.exists(os.path.join(grammar_dir, pet_file)):
      sys.exit("Error: " + pet_file + " not found.")
    cmd = os.path.join(os.environ['LOGONROOT'], 'bin/flop')
    devnull = open('/dev/null', 'w')
    subprocess.call([cmd, pet_file], cwd=grammar_dir,
                    env=os.environ, stderr=devnull)


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

def run_web_tests():
  ensure_customization_root_set()
  try:
    import selenium
  except (NameError):
    sys.stderr.write("Seleinum not installed: run \"pip install -U selenium\"\n")
  cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'], '../install')
#  user_name = raw_input("Patas user name:")
#  subprocess.call([cmd, '-r', '-a', 'patas.ling.washington.edu', '-u', user_name, '/home2/www-uakari/html/matrix/test'], env=os.environ);

  import unittest
  import gmcs.web_tests.testWeb
  loader = unittest.defaultTestLoader
  runner = unittest.TextTestRunner(verbosity=1)
  print 75 * '='
  print 'Web tests:'
  runner.run(loader.loadTestsFromModule(gmcs.web_tests.testWeb))
  print 75 * '='

def add_web_test(filename, comment):
  import re
  file_out = open(filename, "r")
  test_file = open('./gmcs/web_tests/testWeb.py', 'r+')
  write = False;
  new_test = []
  for line in file_out:
    #print line;
    if re.match("class", line):
      write = True
    elif line == "if __name__ == \"__main__\":\n":
      write = False
    if write:
      new_test.append(line)
  file_out.close();
#  print new_test;
#  for line in test_file:
#  if line == "if __name__ == \"__main__\":\n":
  test_file.seek(-47, 2)
  test_file.write(new_test.pop(0))
  if comment is not None:
    test_file.write("    '''"+str(comment)+"'''\n")
  for new_line in new_test:
    test_file.write(new_line)
  test_file.write("if __name__ == \"__main__\":\n")
  test_file.write("    unittest.main()")
  test_file.close()

def remove_web_test(testname):
  import re
  test_file = open('./gmcs/web_tests/testWeb.py', 'r')
  new_lines = []
  keep = True
  for line in test_file:
    if re.match("class", line) or line == "if __name__ == \"__main\":\n":
      if re.match("class "+str(testname), line):
        keep = False
      else:
        keep = True
    if keep:
      new_lines.append(line)
  test_file.close()
  test_file = open('./gmcs/web_tests/testWeb.py', 'w')
  for line in new_lines:
    test_file.write(line)
  test_file.close()

if __name__ == '__main__':
  validate_python_version()
  main()
