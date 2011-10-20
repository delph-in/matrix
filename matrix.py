#!/usr/bin/python
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
    lgnames = get_regression_tests(args[1:])
    if lgnames is None:
        sys.exit('No regression tests found for %s' % str(args[1:]))
    #Using subprocess makes it difficult to kill the process
    # (e.g. with Ctrl-C), so we need to handle KeyboardInterrupts
    # (or alternatively use a os.exec* function)
    try:
      p = subprocess.Popen([cmd] + lgnames, env=os.environ)
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
                      [os.path.join(rpath, 'home/gold', lg),
                       os.path.join(rpath, 'skeletons', lg)])
      subprocess.call(['svn', '-q', 'add'] +\
                      [os.path.join(rpath, 'home/gold', lg, '/[a-z]*'),
                       os.path.join(rpath, 'skeletons', lg, '/[a-z]*'),
                       os.path.join(rpath, 'choices', lg),
                       os.path.join(rpath, 'txt-suites', lg)])
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

  elif args[0] in ('regression-test-remove', 'rr'):
    from gmcs import utils
    test = args[1]
    rpath = os.path.join(os.environ['CUSTOMIZATIONROOT'], 'regression_tests')
    test_paths = []
    for test_path in [os.path.join(rpath, 'home', 'gold', test),
                      os.path.join(rpath, 'skeletons', test),
                      os.path.join(rpath, 'choices', test),
                      os.path.join(rpath, 'txt-suites', test)]:
      if os.path.exists(test_path):
        test_paths += [test_path]
    print "The following paths were found relating to the test:\n"
    for test_path in test_paths:
      print "  ", test_path
    print
    print "Do you want to remove them from subversion? If you choose to remove"
    print "them, the test entry in regression-test-index will also be removed."
    if utils.verify():
      # remove the entry from regression-test-index
      rti_path = os.path.join(rpath, 'regression-test-index')
      rti = open(rti_path).readlines()
      rti_file = open(rti_path, 'w')
      for l in rti:
        if l.split('=')[0] != test:
          print >>rti_file, l.strip()
      rti_file.close()
      # remove the relevant files from subversion
      for test_path in test_paths:
        subprocess.call(['svn', '-q', '--non-interactive', 'rm', test_path])
      # All done, print a success message and reminder
      print "Remember you must commit your changes to subversion. Also,"
      print "there may still be files not in the repository related to this"
      print "test. Look in the following directories:"
      print os.path.join(rpath, 'grammars')
      print os.path.join(rpath, 'home', 'current')
      print os.path.join(rpath, 'logs')
    else:
      print "Aborted."
    sys.exit(1)

  elif args[0] in ('regression-test-rename', 'rn'):
    oldname = args[1]
    newname = args[2]
    rpath = os.path.join(os.environ['CUSTOMIZATIONROOT'], 'regression_tests')
    rti = open(os.path.join(rpath, 'regression-test-index')).readlines()
    if oldname not in (l.split('=')[0] for l in rti):
      print 'Error: cannot find test', oldname
      sys.exit(2)
    rti_file = open(os.path.join(rpath, 'regression-test-index'), 'w')
    for l in rti:
      if l.split('=')[0] == oldname:
        print >>rti_file, l.replace(oldname, newname, 1)
      else:
        print >>rti_file, l
    rti_file.close()
    subprocess.call(['svn', '-q', '--non-interactive', 'mv',
                     os.path.join(rpath, 'home', 'gold', oldname),
                     os.path.join(rpath, 'home', 'gold', newname)])
    subprocess.call(['svn', '-q', '--non-interactive', 'mv',
                     os.path.join(rpath, 'skeletons', oldname),
                     os.path.join(rpath, 'skeletons', newname)])
    subprocess.call(['svn', '-q', '--non-interactive', 'mv',
                     os.path.join(rpath, 'choices', oldname),
                     os.path.join(rpath, 'choices', newname)])
    subprocess.call(['svn', '-q', '--non-interactive', 'mv',
                     os.path.join(rpath, 'txt-suites', oldname),
                     os.path.join(rpath, 'txt-suites', newname)])
    print "Remember you must commit your changes to subversion. Also,"
    print "there may still be files not in the repository related to this"
    print "test. Look in the following directories:"
    print os.path.join(rpath, 'grammars')
    print os.path.join(rpath, 'home', 'current')
    print os.path.join(rpath, 'logs')

  elif args[0] in ('regression-test-list', 'rl'):
    patterns = ['*']
    if len(args) > 1:
        patterns = args[1:]
    tests = get_regression_tests(patterns)
    if tests is None: return
    for test in tests:
        print test

  elif args[0] in ('i', 'install'):
    cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'], '../install')
    location = args[1]
    # Installations to the live site require validations, so abort.
    if location.strip('/') == 'matrix/customize':
      print "Error: For installation to the live site, please use:"
      print "  matrix.py vivify"
      sys.exit(2)
    subprocess.call([cmd, '-lkb', '-r', location], env=os.environ)

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
  elif args[0] == 'import-lex':
    import gmcs.linglib.toolboximport
    gmcs.linglib.toolboximport.import_toolbox_lexicon(args[1])

  elif args[0] == 'integrate-lex':
    import gmcs.linglib.toolboximport
    import gmcs.choices
    ch = gmcs.choices.ChoicesFile(args[1])
    gmcs.linglib.toolboximport.integrate_imported_entries(ch)


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
  cwd = os.getcwd()
  if 'CUSTOMIZATIONROOT' in os.environ:
    if not os.path.exists(os.path.join(os.environ['CUSTOMIZATIONROOT'],
                                       'customize.py')):
      print "CUSTOMIZATIONROOT is incorrectly set."
      sys.exit(2)
  elif os.path.exists(os.path.join(cwd, 'customize.py')):
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
  elif args[0] in ('regression-test-remove', 'rr'):
    if len(args) < 2: usage(command='regression-test-remove')
  elif args[0] in ('regression-test-rename', 'rn'):
    if len(args) < 3: usage(command='regression-test-rename')
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
    p("regression-test [-TASK] [TESTS]")
    p("            Run regression test TASK (or all tasks if unsprecified)")
    p("            over TEST (or all tests if unspecified). TASKS can be any")
    p("            of the following and can be combined (e.g. -vc):")
    p("              [none]       : run all tests")
    p("              -v : validate and report errors")
    p("              -c : customize and report errors")
    p("              -p : customize and parse, report differences with gold")
    p("            TESTS can be a single test name or a list of names.")
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
  if command in ('regression-test-remove', 'rr', 'all'):
    p("regression-test-remove (rr) TEST")
    p("            Remove TEST from the regression test suite. This command")
    p("            removes all files checked into subversion.")
    something_printed = True
  if command in ('regression-test-rename', 'rn', 'all'):
    p("regression-test-rename (rn) OLDTEST NEWTEST")
    p("            Rename OLDTEST to NEWTEST. This is performed with a call")
    p("            to 'svn mv' on the files in the repository. Remember to")
    p("            commit your changes.")
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
    p("  matrix.py -C gmcs/ r -v")
    p("  matrix.py -C gmcs/ r -cp vso-aux-before-vp Fore")
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
    if not os.path.exists(irules_path) or \
       not any(':=' in line for line in open(irules_path, 'r')):
      irules = open(irules_path, 'a')
      print >>irules, 'CHEAP-HACK-RULE :='
      print >>irules, '%prefix (xyx zyz)'
      print >>irules, 'lex-rule & [ NEEDS-AFFIX + ].'
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

  print_line()
  print 'Toolbox import tests:'
  import gmcs.linglib.tests.testToolboxImport
  runner.run(loader.loadTestsFromModule(gmcs.linglib.tests.testToolboxImport))

  #print_line()
  #print 'Linglib/Morphotactics tests:'
  #import gmcs.linglib.tests.testMorphotactics
  #runner.run(loader.loadTestsFromModule(gmcs.linglib.tests.testMorphotactics))

  print_line()

def get_regression_tests(patterns):
  import fnmatch
  rpath = os.path.join(os.environ['CUSTOMIZATIONROOT'], 'regression_tests')
  if isinstance(patterns, basestring):
      patterns = [patterns]
  names = []
  for line in open(os.path.join(rpath, 'regression-test-index')):
      if line.strip() == '': continue
      line = line.split('=')[0]
      if any(fnmatch.fnmatch(line, p) for p in patterns):
        names += [line]
  if len(names) == 0 and patterns != []:
    return None
  return names

def vivify(force):
  # Before vivifying, make sure the following have occurred:
  #  1. Regression tests have been run if any code has been altered
  #     since the last vivification.
  #  2. There are no remaining modifications not checked into SVN.
  cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'], '../install')
  subprocess.call([cmd, '-lkb', '-r', 'matrix/customize'], env=os.environ)

def run_web_tests():
  ensure_customization_root_set()
  try:
    import selenium
  except (NameError):
    sys.stderr.write("Seleinum not installed: run \"pip install -U selenium\"\n")
  cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'], '../install')
  user_name = raw_input("Patas user name:")
  subprocess.call([cmd, user_name+'@patas.ling.washington.edu:/home2/www-uakari/html/matrix/test'], env=os.environ);

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
