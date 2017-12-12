#!/usr/bin/python
import sys
import os
import getopt
import subprocess
import random
from gmcs.choices import ChoicesFile
from gmcs.deffile import MatrixDefFile

#import cgi

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
    #                also prints info messages
    show_warnings = False
    # for the install command, if install_lkb is True also install the lkb
    install_lkb = False
    get_iso = False

    # Extract the options and arguments, act on the options, and validate
    # the commands.
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'C:Fhw',
                                   ['customizationroot=', 'CUSTOMIZATIONROOT=',
                                    'force', 'help', 'warning',
                                    'cheap-hack', 'lkb', 'iso'])
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
        elif o == '--lkb':
            install_lkb = True
        elif o == '--iso':
            get_iso = True

    # if CUSTOMIZATIONROOT is not set externally or through an option, try
    # to find an appropriate default directory
    ensure_customization_root_set()
    # make sure the argument have the correct number of parameters
    validate_args(args)

    #######################
    #### CUSTOMIZATION ####

    if args[0] in ('c', 'customize'):
        dest = None
        if len(args) > 2:
            dest = args[2]
        customize_grammar(args[1], destination=dest, cheaphack=cheaphack)

    elif args[0] in ('cd', 'customize-to-destination'):
        dest = None
        if len(args) > 2:
            dest = args[2]
        customize_grammar(path=args[1], destination=dest, cheaphack=cheaphack,force_dest=True)


    elif args[0] in ('cf', 'customize-and-flop'):
        dest = None
        if len(args) > 2:
            dest = args[2]
        customize_grammar(args[1], destination=dest,
                          flop=True, cheaphack=cheaphack)

    elif args[0] in ('uc', 'update-choices'):
        c = ChoicesFile(args[1])
        print c

    #################
    #### TESTING ####

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
            for x in v.infos:
                print x
                print '  ', v.infos[x].message
        # If there are errors, exit with a return code of 1, otherwise 0
        sys.exit(len(v.errors) > 0)

    elif args[0] in ('gm', 'generate-mrs'):
        choices_file = args[1]
        if os.path.isdir(choices_file):
            choices_file = os.path.join(choices_file, 'choices')
        if not os.path.exists(choices_file):
            sys.exit("Error: Choices file not found at " + choices_file)
        import gmcs.choices
        import gmcs.generate
        c = gmcs.choices.ChoicesFile(choices_file)
        for mrs_string in gmcs.generate.configure_mrs(c):
            print mrs_string

    elif args[0] in ('u', 'unit-test'):
        run_unit_tests()

    elif args[0] in ('hv', 'html-validate'):
        if len(args) > 1:
            validate_html(args[1])
        else:
            validate_html('')

    #### REGRESSION TESTS ####

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

    elif args[0] in ('rs', 'regression-test-sample'):
        cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'],
                           'regression_tests/run_regression_tests.sh')
        num = int(args[1])
        testlist = get_regression_tests(['*'])
        lgnames = []
        if (num > len(testlist)):
            sys.exit("\nThere are only "+str(len(testlist))+" tests available.\
                \nPass a smaller argument to 'rs' or just call 'r' with no\
                \narguments to run all tests.\
                \neg:\
                \n$./matrix.py -C gmcs/ r\n")
        else:
            lgnames = random.sample(testlist, num)
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

            # TJT 2014-09-14: Fixing this: it was not adding files to subversion
            rpath = os.path.join(os.environ['CUSTOMIZATIONROOT'], 'regression_tests')
            """
            subprocess.call(['svn', '-q', 'add'] +\
                            [os.path.join(rpath, 'home/gold', lg),
                             os.path.join(rpath, 'skeletons', lg)])
            """
            # os.path.join doesn't want slashes in the list elements
            # http://stackoverflow.com/questions/1945920/os-path-join-python
            subprocess.call(['svn', '-q', 'add'] + \
                            [os.path.join(rpath, 'home', 'gold', lg),
                             os.path.join(rpath, 'skeletons', lg),
                             os.path.join(rpath, 'choices', lg),
                             os.path.join(rpath, 'txt-suites', lg)])
            print 'Succeeded adding files to Subversion. Be sure to commit!'
        except ValueError, er:
            print "Error adding regression test."
            print er.message
        except OSError, er:
            print "OS Error. Most likely Subversion is not installed."
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
            # TJT 2015-02-06: Add to SVN
            rpath = os.path.join(os.environ['CUSTOMIZATIONROOT'], 'regression_tests')
            subprocess.call(['svn', '-q', 'add'] + \
                            [os.path.join(rpath, 'home', 'gold', test)])
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
            try:
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
                # remove the entry from regression-test-index
                rti_path = os.path.join(rpath, 'regression-test-index')
                rti = open(rti_path).readlines()
                rti_file = open(rti_path, 'w')
                for l in rti:
                    if l.split('=')[0] != test:
                        print >>rti_file, l.strip()
                rti_file.close()
            except OSError, er:
                print "OS Error. Most likely Subversion is not installed."
                print er.message
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
        try:
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
            # modify the regression test index after making SVN changes
            rti_file = open(os.path.join(rpath, 'regression-test-index'), 'w')
            for l in rti:
                if l.split('=')[0] == oldname:
                    print >>rti_file, l.replace(oldname, newname, 1)
                else:
                    print >>rti_file, l
            rti_file.close()
        except OSError, er:
            print "OS Error. Most likely Subversion is not installed."
            print er.message

    elif args[0] in ('regression-test-list', 'rl'):
        patterns = ['*']
        if len(args) > 1:
            patterns = args[1:]
        tests = get_regression_tests(patterns)
        if tests is None: return
        for test in tests:
            print test

    #### WEB TESTS ####

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

    ######################
    #### INSTALLATION ####

    elif args[0] in ('i', 'install', 'ih', 'install-homer'):
        # args[1] is the install target location
        if args[1].strip('/') == 'customize':
            # Installations to the live site require validations, so abort.
            print "Error: For installation to the live site, please use:"
            print "  matrix.py vivify"
            sys.exit(2)
        cmd = [os.path.join(os.environ['CUSTOMIZATIONROOT'], '../install')]
        if install_lkb:
            cmd += ['-lkb']
        if get_iso:
            cmd += ['-iso']
        if args[0] in ('ih', 'install-homer'):
            cmd += ['-r']
        subprocess.call(cmd + [args[1]], env=os.environ)

    elif args[0] == 'vivify':
        # pass the force flag in case the user wants to avoid checks
        vivify(force)

    ###############
    #### USAGE ####
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
    elif args[0] in ('cd', 'customize-to-destination'):
        if len(args) < 3: usage(command='customize-to-destination')
    elif args[0] in ('uc', 'update-choices'):
        if len(args) < 2: usage(command='update-choices')
    elif args[0] in ('v', 'validate'):
        if len(args) < 2: usage(command='validate')
    elif args[0] in ('gm', 'generate-mrs'):
        if len(args) < 2: usage(command='generate-mrs')
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
    # Collect and print examples at the end
    examples = []
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
            p("--lkb")
            p("            Install the LKB binaries to a live site.")
            p("--iso")
            p("            Used with install to get the iso code table file from")
            p("            sil.org in order to enable iso639-3 validation.")
            p("--warning (-w)")
            p("            Print warnings and infos when running validate.")
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
        examples += ["  matrix.py customize ../choices/Finnish",
                     "  matrix.py c ../choices/Finnish"]
        something_printed = True
    if command in ('customize-and-flop', 'cf', 'all'):
        p("customize-and-flop (cf) PATH [DEST]")
        p("            Customize and flop the grammar at PATH, with the output")
        p("            written to DEST or the directory at PATH. PATH points to a")
        p("            choices file or a directory that contains a choices file.")
        examples += ["  matrix.py customize-and-flop ../choices/Finnish",
                     "  matrix.py cf ../choices/Finnish"]
        something_printed = True
    if command in ('customize-to-destination', 'cd', 'all'):
        p("customize-to-destination (cd) PATH DEST")
        p("            Customize the grammar at PATH, with the output")
        p("            written directly to DEST. PATH points to a")
        p("            choices file or a directory that contains a choices file.")
        examples += ["  matrix.py customize-to-destination ../choices/Finnish ../grammars/Finnish",
                     "  matrix.py cd ../choices/Finnish ../grammars/Finnish"]
        something_printed = True
    if command in ('update-choices', 'uc', 'all'):
        p("update-choices (uc) PATH")
        p("            Print out an upreved choices file at PATH.")
        examples += ["  matrix.py update-choices ../choices/Finnish/choices",
                     "  matrix.py uc ../choices/Finnish/choices"]
        something_printed = True
    if command in ('validate', 'v', 'all'):
        p("validate (v) PATH")
        p("            Validate the choices file at PATH.")
        examples += ["  matrix.py validate ../choices/Finnish",
                     "  matrix.py v ../choices/Finnish"]
        something_printed = True
    if command in ('generate-mrs', 'gm', 'all'):
        p("generate-mrs (gm) PATH")
        p("            Create MRS strings from MRS templates using the choices")
        p("            file at PATH")
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
        examples += ["  matrix.py --customizationroot=gmcs/ regression-test",
                     "  matrix.py -C gmcs/ r -v",
                     "  matrix.py -C gmcs/ r -cp vso-aux-before-vp Fore"]
        something_printed = True
    if command in ('regression-test-sample', 'rs', 'all'):
        p("regression-test-random (rs) INT")
        p("            Run a random sample of regression tests.")
        p("            The size of the sample is given by the argument")
        p("            which should be a (positive) integer and must ")
        p("            not be greater than the number of defined tests.")
        examples += ["  matrix.py --customizationroot=gmcs/ regression-test-sample 50",
                     "  matrix.py -C gmcs/ rs 100"]
        something_printed = True
    if command in ('regression-test-add', 'ra', 'all'):
        p("regression-test-add (ra) CHOICES TXTSUITE")
        p("            Add CHOICES (a choices file) and TXTSUITE (a text file")
        p("            containing test sentences) as a new regression test. Both")
        p("            CHOICES and TXTSUITE are filenames, not paths, and the")
        p("            respective files should exist in the scratch directory")
        p("            (gmcs/regression_tests/scratch/).")
        examples += ["  matrix.py -C gmcs/ ra Cree_choices Cree_test_suite"]
        something_printed = True
    if command in ('regression-test-update', 'ru', 'all'):
        p("regression-test-update (ru) TEST")
        p("            Update the gold standard of TEST to use the results of the")
        p("            current system.")
        examples += ["  matrix.py -C gmcs/ ru Cree"]
        something_printed = True
    if command in ('regression-test-remove', 'rr', 'all'):
        p("regression-test-remove (rr) TEST")
        p("            Remove TEST from the regression test suite. This command")
        p("            removes all files checked into subversion.")
        examples += ["  matrix.py -C gmcs/ rr tiniest"]
        something_printed = True
    if command in ('regression-test-rename', 'rn', 'all'):
        p("regression-test-rename (rn) OLDTEST NEWTEST")
        p("            Rename OLDTEST to NEWTEST. This is performed with a call")
        p("            to 'svn mv' on the files in the repository. Remember to")
        p("            commit your changes.")
        examples += ["  matrix.py -C gmcs/ rn sujb-drop subj-drop"]
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
    if command in ('html-validate', 'hv', 'all'):
        p("html-validate (hv)")
        p("            Send off html output of matrix.cgi to W3C validator")
        p("            API and receive feedback.")
        something_printed = True
    if command in ('install', 'i', 'all'):
        p("install (i) PATH")
        p("            Install a custom instance of the Grammar Matrix")
        p("            Customization System and Questionnaire at PATH (PATH may")
        p("            be a URL for remote installs).")
        examples += ["  matrix.py -C gmcs install local/dir",
                     "  matrix.py -C gmcs/ --lkb i " + \
                     "uwcl@homer.u.washington.edu:~/public_html/my_matrix"]
        something_printed = True
    if command in ('install-homer', 'ih', 'all'):
        p("install-homer (ih) PATH")
        p("            Install a custom instance of the Grammar Matrix")
        p("            Customization System and Questionnaire at the PATH")
        p("            specified on the default server (Homer).")
        examples += ["  matrix.py -C gmcs/ install-homer my_matrix",
                     "  matrix.py -C gmcs/ --lkb ih my_matrix"]
        something_printed = True
    if command in ('vivify', 'v', 'all'):
        p("vivify (v)")
        p("            Install a new version of the Grammar Matrix Customization")
        p("            System and Questionnaire to the live site after verifying")
        p("            the code has been tested and committed to SVN.")
        examples += ["  matrix.py -C gmcs/ vivify"]
        something_printed = True
    indent = 0
    # Just print the generic usage message if an invalid command was provided
    if not something_printed:
        p("COMMAND [ARGS...]\n")
    p("\nEXAMPLES:")
    for ex in examples:
        p(ex)
    if command != 'all':
        p("\nTry `matrix.py --help' for more information.")
    sys.exit(exitcode)

def verify_force():
    print "   You have selected to skip safety checks. Please only do this in"
    print "   rare circumstances when a change is minor and urgently needed. Do"
    print "   not use this option out of impatience or laziness!"
    if utils.verify():
        return True
    print "   Aborted."
    sys.exit(1)

def customize_grammar(path, destination=None, flop=False, cheaphack=False, force_dest=False):
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
    grammar_dir = gmcs.customize.customize_matrix(path=path, arch_type='tgz', destination=destination, force_dest=force_dest)
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

    #   print_line()
    #   print 'Linglib/Morphotactics tests:'
    #   import gmcs.linglib.tests.testMorphotactics
    #   runner.run(loader.loadTestsFromModule(gmcs.linglib.tests.testMorphotactics))

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
    subprocess.call([cmd, '-lkb','-iso','-r', 'customize'], env=os.environ)

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

def validate_html(arg):
    # takes name of a subpage, or 'main' or no argument = all
    import time
    errors = 0
    print "Checking html validation for "+(arg if arg != '' else 'all')

    # to check any subpages, we'll need a cookie
    # (ie, a session number) that we get by hitting
    # the main page, so go ahead and do this

    cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'],
                       '../matrix.cgi')
    httpstr = os.popen(cmd).read()
    i = httpstr.find('session=')
    sess = httpstr[i+8:i+13]
    os.system('export HTTP_COOKIE="session='+sess+'"')

    # if arg is 'main', ie, main only, send
    # the httpstr off for validation after dropping
    # the http headers

    if arg in ['main','m','']:
        print "main page:"

        # need to drop the first few lines of the reply from
        # matrix.cgi (HTTP headers!), validation starts at
        # doctype
        i = httpstr.lower().find('<!doctype')
        if i == -1:
            print "No doctype string found in reply, validator will complain."
            i = httpstr.lower().find('<html')
            html = httpstr[i:]
        else:
            html = httpstr[i:]

        e = send_page(html)
        if e == 0:
            print "  No errors were found."
        else:
            errors += e

    if arg == '':
        # get the list of subpages by instantiating matrixdef
        md = MatrixDefFile('web/matrixdef')
        print "subpages:"
        for s in md.sections.keys():
            time.sleep(2) # w3c asks for >= 1s  b/t requests
            print "\nsending subpage: ",s,"..."
            cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'],
                               '../matrix.cgi')
            httpstr = os.popen(cmd+' subpage='+s).read()

            i = httpstr.lower().find('<!doctype')
            if i == -1:
                print "No doctype string found in reply, validator will complain."
                i = httpstr.lower().find('<html')
                html = httpstr[i:]
            else:
                html = httpstr[i:]

            e = send_page(html)
            if e == 0:
                print "  No errors were found."
            else:
                errors += e
    else:
        # else run the specific subpage in the arg
        print "\nsending subpage: ",arg,"..."
        cmd = os.path.join(os.environ['CUSTOMIZATIONROOT'],
                           '../matrix.cgi')
        httpstr = os.popen(cmd+' subpage='+arg).read()

        i = httpstr.lower().find('<!doctype')
        if i == -1:
            print "No doctype string found in reply, validator will complain."
            i = httpstr.lower().find('<html')
            html = httpstr[i:]
        else:
            html = httpstr[i:]

        e = send_page(html)
        if e == 0:
            print "  No errors were found."
        else:
            errors += e


    print "Total errors on all checked pages: ",errors



def send_page(page):
    import urllib
    import urllib2
    from xml.dom.minidom import parseString
    values = { 'uploaded_file':page, 'output':'soap12' }
    data = urllib.urlencode(values)
    req = urllib2.Request('http://validator.w3.org/check', data)
    reply = urllib2.urlopen(req).read()

    dom = parseString(reply)
    ecount = int(dom.getElementsByTagName('m:errorcount')[0].firstChild.data)
    try:
        dc = dom.getElementsByTagName('m:doctype')[0].firstChild.data
        print "  Checked document as type:",dc
    except:
        pass
    if ecount > 0:
        print "  The W3C validator found "+str(ecount)+" error(s) in the document submitted."
        errors = dom.getElementsByTagName('m:error')
        for e in errors:
            l = e.getElementsByTagName('m:line')[0].firstChild.data
            s = e.getElementsByTagName('m:source')[0].firstChild.data
            m = e.getElementsByTagName('m:message')[0].firstChild.data
            print "  error at line "+l+":"
            s = s.replace('<strong title="Position where error was detected.">','')
            s = s.replace('</strong>','')
            print "    source: \""+unescape(s)+"\""
            print "    message:\""+unescape(m)+"\""
            print "  --------------------"
    return ecount

def unescape(text):
    import re
    import htmlentitydefs
    def fixup(m):
        text = m.group(0)
        if text[:2] == "&#":
            # character reference
            try:
                if text[:3] == "&#x":
                    return unichr(int(text[3:-1], 16))
                else:
                    return unichr(int(text[2:-1]))
            except ValueError:
                pass
        else:
            # named entity
            try:
                text = unichr(htmlentitydefs.name2codepoint[text[1:-1]])
            except KeyError:
                pass
            return text # leave as is
    return re.sub("&#?\w+;", fixup, text)

if __name__ == '__main__':
    validate_python_version()
    main()
