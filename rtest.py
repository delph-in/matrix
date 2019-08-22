#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Grammar Matrix Regression Testing
"""
import os
import traceback
import shutil
import argparse
import pathlib
import fnmatch
import subprocess
import datetime
import textwrap

from pathlib import Path

from delphin import ace
from delphin import tsdb
from delphin.commands import (
    mkprof,
    process,
    compare,
    CommandError
)


# COMMON FILES AND DIRECTORIES ################################################

SCRIPT_DIR     = pathlib.Path(__file__).parent.resolve()
RTEST_DIR      = SCRIPT_DIR / 'gmcs' / 'regression_tests'
CHOICES_DIR    = RTEST_DIR / 'choices'
GRAMMARS_DIR   = RTEST_DIR / 'grammars'
SKELETONS_DIR  = RTEST_DIR / 'skeletons'
RELATIONS_FILE = SKELETONS_DIR / 'Relations'
CURRENT_DIR    = RTEST_DIR / 'home' / 'current'
GOLD_DIR       = RTEST_DIR / 'home' / 'gold'
TXT_SUITE_DIR  = RTEST_DIR / 'txt-suites'
LOGS_DIR       = RTEST_DIR / 'logs'
INDEX          = RTEST_DIR / 'regression-test-index'
DAT_FILENAME   = 'grm.dat'


# REPORT FORMATTING PARAMETERS ################################################

MAX_LINE_WIDTH = 120
RESULT_WIDTH   = 6  # number of spaces reserved for DONE, PASS, FAIL, ERROR
DONE           = 'DONE'
PASS           = 'PASS'
FAIL           = 'FAIL'
ERROR          = 'ERROR'

def linewidth(): return min(MAX_LINE_WIDTH, shutil.get_terminal_size()[0])

# ANSI colors; see: https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
def red(s): return color('\x1b[31m', s)
def green(s): return color('\x1b[32m', s)
def yellow(s): return color('\x1b[33m', s)
def color(clr, s): return '{}{}\x1b[0m'.format(clr, s)
def nocolor(clr, s): return s
def yes_or_no(obj): return red('no') if obj is None else green('yes')

REPORT_COLOR = {
    DONE: green,
    PASS: green,
    FAIL: yellow,
    ERROR: red
}


# EXCEPTIONS ##################################################################

class RegressionTestError(Exception):
    """Raised when a regression test fails for any reason."""


# MAIN FUNCTIONS ##############################################################

def main(args):
    # if no steps are specified, do all of them (but this may be
    # avoided by using --list or --update)
    if not any([args.customize, args.mkskel, args.mkprof,
                args.process, args.compare, args.test]):
        args.customize = True
        args.mkskel = None  # `None` here means "only if needed"
        args.mkprof = True
        args.process = True
        args.compare = True

    if not args.test:
        args.test = ['*']
    if args.index:
        args.index = pathlib.Path(args.index).expanduser()
    else:
        args.index = INDEX
    if args.list:
        list_tests(args)
    elif args.update:
        update_test(args)
    elif args.add:
        if args.test[1] == None and args.test[2] == None:
            raise RegressionTestError('For --add command,  provide a path to the choices file and to the test suite.')
        args.customize = True
        args.mkskel = None
        args.mkprof = True
        args.process = True
        args.compare = False
        add_test(args)
    elif args.remove:
        if args.test[0] == None:
            raise RegressionTestError('For --remove command, add a name for the test to be removed.')
        remove_test(args)
    else:
        run_tests(args)


def run_tests(args):
    """
    Run regression tests and report the results.
    """
    total_passed = 0
    total_error = 0
    total_failed = 0
    total = 0

    for name, desc, chc, dat, txt, skel, prof, gold in _discover(args):
        log = _unique_log_path(name)
        total += 1
        passed = None

        with log.open(mode='at') as logf:
            _lognow('== Testing {} at {} ==\n'
                    .format(name, datetime.datetime.now().isoformat()),
                    logf)
            _progress(name, 'begin', logf)

            try:
                if args.customize:
                    grm = _customize(name, chc, logf)
                    dat = _compile(name, grm, logf)
                # mkskel if requested or if necessary
                if args.mkskel or (args.mkskel is None and skel is None):
                    skel = _mkskel(name, txt, logf)
                if args.mkprof:
                    prof = _mkprof(name, skel, logf)
                if args.process:
                    _process(name, dat, prof, logf)
                if args.compare:
                    passed = _compare(name, prof, gold, logf)

            except Exception:
                _report(name, ERROR, logf)
                _lognow('\n=====', logf)
                traceback.print_exc(file=logf)
                print('  see: {}'.format(str(log)))
                total_error += 1

            else:
                if passed is None:  # only if _compare() was not run
                    _report(name, DONE, logf)
                elif passed:
                    _progress(name, '', logf)  # clear progress only if passed
                    _report(name, PASS, logf)
                    total_passed += 1
                else:
                    _report(name, FAIL, logf)
                    print('  see: {}'.format(str(log)))
                    total_failed += 1

    if args.compare:
        print('\n******** SUMMARY *************')
        width = len(str(total))  # to align the numbers on /
        print('Passed {0:{2}}/{1} tests;'.format(total_passed, total, width))
        print('Failed {0:{2}}/{1} tests;'.format(total_failed, total, width))
        print('Errors {0:{2}}/{1} tests.'.format(total_error, total, width))


def list_tests(args):
    """
    Print each test name and exit.

    If --verbose is used, also print the description, whether the test
    appears in the index, and whether it has an associated txt-suite,
    choices file, skeleton, and gold profile.
    """
    for name, desc, chc, dat, txt, skel, prof, gold in _discover(args):

        print(name)

        if args.verbosity >= 2:
            print('    Description: ', desc)
            print('    Indexed:     ' + yes_or_no(desc))
            print('    Txt-suite:   ' + yes_or_no(txt))
            print('    Choices:     ' + yes_or_no(chc))
            print('    Skeleton:    ' + yes_or_no(skel))
            print('    Gold:        ' + yes_or_no(gold))
            print()


def update_test(args):
    """
    Use the current test profile to the gold.
    """
    tests = list(_discover(args))
    if len(tests) != 1:
        raise RegressionTestError('only 1 test may be updated at a time')
    name, desc, chc, dat, txt, skel, prof, gold = tests[0]

    try:
        db = tsdb.Database(prof)
        tsdb.write_database(db, gold)
    except tsdb.TSDBError as exc:
        raise RegressionTestError('Failed to update gold.') from exc

def add_test(args):
    """
    name = name for the new regression test
    comment = Description
    choices = path to the choices file
    txt = path to the test suite

    Copy the choices and the txt suite to the right locations, renaming them to the new test's name
    if necessary. Create a skeleton using the txt suite. Copy the skeleton to home/gold.
    Create a grammar using the current customization
    system, process the profile. TODO: Add files to SVN.
    """
    name = args.test[0]
    comment = args.test[1]
    choices = args.test[2]
    txt = args.test[3]
    mess = 'File {} already exists; if you want to update the test, use the --update command; ' \
           'if you are sure the file is rogue, delete it. Otherwise, use a different name for a new test.'
    if Path(CHOICES_DIR / name).is_file():
        raise RegressionTestError(mess.format(CHOICES_DIR / name))
    if Path(TXT_SUITE_DIR / name).is_file():
        raise RegressionTestError(mess.format(TXT_SUITE_DIR / name))
    shutil.copy(choices,CHOICES_DIR / name)
    shutil.copy(txt,TXT_SUITE_DIR / name)

    with open(INDEX, 'a+') as regression_test_index:
        all_tests = regression_test_index.readlines()
        # Check that the test name hasn't been used:
        for t in all_tests:
            if t.startswith(name):
                raise RegressionTestError('A test with this name is already in {}. '
                                          'You should use a different name unless you are sure this is a mistake,'
                                          'in which case you should carefully remove this name, along with any '
                                          'rogue files and profiles, from the index, before proceeding.'.format(INDEX))
        # After making sure the name isn't already in the index, add it to the index:
        regression_test_index.write('\n'+name+'='+comment)
    # Now we can actually run the test, creating the skeleton and the current profile.
    run_tests(args)
    try:
        # Need to copy current profile to gold, as at this stage the assumption is they are the same.
        shutil.copytree(CURRENT_DIR / name, GOLD_DIR / name)
    except:
        raise RegressionTestError('Failed to copy the current profile to the gold directory.')
    # Test the new test:
    args.compare = True
    run_tests(args)
    print('New regression test {} added successfully.'.format(name))

def remove_test(args):
    '''
    Remove a test given a name. Will look for:
    1) choices
    2) txt-suite
    3) skeleton
    4) current profile
    5) gold profile
    6) the corresponding line in regression-test-index

    Will silently delete whichever files and directories are found,
    so, good for removing partially created tests.
    TODO: svn del
    '''
    name = args.test[0]
    try:
        if Path.exists(CHOICES_DIR / name):
            print('Deleting {}'.format(CHOICES_DIR / name))
            Path.unlink(CHOICES_DIR / name)
        if Path.exists(TXT_SUITE_DIR / name):
            print('Deleting {}'.format(TXT_SUITE_DIR / name))
            Path.unlink(TXT_SUITE_DIR / name)
        if Path.exists(SKELETONS_DIR / name):
            print('Deleting {}'.format(SKELETONS_DIR / name))
            shutil.rmtree(SKELETONS_DIR / name)
        if Path.exists(CURRENT_DIR / name):
            print('Deleting {}'.format(CURRENT_DIR / name))
            shutil.rmtree(CURRENT_DIR / name)
        if Path.exists(GOLD_DIR / name):
            print('Deleting {}'.format(GOLD_DIR / name))
            shutil.rmtree(GOLD_DIR / name)

        # Recreate regression-test-index:
        shutil.copy(INDEX, str(INDEX)+'-backup')
        with open(INDEX,'r') as regression_test_index:
            lines = regression_test_index.readlines()
        newlines = []
        found = False
        for ln in lines:
            this_name,comment = ln.split('=')
            if this_name == name:
                found = True
            else:
                newlines.append(ln)
        if not found:
            print('Could not find {} in regression-test-index'.format(name))
        else:
            Path.unlink(INDEX)
            with open(INDEX,'w') as new_index:
                new_index.writelines(newlines)
            print('Successfully removed all files and directories associated with {}, and the corresponding '
                  'line in the regression-test-index'.format(name))
    except:
        raise RegressionTestError('Failed to remove regression test {}.'.format(name))

# HELPER FUNCTIONS ############################################################

def _discover(args):
    """
    Find tests by any of their locations.

    Test information is scattered across an index file, a directory of
    sentence files (txt-suites), a directory of choices files, a
    directory of [incr tsdb()] skeleton subdirectories, and a
    directory of gold [incr tsdb()] profiles, joined by a shared test
    name. Collect and merge these sources of information and yield
    each as a tuple of (name, description, txt-suite-path,
    choices-path, skeleton-path, gold-path). If --all-tests is used,
    partially described tests not in the index will be yielded, too.
    """
    index     = _parse_index(args.index)
    choices   = _list_files(CHOICES_DIR)
    grammars  = _list_dat_files(GRAMMARS_DIR)
    txtsuites = _list_files(TXT_SUITE_DIR)
    skeletons = _list_testsuites(SKELETONS_DIR)
    profiles  = _list_testsuites(CURRENT_DIR)
    gold      = _list_testsuites(GOLD_DIR)

    all_names = (set(index)
                 .union(choices)
                 .union(grammars)
                 .union(txtsuites)
                 .union(skeletons)
                 .union(profiles)
                 .union(gold))
    if not args.all_tests:
        all_names = filter(index.__contains__, all_names)
    all_names = sorted(all_names)

    for pattern in args.test:
        for name in fnmatch.filter(all_names, pattern):
            yield (name,
                   index.get(name),
                   choices.get(name),
                   grammars.get(name),
                   txtsuites.get(name),
                   skeletons.get(name),
                   profiles.get(name),
                   gold.get(name))


def _parse_index(path):
    """Map names to descriptions in the index at *path*."""
    index = {}
    for line in open(path):
        line = line.strip()
        if line:
            name, _, description = line.partition('=')
            index[name] = description
    return index


def _list_testsuites(dir):
    """Map basename to path for test suites in *dir*."""
    paths = {}
    for path in dir.glob('*'):
        if tsdb.is_database_directory(path):
            paths[path.name] = path
    return paths


def _list_files(dir):
    """Map basename to path for files in *dir*."""
    paths = {}
    for path in dir.glob('*'):
        if path.is_file() and path.name != 'README':
            paths[path.name] = path
    return paths


def _list_dat_files(dir):
    """Map basename to dat files."""
    paths = {}
    for path in dir.rglob(DAT_FILENAME):
        paths[path.parent.parent.name] = path
    return paths


def _customize(name, chc, logf):
    """Customize the test grammar from a choices file."""
    _progress(name, 'customizing', logf)
    _lognow('  Choices file: {!s}'.format(chc), logf)

    cmd = SCRIPT_DIR / 'matrix.py'
    dest = GRAMMARS_DIR / name
    _lognow('  Destination: {!s}'.format(dest), logf)

    if chc is None:
        # chc is None when no choices file is found, but give a path
        # anyway for logging purposes
        chc = CHOICES_DIR / name
    else:
        # but only create the directory if we already had one
        dest.mkdir(exist_ok=True)
        # should we clear any existing files?

    # need to run this with Python2 until the Matrix is ported to
    # Python3. At that time, we can just call
    # customize.customize_matrix() directly. This way, at least, makes
    # it easy to capture stdout and stderr.
    result = subprocess.run(
        ['python2', str(cmd), 'c', str(chc), str(dest)],
        stdout=logf,
        stderr=logf)

    if result.returncode != 0:
        raise RegressionTestError('Failed to customize.')

    return next(dest.iterdir())


def _compile(name, grm, logf):
    """Compile the test grammar with ACE."""
    _progress(name, 'compiling', logf)
    _lognow('  Grammar directory: {!s}'.format(grm), logf)

    dat = grm / DAT_FILENAME
    _lognow('  Destination: {!s}'.format(dat), logf)
    _lognow('', logf)  # blank line before ACE output

    try:
        ace.compile(grm / 'ace' / 'config.tdl',
                    dat,
                    stdout=logf,
                    stderr=logf)
    except (subprocess.CalledProcessError, OSError) as exc:
        raise RegressionTestError('Failed to compile.') from exc

    return dat


def _mkskel(name, txt, logf):
    """Prepare the skeleton from the txt-suite."""
    _progress(name, 'preparing skeleton', logf)
    _lognow('  Txt-suite: {!s}'.format(txt), logf)

    dest = SKELETONS_DIR / name
    _lognow('  Destination: {!s}'.format(dest), logf)
    if not txt:
        raise RegressionTestError('Did you forget to add the new txt-suite to gmcs/regression_tests/txt-suites?')
    try:
        mkprof(dest, source=txt, schema=RELATIONS_FILE, quiet=True)
    except CommandError as exc:
        raise RegressionTestError('Failed to prepare skeleton.') from exc

    return dest


def _mkprof(name, skel, logf):
    """Prepare the current profile directory and files."""
    _progress(name, 'preparing profile', logf)
    _lognow('  Skeleton path: {!s}'.format(skel), logf)

    dest = CURRENT_DIR / name
    _lognow('  Destination: {!s}'.format(dest), logf)

    try:
        mkprof(dest, source=skel, quiet=True)
    except CommandError as exc:
        raise RegressionTestError('Failed to prepare profile.') from exc

    return dest


def _process(name, dat, prof, logf):
    """Process the input items of the current profile."""
    _progress(name, 'processing', logf)
    _lognow('  Grammar image: {!s}'.format(dat), logf)
    _lognow('  Profile path: {!s}'.format(prof), logf)

    try:
        process(dat, prof, stderr=logf)
    except CommandError as exc:
        raise RegressionTestError('Failed to process profile.') from exc


def _compare(name, prof, gold, logf):
    """Compare the MRSs of the current profile to the gold ones."""
    _progress(name, 'comparing to gold', logf)
    _lognow('  Current profile: {!s}'.format(prof), logf)
    _lognow('  Gold profile: {!s}'.format(gold), logf)

    passed = True
    try:
        for result in compare(prof, gold):
            _progress(name, 'compared i-id={}'.format(result['id']), None)
            _lognow('  {:40} <{},{},{}>'
                    .format(result['id'],
                            result['test'], result['shared'], result['gold']),
                    logf)
            if result['test'] > 0 or result['gold'] > 0:
                passed = False
    except CommandError as exc:
        raise RegressionTestError('Failed to compare to gold.') from exc

    return passed


def _unique_log_path(name):
    """Return a unique log path based on *name* and today's date."""
    basename = name + '-' + datetime.date.today().isoformat()
    log = LOGS_DIR / basename
    i = 1
    while log.is_file():
        log = LOGS_DIR / (basename + '.' + str(i))
        i += 1
    return log


# REPORTING FUNCTIONS #########################################################

def _progress(name, status, logf):
    """
    Update the progress line.

    An empty status message can be used to clear the status.
    """
    if status:
        status = '[{}]'.format(status)
    name_width = linewidth() - RESULT_WIDTH - len(status) - 2
    if name_width < 10:  # minimum for rewriting progress line
        print('{} {}'.format(name, status))
    else:
        if len(name) > name_width:
            _name = name[:(name_width - 3)] + '...'
        else:
            _name = name.ljust(name_width)
        print('\r{} {} '.format(_name, status), end='')

    if status and logf:
        _lognow(status, logf)
        logf.flush()


def _report(name, result, logf):
    """Print the final result."""
    colorize = REPORT_COLOR[result]
    print(colorize(result))
    _lognow('Result: ' + result, logf)


def _lognow(message, logf):
    """Print to the log and flush immediately."""
    print(message, file=logf)
    logf.flush()


# SCRIPT ENTRYPOINT ###########################################################

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=textwrap.dedent('''\
        Regression testing involves a pipeline of independent steps.
        If no steps are specified, all test steps below are executed:

        Step         Requires       Result
        ===========  =============  ============================
        --customize  choices        dat (compiled grammar image)
        --mkprof     skeleton       profile (unprocessed)
        --process    dat, profile   profile (processed)
        --compare    profile, gold  regression test results

        Also, the following are for constructing and updating tests:

        Step         Requires       Result
        ===========  =============  ============================
        --mkskel     txt-suite      skeleton
        --update     profile        gold (profile)
        --add        name, comment  regression test added 
                     choices,
                     txt-suite 
        --remove     name           regression test removed
        '''),
        epilog=textwrap.dedent('''\
        Examples:
            %(prog)s                 # run all indexed regression tests
            %(prog)s --customize     # customize and compile only
            %(prog)s TEST1 "TEST2*"  # run TEST1 and all matching TEST2*
            %(prog)s --mkskel TEST   # build skeleton for TEST
            %(prog)s --update TEST   # update gold profile for TEST
            %(prog)s --add NAME COMMENT CHOICES TXT-SUITE # add TEST
            %(prog)s --remove NAME   # remove TEST
        '''))
    parser.add_argument('-v', '--verbose',
                        action='count',
                        dest='verbosity',
                        default=1,
                        help='increase verbosity')
    parser.add_argument('--index',
                        metavar='PATH',
                        help='path to a test index')
    parser.add_argument('--all-tests',
                        action='store_true',
                        help='don\'t exclude tests not in the index')
    parser.add_argument('-l', '--list',
                        action='store_true',
                        help='list available tests (-v, -vv for more info)')
    parser.add_argument('-c', '--customize',
                        action='store_true',
                        help='customize and compile test grammars')
    parser.add_argument('-s', '--mkskel',
                        action='store_true',
                        help='make test skeletons from txt-suites')
    parser.add_argument('-m', '--mkprof',
                        action='store_true',
                        help='make test profiles from skeletons')
    parser.add_argument('-p', '--process',
                        action='store_true',
                        help='process test profiles with compiled grammars')
    parser.add_argument('-e', '--compare',
                        action='store_true',
                        help='compare test profile to gold')
    parser.add_argument('-u', '--update',
                        action='store_true',
                        help='copy the current profile to gold')
    parser.add_argument('-a', '--add',
                        action='store_true',
                        help='Add a new test from a choices and a txt suite')
    parser.add_argument('-rm','--remove',
                        action='store_true',
                        help='Remove a test from the system')
    parser.add_argument('test',
                        nargs='*')

    args = parser.parse_args()

    main(args)
