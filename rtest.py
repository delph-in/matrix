#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Grammar Matrix Regression Testing
"""

from typing import Tuple
import sys
import traceback
import shutil
import argparse
import pathlib
import fnmatch
import subprocess
import multiprocessing
import functools
import datetime
import textwrap

from delphin import ace
from delphin import tsdb
from delphin.commands import (
    mkprof,
    process,
    compare,
    CommandError
)


# COMMON FILES AND DIRECTORIES ################################################

SCRIPT_DIR = pathlib.Path(__file__).parent.resolve()
RTEST_DIR = SCRIPT_DIR / 'tests' / 'regression'
CHOICES_DIR = RTEST_DIR / 'choices'
GRAMMARS_DIR = RTEST_DIR / 'grammars'
SKELETONS_DIR = RTEST_DIR / 'skeletons'
RELATIONS_FILE = SKELETONS_DIR / 'Relations'
CURRENT_DIR = RTEST_DIR / 'home' / 'current'
GOLD_DIR = RTEST_DIR / 'home' / 'gold'
TXT_SUITE_DIR = RTEST_DIR / 'txt-suites'
LOGS_DIR = RTEST_DIR / 'logs'
INDEX = RTEST_DIR / 'regression-test-index'
DAT_FILENAME = 'grm.dat'

# The following are mostly empty directories so make sure they exist (in case
# someone deletes the entire directory to clear temporary files)

GRAMMARS_DIR.mkdir(exist_ok=True)
CURRENT_DIR.mkdir(exist_ok=True)
LOGS_DIR.mkdir(exist_ok=True)

# These are patterns that rtest should ignore when discovering tests

IGNORE_FILES = {
    'README',
    'README.md',
    '.DS_Store',
}


# MULTIPROCESSING PARAMETERS ##################################################

PROCESSES = None  # max parallel processes for testing; 'None'->os.cpu_count()
BATCH_SIZE = 1  # number of jobs per process to complete before reporting


# REPORT FORMATTING PARAMETERS ################################################

MAX_LINE_WIDTH = 120
RESULT_WIDTH = 6  # number of spaces reserved for DONE, PASS, FAIL, ERROR
PROGRESS_BAR_WIDTH = MAX_LINE_WIDTH  # including [, ], trailing count, etc.
DONE = 'DONE'
PASS = 'PASS'
FAIL = 'FAIL'
ERROR = 'ERROR'
SKIP = 'SKIP'


def linewidth(): return min(MAX_LINE_WIDTH, shutil.get_terminal_size()[0])


# ANSI colors; see: https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
def red(s): return color('\x1b[31m', s)
def boldred(s): return color('\x1b[31m\x1b[1m', s)
def green(s): return color('\x1b[32m', s)
def yellow(s): return color('\x1b[33m', s)
def color(clr, s): return '{}{}\x1b[0m'.format(clr, s)
def nocolor(clr, s): return s
def yes_or_no(obj): return red('no') if obj is None else green('yes')


REPORT_COLOR = {
    DONE: green,
    PASS: green,
    FAIL: red,
    SKIP: yellow,
    ERROR: boldred
}


# EXCEPTIONS ##################################################################

class RegressionTestError(Exception):
    """Raised when a regression test fails for any reason."""


# MAIN FUNCTIONS ##############################################################

def main(args):
    # if no steps are specified, do all of them (but this may be
    # avoided by using --list or --update)
    if not any([args.customize, args.mkskel, args.process, args.compare]):
        args.customize = True
        args.mkskel = None  # `None` here means "only if needed"
        args.process = True
        args.compare = True

    if not args.test:
        args.test = ['*']
    if args.index:
        args.index = pathlib.Path(args.index).expanduser()
    else:
        args.index = INDEX
    if args.add:
        args.function = add_test
    return args.function(args)


def run_tests(args):
    """
    Run regression tests and report the results.
    """
    tests = list(_discover(args))
    total = len(tests)

    run_test = functools.partial(
        _run_test,
        customize=args.customize,
        mkskel=args.mkskel,
        process=args.process,
        compare=args.compare,
        force=args.force or args.skipped,
    )

    if args.debug:
        results = map(run_test, tests)
        totals = _accumulate(results, total)
    else:
        with multiprocessing.Pool(PROCESSES) as pool:
            results = pool.imap(run_test, tests, chunksize=BATCH_SIZE)
            totals = _accumulate(results, total)

    if args.compare:
        print('\n************* SUMMARY *************')
        width = len(str(total))  # to align the numbers on /
        print('Passed  {0:{2}}/{1} tests'.format(totals[PASS], total, width))
        print('Failed  {0:{2}}/{1} tests'.format(totals[FAIL], total, width))
        print('Errors  {0:{2}}/{1} tests'.format(totals[ERROR], total, width))
        if totals[SKIP]:
            print('Skipped {0:{2}}/{1} tests'
                  ' (run rtest.py --list --skipped --verbose for more info)'
                  .format(totals[SKIP], total, width))

    success = totals[ERROR] + totals[FAIL] == 0
    return 0 if success else 1


def _run_test(
        args,
        customize=False,
        mkskel=False,
        process=False,
        compare=False,
        force=False,
) -> Tuple[str, str, pathlib.Path]:
    name, idx, chc, txt, skel, prof, gold = args
    log = _unique_log_path(name)
    result = DONE  # default if no skip, error, failure, or comparison pass

    with log.open(mode='at') as logf:
        _lognow('== Testing {} at {} =='
                .format(name, datetime.datetime.now().isoformat()),
                logf)

        if idx is None:
            _lognow(f'Unknown test (not indexed): {name}\n', logf)
            result = ERROR
        elif not force and idx.get('skip'):
            result = SKIP
        else:
            try:
                if customize or process:
                    grm = _customize(name, chc, logf)
                # mkskel if requested or if necessary
                if mkskel or (mkskel is None and skel is None):
                    skel = _mkskel(name, txt, logf)
                if process:
                    dat = _compile(name, grm, logf)
                    prof = _mkprof(name, skel, logf)
                    _process(name, dat, prof, logf)
                    dat.unlink()
                if compare:
                    passed = _compare(name, prof, gold, logf)
                    result = PASS if passed else FAIL
            except Exception:
                _lognow('\n=====', logf)
                traceback.print_exc(file=logf)
                result = ERROR

        _lognow('\nResult: ' + result, logf)
        return name, result, log


def _accumulate(results, total):
    totals = {
        PASS: 0,
        ERROR: 0,
        FAIL: 0,
        DONE: 0,
        SKIP: 0
    }
    for i, (name, result, logpath) in enumerate(results, 1):
        _report(name, result, logpath)
        print('\r' + _progress_bar(i, total), end='')
        totals[result] += 1
    print()  # end progress bar line
    return totals


def list_tests(args, verbose=False):
    """
    Print each test name and exit.

    If --verbose is used, also print the description, whether the test
    appears in the index, whether it has an associated txt-suite,
    choices file, skeleton, and gold profile, and whether it is to be
    skipped.
    """
    for name, idx, chc, txt, skel, prof, gold in _discover(args):

        print(name)

        if verbose or args.verbosity >= 2:
            desc = None
            skip = ''
            if idx:
                desc = idx.get('description')
                if idx.get('skip'):
                    skip = f'({yellow("skipped")})'
            print('    Description:', desc)
            print('    Indexed:    ', yes_or_no(idx), skip)
            print('    Txt-suite:  ', yes_or_no(txt))
            print('    Choices:    ', yes_or_no(chc))
            print('    Skeleton:   ', yes_or_no(skel))
            print('    Gold:       ', yes_or_no(gold))
            print()


def update_test(args):
    """
    Update the current test profile to the gold.
    """
    tests = list(_discover(args))
    if len(tests) != 1:
        raise RegressionTestError('only 1 test may be updated at a time')
    name, idx, chc, txt, skel, prof, gold = tests[0]

    try:
        db = tsdb.Database(prof)
        tsdb.write_database(db, gold)
    except tsdb.TSDBError as exc:
        raise RegressionTestError('Failed to update gold.') from exc


def add_test(args):
    """
    Add a new regression test.

    Copy the choices and the txt suite to the right locations,
    renaming them to the new test's name if necessary. Create a
    skeleton using the txt suite. Copy the skeleton to home/gold.
    Create a grammar using the current customization system, process
    the profile. It is the developer's responsibility to add these
    files to Git afterwards.
    """
    tests = list(_discover(args))
    if len(tests) == 0 and len(args.test) == 1:
        name = args.test[0]
        idx, chc, txt = None, None, None
    elif len(tests) == 1:
        name, idx, chc, txt, _, _, _ = tests[0]
    else:
        raise RegressionTestError('only 1 test may be added at a time')

    msg = ('File {!s} already exists; if you want to update the test, use '
           'the --update command; if you are sure the file is rogue, delete '
           'it. Otherwise, use a different name for a new test.')
    if idx is not None:
        raise RegressionTestError(
            'test already exists in index: {}'.format(name))
    if chc is not None:
        raise RegressionTestError(msg.format(chc))
    if txt is not None:
        raise RegressionTestError(msg.format(txt))
    chc, txt = args.add

    desc = input('Test description: ')
    index = _parse_index(args.index)
    index[name] = {'description': desc.strip()}

    # Copy files and recreate the index
    shutil.copy(str(chc), str(CHOICES_DIR / name))
    shutil.copy(str(txt), str(TXT_SUITE_DIR / name))
    _recreate_index(index, args.index)

    # Now we can actually run the test, creating the skeleton and the
    # current profile.
    args.customize = True
    args.mkskel = True
    args.process = True
    args.compare = False
    args.force = True  # it would skip otherwise because GOLD is not there
    run_tests(args)
    try:
        # Need to copy current profile to gold, as at this stage the
        # assumption is they are the same.
        shutil.copytree(str(CURRENT_DIR / name), str(GOLD_DIR / name))
    except shutil.Error:
        raise RegressionTestError(
            'Failed to copy the current profile to the gold directory.')

    # Test the new test:
    args.customize = False
    args.mkskel = False
    args.process = False
    args.compare = True
    args.force = False  # should work now
    run_tests(args)

    # list the current state
    list_tests(args, verbose=True)

    print('New regression test {} added successfully.'.format(name))


def remove_test(args):
    '''
    Remove a test from the filesystem.

    This will look for and remove:

    1) choices
    2) grammar directory
    3) txt-suite
    4) skeleton
    5) current profile
    6) gold profile
    7) the corresponding line in regression-test-index

    As this will silently delete whichever files and directories are
    found, it is also good for removing partially created tests. It is
    the developer's responsibility to commit these changes to Git
    afterwards.
    '''
    tests = list(_discover(args))
    if len(tests) > 1:
        raise RegressionTestError('only 1 test may be removed at a time')
    name, idx, chc, txt, skel, prof, gold = tests[0]

    for obj in (chc, txt, skel, prof, gold):
        if obj is not None:
            print('Deleting {!s}'.format(chc))
            if obj.is_file():
                obj.unlink()
            else:
                shutil.rmtree(str(obj))
    # do the grammar dir separately in case it has unexpected depth
    grm = GRAMMARS_DIR / name
    if grm.is_dir():
        shutil.rmtree(str(grm))

    index = _parse_index(args.index)
    if name in index:
        del index[name]
    _recreate_index(index, args.index)

    print('Successfully removed all files and directories associated with '
          '{}, and any corresponding line in the regression-test-index.'
          .format(name))


def clean_up(args):
    '''
    Delete temporary testing files.

    Temporary testing files include:

    1) customized grammars
    2) current profiles
    3) logs
    '''
    count = 0
    for name, _, _, _, _, prof, _ in _discover(args):
        deleted = False
        grm = GRAMMARS_DIR / name
        if grm.exists():
            shutil.rmtree(grm)
            deleted = True
        if prof and prof.exists():
            shutil.rmtree(prof)
            deleted = True
        # a test name may be a substring of another test name, so use a
        # glob pattern that looks like a date to mitigate this
        date_glob = '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]'
        for log in LOGS_DIR.glob(f'{name}-{date_glob}*'):
            log.unlink()
            deleted = True
        if deleted:
            count += 1
            if args.verbosity >= 2:
                print(f'cleaned files for {name}')
    print(f'Finished cleaning files for {count} tests.')


# HELPER FUNCTIONS ############################################################

def _discover(args):
    """
    Find tests by any of their locations.

    Test information is scattered across an index file, a directory of
    sentence files (txt-suites), a directory of choices files, and
    directories of [incr tsdb()] skeletons, current profiles, and gold
    profiles, joined by a shared test name. Collect and merge these
    sources of information and yield each as a tuple of (name,
    index entry, txt-suite-path, choices-path, skeleton-path,
    current-path, gold-path).
    """
    index = _parse_index(args.index)
    choices = _list_files(CHOICES_DIR)
    txtsuites = _list_files(TXT_SUITE_DIR)
    skeletons = _list_testsuites(SKELETONS_DIR)
    profiles = _list_testsuites(CURRENT_DIR)
    gold = _list_testsuites(GOLD_DIR)

    all_names = sorted(
        (set(index)
         .union(choices)
         .union(txtsuites)
         .union(skeletons)
         .union(profiles)
         .union(gold))
    )

    for pattern in args.test:
        for name in fnmatch.filter(all_names, pattern):
            idx = index.get(name)
            chc = choices.get(name)
            skl = skeletons.get(name)
            gld = gold.get(name)
            if args.skipped and (idx is None or not idx.get('skip')):
                continue
            yield (name,
                   idx,
                   chc,
                   txtsuites.get(name),
                   skl,
                   profiles.get(name),
                   gld)


def _parse_index(path):
    """Map names to descriptions in the index at *path*."""
    index = {}
    for line in path.open():
        line = line.strip()
        if line:
            name, data = _parse_index_line(line)
            index[name] = data
    return index


def _recreate_index(index, path):
    """Overwrite the index with an updated one."""
    with path.open('w') as f:
        for name, data in index.items():
            skip = '!' if data.get('skip') else ''
            print(f'{skip}{name}={data["description"]}', file=f)


def _parse_index_line(line):
    """Return the name and associated data for an index entry."""
    data = {}
    if line.startswith('!'):
        data['skip'] = True
        line = line[1:]
    name, _, description = line.partition('=')
    data['description'] = description
    return name, data


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
        if path.is_file() and path.name not in IGNORE_FILES:
            paths[path.name] = path
    return paths


def _customize(name, chc, logf):
    """Customize the test grammar from a choices file."""
    _lognow('\n[customizing]', logf)
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

    # We can just call customize.customize_matrix() directly. This
    # way, at least, makes it easy to capture stdout and stderr.
    result = subprocess.run(
        ['python3', str(cmd), 'cd', str(chc), str(dest)],
        stdout=logf,
        stderr=logf)

    if result.returncode != 0:
        raise RegressionTestError('Failed to customize.')

    return dest


def _compile(name, grm, logf):
    """Compile the test grammar with ACE."""
    _lognow('\n[compiling]', logf)
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
    _lognow('\n[preparing skeleton]', logf)
    _lognow('  Txt-suite: {!s}'.format(txt), logf)

    dest = SKELETONS_DIR / name
    _lognow('  Destination: {!s}'.format(dest), logf)
    if not txt:
        raise RegressionTestError(
            f'Did you forget to add the new txt-suite to {TXT_SUITE_DIR!s}?')
    try:
        mkprof(dest,
               source=txt,
               schema=RELATIONS_FILE,
               skeleton=True,
               quiet=True)
    except CommandError as exc:
        raise RegressionTestError('Failed to prepare skeleton.') from exc

    return dest


def _mkprof(name, skel, logf):
    """Prepare the current profile directory and files."""
    _lognow('\n[preparing profile]', logf)
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
    _lognow('\n[processing]', logf)
    _lognow('  Grammar image: {!s}'.format(dat), logf)
    _lognow('  Profile path: {!s}'.format(prof), logf)

    try:
        process(dat, prof, stderr=logf, report_progress=False)
    except CommandError as exc:
        raise RegressionTestError('Failed to process profile.') from exc


def _compare(name, prof, gold, logf):
    """Compare the MRSs of the current profile to the gold ones."""
    _lognow('\n[comparing to gold]', logf)
    _lognow('  Current profile: {!s}'.format(prof), logf)
    _lognow('  Gold profile: {!s}'.format(gold), logf)

    passed = True
    try:
        for result in compare(prof, gold):
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

def _progress_bar(numerator: int, denominator: int) -> str:
    max_width = min(PROGRESS_BAR_WIDTH, linewidth())
    count_width = len(str(denominator))
    bar_width = max(10, max_width - (count_width * 2) - len('[] (/) '))
    fillcols = int((numerator / denominator) * bar_width)
    fill = '#' * fillcols
    return f'[{fill:<{bar_width}}] ({numerator:>{count_width}}/{denominator})'


def _report(name, result, logpath):
    """Print the final result."""
    colorize = REPORT_COLOR[result]
    name_width = linewidth() - RESULT_WIDTH
    name = _fill(name, name_width)
    print('\r\033[K', end='')  # clear line
    print(f'{name}{colorize(result)}')
    if result in (ERROR, FAIL):
        print('  see: {}'.format(str(logpath)))


def _fill(s: str, width: int) -> str:
    if len(s) > width:
        s = s[:max(0, width - 3)] + '...'
    return s.ljust(width)


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

        Step         Requires           Result
        ===========  =============      ============================
        --customize  choices            customized grammar
        --process    grammar, skeleton  processed profile
        --compare    profile, gold      regression test results

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
            %(prog)s                        # run all indexed regression tests
            %(prog)s --customize            # customize and compile only
            %(prog)s dir-inv-fore "case-*"  # run matching tests
            %(prog)s --mkskel "adj-*"       # build skeleton for adj-*
            %(prog)s --update Zulu          # update gold profile for Zulu
            %(prog)s --add choices txt abc  # add new test named abc
            %(prog)s --remove my-old-test   # remove a test
        '''))
    parser.add_argument('-v', '--verbose',
                        action='count',
                        dest='verbosity',
                        default=1,
                        help='increase verbosity')
    parser.add_argument('--index',
                        metavar='PATH',
                        help='path to a test index')
    parser.add_argument('-l', '--list',
                        const=list_tests,
                        dest='function',
                        action='store_const',
                        help='list available tests (-v, -vv for more info)')
    parser.add_argument('--skipped',
                        action='store_true',
                        help='find skipped/incomplete tests, ignore others')
    parser.add_argument('-c', '--customize',
                        action='store_true',
                        help='customize test grammars')
    parser.add_argument('-s', '--mkskel',
                        action='store_true',
                        help='make test skeletons from txt-suites')
    parser.add_argument('-p', '--process',
                        action='store_true',
                        help='process test profiles with compiled grammars')
    parser.add_argument('-e', '--compare',
                        action='store_true',
                        help='compare test profile to gold')
    parser.add_argument('-u', '--update',
                        const=update_test,
                        dest='function',
                        action='store_const',
                        help='copy the current profile to gold')
    parser.add_argument('-a', '--add',
                        nargs=2,
                        metavar=('CHOICES', 'TXTSUITE'),
                        help='add a new test to the system')
    parser.add_argument('-r', '--remove',
                        const=remove_test,
                        dest='function',
                        action='store_const',
                        help='remove a test from the system')
    parser.add_argument('--clean',
                        const=clean_up,
                        dest='function',
                        action='store_const',
                        help='delete temporary grammars, logs, and profiles')
    parser.add_argument('--debug',
                        action='store_true',
                        help='disable multiprocessing to help debuggers')
    parser.add_argument('test',
                        nargs='*')

    parser.set_defaults(force=False,
                        function=run_tests)

    args = parser.parse_args()

    exit_status = main(args)
    sys.exit(exit_status)
