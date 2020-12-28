#
# $Id: customize.py,v 1.71 2008-09-30 23:50:02 lpoulson Exp $

######################################################################
# imports

import datetime
import os
import shutil
import sys
from subprocess import call

from delphin import tsdb

from gmcs import tdl
from gmcs.choices import ChoicesFile
from gmcs.linglib import adnominal_possession
from gmcs.linglib import adverbs_adpositions
from gmcs.linglib import agreement_features
from gmcs.linglib import argument_optionality
from gmcs.linglib import case
from gmcs.linglib import clausalcomps
from gmcs.linglib import clausalmods
from gmcs.linglib import coordination
from gmcs.linglib import direct_inverse
from gmcs.linglib import features
from gmcs.linglib import information_structure
from gmcs.linglib import lexical_items
from gmcs.linglib import morphotactics
from gmcs.linglib import negation
from gmcs.linglib import nominalized_clauses
from gmcs.linglib import toolboximport
from gmcs.linglib import valence_change
from gmcs.linglib import verbal_features
from gmcs.linglib import word_order
from gmcs.linglib import wh_ques
from gmcs.linglib import yes_no_questions
from gmcs.utils import format_comment_block

######################################################################
# globals

ch = {}

hierarchies = {}

mylang = None
rules = None
irules = None
lrules = None
lexicon = None
roots = None
trigger = None
vpm = None


######################################################################
# customize_punctuation(grammar_path)
#   Determine which punctuation characters to ignore in parsing

def customize_punctuation(grammar_path):
    '''sets up repp preprocessing for lkb according to one of
       three choices on the questionnaire.  '''
    # TODO: pet.set output needs to be updated for
    # current questionnaire choices and for repp!

    default_splits_str = ' \\t!"#$%&\'()\*\+,-\./:;<=>?@\[\]\^_`{|}~\\\\'

    if ch.get('punctuation-chars') == 'keep-all':
        # in this case, we just split on [ \t], and that's
        # what vanilla.rpp already does, so we're done
        return
    elif ch.get('punctuation-chars') == 'discard-all':
        # in this case, "all" punctuation (from the default list)
        # should be split on and dropped
        # to do this we have to build a regex for the : line of
        # the repp file
        #
        filename = os.path.join(grammar_path, 'repp', 'vanilla.rpp')
        with open(filename, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        with open(filename, 'w', encoding='utf-8') as van_rpp:
            for line in lines:
                if line.startswith(':'):
                    line = ":["+default_splits_str+"]"
                print(line.rstrip('\n'), file=van_rpp)
    else:  # ch.get('punctuation-chars') == 'keep-list':
        # keep list with the hyphen on the keep list is the new default
        # here we split on the default list (like discard-all),
        # but *minus* whatevers on the keep list
        chars = list(ch['punctuation-chars-list'])
        if not chars:
            chars = ['-', '=', ':']
        filename = os.path.join(grammar_path, 'repp', 'vanilla.rpp')
        with open(filename, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        with open(filename, 'w', encoding='utf-8') as van_rpp:
            for line in lines:
                if line.startswith(':'):
                    line = line[2:-2]
                    # NOTE: repp syntax says that the line that starts with ':'
                    # defines a list of chars to split on
                    for c in chars:
                        # \ char needs some special treatment
                        # so do the other escaped chars!
                        if c == '\\':
                            c = '\\\\'
                        default_splits_str = default_splits_str.replace(c, '')
                    line = ":["+default_splits_str+"]"
                print(line.rstrip('\n'), file=van_rpp)


######################################################################
# customize_test_sentences(grammar_path)
#   Create the script file entries for the user's test sentences.

def customize_test_sentences(grammar_path):
    try:
        with open(os.path.join(grammar_path, 'lkb/script'), 'r', encoding='utf-8') as b:
            lines = b.readlines()
        with open(os.path.join(grammar_path, 'lkb/script'), 'w', encoding='utf-8') as s, \
                open(os.path.join(grammar_path, 'test_sentences'), 'w', encoding='utf-8') as ts:
            for l in lines:
                l = l.strip()
                if l == ';;; Modules: Default sentences':
                    s.write('(if (eq (length *last-parses*) 1)\n')
                    s.write('   (setf *last-parses* \'(')
                    if 'sentence' not in ch:
                        s.write('""')
                    for sentence in ch.get('sentence', []):
                        s.write('"' + sentence.get('orth', '') + '" ')
                        # 2017-12-13 OZ: Adding two lines below.
                        # # Shouldn't the start be printed in test_sentences
                        # if the sentence is ungrammatical? See choices.py uprev convert_23_to_24() though.
                        if sentence['star'] == 'on':
                            ts.write('*' + sentence.get('orth', '') + '\n')
                        else:
                            ts.write(sentence.get('orth', '') + '\n')
                    s.write(')))\n')
                else:
                    s.write(l + '\n')
    except:
        pass


def customize_itsdb(grammar_path):
    if 'sentence' not in ch:
        return

    today = datetime.datetime.today()
    author = 'Grammar Matrix Customization System'

    def get_item(s, i):
        return {'i-id': str(i),
                'i-origin': 'unknown',
                'i-register': 'unknown',
                'i-format': 'none',
                'i-difficulty': '1',
                'i-category': 'S' if not s.get('star', False) else '',
                'i-input': s['orth'],
                'i-wf': '0' if s.get('star', False) else '1',
                'i-length': str(len(s['orth'].split())),
                'i-author': author,
                'i-date': today}

    skeletons = os.path.join(grammar_path, 'tsdb', 'skeletons')
    matrix_skeleton = os.path.join(skeletons, 'matrix')
    schema = tsdb.read_schema(os.path.join(skeletons, 'Relations'))
    tsdb.initialize_database(matrix_skeleton, schema=schema)
    records = [tsdb.make_record(get_item(s, i), schema['item'])
               for i, s in enumerate(ch['sentence'], 1)]
    tsdb.write(matrix_skeleton, 'item', records, schema['item'])


def customize_script(grammar_path):
    try:
        with open(os.path.join(grammar_path, 'lkb/script'), 'r', encoding='utf-8') as b:
            lines = b.readlines()
        with open(os.path.join(grammar_path, 'lkb/script'), 'w', encoding='utf-8') as s:
            for l in lines:
                l = l.strip()
                if l == ';;; Modules: LOAD my_language.tdl':
                    myl = ch.get('language').lower() + '.tdl'
                    s.write('   (lkb-pathname (parent-directory) "' + myl + '")\n')
                else:
                    s.write(l + '\n')
    except:
        pass

######################################################################
# customize_pettdl()
#


def customize_pettdl(grammar_path):
    try:
        with open(os.path.join(get_matrix_core_path(), 'pet.tdl'), 'r', encoding='utf-8') as p_in:
            lines = p_in.readlines()
        myl = ch.get('language').lower()
        with open(os.path.join(grammar_path, myl + '-pet.tdl'), 'w', encoding='utf-8') as p_out:
            for l in lines:
                l = l.strip()
                p_out.write(l + '\n')
                if l == ':include "matrix".':
                    p_out.write(':include "' + myl + '".\n')
        with open(os.path.join(grammar_path, 'pet/' + myl + '-pet.set'), 'w', encoding='utf-8') as set_out:
            set_out.write(
                ';;;; settings for CHEAP -*- Mode: TDL; Coding: utf-8 -*-\n')
            set_out.write('include "flop".\n')
            set_out.write('include "pet".\n')
    except:
        pass

######################################################################
# customize_acetdl()
#


def customize_acetdl(grammar_path):
    myl = ch.get('language').lower()
    ace_config = os.path.join(grammar_path, 'ace', 'config.tdl')
    replace_strings = {'mylanguage': os.path.join('..', myl + '-pet.tdl')}
    with open(ace_config, 'r', encoding='utf-8') as a_in:
        lines = a_in.read()
    with open(ace_config, 'w', encoding='utf-8') as a_out:
        print(lines % replace_strings, file=a_out)

######################################################################
# customize_roots()
#   Create the file roots.tdl


def customize_roots():
    comment = \
        'A sample start symbol: Accept fully-saturated verbal\n' + \
        'projections only; if a grammar makes use of the head-subject and\n' + \
        'head-complement types as provided by the Matrix, this should be a\n' + \
        'good starting point.  Note that it is legal to have multiple start\n' + \
        'symbols, but they all need to be listed as the value of\n' + \
        '`*start-symbol*\' (see `lkb/user-fns.lsp\').'

    # ERB 2007-01-21 Need to add [MC +] for inversion strategy for
    # questions, but it's hard to see how this could hurt in general,
    # so let's just put it in.

    # CMC 2017-02-26 Changed root from phrase to sign.
    # Also added corrected non-local-none constraint from (spurious) roots.tdl
    typedef = \
        'root := sign & \
           [ SYNSEM [ LOCAL [ CAT [ VAL [ SUBJ < >, \
                                          COMPS < > ], \
                                    MC + ],\
                              COORD - ], \
                      NON-LOCAL non-local-none ] ].'
    roots.add(typedef, comment)

    if 'form-fin-nf' in ch:
        roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ].')

    # ERB 2006-10-05 I predict a bug here:  If we a language with auxiliaries
    # and question particles, we're going to need to make sure that FORM is
    # compatible with comp.

    # TJT 2014-08-16 set root condition to the proper head including
    # adjective if language contains stative predicate adjectives
    has_stative_predicate_adjectives = False
    for (key, value) in ch.walk():
        if "predcop" in key.lower():
            if ch.get(key).lower() in ("imp", "opt"):
                has_stative_predicate_adjectives = True
                break
    has_question_particles = bool(ch.get('q-part'))

    if has_question_particles and has_stative_predicate_adjectives:
        roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD +vjc ].')
    elif has_question_particles:
        roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD +vc ].')
    elif has_stative_predicate_adjectives:
        roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD +vj ].')
    else:
        roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD verb ].')

    comment = \
        'This start symbol allows you to parse single words as stand-alone\n' + \
        'utterances.  This can be useful for grammar debugging purposes.'
    typedef = \
        'lex-root := word-or-lexrule.'
    roots.add(typedef, comment)


######################################################################
# customize_vpm()
# Automatically create semi.vpm blocks.

def customize_vpm(ch, vpm, hierarchies):
    # Add default values to the file semi.vpm
    vpm.add_literal("""; A basic VPM for Matrix grammars.
event          <> e
ref-ind        <> x
individual     <> i
handle         <> h
non_event      <> p
*              >> u
semarg         << u

SPECI : SPECI
  * <> *

SF : SF
  prop <> prop
  ques <> ques
  prop-or-ques >> prop-or-ques
  prop << prop-or-ques
  comm <> comm
  * <> *

COG-ST : COG-ST
  type-id <> type-id
  uniq-id <> uniq-id
  familiar <> familiar
  activated <> activated
  in-foc <> in-foc
  activ+fam <> activ+fam
  uniq+fam <> uniq+fam
  activ-or-more <> activ-or-more
  uniq-or-less <> uniq-or-less
  uniq+fam+act <> uniq+fam+act
  fam-or-more <> fam-or-more
  fam-or-less <> fam-or-less
  uniq-or-more <> uniq-or-more
  activ-or-less <> activ-or-less
  * <> *
""")
    # Add customized mappings
    agreement_features.create_vpm_blocks(ch, vpm, hierarchies)
    verbal_features.create_vpm_blocks(ch, vpm, hierarchies)

######################################################################
# Version Control
#   Use shell commands to setup Mercurial or Bazaar, if the user
#   has specified that they want one or the other.


def setup_vcs(ch, grammar_path):
    if 'vcs' in ch:
        with open(os.devnull, 'w') as IGNORE:
            cwd = os.getcwd()
            os.chdir(grammar_path)
            try:
                if ch['vcs'] == 'git':
                    call(['git', 'init'], stdout=IGNORE, stderr=IGNORE)
                    call(['git', 'add', '.'], stdout=IGNORE, stderr=IGNORE)
                    call(['git', 'commit',
                          '--author="Grammar Matrix <matrix-dev@u.washington.edu>"',
                          '-m "Initial commit."'], stdout=IGNORE, stderr=IGNORE)
                elif ch['vcs'] == 'hg':
                    call(['hg', 'init'], stdout=IGNORE, stderr=IGNORE)
                    call(['hg', 'add'], stdout=IGNORE, stderr=IGNORE)
                    call(['hg', 'commit',
                          '-u Grammar Matrix <matrix-dev@u.washington.edu>',
                          '-m "Initial commit."'], stdout=IGNORE, stderr=IGNORE)
                elif ch['vcs'] == 'bzr':
                    call(['bzr', 'init'], stdout=IGNORE, stderr=IGNORE)
                    call(['bzr', 'add'], stdout=IGNORE, stderr=IGNORE)
                    call(['bzr', 'whoami', '--branch',
                          'Grammar Matrix Customization System <matrix-dev@uw.edu>'],
                         stdout=IGNORE, stderr=IGNORE)
                    call(['bzr', 'commit', '-m "Initial commit."'],
                         stdout=IGNORE, stderr=IGNORE)
            except OSError as er:
                print("OS Error. Most likely %s is not installed." % ch['vcs'])
                print(er.message)
            os.chdir(cwd)

######################################################################
# customize_matrix(path)
#   Create and prepare for download a copy of the matrix based on
#   the choices file in the directory 'path'.  This function
#   assumes that validation of the choices has already occurred.


def customize_matrix(path, arch_type, destination=None, force_dest=False):
    if os.path.isdir(path):
        path = os.path.join(path, 'choices')
    # if no destination dir is specified, just use the choices file's dir
    destination = destination or os.path.dirname(path)

    global ch
    ch = ChoicesFile(path)

    language = ch['language']

    if force_dest:
        grammar_path = destination
    else:
        grammar_path = get_grammar_path(ch.get('iso-code', language).lower(),
                                        language.lower(), destination)

    # delete any existing contents at grammar path
    if os.path.exists(grammar_path):
        shutil.rmtree(grammar_path)
    # the rsync command won't create the target dirs, so do it now
    os.makedirs(grammar_path)

    # Use the following command when python2.6 is available
    # shutil.copytree('matrix-core', grammar_path,
    #                ignore=shutil.ignore_patterns('.svn'))
    with open(os.devnull, 'w') as IGNORE:
        try:
            call(['rsync', '-a', '--exclude=.svn',
                  get_matrix_core_path() + os.path.sep, grammar_path],
                 stdout=IGNORE, stderr=IGNORE)
        except OSError as er:
            print("OS Error. Most likely rsync is not installed.")
            print(er.message)
            sys.exit(1)

    # include a copy of choices (named 'choices' to avoid collisions)
    shutil.copy(path, os.path.join(grammar_path, 'choices'))

    # Create TDL object for each output file
    global mylang, rules, irules, lrules, lexicon, roots
    mylang = tdl.TDLfile(os.path.join(grammar_path, language.lower() + '.tdl'))
    mylang.define_sections([['addenda', 'Matrix Type Addenda', True, False],
                            ['features', 'Features', True, False],
                            ['dirinv', 'Direct-Inverse', True, False],
                            ['lextypes', 'Lexical Types', True, True],
                            ['nounlex', 'Nouns', False, False],
                            ['verblex', 'Verbs', False, False],
                            ['auxlex', 'Auxiliaries', False, False],
                            ['coplex', 'Copulas', False, False],
                            ['adjlex', 'Adjectives', False, False],
                            ['subordlex', 'Subordinators', True, False],
                            ['complex', 'Complementizers', False, True],
                            ['otherlex', 'Others', False, False],
                            ['lexrules', 'Lexical Rules', True, False],
                            ['phrases', 'Phrasal Types', True, False],
                            ['coord', 'Coordination', True, False]])
    rules = tdl.TDLfile(os.path.join(grammar_path, 'rules.tdl'))
    irules = tdl.TDLfile(os.path.join(grammar_path, 'irules.tdl'))
    lrules = tdl.TDLfile(os.path.join(grammar_path, 'lrules.tdl'))
    lexicon = tdl.TDLfile(os.path.join(grammar_path, 'lexicon.tdl'))
    roots = tdl.TDLfile(os.path.join(grammar_path, 'roots.tdl'))
    trigger = tdl.TDLfile(os.path.join(grammar_path, 'trigger.mtr'))
    trigger.add_literal(';;; Semantically Empty Lexical Entries')
    vpm = tdl.TDLfile(os.path.join(grammar_path, 'semi.vpm'))

    # date/time
    try:
        with open('datestamp', 'r') as f:
            matrix_dt = f.readlines()[0].strip()
    except:
        matrix_dt = 'unknown time'

    current_dt = datetime.datetime.utcnow()
    tdl_dt = current_dt.strftime('%a %b %d %H:%M:%S UTC %Y')
    lisp_dt = current_dt.strftime('%Y-%m-%d_%H:%M:%S_UTC')

    # Put the current date/time in my_language.tdl...
    mylang.add_literal(
        ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n' +
        ';;; Grammar of ' + ch.get('language') + '\n' +
        ';;; created at:\n' +
        ';;;     ' + tdl_dt + '\n' +
        ';;; based on Matrix customization system version of:\n' +
        ';;;     ' + matrix_dt + '\n' +
        ';;;\n' + format_comment_block(ch.get('comment')) + '\n' +
        ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;')

    # Put the date/time of the Matrix version in Version.lsp (along
    # with the name of the language).
    global version_lsp
    version_lsp = tdl.TDLfile(os.path.join(grammar_path, 'Version.lsp'))

    version_lsp.add_literal('(in-package :common-lisp-user)\n\n' +
                            '(defparameter *grammar-version* \"' +
                            ch.get('language') + ' (' + lisp_dt + ')\")')

    # Initialize various type hierarchies
    case.init_case_hierarchy(ch, hierarchies)
    agreement_features.init_agreement_hierarchies(ch, mylang, hierarchies)
    verbal_features.init_verbal_hierarchies(ch, hierarchies)

    # Integrate choices related to lexical entries imported from
    # Toolbox lexicon file(s), if any.  NOTE: This needs to be called
    # before anything else that looks at the lexicon-related choices,
    # so before lexical_items.insert_ids().
    toolboximport.integrate_imported_entries(ch)

    # Create unique ids for each lexical entry; this allows
    # us to do the same merging on the lexicon TDL file as we
    # do on the other TDL files.  NOTE: This needs to be called
    # before customize_lexicon() and customize_inflection()
    lexical_items.insert_ids(ch)
    # Currently, yes-no questions need to go before the lexicon
    # because the lexicon needs to know whether there are obligatory question particles,
    # to determine the head on the question-embedding verb.
    yes_no_questions.customize_yesno_questions(
        mylang, ch, rules, lrules, hierarchies, roots)

    # The following might modify hierarchies in some way, so it's best
    # to customize those components and only have them contribute their
    # information to lexical rules when we customize inflection.
    lexical_items.customize_lexicon(
        mylang, ch, lexicon, trigger, hierarchies, rules)
    information_structure.customize_information_structure(
        mylang, ch, rules, irules, lexicon, trigger, hierarchies)
    argument_optionality.customize_arg_op(mylang, ch, rules, hierarchies)
    direct_inverse.customize_direct_inverse(ch, mylang, hierarchies)
    case.customize_case(mylang, ch, hierarchies)

    # after all structures have been customized, customize inflection,
    # but provide the methods the components above have for their own
    # contributions to the lexical rules

    nominalized_clauses.customize_nmcs(mylang, ch, rules, roots)
    negation.customize_sentential_negation(
        mylang, ch, lexicon, rules, lrules, hierarchies)

    # save the hierarchies to the choices object
    ch['hierarchies'] = hierarchies

    add_lexrules_methods = [case.add_lexrules,
                            argument_optionality.add_lexrules,
                            valence_change.add_lexrules,
                            direct_inverse.add_lexrules,
                            wh_ques.add_lexrules]
    to_cfv = morphotactics.customize_inflection(ch, add_lexrules_methods,
                                                mylang, irules, lrules, lexicon)

    # customize_feature_values is called by process_cfv_list
    # negation.py needs to run first!

    features.process_cfv_list(mylang, ch, hierarchies, to_cfv)

    # Call the other customization functions
    agreement_features.customize_agreement_features(mylang, hierarchies)
    adnominal_possession.customize_adnominal_possession(
        mylang, ch, rules, irules, lexicon, hierarchies)
    verbal_features.customize_verbal_features(mylang, hierarchies)
    valence_change.customize_valence_change(
        mylang, ch, lexicon, rules, lrules, hierarchies)
    word_order.customize_word_order(mylang, ch, rules)
    coordination.customize_coordination(mylang, ch, lexicon, rules, irules)
    clausalmods.customize_clausalmods(
        mylang, ch, lexicon, rules, roots, trigger)
    clausalcomps.customize_clausalcomps(mylang, ch, lexicon, rules)
    adverbs_adpositions.customize_adv_adp(ch, mylang, rules)
    wh_ques.customize_wh_ques(mylang, ch, rules, roots)

    # Customization having to do with punctuation, [incr tsdb()],
    # parsers, roots, and vpm.
    customize_punctuation(grammar_path)
    customize_test_sentences(grammar_path)
    customize_itsdb(grammar_path)
    customize_script(grammar_path)
    customize_pettdl(grammar_path)
    customize_acetdl(grammar_path)
    customize_roots()
    customize_vpm(ch, vpm, hierarchies)

    # Save the output files
    mylang.save()
    rules.save()
    irules.save()
    lrules.save()
    lexicon.save()
    roots.save()
    trigger.save()
    vpm.save()
    version_lsp.save()

    # Setup version control, if any
    setup_vcs(ch, grammar_path)

    return grammar_path


def get_matrix_core_path():
    # customizationroot is only set for local use. The installation for
    # the questionnaire does not use it.
    cr = os.environ.get('CUSTOMIZATIONROOT', '')
    if cr:
        cr = os.path.join(cr, '..')
    return os.path.join(cr, 'matrix-core')


def get_grammar_path(isocode, language, destination):
    '''
    Using the language or iso-code, get a unique pathname
    for the grammar directory.
    '''
    # three possibilities for dir names. If all are taken, raise an exception
    for dir_name in [isocode, language, isocode + '_grammar']:
        if dir_name == '':
            continue
        grammar_path = os.path.join(destination, dir_name.replace(' ', '_'))
        # if grammar_path already exists as a file, it is likely the choices file
        if not (os.path.exists(grammar_path) and os.path.isfile(grammar_path)):
            return grammar_path
    raise Exception("Grammar directory not available.")

###############################################################
# Allow customize_matrix() to be called directly from the
# command line or shell scripts.


if __name__ == "__main__":
    customize_matrix(sys.argv[1], 'tgz')
