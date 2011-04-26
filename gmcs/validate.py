### $Id: validate.py,v 1.44 2008-09-30 23:50:02 lpoulson Exp $

######################################################################
# imports

import sys
import re
import os

from gmcs import tdl
from gmcs.choices import ChoicesFile
from gmcs.utils import get_name

import gmcs.linglib.case
import gmcs.linglib.morphotactics
import gmcs.linglib.negation
from gmcs.linglib import negation


######################################################################
# ValidationResult class

class ValidationResult:
  def __init__(self):
    self.errors = {}
    self.warnings = {}

  def has_errors(self):
    return len(self.errors) != 0

  def has_warnings(self):
    return len(self.warnings) != 0

  def err(self, key, message, anchor=None, concat=True):
    """
    Add an error message to key (a choices variable).  If the key
    already has an error and 'concat' is set to true, concatenate 
    the new message with the existing one. Otherwise replace the 
    message. 
    """
    if key in self.errors and concat:
      self.errors[key].add_message(message)
    else:
      self.errors[key] = ValidationMessage(key+"_error", message, anchor)

  def warn(self, key, message, anchor=None, concat=True):
    """
    Add an warning message to key (a choices variable).  If the key
    already has an warning and 'concat' is set to true, concatenate 
    the new message with the existing one. Otherwise replace the 
    message. 
    """
    if key in self.warnings and concat:
      self.warnings[key].add_message(message)
    else:
      self.warnings[key] = ValidationMessage(key+"_warn", message, anchor)

#NTS: we only need to define an anchor for the main page versionsx
class ValidationMessage:
  def __init__(self, key, text, anchor):
    self.name = key
    self.message = text
    self.href = anchor

  def add_message(self, text):
    self.message += ' ' + text

######################################################################
# Namespace validation
#   A grammar produced by customization consists of several files
#   in TDL that define a number of types.  Some of the names of
#   these types are based on the user's choices.  The system
#   needs to prevent the user from choosing type names that
#   collide with existing types in the system (either types
#   in matrix.tdl or head-types.tdl, or types that the customization
#   system typically uses in grammars).

# type names reserved for the customization system
cust_types = [
  'case-marking-adp-lex',
  'dir-inv-scale',
  'comp-head-phrase',
  'head-comp-phrase',
  'subj-head-phrase',
  'head-subj-phrase',
  'head-final-head-nexus',
  'head-initial-head-nexus',
  'verbal-head-nexus',
  'comp-head-phrase-2',
  'head-comp-phrase-2',
  'head-spec-phrase',
  'bare-np-phrase',
  'comp-aux-phrase',
  'aux-comp-phrase',
  'neg-adv-lex',
  'verb-lex',
  'subj-v-inv-lrule',
  'int-cl',
  'complementizer-lex-item',
  'qpart-lex-item',
  'no-subj-drop-verb-lex',
  'subj-drop-verb-lex',
  'no-obj-drop-verb-lex',
  'obj-drop-verb-lex',
  'subj-drop-only-verb-lex',
  'obj-drop-only-verb-lex',
  'subj-obj-drop-verb-lex',
  'no-drop-verb-lex',
  'noun-lex',
  'obl-spr-noun-lex',
  'no-spr-noun-lex',
  'main-verb-lex',
  'aux-lex',
  'transitive-verb-lex',
  'intransitive-verb-lex',
  'subj-raise-aux',
  'subj-raise-aux-with-pred',
  'subj-raise-aux-no-pred',
  'arg-comp-aux',
  'arg-comp-aux-with-pred',
  'arg-comp-aux-no-pred',
  's-comp-aux',
  's-comp-aux-with-pred',
  's-comp-aux-no-pred',
  'determiner-lex',
  'word-to-lexeme-rule',
  'track'
]

# regex patterns for sets of names that are not available for
# user-defined types
forbidden_type_patterns = [
  'dir-inv-[0-9]+',
  'dir-inv-non-[0-9]+',
  '[a-z]+[0-9]+-top-coord-rule',
  '[a-z]+[0-9]+-mid-coord-rule',
  '[a-z]+[0-9]+-bottom-coord-rule',
  '[a-z]+[0-9]+-left-coord-rule',
  'context[0-9]+-decl-head-opt-subj-phrase'
]

def validate_names(ch, vr):
  # reserved_types contains type names that are not available
  # for user-defined types
  reserved_types = {}

  # read matrix types and head types from file
  try:
    filename = 'matrix-types'
    f = open(filename, 'r')
    for t in f.readlines():
      reserved_types[t.strip()] = True
    f.close()
    filename = os.path.join(os.environ.get('CUSTOMIZATIONROOT', ''),
                            'matrix-core/head-types')
    f = open(filename, 'r')
    for t in f.readlines():
      reserved_types[t.strip()] = True
    f.close()
  except IOError:
    pass

  # add the types from cust_types above to reserved_types
  for ct in cust_types:
    reserved_types[ct] = True

  # if called for by current choices, add reserved types for:
  # case, direction, person, number, pernum, gender, tense, aspect,
  # situation, mood, form, and trans/intrans verb types.
  if ch.get('case-marking', None) is not None:
    reserved_types['case'] = True

  if ch.get('scale', []):
    reserved_types['direction'] = True
    reserved_types['dir'] = True
    reserved_types['inv'] = True

  if ch.pernums():
    reserved_types['pernum'] = True
    persons = [p[0] for p in ch.persons()]
    numbers = [n[0] for n in ch.numbers()]
    for pernum in ch.pernums():
      if pernum[0] not in persons + numbers:
        reserved_types[pernum[0]] = True
  else:
    if ch.persons():
      reserved_types['person'] = True
      for person in ch.persons():
        reserved_types[person[0]] = True
    if ch.numbers():
      reserved_types['number'] = True

  if 'gender' in ch:
    reserved_types['gender'] = True

  if ch.tenses():
    reserved_types['tense'] = True

  if ch.aspects():
    reserved_types['aspect'] = True

  if ch.situations():
    reserved_types['situation'] = True

  if ch.moods():
    reserved_types['mood'] = True

  if ch.forms():
    reserved_types['form'] = True
    reserved_types['finite'] = True
    reserved_types['nonfinite'] = True

  for pattern in ch.patterns():
    p = pattern[0].split(',')
    dir_inv = ''
    if len(p) > 1:
      dir_inv = 'dir-inv-'
    c = p[0].split('-')

    if p[0] == 'intrans':
      reserved_types[dir_inv + 'intransitive-verb-lex'] = True
    elif p[0] == 'trans':
      reserved_types[dir_inv + 'transitive-verb-lex'] = True
    elif len(c) == 1:
      reserved_types[dir_inv +
                     c[0] + '-intransitive-verb-lex'] = True
    else:
      reserved_types[dir_inv +
                     c[0] + '-' + c[1] + '-transitive-verb-lex'] = True

  # fill out the user_types list with pairs:
  #   [type name, variable name]
  user_types = []

  for case in ['nom-acc-nom', 'nom-acc-acc',
               'erg-abs-erg', 'erg-abs-abs',
               'tripartite-s', 'tripartite-a', 'tripartite-o',
               'split-s-a', 'split-s-o',
               'fluid-s-a', 'fluid-s-o',
               'split-n-nom', 'split-n-acc', 'split-n-erg', 'split-n-abs',
               'split-v-nom', 'split-v-acc', 'split-v-erg', 'split-v-abs',
               'focus-focus', 'focus-a', 'focus-o']:
    vn = case + '-case-name'
    if vn in ch:
      user_types += [[ch[vn], vn]]

  for case in ch.get('case', []):
    user_types += [[case.get('name'), case.full_key + '_name']]

  for number in ch.get('number', []):
    user_types += [[number.get('name'), number.full_key + '_name']]

  for gender in ch.get('gender', []):
    user_types += [[gender.get('name'), gender.full_key + '_name']]

  for feature in ch.get('feature', []):
    user_types += [[feature.get('name'), feature.full_key + '_name']]
    for value in feature.get('value', []):
      user_types += [[value.get('name'), value.full_key + '_name']]

  for tense in ['past', 'present', 'future', 'nonpast', 'nonfuture']:
    if tense in ch:
      user_types += [[tense, tense]]
      for st in ch.get(tense + '-subtype', []):
        user_types += [[st.get('name'), st.full_key + '_name']]

  for tense in ch.get('tense', []):
    user_types += [[tense.get('name'), tense.full_key + '_name']]

  for aspect in ch.get('aspect', []):
    user_types += [[aspect.get('name'), aspect.full_key + '_name']]

  for situation in ch.get('situation', []):
    user_types += [[situation.get('name'), situation.full_key + '_name']]

  for mood in ch.get('mood', []):
    user_types += [[mood.get('name'), mood.full_key + '_name']]

  for sf in ch.get('nf-subform', []):
    user_types += [[sf.get('name'), sf.full_key + '_name']]

  for sf in ch.get('fin-subform', []):
    user_types += [[sf.get('name'), sf.full_key + '_name']]

  for noun in ch.get('noun', []):
    user_types += [[get_name(noun) + '-noun-lex',
                    noun.full_key + '_name']]

  for det in ch.get('det', []):
    user_types += [[get_name(det) + '-determiner-lex',
                    det.full_key + '_name']]

  for verb in ch.get('verb', []):
    user_types += [[get_name(verb) + '-verb-lex',
                    verb.full_key + '_name']]
    user_types += [[get_name(verb) + '-dir-inv-lex-rule',
                    verb.full_key + '_name']]
    user_types += [[get_name(verb) + '-dir-lex-rule',
                    verb.full_key + '_name']]
    user_types += [[get_name(verb) + '-inv-lex-rule',
                    verb.full_key + '_name']]

  for pcprefix in ['noun', 'verb', 'det', 'aux']:
    for pc in ch.get(pcprefix + '-pc', []):
      user_types += [[get_name(pc) + '-lex-rule',
                      pc.full_key + '_name']]
      user_types += [[get_name(pc) + '-rule-dtr',
                      pc.full_key + '_name']]
      for lrt in pc.get('lrt', []):
        user_types += [[get_name(lrt) + '-lex-rule',
                        lrt.full_key + '_name']]

  # Cull entries in user_types where there's no type name (and assume
  # these will be caught by other validation).  This could happen, for
  # for example, if the user adds a value for number and specifies a
  # supertype but no name.  We don't want to issue a "duplicate type
  # names" error when '' == ''.
  user_types = [x for x in user_types if x[0]]

  # Force all the user_types into lower case (obeying the appropriate
  # Unicode character semantics), because TDL is case-insensitive.
  user_types = [[unicode(x[0], 'utf-8').lower().encode('utf-8'), x[1]]
                for x in user_types]

  # Whew!  OK, now we have two sets of type names and a set of
  # patterns:
  #
  #   reserved_types: types that users may not use
  #   user_types: the types the user is trying to use
  #   forbidden_type_patterns: user types must not match these
  #
  # Pass through the list of user types, checking each one to see if
  # it's a reserved type or if it matches a forbidden pattern.  Also
  # sort the list by type name, and check to see if we have any
  # duplicates.  Mark all errors on the appropriate variables.

  user_types.sort(lambda x, y: cmp(x[0], y[0]))

  last_was_collision = False
  for i in range(len(user_types)):
    matrix_error = 'You must choose a different name to avoid ' + \
                   'duplicating the internal Matrix type name "' + \
                   user_types[i][0] + '".'

    if user_types[i][0] in reserved_types:
      vr.err(user_types[i][1], matrix_error)

    for forb in forbidden_type_patterns:
      if re.match(forb + '$', user_types[i][0]):
        vr.err(user_types[i][1], matrix_error)

    collision_error = \
      'You must choose a different name to avoid duplicating the ' + \
      'type name "' + user_types[i][0] + '".'

    if i < len(user_types) - 1 and user_types[i][0] == user_types[i+1][0]:
      vr.err(user_types[i][1], collision_error)
      last_was_collision = True
    else:
      if last_was_collision:
        vr.err(user_types[i][1], collision_error)
      last_was_collision = False

    invalids = [t for t in user_types[i][0] if not tdl.isid(t)]
    if len(invalids) > 0:
      vr.err(user_types[i][1],
             '"' + user_types[i][0] + '" contains invalid characters: ' +\
             ''.join(invalids))

######################################################################
# validate_general(ch, vr)
#   Validate the user's choices about general information

def validate_general(ch, vr):
  lang = ch.get('language')

  if not lang:
    vr.err('language', 'You must specify the name of your language')
  else:
    bad_lang = False
    if lang[0] in '.~':
      bad_lang = True
    for c in lang:
      if ord(c) < 32 or c in '?*:<>|/\\"^':
        bad_lang = True
    if bad_lang:
      vr.err('language', 'The language name contains an illegal character')

  if not ch.get('archive'):
    vr.warn('archive',
            'Please answer whether you will allow ' +
            'your answers to be retained.')

######################################################################
# validate_person(ch, vr)
#   Validate the user's choices about person

def validate_person(ch, vr):
  person = ch.get('person')
  fp = ch.get('first-person')

  if not person:
    vr.err('person',
           'You must specify how many persons your language distinguishes.')
  else:
    if person in ['none', '2-non-2', '3-non-3']:
      if fp and fp != 'none':
        vr.err('first-person',
               'If your language does not have the first person, it ' +
               'cannot distinguish sub-values of the first person.')
    if person in ['1-2-3', '1-2-3-4', '1-non-1']:
      if not fp:
        vr.err('first-person',
               'If your language has the first person, you must specify ' +
               'whether it makes finer distinctions within that category.')


######################################################################
# validate_number(ch, vr)
#   Validate the user's choices about number

def validate_number(ch, vr):
  for number in ch.get('number'):
    if 'name' not in number:
      vr.err(number.full_key + '_name',
             'You must specify a name for each number you define.')


######################################################################
# validate_gender(ch, vr)
#   Validate the user's choices about gender

def validate_gender(ch, vr):
  for gender in ch.get('gender'):
    if 'name' not in gender:
      vr.err(gender.full_key + '_name',
             'You must specify a name for each gender you define.')


######################################################################
# validate_other_features(ch, vr)
#   Validate the user's choices about other features

def validate_other_features(ch, vr):
  for feature in ch.get('feature'):
    if 'name' not in feature:
      vr.err(feature.full_key + '_name',
             'You must specify a name for each feature you define.')

    if 'type' not in feature:
      vr.err(feature.full_key + '_type',
             'You must specify a type for each feature you define.')

    for value in feature.get('value', []):
      if 'name' not in value:
        vr.err(value.full_key + '_name',
               'You must specify a name for each value you define.')
      if 'supertype' not in value:
        vr.err(value.full_key + '_supertype1_name',
               'You must specify a supertype for each value you define.')


######################################################################
# validate_word_order(ch, vr)
#   Validate the user's choices about basic word order.

# There should be some value for word order
# If has-dets is true, there should be some value for noun-det-order
# If aux-verb is defined, then aux-order needs to be too
# I'm currently adding AUX as a feature in specialize_word_order()
# but wonder if instead (in addition?) we should be doing validation
# so that we don't find ourselves worrying about auxiliaries if we
# don't have any in the lexicon.

def validate_word_order(ch, vr):
  # General word order
  if (not ch.get('word-order')):
    vr.err('word-order',
           'You must specify a choice for the basic word order.')

  # Things to do with determiners
  if (not ch.get('has-dets')):
    vr.err('has-dets',
           'You must specify whether your language has determiners.')

  if ((ch.get('has-dets') == 'yes') and (not ch.get('noun-det-order'))):
    vr.err('noun-det-order',
           'If your language has determiners, ' +
           'you must specify their order with respect to nouns.')

  if (ch.get('noun-det-order') and (not ch.get('has-dets'))):
    vr.err('has-dets',
           'You specified an order of nouns and dets, ' +
           'but not whether your language has determiners at all.')

  if 'det' in ch and ch.get('has-dets') == 'no':
    vr.err('has-dets',
           'You specified lexical entries for determiners, ' +
           'but said your language has none.')

  #Things to do with auxiliaries
  if (not ch.get('has-aux')):
    vr.err('has-aux',
           'You must specify whether your language has auxiliary verbs.')

  if ((ch.get('has-aux') == 'yes') and (not ch.get('aux-comp-order'))):
    vr.err('aux-comp-order',
           'If your language has auxiliaries, you must specify their order ' +
           'with respect to their complements.')

  if (ch.get('aux-comp-order') and (not ch.get('has-aux'))):
    vr.err('has-aux',
           'You specified an order for auxiliaries and their complements, ' +
           'but not whether your language has auxiliaries at all.')

  if ((ch.get('has-aux') == 'yes') and (not ch.get('aux-comp'))):
    vr.err('aux-comp',
           'If your language has auxiliaries, you must specify ' +
           'whether they take s, vp, or v complements.')

  wo = ch.get('word-order')
  co = ch.get('aux-comp-order')
  ac = ch.get('aux-comp')

  # Added check on whether question on more than one auxiliary is answered.
  # mwg: Antske says this check should only happen if wo is free
  if (wo == 'free' and (ch.get('has-aux') == 'yes') \
      and (not ch.get('multiple-aux'))):
    vr.err('multiple-aux',
           'If your language has free word order and auxiliaries, you must ' +
           'specify whether a clause may contain more than one of them.')

# ASF 2009-12-21 Question on verbal clusters no longer exists, removed all
# related checks.

  if (((wo == 'vso' and co == 'after') or (wo == 'osv' and co == 'before')) and ac == 'vp'):
    vr.err('aux-comp',
           'The general word order and aux-comp order ' +
           'are not compatible with vp complements.')

######################################################################
# validate_coordination(ch, vr)
#   Validate the user's choices about coordination.

def validate_coordination(ch, vr):
  for cs in ch.get('cs'):
    csnum = str(cs.iter_num())

    cs_n =     cs.get('n')
    cs_np =    cs.get('np')
    cs_vp =    cs.get('vp')
    cs_s =     cs.get('s')
    cs_pat =   cs.get('pat')
    cs_mark =  cs.get('mark')
    cs_order = cs.get('order')
    cs_orth =  cs.get('orth')

    if not (cs_n or cs_np or cs_vp or cs_s):
      mess = 'You must specify a phrase type for coordination strategy ' + csnum
      vr.err(cs.full_key + '_n', mess)
      vr.err(cs.full_key + '_np', mess)
      vr.err(cs.full_key + '_vp', mess)
      vr.err(cs.full_key + '_s', mess)

    if cs_pat == 'a':
      if cs_mark:
        mess = 'You must not specify word/affix ' +\
               'for an asyndetic coordination strategy.'
        vr.err(cs.full_key + '_mark', mess)
      if cs_order:
        mess = 'You must not specify before/after ' +\
               'for an asyndetic coordination strategy.'
        vr.err(cs.full_key + '_order', mess)
      if cs_orth:
        mess = 'You must not specify a spelling ' +\
               'for an asyndetic coordination strategy.'
        vr.err(cs.full_key + '_orth', mess)
    else:
      if not cs_pat:
        mess = 'You must specify a pattern ' +\
               'for coordination strategy ' + csnum
        vr.err(cs.full_key + '_pat', mess)
      if not cs_mark:
        mess = 'You must specify word/affix ' +\
               'for coordination strategy ' + csnum
        vr.err(cs.full_key + '_mark', mess)
      if not cs_order:
        mess = 'You must specify before/after ' +\
               'for coordination strategy ' + csnum
        vr.err(cs.full_key + '_order', mess)
      if not cs_orth:
        mess = 'You must specify a spelling ' +\
               'for coordination strategy ' + csnum
        vr.err(cs.full_key + '_orth', mess)

    if cs_mark == 'affix' and (cs_np or cs_vp or cs_s):
      mess = 'Marking coordination with an affix is not yet supported ' +\
             'on phrases (NPs, VPs, or sentences)'
      vr.err(cs.full_key + '_mark', mess)


######################################################################
# validate_yesno_questions(ch, vr)
#   Validate the user's choices about matrix yes/no questions.

def validate_yesno_questions(ch, vr):
  qinvverb = ch.get('q-inv-verb')
  qpartorder = ch.get('q-part-order')
  qpartorth = ch.get('q-part-orth')
  qinfltype = ch.get('q-infl-type')

  if ch.get('q-part'):
    if not qpartorder:
      mess = 'If you chose the question particle strategy ' +\
             'for yes-no questions, you must specify ' +\
             'where the question particle appears.'
      vr.err('q-part-order', mess)
    if not qpartorth:
      mess = 'If you chose the question particle strategy ' +\
             'for yes-no questions, you must specify ' +\
             'the form of the question particle.'
      vr.err('q-part-orth', mess)

  if ch.get('q-inv'):
    #    if qinvverb != 'aux' and qinvverb != 'main' and qinvverb != 'main-aux':
    #      mess = 'There is something wrong with the verb type (main/aux) for inverted questions.  Please contact developers.'
    #      vr.err('q-inv-verb', mess)
    if not qinvverb:
      mess = 'If you chose subject-verb inversion strategy ' +\
             'for yes-no questions, you must specify ' +\
             'which types of verbs invert.'
      vr.err('q-inv-verb', mess)
    if ch.get('word-order') == 'v-final' or \
       ch.get('word-order') == 'v-initial' or \
       ch.get('word-order') == 'free':
      mess = 'Subject-verb inversion strategy for yes-no questions ' +\
             'is not supported for V-final, V-initial, or ' +\
             'free word order languages.  If you believe you have ' +\
             'a counterexample to this, please contact us.'
      vr.err('q-inv', mess)
    if ((qinvverb == 'aux' or qinvverb == 'aux-main') and
        ch.get('has-aux') != 'yes'):
      mess = 'You have not indicated on the word order page ' +\
             'that your language has auxiliaries.'
      vr.err('q-inv-verb', mess)

  if ch.get('q-infl'):
    # need to search inflectional rules for one that specifies 'question'
    ques_aff = any([feat.get('name','') == 'question'
                    for pcprefix in ('noun', 'verb', 'det', 'aux')
                    for pc in ch[pcprefix + '-pc']
                    for lrt in pc.get('lrt',[])
                    for feat in lrt.get('feat',[])])
    if not ques_aff:
      mess = 'If matrix yes-no questions are expressed through affixation, ' +\
             'you must specify an affix with the "question" feature in the ' +\
             'lexicon page.'
      vr.err('q-infl', mess)

   # the above change was implemented as a first-pass to allow a student to
   # complete their grammar, but it should be revised (or reviewed) to make
   # sure it is a correct solution. When it has been revised, please delete
   # the following lines of commented code.
   #
   # if (not ch.get('q-infl-type')):
   #   mess = 'If matrix yes-no questions are expressed through affixation, ' +\
   #          'you must specify what the affix attaches to.'
   #   vr.err('q-infl-type', mess)
   # if (not ch.get('ques-aff')):
   #   mess = 'If matrix yes-no questions are expressed through affixation, ' +\
   #          'you must specify whether it\'s a prefix or a suffix'
   #   vr.err('ques-aff', mess)
   # if (not ch.get('ques-aff-orth')):
   #   mess = 'If matrix yes-no questions are expressed through affixation, ' +\
   #          'you must specify the form of the affix'
   #   vr.err('ques-aff-orth', mess)
   # if ((qinfltype == 'aux' or qinfltype == 'aux-main') and
   #     ch.get('has-aux') != 'yes'):
   #   mess = 'You have not indicated on the word order page ' +\
   #          'that your language has auxiliaries.'
   #   vr.err('q-infl-type', mess)

# validate_tanda(ch, vr)
#   Validate the user's choices about tense, aspect (viewpoint and
#   situation) and form features

def validate_tanda(ch, vr):
  """
  Validate the user's choices about tense, aspect (viewpoint and situation), mood and form features
  """

  ## validate tense
  chosen = False
  ten = ('past', 'present', 'future', 'nonpast', 'nonfuture')
  for t in ten:
    if ch.get(t):
      chosen = True
    elif t + '-subtype' in ch:
      mess = 'You cannot add a subtype if the supertype is not selected.'
      for st in ch[t + '-subtype']:
        vr.err(st.full_key + '_name', mess)

  if ch.get('tense-definition') == 'choose' and not chosen:
    mess = 'You have chosen to select among hierarchy elements. ' +\
           'You need to select at least one tense element.'
    for t in ten:
      vr.err(t, mess)

  if ch.get('tense-definition') == 'build':
    if 'tense' not in ch:
      mess = 'You have chosen to build your own tense hierarchy ' +\
             'so you must enter at least one tense subtype.'
      vr.err('tense-definition', mess)

    for tense in ch.get('tense'):
      if 'name' not in tense:
        vr.err(tense.full_key + '_name',
               'You must specify a name for each tense subtype you define.')
      if 'supertype' not in tense:
        vr.err(tense.full_key + '_supertype1_name',
               'You must specify a supertype for each tense subtype you define.')

  ## validate aspect
  for aspect in ch.get('aspect'):
    if 'name' not in aspect:
      vr.err(aspect.full_key + '_name',
             'You must specify a name for each ' +
             'viewpoint aspect subtype you define.')
    if 'supertype' not in aspect:
      vr.err(aspect.full_key + '_supertype1_name',
             'You must specify at least one supertype for each ' +
             'viewpoint aspect subtype you define.')

  ## validate situation
  for situation in ch.get('situation'):
    if 'name' not in situation:
      vr.err(situation.full_key + '_name',
             'You must specify a name for each ' +
             'situation aspect subtype you define.')
    if 'supertype' not in situation:
      vr.err(situation.full_key + '_supertype1_name',
             'You must specify at least one supertype for each ' +
             'situation aspect subtype you define.')

  ## validate mood
  for mood in ch.get('mood'):
    if 'name' not in mood:
      vr.err(mood.full_key + '_name',
             'You must specify a name for each ' +
             'mood subtype you define.')
    if 'supertype' not in mood:
      vr.err(mood.full_key + '_supertype1_name',
             'You must specify at least one supertype for each ' +
             'mood subtype you define.')

  ## validate form
  if ch.get('has-aux') == 'yes' and ch.get('noaux-fin-nf') == 'on':
    mess = 'You have indicated on the word order page that ' +\
           'your language has auxiliaries.'
    vr.err('noaux-fin-nf', mess)

  if ch.get('has-aux') == 'no' and not (ch.get('noaux-fin-nf') == 'on'):
    if 'nf-subform' in ch:
      mess = 'You have indicated that your language has no auxiliaries ' +\
             'but you have entered subforms of finite or non-finite.'
      vr.err('noaux-fin-nf', mess)

######################################################################
# validate_lexicon(ch, vr)
#   Validate the user's choices about the test lexicon.

def validate_lexicon(ch, vr):

  # Did they specify enough lexical entries?
  if 'noun' not in ch:
    mess = 'You should create at least one noun class.'
    vr.warn('noun1_stem1_orth', mess)

  # Nouns
  for noun in ch.get('noun'):
    det = noun.get('det')

    # Did they answer the question about determiners?
    if not det:
      mess = 'You must specify whether each noun you define takes a determiner.'
      vr.err(noun.full_key + '_det', mess)

    # If they said the noun takes an obligatory determiner, did they
    # say their language has determiners?
    if det == 'obl' and ch.get('has-dets') == 'no':
      mess = 'You defined a noun that obligatorily takes a determiner, ' +\
             'but also said your language does not have determiners.'
      vr.err('has-dets', mess)
      vr.err(noun.full_key + '_det', mess)

    for stem in noun.get('stem', []):
      orth = stem.get('orth')
      pred = stem.get('pred')

      # Did they give a spelling?
      if not orth:
        mess = 'You must specify a spelling for each noun you define.'
        vr.err(stem.full_key + '_orth', mess)

      # Did they give a predicate?
      if not pred:
        mess = 'You must specify a predicate for each noun you define.'
        vr.err(stem.full_key + '_pred', mess)

  # Verbs
  seenTrans = False
  seenIntrans = False
  for verb in ch.get('verb'):
    val = verb.get('valence')

    if not val:
      mess = 'You must specify the argument structure of each verb you define.'
      vr.err(verb.full_key + '_valence', mess)
    elif val[0:5] == 'trans' or '-' in val:
      seenTrans = True
    else:
      seenIntrans = True

    for stem in verb.get('stem', []):
      orth = stem.get('orth')
      pred = stem.get('pred')

      if not orth:
        mess = 'You must specify a spelling for each verb you define.'
        vr.err(stem.full_key + '_orth', mess)

      if not pred:
        mess = 'You must specify a predicate for each verb you define.'
        vr.err(stem.full_key + '_pred', mess)

  if not (seenTrans and seenIntrans):
    mess = 'You should create intransitive and transitive verb classes.'
    vr.warn('verb1_valence', mess)
    vr.warn('verb2_valence', mess)

  # Auxiliaries
  aux_defined = 'aux' in ch
  if ch.get('has-aux') != 'yes':
    if aux_defined:
      mess = 'You have indicated that your language has no auxiliaries ' +\
             'but have entered an auxiliary on the Lexicon page.'
      vr.err('has-aux', mess)

  if ch.get('has-aux') == 'yes':
    if not aux_defined:
      mess = 'You have indicated that your language has auxiliaries. ' +\
             'You must define at least one auxiliary type.'
      vr.err('auxlabel', mess)

  comp = ch.get('aux-comp')
  for aux in ch.get('aux'):
    sem = aux.get('sem')
    pred = aux.get('pred')
    subj = aux.get('subj')

    if 'stem' not in aux:
      mess = 'You must specify a stem for each auxiliary type defined.'
      vr.err(aux.full_key + '_stem1_orth', mess)

    if not sem:
      mess = 'You must specify whether the auxiliary contributes a predicate.'
      vr.err(aux.full_key + '_sem', mess)

    if sem == 'add-pred':
      for feat in aux.get('feat', []):
        if feat.get('name') and not feat.get('value'):
          mess = 'You must specify a value for this feature.'
          vr.err(feat.full_key + '_value', mess)

    if comp == 'vp' or comp == 'v':
      if not subj:
        mess = 'You must specify the subject type.'
        vr.err(aux.full_key + '_subj', mess)

    compform = 'no'
    for cf in aux.get('compfeature', []):
      name = cf.get('name')
      if name == 'form':
        compform = 'yes'
      if name and not cf.get('value'):
        mess = 'You must specify a value for this feature.'
        vr.err(cf.full_key + '_value', mess)

    if not compform == 'yes':
      mess = 'You must specify the form of the verb in the complement, ' +\
             'i.e., the value of the complement feature FORM.'
      vr.err(aux.full_key + '_complabel', mess)


    for stem in aux.get('stem', []):
      if sem == 'add-pred' and not stem.get('pred'):
        mess = 'You have indicated that this type contributes a predicate. ' +\
               'You must specify the predicate name.'
        vr.err(stem.full_key + '_pred', mess)
      if sem != 'add-pred' and stem.get('pred'):
        mess = 'You have specified a predicate but indicated ' +\
               'that this type does not contribute a predicate.'
        vr.err(aux.full_key + '_sem', mess)

  # Determiners
  for det in ch.get('det'):
    for stem in det.get('stem', []):
      if not stem.get('orth'):
        mess = 'You must specify a spelling for each determiner you define.'
        vr.err(stem.full_key + '_orth', mess)

      if not stem.get('pred'):
        mess = 'You must specify a predicate for each determiner you define.'
        vr.err(stem.full_key + '_pred', mess)

  # Adpositions
  for adp in ch.get('adp'):
    if 'feat' not in adp:
      mess = 'You should specify a value for at least one feature (e.g., CASE).'
      vr.warn(adp.full_key + '_feat1_name', mess)

  # For verbs and verbal inflection, we need to prevent that index features are 
  # assigned to verbs: making set of features that should not be assigned to 
  # verbs

  index_feat = ['person', 'number','gender']
  for feature in ch.get('feature'):
    if 'name' in feature:
      if feature.get('type') == 'index':
        index_feat.append(feature.get('name'))



  # Features on all lexical types
  for lextype in ('noun', 'verb', 'aux', 'det', 'adp'):
    for lt in ch.get(lextype):
      for feat in lt.get('feat', []):
        if not feat.get('name'):
          mess = 'You must choose which feature you are specifying.'
          vr.err(feat.full_key + '_name', mess)
        if not feat.get('value'):
          mess = 'You must choose a value for each feature you specify.'
          vr.err(feat.full_key + '_value', mess)

        if feat.get('name') == 'argument structure':
          mess = 'The pseudo-feature "argument structure" is only ' +\
                 'appropriate for inflectional morphemes.  For verbs, ' +\
                 'please use the special argument structure drop-down; ' +\
                 'other lexical types do not yet support argument structure.'
          vr.err(feat.full_key + '_name', mess)

        if lextype == 'verb': # or lextype == 'aux': lap: 1/5/11 removed aux to get rid of overzealous validation

          if not feat.get('head'):
            mess = 'You must choose where the feature is specified.'
            vr.err(feat.full_key + '_head', mess)
          elif feat.get('head') == 'verb' and index_feat.count(feat.get('name')) > 0:
            mess = 'This feature is associated with nouns, please select one of the NP-options.'
            vr.err(feat.full_key + '_head', mess)

        if not ch.has_dirinv() and feat.get('head') in ['higher', 'lower']:
          mess = 'That choice is not available in languages ' +\
                 'without a direct-inverse scale.'
          vr.err(feat.full_key + '_head', mess)

######################################################################
# validate_test_sentences(ch, vr)
#   Validate the user's choices about test sentences.

def validate_test_sentences(ch, vr):
  pass

######################################################################
# validate_extra_constraints()
#   Some extra constraints we want to put on the random grammars
#   for the regression/other testing

def validate_extra_constraints(ch, vr):

  if ch.get('aux-sem') == 'pred':
    mess = 'Only semantically empty auxiliaries in test grammars.'
    vr.err('aux-sem', mess)
  if ch.get('has-dets') == 'yes' and not ch.get('det1_stem1_orth'):
    mess = 'To get uniform semantics, we always want det1 specified.'
    vr.err('det1_stem1_orth', mess)
  if ch.get('cs1_n') != 'on' and ch.get('cs2_n') != 'on':
    mess = 'The test grammars must have some way to coordinate nouns.'
    vr.err('cs1_n', mess)
#  if ch.get('multi-neg') != '':
#    if ch.get('infl-neg') != 'on' or ch.get('adv-neg') != 'on':
#      mess = 'Giving a value for multi-neg means you have selected both neg. strategies.'
#      vr.err('multi-neg', mess)
#   if ch.get('infl-neg') == '':
#     if ch.get('neg-infl-type') != '' or \
#        ch.get('neg-aff') != '' or \
#        ch.get('neg-aff-orth') != '' :
#       mess = 'You have not selected inflectional negation.'
#       vr.err('infl-neg', mess)
#   if ch.get('adv-neg') == '':
#     if ch.get('neg-adv') != '' or \
#        ch.get('neg-mod') != '' or \
#        ch.get('neg-order') != '' or \
#        ch.get('neg-adv') != '' or \
#        ch.get('neg-sel-adv') != '' :
#       mess = 'You have not selected adverbial negation.'
#       vr.err('adv-neg', mess)


######################################################################
# Validation of TDL type names

def validate_types(ch, vr):
  """
  Consider every choice that results in the definition of a type in
  the output TDL, and make sure that (a) the types are legal and (b)
  they're unique.
  """
  pass


######################################################################
# Validation of features and feature values

def validate_features(ch, vr):
  """
  Consider every choice that results in the definition of a feature or
  a feature value.  Make sure that the features are actually defined
  by the current choices, and that values are appropriate for the
  features for which they're specified.
  """
  # Make two lists:
  # 1) a list of feature name variables and their values
  # 2) a list of feature value variables and their values, along with
  #    the feature name each is the value of
  name_list = []
  value_list = []

  for scale in ch.get('scale'):
    for feat in scale.get('feat', []):
      name_list += \
        [[ feat.full_key + '_name', feat.get('name') ]]
      value_list += \
        [[ feat.full_key + '_value', feat.get('name'), feat.get('value') ]]

  for lexprefix in ('noun', 'verb', 'det', 'aux'):
    for lex in ch.get(lexprefix):
      for feat in lex.get('feat', []):
        name_list += \
          [[ feat.full_key + '_name', feat.get('name') ]]
        value_list += \
          [[ feat.full_key + '_value', feat.get('name'), feat.get('value') ]]

  for pcprefix in ('noun', 'verb', 'det', 'aux'):
    for pc in ch.get(pcprefix + '-pc'):
      for lrt in pc.get('lrt', []):
        for feat in lrt.get('feat', []):
          name_list += \
            [[ feat.full_key + '_name', feat.get('name') ]]
          value_list += \
            [[ feat.full_key + '_value', feat.get('name'), feat.get('value') ]]


  for context in ch.get('context',[]):
    for feat in context.get('feat',[]):
      name_list += \
       [[ feat.full_key + '_name', feat.get('name') ]]
      value_list += \
       [[ feat.full_key + '_value', feat.get('name'), feat.get('value') ]]

  # Check the name list to ensure they're all valid features
  features = ch.features()
  for item in name_list:
    var = item[0]   # choices variable name
    name = item[1]  # feature name
    valid = False
    for f in features:
      if f[0] == name:
        valid = True
    if not valid:
      vr.err(var, 'You have selected an invalid feature name.')

  # Check the value list to ensure they're all valid values
  features = ch.features()
  for item in value_list:
    var = item[0] or ''    # choices variable name
    name = item[1] or ''   # feature name
    value = item[2] or ''  # feature value
    for subval in value.split(', '):
      valid = False
      for f in features:
        if f[0] == name:
          for v in f[1].split(';'):
            (vn, vf) = v.split('|')
            if vn == subval:
              valid = True
      if not valid:
        break
    if not valid:
      vr.err(var, 'You have selected an invalid feature value.')

def validate_arg_opt(ch, vr):
  """Check to see if the user completed the necessary portions of the arg
   opt page"""

  if ch.get('subj-drop') and not ch.get('subj-mark-drop'):
    vr.err('subj-mark-drop',
           'You must select whether a subject marker is ' +
           'required, optional, or not permitted with subject dropping.')

  if ch.get('subj-drop') and not ch.get('subj-mark-no-drop'):
    vr.err('subj-mark-no-drop',
           'You must select whether a subject marker is ' +
           'required, optional, or not permitted with an overt subject.')

  if ch.get('obj-drop') and not ch.get('obj-mark-drop'):
    vr.err('obj-mark-drop',
           'You must select whether an object marker is ' +
           'required, optional, or not permitted with object dropping.')

  if ch.get('obj-drop') and not ch.get('obj-mark-no-drop'):
    vr.err('obj-mark-no-drop',
           'You must select whether a object marker is ' +
           'required, optional, or not permitted with an overt object.')

  for context in ch.get('context',[]):
    for feat in context.get('feat',[]):
      if not feat.get('head'):
        mess = 'You must choose where the feature is specified.'
        vr.err(feat.full_key+'_head',mess)


def validate(ch, extra = False):
  """
  Validate the ChoicesFile ch.  Return a ValidationResult that
  contains any errors and warnings.
  """
  vr = ValidationResult()

  validate_names(ch, vr)
  validate_general(ch, vr)
  gmcs.linglib.case.validate(ch, vr)
  validate_person(ch, vr)
  validate_number(ch, vr)
  validate_gender(ch, vr)
  validate_other_features(ch, vr)
  validate_word_order(ch, vr)
  validate_tanda(ch, vr)
  gmcs.linglib.negation.validate(ch, vr)
  validate_coordination(ch, vr)
  validate_yesno_questions(ch, vr)
  validate_lexicon(ch, vr)
  gmcs.linglib.morphotactics.validate(ch, vr)
  validate_test_sentences(ch, vr)

  validate_types(ch, vr)
  validate_features(ch, vr)
  validate_arg_opt(ch, vr)

  if extra:
    validate_extra_constraints(ch, vr)

  return vr

def validate_choices(choices_file, extra = False):
  """
  Validate the choices file found in choices_file.  Return a
  ValidationResult that contains any errors and warnings.
  """
  ch = ChoicesFile(choices_file)
  return validate(ch, extra)


###############################################################
# Allow validate_choices() to be called directly from the
# command line or shell scripts, and print out the errors
# that result.

if __name__ == "__main__":
  vr = validate_choices(sys.argv[1])

  print sys.argv[1]
  for k in vr.errors.keys():
    print '  ' + k + ':'

    print '   ',
    column = 4
    for w in vr.errors[k].split():
      if column + len(w) > 70:
        print
        print '    ' + w,
        column = len(w) + 5
      else:
        print w,
        column += len(w) + 1
    print
  print
