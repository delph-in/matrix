### encoding: utf8
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
import gmcs.linglib.lexicon
import gmcs.linglib.clausalcomps


######################################################################
# ValidationResult class

class ValidationResult:
    def __init__(self):
        self.errors = {}
        self.warnings = {}
        self.infos = {}

    def has_errors(self):
        return len(self.errors) != 0

    def has_warnings(self):
        return len(self.warnings) != 0

    def has_infos(self):
        return len(self.infos) != 0

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
        already has a warning and 'concat' is set to true, concatenate
        the new message with the existing one. Otherwise replace the
        message.
        """
        if key in self.warnings and concat:
            self.warnings[key].add_message(message)
        else:
            self.warnings[key] = ValidationMessage(key+"_warning", message, anchor)

    def info(self, key, message, anchor=None, concat=True):
        """
        Add an informational message to key (a choices variable).  If the key
        already has a message and 'concat' is set to true, concatenate
        the new message with the existing one.  Otherwise replace the message.
        """
        if key in self.infos and concat:
            self.infos[key].add_message(message)
        else:
            self.infos[key] = ValidationMessage(key+"_info", message, anchor)

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
# TJT 2014-08-25: Making into tuple for speed
cust_types = (
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
    # TJT 2014-08-25: adding mylanguage.tdl adjective and copula types
    'stative-pred-adj-lex',
    'basic-copula-verb-lex',
    'adj-comp-copula-verb-lex',
    'adj_incorporation-lex-rule',
)

# regex patterns for sets of names that are not available for
# user-defined types
forbidden_type_patterns = [
    'dir-inv-[0-9]+',
    'dir-inv-non-[0-9]+',
    '[a-z]+[0-9]+-top-coord-rule',
    '[a-z]+[0-9]+-mid-coord-rule',
    '[a-z]+[0-9]+-bottom-coord-rule',
    '[a-z]+[0-9]+-left-coord-rule',

    #type names that collide with the type names used by mtr.tdl
    '[aeihpuxAEIHPUX]',

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
            type_name = t.strip()
            reserved_types[type_name] = True
        f.close()
    except IOError:
        pass

    # add the types from cust_types above to reserved_types
    for ct in cust_types:
        reserved_types[ct] = True

    # if called for by current choices, add reserved types for:
    # case, direction, person, number, pernum, gender, tense, aspect,
    # situation, mood, form, nominalization, and trans/intrans verb types.
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

    if 'ns' in ch:
        reserved_types['nominalization'] = True

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

    for sf in ch.get('form-subtype', []):
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

    # TJT 2014-08-25: Adding adj + cop; changing to tuple for speed
    for pcprefix in ('noun', 'verb', 'det', 'aux', 'adj', 'cop'):
        for pc in ch.get(pcprefix + '-pc', []):
            user_types += [[get_name(pc) + '-lex-rule',
                            pc.full_key + '_name']]
            user_types += [[get_name(pc) + '-rule-dtr',
                            pc.full_key + '_name']]
            for lrt in pc.get('lrt', []):
                user_types += [[get_name(lrt) + '-lex-rule',
                                lrt.full_key + '_name']]

    for ns in ch.get('ns'):
        user_types += [[ns.get('name'), ns.full_key + '_name']]

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
                   '"' + user_types[i][0] + '" contains invalid characters: ' + \
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

    iso = ch.get('iso-code')
    if len(iso) != 3:
        vr.warn('iso-code', 'ISO-639 codes should be three letter sequences.')
    else:
        url = "http://www.ethnologue.com/show_language.asp?code="+iso
        try:
            valid = False
            f = open('iso.tab')
            lines = f.readlines()
            for l in lines:
                toks = l.split('\t')
                if iso in toks[0]:
                    vr.info('iso-code', 'ISO 693-3 suggests the reference name for your language is: '+toks[6], anchor=url)
                    valid = True
            f.close()
            if not valid:
                vr.warn('iso-code',
                        'The three-letter code you provided is not in ISO-639-3.')
        except IOError:
            sys.stderr.write('''
[iso-code not validated] Get the latest code table file from sil.org and put it in
your installation root directory as iso.tab to enable iso validation:
\n$wget http://www.sil.org/iso639-3/iso-639-3_20120206.tab -O iso.tab\n\n''')

    if not ch.get('archive'):
        vr.warn('archive',
                'Please answer whether you will allow ' +
                'your answers to be retained.')

    if not ch.get('punctuation-chars'):
        vr.warn('punctuation-chars',
                'Please provide an answer about tokenization and punctuation ' + \
                'characters.')
    chars = unicode(ch.get('punctuation-chars-list',''), 'utf8')
    if chars:
        if ' ' in chars:
            vr.err('punctuation-chars',
                   'Spaces are not allowed as parsable punctuation.')
        if chars.count(',') > 1:
            vr.warn('punctuation-chars',
                    'No delimiters are necessary for the punctuation string.' + \
                    'If a comma appears in the string, it will become parsable.')

    # check punctuation chars in all orth fields.
    # LLD 12-03-2015 Excluded test sentences from this check, since they often
    # contain punctuation that is meant to be discarded.
    non_chars = [re.escape(c)
                 for c in u'!"&\'()*+,−./\;<>?@[]^`{|}~。！？…．　○●◎＊☆★◇◆'
                 if c not in chars]
    char_re = re.compile(r'(' + r'|'.join(non_chars) + r')')
    for (key, val) in ch.walk():
        if key.endswith('orth') and not key.startswith('sentence'):
            if char_re.search(unicode(val, 'utf8')):
                vr.warn(key, 'String contains an unparsable punctuation character.' + \
                        ' Please see the General subpage.')

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
                   'You must specify a name for each feature.')

        if 'type' not in feature:
            vr.err(feature.full_key + '_type',
                   'You must specify a type for each feature.')

        if 'cat' not in feature:
            vr.err(feature.full_key + '_cat',
                   'You must specify a category for each feature.')

        if 'new' not in feature:
            vr.err(feature.full_key + '_new',
                   'You must specify whether you want to use an existing value type or define a new one.')
        else:
            if feature['new'] == 'no':
                if 'existing' not in feature:
                    vr.err(feature.full_key + '_existing',
                           'You must specify which existing value type is used.')
                if 'value' in feature:
                    vr.err(feature.full_key + '_new',
                           'You must check [define a new value type] if you add a value.')
            else:
                if 'value' not in feature:
                    vr.err(feature.full_key + '_new',
                           'You must add a value if you check [define a new value type].')

        for value in feature.get('value', []):
            if 'name' not in value:
                vr.err(value.full_key + '_name',
                       'You must specify a name for each value you define.')
            if 'supertype' not in value:
                vr.err(value.full_key + '_supertype1_name',
                       'You must specify a supertype for each value you define.')



######################################################################
# validate_information_structure_msg(ch, vr, marker, _type, msg)
#   Leave an error msg for information structural affixes
def validate_information_structure_msg(ch, vr, marker, _type, msg):
    for m in ch.get(marker):
        if m['type'].strip() == _type:
            vr.err(m.full_key + '_type', msg)

######################################################################
# validate_information_structure_affix(ch, marker, values)
#   Validate the user's choices about information structure for each value
def validate_information_structure_affix(ch, marker, values):
    for cat in ['noun-pc', 'verb-pc']:
        for pc in ch.get(cat):
            for lrt in pc.get('lrt', []):
                for feat in lrt.get('feat', []):
                    if feat['name'] == 'information-structure meaning' and feat['value'] in values:
                        return True
    return False

######################################################################
# validate_information_structure_adp(ch, marker, values)
#   Validate the user's choices about information structure for each value
def validate_information_structure_adp(ch, marker, values):
    for adp in ch.get('adp'):
        for feat in adp.get('feat', []):
            if feat['name'] == 'information-structure meaning' and feat['value'] in values:
                return True
    return False

######################################################################
# validate_information_structure(ch, vr)
#   Validate the user's choices about information structure

def validate_information_structure(ch, vr):
    infostr_values = {
        'focus-marker' : ['focus', 'semantic-focus', 'contrast-focus', 'focus-or-topic', 'contrast-or-focus', 'non-topic'],
        'topic-marker' : ['topic', 'aboutness-topic', 'contrast-topic', 'frame-setting-topic', 'focus-or-topic', 'contrast-or-topic', 'non-focus'],
        'c-focus-marker' : ['contrast', 'contrast-focus', 'contrast-or-focus'],
        'c-topic-marker' : ['contrast', 'contrast-topic', 'contrast-or-topic']
    }

    infostr_markers = []
    for marker in infostr_values.keys():
        for m in ch.get(marker):
            if m['type'].strip() == 'affix' and marker not in infostr_markers:
                infostr_markers.append(marker)
                break
    for m in infostr_markers:
        if not validate_information_structure_affix(ch, m, infostr_values[m]):
            validate_information_structure_msg(ch, vr, m, 'affix', 'You must create at least one affix involving this feature on Morphology.')

    infostr_markers = []
    for marker in infostr_values.keys():
        for m in ch.get(marker):
            if m['type'].strip() == 'adp' and marker not in infostr_markers:
                infostr_markers.append(marker)
                break
    for m in infostr_markers:
        if not validate_information_structure_adp(ch, m, infostr_values[m]):
            validate_information_structure_msg(ch, vr, m, 'adp', 'You must create at least one adposition involving this feature on Lexicon.')



    for marker in infostr_values.keys():
        for m in ch.get(marker):
            if m['type'].strip() in ['affix', 'adp']:
                if m['pos'].strip() != '' or m['cat'].strip() != '' or m['orth'].strip() != '':
                    vr.err(m.full_key + '_type', 'You must either check a modifier or delete the choices following a modifier.')


    if ch.get('word-order') == 'free':
        warning_msg = 'Information structural modules for free word order languages are under development. If positions are multiply checked, your grammar may have some overgeneration.'
        if ch.get('focus-pos') != '' and ch.get('topic-first') != '':
            if ch.get('focus-pos') != 'clause-initial':
                vr.warn('focus-pos', warning_msg)

        if ch.get('focus-pos') != '' and ch.get('c-focus-pos') != '':
            if ch.get('focus-pos') != ch.get('c-focus-pos') != '':
                vr.warn('focus-pos', warning_msg)

        if ch.get('c-focus-pos') != '' and ch.get('topic-first') != '':
            if ch.get('c-focus-pos') != 'clause-initial':
                vr.warn('topic-first', warning_msg)


    if ch.get('topic-first') != '' and ch.get('topic-marker') != '':
        vr.warn('topic-first', 'You may need some additional constraint(s) on sentence positioning of topic-marked constituents.')

    if ch.get('focus-pos') == '' and ch.get('c-focus') != '':
        vr.err('c-focus', 'You must check a specific position for focus above.')
    if ch.get('c-focus') != '' and ch.get('c-focus-pos') != '':
        vr.err('c-focus', 'Your description is inconsistent. You must either check this or choose a specific position below.')



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

    # matrix-dev group rejected this validation step in Jan 2012
    # if (ch.get('has-dets') == 'yes') and (not 'det' in ch):
    #  vr.err('has-dets',
    #         'You specified that your language has determiners, ' +
    #         'you must define them on the lexicon page.')


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

    #OZ 2017-11-13 Validate subordinate clauses word order.

    if (ch.get('subord-word-order')):
        if not (wo == 'v2' or ch.get('subord-word-order') == 'same'):
            vr.err('subord-word-order',
                   'V-final subordinate word order is ' +
                   'only supported with V2 matrix order.')
        elif wo == 'v2' and ch.get('subord-word-order') == 'vfinal' \
                and ch.get('has-aux') == 'yes' and ch.get('aux-comp') != 'v':
            vr.err('aux-comp','The only supported choice for auxiliary complement '
                   'type for v2/vfinal word order combination is V.')


######################################################################
# validate_coordination(ch, vr)
#   Validate the user's choices about coordination.

def validate_coordination(ch, vr):
    used_patterns = set() # used later to check which aps were used in a cs
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
                mess = 'You must not specify word/affix ' + \
                       'for an asyndetic coordination strategy.'
                vr.err(cs.full_key + '_mark', mess)
            if cs_order:
                mess = 'You must not specify before/after ' + \
                       'for an asyndetic coordination strategy.'
                vr.err(cs.full_key + '_order', mess)
            if cs_orth:
                mess = 'You must not specify a spelling ' + \
                       'for an asyndetic coordination strategy.'
                vr.err(cs.full_key + '_orth', mess)
        else:
            if not cs_pat:
                mess = 'You must specify a pattern ' + \
                       'for coordination strategy ' + csnum
                vr.err(cs.full_key + '_pat', mess)
            if not cs_mark:
                mess = 'You must specify word/affix ' + \
                       'for coordination strategy ' + csnum
                vr.err(cs.full_key + '_mark', mess)
            if not cs_order:
                mess = 'You must specify before/after ' + \
                       'for coordination strategy ' + csnum
                vr.err(cs.full_key + '_order', mess)
            if not cs_orth:
                mess = 'You must specify a spelling ' + \
                       'for coordination strategy ' + csnum
                vr.err(cs.full_key + '_orth', mess)

        if cs_mark == 'affix' and (cs_np or cs_vp or cs_s):
            mess = 'Marking coordination with an affix is not yet supported ' + \
                   'on phrases (NPs, VPs, or sentences)'
            vr.err(cs.full_key + '_mark', mess)

        ##########################################################
        ### validation for agreement patterns attached to a cs ###
        ##########################################################

        # first, quickly check whether a cs has an ap but not for all arguments
        subj = False
        obj = False
        for csap in cs.get('csap'):
            target = csap.get('target')
            if target == 'all':
                subj = True
                obj = True
            elif target == 'subj':
                subj = True
            elif target == 'obj':
                obj = True
        if cs.get('csap') and (subj == False or obj == False):
            mess = 'You have added an agreement pattern but have not accounted for both subjects and objects. ' \
                   'In general, \'subject/object only\' is used with languages that, for example, use distinguished conjunct for ' \
                   'subject and feature resolution for the object. \'All arguments\' should be the default in most other cases.'
            vr.warn(csap.full_key+'_target', mess)



        # setup for tracking whether subjects or objects have been accounted for too often
        subj = False
        obj = False
        valid = True

        for csap in cs.get('csap'):
            used_patterns.add(csap.get('pat')) # used to check for unused aps later

            # ap named in a cs must exist
            if not ch.get(csap.get('pat')):
                mess = 'You have set this coordination strategy to use an agreement pattern that ' \
                       'doesn\'t exist.'
                vr.err(csap.full_key+"_pat", mess)

            # an ap must apply to N or NPs to do anything (although it won't break anything)
            if not (cs_n or cs_np):
                mess = 'You have attached an agreement pattern to this coordination strategy, but it won\'t do anything because ' \
                       'the coordination strategy doesn\'t coordinate nouns or NPs.'
                vr.warn(csap.full_key + "_pat", mess)

            # TODO flag if a cs has an ap but doesn't use them for all arguments (this is probably a mistake)

            # only one dconj pattern per subject/object per cs
            if csap.get('pat').startswith('dconj'):
                target = csap.get('target')
                if target == 'all':
                    if (subj == True or obj == True):
                        valid = False
                    subj = True
                    obj = True
                elif target == 'subj':
                    if subj == True:
                        valid = False
                    subj = True
                elif target == 'obj':
                    if obj == True:
                        valid = False
                    obj = True
                if not valid:
                    valid = True
                    mess = 'You can\'t choose more than one distinguished conjunct pattern for subject or objects \
                  within one coordination strategy.'
                    vr.err(csap.full_key+"_pat", mess)

    # feature resolution validation
    for fr in ch.get('fr'):
        feats = set()
        features = ch.features()

        # warn if a fr has been defined but not used
        if fr.full_key not in used_patterns:
            mess = "You defined an agreement pattern but didn't attach it to a coordination strategy. You must attach it to a" \
                   " coordination strategy before the rules will apply to coordinated N/NPs."
            vr.warn(fr.full_key+ '_name', mess)

        # have to have features defined
        # TODO not sure whether this actually needs to be an error instead of a warning (does it crash?)
        if not fr.get('feat'):
            mess = 'You have defined an agreement pattern but not added any features or rules.'
            vr.err(fr.full_key+'_name', mess)

        for feat in fr.get('feat'):
            # no feature in the same agreement pattern more than once
            if feat['name'] in feats and feat['name'] != 'pernum':
                mess = 'You have used this feature more than once in the same feature resolution pattern.'
                vr.err(feat.full_key + '_name', mess)
            feats.add(feat['name'])

            # features must exist in the grammar
            valid = False
            for f in features:
                if f[0] == feat['name']:
                    valid = True
            if not valid:
                mess = 'You have chosen a feature that does not exist in the grammar.'
                vr.err(feat.full_key + '_name', mess)

            # feature values must also exist
            values = ['any', 'same', 'nonmatching', ' ']
            for f in features:
                if f[0] == feat['name']:
                    for v in f[1].split(';'):
                        (vn, vf) = v.split('|')
                        values += [vn]

            # check all feature values used in rules
            for rule in feat.get('rule'):
                # break up the list into its constituent values
                left_rule_list = rule.get('left').split(", ")

                # no "any" in a list
                if len(left_rule_list) > 1 and "any" in left_rule_list:
                    mess = 'This list of feature values contains \"any,\" which shouldn\'t be necessary.' \
                           'The \'any\' value means that the value is underspecified, which should encompass all other values of that' \
                           ' feature, so a list shouldn\'t be necessary.'
                    vr.err(rule.full_key + "_left", mess)

                # no "same" in a list
                if len(left_rule_list) > 1 and "same" in left_rule_list:
                    mess = "This list contains \'the same,\' which is a special value that shouldn't be in a list. " \
                           "See the MatrixDoc pages for more information on how to use it."
                    vr.err(rule.full_key + "_left", mess)

                # no list + "the same" - possibly a misunderstanding of how to use "the same"
                if len(left_rule_list) > 1 and rule.get('right') == "same":
                    mess = 'You defined a list of feature values, then said the other child in the rule should be "the same."' \
                           ' If you meant to create a rule using identified values, choose "the same" for each identified value.'
                    vr.err(rule.full_key + "_left", mess)

                # must be at least 2 "the same" for identification to work
                if rule.get('right') == "same" or rule.get('left') == "same" or rule.get('par') == "same":
                    samecount = 0
                    for dir in ['left', 'right', 'par']:
                        if rule.get(dir) == "same":
                            samecount += 1
                    if samecount == 1:
                        mess = "You defined only one value as 'the same', but there must be at least two for identification to work properly."
                        vr.err(rule.full_key + "_left", mess)

                # don't allow values that don't exist (handle the list separately)
                if not (rule.get('right') and rule.get('par')) in values:
                    mess = 'This rule contains an invalid feature value.'
                    vr.err(rule.full_key + "_left", mess)

                for left in left_rule_list:
                    # don't allow values that don't exist
                    if not left in values:
                        mess = 'This rule contains an invalid feature value. (But if you open the list, then click save & stay, ' \
                               'it will disappear automatically.)'
                        vr.err(rule.full_key + "_left", mess)

                    # no any + any = any rules
                    if (left == 'any' and rule.get('right') == 'any' and rule.get('par') == 'any'):
                        mess = 'An any+any=any rule doesn\'t constrain or add any feature information,' \
                               'and will add to the complexity of your grammar. You should remove it.'
                        vr.warn(rule.full_key + "_left", mess)


                        # TODO make sure they split up PERNUM into person and number

                        # TODO add validation for "nonmatching" but not a list on the left

                        # TODO 'nonmatching' must have a list to the left

                        # TODO if they defined a feature, must also have rules

                        # TODO all non-optional fields should be defined

                        # TODO no conflicting rules? 1 + 2 = 3, 1 + 2 = 2

    for dconj in ch.get('dconj'):
        # warn if a dconj pattern has been defined but not used
        if dconj.full_key not in used_patterns:
            mess = "You defined an agreement pattern but didn't attach it to a coordination strategy. You must attach it to a" \
                   " coordination strategy before the rules will apply to coordinated N/NPs."
            vr.warn(dconj.full_key + "_name", mess)



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
            mess = 'If you chose the question particle strategy ' + \
                   'for yes-no questions, you must specify ' + \
                   'where the question particle appears.'
            vr.err('q-part-order', mess)
        if not qpartorth:
            mess = 'If you chose the question particle strategy ' + \
                   'for yes-no questions, you must specify ' + \
                   'the form of the question particle.'
            vr.err('q-part-orth', mess)

    if ch.get('q-inv'):
        #    if qinvverb != 'aux' and qinvverb != 'main' and qinvverb != 'main-aux':
        #      mess = 'There is something wrong with the verb type (main/aux) for inverted questions.  Please contact developers.'
        #      vr.err('q-inv-verb', mess)
        if not qinvverb:
            mess = 'If you chose subject-verb inversion strategy ' + \
                   'for yes-no questions, you must specify ' + \
                   'which types of verbs invert.'
            vr.err('q-inv-verb', mess)
        if ch.get('word-order') == 'v-final' or \
                        ch.get('word-order') == 'v-initial' or \
                        ch.get('word-order') == 'free':
            mess = 'Subject-verb inversion strategy for yes-no questions ' + \
                   'is not supported for V-final, V-initial, or ' + \
                   'free word order languages.  If you believe you have ' + \
                   'a counterexample to this, please contact us.'
            vr.err('q-inv', mess)
        if ((qinvverb == 'aux' or qinvverb == 'aux-main') and
                    ch.get('has-aux') != 'yes'):
            mess = 'You have not indicated on the word order page ' + \
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
            mess = 'If matrix yes-no questions are expressed through affixation, ' + \
                   'you must specify a lexical rule with the "question" feature ' + \
                   'in the morphology page.'
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
        mess = 'You have chosen to select among hierarchy elements. ' + \
               'You need to select at least one tense element.'
        for t in ten:
            vr.err(t, mess)

    if ch.get('tense-definition') == 'build':
        if 'tense' not in ch:
            mess = 'You have chosen to build your own tense hierarchy ' + \
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
    if ch.get('has-aux') == 'yes' and not ch.get('form-fin-nf') == 'on':
        mess = 'You have indicated on the word order page that ' + \
               'your language has auxiliaries or picked a sentential negation strategy' \
               'that assumes auxiliaries, but have not initialized a FORM hierarchy.'
        vr.err('form-fin-nf', mess)

    # if ch.get('has-aux') == 'no' and not (ch.get('noaux-fin-nf') == 'on'):
    #     if 'nf-subform' in ch:
    #         mess = 'You have indicated that your language has no auxiliaries ' + \
    #                'but you have entered subforms of finite or non-finite.'
    #         vr.err('noaux-fin-nf', mess)

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

    for lexprefix in ('noun', 'verb', 'det', 'aux', 'adj'):
        for lex in ch.get(lexprefix):
            for feat in lex.get('feat', []):
                name_list += \
                    [[ feat.full_key + '_name', feat.get('name') ]]
                value_list += \
                    [[ feat.full_key + '_value', feat.get('name'), feat.get('value') ]]

    for pcprefix in ('noun', 'verb', 'det', 'aux', 'adj'):
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

    ## LLD 12-29-2015 Check that argument structure choices are currently defined
    for lex in ch.get('verb'):
        if lex.get('valence', []):
            value_list += \
                [[ lex.full_key + '_valence', 'argument structure', lex.get('valence') ]]

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


def validate_hierarchy(ch, vr):
    """
    Check that hierarchies are well-formed. Check for vacuous inheritance, cycles,
    and supertypes that are not defined in the current choices.
    """

    # LLD 1-3-2016
    # Check that supertypes have been defined. This needs to be handled slightly differently
    # across categories.
    for section in ['number', 'gender']:
        valid_types = [section]
        for feat in ch.get(section, []):
            valid_types += [feat.get('name')]
        for feat in ch.get(section, []):
            for st in feat.get('supertype', []):
                if st.get('name') not in valid_types:
                    vr.err(st.full_key + '_name', 'You have specified an invalid supertype.')

    for section in ['feature']:
        valid_types = []
        for feat in ch.get(section, []):
            valid_types += [feat.get('name')]
            for value in feat.get('value'):
                valid_types += [value.get('name')]
        for feat in ch.get(section, []):
            for value in feat.get('value', []):
                for st in value.get('supertype', []):
                    if st.get('name') not in valid_types:
                        vr.err(st.full_key + '_name', 'You have specified an invalid supertype.')

    # Adjectives have a similar check already in place.
    for lexprefix in ['verb', 'noun']:
        valid_types = ['']
        for lex in ch.get(lexprefix, []):
            valid_types += [lex.full_key]
        for lex in ch.get(lexprefix, []):
            for st in lex.get('supertypes', '').split(", "):
                if st not in valid_types:
                    vr.err(lex.full_key + '_supertypes', 'You have specified an invalid supertype.')


    # LLD 1-3-2016 Check for cycles and vacuous inheritance. The lexicon section has its own
    # version of this code, so I left those alone for now.

    # xsts is a dictionary that contains some item x's supertypes.
    xsts = {}
    for type in ['number', 'gender']:
        xsts[type] = [] # 'number' and 'gender' can be supertypes, so they need a dict entry.
        for x in ch.get(type, []):
            sts = []
            for st in x.get('supertype',''):
                sts.append(st.get('name'))

            xsts[x.get('name')] = sts

        for x in ch.get(type, []):
            st_anc = [] #used to check for vacuous inheritance
            seen = []
            paths = []
            xkey = x.get('name','')

            for st in xsts[xkey]:
                paths.append([xkey, st])
            parents = xsts[xkey]
            while (True):
                next_parents = []
                for p in parents:
                    if p:
                        # add sts to the next generation
                        to_be_seen = []
                        for r in paths:
                            if r[-1] == p: #this is the path to extend
                                paths.remove(r)
                                if p in xsts:
                                    for q in xsts[p]:
                                        if q not in st_anc:
                                            st_anc.append(q)
                                        if q in r:
                                            vr.err(x.full_key + '_name', "This hierarchy "+
                                                   "contains a cycle. The type "+q+" was found "+
                                                   "at multiple points in the inheritance path: "+
                                                   str(r+[q]), concat=False)
                                        else:
                                            new_path = r + [q]
                                            paths.append(new_path)
                                        if (q != ''):
                                            if not (q in seen):
                                                next_parents.append(q)
                                                to_be_seen.append(q)
                        seen = seen + to_be_seen

                if len(next_parents) == 0:
                    break

                parents = next_parents

            # Check for vacuous inheritance by finding the intersection of supertypes
            # and the supertypes's ancestors
            for t in xsts[xkey]:
                if t in st_anc:
                    vr.err(x.full_key + '_name', "This hierarchy contains a "+
                           "redundant link that will result in an LKB error. The type '"+t+
                           "' is an immediate supertype of '"+x.get('name','')+"' and also "+
                           "an ancestor of another supertype.")

def validate_arg_opt(ch, vr):
    """Check to see if the user completed the necessary portions of the arg
     opt page and see that the OPT feature is used correctly elsewhere"""

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

    verbslist =  ch.get('verb')
    for v in verbslist:
        for feat in v['feat']:
            if feat.get('name') == 'OPT' and not feat.get('head') in ['subj','obj']:
                mess = "The OPT feature on verbs should be specified " + \
                       "on the subject NP or the object NP."
                vr.err(feat.full_key+'_head',mess)

######################################################################
# Validation of clausal modifiers
def validate_clausalmods(ch, vr):
    """Check to see if the user completed the necessary portions of the
       Clausal Modifiers page and check for unsupported combinations of choices"""
    for cms in ch.get('cms'):
        # First check the choices that are required for all clausal mod strategies
        if not cms.get('position'):
            mess = 'You must select a position for the clausal modifier.'
            vr.err(cms.full_key + '_position', mess)
        if not cms.get('modifier-attach'):
            mess = 'You must select VP or S attachment for the clausal modifier.'
            vr.err(cms.full_key + '_modifier-attach', mess)
        if not cms.get('subordinator'):
            mess = 'You must select a subordinator type.'
            vr.err(cms.full_key + '_subordinator', mess)

        # Next check the choices required for free and pair subordinators
        if cms.get('subordinator') == 'free' or cms.get('subordinator') == 'pair':
            if not cms.get('subposition'):
                mess = 'You must select a position for the subordinator.'
                vr.err(cms.full_key + '_subposition', mess)
            if not cms.get('subordinator-type'):
                mess = 'You must makes a selection for whether the subordinator is an adverb or head.'
                vr.err(cms.full_key + '_subordinator-type', mess)
            if cms.get('subordinator-type') == 'adverb':
                if not cms.get('adverb-attach'):
                    mess = 'You must makes a selection for whether the subordinator attaches to a VP or S.'
                    vr.err(cms.full_key + '_adverb-attach', mess)

        # Check the choices required for free subordinators only
        if cms.get('subordinator') == 'free':
            if not cms.get('freemorph'):
                mess = 'You must add at least one free subordinator morpheme.'
                vr.err(cms.full_key + '_freemorph', mess)


        # Check the choices required for pair subordinators only
        if cms.get('subordinator') == 'pair':
            if not cms.get('matrix-subposition'):
                mess = 'You must select a position for the adverb in the matrix clause.'
                vr.err(cms.full_key + '_matrix-subposition', mess)
            if not cms.get('matrix-adverb-attach'):
                mess = 'You must makes a selection for whether the adverb' + \
                       ' in the matrix clause attaches to a VP or S.'
                vr.err(cms.full_key + '_matrix-adverb-attach', mess)
            if not cms.get('morphpair'):
                mess = 'You must add at least one free subordinator morpheme pair.'
                vr.err(cms.full_key + '_morphpair', mess)


        # Warnings for no subordinator morpheme
        if cms.get('subordinator') == 'none':
            if not cms.get('pred'):
                mess = 'If you do not enter a predication for this strategy' + \
                    ' a generic _subord_rel will be added.'
                vr.warn(cms.full_key + '_pred', mess)
            if not cms.get('feat'):
                mess = 'You have not added any subordinator (free or bound) to this strategy.'
                vr.warn(cms.full_key + '_subordinator', mess)

        # Check for unsupoorted combinations with nominalization
        nominalized = False
        for feat in cms.get('feat'):
            if feat.get('name') == 'nominalization':
                nominalized = True
        if nominalized == True:
            mess = 'If multiple nominalization strategies are allowed in the grammar,' +\
                   ' and clausal modifiers require nominalization, the produced grammar' +\
                   ' will allow any nominalinalization strategy for the clausal modifier strategy.'
            vr.warn(feat.full_key + '_name', mess)
            if cms.get('subordinaotor-type') == 'adverb':
                mess = 'Nominalization is not supported of the adverb analysis.'
                vr.err(cms.full_key + '_subordinator-type', mess)
            for feat in cms.get('feat'):
                if feat.get('name') == 'mood' or feat.get('name') == 'aspect':
                    mess = 'Aspect and mood are not present on nominal projections,' +\
                           ' and therefore are not supported in connection with nominalization.'
                    vr.err(feat.full_key + '_name', mess)

        # Check for unsupported features
        for feat in cms.get('feat'):
            if feat.get('name') == 'index':
                mess = 'Index besides aspect and mood are not supported at this time.'
                vr.err(feat.full_key + '_name', mess)
            if feat.get('name') == 'situation':
                mess = 'Situation aspect is not supported at this time.'
                vr.err(feat.full_key + '_name', mess)
            if feat.get('name') == 'evidential':
                mess = 'Evidential is not a supported feature at this time.'
                vr.err(feat.full_key + '_name', mess)


######################################################################
# Validation nominalized clauses
def validate_nominalized_clauses(ch, vr):
    """Check to see if the user completed the necessary portions of the
       Nominalized Clauses page and check for conflicts with word order"""
    for ns in ch.get('ns'):
        if not ns.get('name'):
            mess = 'You must enter a name for the nominalization strategy.'
            vr.err(ns.full_key + '_name', mess)
        level = ns.get('level')
        if level in ['mid','low'] and not ns['nmzRel'] == 'yes':
            vr.err(ns.full_key + '_level','Mid and low nominalization must be specified as having semantics.')
        if not ns['nmzRel'] == 'yes' and not ns['nmzRel'] == 'no':
            vr.err(ns.full_key + '_nmzRel','Please choose whether nominalization contributes to the semantics.')
        if ns.get('level') == 'mid':
            if ch.get('word-order') == 'vso' or ch.get('word-order') == 'osv':
                mess = 'The analysis for your word order does not include a' + \
                       ' VP constituent. You must select V or S nominalization.'
                vr.err(ns.full_key + '_level', mess)



######################################################################
# validate_adnominal_possession(ch, vr)
#   Validate the user's choices about adnominal possession
def validate_adnominal_possession(ch, vr):
    png_feats=set(['person','number','gender'])
    for strat in ch.get('poss-strat'):
        # CHECK THAT ALL THE CHOICES ARE CHOSEN
        # Require basic input for all possessive strategies:
        if not strat.get('order'):
            mess='You must choose a possessive phrase order.'
            vr.err(strat.full_key+'_order',mess)
        if not strat.get('mod-spec'):
            mess='You must choose either modifier-like or specifier-like.'
            vr.err(strat.full_key+'_mod-spec',mess)
        if not strat.get('mark-loc'):
            mess='You must indicate where possessive markings appear.'
            vr.err(strat.full_key+'_mark-loc',mess)
        # Require input for possessor-marking 
        if strat.get('mark-loc')=='possessor' or strat.get('mark-loc')=='both':
            if not strat.get('possessor-type'):
                mess='You must indicate what form the possessor marking takes.'
                vr.err(strat.full_key+'_possessor-type',mess)
            # Require input for affix possessor-marking
            elif strat.get('possessor-type')=='affix':
                if not strat.get('possessor-affix-agr'):
                    mess='You must indicate whether the possessor affix agrees with the possessum.'
                    vr.err(strat.full_key+'_possessor-agr',mess)
            # Require input for non-affix possessor-marking
            elif strat.get('possessor-type')=='non-affix':
#                if not strat.get('possessor-marker-order'):
#                    mess='You must indicate the word order of the possessor marking word.'
#                    vr.err(strat.full_key+'_possessor-marker-order',mess)
                if not strat.get('possessor-agr'):
                    mess='You must indicate whether the possessor affix agrees with the possessum.'
                    vr.err(strat.full_key+'_possessor-agr',mess)
                # Require input for the case when the possessor-marking word doesn't do agreement
                elif strat.get('possessor-agr')=='non-agree':
                    if not strat.get('possessor-orth'):
                        mess='You must give the possessor marker\'s orthographic form.'
                        vr.err(strat.full_key+'_possessor-orth',mess)
                # Require input for the case when the possessor-marking word does do agreement
                elif strat.get('possessor-agr')=='agree':
                    for form in strat.get('possessor-form'):
                        if not form.get('name'):
                           mess='You must give a name for this form.'
                           vr.err(form.full_key+'_name',mess)
                        if not form.get('agr-orth'):
                           mess='You must give the spelling for this form.'
                           vr.err(form.full_key+'_agr-orth',mess)
                        for feat in form.get('feat'):
                           if not feat.get('name'):
                              mess='You must give the name of this feature.'   
                              vr.err(feat.full_key+'_name',mess)
                           # Limit agr features to PNG
                           elif feat.get('name') not in png_feats:
                               mess='Agreement between elements of the possessive phrase ' +\
                                   'is only supported for person, number, and gender.'
                               vr.err(feat.full_key+'_name',mess)
                           if not feat.get('value'):
                              mess='You must give the value of this feature.'   
                              vr.err(feat.full_key+'_value',mess)

        # Require input for possessum-marking 
        if strat.get('mark-loc')=='possessum' or strat.get('mark-loc')=='both':
            if not strat.get('possessum-type'):
                mess='You must indicate what form the possessum marking takes.'
                vr.err(strat.full_key+'_possessum-type',mess)
            # Require input for affix possessum-marking
            elif strat.get('possessum-type')=='affix':
                if not strat.get('possessum-affix-agr'):
                    mess='You must indicate whether the possessum affix agrees with the possessor.'
                    vr.err(strat.full_key+'_possessum-agr',mess)
            # Require input for non-affix possessum-marking
            elif strat.get('possessum-type')=='non-affix':
                # Rule out the scenario: mod-like attachment + non-affixal possessum mark
                if strat.get('mod-spec')=='mod':
                    mess='A modifier-like analysis is not supported in the case where '+ \
                         'the possessum is marked by a separate word or clitic. ' + \
                         'Please select a modifier-like analysis.'
                    vr.err(strat.full_key+'_mod-spec',mess)
#                if not strat.get('possessum-marker-order'):
#                    mess='You must indicate the word order of the possessum-marking word.'
#                    vr.err(strat.full_key+'_possessum-marker-order',mess)
                if not strat.get('possessum-agr'):
                    mess='You must indicate whether the possessum affix agrees with the possessor.'
                    vr.err(strat.full_key+'_possessum-agr',mess)
                # Require input for the case when the possessum-marking word doesn't do agreement
                elif strat.get('possessum-agr')=='non-agree':
                    if not strat.get('possessum-orth'):
                        mess='You must give the possessum marker\'s orthographic form.'
                        vr.err(strat.full_key+'_possessum-orth',mess)
                # Require input for the case when the possessum-marking word does do agreement
                elif strat.get('possessum-agr')=='agree':
                    for form in strat.get('possessum-form'):
                        if not form.get('name'):
                           mess='You must give a name for this form.'
                           vr.err(form.full_key+'_name',mess)
                        if not form.get('agr-orth'):
                           mess='You must give the spelling for this form.'
                           vr.err(form.full_key+'_agr-orth',mess)
                        for feat in form.get('feat'):
                           if not feat.get('name'):
                              mess='You must give the name of this feature.'   
                              vr.err(feat.full_key+'_name',mess)
                           # Limit agr features to PNG
                           elif feat.get('name') not in png_feats:
                               mess='Agreement between elements of the possessive phrase ' +\
                                   'is only supported for person, number, and gender.'
                               vr.err(feat.full_key+'_name',mess)
                           if not feat.get('value'):
                              mess='You must give the value of this feature.'   
                              vr.err(feat.full_key+'_value',mess)
    for pron in ch.get('poss-pron'):
        # Require basic input for all possessive pronouns
        if not pron.get('type'):
            mess='You must specify the type of this possessive pronoun.'
            vr.err(pron.full_key+'_type',mess)
        elif pron.get('type')=='affix':
            if not pron.get('agr'):
                mess='You must specify the whether this pronoun affix agrees with the possessum.'
                vr.err(pron.full_key+'_agr',mess)
            if not pron.get('mod-spec'):
                mess='You must specify the whether this pronoun affix appears with determiners or not.'
                vr.err(pron.full_key+'_mod-spec',mess)
        elif pron.get('type')=='non-affix':
            if not pron.get('order'):
                mess='You must specify the order this pronoun appears in.'
                vr.err(pron.full_key+'_order',mess)
            if not pron.get('mod-spec'):
                mess='You must specify the whether this pronoun affix appears with determiners or not.'
                vr.err(pron.full_key+'_mod-spec',mess)
            if not pron.get('agr'):
                mess='You must specify the whether this pronoun affix agrees with the possessum.'
                vr.err(pron.full_key+'_agr',mess)
            for inst in pron.get('instance'):
                if not inst.get('name'):
                    mess='You must give a name for this pronoun.'
                    vr.err(inst.full_key+'_name',mess)
                if not inst.get('orth'):
                    mess='You must give the spelling for this pronoun.'
                    vr.err(inst.full_key+'_orth',mess)
                for feat in inst.get('feat'):
                    if not feat.get('name'):
                        mess='You must give a name for this feature.'
                        vr.err(feat.full_key+'_name',mess)
                    if not feat.get('value'):
                        mess='You must give a value for this feature.'
                        vr.err(feat.full_key+'_value',mess)
                for feat in inst.get('agr-feat'):
                    if not feat.get('name'):
                        mess='You must give a name for this feature.'
                        vr.err(feat.full_key+'_name',mess)
                    if not feat.get('value'):
                        mess='You must give a value for this feature.'
                        vr.err(feat.full_key+'_value',mess)
        
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
    validate_information_structure(ch, vr)
    validate_tanda(ch, vr)
    gmcs.linglib.negation.validate(ch, vr)
    validate_coordination(ch, vr)
    validate_yesno_questions(ch, vr)
    gmcs.linglib.lexicon.validate_lexicon(ch, vr)
    gmcs.linglib.morphotactics.validate(ch, vr)
    validate_test_sentences(ch, vr)
    validate_adnominal_possession(ch, vr)
    gmcs.linglib.clausalcomps.validate(ch, vr)
    validate_clausalmods(ch, vr)
    validate_nominalized_clauses(ch, vr)
    validate_types(ch, vr)
    validate_features(ch, vr)
    validate_hierarchy(ch, vr)
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


