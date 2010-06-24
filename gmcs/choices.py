### $Id: choices.py,v 1.24 2008-09-30 23:50:02 lpoulson Exp $

######################################################################
# imports

import re
import sys
from util.misc import safe_int

######################################################################
# globals

######################################################################
# Errors

class ChoicesFileParseError(Exception):
  def __init__(self, msg=''):
    self.msg = msg
  def __str__(self):
    return repr(self.msg)

######################################################################
# ChoiceCategory is a parent-class for ChoiceDict and ChoiceList.
# Any meta-information about choices should be encoded in
# ChoiceCategory, and ChoiceDict and ChoiceList should most likely
# just be empty classes inheriting from ChoiceCategory and their
# namesake datatype.

class ChoiceCategory:
  def __init__(self, full_key=None):
    self.full_key = full_key

class ChoiceDict(ChoiceCategory, dict):
  def __getitem__(self, key):
    try:
      return dict.__getitem__(self, key)
    except KeyError:
      return ''

  def iter_num(self):
    if self.full_key is not None:
        result = re.search('[0-9]+$', self.full_key)
        if result is not None:
            return result.group(0)
    return None

class ChoiceList(ChoiceCategory, list):
  def __getitem__(self, key):
    try:
      return list.__getitem__(self, key)
    except IndexError:
      return []

  # custom iterator ignores empty items (e.g. when a
  # user deletes an item in the middle of a list)
  def __iter__(self):
    for item in list.__iter__(self):
      if len(item) > 0:
        yield item

######################################################################
# ChoicesFile is a class that wraps the choices file, a list of
# variables and values, and provides methods for loading, accessing,
# and saving them.

class ChoicesFile:

  # initialize by passing either a file name or ???file handle
  def __init__(self, choices_file=None):

    self.iter_stack = []
    self.cached_values = {}
    self.cached_iter_values = None
    self.choices = ChoiceDict()

    if choices_file is not None:
      try:
        f = choices_file
        if type(choices_file) == str:
          f = open(choices_file, 'r')
        f.seek(0)
        lines = f.readlines()
        self.load_choices([l.strip() for l in lines if l.strip() != ''])
        if type(choices_file) == str:
          f.close()
      except IOError:
        pass # TODO: we should really be logging these

  ############################################################################
  ### Choices file parsing functions

  def load_choices(self, choice_lines):
    # attempt to get version first, since preparse_uprev() needs it
    self.version = self.get_version(choice_lines)
    # some key-values cannot be parsed by the current system, so
    # we need to handle these first
    choice_lines = self.preparse_uprev(choice_lines)
    self.choices = self.parse_choices(choice_lines)
    self.postparse_uprev()

  def get_version(self, choice_lines):
    """
    Return the version number from the choices file, or 0 if there was none.
    """
    version = 0
    for line in [l.strip() for l in choice_lines if l.strip() != '']:
      (key, value) = line.split('=',1)
      if key == 'version':
         version = int(value)
    return version

  def parse_choices(self, choice_lines):
    """
    Get the data structure for each choice in the choices file, then
    merge them all together into one data structure.
    """
    choices = ChoiceDict()
    for line in [l.strip() for l in choice_lines if l.strip() != '']:
      try:
        (key, value) = line.split('=',1)
        if key in ('section', 'version'):
            continue
        choices = self.__set_variable(choices,
                                      self.split_variable_key(key),
                                      value,
                                      allow_overwrite=False)
      except ValueError:
        pass # TODO: log this!
      except ChoicesFileParseError:
        raise ChoicesFileParseError('Variable is multiply defined: %s' % key)

    return choices

  # use the following re if keys like abc_def should be split:
  #var_delim_re = re.compile(r'(\d+)?(?:_|$)')
  var_delim_re = re.compile(r'(\d+)(?:_|$)')
  def split_variable_key(self, key):
    """
    Split a compound variable key into a list of its component parts.
    """
    if key == '': return []
    return [k for k in self.var_delim_re.split(key) if k]

  ############################################################################
  ### Choices access functions

  def get(self, key, default=None):
    # integers have an offset of -1 for list indices
    keys = [safe_int(k, offset=-1) for k in self.split_variable_key(key)]
    d = self.choices
    try:
      for k in keys:
        d = d[k]
    except (KeyError, IndexError):
      return default or []
    return d

  # A __getitem__ method so that ChoicesFile can be used with brackets,
  # e.g., ch['language'].
  def __getitem__(self, key):
    return self.get(key)

  def __set_variable(self, choices, keys, value,
                     allow_overwrite=True, key_prefix=None):
    """
    Set the value parameter in the dict/list data structure choices,
    with the location defined by keys.
    """
    # if there are no more keys, we need to set the value by returning it
    if len(keys) == 0:
      if choices and not allow_overwrite:
        raise ChoicesFileParseError(
                'Variable is multiply defined.')
      return value
    # Now we should be dealing with either a list or dict
    var = keys.pop(0)
    try:
      var = int(var)
      # If no error was thrown, we're dealing with a list index.
      # Create the list if it doesn't already exist
      if not choices:
        choices = ChoiceList(full_key=key_prefix)
      count = len(choices)
      if count < var:
        choices += [ChoiceDict(full_key=key_prefix + str(count+i+1))
                    for i in range(var - count)]
      choices[var - 1] = self.__set_variable(choices[var - 1],
                                           keys,
                                             value,
                                             allow_overwrite,
                                             key_prefix + str(var))
    except ValueError:
      new_key_prefix = '_'.join([k for k in [key_prefix, var] if k])
      choices[var] = self.__set_variable(choices.get(var, None),
                                         keys,
                                         value,
                                         allow_overwrite,
                                         new_key_prefix)
    return choices

  def __setitem__(self, key, value):
    self.__set_variable(self.choices, self.split_variable_key(key), value)

  def __delete(self, choices, keys, prune):
    """
    Delete a choice from the data structure. If prune is True, remove
    empty dictionaries and list items (changing list size).
    """
    if len(keys) == 0:
      return
    elif len(keys) == 1:
      if not prune and type(keys[0]) == int:
        # if not pruning, replace list items to maintain list size
        choices[keys[0]] = ChoiceDict()
      else:
        del choices[keys[0]]
    else:
      # recursively delete the next subitem...
      self.__delete(choices[keys[0]], keys[1:], prune)
      # ... and prune if the resulting branch is empty and we're pruning
      if prune and len(choices[keys[0]]) == 0:
        del choices[keys[0]]

  def delete(self, key, prune=False):
    if key not in self:
        return
    # integers have an offset of -1 for list indices
    keys = [safe_int(k, offset=-1) for k in self.split_variable_key(key)]
    self.__delete(self.choices, keys, prune)
    # full_key values will be corrupted if we pruned, so re-evaluate
    if prune:
      [self.__reset_full_keys(k) for k in self]

  def __delitem__(self, key):
    self.delete(key, prune=False)

  def __contains__(self, key):
    if self.get(key):
      return True
    return False

  def __iter__(self):
    return self.choices.__iter__()

  def __len__(self):
    return len(self.choices)

  def __reset_full_keys(self, key):
    """
    Starting at the given key, reset the full_key values of all
    choices contained by that key.
    """
    # make sure the current key exists (e.g. was not pruned)
    if key not in self:
      return
    for i, c in enumerate(self[key]):
      c_type = type(c)
      if c_type is ChoiceDict:
        c.full_key = key + str(i + 1)
      elif c_type is ChoiceList:
        c.full_key = key + str(c)
      else:
        continue
      self.__reset_full_keys(c.full_key)

  ############################################################################
  ### Up-revisioning handler

  def preparse_uprev(self, choice_lines):
    """
    Convert choices file lines before they are parsed.
    """
    new_lines = []
    for line in choice_lines:
      try:
        (key, value) = line.split('=',1)
        if key in ('section', 'version'):
            continue
        if self.version < 4:
          (key, value) = self.preparse_convert_3_to_4(key, value)
        if self.version < 19:
          (key, value) = self.preparse_convert_18_to_19(key, value)
        # If future versions require a choices file line to be converted
        # before it is parsed, but the appropriate method here:
        # if self.version < N
        #   self.preparse_convert_N-1_to_N(key, value)
        new_lines += ['='.join([key, value])]
      except ValueError:
        pass # TODO: log this!
      except ChoicesFileParseError:
        raise ChoicesFileParseError('Variable is multiply defined: %s' % key)

    return new_lines

  def postparse_uprev(self):
    if self.version < 1:
      self.convert_0_to_1()
    if self.version < 2:
      self.convert_1_to_2()
    if self.version < 3:
      self.convert_2_to_3()
    if self.version < 4:
      self.convert_3_to_4()
    if self.version < 5:
      self.convert_4_to_5()
    if self.version < 6:
      self.convert_5_to_6()
    if self.version < 7:
      self.convert_6_to_7()
    if self.version < 8:
      self.convert_7_to_8()
    if self.version < 9:
      self.convert_8_to_9()
    if self.version < 10:
      self.convert_9_to_10()
    if self.version < 11:
      self.convert_10_to_11()
    if self.version < 12:
      self.convert_11_to_12()
    if self.version < 13:
      self.convert_12_to_13()
    if self.version < 14:
      self.convert_13_to_14()
    if self.version < 15:
      self.convert_14_to_15()
    if self.version < 16:
      self.convert_15_to_16()
    if self.version < 17:
      self.convert_16_to_17()
    if self.version < 18:
      self.convert_17_to_18()
    if self.version < 19:
      self.convert_18_to_19()
    if self.version < 20:
      self.convert_19_to_20()
    # As we get more versions, add more version-conversion methods, and:
    # if self.version < N:
    #   self.convert_N-1_to_N

    # reset the full_keys to be safe
    #[self.__reset_full_keys(key) for key in self]

  # Return the keys for the choices dict
  def keys(self):
    return self.choices.keys()


  def clear_cached_values(self):
    self.cached_values = {}
    self.cached_iter_values = None

  ######################################################################
  # Methods for accessing "derived" values -- that is, groups of values
  # that are implied by the list of choices, but not directly stored
  # in it.  For example, it is convenient to be able to get a list of
  # all features defined in the languages, even though they're not
  # all stored in a single place.

  def has_case(self, feat, case):
    """
    Return true if the feature has matching case or if case is empty.
    """
    return feat['name'] == 'case' and (feat['value'] == case or case == '')

  def has_noun_case(self, case = ''):
    """
    Returns True iff the target language has either morphologically or
    lexically marked case (restricting the calculation to the
    passed-in case if it's non-empty).
    """

    k = 'has_noun_case(' + case + ')'
    if self.cached_values.has_key(k):
      return self.cached_values[k]

    result = False

    # check lexical types
    for noun in self.get('noun'):
      for feat in noun.get('feat',[]):
        result = result or self.has_case(feat, case)

    # check morphemes
    for slotprefix in ('noun', 'verb', 'det'):
      for slot in self.get(slotprefix + '-slot'):
        for morph in slot.get('morph',[]):
          for feat in morph.get('feat',[]):
            result = result or self.has_case(feat, case)

    self.cached_values[k] = result

    return result


  def has_adp_case(self, case = '', check_opt = False):
    """
    Returns True iff the target language has case-marking adpositions
    (restricting the calculation to the passed-in case if it's
    non-empty).  If the check_opt argument is True, only return True
    if the adposition is optional.
    """

    result = False

    for adp in self.get('adp'):
      opt = adp.get('opt')
      for feat in adp.get('feat', []):
        result = result or (self.has_case(feat, case) and \
                            (opt or not check_opt))

    return result


  def has_optadp_case(self, case = ''):
    """
    Returns True iff the target language has optional case-marking
    adpositions (restricting the calculation to the passed-in case if
    it's non-empty).
    """

    return self.has_adp_case(case, True)


  def has_mixed_case(self, case = ''):
    """
    Returns True iff the target language has both case-marking
    adpositions and case on nouns (restricting the calculation to the
    passed-in case if it's non-empty).
    """

    has_noun = self.has_noun_case(case)
    has_adp = self.has_adp_case(case)

    return has_noun and has_adp


  # case_head()
  def case_head(self, case = ''):
    """
    Returns the appropriate head type for case-marked arguments in the
    target language (restricting the calculation to the passed-in case
    if it's non-empty).
    """

    has_noun = self.has_noun_case(case)
    has_adp = self.has_adp_case(case)
    has_optadp = self.has_optadp_case(case)

    if (has_noun and has_adp) or has_optadp:
      return '+np'
    elif has_adp:
      return 'adp'
    else:
      return 'noun'


  def has_dirinv(self):
    """
    Returns True iff the target language has a direct-inverse scale.
    """
    return 'scale' in self.choices


  def has_SCARGS(self):
    """
    Returns True iff the target language requires the SC-ARGS feature,
    which contains the arguments in the order they are ranked by the
    direct-inverse hierarchy.
    """
    result = False

    for verb in self.get('verb'):
      for feat in verb.get('feat', []):
        result = result or feat['head'] in ('higher', 'lower')

    for verb_slot in self.get('verb-slot'):
      for morph in verb_slot.get('morph',[]):
        for feat in morph.get('feat',[]):
          result = result or feat['head'] in ('higher', 'lower')

    return result


  # cases()
  #   Create and return a list containing information about the cases
  #   in the language described by the current choices.  This list consists
  #   of tuples with three values:
  #     [canonical name, friendly name, abbreviation]
  def cases(self):
    # first, make two lists: the canonical and user-provided case names
    cm = self.get('case-marking')
    canon = []
    user = []
    if cm == 'nom-acc':
      canon.append('nom')
      user.append(self.choices[cm + '-nom-case-name'])
      canon.append('acc')
      user.append(self.choices[cm + '-acc-case-name'])
    elif cm == 'erg-abs':
      canon.append('erg')
      user.append(self.choices[cm + '-erg-case-name'])
      canon.append('abs')
      user.append(self.choices[cm + '-abs-case-name'])
    elif cm == 'tripartite':
      canon.append('s')
      user.append(self.choices[cm + '-s-case-name'])
      canon.append('a')
      user.append(self.choices[cm + '-a-case-name'])
      canon.append('o')
      user.append(self.choices[cm + '-o-case-name'])
    elif cm in ['split-s']:
      canon.append('a')
      user.append(self.choices[cm + '-a-case-name'])
      canon.append('o')
      user.append(self.choices[cm + '-o-case-name'])
    elif cm in ['fluid-s']:
      a_name = self.choices[cm + '-a-case-name']
      o_name = self.choices[cm + '-o-case-name']
      canon.append('a+o')
      user.append('fluid')
      canon.append('a')
      user.append(a_name)
      canon.append('o')
      user.append(o_name)
    elif cm in ['split-n', 'split-v']:
      canon.append('nom')
      user.append(self.choices[cm + '-nom-case-name'])
      canon.append('acc')
      user.append(self.choices[cm + '-acc-case-name'])
      canon.append('erg')
      user.append(self.choices[cm + '-erg-case-name'])
      canon.append('abs')
      user.append(self.choices[cm + '-abs-case-name'])
    elif cm in ['focus']:
      canon.append('focus')
      user.append(self.choices[cm + '-focus-case-name'])
      canon.append('a')
      user.append(self.choices[cm + '-a-case-name'])
      canon.append('o')
      user.append(self.choices[cm + '-o-case-name'])

    # fill in any additional cases the user has specified
    for case in self.get('case'):
      canon.append(case['name'])
      user.append(case['name'])

    # if possible without causing collisions, shorten the case names to
    # three-letter abbreviations; otherwise, just use the names as the
    # abbreviations
    abbrev = [ l[0:3] for l in user ]
    if len(set(abbrev)) != len(abbrev):
      abbrev = user

    cases = []
    for i in range(0, len(canon)):
      cases.append([canon[i], user[i], abbrev[i]])

    return cases


  # patterns()
  #   Create and return a list containing information about the
  #   case-marking patterns implied by the current case choices.
  #   This list consists of tuples:
  #       [ canonical pattern name,
  #         friendly pattern name,
  #         rule?,
  #         direct-inverse? ]
  #   A pattern name is:
  #       (in)?transitive \(subject case-object case)
  #   In a canonical name (which is used in the choices file), the
  #   case names are the same as those used in the choices variable
  #   names.  The friendly name uses the names supplied by the
  #   user.  The third element is either True if the case pattern
  #   is one that should be used in lexical rules or False if it
  #   should be used on lexical types (subtypes of verb-lex).  The
  #   fourth argument is true if the verb follows a direct-inverse
  #   marking pattern.
  def patterns(self):
    cm = self.get('case-marking')
    cases = self.cases()

    patterns = []

    # Fill in the canonical names based on the case-marking.
    if cm == 'nom-acc':
      patterns += [ ['nom', '', False] ]
      patterns += [ ['nom-acc', '', False] ]
    elif cm == 'erg-abs':
      patterns += [ ['abs', '', False] ]
      patterns += [ ['erg-abs', '', False] ]
    elif cm == 'tripartite':
      patterns += [ ['s', '', False] ]
      patterns += [ ['a-o', '', False] ]
    elif cm == 'split-s':
      patterns += [ ['a', '', False] ]
      patterns += [ ['o', '', False] ]
      patterns += [ ['a-o', '', False] ]
    elif cm == 'fluid-s':
      patterns += [ ['a', '', False] ]
      patterns += [ ['o', '', False] ]
      patterns += [ ['a+o', '', False] ]
      patterns += [ ['a-o', '', False] ]
    elif cm == 'split-n':
      patterns += [ ['s', '', False] ]
      patterns += [ ['a-o', '', False] ]
    elif cm == 'split-v':
      patterns += [ ['nom', '', True] ]
      patterns += [ ['abs', '', True] ]
      patterns += [ ['nom-acc', '', True] ]
      patterns += [ ['erg-abs', '', True] ]
    elif cm == 'focus':
      patterns += [ ['focus', '', True] ]
      patterns += [ ['focus-o', '', True] ]
      patterns += [ ['a-focus', '', True] ]

    # Add intransitive and transitive, which are always available.
    patterns += [ ['intrans', '', False] ]
    patterns += [ ['trans', '', False] ]

    # Fill in the friendly names based on the canonical names
    for i in range(0, len(patterns)):
      if patterns[i][0] in ['trans', 'intrans']:
        patterns[i][1] = patterns[i][0] + 'itive'
        if cm != 'none':
          patterns[i][1] += ' (case unspecified)'
      else:
        w = patterns[i][0].split('-')
        for j in range(0, len(w)):
          for c in cases:
            if w[j] == c[0]:
              w[j] = c[1]
        if len(w) == 1:
          patterns[i][1] = 'intransitive (%s)' % (w[0])
        elif len(w) == 2:
          patterns[i][1] = 'transitive (%s-%s)' % (w[0], w[1])

    # Finally, extend the patterns to include direct-inverse, as needed
    if self.has_dirinv():
      for i in range(0, len(patterns)):
        if patterns[i][0] == 'trans' or patterns[i][0].find('-') != -1:
          patterns += [ [ patterns[i][0] + ',dirinv',
                          patterns[i][1] + ', direct-inverse',
                          patterns[i][2] ] ]

    return patterns


  # numbers()
  #   Create and return a list containing information about the values
  #   of the number feature implied by the current choices.
  #   This list consists of tuples:
  #     [name, supertype;supertype;...]
  def numbers(self):
    numbers = []

    for n in self.get('number'):
      name = n['name']
      stype = ';'.join([s['name'] for s in n.get('supertype',[])]) or 'number'
      numbers += [[name, stype]]

    return numbers


  # persons()
  #   Create and return a list containing information about the values
  #   of the person feature implied by the current choices.
  #   This list consists of tuples:
  #     [name, supertype]
  def persons(self):
    persons = []

    person = self.get('person')
    if person == '1-2-3':
      persons += [['1st', 'person']]
      persons += [['2nd', 'person']]
      persons += [['3rd', 'person']]
    elif person == '1-2-3-4':
      persons += [['1st', 'person']]
      persons += [['2nd', 'person']]
      persons += [['3rd', 'person']]
      persons += [['4th', 'person']]
    elif person == '1-non-1':
      persons += [['1st', 'person']]
      persons += [['non-1st', 'person']]
    elif person == '2-non-2':
      persons += [['2nd', 'person']]
      persons += [['non-2nd', 'person']]
    elif person == '3-non-3':
      persons += [['3rd', 'person']]
      persons += [['non-3rd', 'person']]

    return persons


  # pernums()
  #   Create and return a list containing information about the values
  #   of the pernum feature implied by the current choices.  A pernum
  #   feature is implied when the user has specified that the
  #   first-person plural has sub-types.
  #   This list consists of tuples:
  #     [name, supertype;supertype;...]
  def pernums(self):
    pernums = []

    fp = self.get('first-person')
    if fp and fp != 'none':
      num_leaves = []
      num_supers = []
      for n in self.numbers():
        if not n[0] in num_leaves:
          num_leaves += [n[0]]
        for st in n[1].split(';'):
          if st not in num_supers:
            num_supers += [st]
        st = n[1]
        if st == 'number':
          st = 'pernum'
        pernums += [[n[0], st]]
      for st in num_supers:
        if st in num_leaves:
          num_leaves.remove(st)

      per_leaves = []
      for p in self.persons():
        if p[0] not in per_leaves:
          per_leaves += [p[0]]
        st = p[1]
        if st == 'person':
          st = 'pernum'
        pernums += [[p[0], st]]

      for n in num_leaves:
        for p in per_leaves:
          pn = p[0] + n
          pernums += [[pn, p + ';' + n]]
          if p == '1st':
            if fp == 'incl-excl':
              for num in self.get('incl-excl-number').split(', '):
                if num == n:
                  pernums += [[pn + '_incl', pn]]
                  pernums += [[pn + '_excl', pn]]
            elif fp == 'other':
              for p_st in self.get('person-subtype'):
                name = p_st['name']
                for num in p_st['number'].split(', '):
                  if num == n:
                    pernums += [[pn + '_' + name, pn]]

    return pernums


  # genders()
  #   Create and return a list containing information about the
  #   genders implied by the current choices.
  #   This list consists of tuples:
  #     [name, supertype;supertype;...]
  def genders(self):
    genders = []

    for g in self.get('gender'):
      name = g['name']
      stype = ';'.join([s['name'] for s in g.get('supertype',[])]) or 'gender'
      genders += [[name, stype]]

    return genders

  # forms()
  #   Create and return a list containing the values of the FORM
  #   feature that constrains the form of auxiliary complements as
  #   defined in the current choices.
  #   This list consists of tuples:
  #     [form name]
  def forms(self):
    forms = []

    if self.get('has-aux') == 'yes' or self.get('noaux-fin-nf') == 'on':
      forms += [ ['finite'], ['nonfinite'] ]
      for p in ['nf', 'fin']:
        for p_sf in self.get(p + '-subform'):
          forms += [[p_sf['name']]]

    return forms

  # tenses()
  #   Create and return a list containing information about the values
  #   of the TENSE feature implied by the current choices.
  #   This list consists of tuples:
  #     [tense name]
  def tenses(self):
    tenses = []

    tdefn = self.get('tense-definition')

    if tdefn == 'choose':
      for ten in ('past', 'present', 'future', 'nonpast', 'nonfuture'):
        if ten in self.choices:
          tenses += [[ten]]
          for t_st in self.get(ten + '-subtype'):
            tenses += [ [t_st['name']] ]
    elif tdefn == 'build':
      for ten in self.get('tense'):
        tenses += [ [ten['name']] ]

    return tenses

  # aspects()
  #   Create and return a list containing information about the values
  #   of the viewpoint ASPECT feature implied by the current choices.
  #   This list consists of tuples:
  #     [aspect name]
  def aspects(self):
    return [[aspect['name']] for aspect in self.get('aspect')]

  # situations()
  #   Create and return a list containing information about the values
  #   of the SITUATION aspect feature implied by the current choices.
  #   This list consists of tuples:
  #     [situation name]
  def situations(self):
    return [[situation['name']] for situation in self.get('situation')]

  def types(self):
    """
    Create and return a list containing type names. FIX - these are
    based on the choices file. Need to include required types and
    inferred types as well. This list consists of tuples: [(type, name)]
    """
    return [(self.choices[t]['name'], t)
            for t in ('noun', 'verb', 'aux', 'det')
            if t in self.choices and 'name' in self.choices[t]]

  def __get_features(self, feat_list, i1, i2, label, tdl):
    """
    If there are values available for the given feature, construct a
    list of the feature label, values, and tdl code for that feature.
    """
    values = ';'.join([x[i1] + '|' + x[i2] for x in feat_list])
    if values:
      return [ [label, values, tdl] ]
    return []

  # features()
  #   Create and return a list containing information about the
  #   features in the language described by the current choices.  This
  #   list consists of tuples with three strings:
  #       [feature name, list of values, feature geometry]
  #   Note that the feature geometry is empty if the feature requires
  #   more complex treatment that just FEAT=VAL (e.g. negation).  The
  #   list of values is separated by semicolons, and each item in the
  #   list is a pair of the form 'name|friendly name'.
  def features(self):
    features = []

    # Case
    features += self.__get_features(self.cases(), 0, 1, 'case',
                                    'LOCAL.CAT.HEAD.CASE')
    # Number, Person, and Pernum
    pernums = self.pernums()
    if pernums:
      features += self.__get_features(pernums, 0, 0, 'pernum',
                                      'LOCAL.CONT.HOOK.INDEX.PNG.PERNUM')
    else:
      features += self.__get_features(self.numbers(), 0, 0, 'number',
                                      'LOCAL.CONT.HOOK.INDEX.PNG.NUM')
      features += self.__get_features(self.persons(), 0, 0, 'person',
                                      'LOCAL.CONT.HOOK.INDEX.PNG.PER')

    # Gender
    features += self.__get_features(self.genders(), 0, 0, 'gender',
                                    'LOCAL.CONT.HOOK.INDEX.PNG.GEND')

    # Case patterns
    features += self.__get_features(self.patterns(), 0, 1,
                                    'argument structure', '')

    # Form
    features += self.__get_features(self.forms(), 0, 0, 'form',
                                    'LOCAL.CAT.HEAD.FORM')

    # Tense
    features += self.__get_features(self.tenses(), 0, 0, 'tense',
                                    'LOCAL.CONT.HOOK.INDEX.E.TENSE')

    # Viewpoint Aspect
    features += self.__get_features(self.aspects(), 0, 0, 'aspect',
                                    'LOCAL.CONT.HOOK.INDEX.E.ASPECT')

    #Situation Aspect
    features += self.__get_features(self.situations(), 0, 0, 'situation',
                                    'LOCAL.CONT.HOOK.INDEX.E.SITUATION')

    # Direction
    if self.has_dirinv():
      features += [ ['direction',
                     'dir|direct;inv|inverse',
                     'LOCAL.CAT.HEAD.DIRECTION'] ]

    # Negaton
    if 'infl-neg' in self.choices:
      features += [ ['negation', 'plus|plus', '' ] ]

    # Questions
    if 'q-infl' in self.choices:
      features += [ ['question', 'plus|plus', '' ] ]

    # Argument Optionality
    if 'subj-drop' in self.choices or 'obj-drop' in self.choices:
      features +=[['OPT', 'plus|plus;minus|minus', '']]

    perm_notperm_string = 'permitted|permitted;not-permitted|not-permitted'
    # Overt Argument
    #if self.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt' and \
    #   self.get('obj-mark-drop') == 'obj-mark-drop-req':
     # features += [['overt-arg', perm_notperm_string, '']]
    if self.get('obj-mark-no-drop') == 'obj-mark-no-drop-not' and \
         self.get('obj-mark-drop') == 'obj-mark-drop-req':
      features += [['overt-arg', perm_notperm_string, '']]
    elif self.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and \
         self.get('subj-mark-drop') == 'subj-mark-drop-req':
      features += [['overt-arg', perm_notperm_string, '']]
    elif self.get('obj-mark-no-drop') == 'obj-mark-no-drop-not' and \
         self.get('obj-mark-drop') == 'obj-mark-drop-opt' :
      features += [['overt-arg', perm_notperm_string, '']]
    elif self.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and \
         self.get('subj-mark-drop') == 'subj-mark-drop-opt' :
      features += [['overt-arg', perm_notperm_string, '']]
    elif self.get('subj-mark-no-drop') == 'subj-mark-no-drop-opt' and \
         self.get('subj-mark-drop') == 'subj-mark-drop-req':
      features += [['overt-arg', perm_notperm_string, '']]

    # Dropped Argument
    if self.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt' and \
       self.get('obj-mark-drop') == 'obj-mark-drop-req':
      features += [['dropped-arg', perm_notperm_string, '']]
    elif self.get('subj-mark-no-drop') == 'subj-mark-no-drop-opt' and \
         self.get('subj-mark-drop') == 'subj-mark-drop-req':
      features += [['dropped-arg', perm_notperm_string, '']]
    elif self.get('obj-mark-drop') == 'obj-mark-drop-not' and \
       self.get('obj-mark-no-drop') == 'obj-mark-no-drop-req':
      features += [['dropped-arg', perm_notperm_string,'']]
    elif self.get('obj-mark-drop') == 'obj-mark-drop-not' and \
         self.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt':
      features += [['dropped-arg', perm_notperm_string,'']]
    elif self.get('subj-mark-drop') == 'subj-mark-drop-not' and \
         self.get('subj-mark-no-drop') == 'subj-mark-no-drop-req':
      features += [['dropped-arg', perm_notperm_string,'']]
    elif self.get('subj-mark-drop') == 'subj-mark-drop-not' and \
         self.get('subj-mark-no-drop') == 'subj-mark-no-drop-opt':
      features += [['dropped-arg', perm_notperm_string,'']]

    for feature in self.get('feature'):
      feat_name = feature['name']
      feat_type = feature['type']

      values = ';'.join([val['name'] + '|' + val['name']
                         for val in feature.get('value', [])])

      geom = ''
      if feat_type == 'head':
        geom = 'LOCAL.CAT.HEAD.' + feat_name.upper()
      else:
        geom = 'LOCAL.CONT.HOOK.INDEX.PNG.' + feat_name.upper()

      if values:
        features += [ [feat_name, values, geom] ]

    return features


  ######################################################################
  # Conversion methods: each of these functions assumes the choices
  # file has already been loaded, then converts an older version into
  # a newer one, updating both old key names and old value names.
  # These methods can be called in a chain: to update from version 2
  # to 5, call convert_2_to_3, convert_3_to_4, and convert_4_to_5, in
  # that order.
  #
  # The methods should consist of a sequence of calls to
  # convert_value(), followed by a sequence of calls to convert_key().
  # That way the calls always contain an old name and a new name.
  def current_version(self):
    return 20

  def convert_value(self, key, old, new):
    if key in self and self[key] == old:
      self[key] = new

  def convert_key(self, old, new, key_prefix=''):
    if key_prefix:
      old = '_'.join([key_prefix, old])
      new = '_'.join([key_prefix, new])
    if old in self:
      self[new] = self[old]
      #self.delete(old, prune=True)
      self.delete(old)

  def convert_0_to_1(self):
    self.convert_key('wordorder', 'word-order')

    self.convert_value('hasDets', 't', 'yes')
    self.convert_value('hasDets', 'nil', 'no')
    self.convert_key('hasDets', 'has-dets')

    self.convert_value('NounDetOrder', 'HeadSpec', 'noun-det')
    self.convert_value('NounDetOrder', 'SpecHead', 'det-noun')
    self.convert_key('NounDetOrder', 'noun-det-order')

    self.convert_key('infl_neg', 'infl-neg')

    self.convert_key('neg-aff-form', 'neg-aff-orth')

    self.convert_key('adv_neg', 'adv-neg')

    self.convert_value('negmod', 'S', 's')
    self.convert_value('negmod', 'VP', 'vp')
    self.convert_value('negmod', 'V', 'v')
    self.convert_key('negmod', 'neg-mod')

    self.convert_value('negprepostmod', 'pre', 'before')
    self.convert_value('negprepostmod', 'post', 'after')
    self.convert_key('negprepostmod', 'neg-order')

    self.convert_value('multineg', 'bothopt', 'both-opt')
    self.convert_value('multineg', 'bothobl', 'both-obl')
    self.convert_value('multineg', 'advobl', 'adv-obl')
    self.convert_value('multineg', 'inflobl', 'infl-obl')
    self.convert_key('multineg', 'multi-neg')

    self.convert_key('cs1n', 'cs1_n')

    self.convert_key('cs1np', 'cs1_np')

    self.convert_key('cs1vp', 'cs1_vp')

    self.convert_key('cs1s', 'cs1_s')

    self.convert_key('cs1pat', 'cs1_pat')

    self.convert_key('cs1mark', 'cs1_mark')

    self.convert_key('cs1orth', 'cs1_orth')

    self.convert_key('cs1order', 'cs1_order')

    self.convert_key('cs2n', 'cs2_n')

    self.convert_key('cs2np', 'cs2_np')

    self.convert_key('cs2vp', 'cs2_vp')

    self.convert_key('cs2s', 'cs2_s')

    self.convert_key('cs2pat', 'cs2_pat')

    self.convert_key('cs2mark', 'cs2_mark')

    self.convert_key('cs2orth', 'cs2_orth')

    self.convert_key('cs2order', 'cs2_order')

    self.convert_value('ques', 'qpart', 'q-part')

    self.convert_key('qinvverb', 'q-inv-verb')

    self.convert_value('qpartposthead', '-', 'before')
    self.convert_value('qpartposthead', '+', 'after')
    self.convert_key('qpartposthead', 'q-part-order')

    self.convert_key('qpartform', 'q-part-orth')

    self.convert_key('noun1pred', 'noun1_pred')

    self.convert_value('noun1spr', 'nil', 'imp')
    self.convert_key('noun1spr', 'noun1_det')

    self.convert_key('noun2pred', 'noun2_pred')

    self.convert_value('noun2spr', 'nil', 'imp')
    self.convert_key('noun2spr', 'noun2_det')

    self.convert_key('ivpred', 'iverb-pred')

    self.convert_value('iverbSubj', 'pp', 'adp')
    self.convert_key('iverbSubj', 'iverb-subj')

    self.convert_key('iverb-nonfinite', 'iverb-non-finite')

    self.convert_key('tvpred', 'tverb-pred')

    self.convert_value('tverbSubj', 'pp', 'adp')
    self.convert_key('tverbSubj', 'tverb-subj')

    self.convert_value('tverbObj', 'pp', 'adp')

    self.convert_key('tverbObj', 'tverb-obj')

    self.convert_key('tverb-nonfinite', 'tverb-non-finite')

    self.convert_key('auxverb', 'aux-verb')

    self.convert_key('auxsem', 'aux-sem')

    self.convert_key('auxpred', 'aux-pred')

    self.convert_value('auxcomp', 'S', 's')
    self.convert_value('auxcomp', 'VP', 'vp')
    self.convert_value('auxcomp', 'V', 'v')
    self.convert_key('auxcomp', 'aux-comp')

    self.convert_value('auxorder', 'left', 'before')
    self.convert_value('auxorder', 'right', 'after')
    self.convert_key('auxorder', 'aux-order')

    self.convert_value('auxsubj', 'noun', 'np')
    self.convert_key('auxsubj', 'aux-subj')

    self.convert_key('det1pred', 'det1_pred')

    self.convert_key('det2pred', 'det2_pred')

    self.convert_key('subjAdpForm', 'subj-adp-orth')

    self.convert_value('subjAdp', 'pre', 'before')
    self.convert_value('subjAdp', 'post', 'after')
    self.convert_key('subjAdp', 'subj-adp-order')

    self.convert_key('objAdpForm', 'obj-adp-orth')

    self.convert_value('objAdp', 'pre', 'before')
    self.convert_value('objAdp', 'post', 'after')
    self.convert_key('objAdp', 'obj-adp-order')

    self.convert_key('negadvform', 'neg-adv-orth')


  def convert_1_to_2(self):
    # The old 'ques' radio button has been converted into a series of
    # checkboxes, of which 'inv' has been renamed 'q-inv' and 'int'
    # has been removed.
    if 'ques' in self:
      ques = self.get('ques')
      self.delete('ques')
      if ques == 'inv':
        ques = 'q-inv'
      if ques != 'int':
        self[ques] = 'on'

  def convert_2_to_3(self):
    # Added a fuller implementation of case marking on core arguments,
    # so convert the old case-marking adposition stuff to the new
    # choices
    S = self.get('iverb-subj')
    A = self.get('tverb-subj')
    O = self.get('tverb-obj')

    Sorth = Aorth = Oorth = ''
    Sorder = Aorder = Oorder = ''
    if S == 'adp':
      Sorth = self.get('subj-adp-orth')
      Sorder = self.get('subj-adp-order')
    if A == 'adp':
      Aorth = self.get('subj-adp-orth')
      Aorder = self.get('subj-adp-order')
    if O == 'adp':
      Oorth = self.get('obj-adp-orth')
      Aorder = self.get('obj-adp-order')

    if Sorth == '' and Aorth == '' and Oorth == '':
      if len(self):  # don't add this if the choices file is empty
        self['case-marking'] = 'none'
    elif Sorth == Aorth and Sorth != Oorth:
      self['case-marking'] = 'nom-acc'
      self['nom-case-label'] = 'nominative'
      self['acc-case-label'] = 'accusative'
      if Aorth:
        self['nom-case-pat'] = 'np'
        self['nom-case-order'] = Aorder
      else:
        self['nom-case-pat'] = 'none'
      if Oorth:
        self['acc-case-pat'] = 'np'
        self['acc-case-order'] = Oorder
      else:
        self['acc-case-pat'] = 'none'
    elif Sorth != Aorth and Sorth == Oorth:
      self['case-marking'] = 'erg-asb'
      self['erg-case-label'] = 'ergative'
      self['abs-case-label'] = 'absolutive'
      if Aorth:
        self['erg-case-pat'] = 'np'
        self['erg-case-order'] = Aorder
      else:
        self['erg-case-pat'] = 'none'
      if Oorth:
        self['abs-case-pat'] = 'np'
        self['abs-case-order'] = Oorder
      else:
        self['abs-case-pat'] = 'none'
    else:
      self['case-marking'] = 'tripartite'
      self['s-case-label'] = 'subjective'
      self['a-case-label'] = 'agentive'
      self['o-case-label'] = 'objective'
      if Sorth:
        self['s-case-pat'] = 'np'
        self['s-case-order'] = Sorder
      else:
        self['s-case-pat'] = 'none'
      if Aorth:
        self['a-case-pat'] = 'np'
        self['a-case-order'] = Aorder
      else:
        self['a-case-pat'] = 'none'
      if Oorth:
        self['o-case-pat'] = 'np'
        self['o-case-order'] = Oorder
      else:
        self['o-case-pat'] = 'none'

    self.delete('iverb-subj')
    self.delete('tverb-subj')
    self.delete('tverb-obj')
    self.delete('subj-adp-orth')
    self.delete('subj-adp-order')
    self.delete('obj-adp-orth')
    self.delete('obj-adp-order')

  def preparse_convert_3_to_4(self, key, value):
    if key in ('noun1', 'noun2'):
      key += '_orth'
    elif key.startswith('iverb'):
      # for iverb-pred or iverb-non-finite
      key = key.replace('iverb-','verb1_', 1)
      # this happens only if previous did nothing (i.e. key = "iverb")
      key = key.replace('iverb', 'verb1_orth', 1)
    elif key.startswith('tverb'):
      key = key.replace('tverb-', 'verb2_', 1)
      key = key.replace('tverb', 'verb2_orth', 1)
    elif key == 'det1':
      key = 'det1_orth'
    elif key == 'det1pred':
      key = 'det1_pred'
    elif key == 'det2':
      key = 'det2_orth'
    elif key == 'det2pred':
      key = 'det2_pred'

    return (key, value)

  def convert_3_to_4(self):
    # Added a fuller implementation of case marking on core arguments,
    # so convert the old case-marking adposition stuff to the new
    # choices. Converting keys like noun1=cat is in
    # preparse_convert_3_to_4
    if self.get('verb1_orth'):
      self['verb1_valence'] = 'intrans'

    if self.get('verb2_orth'):
      self['verb2_valence'] = 'trans'

  def convert_4_to_5(self):
    # An even fuller implementation of case marking, with some of the
    # work shifted off on Kelly's morphology code.
    # Get a list of choices-variable prefixes, one for each case
    prefixes = []
    cm = self.get('case-marking')
    if cm == 'nom-acc':
      prefixes.append('nom')
      prefixes.append('acc')
    elif cm == 'erg-abs':
      prefixes.append('erg')
      prefixes.append('abs')
    elif cm == 'tripartite':
      prefixes.append('s')
      prefixes.append('a')
      prefixes.append('o')

    cur_ns = 1       # noun slot
    cur_nm = 1       # noun morph
    cur_ni = 'noun'  # noun input
    last_ns_order = ''

    cur_ds = 1       # det slot
    cur_dm = 1       # det morph
    cur_ni = 'det'   # det input
    last_ds_order = ''

    cur_adp = 1

    for p in prefixes:
      label = self.get(p + '-case-label')
      pat = self.get(p + '-case-pat')
      order = self.get(p + '-case-order')
      orth = self.get(p + '-case-orth')

      # create noun slot and morph
      if pat in ('noun', 'noun-det'):
        if last_ns_order and last_ns_order != order:
          cur_ni = 'noun-slot' + str(cur_ns)
          cur_ns += 1
          cur_nm = 1

        ns_pre = 'noun-slot' + str(cur_ns)
        nm_pre = ns_pre + '_morph' + str(cur_nm)

        self[ns_pre + '_input1_type'] = cur_ni
        self[ns_pre + '_name'] = 'case'
        self[ns_pre + '_order'] = order

        self[nm_pre + '_name'] = label
        self[nm_pre + '_orth'] = orth
        self[nm_pre + '_feat1_name'] = 'case'
        self[nm_pre + '_feat1_value'] = label
        cur_nm += 1

      # create det slot and morph
      if pat in ('det', 'noun-det'):
        if last_ds_order and last_ds_order != order:
          cur_di = 'det-slot' + str(cur_ds)
          cur_ds += 1
          cur_dm = 1

        ds_pre = 'det-slot' + str(cur_ds)
        dm_pre = ds_pre + '_morph' + str(cur_dm)

        self[ds_pre + '_input1_type'] = cur_di
        self[ds_pre + '_name'] = 'case'
        self[ds_pre + '_order'] = order

        self[dm_pre + '_name'] = label
        self[dm_pre + '_orth'] = orth
        self[dm_pre + '_feat1_name'] = 'case'
        self[dm_pre + '_feat1_value'] = label
        cur_dm += 1

      # create adposition
      if pat == 'np':
        adp_pre = 'adp' + str(cur_adp)
        self[adp_pre + '_orth'] = orth
        self[adp_pre + '_order'] = order
        self[adp_pre + '_feat1_name'] = 'case'
        self[adp_pre + '_feat1_value'] = label

    self.convert_key('nom-case-label', 'nom-acc-nom-case-name')
    self.convert_key('acc-case-label', 'nom-acc-acc-case-name')

    self.convert_key('erg-case-label', 'erg-abs-erg-case-name')
    self.convert_key('abs-case-label', 'erg-abs-abs-case-name')

    self.convert_key('s-case-label', 'tripartite-s-case-name')
    self.convert_key('a-case-label', 'tripartite-a-case-name')
    self.convert_key('o-case-label', 'tripartite-o-case-name')

    for p in ['nom', 'acc', 'erg', 'abs', 's', 'a', 'o']:
      self.delete(p + '-case-pat')
      self.delete(p + '-case-order')
      self.delete(p + '-case-orth')

    for verb in self['verb']:
      v = verb.get('valence')
      if v == 'intrans':
        if cm == 'none':
          pass
        elif cm == 'nom-acc':
          self['valence'] = 'nom'
        elif cm == 'erg-abs':
          self['valence'] = 'abs'
        elif cm == 'tripartite':
          self['valence'] = 's'
      elif v == 'trans':
        if cm == 'none':
          pass
        elif cm == 'nom-acc':
          self['valence'] = 'nom-acc'
        elif cm == 'erg-abs':
          self['valence'] = 'erg-abs'
        elif cm == 'tripartite':
          self['valence'] = 'a-o'

  def convert_5_to_6(self):
    self.convert_key('aux-order', 'aux-comp-order')
    self.convert_key('aux-verb', 'aux1_orth')
    self.convert_value('aux-sem', 'pred', 'add-pred')
    self.convert_key('aux-sem', 'aux1_sem')
    self.convert_key('aux-comp', 'aux1_comp')
    self.convert_key('aux-pred', 'aux1_pred')
    self.convert_key('aux-subj', 'aux1_subj')
    if self.get('aux1_orth'):
      self['has-aux'] = 'yes'
    elif len(self):  # don't add this if the choices file is empty
      self['has-aux'] = 'no'

    for verb in self['verb']:
      self.delete('_'.join([verb.full_key, 'non-finite']))

  def convert_6_to_7(self):
    # Lexical types now have multiple stems
    for lextype in ['noun', 'verb', 'det']:
      for lt in self[lextype]:
        self.convert_key('orth', 'stem1_orth', key_prefix=lt.full_key)
        self.convert_key('pred', 'stem1_pred', key_prefix=lt.full_key)

    if not self.get('person') and len(self):
      self['person'] = 'none'

  def convert_7_to_8(self):
    # Other features no longer use the magic word 'root', they instead
    # use the name of the feature.
    for feature in self['feature']:
      fname = feature['name']
      for value in feature.get('value',[]):
        for st in value.get('supertype',[]):
          self.convert_value(st.full_key + '_name', 'root', fname)

  def convert_8_to_9(self):
    # finite and nonfinite feature value name changes
    # in aux complement form values
    for aux in self['aux']:
      self.convert_value(aux.full_key + '_compform','fin','finite')
      self.convert_value(aux.full_key + '_compform', 'nf', 'nonfinite')
    # in slot feature values
    for lextype in ['aux','det','verb','noun']:
      for slot in self[lextype + '-slot']:
        for morph in slot.get('morph',[]):
          for feat in morph.get('feat',[]):
            self.convert_value(feat.full_key + '_value','fin','finite')
            self.convert_value(feat.full_key + '_value','nf','nonfinite')

  def convert_9_to_10(self):
    """
    Previous versions defined (only) nonfinite compforms for each
    auxiliary iff the aux comp was constrained to be nonfinite.
    The current version creates a hierarchy of verb forms and then
    for each aux constrains the form of the complement. For each
    auxiliary, this conversion takes the value of the specified
    (nonfinite) compform and assigns it as the value of a member of
    the nonfinite hierarchy as well as the value of the auxiliary's
    compform.
    """
    self.convert_key('non-past', 'nonpast')
    self.convert_key('non-future', 'nonfuture')

    for aux in self['aux']:
      v = aux['nonfincompform']
      k = 'nf-subform' + aux.iter_num() + '_name'
      self.convert_value(aux.full_key + '_compform', 'nonfinite', v)

      if 'nonfincompform' in aux:
        self[k] = v
        #self.delete(aux.full_key + '_nonfincompform', prune=True)
        self.delete(aux.full_key + '_nonfincompform')

  def convert_10_to_11(self):
    """
    Previous versions allowed only one stem per auxiliary type.
    This conversion changes auxiliary orth and pred values to stem1 orth and pred.
    """
    for aux in self['aux']:
      self.convert_key('orth', 'stem1_orth', key_prefix=aux.full_key)
      self.convert_key('pred', 'stem1_pred', key_prefix=aux.full_key)

  def convert_11_to_12(self):
    """
    Previously the kind of comp (s, vp, v) was defined for each auxiliary type.
    Since our current word order implementation couldn't handle differences on this level anyway,
    this is no answered by one question for all auxiliaries.
    This conversion gives aux-comp the value of the first aux's comp, and deletes all type specific aux-comp values.
    """

    if self.get('has-aux') == 'yes':
      # just need the first (non-empty) item
      auxval = [aux.get('comp') for aux in self['aux']][0]
      self['aux-comp'] = auxval
      for aux in self['aux']:
          self.delete(aux.full_key + '_comp')

  def convert_12_to_13(self):
    """
    ERB: stupidly used "+" as a feature value.  Updating this
    to "plus".  Feature name was "negation".
    """
    for lextype in ['aux','det','verb','noun']:
      for lt in self[lextype + '-slot']:
        for morph in lt.get('morph',[]):
          for feat in morph.get('feat',[]):
            if feat['name'] == 'negation':
              self.convert_value(feat.full_key + '_value','+','plus')

  def convert_13_to_14(self):
    """
    Revised the Person subpage.  Convert the old single radio button
    for defining subtypes under 1p-non-sg into the choices for defining
    your own subtypes.
    """
    numbers = [num['name'] for num in self['numbers']]

    number = ', '.join(numbers[1:])

    fp = self.get('first-person')
    subtypes = []
    if fp == 'incl-excl':
      self['incl-excl-number'] = number
    elif fp == 'min-incl':
      subtypes = ['min', 'incl']
    elif fp == 'aug-incl':
      subtypes = ['aug']
    elif fp == 'min-aug':
      subtypes = ['min', 'incl', 'aug']

    if len(subtypes) and len(number):
      self['first-person'] = 'other'
      for person_subtype in self['person-subtype']:
        for st in subtypes:
          person_subtype['name'] = st
          person_subtype['number'] = number

  def convert_14_to_15(self):
    """
    Revised slot co-occurrence constraints in the Lexicon subpage.
    Before, there were three iterators, req, disreq, and forces, each
    of which contained a single choice, type.  Now there's a single
    iterator, constraint, that contains the choices type (req, disreq,
    or forces) and other-slot.
    """

    for slotprefix in ('noun', 'verb', 'det', 'aux'):
      for slot in self.get(slotprefix + '-slot',[]):
        constraints = []

        for contype in ('forces', 'req', 'disreq'):
          for ct in slot.get(contype, []):
            constraints += [ [ contype, ct.get('type') ] ]
            #self.delete(ct.full_key + '_type', prune=True)
            self.delete(ct.full_key + '_type')

        for i, c in enumerate(constraints):
          constraint_key = slot.full_key + '_constraint%d' % (i+1)
          self[constraint_key + '_type'] = c[0]
          self[constraint_key + '_other-slot'] = c[1]

  def convert_15_to_16(self):
    """
    Removes the feature MARK. Converts MARK feature to featureX_name (Other Features)
    where X places it at the end of the list of other features:
    --mark -> featureX_name=mark
    --featureX_type=head 
    All MARK values are converted to featureX values:
    --markY_name=mY -> featureX_valueY_name=mY
    --featureX_valueY_supertype_name=mark
    """
    mvalues = [mark['name'] for mark in self['mark']]

    if len(mvalues) == 0:
      return
    next_feat_index = len(self.get('feature',[])) + 1
    feat_key = 'feature%d' % (next_feat_index)

    self[feat_key + '_name'] = 'mark'
    self[feat_key + '_type'] = 'head'

    for i, mv in enumerate(mvalues):
      val_key = '_value%d' % (i+1)
      self[feat_key + val_key + '_name'] = mv
      self[feat_key + val_key + '_supertype1_name'] = 'mark'

  def convert_16_to_17(self):
    """
    Relates to Auxiliary complement feature definition:
    --replaces 'compvalue' with 'value'
    --replaces compform=Y with compfeatureX_name=form, compfeature_value=Y
    """
    for aux in self['aux']:
      complementform = aux.get('compform')
      for cf in aux.get('compfeature',[]):
        self.convert_key('compvalue', 'value', key_prefix=cf.full_key)
      index = str(len(aux.get('compfeature', [])) + 1)
      new_key = aux.full_key + '_compfeature' + index
      self[new_key + '_name'] = 'form'
      self[new_key + '_value'] = complementform
      #self.delete(aux.full_key + '_compform', prune=True)
      self.delete(aux.full_key + '_compform')

  def convert_17_to_18(self):
    """
    Retrofitted yesno questions library to integrate question affixes
    with morphotactic infrastructure.  'aux-main' possibility for
    q-infl-type said in the prose 'any finite verb', but I don't think
    we had actually implemented this.  This translation does not
    put [FORM fin] on the q-infl rule, since this rule will end up
    as a separate path from any other verbal inflection, again mimicking
    what was going on in the old system.
    """
    if self.get('q-infl') == 'on':
      n = len(self['verb-slot']) + 1
      pref = 'verb-slot' + str(n)
      if self.get('ques-aff') == 'suffix':
        self[pref + '_order'] = 'after'
      if self.get('ques-aff') == 'prefix':
        self[pref + '_order'] = 'before'
      if self.get('q-infl-type') == 'main':
        self[pref + '_input1_type'] = 'iverb'
        self[pref + '_input2_type'] = 'tverb'
      if self.get('q-infl-type') == 'aux':
        self[pref + '_input1_type'] = 'aux'
      if self.get('q-infl-type') == 'aux-main':
        self[pref + '_input1_type'] = 'verb'
      if 'ques-aff-orth' in self:
        self[pref + '_morph1_orth'] = self.get('ques-aff-orth')
      self[pref + '_name'] = 'q-infl'
      self[pref + '_morph1_feat1_name'] = 'question'
      self[pref + '_morph1_feat1_value'] = 'plus'
      self[pref + '_opt'] = 'on'

  def preparse_convert_18_to_19(self, key, value):
    """
    Convert the old test sentence choices to the new iterator format.
    """
    if key.startswith('sentence'):
      key += '_orth'
    return (key, value)

  def convert_18_to_19(self):
    """
    Do nothing here. All conversion for version 19 is in the method
    preparse_convert_18_to_19(). This stub is here for record keeping.
    """
    pass # version 19 only requires preparse conversion

  def convert_19_to_20(self):
    """
    Removed question on verbal clusters from the word order library. They are
    now formed when free word order has v-comp and more than one auxiliary per
    clause, and always for VSO, OSV when having v-comp, other conditions remain
    as before. For other word orders the verbal cluster question is ignored.
    """
    if self.get('has-aux') == 'yes':
      wo = self.get('word-order')
      if self.get('aux-comp') == 'v':
        if wo == 'free':
          self.convert_key('v-cluster','multiple-aux')
        elif wo == 'vso' or wo == 'osv':
          if self.get('v-cluster') == 'no':
            self.convert_value('aux-comp','v','vp')
    else:
      pass
#if v-comp if free word order if v-cluster more than one aux, if no cluster 1max
    # if svo,ovs do nothing, else v-comp is vp-comp
