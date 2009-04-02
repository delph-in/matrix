### $Id: choices.py,v 1.24 2008-09-30 23:50:02 lpoulson Exp $

######################################################################
# imports

import re

######################################################################
# globals

######################################################################
# ChoicesFile is a class that wraps the choices file, a list of
# attributes and values, and provides methods for loading, accessing,
# and saving them.

class ChoicesFile:
  # initialize by passing either a file name or ???file handle
  def __init__(self, choices_file):
    self.file_name = choices_file
    self.iter_stack = []
    self.choices = {}
    try:
      if type(choices_file) == str:
        f = open(choices_file, 'r')
      else:
        f = choices_file
        f.seek(0)

      lines = f.readlines()

      if type(choices_file) == str:
        f.close()

      for l in lines:
        l = l.strip()
        if l:
          (key, value) = l.split('=', 1)
          self.set(key, value)
    except:
      pass

    if choices_file:  # don't up-rev if we're creating an empty ChoicesFile
      if self.is_set('version'):
        version = int(self.get('version'))
      else:
        version = 0
      if version < 1:
        self.convert_0_to_1()
      if version < 2:
        self.convert_1_to_2()
      if version < 3:
        self.convert_2_to_3()
      if version < 4:
        self.convert_3_to_4()
      if version < 5:
        self.convert_4_to_5()
      if version < 6:
        self.convert_5_to_6()
      if version < 7:
        self.convert_6_to_7()
      if version < 8:
        self.convert_7_to_8()
      if version < 9:
        self.convert_8_to_9()
      if version < 10:
        self.convert_9_to_10()
      if version < 11:
        self.convert_10_to_11()
      if version < 12:
        self.convert_11_to_12()
      if version < 13:
        self.convert_12_to_13()
      if version < 14:
        self.convert_13_to_14()
      if version < 15:
        self.convert_14_to_15()
      if version < 16:
        self.convert_15_to_16()
      # As we get more versions, add more version-conversion methods, and:
      # if version < N:
      #   self.convert_N-1_to_N

    # Remove pseudo-choices that don't actually represent the answer to
    # any question in the questionnaire.
    self.delete('version')
    self.delete('section')


  # Return the keys for the choices dict
  def keys(self):
    return self.choices.keys()


  ######################################################################
  # Choices values and iterators:
  #
  # The ChoicesFile data is stored in a flat dictionary, but the names
  # of the values in that dictionary form a hierarchical structure.
  # Names consist of strings of alphabetic (plus dash) strings,
  # optionally separated into segments by a trailing number and an
  # underscore.  For example, noun2_morph would be the morph value of
  # the second of a series of nouns, while noun2_morph3_orth would be
  # the orth value of the third morph value of the second noun.
  #
  # The programmer deals with iterators using the iter_begin,
  # iter_next, and iter_end methods.  iter_begin starts the iteration,
  # pushing a value name on the stack.  iter_next moves to the next
  # integer value.  iter_end pops a value off the stack.
  #
  # Example code:
  #
  #   choices.iter_begin('noun')       # iterate through all nouns
  #   while choices.iter_valid():
  #     type = choices.get('type')
  #     choices.iter_begin('morph')    # sub-iterate through all morphs
  #     while choices.iter_valid():
  #       orth = choices.get('orth')
  #       order = choices.get('order')
  #       choices.iter_next()          # advance the morph iteration
  #     choices.iter_end()             # end the morph iteration
  #     choices.iter_next()            # advance the noun iteration
  #   choices.iter_end()               # end the noun iteration
  
  def iter_begin(self, key):
    var = 1
    # If key contains a number at the end (i.e. 'noun5'), initialize
    # the iteration at that number rather than the default of 1.
    s = re.search('[0-9]+$', key)
    if s:
      offset = s.span()[0]
      var = int(key[offset:])
      key = key[0:offset]
      
    self.iter_stack.append([key, var, True])
    self.iter_stack[-1][2] = self.__calc_iter_valid()

  # Are there any choices with a name prefixed by the current
  # iterator?  Useful as the condition in loops.
  def iter_valid(self):
    if len(self.iter_stack) > 0:
      return self.iter_stack[-1][2]
    else:
      return False

  def iter_next(self):
    self.iter_stack[-1][1] += 1
    self.iter_stack[-1][2] = self.__calc_iter_valid()

  def iter_end(self):
    self.iter_stack.pop()

  def iter_prefix(self):
    prefix = ''
    for i in self.iter_stack:
      prefix += i[0] + str(i[1]) + '_'
    return prefix

  # Based on the current top of the iterator stack, decide if the
  # iterator is valid -- that is, if there are exist any values in the
  # choices dictionary for which the current iterator is a prefix.
  # Users should not call this.
  def __calc_iter_valid(self):
    prefix = self.iter_prefix()
    valid = False
    for k in self.keys():
      if k[0:len(prefix)] == prefix:
        return True
    return False

  ######################################################################
  # Methods for saving and restoring the iterator state (the stack)

  def iter_state(self):
    return self.iter_stack

  def iter_set_state(self,state):
    self.iter_stack = state

  def iter_reset(self):
    self.iter_stack = []


  ######################################################################
  # Methods for accessing full-name values.  These methods are
  # insensitive to the current iterator state, and take the full name
  # of a dictionary entry (e.g. noun2_morph) rather than

  # Return the value of 'key', if any.  If not, return the empty string.
  def get_full(self, key):
    if self.is_set_full(key):
      return self.choices[key]
    else:
      return ''

  # Set the value of 'key' to 'value'
  def set_full(self, key, value):
    self.choices[key] = value

  # Remove 'key' and its value from the list of choices
  def delete_full(self, key):
    if self.is_set_full(key):
      del self.choices[key]

  # Return True iff there if 'key' is currently set
  def is_set_full(self, key):
    return key in self.choices

  ######################################################################
  # Methods for accessing values.  These methods are sensitive to the
  # current iterator state, prepending the current iter_prefix to the
  # passed keys.

  # Return the value of 'key', if any.  If not, return the empty string.
  def get(self, key):
    return self.get_full(self.iter_prefix() + key)

  # Set the value of 'key' to 'value'
  def set(self, key, value):
    self.set_full(self.iter_prefix() + key, value)

  # Remove 'key' and its value from the list of choices
  def delete(self, key):
    self.delete_full(self.iter_prefix() + key)

  # Return True iff there if 'key' is currently set
  def is_set(self, key):
    return self.is_set_full(self.iter_prefix() + key)


  ######################################################################
  # Methods for accessing "derived" values -- that is, groups of values
  # that are implied by the list of choices, but not directly stored
  # in it.  For example, it is convenient to be able to get a list of
  # all features defined in the languages, even though they're not
  # all stored in a single place.

  def has_noun_case(self, case = ''):
    """
    Returns True iff the target language has either morphologically or
    lexically marked case (restricting the calculation to the
    passed-in case if it's non-empty).
    """

    result = False

    state = self.iter_state()
    self.iter_reset()

    # check lexical types
    self.iter_begin('noun')
    while self.iter_valid():
      self.iter_begin('feat')
      while self.iter_valid():
        if self.get('name') == 'case' and \
           (self.get('value') == case or not case):
          result = True
        self.iter_next()
      self.iter_end()
      self.iter_next()
    self.iter_end()

    # check morphemes
    for slotprefix in ('noun', 'verb', 'det'):
      self.iter_begin(slotprefix + '-slot')
      while self.iter_valid():
        self.iter_begin('morph')
        while self.iter_valid():
          self.iter_begin('feat')
          while self.iter_valid():
            if self.get('name') == 'case' and \
               (self.get('value') == case or not case):
              result = True
            self.iter_next()
          self.iter_end()
          self.iter_next()
        self.iter_end()
        self.iter_next()
      self.iter_end()

    self.iter_set_state(state)

    return result


  def has_adp_case(self, case = '', check_opt = False):
    """
    Returns True iff the target language has case-marking adpositions
    (restricting the calculation to the passed-in case if it's
    non-empty).  If the check_opt argument is True, only return True
    if the adposition is optional.
    """

    result = False

    state = self.iter_state()
    self.iter_reset()

    self.iter_begin('adp')
    while self.iter_valid():
      opt = self.get('opt')
      self.iter_begin('feat')
      while self.iter_valid():
        if self.get('name') == 'case' and \
           (self.get('value') == case or not case) and \
           (opt or not check_opt):
          result = True
        self.iter_next()
      self.iter_end()
      self.iter_next()
    self.iter_end()

    self.iter_set_state(state)

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
    result = False

    state = self.iter_state()
    self.iter_reset()

    self.iter_begin('scale')
    result = self.iter_valid()

    self.iter_set_state(state)

    return result


  def has_scale(self):
    """
    Returns True iff the target language requires the SCALE feature,
    which contains the arguments in the order they are ranked by
    the direct-inverse hierarchy.
    """
    result = False

    state = self.iter_state()
    self.iter_reset()

    # verb lexical items
    self.iter_begin('verb')
    while self.iter_valid():
      self.iter_begin('feat')
      while self.iter_valid():
        if self.get('head') in ['higher', 'lower']:
          result = True
        self.iter_next()
      self.iter_end()
      self.iter_next()
    self.iter_end()

    # verb morphs
    self.iter_begin('verb-slot')
    while self.iter_valid():
      self.iter_begin('morph')
      while self.iter_valid():
        self.iter_begin('feat')
        while self.iter_valid():
          if self.get('head') in ['higher', 'lower']:
            result = True
          self.iter_next()
        self.iter_end()
        self.iter_next()
      self.iter_end()
      self.iter_next()
    self.iter_end()

    self.iter_set_state(state)

    return result


  # cases()
  #   Create and return a list containing information about the cases
  #   in the language described by the current choices.  This list consists
  #   of tuples with three values:
  #     [canonical name, friendly name, abbreviation]
  def cases(self):
    # first, make two lists: the canonical and user-provided case names
    cm = self.get_full('case-marking')
    canon = []
    user = []
    if cm == 'nom-acc':
      canon.append('nom')
      user.append(self.get_full(cm + '-nom-case-name'))
      canon.append('acc')
      user.append(self.get_full(cm + '-acc-case-name'))
    elif cm == 'erg-abs':
      canon.append('erg')
      user.append(self.get_full(cm + '-erg-case-name'))
      canon.append('abs')
      user.append(self.get_full(cm + '-abs-case-name'))
    elif cm == 'tripartite':
      canon.append('s')
      user.append(self.get_full(cm + '-s-case-name'))
      canon.append('a')
      user.append(self.get_full(cm + '-a-case-name'))
      canon.append('o')
      user.append(self.get_full(cm + '-o-case-name'))
    elif cm in ['split-s']:
      canon.append('a')
      user.append(self.get_full(cm + '-a-case-name'))
      canon.append('o')
      user.append(self.get_full(cm + '-o-case-name'))
    elif cm in ['fluid-s']:
      a_name = self.get_full(cm + '-a-case-name')
      o_name = self.get_full(cm + '-o-case-name')
      canon.append('a+o')
      user.append('fluid')
      canon.append('a')
      user.append(a_name)
      canon.append('o')
      user.append(o_name)
    elif cm in ['split-n', 'split-v']:
      canon.append('nom')
      user.append(self.get_full(cm + '-nom-case-name'))
      canon.append('acc')
      user.append(self.get_full(cm + '-acc-case-name'))
      canon.append('erg')
      user.append(self.get_full(cm + '-erg-case-name'))
      canon.append('abs')
      user.append(self.get_full(cm + '-abs-case-name'))
    elif cm in ['focus']:
      canon.append('focus')
      user.append(self.get_full(cm + '-focus-case-name'))
      canon.append('a')
      user.append(self.get_full(cm + '-a-case-name'))
      canon.append('o')
      user.append(self.get_full(cm + '-o-case-name'))

    # fill in any additional cases the user has specified
    state = self.iter_state()
    self.iter_reset()

    self.iter_begin('case')
    while self.iter_valid():
      name = self.get('name')
      canon.append(name)
      user.append(name)
      self.iter_next()
    self.iter_end()

    self.iter_set_state(state)

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
    cm = self.get_full('case-marking')
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
    if self.get('scale1_feat1_name'):
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

    state = self.iter_state()
    self.iter_reset()

    self.iter_begin('number')
    while self.iter_valid():
      name = self.get('name')

      stype = ''
      self.iter_begin('supertype')
      while self.iter_valid():
        if stype:
          stype += ';'
        stype += self.get('name')
        self.iter_next()
      self.iter_end()

      if not stype:
        stype = 'number'

      numbers += [[name, stype]]
      
      self.iter_next()
    self.iter_end()

    self.iter_set_state(state)

    return numbers


  # persons()
  #   Create and return a list containing information about the values
  #   of the person feature implied by the current choices.
  #   This list consists of tuples:
  #     [name, supertype]
  def persons(self):
    persons = []

    person = self.get_full('person')
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

    state = self.iter_state()
    self.iter_reset()

    fp = self.get('first-person')
    if fp not in ['', 'none']:
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
              self.iter_begin('person-subtype')
              while self.iter_valid():
                name = self.get('name')
                for num in self.get('number').split(', '):
                  if num == n:
                    pernums += [[pn + '_' + name, pn]]
                self.iter_next()
              self.iter_end()

    self.iter_set_state(state)

    return pernums


  # genders()
  #   Create and return a list containing information about the
  #   genders implied by the current choices.
  #   This list consists of tuples:
  #     [name, supertype;supertype;...]
  def genders(self):
    genders = []

    state = self.iter_state()
    self.iter_reset()

    self.iter_begin('gender')
    while self.iter_valid():
      name = self.get('name')

      stype = ''
      self.iter_begin('supertype')
      while self.iter_valid():
        if stype:
          stype += ';'
        stype += self.get('name')
        self.iter_next()
      self.iter_end()

      if not stype:
        stype = 'gender'

      genders += [[name, stype]]
      
      self.iter_next()
    self.iter_end()

    self.iter_set_state(state)

    return genders

  # forms()
  #   Create and return a list containing the values of the FORM
  #   feature that constrains the form of auxiliary complements as
  #   defined in the current choices.
  #   This list consists of tuples:
  #     [form name]
  def forms(self):
    forms = []

    state = self.iter_state()
    self.iter_reset()
    
    if self.get('has-aux') == 'yes' or  self.get('noaux-fin-nf') == 'on':
      forms += [ ['finite'], ['nonfinite'] ]

      for p in ['nf', 'fin']: 
        self.iter_begin(p + '-subform')
        while self.iter_valid():
          forms += [ [self.get('name')] ]

          self.iter_next()
        self.iter_end()

    self.iter_set_state(state)

    return forms

  # tenses()
  #   Create and return a list containing information about the values
  #   of the TENSE feature implied by the current choices.
  #   This list consists of tuples:
  #     [tense name]
  def tenses(self):
    tenses = []

    state = self.iter_state()
    self.iter_reset()

    tdefn = self.get_full('tense-definition')
    
    if tdefn == 'choose':

      for ten in ('past', 'present', 'future', 'nonpast', 'nonfuture'):

        if self.is_set(ten):
          tenses += [ [ten] ]
          self.iter_begin(ten + '-subtype')
          while self.iter_valid():
            tenses += [ [self.get('name')] ]
            self.iter_next()
          self.iter_end()

    elif tdefn == 'build':
      self.iter_begin('tense')
      while self.iter_valid():
        tenses += [ [self.get('name')] ]
        self.iter_next()
      self.iter_end()

    self.iter_set_state(state)

    return tenses

  # aspects()
  #   Create and return a list containing information about the values
  #   of the viewpoint ASPECT feature implied by the current choices.
  #   This list consists of tuples:
  #     [aspect name]
  def aspects(self):
    aspects = []

    state = self.iter_state()
    self.iter_reset()

    self.iter_begin('aspect')
    while self.iter_valid():
      aspects += [ [self.get('name')] ]
      self.iter_next()
    self.iter_end()

    self.iter_set_state(state)

    return aspects

  # situations()
  #   Create and return a list containing information about the values
  #   of the SITUATION aspect feature implied by the current choices.
  #   This list consists of tuples:
  #     [situation name]
  def situations(self):
    situations = []

    state = self.iter_state()
    self.iter_reset()

    self.iter_begin('situation')
    while self.iter_valid():
      situations += [ [self.get('name')] ]
      self.iter_next()
    self.iter_end()

    self.iter_set_state(state)

    return situations

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
    values = ''
    for c in self.cases():
      if values:
        values += ';'
      values += c[0] + '|' + c[1]

    if values:
      features += [ ['case', values, 'LOCAL.CAT.HEAD.CASE'] ]

    # Number, Person, and Pernum
    pernums = self.pernums()
    if pernums:
      values = ''
      for pn in pernums:
        if values:
          values += ';'
        values += pn[0] + '|' + pn[0]

      if values:
        features += [ ['pernum', values, 'LOCAL.CONT.HOOK.INDEX.PNG.PERNUM'] ]
    else:
      values = ''
      for n in self.numbers():
        if values:
          values += ';'
        values += n[0] + '|' + n[0]

      if values:
        features += [ ['number', values, 'LOCAL.CONT.HOOK.INDEX.PNG.NUM'] ]

      values = ''
      for p in self.persons():
        if values:
          values += ';'
        values += p[0] + '|' + p[0]

      if values:
        features += [ ['person', values, 'LOCAL.CONT.HOOK.INDEX.PNG.PER'] ]

    # Gender
    values = ''
    for g in self.genders():
      if values:
        values += ';'
      values += g[0] + '|' + g[0]

    if values:
      features += [ ['gender', values, 'LOCAL.CONT.HOOK.INDEX.PNG.GEND'] ]

    # Case patterns
    values = ''
    for p in self.patterns():
      if values:
        values += ';'
      if p[2]:
        values += p[0] + '|' + p[1]

    if values:
      features += [ ['argument structure', values, ''] ]
    
    # Form (auxiliary complement form)
    values = ''
    for f in self.forms():
      if values:
        values += ';'
      values += f[0] + '|' + f[0]

    if values:
      features += [ ['form', values, 'LOCAL.CAT.HEAD.FORM'] ]

    # Tense
    values = ''
    for t in self.tenses():
      if values:
        values += ';'
      values += t[0] + '|' + t[0]
    
    if values:
      features += [ ['tense', values, 'LOCAL.CONT.HOOK.INDEX.E.TENSE'] ]

    # Viewpoint Aspect
    values = ''
    for a in self.aspects():
      if values:
        values += ';'
      values += a[0] + '|' + a[0]
    
    if values:
      features += [ ['aspect', values, 'LOCAL.CONT.HOOK.INDEX.E.ASPECT'] ]

    #Situation Aspect
    values = ''
    for s in self.situations():
      if values:
        values += ';'
      values += s[0] + '|' + s[0]
    
    if values:
      features += [ ['situation', values, 'LOCAL.CONT.HOOK.INDEX.E.SITUATION'] ]

    # Direction
    if self.get_full('scale1_feat1_name'):
      features += [ ['direction',
                     'dir|direct;inv|inverse',
                     'LOCAL.CAT.HEAD.DIRECTION'] ]

    # Negaton
    if self.get_full('infl-neg'):
      features += [ ['negation', 'plus|plus', '' ] ]

    # Other features
    state = self.iter_state()
    self.iter_reset()

    self.iter_begin('feature')
    while self.iter_valid():
      feat = self.get('name')
      type = self.get('type')
      values = ''

      self.iter_begin('value')
      while self.iter_valid():
        val = self.get('name')

        if values:
          values += ';'
        values += val + '|' + val
        
        self.iter_next()
      self.iter_end()

      geom = ''
      if type == 'head':
        geom = 'LOCAL.CAT.HEAD.' + feat.upper()
      else:
        geom = 'LOCAL.CONT.HOOK.INDEX.PNG.' + feat.upper()

      if values:
        features += [ [feat, values, geom] ]
      
      self.iter_next()
    self.iter_end()

    self.iter_set_state(state)

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
    return 16


  def convert_value(self, key, old, new):
    if self.is_set(key) and self.get(key) == old:
      self.set(key, new)


  def convert_key(self, old, new):
    if self.is_set(old):
      self.set(new, self.get(old))
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
    if self.is_set('ques'):
      ques = self.get('ques')
      self.delete('ques')
      if ques == 'inv':
        ques = 'q-inv'
      if ques != 'int':
        self.set(ques, 'on')

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
      if len(self.keys()):  # don't add this if the choices file is empty
        self.set('case-marking', 'none')
    elif Sorth == Aorth and Sorth != Oorth:
      self.set('case-marking', 'nom-acc')
      self.set('nom-case-label', 'nominative')
      self.set('acc-case-label', 'accusative')
      if Aorth:
        self.set('nom-case-pat', 'np')
        self.set('nom-case-order', Aorder)
      else:
        self.set('nom-case-pat', 'none')
      if Oorth:
        self.set('acc-case-pat', 'np')
        self.set('acc-case-order', Oorder)
      else:
        self.set('acc-case-pat', 'none')
    elif Sorth != Aorth and Sorth == Oorth:
      self.set('case-marking', 'erg-asb')
      self.set('erg-case-label', 'ergative')
      self.set('abs-case-label', 'absolutive')
      if Aorth:
        self.set('erg-case-pat', 'np')
        self.set('erg-case-order', Aorder)
      else:
        self.set('erg-case-pat', 'none')
      if Oorth:
        self.set('abs-case-pat', 'np')
        self.set('abs-case-order', Oorder)
      else:
        self.set('abs-case-pat', 'none')
    else:
      self.set('case-marking', 'tripartite')
      self.set('s-case-label', 'subjective')
      self.set('a-case-label', 'agentive')
      self.set('o-case-label', 'objective')
      if Sorth:
        self.set('s-case-pat', 'np')
        self.set('s-case-order', Sorder)
      else:
        self.set('s-case-pat', 'none')
      if Aorth:
        self.set('a-case-pat', 'np')
        self.set('a-case-order', Aorder)
      else:
        self.set('a-case-pat', 'none')
      if Oorth:
        self.set('o-case-pat', 'np')
        self.set('o-case-order', Oorder)
      else:
        self.set('o-case-pat', 'none')

    self.delete('iverb-subj')
    self.delete('tverb-subj')
    self.delete('tverb-obj')
    self.delete('subj-adp-orth')
    self.delete('subj-adp-order')
    self.delete('obj-adp-orth')
    self.delete('obj-adp-order')

  def convert_3_to_4(self):
    # Added a fuller implementation of case marking on core arguments,
    # so convert the old case-marking adposition stuff to the new
    # choices
    self.convert_key('noun1', 'noun1_orth')
    self.convert_key('noun2', 'noun2_orth')

    self.convert_key('iverb', 'verb1_orth')
    self.convert_key('iverb-pred', 'verb1_pred')
    self.convert_key('iverb-non-finite', 'verb1_non-finite')
    if self.get('verb1_orth'):
      self.set('verb1_valence', 'intrans')

    self.convert_key('tverb', 'verb2_orth')
    self.convert_key('tverb-pred', 'verb2_pred')
    self.convert_key('tverb-non-finite', 'verb2_non-finite')
    if self.get('verb2_orth'):
      self.set('verb2_valence', 'trans')

    self.convert_key('det1', 'det1_orth')
    self.convert_key('det1pred', 'det1_pred')
    self.convert_key('det2', 'det2_orth')
    self.convert_key('det2pred', 'det2_pred')

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

        self.set(ns_pre + '_input1_type', cur_ni)
        self.set(ns_pre + '_name', 'case')
        self.set(ns_pre + '_order', order)

        self.set(nm_pre + '_name', label)
        self.set(nm_pre + '_orth', orth)
        self.set(nm_pre + '_feat1_name', 'case')
        self.set(nm_pre + '_feat1_value', label)
        cur_nm += 1

      # create det slot and morph
      if pat in ('det', 'noun-det'):
        if last_ds_order and last_ds_order != order:
          cur_di = 'det-slot' + str(cur_ds)
          cur_ds += 1
          cur_dm = 1

        ds_pre = 'det-slot' + str(cur_ds)
        dm_pre = ds_pre + '_morph' + str(cur_dm)

        self.set(ds_pre + '_input1_type', cur_di)
        self.set(ds_pre + '_name', 'case')
        self.set(ds_pre + '_order', order)

        self.set(dm_pre + '_name', label)
        self.set(dm_pre + '_orth', orth)
        self.set(dm_pre + '_feat1_name', 'case')
        self.set(dm_pre + '_feat1_value', label)
        cur_dm += 1

      # create adposition
      if pat == 'np':
        adp_pre = 'adp' + str(cur_adp)
        self.set(adp_pre + '_orth', orth)
        self.set(adp_pre + '_order', order)
        self.set(adp_pre + '_feat1_name', 'case')
        self.set(adp_pre + '_feat1_value', label)

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

    self.iter_begin('verb')
    while self.iter_valid():
      v = self.get('valence')
      if v == 'intrans':
        if cm == 'none':
          pass
        elif cm == 'nom-acc':
          self.set('valence', 'nom')
        elif cm == 'erg-abs':
          self.set('valence', 'abs')
        elif cm == 'tripartite':
          self.set('valence', 's')
      elif v == 'trans':
        if cm == 'none':
          pass
        elif cm == 'nom-acc':
          self.set('valence', 'nom-acc')
        elif cm == 'erg-abs':
          self.set('valence', 'erg-abs')
        elif cm == 'tripartite':
          self.set('valence', 'a-o')

      self.iter_next()
    self.iter_end()

  def convert_5_to_6(self):
    self.convert_key('aux-order', 'aux-comp-order')
    self.convert_key('aux-verb', 'aux1_orth')
    self.convert_value('aux-sem', 'pred', 'add-pred')
    self.convert_key('aux-sem', 'aux1_sem')
    self.convert_key('aux-comp', 'aux1_comp')
    self.convert_key('aux-pred', 'aux1_pred')
    self.convert_key('aux-subj', 'aux1_subj')
    if self.get('aux1_orth'):
      self.set('has-aux','yes')
    elif len(self.keys()):  # don't add this if the choices file is empty
      self.set('has-aux','no')

    self.iter_begin('verb')
    while self.iter_valid():
      self.delete('non-finite')
      self.iter_next()
    self.iter_end()

  def convert_6_to_7(self):
    # Lexical types now have multiple stems
    for lextype in ['noun', 'verb', 'det']:
      self.iter_begin(lextype)
      while self.iter_valid():
        self.convert_key('orth', 'stem1_orth')
        self.convert_key('pred', 'stem1_pred')

        self.iter_next()
      self.iter_end()

    if not self.get('person') and len(self.keys()):
      self.set('person', 'none')

  def convert_7_to_8(self):
    # Other features no longer use the magic word 'root', they instead
    # use the name of the feature.
    self.iter_begin('feature')
    while self.iter_valid():
      fname = self.get('name')
      
      self.iter_begin('value')
      while self.iter_valid():
        self.iter_begin('supertype')
        while self.iter_valid():
          self.convert_value('name', 'root', fname)

          self.iter_next()
        self.iter_end()

        self.iter_next()
      self.iter_end()

      self.iter_next()
    self.iter_end()

  def convert_8_to_9(self):
    # finite and nonfinite feature value name changes
    # in aux complement form values
    for lextype in ['aux','det','verb','noun']:
      if lextype == 'aux':
        self.iter_begin(lextype)
        while self.iter_valid():
          self.convert_value('compform','fin','finite')
          self.convert_value('compform', 'nf', 'nonfinite')
          self.iter_next()
        self.iter_end()
      # in slot feature values
      self.iter_begin(lextype + '-slot')
      while self.iter_valid():
        self.iter_begin('morph')
        while self.iter_valid():
          self.iter_begin('feat')
          while self.iter_valid():
            self.convert_value('value','fin','finite')
            self.convert_value('value','nf','nonfinite')
            self.iter_next()
          self.iter_end()
          self.iter_next()
        self.iter_end()
        self.iter_next()
      self.iter_end()  

  def convert_9_to_10(self):
    """
    Previous versions defined (only) nonfinite compforms for each auxiliary 
    iff the aux comp was constrained to be nonfinite. 
    The current version creates a hierarchy of verb forms and then for each 
    aux constrains the form of the complement.  
    For each auxiliary, this conversion takes the value of the specified (nonfinite)compform 
    and assigns it as the value of a member of the nonfinite hierarchy 
    as well as the value of the auxiliary's compform.
    """
    self.convert_key('non-past', 'nonpast')
    self.convert_key('non-future', 'nonfuture') 

    i = 0
    self.iter_begin('aux')
    while self.iter_valid():
      i += 1
      v = self.get('nonfincompform')
      k = 'nf-subform' + str(i) + '_name'
      self.convert_value('compform', 'nonfinite', v)

      if self.is_set('nonfincompform'):
        self.set_full(k,v)
      self.delete('nonfincompform')

      self.iter_next()
    self.iter_end()
  
  def convert_10_to_11(self):
    """
    Previous versions allowed only one stem per auxiliary type.
    This conversion changes auxiliary orth and pred values to stem1 orth and pred.
    """
    self.iter_begin('aux')

    while self.iter_valid():
      self.convert_key('orth', 'stem1_orth')
      self.convert_key('pred', 'stem1_pred')

      self.iter_next()
    self.iter_end()

  def convert_11_to_12(self):
    """
    Previously the kind of comp (s, vp, v) was defined for each auxiliary type.
    Since our current word order implementation couldn't handle differences on this level anyway,
    this is no answered by one question for all auxiliaries.
    This conversion gives aux-comp the value of the first aux's comp, and deletes all type specific aux-comp values.
    """
   
    if self.get('has-aux') == 'yes':
      auxval = self.get('aux1_comp')
      self.set('aux-comp',auxval)
      i = 0
      while self.iter_valid():
        i += 1
        self.delete('aux' + i + '_comp')

  def convert_12_to_13(self):
    # EB stupidly used "+" as a feature value.  Updating this
    # to "plus".  Feature name was "negation".
    for lextype in ['aux','det','verb','noun']:
      self.iter_begin(lextype + '-slot')
      while self.iter_valid():
        self.iter_begin('morph')
        while self.iter_valid():
          self.iter_begin('feat')
          while self.iter_valid():
            if self.get('name') == 'negation':
              self.convert_value('value','+','plus')
            self.iter_next()
          self.iter_end()
          self.iter_next()
        self.iter_end()
        self.iter_next()
      self.iter_end()  

  def convert_13_to_14(self):
    """
    Revised the Person subpage.  Convert the old single radio button
    for defining subtypes under 1p-non-sg into the choices for defining
    your own subtypes.
    """
    numbers = []
    self.iter_begin('number')
    while self.iter_valid():
      numbers += [ self.get('name') ]
      self.iter_next()
    self.iter_end()

    number = ''
    for n in numbers[1:]:
      if len(number):
        number += ', '
      number += n

    fp = self.get('first-person')
    subtypes = []
    if fp == 'incl-excl':
      self.set('incl-excl-number', number)
    elif fp == 'min-incl':
      subtypes = ['min', 'incl']
    elif fp == 'aug-incl':
      subtypes = ['aug']
    elif fp == 'min-aug':
      subtypes = ['min', 'incl', 'aug']

    if len(subtypes) and len(number):
      self.set('first-person', 'other')
      self.iter_begin('person-subtype')
      for st in subtypes:
        self.set('name', st)
        self.set('number', number)
        self.iter_next()
      self.iter_end()

  def convert_14_to_15(self):
    """
    Revised slot co-occurrence constraints in the Lexicon subpage.
    Before, there were three iterators, req, disreq, and forces, each
    of which contained a single choice, type.  Now there's a single
    iterator, constraint, that contains the choices type (req, disreq,
    or forces) and other-slot.
    """

    for slotprefix in ('noun', 'verb', 'det', 'aux'):
      self.iter_begin(slotprefix + '-slot')
      while self.iter_valid():
        constraints = []

        for contype in ('forces', 'req', 'disreq'):
          self.iter_begin(contype)
          while self.iter_valid():
            constraints += [ [ contype, self.get('type') ] ]
            self.delete('type')
            self.iter_next()
          self.iter_end()

        self.iter_begin('constraint')
        for c in constraints:
          self.set('type', c[0])
          self.set('other-slot', c[1])
          self.iter_next()
        self.iter_end()

        self.iter_next()
      self.iter_end()

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
    mvalues = []
    self.iter_begin('mark')
    while self.iter_valid():
      mvalues.append(self.get('name'))
      self.iter_next()
    self.iter_end()

    if len(mvalues) != 0:
      self.iter_begin('feature')
      while self.iter_valid():
        self.iter_next()
      self.set('name','mark')
      self.set('type','head')
      self.iter_begin('value')
      for mv in mvalues:
        self.set('name', mv)
        self.iter_begin('supertype')
        self.set('name','mark')
        self.iter_end()
        self.iter_next()
      self.iter_end()
      self.iter_end()
