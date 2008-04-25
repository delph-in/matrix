######################################################################
# imports

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
          (key, value) = l.split('=')
          self.set(key, value)
    except:
      pass

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
    self.iter_stack.append([key, 1, True])
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

  # Return true iff there if 'key' is currently set
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

  # Return true iff there if 'key' is currently set
  def is_set(self, key):
    return self.is_set_full(self.iter_prefix() + key)

  
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
    return 4


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
