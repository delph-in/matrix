#!/usr/local/bin/python

######################################################################
# imports

######################################################################
# globals

choices = {}
wrong = {}


######################################################################
# ch(s)
#   Return the value of choice s, or '' if it has none

def ch(s):
  if choices.has_key(s):
    return choices[s]
  else:
    return ''


######################################################################
# validate_language()
#   Validate the user's choice about language

def validate_language():
  if not ch('language'):
    wrong['language'] = 'You must specify the name of your language'


######################################################################
# validate_word_order()
#   Validate the user's choices about basic word order.

# There should be some value for word order
# If hasDets is true, there should be some value for NounDetOrder
# If auxverb is defined, then auxorder needs to be too
# I'm currently adding AUX as a feature in specialize_word_order()
# but wonder if instead (in addition?) we should be doing validation
# so that we don't find ourselves worrying about auxiliaries if we
# don't have any in the lexicon.

def validate_word_order():
  pass


######################################################################
# validate_sentential_negation()
#   Validate the user's choices about sentential negation.

# If affix is indicated, must select prefix/suffix and give form.
# If adverb is indicated, must give form.

def validate_sentential_negation():
  pass


######################################################################
# validate_coordination()
#   Validate the user's choices about coordination.

def validate_coordination():
  for n in (1, 2):
    i = str(n)
    if choices.has_key('ch' + i):
      csn =     ch('cs' + i + 'n')
      csnp =    ch('cs' + i + 'np')
      csvp =    ch('cs' + i + 'vp')
      css =     ch('cs' + i + 's')
      cspat =   ch('cs' + i + 'pat')
      csmark =  ch('cs' + i + 'mark')
      csorder = ch('cs' + i + 'order')
      csorth =  ch('cs' + i + 'orth')

      if not (csn or csnp or csvp or css):
        err = 'You must specify a phrase type for Coordination Strategy ' + i
        wrong['cs' + i + 'n'] = err
        wrong['cs' + i + 'np'] = err
        wrong['cs' + i + 'vp'] = err
        wrong['cs' + i + 's'] = err

      if cspat == 'a':
        if csmark:
          err = 'You must not specify word/affix for asyndetic Coordination Strategy ' + i
          wrong['cs' + i + 'mark'] = err
        if csorder:
          err = 'You must not specify before/after for asyndetic Coordination Strategy ' + i
          wrong['cs' + i + 'order'] = err
        if csorth:
          err = 'You must not specify a spelling for asyndetic Coordination Strategy ' + i
          wrong['cs' + i + 'orth'] = err
      else:
        if not cspat:
          err = 'You must specify a pattern for Coordination Strategy ' + i
          wrong['cs' + i + 'pat'] = err
        if not csmark:
          err = 'You must specify word/affix for Coordination Strategy ' + i
          wrong['cs' + i + 'mark'] = err
        if not csorder:
          err = 'You must specify before/after for Coordination Strategy ' + i
          wrong['cs' + i + 'order'] = err
        if not csorth:
          err = 'You must specify a spelling for Coordination Strategy ' + i
          wrong['cs' + i + 'orth'] = err

      if csmark == 'affix' and (csnp or csvp or css):
        err = 'Marking coordination with an affix is not supported on phrases (NPs, VPs, or sentences)'
        wrong['cs' + i + 'mark']


######################################################################
# validate_yesno_questions()
#   Validate the user's choices about matrix yes/no questions.

def validate_yesno_questions():
  ques = ch('ques')
  qinvverb = ch('qinvverb')
  qpartposthead = ch('qpartposthead')
  qpartform = ch('qpartform')

  if ques == 'qpart':
    if not qpartposthead:
      err = 'If you chose the question particle strategy for yes-no questions, you must specify where the question particle appears.'
      wrong['ques'] = err
    if not qpartform:
      err = 'If you chose the question particle strategy for yes-no questions, you must specify the form of the question particle.'
      wrong['qpartform'] = err

  if ques == 'inv':
    if qinvverb != 'aux' and qinvverb != 'main' and qinvverb != 'main-aux':
      err = 'There is something wrong with the verb type (main/aux) for inverted questions.  Please contact developers.'
      wrong['qinvverb'] = err


######################################################################
# validate_lexicon()
#   Validate the user's choices about the test lexicon.

def validate_lexicon():
  pass


######################################################################
# validate_test_sentences()
#   Validate the user's choices about test sentences.

def validate_test_sentences():
  pass


######################################################################
# validate_choices(path)
#   Validate the choices file found in the directory 'path'.  Return
#   the names of choice file variables that are incorrect (stored
#   in the list 'wrong'.

def validate_choices(path):
  try:
    f = open(path + '/choices', 'r')
    lines = f.readlines()
    f.close()
    for l in lines:
      l = l.strip()
      w = l.split('=')
      choices[w[0]] = w[1]
  except:
    pass

  validate_language()
  validate_word_order()
  validate_sentential_negation()
  validate_coordination()
  validate_yesno_questions()
  validate_lexicon()
  validate_test_sentences()

  return wrong
