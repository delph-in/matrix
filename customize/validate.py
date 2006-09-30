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
  
  # General word order
  if (not ch('wordorder')):
    wrong['wordorder'] = 'You must specify a choice for the basic word order.'

  # Things to do with determiners
  if (not ch('hasDets')):
    wrong['hasDets'] += 'You must specify whether your language has determiners.'
  
  if ((ch('hasDets') == 't') and (not ch('NounDetOrder'))):
    wrong['NounDetOrder'] = 'If your language has determiners, you must specify'\
                            + 'their order with respect to nouns.'

  if (ch('NounDetOrder') and (not ch('hasDets'))):
    wrong['hasDets'] += 'You specified an order of nouns and dets, but'\
                       + 'not whether your language has determiners at all.'

  if ((ch('det1') or ch('det2')) and (ch('hasDets') == 'nil')):
    wrong['hasDets'] += 'You specified lexical entries for determiners, but'\
                       + 'said your language has none.'


######################################################################
# validate_sentential_negation()
#   Validate the user's choices about sentential negation.

def validate_sentential_negation():

  # If affix is indicated, must select prefix/suffix and main/aux/either and give form.
  if (ch('neg-infl') == 'on'):
    if (not ch('neg-infl-type')):
      err = 'If sentential negation is expressed through affixation, you must specify what the affix attaches to.'
      wrong['neg-infl-type'] = err
    if (not ch('neg-aff')):
      err = 'If sentential negation is expressed through affixation, you must specify whether its a prefix or a suffix'
      wrong['neg-aff'] = err
    if (not ch('neg-aff-form')):
      err = 'If sentential negation is expressed through affixation, you must specify the form of the affix'
      wrong['neg-aff-form'] = err
    
  # If adverb is indicated, must lexical entry, what it modifies, and ind/selected modifier
  if (ch('adv_neg') == 'on'):
    if (not ch('neg-adv')):
      err = 'If sentential negaton is expressed through an adverb, you must specify whether the adverb is a selected complement or an independent modifier.'
      wrong['neg-adv'] = err
    if (ch('neg-adv') == 'ind-adv'):
      if (not ch('negmod')):
        err = 'If sentential negaton is expressed through an adverb, you must specify what type of constituent the adverb modifies.'
        wrong['negmod'] = err
      if (not ch('negprepostmod')):
        err = 'If sentential negaton is expressed through an adverb, you must specify what side of its host the adverb attaches to.'
        wrong['negprepostmod'] = err
    if (not ch('negadvform')):
      err = 'If sentential negation is expressed through an adverb, you must specify the form of the adverb.'
      wrong['negadvform'] = err

  # If both strategies are checked, then they must say how they combine:
  if ((ch('neg-infl') == 'on') and (ch('adv_neg') == 'on')):
    if (not ch('multineg')):
      err = 'If you have selected both affix and adverb realizations of sentential negation, you must specify how they interact.'
      wrong['multineg'] = err

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
  noun1 = ch('noun1')
  noun1pred = ch('noun1pred')
  noun1spr = ch('noun1spr')
  noun2 = ch('noun2')
  noun2pred = ch('noun2pred')
  noun2spr = ch('noun2spr')

  iverb = ch('iverb')
  ivpred = ch('ivpred')
  iverbSubj = ch('iverbSubj')
  iverbnf = ch('iverbnf')

  tverb = ch('tverb')
  tvpred = ch('tvpred')
  tverbSubj = ch('tverbSubj')
  tverbObj = ch('tverbObj')
  tverbnf = ch('tverbnf')

  subjAdp = ch('subjAdp')
  subjAdpForm = ch('subjAdpForm')
  objAdp = ch('objAdp')
  objAdpForm = ch('objAdpForm')

  det1 = ch('det1')
  det1pred = ch('det1pred')
  det2 = ch('det2')
  det2pred = ch('det2pred')

  auxverb = ch('auxverb')
  auxsem = ch('auxsem')
  auxcomp = ch('auxcomp')
  auxorder = ch('auxorder')

  # ERB 2006-09-29: What are these two picking up?  I don't see the match in matrixdef.

  neg = ch('neg')
  negadv = ch('negadv')

  # ERB 2006-09-29: Doing this over in validate_sentential_negation()
  # If negation involves an adverb, did they say which kind?
  #  if neg and not negadv:
  #    err = 'If your language expresses sentential negation with a negative adverb, you must specify whether it is independent or selected.'
  #    wrong['negadv'] = err



  # Did they specify enough lexical entries?
  if not (noun1 and iverb and tverb):
    err = 'You must create an intransitive verb entry, a transitive verb entry, and at least one noun entry.'
    wrong['noun1'] = err
    wrong['iverb'] = err
    wrong['tverb'] = err
    
  # Did they give pred names?
  # sfd ToDo: If/when we start showing exactly which form fields were in
  # error, this should be broken out into separate error messages for each
  # choice
  if (noun1 and not noun1pred) or \
     (noun2 and not noun2pred) or \
     (det1 and not det1pred) or \
     (det2 and not det2pred) or \
     not ivpred or not tvpred:
    err = 'You must specify a predicate value for each noun, (main) verb, and determiner you specify.'
    wrong['noun1pred'] = err
    wrong['noun2pred'] = err
    wrong['det1pred'] = err
    wrong['det2pred'] = err
    wrong['ivpred'] = err
    wrong['tvpred'] = err

  # Did they answer all of the questions about lexical entries?
  # sfd ToDo: As above, separate error messages
  if (noun1 and not noun1spr) or \
     (noun2 and not noun2spr) or \
     (iverb and not iverbSubj) or \
     (tverb and not (tverbSubj and tverbObj)) or \
     (subjAdp and not subjAdpForm) or \
     (objAdp and not objAdpForm) or \
     (auxverb and not (auxsem and auxcomp and auxorder)):
    err = 'You must answer all questions for each lexical entry you specify.'
    wrong['noun1spr'] = err
    wrong['noun2spr'] = err
    wrong['iverbSubj'] = err
    wrong['tverbSubj'] = err
    wrong['tverbObj'] = err
    wrong['subjAdpForm'] = err
    wrong['objAdpForm'] = err
    wrong['auxsem'] = err
    wrong['auxcomp'] = err
    wrong['auxorder'] = err

  # Did they give us the same form for both finite and nonfinite verbs?
  if iverb == iverbnf or tverb == tverbnf:
    err = 'If you provide a form for a verb when it cooccurs with an auxiliary, it must be different from the other (finite) form.'
    wrong['iverbnf'] = err
    wrong['tverbnf'] = err

  # If they're specifying an auxiliary, and they say it takes a VP or V
  # complement, did they tell us what type of subject?
  if auxverb and auxcomp == 'V' and not auxsubj:
    err = 'If your auxiliary takes a V or VP complement, you must specify whether its subject is an NP or a PP.'
    wrong['auxsubj'] = err

  # ERB 2006-09-29
  # If they said that any argument of a verb as a PP, did they define an
  # appropriate adposition?

  if (((tverbSubj == 'pp') or (iverbSubj == 'pp')) and not subjAdp):
    err = 'You said that one or more of your verbs takes a PP subject, but you did not define a subject-marking adposition.'
    wrong['subjAdp'] += err
    wrong['subjAdpForm'] += err

  if ((tverbObj == 'pp') and not objAdp):
    err = 'You said that your transitive verb takes a PP object, but you did not define an object-marking adposition.'
    wrong['objAdp'] += err
    wrong['subjAdpForm'] += err

  # ERB 2006-09-29
  # Since we aren't supporting both pre- and post- positions in the same grammar

  if ((subjAdp == 'pre' and objAdp == 'post') or (subjAdp == 'post' and objAdp == 'pre')):
    err = 'We assume that argument-marking adpositions in any given language are uniformly prepositions or uniformly postpositions.  If your language is a counterexample to this, please contact the developers.'
    wrong['subjAdp'] += err
    wrong['objAdp'] += err

  # ERB 2006-09-29
  # If they said that either negation or questions required auxiliaries, did they specify
  # an auxiliary?

  if (not ch('auxverbform')):
    if (ch('neginfltype') == 'aux'):
      err = 'You specified that sentential negation is expressed through inflection of auxiliary verbs, but you did not specify an auxiliary in the lexicon.'
      wrong['auxverbform'] += err
    if (ch('negseladv') == 'aux'):
      err = 'You specified that sentential negation is expressed through an adverb selected by auxiliary verbs, but you did not specify an auxiliary in the lexicon.'
      wrong['auxverbform'] += err
    if (ch('qinvverb') == 'aux'):
      err = 'You specified that matrix yes-no questions are expressed through subject-auxiliary inversion, but you did not specify an auxiliary in the lexicon.'
      wrong['auxverbform'] += err

  # ERB 2006-0929
  # If they said that either noun takes an obligatory determiner, did they say their language has
  # determiners? ToDo: Consider whether this error should point to all things involved, or just
  # the hasDets field.

  if (((noun1spr == 'obl') or (noun2spr == 'obl')) and ch('hasDets') == nil):
    err = 'You specified that one or more of your nouns obligatorily takes a determiner, but also that your language does not have determiners.'
    wrong['hasDets'] = err
    if (noun1spr == 'obl'):
      wrong['noun1spr'] = err
    if (noun1spr == 'obl'):
      wrong['noun2spr'] = err
    

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
