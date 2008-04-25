######################################################################
# imports

from choices import ChoicesFile

######################################################################
# globals

ch = {}
wrong = {}


######################################################################
# add_err(key,string)
#   Add an error in wrong for key, concatenating if another error is
#   already there. (ERB 2006-09-29)

def add_err(key,err):
  if wrong.has_key(key):
    wrong[key] += ' ' + err
  else:
    wrong[key] = err
  
######################################################################
# validate_language()
#   Validate the user's choice about language

def validate_language():
  if not ch.get('language'):
    add_err('language','You must specify the name of your language')



######################################################################
# validate_one_case(l, pre)
#   A helper function to validate the user's choices about one case.
#   l is the case label (e.g. 'nominative'), while pre is the first
#   few characters of the associated choices names (e.g. 'nom')

def validate_one_case(l, pre):
  if not ch.get(pre + '-case-label'):
    add_err(pre + '-case-label', 'You must specify a ' +l+ ' case label.')
  if not ch.get(pre + '-case-pat'):
    add_err(pre + '-case-pat', 'You must specify a ' +l+ ' case pattern.')
  if ch.get(pre + '-case-pat') == 'unmarked':
    if ch.get(pre + '-case-order'):
      add_err(pre + '-case-order',
              'If ' +l+ ' case is unmarked, you cannot specify an order.')
    if ch.get(pre + '-case-orth'):
      add_err(pre + '-case-orth',
              'If ' +l+ ' case is unmarked, you cannot specify a spelling.')
  else:
    if not ch.get(pre + '-case-order'):
      add_err(pre + '-case-order', 'You must specify a ' +l+ ' case order.')
    if not ch.get(pre + '-case-orth'):
      add_err(pre + '-case-orth', 'You must specify a ' +l+ ' case spelling.')


######################################################################
# validate_features()
#   Validate the user's choices about features

def validate_features():
  cm = ch.get('case-marking')

  if not cm:
    add_err('case-marking', 'You must specify if/how case is marked.')

  if cm == 'nom-acc':
    validate_one_case('nominative', 'nom')
    validate_one_case('accusative', 'acc')
  elif cm == 'erg-abs':
    validate_one_case('ergative', 'erg')
    validate_one_case('absolutive', 'abs')
  elif cm == 'tripartite':
    validate_one_case('S', 's')
    validate_one_case('A', 'a')
    validate_one_case('O', 'o')


######################################################################
# validate_word_order()
#   Validate the user's choices about basic word order.

# There should be some value for word order
# If has-dets is true, there should be some value for noun-det-order
# If aux-verb is defined, then aux-order needs to be too
# I'm currently adding AUX as a feature in specialize_word_order()
# but wonder if instead (in addition?) we should be doing validation
# so that we don't find ourselves worrying about auxiliaries if we
# don't have any in the lexicon.

def validate_word_order():
  
  # General word order
  if (not ch.get('word-order')):
    add_err('word-order','You must specify a choice for the basic word order.')

  # Things to do with determiners
  if (not ch.get('has-dets')):
    add_err('has-dets','You must specify whether your language has determiners.')
  
  if ((ch.get('has-dets') == 'yes') and (not ch.get('noun-det-order'))):
    add_err('noun-det-order','If your language has determiners, you must specify their order with respect to nouns.')

  if (ch.get('noun-det-order') and (not ch.get('has-dets'))):
    add_err('has-dets','You specified an order of nouns and dets, but not whether your language has determiners at all.')

  if ch.get('det1_orth') and ch.get('has-dets') == 'no':
    add_err('has-dets','You specified lexical entries for determiners, but said your language has none.')


######################################################################
# validate_sentential_negation()
#   Validate the user's choices about sentential negation.

def validate_sentential_negation():

  # If affix is indicated, must select prefix/suffix and main/aux/either and give form.
  if (ch.get('infl-neg') == 'on'):
    if (not ch.get('neg-infl-type')):
      err = 'If sentential negation is expressed through affixation, you must specify what the affix attaches to.'
      add_err('neg-infl-type', err)
    if (not ch.get('neg-aff')):
      err = 'If sentential negation is expressed through affixation, you must specify whether its a prefix or a suffix'
      add_err('neg-aff', err)
    if (not ch.get('neg-aff-orth')):
      err = 'If sentential negation is expressed through affixation, you must specify the form of the affix'
      add_err('neg-aff-orth', err)
    
  # If adverb is indicated, must lexical entry, what it modifies, and ind/selected modifier
  if (ch.get('adv-neg') == 'on'):
    if (not ch.get('neg-adv')):
      err = 'If sentential negation is expressed through an adverb, you must specify whether the adverb is a selected complement or an independent modifier.'
      add_err('neg-adv', err)
    if (ch.get('neg-adv') == 'ind-adv'):
      if (not ch.get('neg-mod')):
        err = 'If sentential negaton is expressed through an adverb, you must specify what type of constituent the adverb modifies.'
        add_err('neg-mod', err)
      if (not ch.get('neg-order')):
        err = 'If sentential negaton is expressed through an adverb, you must specify what side of its host the adverb attaches to.'
        add_err('neg-order', err)
    if (not ch.get('neg-adv-orth')):
      err = 'If sentential negation is expressed through an adverb, you must specify the form of the adverb.'
      add_err('neg-adv-orth', err)

  # If both strategies are checked, then they must say how they combine:
  if ((ch.get('infl-neg') == 'on') and (ch.get('adv-neg') == 'on')):
    if (not ch.get('multi-neg')):
      err = 'If you have selected both affix and adverb realizations of sentential negation, you must specify how they interact.'
      add_err('multi-neg', err)

######################################################################
# validate_coordination()
#   Validate the user's choices about coordination.

def validate_coordination():
  for n in (1, 2):
    i = str(n)
    if ch.is_set('cs' + i):
      cs_n =     ch.get('cs' + i + '_n')
      cs_np =    ch.get('cs' + i + '_np')
      cs_vp =    ch.get('cs' + i + '_vp')
      cs_s =     ch.get('cs' + i + '_s')
      cs_pat =   ch.get('cs' + i + '_pat')
      cs_mark =  ch.get('cs' + i + '_mark')
      cs_order = ch.get('cs' + i + '_order')
      cs_orth =  ch.get('cs' + i + '_orth')

      if not (cs_n or cs_np or cs_vp or cs_s):
        err = 'You must specify a phrase type for Coordination Strategy ' + i
        add_err('cs' + i + '_n', err)
        add_err('cs' + i + '_np', err)
        add_err('cs' + i + '_vp', err)
        add_err('cs' + i + '_s', err)

      if cs_pat == 'a':
        if cs_mark:
          err = 'You must not specify word/affix for asyndetic Coordination Strategy ' + i
          add_err('cs' + i + '_mark', err)
        if cs_order:
          err = 'You must not specify before/after for asyndetic Coordination Strategy ' + i
          add_err('cs' + i + '_order', err)
        if cs_orth:
          err = 'You must not specify a spelling for asyndetic Coordination Strategy ' + i
          add_err('cs' + i + '_orth', err)
      else:
        if not cs_pat:
          err = 'You must specify a pattern for Coordination Strategy ' + i
          add_err('cs' + i + '_pat', err)
        if not cs_mark:
          err = 'You must specify word/affix for Coordination Strategy ' + i
          add_err('cs' + i + '_mark', err)
        if not cs_order:
          err = 'You must specify before/after for Coordination Strategy ' + i
          add_err('cs' + i + '_order', err)
        if not cs_orth:
          err = 'You must specify a spelling for Coordination Strategy ' + i
          add_err('cs' + i + '_orth', err)

      if cs_mark == 'affix' and (cs_np or cs_vp or cs_s):
        err = 'Marking coordination with an affix is not supported on phrases (NPs, VPs, or sentences)'
        add_err('cs' + i + '_mark', err)


######################################################################
# validate_yesno_questions()
#   Validate the user's choices about matrix yes/no questions.

def validate_yesno_questions():
  qinvverb = ch.get('q-inv-verb')
  qpartorder = ch.get('q-part-order')
  qpartorth = ch.get('q-part-orth')

  if ch.get('q-part'):
    if not qpartorder:
      err = 'If you chose the question particle strategy for yes-no questions, you must specify where the question particle appears.'
      add_err('ques', err)
    if not qpartorth:
      err = 'If you chose the question particle strategy for yes-no questions, you must specify the form of the question particle.'
      add_err('q-part-orth', err)

  if ch.get('q-inv'):
    #    if qinvverb != 'aux' and qinvverb != 'main' and qinvverb != 'main-aux':
    #      err = 'There is something wrong with the verb type (main/aux) for inverted questions.  Please contact developers.'
    #      add_err('q-inv-verb', err)
    if not qinvverb:
      err = 'If you chose subject-verb inversion strategy for yes-no questions, you must specify which types of verbs invert.'
      add_err('q-inv-verb', err)
    if ch.get('word-order') == 'v-final' or \
       ch.get('word-order') == 'v-initial' or \
       ch.get('word-order') == 'free':
      err = 'Subject-verb inversion strategy for yes-no questions is not supported for V-final, V-initial, or free word order languages.  If you believe you have a counterexample to this, please contact us.'
      add_err('q-inv', err)

  if ch.get('q-infl'):
    if (not ch.get('q-infl-type')):
      err = 'If matrix yes-no questions are expressed through affixation, you must specify what the affix attaches to.'
      add_err('q-infl-type', err)
    if (not ch.get('ques-aff')):
      err = "If matrix yes-no questions are expressed through affixation, you must specify whether it's a prefix or a suffix"
      add_err('ques-aff', err)
    if (not ch.get('ques-aff-orth')):
      err = 'If matrix yes-no questions are expressed through affixation, you must specify the form of the affix'
      add_err('ques-aff-orth', err)
                                              


######################################################################
# validate_lexicon()
#   Validate the user's choices about the test lexicon.

def validate_lexicon():
  noun1_orth = ch.get('noun1_orth')
  noun1_pred = ch.get('noun1_pred')

  auxverb = ch.get('aux-verb')
  auxsem = ch.get('aux-sem')
  auxcomp = ch.get('aux-comp')
  auxorder = ch.get('aux-order')
  auxsubj = ch.get('aux-subj')

  # First, handle the non-iterated lexical entries

  # ERB 2006-09-29: What are these two picking up?  I don't see the
  # match in matrixdef.

  # neg = ch.get('neg')
  # negadv = ch.get('negadv')

  # ERB 2006-09-29: Doing this over in validate_sentential_negation()
  # If negation involves an adverb, did they say which kind?
  #  if neg and not negadv:
  #    err = 'If your language expresses sentential negation with a negative adverb, you must specify whether it is independent or selected.'
  #    add_err('negadv', err)

  # Did they specify enough lexical entries?
  if not noun1_orth:
    err = 'You must create at least one noun class.'
    add_err('noun1_orth', err)

  # Did they answer all of the questions about lexical entries?
  if auxverb and not (auxsem and auxcomp and auxorder):
    err = 'You must answer all questions for each lexical entry you specify.'
    add_err('aux-sem', err)
    add_err('aux-comp', err)
    add_err('aux-order', err)

  # If they're specifying an auxiliary, and they say it takes a VP or V
  # complement, did they tell us what type of subject?
  if auxverb and auxcomp == 'v' and not auxsubj:
    err = 'If your auxiliary takes a V or VP complement, you must specify whether its subject is an NP or a PP.'
    add_err('aux-subj', err)

  # ERB 2006-09-29
  # If they said that either negation or questions required
  # auxiliaries, did they specify an auxiliary?
  if (not ch.get('aux-verb')):
    if (ch.get('neg-infl-type') == 'aux'):
      err = 'You specified that sentential negation is expressed through inflection of auxiliary verbs, but you did not specify an auxiliary in the lexicon.'
      add_err('aux-verb', err)
    if (ch.get('negseladv') == 'aux'):
      err = 'You specified that sentential negation is expressed through an adverb selected by auxiliary verbs, but you did not specify an auxiliary in the lexicon.'
      add_err('aux-verb', err)
    if (ch.get('qinverb') == 'aux'):
      err = 'You specified that matrix yes-no questions are expressed through subject-auxiliary inversion, but you did not specify an auxiliary in the lexicon.'
      add_err('aux-verb', err)
    if (ch.get('q-infl-type') == 'aux'):
      err = 'You specified that matrix yes-no questions are expressed through inflection of auxiliary verbs, but you did not specify an auxiliary in the lexicon.'
      add_err('aux-verb', err)

  # Now, do the iterated lexical entries
  
  # Nouns
  ch.iter_begin('noun')
  while ch.iter_valid():
    orth = ch.get('orth')
    pred = ch.get('pred')
    det = ch.get('det')

    # Did they give a spelling?
    if not orth:
      err = 'You must specify a spelling for each noun you define.'
      add_err(ch.iter_prefix() + 'orth', err)

    # Did they give a predicate?
    if not pred:
      err = 'You must specify a predicate for each noun you define.'
      add_err(ch.iter_prefix() + 'pred', err)

    # Did they answer the question about determiners?
    if not det:
      err = 'You must specify whether each noun you define takes a determiner.'
      add_err(ch.iter_prefix() + 'det', err)

    # If they said the noun takes an obligatory determiner, did they
    # say their language has determiners?
    if det == 'obl' and ch.get_full('has-dets') == 'no':
      err = 'You defined a noun that obligatorily takes a determiner, but also said your language does not have determiners.'
      add_err('has-dets', err)
      add_err(ch.iter_prefix() + 'det', err)
    
    ch.iter_next()
  ch.iter_end()

  # Verbs
  seenTrans = False
  seenIntrans = False
  ch.iter_begin('verb')
  while ch.iter_valid():
    orth = ch.get('orth')
    pred = ch.get('pred')
    val = ch.get('valence')
    nf = ch.get('non-finite')

    if val == 'trans':
      seenTrans = True
    if val == 'intrans':
      seenIntrans = True

    if not orth:
      err = 'You must specify a spelling for each verb you define.'
      add_err(ch.iter_prefix() + 'orth', err)

    if not pred:
      err = 'You must specify a predicate for each verb you define.'
      add_err(ch.iter_prefix() + 'pred', err)

    # Did they give us the same form for both finite and nonfinite verbs?
    if orth == nf:
      err = 'If you provide a form for a verb when it cooccurs with an auxiliary, it must be different from the other (finite) form.'
      add_err(ch.iter_prefix() + 'non-finite', err)

    ch.iter_next()
  ch.iter_end()

  if not (seenTrans and seenIntrans):
    err = 'You must create intransitive and transitive verb classes.'
    add_err('verb1_valence', err)
    add_err('verb2_valence', err)

  # Determiners
  ch.iter_begin('det')
  while ch.iter_valid():
    if not ch.get('orth'):
      err = 'You must specify a spelling for each determiner you define.'
      add_err(ch.iter_prefix() + 'orth', err)

    if not ch.get('pred'):
      err = 'You must specify a predicate for each determiner you define.'
      add_err(ch.iter_prefix() + 'pred', err)

    ch.iter_next()
  ch.iter_end()


######################################################################
# validate_test_sentences()
#   Validate the user's choices about test sentences.

def validate_test_sentences():
  pass

######################################################################
# validate_extra_constraints()
#   Some extra constraints we want to put on the random grammars
#   for the regression/other testing

def validate_extra_constraints():

  if ch.get('aux-sem') == 'pred':
    err = 'Only semantically empty auxiliaries in test grammars.'
    add_err('aux-sem', err)
  if ch.get('has-dets') == 'yes' and not ch.get('det1_orth'):
    err = 'To get uniform semantics, we always want det1 specified.'
    add_err('det1_orth', err)
  if not ((ch.get('cs1') == 'on' and ch.get('cs1_n') == 'on') or \
          (ch.get('cs2') == 'on' and ch.get('cs2n') == 'on')):
    err = 'The test grammars must have some way to coordinate nouns.'
    add_err('cs1_n', err)
#  if ch.get('multi-neg') != '':
#    if ch.get('infl-neg') != 'on' or ch.get('adv-neg') != 'on':
#      err = 'Giving a value for multi-neg means you have selected both neg. strategies.'
#      add_err('multi-neg', err)
#   if ch.get('infl-neg') == '':
#     if ch.get('neg-infl-type') != '' or \
#        ch.get('neg-aff') != '' or \
#        ch.get('neg-aff-orth') != '' :
#       err = 'You have not selected inflectional negation.'
#       add_err('infl-neg', err)
#   if ch.get('adv-neg') == '':
#     if ch.get('neg-adv') != '' or \
#        ch.get('neg-mod') != '' or \
#        ch.get('neg-order') != '' or \
#        ch.get('neg-adv') != '' or \
#        ch.get('neg-sel-adv') != '' :
#       err = 'You have not selected adverbial negation.'
#       add_err('adv-neg', err)

      

######################################################################
# validate_choices(choices_file)
#   Validate the choices file found in choices_file.  Return
#   the names of choice file variables that are incorrect (stored
#   in the list 'wrong'.

def validate_choices(choices_file, extra = False):
  global wrong
  wrong = {}
  global ch
  ch = ChoicesFile(choices_file)

  validate_language()
  validate_features()
  validate_word_order()
  validate_sentential_negation()
  validate_coordination()
  validate_yesno_questions()
  validate_lexicon()
  validate_test_sentences()

  if extra:
    validate_extra_constraints()

  return wrong


