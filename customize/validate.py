### $Id: validate.py,v 1.44 2008-09-30 23:50:02 lpoulson Exp $

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
# validate_general()
#   Validate the user's choices about general information

def validate_general():
  lang = ch.get('language')
  
  if not lang:
    add_err('language','You must specify the name of your language')
  else:
    bad_lang = False
    if lang[0] in '.~':
      bad_lang = True
    for c in ch.get('language'):
      if ord(c) < 32 or c in '?*:<>|/\\"^':
        bad_lang = True
    if bad_lang:
      add_err('language','The language name contains an illegal character')

  if not ch.get('archive'):
    add_err('archive','You must answer whether you will allow your answers to be retained.')



######################################################################
# validate_one_case(pre)
#   A helper function to validate the user's choices about one case.
#   pre is the first few characters of the associated choices names
#  (e.g. 'nom-acc-nom')

def validate_one_case(pre):
  if not ch.get(pre + '-case-name'):
    add_err(pre + '-case-name', 'You must specify a name for every case.')


######################################################################
# validate_case()
#   Validate the user's choices about case

def validate_case():
  cm = ch.get('case-marking')

  if not cm:
    add_err('case-marking', 'You must specify if/how case is marked.')

  if cm in ['nom-acc', 'split-n', 'split-v']:
    validate_one_case(cm + '-nom')
    validate_one_case(cm + '-acc')
  if cm in ['erg-abs', 'split-n', 'split-v']:
    validate_one_case(cm + '-erg')
    validate_one_case(cm + '-abs')
  if cm in ['tripartite', 'split-s', 'fluid-s', 'focus']:
    validate_one_case(cm + '-a')
    validate_one_case(cm + '-o')
  if cm in ['tripartite']:
    validate_one_case(cm + '-s')
  if cm in ['focus']:
    validate_one_case(cm + '-focus')

  if ch.get('scale1_feat1_name') and not ch.get('scale-equal'):
    add_err('scale-equal', 'If you define a direct-inverse scale, you must say what direction the verb is when the agent and patient have equal rank.')


######################################################################
# validate_person()
#   Validate the user's choices about person

def validate_person():
  person = ch.get('person')
  fp = ch.get('first-person')

  if not person:
    add_err('person',
            'You must specify how many persons your language distinguishes.')
  else:
    if person in ['none', '2-non-2', '3-non-3']:
      if fp not in ['', 'none']:
        add_err('first-person',
                'If your language does not have the first person, it ' + \
                'cannot distinguish sub-values of the first person.')
    if person in ['1-2-3', '1-non-1']:
      if not fp:
        add_err('first-person',
                'If your language has the first person, you must specify ' + \
                'whether it makes finer distinctions within that category.')


######################################################################
# validate_number()
#   Validate the user's choices about number

def validate_number():
  pass


######################################################################
# validate_gender()
#   Validate the user's choices about gender

def validate_gender():
  ch.iter_begin('gender')
  while ch.iter_valid():
    if not ch.get('name'):
      add_err(ch.iter_prefix() + 'name',
              'You must specify a name for each gender you define.')
    if not ch.get('supertype1_name'):
      add_err(ch.iter_prefix() + 'supertype1_name',
              'You must specify a supertype for each gender you define.')
      
    ch.iter_next()
  ch.iter_end()


######################################################################
# validate_other_features()
#   Validate the user's choices about other features

def validate_other_features():
  ch.iter_begin('feature')
  while ch.iter_valid():
    if not ch.get('name'):
      add_err(ch.iter_prefix() + 'name',
              'You must specify a name for each feature you define.')

    if not ch.get('type'):
      add_err(ch.iter_prefix() + 'type',
              'You must specify a type for each feature you define.')

    ch.iter_begin('value')
    while ch.iter_valid():
      if not ch.get('name'):
        add_err(ch.iter_prefix() + 'name',
                'You must specify a name for each value you define.')
      if not ch.get('supertype1_name'):
        add_err(ch.iter_prefix() + 'supertype1_name',
                'You must specify a supertype for each value you define.')

      ch.iter_next()
    ch.iter_end()

    ch.iter_next()
  ch.iter_end()


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

  if ch.get('det1_stem1_orth') and ch.get('has-dets') == 'no':
    add_err('has-dets','You specified lexical entries for determiners, but said your language has none.')

  #Things to do with auxiliaries
  if (not ch.get('has-aux')):
    add_err('has-aux','You must specify whether your language has auxiliary verbs.')

  if ((ch.get('has-aux') == 'yes') and (not ch.get('aux-comp-order'))):
    add_err('aux-comp-order','If your language has auxiliaries, you must specify their order with respect to their complements.')

  if (ch.get('aux-comp-order') and (not ch.get('has-aux'))):
    add_err('has-aux','You specified an order for auxiliaries and their complements, but not whether your language has auxiliaries at all.')

#  if (ch.get('aux1_orth') and (ch.get('has-aux') == 'no')):
#    add_err('has-aux','You specified a lexical entry for an auxiliary, but said your language has none.')

#  if ((not ch.get('aux1_orth')) and (ch.get('has-aux') == 'yes')):
#    add_err('has-aux', 'You must add at least one auxiliary on the lexicon page.')
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
    # If aux is selected then has-aux = 'yes' must be chosen in word order section
    if ((ch.get('neg-infl-type') == 'aux') or (ch.get('neg-infl-type') == 'aux-main')): 
      if (ch.get('has-aux') == 'no'):
        err = 'You have indicated that verbal inflection applies to auxiliaries but that your language has no auxiliaires'
        add_err('neg-infl-type', err)

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
   # If aux is selected then has-aux = 'yes' must be chosen in word order section
    if ((ch.get('neg-sel-adv') == 'aux') or (ch.get('neg-sel-adv') == 'main-aux')):
      if (ch.get('has-aux') == 'no'):
        err = 'You have indicated that negation is a selected complement of auxiliaries but that your language has no auxiliaires'
        add_err('neg-sel-adv', err)  

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
                                              

# validate_tanda()
#  Validate the user's choices about tense, aspect and form features

def validate_tanda():
  """
  Validate the user's choices about tense, aspect and form features
  """
  
  ## validate tense
  chosen = ""
  ten = ('past', 'present', 'future', 'nonpast', 'nonfuture') 
  for t in ten:
    if ch.is_set(t):
      chosen = 'yes'
      break
  if ch.get('tense-definition') == "choose" and (not (chosen == 'yes')):
    err = 'You have chosen to select among hierarchy elements. You need to select at least one tense element.'
    for t in ten:
      add_err(t, err)

  if ch.get('tense-definition') == "build" and (not (ch.get('tense1_name'))):
    err = "You have chosen to build your own tense hierarchy. You need to enter at least one tense subtype."
    add_err('tense1_name', err)

    ch.iter_begin('tense')
    while ch.iter_valid():
      if not ch.get('name'):
        add_err(ch.iter_prefix() + 'name',
              'You must specify a name for each tense subtype you define.')
      if not ch.get('supertype1_name'):
        add_err(ch.iter_prefix() + 'supertype1_name',
              'You must specify a supertype for each tense subtype you define.')

      ch.iter_next()
    ch.iter_end()
  
  ## validate aspect
  ch.iter_begin('aspect')
  while ch.iter_valid():
    if not ch.get('name'):
      add_err(ch.iter_prefix() + 'name',
            'You must specify a name for each aspect subtype you define.')
    if not ch.get('supertype1_name'):
      add_err(ch.iter_prefix() + 'supertype1_name',
            'You must specify at least one supertype for each aspect subtype you define.')

    ch.iter_next()
  ch.iter_end()

  ## validate form
  if ch.get('has-aux') == 'yes' and ch.get('noaux-fin-nf') == 'on':
    err = 'You have indicated on the word order page that your language has auxiliaries.'
    add_err('noaux-fin-nf', err)
  
#  if ch.get('has-aux') == 'no' and (ch.get('nf-subform1_name') or ch.get('fin-subform1_name')):
  if ch.get('has-aux') == 'no' and not (ch.get('noaux-fin-nf') == 'on'):
    if ch.is_set('nf-subform1_name'):

      err = 'You have indicated that your language has no auxiliaries but you have entered subforms of finite or non-finite.'
      add_err('noaux-fin-nf', err)

######################################################################
# validate_lexicon()
#   Validate the user's choices about the test lexicon.

def validate_lexicon():

  auxverb = ch.get('aux-verb')
  auxsem = ch.get('aux-sem')
  auxcomp = ch.get('aux-comp')
  auxorder = ch.get('aux-order')
  auxsubj = ch.get('aux-subj')

  # First, handle the non-iterated lexical entries

  # Did they specify enough lexical entries?
  if not ch.get('noun1_stem1_orth'):
    err = 'You must create at least one noun class.'
    add_err('noun1_stem1_orth', err)

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
  
  ch.iter_begin('noun')
  while ch.iter_valid():
    det = ch.get('det')

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

    ch.iter_begin('stem')
    while ch.iter_valid():
      orth = ch.get('orth')
      pred = ch.get('pred')

      # Did they give a spelling?
      if not orth:
        err = 'You must specify a spelling for each noun you define.'
        add_err(ch.iter_prefix() + 'orth', err)

      # Did they give a predicate?
      if not pred:
        err = 'You must specify a predicate for each noun you define.'
        add_err(ch.iter_prefix() + 'pred', err)

      ch.iter_next()
    ch.iter_end()

    ch.iter_next()
  ch.iter_end()

  # Verbs
  seenTrans = False
  seenIntrans = False
  ch.iter_begin('verb')
  while ch.iter_valid():
    val = ch.get('valence')

    if not val:
      err = 'You must specify the argument structure of each verb you define.'
      add_err(ch.iter_prefix() + 'valence', err)
    elif val[0:5] == 'trans' or val.find('-') != -1:
      seenTrans = True
    else:
      seenIntrans = True

    ch.iter_begin('stem')
    while ch.iter_valid():
      orth = ch.get('orth')
      pred = ch.get('pred')

      if not orth:
        err = 'You must specify a spelling for each verb you define.'
        add_err(ch.iter_prefix() + 'orth', err)

      if not pred:
        err = 'You must specify a predicate for each verb you define.'
        add_err(ch.iter_prefix() + 'pred', err)

      ch.iter_next()
    ch.iter_end()

    ch.iter_next()
  ch.iter_end()

  if not (seenTrans and seenIntrans):
    err = 'You must create intransitive and transitive verb classes.'
    add_err('verb1_valence', err)
    add_err('verb2_valence', err)


  # Auxiliaries
  aux1_name = ch.get('aux1_name')
  if ch.get('has-aux') == 'no':
    if aux1_name:
      err = 'You have indicated on the word order page that your language has no auxiliaries.'
      add_err('aux1_name', err)

  if ch.get('has-aux') == 'yes':
    if not aux1_name:
      err = 'You have indicated on the word order page that your language has auxiliaries. You must define at least one auxiliary type.'
      add_err('aux1_name', err)

  ch.iter_begin('aux')
  while ch.iter_valid():
    orth = ch.get('orth')
    sem = ch.get('sem')
    pred = ch.get('pred')
    comp = ch.get('comp')
    compform = ch.get('compform')
    subj = ch.get('subj')
    prefix = ch.iter_prefix()
      
    if not orth:
      err = 'You must specify a spelling for each auxiliary you define.'
      add_err(prefix + 'orth', err)

    if not sem:
      err = 'You must specify whether the auxiliary contributes a predicate.'
      add_err(prefix + 'sem', err)

    if ((sem == 'add-pred') and (not pred)):
      err = 'You must provide a predicate.'
      add_err(prefix + 'pred', err)

    if not comp:
      err = 'You must specify the complement type.'
      add_err(prefix + 'comp', err)

    if not compform:
      err = 'You must specify the form of the verb in the complement.'
      add_err(prefix + 'compform', err)

    if ((comp == 'vp') or (comp == 'v')):
      if not subj:
        err = 'You must specify the subject type.'
        add_err(prefix + 'subj', err)
    
    ch.iter_begin('compfeature')
    while ch.iter_valid():
      if ch.get('name') and not ch.get('compvalue'):
        err = 'You must specify a value for this feature.'
        add_err(ch.iter_prefix() + 'compvalue', err)
      ch.iter_next()
    ch.iter_end()

    ch.iter_next()
  ch.iter_end()

  # Determiners
  ch.iter_begin('det')
  while ch.iter_valid():
    ch.iter_begin('stem')
    while ch.iter_valid():
      if not ch.get('orth'):
        err = 'You must specify a spelling for each determiner you define.'
        add_err(ch.iter_prefix() + 'orth', err)

      if not ch.get('pred'):
        err = 'You must specify a predicate for each determiner you define.'
        add_err(ch.iter_prefix() + 'pred', err)

      ch.iter_next()
    ch.iter_end()

    ch.iter_next()
  ch.iter_end()

  # Inflectional Slots
  for slotprefix in ('noun', 'verb', 'det'):
    ch.iter_begin(slotprefix + '-slot')
    while ch.iter_valid():
      if not ch.get('order'):
        err = 'You must specify an order for every slot you define.'
        add_err(ch.iter_prefix() + 'order', err)

      if not ch.get('input1_type'):
        err = 'You must specify at least one input for every slot.'
        add_err(ch.iter_prefix() + 'input1_type', err)

      ch.iter_begin('morph')
      while ch.iter_valid():
        ch.iter_begin('feat')
        while ch.iter_valid():
          if not ch.get('name'):
            err = 'You must choose which feature you are specifying.'
            add_err(ch.iter_prefix() + 'name', err)
          if not ch.get('value'):
            err = 'You must choose a value for each feature you specify.'
            add_err(ch.iter_prefix() + 'value', err)

          if slotprefix == 'verb' and not ch.get('head'):
            err = 'You must choose where the feature is specified.'
            add_err(ch.iter_prefix() + 'head', err)
          
          ch.iter_next()
        ch.iter_end()

        ch.iter_next()
      ch.iter_end()

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
  if ch.get('has-dets') == 'yes' and not ch.get('det1_stem1_orth'):
    err = 'To get uniform semantics, we always want det1 specified.'
    add_err('det1_stem1_orth', err)
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

  validate_general()
  validate_case()
  validate_person()
  validate_number()
  validate_gender()
  validate_other_features()
  validate_word_order()
  validate_tanda()
  validate_sentential_negation()
  validate_coordination()
  validate_yesno_questions()
  validate_lexicon()
  validate_test_sentences()

  if extra:
    validate_extra_constraints()

  return wrong


