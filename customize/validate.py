### $Id: validate.py,v 1.44 2008-09-30 23:50:02 lpoulson Exp $

######################################################################
# imports

from choices import ChoicesFile


######################################################################
# add_err(errors, key, message)
#   Insert into the dict errors the key with the value message.  If
#   the key already exists, concatenate the message with the existing
#   value.

def add_err(err, key, message):
  if err.has_key(key):
    err[key] += ' ' + message
  else:
    err[key] = message
  
######################################################################
# validate_general(ch, err)
#   Validate the user's choices about general information

def validate_general(ch, err):
  lang = ch.get('language')
  
  if not lang:
    add_err(err, 'language', 'You must specify the name of your language')
  else:
    bad_lang = False
    if lang[0] in '.~':
      bad_lang = True
    for c in lang:
      if ord(c) < 32 or c in '?*:<>|/\\"^':
        bad_lang = True
    if bad_lang:
      add_err(err,
              'language',
              'The language name contains an illegal character')

  if not ch.get('archive'):
    add_err(err,
            'archive',
            'You must answer whether you will allow ' +
            'your answers to be retained.')



######################################################################
# validate_one_case(pre)
#   A helper function to validate the user's choices about one case.
#   pre is the first few characters of the associated choices names
#  (e.g. 'nom-acc-nom')

def validate_one_case(ch, err, pre):
  if not ch.get(pre + '-case-name'):
    add_err(err, pre + '-case-name', 'You must specify a name for every case.')


######################################################################
# validate_case(ch, err)
#   Validate the user's choices about case

def validate_case(ch, err):
  cm = ch.get('case-marking')

  if not cm:
    add_err(err, 'case-marking', 'You must specify if/how case is marked.')

  if cm in ['nom-acc', 'split-n', 'split-v']:
    validate_one_case(ch, err, cm + '-nom')
    validate_one_case(ch, err, cm + '-acc')
  if cm in ['erg-abs', 'split-n', 'split-v']:
    validate_one_case(ch, err, cm + '-erg')
    validate_one_case(ch, err, cm + '-abs')
  if cm in ['tripartite', 'split-s', 'fluid-s', 'focus']:
    validate_one_case(ch, err, cm + '-a')
    validate_one_case(ch, err, cm + '-o')
  if cm in ['tripartite']:
    validate_one_case(ch, err, cm + '-s')
  if cm in ['focus']:
    validate_one_case(ch, err, cm + '-focus')

  if cm == 'none' and ch.get('case1_name'):
    add_err(err,
            'case1_name',
            'You may not specify additional cases ' +
            'if your language has no case marking.')

  if ch.get('scale1_feat1_name') and not ch.get('scale-equal'):
    add_err(err,
            'scale-equal',
            'If you define a direct-inverse scale, ' +
            'you must say what direction the verb is ' +
            'when the agent and patient have equal rank.')


######################################################################
# validate_person(ch, err)
#   Validate the user's choices about person

def validate_person(ch, err):
  person = ch.get('person')
  fp = ch.get('first-person')

  if not person:
    add_err(err,
            'person',
            'You must specify how many persons your language distinguishes.')
  else:
    if person in ['none', '2-non-2', '3-non-3']:
      if fp and fp != 'none':
        add_err(err,
                'first-person',
                'If your language does not have the first person, it ' +
                'cannot distinguish sub-values of the first person.')
    if person in ['1-2-3', '1-2-3-4', '1-non-1']:
      if not fp:
        add_err(err,
                'first-person',
                'If your language has the first person, you must specify ' +
                'whether it makes finer distinctions within that category.')


######################################################################
# validate_number(ch, err)
#   Validate the user's choices about number

def validate_number(ch, err):
  for n, number in enumerate(ch.get('number')):
    if 'name' not in number:
      add_err(err,
              'number' + str(n) + '_name',
              'You must specify a name for each number you define.')


######################################################################
# validate_gender(ch, err)
#   Validate the user's choices about gender

def validate_gender(ch, err):
  for g, gender in enumerate(ch.get('gender')):
    if 'name' not in gender:
      add_err(err,
              'gender' + str(g) + '_name',
              'You must specify a name for each gender you define.')


######################################################################
# validate_other_features(ch, err)
#   Validate the user's choices about other features

def validate_other_features(ch, err):
  for f, feature in enumerate(ch.get('feature')):
    if 'name' not in feature:
      add_err(err,
              'feature' + str(f+1) + '_name',
              'You must specify a name for each feature you define.')

    if 'type' not in feature:
      add_err(err,
              'feature' + str(f+1) + '_type',
              'You must specify a type for each feature you define.')

    for v, value in enumerate(feature.get('value', [])):
      if 'name' not in value:
        add_err(err,
                'feature' + str(f+1) + '_value' + str(v+1) + '_name',
                'You must specify a name for each value you define.')
      if 'supertype' not in value or 'name' not in value['supertype'][0]:
        add_err(err,
                'feature' + str(f+1) + '_value' + str(v+1) + '_supertype1_name',
                'You must specify a supertype for each value you define.')


######################################################################
# validate_word_order(ch, err)
#   Validate the user's choices about basic word order.

# There should be some value for word order
# If has-dets is true, there should be some value for noun-det-order
# If aux-verb is defined, then aux-order needs to be too
# I'm currently adding AUX as a feature in specialize_word_order()
# but wonder if instead (in addition?) we should be doing validation
# so that we don't find ourselves worrying about auxiliaries if we
# don't have any in the lexicon.

def validate_word_order(ch, err):
  
  # General word order
  if (not ch.get('word-order')):
    add_err(err,
            'word-order',
            'You must specify a choice for the basic word order.')

  # Things to do with determiners
  if (not ch.get('has-dets')):
    add_err(err,
            'has-dets',
            'You must specify whether your language has determiners.')
  
  if ((ch.get('has-dets') == 'yes') and (not ch.get('noun-det-order'))):
    add_err(err,
            'noun-det-order',
            'If your language has determiners, ' +
            'you must specify their order with respect to nouns.')

  if (ch.get('noun-det-order') and (not ch.get('has-dets'))):
    add_err(err,
            'has-dets',
            'You specified an order of nouns and dets, ' +
            'but not whether your language has determiners at all.')

  if ch.get('det1_stem1_orth') and ch.get('has-dets') == 'no':
    add_err(err,
            'has-dets',
            'You specified lexical entries for determiners, ' +
            'but said your language has none.')

  #Things to do with auxiliaries
  if (not ch.get('has-aux')):
    add_err(err,
            'has-aux',
            'You must specify whether your language has auxiliary verbs.')

  if ((ch.get('has-aux') == 'yes') and (not ch.get('aux-comp-order'))):
    add_err(err,
            'aux-comp-order',
            'If your language has auxiliaries, you must specify their order ' +
            'with respect to their complements.')

  if (ch.get('aux-comp-order') and (not ch.get('has-aux'))):
    add_err(err,
            'has-aux',
            'You specified an order for auxiliaries and their complements, ' +
            'but not whether your language has auxiliaries at all.')

  if ((ch.get('has-aux') == 'yes') and (not ch.get('aux-comp'))):
    add_err(err,
            'aux-comp',
            'If your language has auxiliaries, you must specify ' +
            'whether they take s, vp, or v complements.')

  wo = ch.get('word-order')
  co = ch.get('aux-comp-order')
  ac = ch.get('aux-comp')

  if ac == 'v' and (wo == 'free' or (wo == 'vso' and co == 'before') or (wo == 'osv' and co == 'after')):
    if (not ch.get('v-cluster')):    
      add_err(err,
              'v-cluster',
              'With this general word order and auxiliary v-comp order, ' +
              'you need to specify whether your language forms ' +
              'vc-clusters or not.')
  if ac == 'vp' and ch.get('v-cluster') == 'yes':
    add_err(err,
            'v-cluster',
            'If your auxiliary takes a vp-complement, we assume ' +
            'it does not form verbal clusters.')

  if (((wo == 'vso' and co == 'after') or (wo == 'osv' and co == 'before')) and ac == 'vp'):
    add_err(err,
            'aux-comp',
            'The general word order and aux-comp order ' +
            'are not compatible with vp complements.')

  if wo == 'v2' and ch.get('v-cluster') == 'yes':
    add_err(err,
            'v-cluster',
            'Sorry, but verbal clusters have not been implemented yet ' +
            'for v2 languages. Please answer "no" to this question.') 

######################################################################
# validate_sentential_negation(ch, err)
#   Validate the user's choices about sentential negation.

def validate_sentential_negation(ch, err):
  
  neginfltype = ch.get('neg-infl-type')
  negseladv = ch.get('neg-sel-adv')

  # ERB 2009-01-23 Commenting out the following because infl-neg is
  # now handled with customize_inflection.  We should eventually give
  # a warning if infl-neg is selected but no lexical rules actually
  # use it.  I think it would make sense for that warning to go
  # on the negation page.

  # If affix is indicated, must select prefix/suffix and
  # main/aux/either and give form.
#   if (ch.get('infl-neg') == 'on'):
#     if (not ch.get('neg-infl-type')):
#       mess = 'If sentential negation is expressed through affixation, you must specify what the affix attaches to.'
#       add_err(err, 'neg-infl-type', mess)
#     if (not ch.get('neg-aff')):
#       mess = 'If sentential negation is expressed through affixation, you must specify whether its a prefix or a suffix'
#       add_err(err, 'neg-aff', mess)
#     if (not ch.get('neg-aff-orth')):
#       mess = 'If sentential negation is expressed through affixation, you must specify the form of the affix'
#       add_err(err, 'neg-aff-orth', mess)
#     # If aux is selected then has-aux = 'yes' must be chosen in word order section
#     if ((neginfltype == 'aux' or neginfltype == 'aux-main') and ch.get('has-aux') != 'yes'):
#         mess = 'You have not indicated on the word order page that your language has auxiliaries.'
#         add_err(err, 'neg-infl-type', mess)

  # If adverb is indicated, must lexical entry, what it modifies, and
  # ind/selected modifier
  if (ch.get('adv-neg') == 'on'):
#    if (not ch.get('neg-adv')):
#      mess = 'If sentential negation is expressed through an adverb, you must specify whether the adverb is a selected complement or an independent modifier.'
#      add_err(err, 'neg-adv', mess)
#    if (ch.get('neg-adv') == 'ind-adv'):
    if (not ch.get('neg-mod')):
      mess = 'If sentential negaton is expressed through an adverb, ' +\
             'you must specify what type of constituent the adverb modifies.'
      add_err(err, 'neg-mod', mess)
    if (not ch.get('neg-order')):
      mess = 'If sentential negaton is expressed through an adverb, ' +\
             'you must specify what side of its host the adverb attaches to.'
      add_err(err, 'neg-order', mess)
    if (not ch.get('neg-adv-orth')):
      mess = 'If sentential negation is expressed through an adverb, ' +\
             'you must specify the form of the adverb.'
      add_err(err, 'neg-adv-orth', mess)

   # If aux is selected then has-aux = 'yes' must be chosen in word
   # order section
    if ((negseladv == 'aux' or negseladv == 'main-aux') and
        ch.get('has-aux') != 'yes'):
        mess = 'You have not indicated on the word order page ' +\
               'that your language has auxiliaries.'
        add_err(err, 'neg-sel-adv', mess)

   # ERB 2009-01-23 Currently not possible to say how they combine.

#   # If both strategies are checked, then they must say how they combine:
#   if ((ch.get('infl-neg') == 'on') and (ch.get('adv-neg') == 'on')):
#     if (not ch.get('multi-neg')):
#       mess = 'If you have selected both affix and adverb realizations of sentential negation, you must specify how they interact.'
#       add_err(err, 'multi-neg', mess)

######################################################################
# validate_coordination(ch, err)
#   Validate the user's choices about coordination.

def validate_coordination(ch, err):
  for c, cs in enumerate(ch.get('cs')):
    cs_n =     cs.get('n')
    cs_np =    cs.get('np')
    cs_vp =    cs.get('vp')
    cs_s =     cs.get('s')
    cs_pat =   cs.get('pat')
    cs_mark =  cs.get('mark')
    cs_order = cs.get('order')
    cs_orth =  cs.get('orth')

    prefix = 'cs' + str(c+1) + '_'

    if not (cs_n or cs_np or cs_vp or cs_s):
      mess = 'You must specify a phrase type for coordination strategy ' + str(c+1)
      add_err(err, prefix + 'n', mess)
      add_err(err, prefix + 'np', mess)
      add_err(err, prefix + 'vp', mess)
      add_err(err, prefix + 's', mess)

    if cs_pat == 'a':
      if cs_mark:
        mess = 'You must not specify word/affix ' +\
               'for an asyndetic coordination strategy.'
        add_err(err, prefix + 'mark', mess)
      if cs_order:
        mess = 'You must not specify before/after ' +\
               'for an asyndetic coordination strategy.'
        add_err(err, prefix + 'order', mess)
      if cs_orth:
        mess = 'You must not specify a spelling ' +\
               'for an asyndetic coordination strategy.'
        add_err(err, prefix + 'orth', mess)
    else:
      if not cs_pat:
        mess = 'You must specify a pattern ' +\
               'for coordination strategy ' + str(c+1)
        add_err(err, prefix + 'pat', mess)
      if not cs_mark:
        mess = 'You must specify word/affix ' +\
               'for coordination strategy ' + str(c+1)
        add_err(err, prefix + 'mark', mess)
      if not cs_order:
        mess = 'You must specify before/after ' +\
               'for coordination strategy ' + str(c+1)
        add_err(err, prefix + 'order', mess)
      if not cs_orth:
        mess = 'You must specify a spelling ' +\
               'for coordination strategy ' + str(c+1)
        add_err(err, prefix + 'orth', mess)

    if cs_mark == 'affix' and (cs_np or cs_vp or cs_s):
      mess = 'Marking coordination with an affix is not yet supported ' +\
             'on phrases (NPs, VPs, or sentences)'
      add_err(err, prefix + 'mark', mess)


######################################################################
# validate_yesno_questions(ch, err)
#   Validate the user's choices about matrix yes/no questions.

def validate_yesno_questions(ch, err):
  qinvverb = ch.get('q-inv-verb')
  qpartorder = ch.get('q-part-order')
  qpartorth = ch.get('q-part-orth')
  qinfltype = ch.get('q-infl-type')

  if ch.get('q-part'):
    if not qpartorder:
      mess = 'If you chose the question particle strategy ' +\
             'for yes-no questions, you must specify ' +\
             'where the question particle appears.'
      add_err(err, 'q-part-order', mss)
    if not qpartorth:
      mess = 'If you chose the question particle strategy ' +\
             'for yes-no questions, you must specify ' +\
             'the form of the question particle.'
      add_err(err, 'q-part-orth', mess)

  if ch.get('q-inv'):
    #    if qinvverb != 'aux' and qinvverb != 'main' and qinvverb != 'main-aux':
    #      mess = 'There is something wrong with the verb type (main/aux) for inverted questions.  Please contact developers.'
    #      add_err(err, 'q-inv-verb', mess)
    if not qinvverb:
      mess = 'If you chose subject-verb inversion strategy ' +\
             'for yes-no questions, you must specify ' +\
             'which types of verbs invert.'
      add_err(err, 'q-inv-verb', mess)
    if ch.get('word-order') == 'v-final' or \
       ch.get('word-order') == 'v-initial' or \
       ch.get('word-order') == 'free':
      mess = 'Subject-verb inversion strategy for yes-no questions ' +\
             'is not supported for V-final, V-initial, or ' +\
             'free word order languages.  If you believe you have ' +\
             'a counterexample to this, please contact us.'
      add_err(err, 'q-inv', mess)
    if ((qinvverb == 'aux' or qinvverb == 'aux-main') and
        ch.get('has-aux') != 'yes'):
      mess = 'You have not indicated on the word order page ' +\
             'that your language has auxiliaries.'
      add_err(err, 'q-inv-verb', mess)

  if ch.get('q-infl'):
    if (not ch.get('q-infl-type')):
      mess = 'If matrix yes-no questions are expressed through affixation, ' +\
             'you must specify what the affix attaches to.'
      add_err(err, 'q-infl-type', mess)
    if (not ch.get('ques-aff')):
      mess = 'If matrix yes-no questions are expressed through affixation, ' +\
             'you must specify whether it\'s a prefix or a suffix'
      add_err(err, 'ques-aff', mess)
    if (not ch.get('ques-aff-orth')):
      mess = 'If matrix yes-no questions are expressed through affixation, ' +\
             'you must specify the form of the affix'
      add_err(err, 'ques-aff-orth', mess)
    if ((qinfltype == 'aux' or qinfltype == 'aux-main') and
        ch.get('has-aux') != 'yes'):
      mess = 'You have not indicated on the word order page ' +\
             'that your language has auxiliaries.'
      add_err(err, 'q-infl-type', mess)

# validate_tanda(ch, err)
#   Validate the user's choices about tense, aspect (viewpoint and
#   situation) and form features

def validate_tanda(ch, err):
  '''
  Validate the user's choices about tense, aspect (viewpoint and situation) and form features
  '''
  
  ## validate tense
  chosen = False
  ten = ('past', 'present', 'future', 'nonpast', 'nonfuture') 
  for t in ten:
    if ch.get(t):
      chosen = True
    elif ch.get(t + '-subtype1_name'):
      mess = 'You cannot add a subtype if the supertype is not selected.'
      add_err(err, t, mess)

  if ch.get('tense-definition') == 'choose' and not chosen:
    mess = 'You have chosen to select among hierarchy elements. ' +\
           'You need to select at least one tense element.'
    for t in ten:
      add_err(err, t, mess)

  if ch.get('tense-definition') == 'build' and not ch.get('tense1_name'):
    mess = 'You have chosen to build your own tense hierarchy ' +\
           'so you must enter at least one tense subtype.'
    add_err(err, 'tense-definition', mess)

    for t, tense in enumerate(ch.get('tense')):
      if 'name' not in tense:
        add_err(err,
                'tense' + str(t+1) + '_name',
                'You must specify a name for each tense subtype you define.')
      if 'supertype' not in tense or 'name' not in tense['supertype'][0]:
        add_err(err,
                'tense' + str(t+1) + '_supertype1_name',
                'You must specify a supertype for each tense subtype you define.')
  
  ## validate aspect
  for a, aspect in enumerate(ch.get('aspect')):
    if 'name' not in aspect:
      add_err(err,
              'aspect' + str(a+1) + '_name',
              'You must specify a name for each ' +
              'viewpoint aspect subtype you define.')
    if 'supertype' not in aspect or 'name' not in aspect['supertype'][0]:
      add_err(err,
              'aspect' + str(a+1) + '_supertype1_name',
              'You must specify at least one supertype for each ' +
              'viewpoint aspect subtype you define.')

  ## validate situation
  for s, situation in enumerate(ch.get('situation')):
    if 'name' not in situation:
      add_err(err,
              'situation' + str(s+1) + '_name',
              'You must specify a name for each ' +
              'situation aspect subtype you define.')
    if 'supertype' not in situation or 'name' not in situation['supertype'][0]:
      add_err(err,
              'situation' + str(s+1) + '_supertype1_name',
              'You must specify at least one supertype for each ' +
              'situation aspect subtype you define.')

  ## validate form
  if ch.get('has-aux') == 'yes' and ch.get('noaux-fin-nf') == 'on':
    mess = 'You have indicated on the word order page that ' +\
           'your language has auxiliaries.'
    add_err(err, 'noaux-fin-nf', mess)
  
  if ch.get('has-aux') == 'no' and not (ch.get('noaux-fin-nf') == 'on'):
    if ch.get('nf-subform1_name'):
      mess = 'You have indicated that your language has no auxiliaries but ' +\
             'you have entered subforms of finite or non-finite.'
      add_err(err, 'noaux-fin-nf', mess)

######################################################################
# validate_lexicon(ch, err)
#   Validate the user's choices about the test lexicon.

def validate_lexicon(ch, err):

  # Did they specify enough lexical entries?
  if not ch.get('noun1_stem1_orth'):
    mess = 'You must create at least one noun class.'
    add_err(err, 'noun1_stem1_orth', mess)

  for n, noun in enumerate(ch.get('noun')):
    det = noun.get('det')

    # Did they answer the question about determiners?
    if not det:
      mess = 'You must specify whether each noun you define takes a determiner.'
      add_err(err, 'noun' + str(n+1) + '_det', mess)

    # If they said the noun takes an obligatory determiner, did they
    # say their language has determiners?
    if det == 'obl' and ch.get('has-dets') == 'no':
      mess = 'You defined a noun that obligatorily takes a determiner, ' +\
             'but also said your language does not have determiners.'
      add_err(err, 'has-dets', mess)
      add_err(err, 'noun' + str(n+1) + '_det', mess)

    for s, stem in enumerate(noun.get('stem', [])):
      orth = stem.get('orth')
      pred = stem.get('pred')

      # Did they give a spelling?
      if not orth:
        mess = 'You must specify a spelling for each noun you define.'
        add_err(err, 'noun' + str(n+1) + '_stem' + str(s+1) + '_orth', mess)

      # Did they give a predicate?
      if not pred:
        mess = 'You must specify a predicate for each noun you define.'
        add_err(err, 'noun' + str(n+1) + '_stem' + str(s+1) + '_pred', mess)

  # Verbs
  seenTrans = False
  seenIntrans = False
  for v, verb in enumerate(ch.get('verb')):
    val = verb.get('valence')

    if not val:
      mess = 'You must specify the argument structure of each verb you define.'
      add_err(err, 'verb' + str(v+1) + '_valence', mess)
    elif val[0:5] == 'trans' or '-' in val:
      seenTrans = True
    else:
      seenIntrans = True

    for s, stem in enumerate(verb.get('stem', [])):
      orth = stem.get('orth')
      pred = stem.get('pred')

      if not orth:
        mess = 'You must specify a spelling for each verb you define.'
        add_err(err, 'verb' + str(v+1) + '_stem' + str(s+1) + '_orth', mess)

      if not pred:
        mess = 'You must specify a predicate for each verb you define.'
        add_err(err, 'verb' + str(v+1) + '_stem' + str(s+1) + '_pred', mess)

  if not (seenTrans and seenIntrans):
    mess = 'You must create intransitive and transitive verb classes.'
    add_err(err, 'verb1_valence', mess)
    add_err(err, 'verb2_valence', mess)


  # Auxiliaries

  aux1_name = ch.get('aux1_name')
  if ch.get('has-aux') != 'yes':
    if aux1_name:
      mess = 'You have indicated that your language has no auxiliaries but ' +\
             'have entered an auxiliary on the Lexicon page.'
      add_err(err, 'has-aux', mess)

  if ch.get('has-aux') == 'yes':
    if not aux1_name:
      mess = 'You have indicated that your language has auxiliaries. ' +\
             'You must define at least one auxiliary type.'
      add_err(err, 'auxlabel', mess)

  comp = ch.get('aux-comp')
  for a, aux in enumerate(ch.get('aux')):
    sem = aux.get('sem')
    pred = aux.get('pred')
    subj = aux.get('subj')

    prefix = 'aux' + str(a + 1) + '_'

    if 'stem' not in aux or 'orth' not in aux['stem'][0]:
      mess = 'You must specify a stem for each auxiliary type defined.'
      add_err(err, prefix + 'stem1_orth', mess)

    if not sem:
      mess = 'You must specify whether the auxiliary contributes a predicate.'
      add_err(err, prefix + 'sem', mess)

    if sem == 'add-pred':
      for f, feat in enumerate(aux.get('feat', [])):
        if feat.get('name') and not feat.get('value'):
          mess = 'You must specify a value for this feature.'
          add_err(err, prefix + 'feat' + str(f+1) + '_value', mess)

    if comp == 'vp' or comp == 'v':
      if not subj:
        mess = 'You must specify the subject type.'
        add_err(err, prefix + 'subj', mess)

    compform = 'no'
    for c, cf in enumerate(aux.get('compfeature', [])):
      name = cf.get('name')
      if name == 'form':
        compform = 'yes'
      if name and not cf.get('value'):
        mess = 'You must specify a value for this feature.'
        add_err(err, prefix + 'compfeature' + str(c+1) + '_value', mess)

    if not compform == 'yes':
      mess = 'You must specify the form of the verb in the complement, ' +\
             'i.e., the value of the complement feature FORM.'
      add_err(err, prefix + 'complabel', mess)


    for s, stem in enumerate(aux.get('stem', [])):
      if sem == 'add-pred' and not stem.get('pred'):
        mess = 'You have indicated that this type contributes a predicate. ' +\
               'You must specify the predicate name.'
        add_err(err, prefix + 'stem' + str(s+1) + '_pred', mess)
      if sem != 'add-pred' and stem.get('pred'):
        mess = 'You have specified a predicate but indicated ' +\
               'that this type does not contribute a predicate.'
        add_err(err, prefix + 'sem', mess)


  # Determiners
  for d, det in enumerate(ch.get('det')):
    for s, stem in enumerate(det.get('stem', [])):
      if not stem.get('orth'):
        mess = 'You must specify a spelling for each determiner you define.'
        add_err(err, 'det' + str(d+1) + '_stem' + str(s+1) + '_orth', mess)

      if not stem.get('pred'):
        mess = 'You must specify a predicate for each determiner you define.'
        add_err(err, 'det' + str(d+1) + '_stem' + str(s+1) + '_pred', mess)

  # Features on all lexical types
  for lextype in ('noun', 'verb', 'aux', 'det', 'adp'):
    for l, lt in enumerate(ch.get(lextype)):
      for f, feat in enumerate(lt.get('feat', [])):
        prefix = lextype + str(l+1) + '_feat' + str(f+1) + '_'
        if not feat.get('name'):
          mess = 'You must choose which feature you are specifying.'
          add_err(err, prefix + 'name', mess)
        if not feat.get('value'):
          mess = 'You must choose a value for each feature you specify.'
          add_err(err, prefix + 'value', mess)

        if lextype == 'verb' and not feat.get('head'):
          mess = 'You must choose where the feature is specified.'
          add_err(err, prefix + 'head', mess)

        if not ch.has_dirinv() and feat.get('head') in ['higher', 'lower']:
          mess = 'That choice is not available in languages ' +\
                 'without a direct-inverse scale.'
          add_err(err, prefix + 'head', mess)

  # Inflectional Slots
  for slotprefix in ('noun', 'verb', 'det'):
    for s, slot in enumerate(ch.get(slotprefix + '-slot')):
      prefix = slotprefix + '-slot' + str(s+1) + '_'

      if not slot.get('order'):
        mess = 'You must specify an order for every slot you define.'
        add_err(err, prefix + 'order', mess)

      if 'input' not in slot or 'type' not in slot['input'][0]:
        mess = 'You must specify at least one input for every slot.'
        add_err(err, prefix + 'input1_type', mess)

      for m, morph in enumerate(slot.get('morph', [])):
        for f, feat in enumerate(morph.get('feat', [])):
          if not feat.get('name'):
            mess = 'You must choose which feature you are specifying.'
            add_err(
              err,
              prefix + '_morph' + str(m+1) + '_feat' + str(f+1) + '_name',
              mess)
          if not feat.get('value'):
            mess = 'You must choose a value for each feature you specify.'
            add_err(
              err,
              prefix + '_morph' + str(m+1) + '_feat' + str(f+1) + '_value',
              mess)

          if slotprefix == 'verb' and not feat.get('head'):
            mess = 'You must choose where the feature is specified.'
            add_err(
              err,
              prefix + '_morph' + str(m+1) + '_feat' + str(f+1) + '_head',
              mess)


######################################################################
# validate_test_sentences(ch, err)
#   Validate the user's choices about test sentences.

def validate_test_sentences(ch, err):
  pass

######################################################################
# validate_extra_constraints()
#   Some extra constraints we want to put on the random grammars
#   for the regression/other testing

def validate_extra_constraints(ch, err):

  if ch.get('aux-sem') == 'pred':
    mess = 'Only semantically empty auxiliaries in test grammars.'
    add_err(err, 'aux-sem', mess)
  if ch.get('has-dets') == 'yes' and not ch.get('det1_stem1_orth'):
    mess = 'To get uniform semantics, we always want det1 specified.'
    add_err(err, 'det1_stem1_orth', mess)
  if ch.get('cs1_n') != 'on' and ch.get('cs2_n') != 'on':
    mess = 'The test grammars must have some way to coordinate nouns.'
    add_err(err, 'cs1_n', mess)
#  if ch.get('multi-neg') != '':
#    if ch.get('infl-neg') != 'on' or ch.get('adv-neg') != 'on':
#      mess = 'Giving a value for multi-neg means you have selected both neg. strategies.'
#      add_err(err, 'multi-neg', mess)
#   if ch.get('infl-neg') == '':
#     if ch.get('neg-infl-type') != '' or \
#        ch.get('neg-aff') != '' or \
#        ch.get('neg-aff-orth') != '' :
#       mess = 'You have not selected inflectional negation.'
#       add_err(err, 'infl-neg', mess)
#   if ch.get('adv-neg') == '':
#     if ch.get('neg-adv') != '' or \
#        ch.get('neg-mod') != '' or \
#        ch.get('neg-order') != '' or \
#        ch.get('neg-adv') != '' or \
#        ch.get('neg-sel-adv') != '' :
#       mess = 'You have not selected adverbial negation.'
#       add_err(err, 'adv-neg', mess)


######################################################################
# Validation of TDL type names

def validate_types(ch, err):
  """
  Consider every choice that results in the definition of a type in
  the output TDL, and make sure that (a) the types are legal and (b)
  they're unique.
  """
  pass


######################################################################
# Validation of features and feature values

def validate_features(ch, err):
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

  for s, scale in enumerate(ch.get('scale')):
    for f, feat in enumerate(scale.get('feat', [])):
      prefix = 'scale' + str(s+1) + '_feat' + str(f+1) + '_'
      name_list += \
        [[ prefix + 'name', feat.get('name') ]]
      value_list += \
        [[ prefix + 'value', feat.get('name'), feat.get('value') ]]

  for lexprefix in ('noun', 'verb', 'det', 'aux'):
    for l, lex in enumerate(ch.get(lexprefix)):
      for f, feat in enumerate(lex.get('feat', [])):
        prefix = lexprefix + str(l+1) + '_feat' + str(f+1) + '_'
        name_list += \
          [[ prefix + 'name', feat.get('name') ]]
        value_list += \
          [[ prefix + 'value', feat.get('name'), feat.get('value') ]]

  for slotprefix in ('noun', 'verb', 'det', 'aux'):
    for s, slot in enumerate(ch.get(slotprefix + '-slot')):
      for m, morph in enumerate(slot.get('morph', [])):
        for f, feat in enumerate(morph.get('feat', [])):
          prefix = slotprefix + '-slot' + str(s+1) + \
                   '_morph' + str(m+1) + '_feat' + str(f+1) + '_'
          name_list += \
            [[ prefix + 'name', feat.get('name') ]]
          value_list += \
            [[ prefix + 'value', feat.get('name'), feat.get('value') ]]

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
      add_err(err, var, 'You have selected an invalid feature name.')

  # Check the value list to ensure they're all valid values
  features = ch.features()
  for item in value_list:
    var = item[0]    # choices variable name
    name = item[1]   # feature name
    value = item[2]  # feature value
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
      add_err(err, var, 'You have selected an invalid feature value.')


def validate_arg_opt(ch, err):
  """Check to see if the user completed the necessary portions of the arg
   opt page"""

  if ch.get('subj-drop') and not ch.get('subj-mark-drop'):
    add_err(err,
            'subj-mark-drop',
            'You must select whether a subject marker is ' +
            'required, optional, or not permitted with subject dropping.')

  if ch.get('subj-drop') and not ch.get('subj-mark-no-drop'):
    add_err(err,
            'subj-mark-no-drop',
            'You must select whether a subject marker is ' +
            'required, optional, or not permitted with an overt subject.')

  if ch.get('obj-drop') and not ch.get('obj-mark-drop'):
    add_err(err,
            'obj-mark-drop',
            'You must select whether an object marker is ' +
            'required, optional, or not permitted with object dropping.')

  if ch.get('obj-drop') and not ch.get('obj-mark-no-drop'):
    add_err(err,
            'obj-mark-no-drop',
            'You must select whether a object marker is ' +
            'required, optional, or not permitted with an overt object.')
  

######################################################################
# validate_choices(choices_file)
#   Validate the choices file found in choices_file.  Return
#   a dictionary whose keys are choices file variables that are
#   incorrect and whose values are messages describing the errors.

def validate_choices(choices_file, extra = False):
  ch = ChoicesFile(choices_file)
  err = {}

  validate_general(ch, err)
  validate_case(ch, err)
  validate_person(ch, err)
  validate_number(ch, err)
  validate_gender(ch, err)
  validate_other_features(ch, err)
  validate_word_order(ch, err)
  validate_tanda(ch, err)
  validate_sentential_negation(ch, err)
  validate_coordination(ch, err)
  validate_yesno_questions(ch, err)
  validate_lexicon(ch, err)
  validate_test_sentences(ch, err)

  validate_types(ch, err)
  validate_features(ch, err)
  validate_arg_opt(ch, err)

  if extra:
    validate_extra_constraints(ch, err)

  return err
