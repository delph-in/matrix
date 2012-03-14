from gmcs.linglib import case
from gmcs.linglib import lexical_items
from gmcs.utils import get_name
from gmcs.linglib.lexbase import LexicalType, PositionClass
from gmcs.linglib.lexbase import ALL_LEX_TYPES
from gmcs.linglib.lexbase import LEXICAL_CATEGORIES
from gmcs.linglib.lexbase import LEXICAL_SUPERTYPES

def lexical_type_hierarchy(choices, lexical_supertype):
  if lexical_supertype not in LEXICAL_CATEGORIES:
    return None
  lth = PositionClass(key=lexical_supertype + '-pc',
                      name=get_lt_name(lexical_supertype, choices),
                      identifier_suffix='lex-super', lex_rule=False)
  lth.add_node(LexicalType(lexical_supertype,
                           get_lt_name(lexical_supertype, choices)))
  lts_to_add = [lexical_supertype]
  if lexical_supertype == 'verb':
    if choices['has-aux'] == 'yes':
      lth.add_node(LexicalType('aux', get_lt_name('aux', choices),
                               parents={'verb':lth.nodes['verb']}))
      lth.add_node(LexicalType('mverb', get_lt_name('mverb', choices),
                               parents={'verb':lth.nodes['verb']}))
      lts_to_add += ['aux']
    st = get_lexical_supertype('iverb', choices)
    lth.add_node(LexicalType('iverb', get_lt_name('iverb', choices),
                             parents={st:lth.nodes[st]}))
    st = get_lexical_supertype('tverb', choices)
    lth.add_node(LexicalType('tverb', get_lt_name('tverb', choices),
                             parents={st:lth.nodes[st]}))
  for lst in lts_to_add:
    for lt in choices[lst]:
      st = get_lexical_supertype(lt.full_key, choices)
      lth.add_node(LexicalType(lt.full_key, get_lt_name(lt.full_key, choices),
                               parents={st:lth.nodes[st]}))
      # If we're dealing with a verb add nodes for all lexical entries
      # because bistems can give rise to flags that need to appear on
      # all verbs.
      if lexical_supertype == 'verb':
        bistems = choices[lt.full_key]['bistem'] or []
        stems = choices[lt.full_key]['stem'] or []
        stems.extend(bistems)
        for stem in stems:
          lth.add_node(LexicalType(stem.full_key, stem['name'],
                                   parents={lt.full_key:lth.nodes[lt.full_key]},
                                   entry=True))
  return lth

def get_lexical_supertype(lt_key, choices):
  lexical_category = lt_key.rstrip('0123456789')
  if lexical_category in ('iverb','tverb') and choices['has-aux'] == 'yes':
    return 'mverb'
  elif lexical_category in ('aux','mverb','iverb','tverb'):
    return 'verb'
  elif lexical_category == 'verb':
    return case.interpret_verb_valence(choices[lt_key]['valence'])
  elif lexical_category in ('noun', 'det', 'aux'):
    return lexical_category
  return None


def expand_lexical_supertype(st_key, choices):
  """
  Given a generic lexical supertype, like those defined in
  lexical_supertypes, return a list of all defined lexical types that
  fit in that pattern. For example, 'tverb' may return verbs with
  valence marked as nom-acc.
  """
  i_t = ['iverb','tverb']
  m = ['mverb']
  if st_key not in LEXICAL_SUPERTYPES: return []
  if (st_key == 'mverb' and choices['has-aux'] == 'yes') or \
     (st_key == 'verb' and choices['has-aux'] == 'no'):
    return [v.full_key for v in choices['verb']] + i_t
  elif st_key == 'verb' and choices['has-aux'] == 'yes':
    return [v.full_key for v in choices['verb'] + choices['aux']] + i_t + m
  elif st_key in ('iverb','tverb'):
    return [v.full_key for v in choices['verb']
            if case.interpret_verb_valence(v['valence']) == st_key]
  else:
    return [x.full_key for x in choices.get(st_key,[])]

def get_lexical_supertype_expansions(choices):
  """
  Return a dictionary mapping each supertype to its possible
  expansions as lexical types defined in the choices file.
  """
  return dict([(st, expand_lexical_supertype(st, choices))
               for st in LEXICAL_SUPERTYPES])

def used_lexical_supertypes(choices):
  """
  Return a list of lexical supertypes (as defined in
  lexical_supertypes) that will actually be used in the grammar.
  """
  used = set()
  for x in ['noun','aux','adj','det']:
    if x in choices:
      used.add(x)
  if 'verb' in choices:
    used.add('verb')
    used.update([case.interpret_verb_valence(v['valence'])
                 for v in choices['verb']])
    if choices['has-aux'] == 'yes':
      used.add('mverb')
  return used

def get_lexical_supertypes(lrt_key, choices):
  """
  Return the list of appropriate lexical types for the given rule
  type. There may be more than one for verbs ([tverb, mverb, verb]) or
  auxiliaries ([aux, verb]).
  """
  lexical_category = lrt_key.rstrip('0123456789')
  # first check if we are already dealing with a generic type
  if lexical_category == lrt_key:
    if lrt_key in ('iverb','tverb'):
      if choices['has-aux'] == 'yes': return ['mverb','verb']
      else: return ['verb']
    elif lrt_key == 'aux': return ['verb']
    else: return []
  # otherwise we have a lexical type (e.g. noun1, verb2, etc)
  elif lexical_category == 'verb':
    verb_type = case.interpret_verb_valence(choices[lrt_key]['valence'])
    return [verb_type] + get_lexical_supertypes(verb_type, choices)
  elif lexical_category == 'aux':
    return ['verb']
  elif lexical_category in ('noun', 'det', 'adj'):
    return [lexical_category]
  return []

def get_lt_name(key, choices):
  if key in LEXICAL_SUPERTYPES:
    # lexical supertype-- pull out of lexical_supertypes, remove '-lex'
    return LEXICAL_SUPERTYPES[key].rsplit('-lex',1)[0]
  else:
    # defined lextype, name may or may not be defined
    name = get_name(choices[key])
    lex_st = LEXICAL_SUPERTYPES[key.strip('1234567890')]
    return '-'.join([name, lex_st.rsplit('-lex',1)[0]])

######################################################################
# validate_lexicon(ch, vr)
#   Validate the user's choices about the test lexicon.

def validate_lexicon(ch, vr):

  # Did they specify enough lexical entries?
  if 'noun' not in ch:
    mess = 'You should create at least one noun class.'
    vr.warn('noun1_stem1_orth', mess)

  # Nouns

  # check noun hierarchy for various properties
  # this is easier if we build some data objects
  
  # for each nountype, 
  #  if it's got stems, it'll need to be put in the lexicon
  #    check that it has an answer for 
  #      * has-dets
  #      * that its has-dets doesn't conflict with any parents
  #      * check its features 
  #        ** see that they don't conflict with each other
  #      * see that the hierarchy won't cause the 
  #        vacuous inheritance eg: 
  #          a := noun                   
  #          b := a
  #          c := b & a

  # ntsts is a dict of nountype names->lists of supertypes
  # inherited_feats dict of nountype names->lists of inherited features
  ntsts = {} 
  feats = {}
  inherited_feats = {}
  
  for noun in ch.get('noun'):
    ntsts[noun.full_key] = noun.get('supertypes').split(', ')
    feats[noun.full_key] = {} 
    for f in noun.get('feat'):
      feats[noun.full_key][f.get('name')]=f.get('value')
   
  
  # now we can figure out inherited features and write them down
  # also, print warnings about conflicts
  # also, figure out the det question for types with stems

  # also, every noun type needs a path to the root
  for n in ch.get('noun'):
    st_anc = [] # used to make sure we don't have an lkb err as described in comments above
    root = False
    det = n.get('det')
    # det == '' is okay for now, check again after inheritance is computed
    seen = [] 
    paths= []
    # seed paths with the starting node and the first crop of sts
    for st in ntsts[n.full_key]:
      paths.append([n.full_key, st])
    parents = ntsts[n.full_key] 
    inherited_feats[n.full_key]= {}
    while (True):
      next_parents = []
      for p in parents:
        if p == '':
          root = True

        # get features from this parent and put em in inherited_feats
        if p != '':
          ptype = ch.get(p)
          for f in feats[p]:
            # see if this feat conflicts with what we already know
            if f in feats[n.full_key] and feats[p][f] != feats[n.full_key][f]:
              # inherited feature conficts with self defined feature
              vr.warn(n.full_key + '_feat', "The specification of "+f+"="+str(feats[n.full_key][f])+" may confict with the value defined on the supertype "+ptype.get('name')+" ("+p+").")
            elif f in inherited_feats[n.full_key] and feats[p][f] != inherited_feats[n.full_key][f]: 
              vr.warn(n.full_key + '_supertypes', "The inherited specification of "+f+"="+str(inherited_feats[n.full_key][f])+" may confict with the value defined on the supertype "+ptype.get('name')+" ("+p+").")
              inherited_feats[n.full_key][f] = "! "+inherited_feats[n.full_key][f]+" && "+feats[p][f]
            else:
              inherited_feats[n.full_key][f] = feats[p][f]

          # det question
          pdet = ptype.get('det')
          if pdet != '':
            if det == '':
              det = pdet
            elif det != pdet:
              vr.err(n.full_key + '_det', "This noun type inherits a conflict in answer to the determiner question.",concat=False)

          # add sts to the next generation
          to_be_seen = []
          for r in paths:
            if r[-1] == p: #this is the path to extend, 
              paths.remove(r)
              for q in ntsts[p]: #go through all sts
                # q is a st_anc of n
                if not q in st_anc:
                  st_anc.append(q)
                if q in r:
                  vr.err(n.full_key + '_supertypes', "This hierarchy contains a cycle.  Found "+q+" at multiple points in path: "+str(r + [q]))
                else:
                  new_path = r + [q]
                  paths.append(new_path)
                if (q != ''):
                  if not (q in seen):
                    next_parents.append(q)
                    to_be_seen.append(q)
                else:
                  root = True
          seen = seen + to_be_seen

      # if there aren't any next parents, we're done
      if len(next_parents) == 0:
        break
      
      # otherwise we go again
      parents = next_parents 
    
    if(len(inherited_feats[n.full_key]) > 0):
      vr.info(n.full_key + '_feat', "inherited features are: "+str(inherited_feats[n.full_key]))

    if not root:
      vr.err(n.full_key + '_supertypes', "This noun type doesn't inherit from noun-lex or a descendent.")

    # Now check for the lkb err about vacuous inheritance using goodmami's method: find
    #  the intersection of supertypes and supertypes's ancestors
    for t in ntsts[n.full_key]:
      if t in st_anc:
        vr.warn(n.full_key + '_supertypes', 'This noun hierarchy may cause an lkb error.  _'+t+'_ is both an immediate supertype and also an ancestor of another supertype.')

    # Now we can check whether there's an answer to the question about determiners?
    # but I only really care about types with stems (right??)
    s = n.get('stem', [])
    if len(s) != 0:
      if not det:
        mess = 'You must specify whether this noun takes a determiner.  Either on this type or on a supertype.'
        vr.err(n.full_key + '_det', mess)

    # If they said the noun takes an obligatory determiner, did they
    # say their language has determiners?
    if det == 'obl' and ch.get('has-dets') == 'no':
      mess = 'You defined a noun that obligatorily takes a determiner, ' +\
             'but also said your language does not have determiners.'
      vr.err('has-dets', mess)
      vr.err(n.full_key + '_det', mess)

    # or if they said the noun takes an obligatory determiner, did they
    # provide any determiners?
    if det == 'obl' and (not 'det' in ch): 
      mess = 'You defined a noun that obligatorily takes a determiner, ' +\
             'but you haven\'t yet defined any determiners.'
      vr.warn(n.full_key + '_det', mess)

    for stem in s:
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
    bistems = verb.get('bistem', [])
    bipartitepc = verb.get('bipartitepc')
    
    if not val:
      mess = 'You must specify the argument structure of each verb you define.'
      vr.err(verb.full_key + '_valence', mess)
    elif val[0:5] == 'trans' or '-' in val:
      seenTrans = True
    else:
      seenIntrans = True

    if bistems and not bipartitepc:
      mess = 'If you add bipartite stems to a class, you must specify a position class for the affix part of the stems.'
      vr.err(verb.full_key + '_bipartitepc', mess)

    for stem in verb.get('stem', []):
      orth = stem.get('orth')
      pred = stem.get('pred')

      if not orth:
        mess = 'You must specify a spelling for each verb you define.'
        vr.err(stem.full_key + '_orth', mess)

      if not pred:
        mess = 'You must specify a predicate for each verb you define.'
        vr.err(stem.full_key + '_pred', mess)

    for bistem in bistems:
      orth = bistem.get('orth')
      aff = bistem.get('aff')
      pred = bistem.get('pred')

      if not orth:
        mess = 'You must specify a spelling for each verb you define.'
        vr.err(bistem.full_key + '_orth', mess)

      if not aff:
        mess = 'You must specify a affix for each bipartite verb stem you define.'
        vr.err(bistem.full_key + '_aff', mess)

      if not pred:
        mess = 'You must specify a predicate for each verb you define.'
        vr.err(bistem.full_key + '_pred', mess)

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
      if not stem.get('orth'):
        mess = 'You must specify a spelling for each auxiliary you define.'
        vr.err(stem.full_key + '_orth', mess);

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

