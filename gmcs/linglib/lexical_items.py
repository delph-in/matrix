from gmcs.utils import get_name
from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

from gmcs.linglib import case
from gmcs.linglib import features
from gmcs.linglib import auxiliaries
from gmcs.linglib.parameters import determine_vcluster
from gmcs.linglib.lexbase import ALL_LEX_TYPES

##########################################################
# insert_ids()

def insert_ids(ch):
  """
  Create a unique identifier for each lexical entry based
  on the stem value but allowing for separate lexical items
  with the same stem.  Store in the choices file object.
  """
  stemids = {}
  stemidcounters = {}
  postypes = ALL_LEX_TYPES

  for postype in postypes:
    for pos in ch.get(postype):
      # For ordinary stems, use the stem orthography itself
      # as the basis of the identifier.
      for stem in pos.get('stem'):
        orth = stem.get('orth')
        if orth in stemids.keys():
          stemids[orth] += 1
        else:
          stemids[orth] = 1
      # For bistems, build the identifier out of the orthography
      # plus the affix, but store these in the same dictionary
      # to account for possible name-space collisions.
      for bistem in pos.get('bistem'):
        aff = bistem.get('aff')
        orth = bistem.get('orth')
        id = orth + '+' + aff

        if id in stemids.keys():
          stemids[id] += 1
        else:
          stemids[id] = 1

  # Now that stemids has the full count, go through and add
  # to the choices file object.

  for postype in postypes:
    for pos in ch.get(postype):
      for stem in pos.get('stem'):
        orth = stem.get('orth')
        if stemids[orth] == 1:
          ch[stem.full_key + '_name'] = orth
        elif orth not in stemidcounters:
          stemidcounters[orth] = 1
          ch[stem.full_key + '_name'] = orth + '_1'
        else:
          stemidcounters[orth] += 1
          ch[stem.full_key + '_name'] = orth + '_' + str(stemidcounters[orth])
      for bistem in pos.get('bistem'):
        orth = bistem.get('orth') + '+' + bistem.get('aff')
        if stemids[orth] == 1:
          ch[bistem.full_key + '_name'] = orth
        elif orth not in stemidcounters:
          stemidcounters[orth] = 1
          ch[bistem.full_key + '_name'] = orth + '_1'
        else:
          stemidcounters[orth] += 1
          ch[bistem.full_key + '_name'] = orth + '_' + str(stemidcounters[orth])


##########################################################
# customize_verbs()

def customize_bipartite_stems(ch):
  """
  Users specify bipartite stems as roots + affixes in bipartite
  stem specifications plus position class for affix in lexical type.
  Take this information and add choices that create the lexical
  rules as well as the constraints that make sure that the two
  parts appear together.
  """
  # For each verb type
  for verb in ch.get('verb'):

    # Check whether there are bipartite stems
    bistems = verb.get('bistem')
    if bistems:
      # Find position class for affixes

      pcname = verb.get('bipartitepc')
      pc = None
      for vpc in ch.get('verb-pc'):
        if vpc.full_key == pcname:
          pc = vpc

      # Make dictionary with affixes as keys and lists
      # of stems as values.  This will let us find out if
      # any verbs share same affix
      avpairs = {}
      for stem in bistems:
        aff = stem.get('aff')
        orth = stem.get('orth')

        # Update affix-stem dictionary
        if aff in avpairs.keys():
          avpairs[aff].append(stem.full_key)
        else:
          avpairs[aff] = [stem.full_key]

      # Get stem list again because I want access to the
      # info I've added since first initializing stems
      bistems = verb.get('bistem')

      for aff in avpairs.keys():
        # Get iter number for lrts:
        if pc['lrt']:
          iternum = str(pc['lrt'].next_iter_num())
        else:
          iternum = '1'

        # Create lexical rules types and instances for each affix
        next_lrt_str = pc.full_key + '_lrt' + iternum
        ch[next_lrt_str + '_require1_others'] = ', '.join(avpairs[aff])
        ch[next_lrt_str + '_lri1_orth'] = aff
        ch[next_lrt_str + '_lri1_inflecting'] = 'yes'

        # Add requires constrains on stems
        for stemid in avpairs[aff]:
          ch[stemid + '_require1_others'] = next_lrt_str

def customize_verbs(mylang, ch, lexicon, hierarchies):
  negmod = ch.get('neg-mod')
  negadv = ch.get('neg-adv')
  wo = ch.get('word-order')
  auxcomp = ch.get('aux-comp')
  auxorder = ch.get('aux-comp-order')
  # Do we need to constrain HC-LIGHT on verbs, to distinguish V from VP?
  hclight = (negadv == 'ind-adv' and negmod == 'v')
  hclightallverbs = False

  if ch.get('has-aux') == 'yes':
    vc = determine_vcluster(auxcomp, auxorder, wo, ch)
    if wo == 'vso' or wo == 'osv':
      wo = 'req-hcl-vp'
    if auxcomp == 'v' and hclight != True:
      hclight = True
      if wo != 'free' or vc == True:
        hclightallverbs = True
    if auxcomp == 'vp' and wo == 'req-hcl-vp':
      hclightallverbs = True
  else:
    vc = False

  if wo == 'req-hcl-vp':
    wo = ch.get('word-order')

  # Lexical types for verbs
  # I'm adding the constraint to associate XARG with the
  # first ARG-ST element here (so raising auxiliaries work),
  # but perhaps this belongs in matrix.tdl?  Or maybe this
  # is another module/parameter (like, the external argument
  # might not be the first one?

  mainorverbtype = main_or_verb(ch)
# The variable mainorverbtype is a type name for lexical/main (non-aux) verbs.
# Note that the use of 'main' instead of 'lexical' is strictly for
# coding clarity
# If there are auxiliaries, non-aux verbs are 'main-verb-lex', and 'verb-lex'
# includes both aux and lexical/main verbs.
# If there are no auxiliaries then 'verb-lex' covers all verbs

  # Neither mainverbs or auxs should start out as modifiers (for now)
  # Assigning constraint to verb-lex

  if ch.get('has-aux') == 'yes':
    mylang.add('head :+ [ AUX bool ].', section='addenda')
    #mainorverbtype = 'main-verb-lex'

# we need to know whether the auxiliaries form a vcluster

    auxcomp = ch.get('aux-comp')
    wo = ch.get('word-order')
    auxorder = ch.get('aux-comp-order')
    vcluster = determine_vcluster(auxcomp, auxorder, wo, ch)

    typedef = \
      'verb-lex := non-mod-lex-item & \
                 [ SYNSEM.LOCAL.CAT.HEAD verb ].'
    mylang.add(typedef)
    typedef = \
      'main-verb-lex := verb-lex & basic-verb-lex & \
                      [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].'
    mylang.add(typedef)
    typedef = \
      'aux-lex := verb-lex & \
                [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].'
    mylang.add(typedef)
    if vcluster:
      mylang.add('main-verb-lex := [ SYNSEM.LOCAL.CAT.VC + ].')
      mylang.add('aux-lex := [ SYNSEM.LOCAL.CAT.VC - ].')
  else:
    #mainorverbtype = 'verb-lex'
    vcluster = False
    mylang.add('verb-lex := basic-verb-lex & non-mod-lex-item.')

  typedef = mainorverbtype + ' :=  \
       [ SYNSEM.LOCAL [ CAT.VAL [ SPR < >, \
                                  SPEC < >, \
                                  SUBJ < #subj > ], \
                        CONT.HOOK.XARG #xarg ], \
         ARG-ST < #subj & \
                  [ LOCAL [ CAT.VAL [ SPR < >, \
                                      COMPS < > ], \
                            CONT.HOOK.INDEX #xarg ] ], ... > ].'
  mylang.add(typedef)

  if hclightallverbs:
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')
  elif hclight:
    comment = \
      ';;; If there are aspects of the syntax which pick out\n' + \
      ';;; lexical Vs (transitive or intransitive) such as V-attachment\n' + \
      ';;; of adverbs or argument composition auxiliaries which take V\n' + \
      ';;; complements, we need to distinguish (intranstive) V and VP.\n' + \
      ';;; To do so, we make use of a feature LIGHT.  Phrases are\n' + \
      ';;; generally [LIGHT -] with the exception of head-complement\n' + \
      ';;; phrases, which take their value for LIGHT from the head\'s\n' + \
      ';;; HC-LIGHT feature.  To make this work for us here, constraint\n' + \
      ';;; HC-LIGHT on verbs to be -.'
#    mylang.add_literal(comment)
    mylang.add(mainorverbtype + ' := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')

  # intransitive verb lexical type
  typedef = \
    'intransitive-verb-lex := ' + mainorverbtype + ' & intransitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].'
  mylang.add(typedef)

  # transitive verb lexical type
  typedef = \
    'transitive-verb-lex := ' + mainorverbtype + ' & transitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
         ARG-ST < [ ], \
                  #comps & \
                  [ LOCAL.CAT [ VAL [ SPR < >, \
                                      COMPS < > ] ] ] > ].'
  mylang.add(typedef)

  case.customize_verb_case(mylang, ch)

  # Add constraints to choices to create lex rules for bipartite stems
  customize_bipartite_stems(ch)

  # Lexical entries
  lexicon.add_literal(';;; Verbs')

  # Now create the lexical entries for all the defined verb types
  cases = case.case_names(ch)
  for verb in ch.get('verb',[]):
    name = get_name(verb)
    val = verb.get('valence')

    i = val.find(',')
    dir_inv = ''
    if i != -1:
      val = val[:i]
      dir_inv = 'dir-inv-'

    if val == 'trans':
      tivity = 'trans'
    elif val == 'intrans':
      tivity = 'intrans'
    elif val.find('-') != -1:
      c = val.split('-')
      a_case = case.canon_to_abbr(c[0], cases)
      o_case = case.canon_to_abbr(c[1], cases)
      tivity = a_case + '-' + o_case + '-trans'
    else:
      s_case = case.canon_to_abbr(val, cases)
      tivity = s_case + '-intrans'

    stype = dir_inv + tivity + 'itive-verb-lex'
    vtype = name + '-verb-lex'

    mylang.add(vtype + ' := ' + stype + '.')

    features.customize_feature_values(mylang, ch, hierarchies, verb, vtype, 'verb', None, cases)

    stems = verb.get('stem', [])
    stems.extend(verb.get('bistem', []))

    for stem in stems:
      orthstr = orth_encode(stem.get('orth'))
      pred = stem.get('pred')
      id = stem.get('name')
      typedef = \
        TDLencode(id) + ' := ' + vtype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

# Returns the verb type for lexical/main verbs.
def main_or_verb(ch):
  if ch.get('has-aux') == 'yes':
    return 'main-verb-lex'
  else:
    return 'verb-lex'


def customize_determiners(mylang, ch, lexicon, hierarchies):

  # Lexical type for determiners, if the language has any:
  if ch.get('has-dets') == 'yes':
    comment = \
      ';;; Determiners\n' + \
      ';;; SPEC is non-empty, and already specified by basic-determiner-lex.'
    mylang.add_literal(comment)

    typedef = \
      'determiner-lex := basic-determiner-lex & basic-zero-arg & \
          [ SYNSEM.LOCAL.CAT.VAL [ SPR < >, \
                                   COMPS < >, \
                                   SUBJ < > ]].'
    mylang.add(typedef)
    
    mylang.add('determiner-lex := non-mod-lex-item.')
  # Determiners
  if 'det' in ch:
    lexicon.add_literal(';;; Determiners')

  for det in ch.get('det',[]):
    name = get_name(det)

    stype = 'determiner-lex'
    dtype = name + '-determiner-lex'

    mylang.add(dtype + ' := ' + stype + '.')

    features.customize_feature_values(mylang, ch, hierarchies, det, dtype, 'det')

    for stem in det.get('stem',[]):
      orthstr = orth_encode(stem.get('orth'))
      pred = stem.get('pred')
      id = stem.get('name')
      typedef = \
        TDLencode(id) + ' := ' + dtype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

def customize_misc_lex(ch, lexicon, trigger):

  #lexicon.add_literal(';;; Other')

  # Question particle
  if ch.get('q-part'):
    orth = ch.get('q-part-orth')
    orthstr = orth_encode(orth)
    typedef = \
      TDLencode(orth) + ' := qpart-lex-item & \
                   [ STEM < "' + orthstr + '" > ].'
    lexicon.add(typedef)
    
    grdef = TDLencode(orth) +'_gr := arg0e_gtr & \
                   [ CONTEXT [ RELS <! [ PRED "non_existing_rel" ] !> ], \
                     FLAGS.TRIGGER "' + TDLencode(orth) + '" ].'
    trigger.add(grdef)

def customize_nouns(mylang, ch, lexicon, hierarchies):
  # Figure out which kinds of determiner-marking are in the language
  seen = {'obl':False, 'opt':False, 'imp':False}
  seenCount = 0

  for noun in ch.get('noun',[]):
    det = noun.get('det')
    if not seen[det]:
      seen[det] = True
      seenCount += 1

  singlentype = (seenCount == 1)

  # Playing fast and loose with the meaning of OPT on SPR.  Using
  # OPT - to mean obligatory (as usual), OPT + to mean impossible (that's
  # weird), and leaving OPT unspecified for truly optional.  Hoping
  # this will work at least for LSA111 lab.

  # ERB 2006-11-28 Update: To make that weird use of OPT work, the
  # head-spec rule has to require [OPT -] on its non-head daughter.
  # Adding that just in case we add the no-spr-noun-lex type.

  typedef = \
    'noun-lex := basic-noun-lex & basic-one-arg & no-hcons-lex-item & \
       [ SYNSEM.LOCAL [ CAT.VAL [ SPR < #spr & [ LOCAL.CAT.HEAD det ] >, \
                                  COMPS < >, \
                                  SUBJ < >, \
                                  SPEC < > ] ], \
         ARG-ST < #spr > ].'
  mylang.add(typedef)

  # Adding empty MOD on general definitiion for noun-lex
  mylang.add('noun-lex := non-mod-lex-item.')

  if singlentype:
    if seen['obl']:
      typedef = 'noun-lex := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
      mylang.add(typedef)
    elif seen['imp']:
      typedef = 'noun-lex := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
      mylang.add(typedef)
  else:
    if seen['obl']:
      typedef = \
        'obl-spr-noun-lex := noun-lex & \
           [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
      mylang.add(typedef)

    if seen['imp']:
      typedef = \
        'no-spr-noun-lex := noun-lex & \
           [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
      mylang.add(typedef)

  if seen['imp'] and ch.get('has-dets') == 'yes':
    mylang.add(
      'head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.OPT - ].',
      'Nouns which cannot take specifiers mark their SPR requirement\n' +
      'as OPT +.  Making the non-head daughter OPT - in this rule\n' +
      'keeps such nouns out.')

  if ch.get('case-marking') != 'none':
    if not ch.has_adp_case():
      mylang.add('noun :+ [ CASE case ].', section='addenda')

  # Add the lexical entries
  lexicon.add_literal(';;; Nouns')

  for noun in ch.get('noun',[]):
    name = get_name(noun)
    det = noun.get('det')

    if singlentype or det == 'opt':
      stype = 'noun-lex'
    elif det == 'obl':
      stype = 'obl-spr-noun-lex'
    else:
      stype = 'no-spr-noun-lex'

    ntype = name + '-noun-lex'

    mylang.add(ntype + ' := ' + stype + '.')

    features.customize_feature_values(mylang, ch, hierarchies, noun, ntype, 'noun')

    for stem in noun.get('stem', []):
      orthstr = orth_encode(stem.get('orth'))
      pred = stem.get('pred')
      id = stem.get('name')
      typedef = TDLencode(id) + ' := ' + ntype + ' & \
                  [ STEM < "' + orthstr + '" >, \
                    SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)



######################################################################
# customize_lexicon()
#   Create the type definitions associated with the user's test
#   lexicon.



#def customize_lexicon(mylang, ch, lexicon, hierarchies):
def customize_lexicon(mylang, ch, lexicon, trigger, hierarchies):

  comment = '''Type assigning empty mod list. Added to basic types for nouns, verbs and determiners.'''
  mylang.add('non-mod-lex-item := lex-item & \
               [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].',comment)

  mylang.set_section('nounlex')
  customize_nouns(mylang, ch, lexicon, hierarchies)

  mylang.set_section('otherlex')
  # to_cfv = case.customize_case_adpositions(mylang, lexicon, ch)
  to_cfv = case.customize_case_adpositions(mylang, lexicon, trigger, ch)
  features.process_cfv_list(mylang, ch, hierarchies, to_cfv, tdlfile=lexicon)

  mylang.set_section('verblex')
  customize_verbs(mylang, ch, lexicon, hierarchies)
  
  if ch.get('has-aux') == 'yes':
    mylang.set_section('auxlex')
    auxiliaries.customize_auxiliaries(mylang, ch, lexicon, trigger, hierarchies)

  mylang.set_section('otherlex')
  customize_determiners(mylang, ch, lexicon, hierarchies)
  customize_misc_lex(ch, lexicon, trigger)
