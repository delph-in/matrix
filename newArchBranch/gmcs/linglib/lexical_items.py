from gmcs.utils import get_name
from gmcs.utils import TDLencode

from gmcs.linglib import case
from gmcs.linglib import features
from gmcs.linglib.parameters import determine_vcluster
from gmcs.linglib.parameters import has_auxiliaries_p

##########################################################
# customize_verbs()

def customize_verbs(mylang, ch, lexicon, hierarchies):
  negmod = ch.get('neg-mod')
  negadv = ch.get('neg-adv')
  wo = ch.get('word-order')
  auxcomp = ch.get('aux-comp')
  auxorder = ch.get('aux-comp-order')
  # Do we need to constrain HC-LIGHT on verbs, to distinguish V from VP?
  hclight = (negadv == 'ind-adv' and negmod == 'v')
  hclightallverbs = False

  if has_auxiliaries_p(ch):
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

  if has_auxiliaries_p(ch):
    mylang.add('head :+ [ AUX bool ].', section='addenda')
    #mainorverbtype = 'main-verb-lex'

# we need to know whether the auxiliaries form a vcluster

    auxcomp = ch.get('aux-comp')
    wo = ch.get('word-order')
    auxorder = ch.get('aux-comp-order')
    vcluster = determine_vcluster(auxcomp, auxorder, wo, ch)

    typedef = \
      'verb-lex := lex-item & \
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
      mylang.add('main-verb-lex := [ SYNSEM.VC + ].')
      mylang.add('aux-lex := [ SYNSEM.VC - ].')
  else:
    #mainorverbtype = 'verb-lex'
    vcluster = False
    mylang.add('verb-lex := basic-verb-lex.')

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

    for stem in verb.get('stem', []):
      orth = stem.get('orth')
      pred = stem.get('pred')
      typedef = \
        TDLencode(orth) + ' := ' + vtype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

# Returns the verb type for lexical/main verbs.
def main_or_verb(ch):
  if has_auxiliaries_p(ch):
    return 'main-verb-lex'
  else:
    return 'verb-lex'



#######################################################
def customize_users_auxtype(aux, userstype, supertype, mylang, ch, hierarchies):
  """
  A utility that declares the userstype as subtype of supertype and
  calls the functions that specify feature values on the type.
  Called by customize_auxiliaries.
  userstype = userstypename in customize_auxiliaries
  supertype = userstypename in customize_auxiliaries
  """

  features.customize_feature_values(mylang, ch, hierarchies, aux, userstype, 'aux')
  features.customize_feature_values(mylang, ch, hierarchies, aux, userstype, 'auxcomplement')
  mylang.add(userstype + ' := ' + supertype + '.')


def add_subj_tdl(type, subj, subjcase, mylang):
  """
  A function to add subject related tdl to type definition if the complement
  is either a V or a VP.
  Called by customize_auxiliaries().
  type = supertype variable in customize_auxiliaries()
  subj = subj variable in customize_auxiliaries()
  """
  if subj == 'adp':
    mylang.add(type + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD adp ].')
  else:
    mylang.add(type + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD noun ].')
    if subj == 'np-comp-case':
      mylang.add(type + ' := [ ARG-ST < [ LOCAL.CAT.HEAD.CASE #case  ], \
                                        [ LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE #case ] > ] > ].')
    elif subj == 'np-aux-case':
      mylang.add(type + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + subjcase + ' ].')

def get_auxtypename(sem, supertype):
  """
  A function that creates the auxiliary type name.
  sem = sem variable from customize_auxiliaries()
  supertype = supertype variable from customize_auxiliaries()
  """
  if sem == 'add-pred':
    auxtypename = supertype + '-with-pred'
  else:
    auxtypename = supertype + '-no-pred'
  return auxtypename

def customize_auxiliaries(mylang, ch, lexicon, hierarchies):

  if has_auxiliaries_p(ch):
    lexicon.add_literal(';;; Auxiliaries')
    auxcomp = ch.get('aux-comp')
    wo = ch.get('word-order')
    auxorder = ch.get('aux-comp-order')
    vc = determine_vcluster(auxcomp, auxorder, wo, ch)#ch.get('v-cluster') 

    for aux in ch.get('aux',[]):
      name = aux.get('name','')
      userstypename = name + '-aux-lex'
      sem = aux.get('sem','')
      subj = aux.get('subj','')
      subjc = aux.get('subj_case','') #TODO: verify _-delimited key
      cases = case.case_names(ch)
      subjcase = case.canon_to_abbr(subjc, cases)

    # Lexical type for auxiliaries.
    # ASF 2008-11-25 added constraints SS.LOC.CONT.HOOK.XARG #xarg and
    # ARG-ST.FIRST.LOCAL.CONT.HOOK.INDEX #xarg for subj-raise-aux and
    # arg-comp-aux, to insure the subject is passed up when several auxs are
    # present in sentence. Implemented here, because it required least changes
    # it may be cleaner to have this in general verb-lex, as well as the first
    # ARG is #subj constraint (but not possible for aux with s-comp)

      if auxcomp == 'vp':
        supertype = 'subj-raise-aux'
        auxtypename = get_auxtypename(sem, supertype)

        typedef = supertype + ' := aux-lex & trans-first-arg-raising-lex-item  & \
                   [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #subj >, \
                                            COMPS < #comps >, \
                                            SPR < >, \
                                            SPEC < > ], \
                                    CONT.HOOK.XARG #xarg ], \
                     ARG-ST < #subj & \
                              [ LOCAL [ CAT.VAL [ SPR < >, \
                                                  COMPS < > ],\
                                        CONT.HOOK.INDEX #xarg ] ], \
                              #comps & \
                              [ LOCAL.CAT [ VAL [ SUBJ < [ ] >, \
                                                  COMPS < > ], \
                                            HEAD verb ]] > ].'
        mylang.add(typedef)
        add_subj_tdl(supertype, subj, subjcase, mylang)

# ASF 2009-12-21 Changing conditions, we now have a question on whether
# there can be more than on auxiliary per clause, this holds for all complements

        if ch.get('multiple-aux') == 'no':
          mylang.add(supertype + ' := [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].')
        if sem == 'add-pred':
          typedef = auxtypename + ' := ' + supertype + ' & norm-sem-lex-item & \
                                        trans-first-arg-raising-lex-item-1 .'
          mylang.add(typedef)

        else:
          comment = \
            '; To keep the semantically empty ones from spinning on\n' + \
            '; generation, require complement to be [AUX -].  The\n' + \
            '; FORM feature might be enough in the starter grammars,\n' + \
            '; but I don\'t want to rely on this.  Then again, [ AUX - ]\n' + \
            '; might not be true.  Be sure to put in a comment.'
          mylang.add_literal(comment)
          # changed inheritance here to remove redundancy
          typedef = auxtypename + ' := ' + supertype + ' & raise-sem-lex-item & \
                      [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
          mylang.add(typedef)

        customize_users_auxtype(aux, userstypename, auxtypename, mylang, ch, hierarchies)

      elif auxcomp == 'v':
        supertype = 'arg-comp-aux'
        auxtypename = get_auxtypename(sem, supertype)
        comment = \
          '; Somewhat surprisingly, this inherits from basic-two-arg, so\n' + \
          '; that the non-local features are amalgamated from subj, the\n' + \
          '; lexical verb complement, but not the other complements, if any.'
        mylang.add_literal(comment)

        typedef = supertype + ' := aux-lex & basic-two-arg & \
             [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #subj  >, \
                                      COMPS < #comps . #vcomps >, \
                                      SPR < >, \
                                      SPEC < > ], \
                              CONT.HOOK.XARG #xarg ], \
               ARG-ST < #subj & \
                        [ LOCAL [ CAT [ VAL [ SPR < >, \
                                              COMPS < > ]], \
                                  CONT.HOOK.INDEX #xarg ]], \
                      #comps & \
                      [ LIGHT +, \
                        LOCAL [ CAT [ VAL [ SUBJ < [ ] >, \
                                            COMPS #vcomps ], \
                                      HEAD verb ], \
                                CONT.HOOK.XARG #xarg ]] > ].'
        mylang.add(typedef)
        add_subj_tdl(supertype, subj, subjcase, mylang)

# ASF 2008-12-07 For now we restrict free word order with v-comp to
# either verbal clusters or one auxiliary max
# ASF 2009-12-21 Changing conditions, we now have a question on whether
# there can be more than on auxiliary per clause, this holds for all complements

        if ch.get('multiple-aux') == 'no':
          mylang.add(supertype + ' := [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].')

        if sem == 'add-pred':
          comment = \
            '; Not inheriting from basic-verb-lex, so need to put in\n' + \
            '; event-relation by hand here.'
          mylang.add_literal(comment)

          typedef = auxtypename + ' := ' + supertype + ' & hcons-lex-item & \
               [ SYNSEM [ LOCAL [ CONT.HCONS <! qeq & \
                                                [ HARG #harg, \
                                                  LARG #larg ] !> ], \
                          LKEYS.KEYREL event-relation & \
                                       [ ARG1 #harg ]], \
                 ARG-ST < [ ], [ LOCAL.CONT.HOOK.LTOP #larg ] > ].'
          mylang.add(typedef)

        else:
          comment = \
            '; Note that raise-sem-lex-item assumes the first complement is\n' + \
            '; where the HOOK comes from.  It\'s not clear to me how you\'d\n' + \
            '; tell that you had an argument composition auxiliary if it\n' + \
            '; wasn\'t appearing adjacent to the verb.'
          mylang.add_literal(comment)

          typedef = auxtypename + ' := ' + supertype + '  & raise-sem-lex-item & \
               [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
          mylang.add(typedef)

        customize_users_auxtype(aux, userstypename, auxtypename, mylang, ch, hierarchies)

      elif auxcomp == 's':
        supertype = 's-comp-aux'
        auxtypename = get_auxtypename(sem, supertype)

        typedef = supertype + ' := aux-lex & basic-one-arg & \
             [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                      COMPS < #comps >, \
                                      SPR < >, \
                                      SPEC < > ], \
               ARG-ST < #comps & \
                        [ LOCAL.CAT [ VAL [ SUBJ < >, \
                                            COMPS < > ], \
                                      HEAD verb ]] > ].'
        mylang.add(typedef)

# ASF 2009-12-21 Changing conditions, we now have a question on whether
# there can be more than on auxiliary per clause, this holds for all complements

        if ch.get('multiple-aux') == 'no':
          mylang.add(supertype + ' := [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].')

        if sem == 'add-pred':
          mylang.add_literal('; S comp aux, with pred')

          typedef = auxtypename + ' := ' + supertype + ' & hcons-lex-item & \
                [ SYNSEM [ LOCAL.CONT.HCONS <! qeq & \
                                               [ HARG #harg, \
                                                 LARG #larg ] !>, \
                           LKEYS.KEYREL event-relation & \
                                        [ ARG1 #harg ]], \
                  ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] > ].'
          mylang.add(typedef)

        else:
          mylang.add_literal('; S comp aux, no predicate')

  # LAP 2008-06-11 FIX this: literals may print more than once
  #   here and elsewhere in the loop
          comment = \
            '; Better say [ AUX - ] on complement here, or we\'ll spin' + \
            ' on generation.'
          mylang.add_literal(comment)

          typedef = auxtypename + ' := ' + supertype + \
               ' & raise-sem-lex-item & \
               [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ] > ].'
          mylang.add(typedef)

        customize_users_auxtype(aux, userstypename, auxtypename, mylang, ch, hierarchies)

      # add stems to lexicon
      for stem in aux.get('stem',[]):
        orth = stem.get('orth')
        typedef = TDLencode(orth) + ' := ' + userstypename + ' & \
                       [ STEM < "' + orth + '" > ].'
        lexicon.add(typedef)

        if sem == 'add-pred':
          pred = stem.get('pred')
          typedef = TDLencode(orth) + \
                    ' := [ SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
          lexicon.add(typedef, merge=True)

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
      orth = stem.get('orth')
      pred = stem.get('pred')
      typedef = \
        TDLencode(orth) + ' := ' + dtype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

def customize_misc_lex(ch, lexicon):

  #lexicon.add_literal(';;; Other')

  # Question particle
  if ch.get('q-part'):
    orth = ch.get('q-part-orth')
    typedef = \
      TDLencode(orth) + ' := qpart-lex-item & \
                   [ STEM < "' + orth + '" > ].'
    lexicon.add(typedef)

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
      orth = stem.get('orth')
      pred = stem.get('pred')
      typedef = TDLencode(orth) + ' := ' + ntype + ' & \
                  [ STEM < "' + orth + '" >, \
                    SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)



######################################################################
# customize_lexicon()
#   Create the type definitions associated with the user's test
#   lexicon.



def customize_lexicon(mylang, ch, lexicon, hierarchies):

  mylang.set_section('nounlex')
  customize_nouns(mylang, ch, lexicon, hierarchies)

  mylang.set_section('otherlex')
  to_cfv = case.customize_case_adpositions(mylang, lexicon, ch)
  features.process_cfv_list(mylang, ch, hierarchies, to_cfv, tdlfile=lexicon)

  mylang.set_section('verblex')
  customize_verbs(mylang, ch, lexicon, hierarchies)

  mylang.set_section('auxlex')
  customize_auxiliaries(mylang, ch, lexicon, hierarchies)

  mylang.set_section('otherlex')
  customize_determiners(mylang, ch, lexicon, hierarchies)
  customize_misc_lex(ch, lexicon)
