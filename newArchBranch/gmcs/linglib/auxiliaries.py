from gmcs.utils import TDLencode

from gmcs.linglib import case
from gmcs.linglib import features
from gmcs.linglib.parameters import determine_vcluster

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
        mylang.add(supertype + ' := [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ] > ].')

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
