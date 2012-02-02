from gmcs.utils import get_name
from gmcs.utils import TDLencode

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

  if ch.get('has-aux') == 'yes':
    mylang.add('head :+ [ AUX bool ].', section='addenda')
    #mainorverbtype = 'main-verb-lex'

# we need to know whether the auxiliaries form a vcluster

    auxcomp = ch.get('aux-comp')
    wo = ch.get('word-order')
    auxorder = ch.get('aux-comp-order')
    vcluster = determine_vcluster(auxcomp, auxorder, wo, ch)

    if ch.get('rel-clause') == 'yes':
      verb_super = 'non-rel-lex-item'
    else:
      verb_super = 'lex-item'

    typedef = \
      'verb-lex := ' + verb_super + ' & \
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

  ###Germanic slightly generalized: using form to allow verbs to select
  ###for an auxiliary (haben/sein + ptc)
    if ch.get('aux-select') == 'yes':
      auxs = ch.get('sel_aux',[])
      for aux in auxs:
        auxn = aux.get('value')
        mylang.add(auxn + '-only-verb-lex := verb-lex & \
                    [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + auxn + '-only ].')
  #Germanic change: uses VC differently
    if vcluster and not wo == 'v2':
      mylang.add('main-verb-lex := [ SYNSEM.LOCAL.CAT.VC + ].')
      mylang.add('aux-lex := [ SYNSEM.LOCAL.CAT.VC - ].')
  #Small addition for Germanic
    if ch.get('verb-cluster') == 'yes' and wo == 'v2':
      if ch.get('vc-analysis') == 'basic' and not ch.get('old-analysis') == 'yes':
        mylang.add('main-verb-lex := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')
        mylang.add('aux-lex := [ SYNSEM.LOCAL.CAT.VFRONT - ].')
  else:
    #mainorverbtype = 'verb-lex'
    vcluster = False
    mylang.add('verb-lex := basic-verb-lex.')
  
  mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].')
  
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

###adding type selecting for reflexive pronouns
  refl = False
  if ch.get('reflexives') == 'yes':
    refl = True
    typedef = \
       'refl-verb-lex := ' + mainorverbtype + ' & \
         [ SYNSEM.LOCAL.CAT.VAL [ COMPS.FIRST.LOCAL refl-local & \
                                     [ CONT.HOOK.INDEX #ind ], \
                             SUBJ < [ LOCAL.CONT.HOOK.INDEX #ind ] > ] ].'
    mylang.add(typedef)    
    if ch.get('rel-clause') == 'yes':
       non_r_loc = 'rel-non-refl-local'
    else:
      non_r_loc = 'non-refl-local'
    mylang.add('non-refl-verb-lex := ' + mainorverbtype + ' & \
                 [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL ' + non_r_loc + ' ].')

  if ch.get('expl-two-arg') == 'yes':
    comment = 'Matix only provides explicit arg only or clausal complement. \
              General type for explicit and referential arg.'
    typedef = \
      'expl-two-arg-lex-item := basic-two-arg-no-hcons & \
              [ ARG-ST < [ LOCAL.CONT.HOOK.INDEX expl-ind ], \
                         [ LOCAL.CONT.HOOK.INDEX ref-ind & #ind ] >, \
                SYNSEM.LKEYS.KEYREL.ARG1 #ind  ].'
    mylang.add(typedef, comment)
    mylang.add('expl-two-arg-verb-lex := expl-two-arg-lex-item & \
                    ' + mainorverbtype + '.')
    if refl:
      mylang.add('expl-two-arg-verb-lex := \
                 [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL ' + non_r_loc + ' ].')
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
    'transitive-verb-lex := transitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
         ARG-ST < [ ], \
                  #comps & \
                  [ LOCAL.CAT [ VAL [ SPR < >, \
                                      COMPS < > ] ] ] > ].'
  mylang.add(typedef)
  if refl:
    mylang.add('transitive-verb-lex := non-refl-verb-lex.')
  else:
    mylang.add('transitive-verb-lex := ' + mainorverbtype + '.')
# ditransitive verb lexical type
  if ch.get('ditransitives') == 'yes':
    typedef = \
    'ditransitive-verb-lex := ditransitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comp1 , #comp2 >, \
         ARG-ST < [ ], \
                  #comp1 & \
                  [ LOCAL.CAT [ VAL [ SPR < >, \
                                      COMPS < > ] ] ], \
                  #comp2 & \
                  [ LOCAL.CAT [ VAL [ SPR < >, \
                                      COMPS < > ] ] ]  > ].'
    mylang.add(typedef)
    if refl:
      mylang.add('ditransitive-verb-lex := non-refl-verb-lex.')
    else:
      mylang.add('ditransitive-verb-lex := ' + mainorverbtype + '.')
 # clausal complement verb
  if ch.get('emb-clause-2nd-verb') == 'yes':
    typedef = \
    's-comp-2nd-arg-verb-lex := ' + mainorverbtype + \
                                 '& clausal-second-arg-trans-lex-item & \
    [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
      ARG-ST < [ ], \
                #comps & \
               [ LOCAL [ CAT [ VAL [ SUBJ < >, \
				     COMPS < >, \
				     SPR < >, \
				     SPEC < > ], \
			       HEAD comp ], \
			 CONT.HOOK.INDEX.SF prop-or-ques ] ] > ].'
    mylang.add(typedef)

  if ch.get('subj-control-verb') == 'yes':
    typedef = \
    'subj-contr-transitive-verb-lex := basic-verb-lex.' 
    mylang.add(typedef)
    if ch.get('vc-analysis') == 'basic':
      mylang.add('subj-contr-transitive-verb-lex := arg-comp-aux & \
       trans-first-arg-control-lex-item.')
    elif ch.get('vc-analysis') == 'aux-rule':
      mylang.add('auxrule-first-arg-control-lex-item := basic-one-arg & \
       [ ARG-ST <  [ LOCAL [ CONT.HOOK [ XARG #ind, \
	                 		 LTOP #larg ] ] ] >, \
         SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg, \
	          			      LARG #larg ] !>, \
	          LKEYS.KEYREL [ ARG1 #ind, \
		                 ARG2 #harg ] ] ].')
      mylang.add('subj-contr-transitive-verb-lex := \
                    auxrule-first-arg-control-lex-item & one-comp-aux.')


  if ch.get('obj-raising') == 'yes':
    if refl:
      mylang.add('obj-raising-verb-lex := non-refl-verb-lex.')
    else:
      mylang.add('obj-raising-verb-lex := ' + mainorverbtype + '.')
    typedef = \
    'obj-raising-verb-lex := distrans-second-arg-raising-lex-item & \
     [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < #subj >, \
			      SPR < >, \
			      SPEC < > ], \
       ARG-ST < #subj & [ LOCAL.CAT [ VAL.SPR < > ] ], [ ], [ ] > ].'

    mylang.add(typedef)
    if ch.get('vc-analysis') == 'aux-rule':
      comps_struc = \
      ' [ SYNSEM.LOCAL.CAT.VAL.COMPS < #obj, #vcomp >, \
          ARG-ST < [ ], #obj & [ LOCAL.CAT.VAL.SPR < > ], \
                   #vcomp & [ LOCAL.CAT.VAL.SUBJ < [ ] > ] > ].' 
    else:
      comps_struc = \
      ' [ SYNSEM.LOCAL.CAT.VAL.COMPS < #obj, #vcomp . #comps >, \
          ARG-ST < [ ], #obj & [ LOCAL.CAT.VAL.SPR < > ], \
                   #vcomp & [ LOCAL.CAT.VAL [ SUBJ < [ ] >, \
                                              COMPS #comps ]] > ].' 

    mylang.add('obj-raising-verb-lex := ' + comps_struc)


  case.customize_verb_case(mylang, ch)

  # Add constraints to choices to create lex rules for bipartite stems
  customize_bipartite_stems(ch)

  # Lexical entries
  lexicon.add_literal(';;; Verbs')

  # Now create the lexical entries for all the defined verb types
  cases = case.case_names(ch)

  # Adding possibility to set default for OPT when defined

####allowing for default in object-dropping
  o_drop_default = ''
  if ch.get('obj-drop') == 'obj-drop-lex':
    o_drop_default = ch.get('obj-drop-default')

  for verb in ch.get('verb',[]):
    
    opt_heads = []
    for feat in verb.get('feat',[]):
      if feat.get('name') == 'OPT':
        opt_heads.append[feat.get('head')]

    name = get_name(verb)
    val = verb.get('valence')
    aux_s = verb.get('aux-select')

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
#c can point to transitive or ditransitive
      if len(c) == 2:
        if c[1] == 'scomp':
          a_case = case.canon_to_abbr(c[0], cases)
          tivity = a_case + '-' + c[1] + '-trans'
        elif c[1] == 'inf' or c[1] == 'zuinf':
          a_case = case.canon_to_abbr(c[0], cases)
          tivity = a_case + '-' + c[1]
          if verb.get('control') == 'subj':
            tivity += '-subj-contr'
          tivity += '-trans'
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          o_case = case.canon_to_abbr(c[1], cases)
          tivity = a_case + '-' + o_case + '-trans'
      elif len(c) == 3:
        if c[2] == 'inf':
          a_case = case.canon_to_abbr(c[0], cases)
          o_case = case.canon_to_abbr(c[1], cases)
          tivity = a_case + '-' + o_case + '-' + c[2]
          if verb.get('raising') == 'obj':
            tivity += '-obj-raising'
          tivity += '-ditrans'
        elif c[1] == 'refl':
          a_case = case.canon_to_abbr(c[0], cases)
          o_case = case.canon_to_abbr(c[1], cases)
          tivity = a_case + '-' + o_case + '-' + c[2]
          tivity += '-trans'
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          b_case = case.canon_to_abbr(c[1], cases)
          o_case = case.canon_to_abbr(c[2], cases)
          tivity = a_case + '-' + b_case + '-' + o_case + '-ditrans'
    else:
      s_case = case.canon_to_abbr(val, cases)
      tivity = s_case + '-intrans'

    stype = dir_inv + tivity + 'itive-verb-lex'
    vtype = name + '-verb-lex'

    mylang.add(vtype + ' := ' + stype + '.')

    if aux_s:
      mylang.add(vtype + ' := ' + aux_s + '-only-verb-lex.')


    if o_drop_default:
      if val == 'trans' or '-' in val:
        if not 'obj' in opt_heads:
          mylang.add(vtype + ' := \
                     [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT \
                                           ' + o_drop_default + ' ].')
        if len(c) == 3 and not 'obj2' in opt_heads:
          if not c[1] == 'refl':
            mylang.add(vtype + ' := \
                     [ ARG-ST < [ ], [ ], \
                                    [ OPT ' + o_drop_default + ' ] > ].')
          else:
            mylang.add(vtype + ' := \
                     [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ], \
                                      [ OPT ' + o_drop_default + ' ] > ].')  
###
# scomp-spec
# does the verb take questions, affirmatives or both as a complement?
#
    sf = verb.get('compl-sf')
    if sf:
      mylang.add(vtype + ' := \
          [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.SF ' \
           + sf + ' ].')

    features.customize_feature_values(mylang, ch, hierarchies, verb, vtype, 'verb', None, cases)

    stems = verb.get('stem', [])
    stems.extend(verb.get('bistem', []))

    for stem in stems:
      orth = stem.get('orth')
      pred = stem.get('pred')
      id = stem.get('name')
      typedef = \
        TDLencode(id) + ' := ' + vtype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

# Returns the verb type for lexical/main verbs.
def main_or_verb(ch):
  if ch.get('has-aux') == 'yes':
    return 'main-verb-lex'
  else:
    return 'verb-lex'


def customize_copula(mylang, ch, lexicon, hierarchies):
  #add copula supertype
  #copula
  if ch.get('has-cop') == 'yes':
    if ch.get('rel-clause') == 'yes':
      mylang.add('+nvjrp :+ [ PRD bool ].')
    else:
      mylang.add('+njrp :+ [ PRD bool ].')
  
#TO DO: more stable mechanism to identify possible heads
    heads = ch.get('cop_pred')
    v = ''
    for h in heads:
      v += h.get('value')+ '-'

    values = v.split('-')

    head = ''
    if len(values) > 2:
      head = '+'
      if 'adj' in values:
        head += 'j'
      if 'adv' in values:
        head += 'r'
      if 'adp' in values:
        head += 'p'
    else:
      head = values[0]

    if ch.get('cop_loc'):
      type_name = 'general-copula-verb-lex'
      mylang.add('copula-verb-lex := ' + type_name + ' & trans-first-arg-raising-lex-item-2.')
      mylang.add('loc-copula-verb-lex := ' + type_name + \
                    ' & trans-first-arg-raising-lex-item & norm-sem-lex-item & \
  [ SYNSEM [ LOCAL [ CAT.VAL.COMPS.FIRST.LOCAL [ CAT.HEAD +rp, \
						 CONT.HOOK [ INDEX #ind, \
							     XARG #xarg ] ], \
		     CONT.HOOK [ INDEX #ind ] ], \
	     LKEYS.KEYREL.ARG1 #xarg ] ].')
    else:
      type_name = 'copula-verb-lex'
      mylang.add('copula-verb-lex := trans-first-arg-raising-lex-item-2.')
      
    mylang.add('copula-verb-lex := \
                 [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD +jp ].')


    mylang.add(type_name + ' := verb-lex & \
             [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #subj >, \
                                        COMPS < #comps >, \
                                        SPR < >, \
                                        SPEC < > ], \
                              CONT.HOOK.XARG #xarg ], \
               ARG-ST < #subj & \
                        [ LOCAL [ CONT.HOOK.INDEX #xarg, \
                                  CAT [ VAL [ SPR < >, \
                                              COMPS < > ],\
                                        HEAD noun ] ] ], \
                        #comps & \
                        [ LOCAL.CAT [ VAL [ COMPS < > ], \
                                      HEAD ' + head + ' & [ PRD + ] ] ] > ].')
    
####TEMP HACK ADDING MC-CONSTRAINT TO GENERAL COP, GERMANIC SPECIFIC
    if ch.get('verb-cluster') == 'yes' and ch.get('word-order') == 'v2':
      mylang.add(type_name + ' := [ SYNSEM.LOCAL.CAT.MC na-or-- ].' )

    if ch.get('has-aux') == 'yes':
      mylang.add(type_name + ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')


####allowing for default in object-dropping

  o_drop_default = ''
  if ch.get('obj-drop') == 'obj-drop-lex':
    o_drop_default = ch.get('obj-drop-default')

  for cop in ch.get('cop',[]):
    name = cop.get('name','')
    userstypename = name + '-copula-lex'
    subj = cop.get('subj','')
    subjc = cop.get('subj_case','') #TODO: verify _-delimited key
    aux_s = cop.get('aux-select')
    cases = case.case_names(ch)
    subjcase = case.canon_to_abbr(subjc, cases)


    if not cop.get('loc') == 'on':
      mylang.add(userstypename + ' := copula-verb-lex.')
    else:
      mylang.add(userstypename + ' := loc-copula-verb-lex.')
    if subjcase:
      mylang.add(userstypename + ' := \
                       [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + subjcase + ' ].')

##object drop on copula just doesn't make sense, prohibiting it for noew
    o_drop_default = ''
    if ch.get('obj-drop') == 'obj-drop-lex':
      mylang.add(userstypename + ' := \
                       [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT - ].')
    
    if aux_s:
      mylang.add(userstypename + ' := ' + aux_s + '-only-verb-lex.')

    features.customize_feature_values(mylang, ch, hierarchies, cop, userstypename, 'cop')
    add_copula_to_lexicon(userstypename, cop, lexicon)

def add_copula_to_lexicon(userstypename, cop, lexicon):
  for stem in cop.get('stem',[]):
    orth = stem.get('orth')
    id = stem.get('name')
    typedef = TDLencode(id) + ' := ' + userstypename + ' & \
                       [ STEM < "' + orth + '" > ].'
    lexicon.add(typedef)
  
    if cop.get('loc') == 'on':
      pred = stem.get('pred')
      typedef = TDLencode(id) + \
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
      'determiner-lex := basic-determiner-lex & zero-arg-nonslash & \
          [ SYNSEM.LOCAL.CAT.VAL [ SPR < >, \
                                   COMPS < >, \
                                   SUBJ < > ]].'
    mylang.add(typedef)

    # also not to be used as modifiers
    mylang.add('determiner-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].')    
    
    if ch.get('rel-clause') == 'yes':
      create_rel_determiner(mylang)
      mylang.add('determiner-lex := non-rel-lex-item.')
  # Determiners
  if 'det' in ch:
    lexicon.add_literal(';;; Determiners')

  for det in ch.get('det',[]):
    name = get_name(det)
    wh = det.get('wh')    
    rel = det.get('rel')

    if wh == 'yes':
      stype = 'wh-determiner-lex'
    elif rel == 'yes':
      stype = 'rel-determiner-lex'
    else:
      stype = 'determiner-lex'
    dtype = name + '-determiner-lex'

    mylang.add(dtype + ' := ' + stype + '.')

    features.customize_feature_values(mylang, ch, hierarchies, det, dtype, 'det')

    for stem in det.get('stem',[]):
      orth = stem.get('orth')
      pred = stem.get('pred')
      id = stem.get('name')
      typedef = \
        TDLencode(id) + ' := ' + dtype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)


def create_rel_determiner(mylang):
  comment = '''relative determiners cannot inherit from basic-determiner-lex, because they have more than one relation. Basic-rel-determiner-lex has an underspecified RELS value (suboptimal structure of hierarchy)'''
  basic_rel_det = \
   '''basic-rel-determiner-lex := norm-hook-lex-item &
  [ SYNSEM [ LOCAL [ CAT [ HEAD det,
			   VAL.SPEC.FIRST.LOCAL.CONT.HOOK [ INDEX #ind,
							    LTOP #larg ]],
		     CONT [ HCONS <! qeq &
				   [ HARG #harg,
				     LARG #larg ] !> ] ],
	     LKEYS.KEYREL quant-relation &
		   [ ARG0 #ind,
		     RSTR #harg ] ] ].'''
  mylang.add(basic_rel_det, comment)
  
#most definitely German specific, can be generalized by allowing choices
#to define pred value of rel-determiner-lex
  rel_det = \
   '''rel-determiner-lex := basic-rel-determiner-lex & zero-arg-nonslash & 
                                                          non-wh-lex-item &
       [ SYNSEM [ LOCAL [ CAT.VAL [ SUBJ < >,
                                    COMPS < >,
                                    SPR < > ],
		          CONT [ HOOK [ INDEX #index ],
			         RELS <! [ ], [ LBL #lbl,
					        PRED "_von_v_mod_rel",
					        ARG0 event,
					        ARG1 #index,
					        ARG2 #relarg ] !> ] ],
	          NON-LOCAL.REL 1-dlist & [ LIST < [ LTOP #lbl,
		     				     INDEX #relarg ] > ] ] ].'''
  mylang.add(rel_det)


def customize_adjectives(mylang, ch, lexicon):

   # Lexical type for adjectives, if the language has any:
  if ch.get('has-adj') == 'yes':
    comment = \
      ';;; Adjectives\n' + \
      ';;; First: inheriting all from matrix.'
    mylang.add_literal(comment)
    mylang.add('scopal-mod-adj-lex := basic-scopal-mod-adj-lex.')
    mylang.add('int-mod-adj-lex := basic-int-mod-adj-lex.')

   #2011-11-07 Fixing semantics of adjectives
    mylang.add('int-mod-adj-lex := [ SYNSEM [ LOCAL.CONT.HOOK.XARG #xarg, \
                                              LKEYS.KEYREL.ARG1 #xarg ] ].')


    if ch.get('verb-cluster') == 'yes':
      mylang.add('scopal-mod-adj-lex := no-cluster-lex-item.')
      mylang.add('int-mod-adj-lex := no-cluster-lex-item.')   


    if ch.get('strength-marking') == 'double':
      mylang.add('+njdo :+ [ STRONG bool ].', section='addenda')
    elif ch.get('strength-marking') == 'triple':
      mylang.add('+njdo :+ [ STRONG luk ].', section='addenda')
  
  ###Agreement properties
    case_agr = False
    strength_agr = False
    for agr in ch.get('adjagr'):
      if agr.get('feat') == 'case':
        case_agr = True
      elif agr.get('feat') == 'strength':
        strength_agr = True

  ####changes if second language learning
  ####agreement is done by rules not lexical items
    ll = False
    if ch.get('2ndll') == 'on':
      for myll in ch.get('ll'):
        if myll.get('phen') == 'adj':
          ll = True
  #depending on agreement properties, CASE is a feature of nouns or of nouns
  #and adjectives
    if case_agr:
      mylang.add('+nj :+ [ CASE case].', section='addenda')
    elif ch.get('case-marking') != 'none':
        mylang.add('noun :+ [ CASE case ].', section='addenda')
      
 # Adjectives
  if 'adj' in ch:
    lexicon.add_literal(';;; Adjectives')

  for adj in ch.get('adj',[]):
    name = get_name(adj)

    stype = '-mod-adj-lex'
    if adj.get('kind') == 'int':
      stype = 'int' + stype
    else:
      stype = 'scopal' + stype
    atype = name + '-adjective-lex'
    
    mylang.add(atype + ' := ' + stype + ' & \
      [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT [ HEAD noun, \
                                                  VAL.SPR <[ ]> ] ] > ].')

    arg_str = adj.get('arg-str')
    val = ''
    if arg_str == 'none':
      val = 'VAL [ SUBJ < >, \
                   COMPS < >, \
                   SPR < >, \
                   SPEC < > ]'

    if val:
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.' + val + ' ].')
 

    if case_agr and not ll:
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.HEAD [ CASE #case, \
                            MOD < [ LOCAL.CAT.HEAD.CASE #case ] > ] ].')

    if strength_agr and not ll:
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.HEAD [ STRONG #strength, \
                            MOD < [ LOCAL.CAT.HEAD.STRONG #strength ] > ] ].')

    for stem in adj.get('stem',[]):
      orth = stem.get('orth')
      pred = stem.get('pred')
      id = stem.get('name')
      typedef = \
        TDLencode(id) + ' := ' + atype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

def customize_adverbs(mylang, ch, lexicon):

   # Lexical type for adverbs, if the language has any:
  if ch.get('has-adv') == 'yes':
    comment = \
      ';;; Adverbs\n' + \
      ';;; First attempt: inheriting all from matrix.'
    mylang.add_literal(comment)
    mylang.add('scopal-adverb-lex := basic-scopal-adverb-lex.')
    mylang.add('int-adverb-lex := basic-int-adverb-lex & \
                [ SYNSEM [ LOCAL.CONT.HOOK.XARG #arg1, \
                           LKEYS.KEYREL.ARG1 #arg1 ] ].')
    if ch.get('adv-argst') == 'yes':
      mylang.add('int-mod-with-one-arg-lex := intersective-mod-lex & \
                    [ SYNSEM.LKEYS.KEYREL.ARG2 #arg2, \
                      ARGS < [ LOCAL.CONT.HOOK.INDEX #arg2 ] > ].')

    if ch.get('verb-cluster') == 'yes':
      mylang.add('scopal-adverb-lex := no-cluster-lex-item.')
      mylang.add('int-adverb-lex := no-cluster-lex-item.')   

 # Adverbs
  if 'adv' in ch:
    lexicon.add_literal(';;; Adverbs')

  for adv in ch.get('adv',[]):
    name = get_name(adv)
    kind = adv.get('kind')
    arg_str = adv.get('arg-str')
    stype = '-adverb-lex'
    if kind == 'int':
      stype = 'int' + stype
    elif kind == 'wh':
      stype = 'wh' + stype
    else:
      stype = 'scopal' + stype
    atype = name + '-adverb-lex'

###default is adverbs modify verb
    mod_head = 'verb'
    mod_val = ''
    modh = adv.get('mod')
    order = adv.get('order')
###simplification for now: if more than one thing modified, all with
###same valency at point of modification    
    if '-' in modh:
      mod_head = '+'
      if 'np' in modh:
        mod_head += 'n'
      if 's' in modh:
        mod_head += 'v'
      if 'pp' in modh:
        mod_head += 'p'
    elif modh == 'pp':
      mod_head = 'adp'
    elif modh == 'det':
      mod_head = 'det'
    elif modh == 'ad':    
      mod_head = '+jr'
    elif modh == 's':    
      mod_head = 's'

    if not mod_head == 'verb':
      if mod_head == 'det':
         mod_val = '[ SUBJ < >, \
                      COMPS < >, \
                      SPR < >, \
                      SPEC < [ ] > ]'
      else:
        mod_val = '[ SUBJ < >, \
                     COMPS < >, \
                     SPR < >, \
                     SPEC < > ]'
      if mod_head == 's':
        mod_head = 'verb'
    if not arg_str == 'scomp':
      mylang.add(atype + ' := ' + stype + ' & \
        [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.HEAD ' + mod_head + ' ] > ].')
    else:
      mylang.add(atype + ' := \
        [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.HEAD ' + mod_head + ' ] > ].')
    
    if mod_val:
      mylang.add(atype + ' :=  \
      [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL ' + mod_val + ' ] > ].')

    if order == 'pre':
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
    elif order == 'post':
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')

    val = ''
    if arg_str == 'none':
      val = 'VAL [ SUBJ < >, \
                   COMPS < >, \
                   SPR < >, \
                   SPEC < > ]'
#####Germanic specific, should be generalized
    elif arg_str == 'zuinf':
      val = 'VAL [ SUBJ < >, \
                   COMPS < [ LOCAL.CAT [ HEAD verb & [ FORM zuinf ], \
                                         VAL [ SUBJ < [ ] >, \
                                               COMPS < >, \
                                               SPR < >, \
                                               SPEC < > ] ], \
                             OPT - ] >, \
                   SPR < >, \
                   SPEC < > ]'
# [ OPT + ] value probably not universally true
    elif arg_str == 'scomp':
      val = 'VAL [ SUBJ < >, \
                   COMPS < [ OPT - ] >, \
                   SPR < >, \
                   SPEC < > ]'
      if kind == 'scop':
        mylang.add('clausal-arg-mod-lex-item := basic-one-arg & \
                    [ ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] >, \
                      SYNSEM [ LOCAL.CONT.HCONS <! [ ], qeq & [ HARG #harg, \
                                                           LARG #larg ] !>, \
                               LKEYS.KEYREL [ ARG2 #harg ] ] ].')
        mylang.add('scopal-mod-with-cl-arg-lex := clausal-arg-mod-lex-item & \
                [ SYNSEM [ LOCAL [ CAT.HEAD.MOD < [ LOCAL scopal-mod & \
					    [ CONT.HOOK.LTOP #larg ]] >, \
		                   CONT [ HCONS <! qeq & \
				                    [ HARG #harg, \
				                      LARG #larg ], [ ] !> ] ], \
     	                    LKEYS.KEYREL.ARG1 #harg ]].')
      else:
        mylang.add('clausal-arg-int-mod-lex-item := basic-one-arg & \
                    [ ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] >, \
                      SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg, \
                                                           LARG #larg ] !>, \
                               LKEYS.KEYREL [ ARG2 #harg ] ] ].')
       
        mylang.add('int-mod-clausal-arg-lex := clausal-arg-int-mod-lex-item & \
                     [ SYNSEM [ LOCAL.CAT.HEAD.MOD < [ LOCAL intersective-mod & \
					             [ CONT.HOOK.INDEX #ind ]] >, \
                                LKEYS.KEYREL.ARG1 #ind ] ].')
    if val:
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.' + val + ' ].')
####Germanic specific  
    if arg_str == 'zuinf':
      mylang.add(atype + ' := int-mod-with-one-arg-lex & \
        [ SYNSEM.LOCAL.CAT [ HEAD.MOD < [ LOCAL.CONT.HOOK.XARG #xarg ] >, \
                             VAL.COMPS < #comp & \
                                        [ LOCAL.CONT.HOOK.XARG #xarg ] > ], \
          ARGS < #comp > ] ].')   
    elif arg_str == 'scomp':
      mylang.add(atype + ' := scopal-mod-with-cl-arg-lex & basic-adverb-lex & \
                  [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comp >, \
                    ARG-ST < #comp & \
                           [ LOCAL.CAT [ HEAD verb & [ FORM finite, \
                                                       INV - ], \
                                         MC -, \
                                         VAL [ SUBJ <  >, \
                                               COMPS < >, \
                                               SPR < >, \
                                               SPEC < > ] ] ]  > ].')
    for stem in adv.get('stem',[]):
      orth = stem.get('orth')
      pred = stem.get('pred')
      id = stem.get('name')
      typedef = \
        TDLencode(id) + ' := ' + atype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)


def customize_adpositions(ch, mylang, lexicon):
  cases = case.case_names(ch)
  if ch.get('has-adp'):
    s_name = create_adposition_supertypes(ch, mylang)
    
    default = ch.get('adp-default')
    exception = ch.get('adp-default-exception')  
    for adp in ch.get('adp',[]):
      set_order = False
      sname = s_name
      incl = ''
      if adp.get('incl') == 'yes':
        incl = True
      if incl:
        sname = 'compl-incl-int-adp-lex-item'
      name = adp.get('name') + '-adp-lex-item'
      kind = adp.get('kind')
      form = adp.get('form')
      if kind == 'mod':
        mod = adp.get('mod')
        if kind in exception and mod in exception:
          set_order = True
        if 'incl' in sname:
          set_order = False
      #add basic frame
        if set_order:
          hco = adp.get('hc_order')
          if hco:
            name = name.replace('adp',hco)
            sname = sname.replace('adp',hco)
          else:
            name = name.replace('adp',default)
            sname = sname.replace('adp',default)

        mylang.add(name + ' := ' + mod + '-' + sname + '.')
        order = adp.get('order')
        if order == 'post':
          mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
        elif order == 'pre':
          mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
        if ch.get('has-cop') == 'yes':
          mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.HEAD.PRD - ].')
      elif kind == 'prd':
        mylang.add(name + ' := prd-' + sname + '& \
          [ SYNSEM.LOCAL.CAT [ HEAD.PRD +, \
                               VC - ] ].' )
     
      for feat in adp.get('feat',[]):
        if feat.get('name') == 'case':
          constr = 'LOCAL.CAT.HEAD.CASE '
          value = case.canon_to_abbr(feat.get('value'), cases)
          value += ' ] >'
        if feat.get('head') == 'comp':
          path = 'SYNSEM.LOCAL.CAT.VAL.COMPS < [ '
          
        typedef = name + ' := [ ' + path + constr + value + ' ].'
        mylang.add(typedef)
      if form:
        sf = ''
        if ch.get('nachfeld') == 'yes' and 'pform' in ch.get('nf-forms'):
          sf += 'nf-'
        sf += 'form'
        mylang.add('pform := ' + sf + '.',section='features')
        mylang.add(form + ' := pform.',section='features')
        mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')
       

      for stem in adp.get('stem',[]):
        orth = stem.get('orth')
        pred = stem.get('pred')
        id = stem.get('name')
        typedef = \
            TDLencode(id) + ' := ' + name + ' & \
                   [ STEM < "' + orth + '" >, \
                     SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
        lexicon.add(typedef)



def create_adposition_supertypes(ch, mylang):
###probably not universal
  if ch.get('obj-drop'):
    mylang.add('basic-adposition-lex :+ \
                  [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].',section='addenda')
  s_name = 'int-adp-lex-item'
  comp = '[ LOCAL.CAT [ HEAD noun, \
                        VAL [ SPR < >, \
                              COMPS < >, \
                              SUBJ < >,\
                              SPEC < > ] ] ] '
  mylang.add(s_name + ' := basic-int-mod-adposition-lex & \
                [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                         COMPS < ' + comp + ' > \
                                         SPR < >, \
                                         SPEC < > ] ].')
  
###adds ARG2, co-indexed with comp  
  mylang.add(s_name + ' := [ SYNSEM [ LKEYS.KEYREL.ARG2 #arg2, \
                  LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX #arg2 ] ].')

  create_adp_cross_classification(ch, mylang, s_name)
###FOR CONTRACTIONS OF PREPOSITION AND PRONOMINAL COMPLEMENT
###e.g. German "darauf", Dutch "daarop" (on that)
  if ch.get('comp-incl-adp') == 'yes':
    comment = 'introducing adpositions that contain their pronominal \
               complement. They cannot inherit from basic-adp-lex which is a \
               one-rel-lex-item, because it needs to introduce the semantics \
               of its complement.'
    sname2 = 'compl-incl-int-adp-lex-item' 
    mylang.add(sname2 + ' := norm-hook-lex-item & no-cluster-lex-item & \
                                                 intersective-mod-lex & \
                  [ SYNSEM.LOCAL.CAT [ HEAD adp, \
                                       VAL [ SUBJ < >, \
                                             SPR < >, \
                                             SPEC < >, \
                                             COMPS < > ] ] ].', comment)
    mylang.add(sname2 + ' := \
             [ SYNSEM [ LOCAL.CONT.RELS <! [ ARG2 #arg2 ], \
                                           [ PRED "_pronoun_n_rel", \
                                             LBL #lbl, \
                                             ARG0 #arg2 & index ], \
                                             quant-relation & \
                                           [ PRED "_exist_q_rel", \
                                             ARG0 #arg2, \
                                             RSTR #lbl ] !>, \
                        NON-LOCAL.QUE 0-dlist, \
                        LKEYS.KEYREL event-relation ] ].') 

    create_adp_cross_classification(ch, mylang, sname2)


  return s_name


def create_adp_cross_classification(ch, mylang, sname):
  
  if ch.get('adp-order') == 'both':
    mylang.add('prep-lex-item := basic-adposition-lex & \
              [ SYNSEM.LOCAL.CAT.HEADFINAL - ].')
    mylang.add('postp-lex-item := basic-adposition-lex & \
              [ SYNSEM.LOCAL.CAT.HEADFINAL + ].')
  default = ch.get('adp-default')
  exception = ch.get('adp-default-exception')  

  if ch.get('verb-cluster') == 'yes':
    mylang.add(sname + ' := no-cluster-lex-item.')

  for spadp in ch.get('sup_adp',[]):

    if spadp.get('kind') == 'mod':
      for mod in spadp.get('mod',[]):
        head = mod.get('head')
        both_orders = False
        if 'mod' in exception and head in exception:
          both_orders = True
        type_n = head + '-' + sname

        mylang.add(type_n + ' := ' + sname + ' & \
               [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.HEAD '+ head +' ].') 
        
        for feat in mod.get('feat',[]):
          if feat.get('name') == 'light':
            constr = 'LIGHT ' + feat.get('value')
          mylang.add(type_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.' + constr + ' ].')
 
        if not 'incl' in type_n:
          if both_orders:
            type_npre = type_n.replace('adp', 'prep')
            type_npost = type_n.replace('adp', 'postp')
            mylang.add(type_npre + ' := prep-lex-item & ' + type_n + '.')
            mylang.add(type_npost + ' := postp-lex-item & ' + type_n + '.')
          elif default:
            mylang.add(type_n + ' := ' + default + '-lex-item.')
 
    elif spadp.get('kind') == 'prd':
      type_n = 'prd-' + sname
      mylang.add(type_n + ' := ' + sname + ' & \
               [ SYNSEM [ LOCAL.CONT.HOOK.XARG #arg1, \
                          LKEYS.KEYREL.ARG1 #arg1 ] ].')
      if not 'incl' in type_n:
        if 'prd' in exception:
          type_npre = type_n.replace('adp', 'prep')
          type_npost = type_n.replace('adp', 'postp')
          mylang.add(type_npre + ' := prep-lex-item & ' + type_n + '.')
          mylang.add(type_npost + ' := postp-lex-item & ' + type_n + '.')
        elif default:
          mylang.add(type_n + ' := ' + default + '-lex-item.')

       
def customize_complementizers(ch, mylang, lexicon):
  if ch.get('has-compl') == 'yes':
  ###create section in lexicon
    lexicon.add_literal(';;; Complementizers')
  ###add general supertype:
    mylang.add('complementizer-lex-item := raise-sem-lex-item & \
             basic-one-arg & \
       [ SYNSEM.LOCAL.CAT [ HEAD comp, \
		      VAL [ SUBJ < >, \
			    COMPS < #comp & \
                               [ LOCAL.CAT [ MC -, \
                                             VAL [ SUBJ < >,\
                                                   COMPS < >,\
                                                   SPR < >,\
                                                   SPEC < > ],\
					     HEAD verb & [ FORM finite, \
                                                           INV - ] ], \
                                 OPT -, \
                                 NON-LOCAL [ REL 0-dlist & [ LIST < > ], \
                                             QUE 0-dlist ] ] >,\
			    SPR < >, \
			    SPEC < > ] ], \
          ARG-ST < #comp > ].')
    for compl in ch.get('compl',[]):
      sf = compl.get('sf')
      type = compl.get('name')
      mylang.add(type + ' := complementizer-lex-item & \
         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ' + sf + ' ].')
      
      for stem in compl.get('stem'):
        orth = stem.get('orth')
        typedef = \
            TDLencode(orth) + ' := ' + type + ' & \
                   [ STEM < "' + orth + '" > ].'
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


  refl = ''
  if ch.get('reflexives') == 'yes':
    refl = True

  compound = ''
  if ch.get('compound-nouns') == 'yes':
    compound = True

  rel_pn = ''
  if ch.get('rel-clause') == 'yes':
    rel_pn = True
  # Playing fast and loose with the meaning of OPT on SPR.  Using
  # OPT - to mean obligatory (as usual), OPT + to mean impossible (that's
  # weird), and leaving OPT unspecified for truly optional.  Hoping
  # this will work at least for LSA111 lab.

  # ERB 2006-11-28 Update: To make that weird use of OPT work, the
  # head-spec rule has to require [OPT -] on its non-head daughter.
  # Adding that just in case we add the no-spr-noun-lex type.

  if refl:
    stype1 = 'non-reflexive-noun-lex'
  else:
    stype1 = 'basic-noun-lex'
  typedef = \
    'general-noun-lex := basic-noun-lex & basic-one-arg & no-hcons-lex-item & \
       [ SYNSEM.LOCAL [ CAT.VAL [ COMPS < >, \
                                  SUBJ < >, \
                                  SPEC < > ] ] ].'
  mylang.add(typedef)
   
  # ASF 2011-12-21 (Germanic only) creating supertype for nouns with scomp
  # non-Germanic should (at least) not have 'no-cluster-lex-item' and probably
  # not have non-wh-lex-item
  # Not needed: these types exist in matrix.tdl
  if ch.get('noun-argst') == 'yes':
    typedef1 = \
      'noun-one-arg-lex := ' + stype1 + ' & no-cluster-lex-item & \
                         non-wh-or-rel-lex-item & spr-plus-one-arg-lex-item & \
        [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].'
    typedef2 = \
      'noun-clausal-arg-lex := ' + stype1 + ' & no-cluster-lex-item & \
                      non-wh-or-rel-lex-item & spr-plus-clausal-arg-lex-item & \
        [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].'
    mylang.add(typedef1)
    mylang.add(typedef2)  

  # Assuming that most nouns typically do not modify
  # until compounds have been added
 
  mylang.add('noun-lex := general-noun-lex & \
         [ SYNSEM.LOCAL.CAT [ HEAD.MOD < >, \
                              VAL.SPR < #spr & [ LOCAL.CAT.HEAD det ] > ], \
           ARG-ST < #spr > ].')

  if compound:
    mylang.add('compound-local := local.')
    mylang.add('compound-allowing-noun-lex := basic-noun-lex & \
                 [ SYNSEM.LOCAL compound-local ].')
    mylang.add('compound-noun-lex := compound-allowing-noun-lex & \
                 general-noun-lex & \
               [ SYNSEM.LOCAL.CAT [ HEAD.MOD < >, \
                                  VAL.SPR < #spr & [ LOCAL.CAT.HEAD det ] > ], \
                 ARG-ST < #spr > ].')
  ###adding type for explitive pronouns
  if ch.get('explitives') == 'yes' or rel_pn:
    comment = '''Explicits, relative pronouns and reflexives cannot inherit from basic-noun-phrase: noun-relation introduces semantics and ARG0.'''
    typedef = 'non-sem-noun-lex := no-hcons-lex-item & \
                 [ SYNSEM [ LOCAL [ CONT [ RELS <! !>, \
                                           HCONS <! !> ], \
                                    CAT [ HEAD noun & [ MOD < > ], \
                                          VAL [ SUBJ < >, \
                                                COMPS < >, \
                                                SPR < >, \
			                        SPEC < > ] ] ], \
	                    NON-LOCAL.QUE 0-dlist ] ].'
    mylang.add(typedef, comment)
    if ch.get('v2-analysis') == 'mc':
      mylang.add('non-sem-noun-lex := [ SYNSEM.NON-LOCAL.SLASH 0-dlist & \
                                                               [ LIST < > ] ].')
    if ch.get('explitives') == 'yes':
      expltype = \
           'expl-noun-lex := non-sem-noun-lex & \
             [ SYNSEM [ LOCAL [ CONT.HOOK.INDEX expl-ind, \
                                CAT.HEAD.CASE nom ], \
                        NON-LOCAL.REL 0-dlist ] ].' 
      mylang.add(expltype)
    if rel_pn:
      relprn = \
      '''rel-pronoun-lex := non-sem-noun-lex &
           [ SYNSEM [ LOCAL rel-local & [ CAT.HEAD.PRD -,
		     CONT.HOOK [ LTOP #hand,
				 INDEX #ind,
				 XARG #xarg ] ],
             NON-LOCAL [ REL 1-dlist & [ LIST < [ LTOP #hand,
						  INDEX #ind,
						  XARG #xarg ] > ] ] ] ]. '''
      mylang.add(relprn)
      if refl:
        mylang.add('rel-local := rel-non-refl-local.',section='features')
      else:
        mylang.add('rel-local := local.',section='features')
    if refl:
      refl_prn = \
       '''reflexive-noun-lex := non-sem-noun-lex &
            [ SYNSEM.LOCAL refl-local ].'''
      mylang.add(refl_prn)
      if ch.get('wh-questions') == 'yes':
        mylang.add('reflexive-noun-lex := non-wh-or-rel-lex-item.')

      mylang.add('refl-local := local.', 'Distinguishing reflexive nouns from non-reflexive nouns.', section='features')
      if ch.get('rel-clause') == 'yes':
        mylang.add('rel-non-refl-local := local.', section='features')
        mylang.add('non-refl-local := rel-non-refl-local.') 
      else:
        mylang.add('non-refl-local := local.', section='features')   
      mylang.add('non-reflexive-noun-lex := basic-noun-lex & \
                              [ SYNSEM.LOCAL non-refl-local ].')
      mylang.add('noun-lex := non-reflexive-noun-lex.')
  mylang.add('non-rel-lex-item := lex-item & \
                                        [ SYNSEM.NON-LOCAL.REL 0-dlist ].')
  if ch.get('wh-questions') == 'yes':
    mylang.add('non-wh-or-rel-lex-item := non-rel-lex-item & non-wh-lex-item.')
  
  if ch.get('mod-noun') == 'yes':
    mylang.add('mod-noun-lex := general-noun-lex & \
                [ SYNSEM.LOCAL.CAT [ HEAD.MOD < [ ] >, \
                                  VAL.SPR < #spr & [ LOCAL.CAT.HEAD det ] > ], \
                  ARG-ST < #spr > ].')
    if refl:
      mylang.add('mod-noun-lex := non-reflexive-noun-lex.')

  if ch.get('verb-cluster') == 'yes':
    mylang.add('general-noun-lex := no-cluster-lex-item.')

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
      if ch.get('wh-det') == 'on':
        mylang.add('no-spr-noun-lex := non-wh-or-rel-lex-item.')

  if seen['imp'] and ch.get('has-dets') == 'yes':
    mylang.add(
      'head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.OPT - ].',
      'Nouns which cannot take specifiers mark their SPR requirement\n' +
      'as OPT +.  Making the non-head daughter OPT - in this rule\n' +
      'keeps such nouns out.')

###Germanic change: if adjectives have case agreement, their heads
###bear case as well: case bearing head decided in adjective code
###if turning up...

  if ch.get('case-marking') != 'none' and ch.get('has-adj') != 'yes':
    if not ch.has_adp_case():
      mylang.add('noun :+ [ CASE case ].', section='addenda')

####German compound nouns: not all nouns can do this: most attach
####mostly compounds are written as one word
#  if ch.get('compound-nouns') == 'yes':
#    typedef = \
#     'compounding-noun-lex := norm-hook-lex-item & intersective-mod-lex & no-cluster-lex-item & \
#  [ SYNSEM [ LOCAL [ CAT [ HEAD noun & [ MOD < [ LOCAL.CAT.HEAD noun,\
#	                                         LIGHT + ] > ], \
#                           VAL [ SUBJ < >, \
#                                 SPR < #spr & [ LOCAL.CAT.HEAD det ] >, \
#                                 SPEC < >, \
#                                 COMPS < > ] ], \
#                     CONT.RELS <! [ ARG0 #arg2, ARG1 #arg1 ], event-relation & \
#                                                  [ PRED "_compound_n_rel", \
#                                                    LBL #lbl, \
#						    ARG1 #arg1, \
#                                        ARG2 #arg2 & index ], quant-relation & #\
#                                            [ PRED "_exist_q_rel", \
#                                              ARG0 #arg2, \
#                                               RSTR #lbl ] !> ], \
#             NON-LOCAL.QUE 0-dlist, \
#             LKEYS.KEYREL noun-relation ], \
#   ARG-ST < #spr & [ ] > ].'
#
#    mylang.add(typedef)
#    if refl:
#      mylang.add('compounding-noun-lex := [ SYNSEM.LOCAL non-refl-local ].')
  # Add the lexical entries
  lexicon.add_literal(';;; Nouns')

  for noun in ch.get('noun',[]):
    name = get_name(noun)
    det = noun.get('det')
    wh = noun.get('wh')
    rel = noun.get('rel-pn')
    expl = noun.get('expl')
    compound = noun.get('compound')
    pers_n = noun.get('pers-name')
    arg_st = ''
    if noun.get('arg-st'):
      arg_st = noun.get('arg-st')
    mod = ''
    if noun.get('mod'):
      mod = noun.get('mod')    
    n_refl = ''
    if noun.get('refl'):
      n_refl = noun.get('refl')  

    ntype = name + '-noun-lex'
    
##TO DO: allow wh-words to be rel pronouns as well

    if wh == 'yes':
      stype = 'wh-noun-lex'
    elif rel == 'yes':
      stype = 'rel-pronoun-lex'
    elif arg_st:
##2011-12-21 just s-comp for now, more to be added
##2012-12-22 also pps now. Assuming arguments on nouns are all optional
####TO DO: CREATE LIGHT VERB CONSTRUCTION FOR HABEN + SOME OF THESE NOUNS
####TO DO2: FIND OUT ABOUT SEMANTICS, WHY MUST ONE-ARG HAVE REF-IND?
      if arg_st == 'scomp' or arg_st == 'qcomp':
        stype = 'noun-clausal-arg-lex'
      else:
        stype = 'noun-clausal-arg-lex'
      if det == 'opt' or det == 'obl':
        mylang.add(ntype + ':= [ SYNSEM.LOCAL.CAT.VAL.SPR < #spr >, \
                               ARG-ST < #spr & [ LOCAL.CAT.HEAD det ], [ ]> ].')
###BROKEN: optional determiner doesn't allow any specifier
###FIXED, misunderstanding on working of 'OPT': should make comment for grammar
###in customization system
        if det == 'imp': 
          mylang.add(ntype + ':= [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].')
      mylang.add(ntype + ' := [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
                                   ARG-ST < [ ], \
                                           #comps & \
                                   [ LOCAL [ CAT [ VAL [ COMPS < >, \
                                                         SPR < >, \
                                                         SPEC < > ] ] ], \
	                            OPT + ] > ].')
      if not arg_st == 'zuinf':
        mylang.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CAT.VAL.SUBJ < > ] > ].')
      if 'comp' in arg_st:
        mylang.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CAT.HEAD comp ] > ].')
        if arg_st == 'scomp-prop':
           mylang.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CONT.HOOK.INDEX.SF prop ] > ].')
        elif arg_st == 'qcomp': 
           mylang.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CONT.HOOK.INDEX.SF ques, \
                            NON-LOCAL.SLASH 0-dlist ] > ].')
      elif arg_st == 'adp':
        mylang.add(ntype + ' := [ ARG-ST < [ ], \
                                [ LOCAL.CAT.HEAD adp & \
                                   [ MOD < [ LOCAL.CAT.HEAD noun ] > ] ] > ].')
      elif arg_st == 'zuinf':
        mylang.add(ntype + ' := [ ARG-ST < [ ], \
                                           [ LOCAL [ CAT [ HEAD verb & [ FORM zuinf ], \
                                                           VAL.SUBJ < [ ] > ], \
                                                     CONT.HOOK.XARG #xarg ] ] >, \
                                   SYNSEM.LOCAL.CONT.HOOK.XARG #xarg ].')

    elif mod:
####TO DO: 1. noun can be mod and have arg-st (at least theoretically possible)
####2. Maybe there are scopal modifier nouns
      stype = 'mod-noun-lex'
      mylang.add(ntype + ' := intersective-mod-lex & \
                      [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.HEAD ' + mod + ' ] > ].')
    elif n_refl == 'obl':
      stype = 'reflexive-noun-lex'
    elif compound:
      stype = 'compound-noun-lex'
    elif singlentype or det == 'opt':
      stype = 'noun-lex'
    elif det == 'obl':
      stype = 'obl-spr-noun-lex'
    else:
      stype = 'no-spr-noun-lex'

    if not expl:
      mylang.add(ntype + ' := ' + stype + '.')

      if pers_n:
        mylang.add(ntype + ' := [ SYNSEM.LKEYS.KEYREL named-relation ].')

    features.customize_feature_values(mylang, ch, hierarchies, noun, ntype, 'noun')

    for stem in noun.get('stem', []):
      orth = stem.get('orth')
      pred = stem.get('pred')
      id = stem.get('name')
      if expl == 'yes':
        typedef = TDLencode(id) + ' := expl-noun-lex & \
                    [ STEM < "' + orth + '" > ].'
      elif rel == 'yes' or n_refl == 'obl':
        typedef = TDLencode(id) + ' :=  ' + ntype + ' & \
                    [ STEM < "' + orth + '" > ].'
      elif pers_n:
        typedef = TDLencode(id) + ' := ' + ntype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.CARG "' + pred + '" ].'
      else:
        typedef = TDLencode(id) + ' := ' + ntype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)


######################################################################
# create_wh_phrases()
# Create basic types and supertypes to get wh-words to work
# Specific subtypes are created in associated libraries
#

def create_wh_phrases(mylang, ch):
  wh_pred = ch.get('wh-pred')
  if ch.get('word-order') == 'v2' and ch.get('verb-cluster') == 'yes':
    super_type = 'no-cluster-lex-item'
  else:
    super_type = 'lex-item'
  basic_wh_type =  'basic-wh-lex := ' + super_type + ' & \
     [ SYNSEM [ LOCAL.CONT [ RELS.LIST [ FIRST.LBL #nhand, \
                                         REST.FIRST [ PRED "_' + wh_pred +  '_q_rel", \
                                                  RSTR #rhand ] ], \
                              HCONS <! qeq & [ HARG #rhand, \
                                               LARG #nhand ] !> ], \
                 NON-LOCAL.QUE 1-dlist ] ].'

  mylang.add(basic_wh_type)
  if ch.get('wh-adv') == 'on':
    bwlnsl = 'basic-wh-loc-non-sp-lex := basic-wh-lex & \
         [ SYNSEM [ LOCAL [ CONT [ HOOK [ LTOP #khand, \
				          INDEX #index ], \
                                   RELS <! #keyrel & [ ARG0 #ind], \
				         [ ARG0 #ind ], \
                                         [ LBL #khand, \
                                           PRED "_loc_nonsp_rel", \
				           ARG0 event, \
				           ARG1 #index, \
				           ARG2 #ind ]  !> ], \
		               CAT.HEAD.MOD \
                                        < [ LOCAL intersective-mod & \
                                          [ CONT.HOOK.INDEX #index ] ] > ], \
                     LKEYS.KEYREL #keyrel ] ].'
    mylang.add(bwlnsl)
    wal = 'wh-adverb-lex := basic-wh-loc-non-sp-lex & \
           [ SYNSEM.LOCAL.CAT [ HEAD +rp, \
                                VAL [ SUBJ < >, \
			              COMPS < >, \
			              SPEC < >, \
			              SPR < > ] ] ].'
    mylang.add(wal)

  if ch.get('wh-np') == 'on':
    bwlsimple = \
      'basic-wh-simple-sem-lex := basic-one-arg & norm-hook-lex-item & \
                                   basic-wh-lex & \
             [ SYNSEM [ LOCAL [ CONT [ HOOK.INDEX #index, \
                                       RELS <! [ ], \
                                               #altkeyrel & \
                                               [ ARG0 #index ] !> ], \
                                 CAT.VAL [ SUBJ < >, \
                                           COMPS < >, \
                                           SPEC < >, \
                                           SPR < > ] ], \
                         LKEYS.ALTKEYREL #altkeyrel ] ].'
    mylang.add(bwlsimple)
 
###future work: cross-classification between wh- and rel- words  
    wh_noun = \
      'wh-noun-lex := basic-wh-simple-sem-lex & non-rel-lex-item & \
                 [ SYNSEM [ LOCAL.CAT.HEAD noun & [ MOD < > ], \
                            LKEYS.KEYREL noun-relation ] ].'
    mylang.add(wh_noun)
  if ch.get('wh-det') == 'on':
    wh_det = \
     'wh-determiner-lex := basic-determiner-lex & zero-arg-nonslash & \
       [ SYNSEM [ LOCAL.CAT.VAL [ SUBJ < >, \
                                  COMPS < >, \
                                  SPR < > ], \
                  NON-LOCAL.QUE 1-dlist ] ].'
    mylang.add(wh_det)
    if ch.get('rel-clause') == 'yes':
      mylang.add('wh-determiner-lex := non-rel-lex-item.')
  mylang.add('non-wh-lex-item := lex-item & [ SYNSEM.NON-LOCAL.QUE 0-dlist].')
  mylang.add('basic-adjective-lex :+ non-wh-lex-item.')
  mylang.add('basic-adverb-lex :+ non-wh-lex-item.')
  mylang.add('basic-adposition-lex :+ basic-one-arg & \
             [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comp & [ ] >, \
               ARG-ST < #comp > ].')
  mylang.add('determiner-lex := non-wh-lex-item.') 


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
  
  if ch.get('has-aux') == 'yes':
    mylang.set_section('auxlex')
    auxiliaries.customize_auxiliaries(mylang, ch, lexicon, hierarchies)
  if ch.get('has-cop') == 'yes':
    customize_copula(mylang, ch, lexicon, hierarchies)
  if ch.get('wh-questions') == 'yes':
    create_wh_phrases(mylang, ch)  

  mylang.set_section('otherlex')
  customize_determiners(mylang, ch, lexicon, hierarchies)
  customize_adjectives(mylang, ch, lexicon)
  customize_adverbs(mylang, ch, lexicon)
  customize_adpositions(ch, mylang, lexicon)
  customize_complementizers(ch, mylang, lexicon) 
  customize_misc_lex(ch, lexicon)
