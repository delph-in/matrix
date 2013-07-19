from gmcs.utils import get_name
from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

from gmcs.linglib import case
from gmcs.linglib import features
from gmcs.linglib import auxiliaries
from gmcs.linglib import subcategorization
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

def customize_verbs(mylang, ch, lexicon, hierarchies, climb_verbs):
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
    climb_verbs.add('head :+ [ AUX bool ].', comment='section=\'addenda\'')
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
    climb_verbs.add(typedef)
    typedef = \
      mainorverbtype + ' := verb-lex & basic-verb-lex & \
                      [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].'
    mylang.add(typedef)
    climb_verbs.add(typedef)
    typedef = \
      'aux-lex := verb-lex & \
                [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].'
    mylang.add(typedef)
    climb_verbs.add(typedef)

  ###Germanic slightly generalized: using form to allow verbs to select
  ###for an auxiliary (haben/sein + ptc)

    if ch.get('aux-select') == 'yes':
      auxs = ch.get('sel_aux',[])
      for aux in auxs:
        auxn = aux.get('value')
        mylang.add(auxn + '-only-verb-lex := verb-lex & \
                    [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + auxn + '-only ].')
        climb_verbs.add(auxn + '-only-verb-lex := verb-lex & \
                    [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + auxn + '-only ].')
  #Germanic change: uses VC differently
    if vcluster and not wo == 'v2':

      mylang.add(mainorverbtype + ' := [ SYNSEM.LOCAL.CAT.VC + ].')
      mylang.add('aux-lex := [ SYNSEM.LOCAL.CAT.VC - ].')
      climb_verbs.add(mainorverbtype + ' := [ SYNSEM.LOCAL.CAT.VC + ].')
      climb_verbs.add('aux-lex := [ SYNSEM.LOCAL.CAT.VC - ].')
  #Small addition for Germanic
    if ch.get('verb-cluster') == 'yes' and wo == 'v2':
      if ch.get('vc-analysis') == 'basic' and not ch.get('old-analysis') == 'yes':
        mylang.add(mainorverbtype + ' := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')
        mylang.add('aux-lex := [ SYNSEM.LOCAL.CAT.VFRONT - ].')
        climb_verbs.add(mainorverbtype + ' := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')
        climb_verbs.add('aux-lex := [ SYNSEM.LOCAL.CAT.VFRONT - ].')
  else:
    #mainorverbtype = 'verb-lex'
    vcluster = False
    mylang.add('verb-lex := basic-verb-lex & non-mod-lex-item.')
    climb_verbs.add('verb-lex := basic-verb-lex & non-mod-lex-item.')
  
  mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].')
  climb_verbs.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].')
  
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
  climb_verbs.add(typedef)
  ###if all verbs take inflection, except some specific group
  ###the following applies
  if 'gen-' in mainorverbtype:
    mylang.add('main-verb-lex := gen-main-verb-lex.')
    mylang.add('infl-main-verb-lex := gen-main-verb-lex & \
                 [ INFLECTED.VERB-INFLECTION-FLAG + ].')
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
    climb_verbs.add(typedef)    
    if ch.get('rel-clause') == 'yes':
       non_r_loc = 'rel-non-refl-local'
    else:
      non_r_loc = 'non-refl-local'
    mylang.add('non-refl-verb-lex := ' + mainorverbtype + ' & \
                 [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL ' + non_r_loc + ' ].')
    climb_verbs.add('non-refl-verb-lex := ' + mainorverbtype + ' & \
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
    climb_verbs.add(typedef, comment)
    climb_verbs.add('expl-two-arg-verb-lex := expl-two-arg-lex-item & \
                    ' + mainorverbtype + '.')
    if refl:
      mylang.add('expl-two-arg-verb-lex := \
                 [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL ' + non_r_loc + ' ].')
      climb_verbs.add('expl-two-arg-verb-lex := \
                 [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL ' + non_r_loc + ' ].')
  if hclightallverbs:
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')
    climb_verbs.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')
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
  climb_verbs.add(typedef)

  # transitive verb lexical type
  typedef = \
    'transitive-verb-lex := transitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
         ARG-ST < [ ], \
                  #comps & \
                  [ LOCAL.CAT [ VAL [ SPR < >, \
                                      COMPS < > ] ] ] > ].'
  mylang.add(typedef)
  climb_verbs.add(typedef)
  if refl:
    mylang.add('transitive-verb-lex := non-refl-verb-lex.')
    climb_verbs.add('transitive-verb-lex := non-refl-verb-lex.')
  else:
    mylang.add('transitive-verb-lex := ' + mainorverbtype + '.')
    climb_verbs.add('transitive-verb-lex := ' + mainorverbtype + '.')
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
    climb_verbs.add(typedef)
    if refl:
      mylang.add('ditransitive-verb-lex := non-refl-verb-lex.')
      climb_verbs.add('ditransitive-verb-lex := non-refl-verb-lex.')
    else:
      mylang.add('ditransitive-verb-lex := ' + mainorverbtype + '.')
      climb_verbs.add('ditransitive-verb-lex := ' + mainorverbtype + '.')
# ditransitive verb lexical type
  if ch.get('verbal-particles') == 'yes':
    typedef = \
    'part-transitive-verb-lex := transitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comp1 , \
                                       [ LOCAL.CAT [ VAL [ SPR < >, \
                                                           SUBJ < >, \
                                                           SPEC < >, \
                                                            COMPS < > ] ] ] >, \
         ARG-ST < [ ], \
                  #comp1 & \
                  [ LOCAL.CAT [ VAL [ SPR < >, \
                                      COMPS < > ] ] ]  > ].'
    mylang.add(typedef)
    climb_verbs.add(typedef)

    if refl:
      mylang.add('part-transitive-verb-lex := non-refl-verb-lex.')
      climb_verbs.add('part-transitive-verb-lex := non-refl-verb-lex.')
    else:
      mylang.add('part-transitive-verb-lex := ' + mainorverbtype + '.')
      climb_verbs.add('part-transitive-verb-lex := ' + mainorverbtype + '.')

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
    climb_verbs.add(typedef)

  if ch.get('subj-control-verb') == 'yes':
    typedef = \
    'subj-contr-transitive-verb-lex := basic-verb-lex & \
        [ ARGS < [ OPT - ], [ OPT - ] > ].' 
    mylang.add(typedef) 
    climb_verbs.add(typedef)
    if ch.get('vc-analysis') == 'basic':
      mylang.add('subj-contr-transitive-verb-lex := arg-comp-aux & \
       trans-first-arg-control-lex-item.')
      climb_verbs.add('subj-contr-transitive-verb-lex := arg-comp-aux & \
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
      climb_verbs.add('auxrule-first-arg-control-lex-item := basic-one-arg & \
       [ ARG-ST <  [ LOCAL [ CONT.HOOK [ XARG #ind, \
	                 		 LTOP #larg ] ] ] >, \
         SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg, \
	          			      LARG #larg ] !>, \
	          LKEYS.KEYREL [ ARG1 #ind, \
		                 ARG2 #harg ] ] ].')
      climb_verbs.add('subj-contr-transitive-verb-lex := \
                    auxrule-first-arg-control-lex-item & one-comp-aux.')


  if ch.get('obj-raising') == 'yes':
    if refl:
      mylang.add('obj-raising-verb-lex := non-refl-verb-lex.')
      climb_verbs.add('obj-raising-verb-lex := non-refl-verb-lex.')
    else:
      mylang.add('obj-raising-verb-lex := ' + mainorverbtype + '.')
      climb_verbs.add('obj-raising-verb-lex := ' + mainorverbtype + '.')
    typedef = \
    'obj-raising-verb-lex := ditrans-second-arg-raising-lex-item & \
     [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < #subj >, \
			      SPR < >, \
			      SPEC < > ], \
       ARG-ST < #subj & [ LOCAL.CAT [ VAL.SPR < > ] ], [ ], [ ] > ].'

    mylang.add(typedef)
    climb_verbs.add(typedef)
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
    climb_verbs.add('obj-raising-verb-lex := ' + comps_struc)


  vcross = False
  if ch.get('verb-cross-classification') == 'yes':
    subcategorization.create_basic_verb_types(ch, mylang, climb_verbs)
  #  subcategorization.customize_verb_case(mylang, ch)
    vcross = True
  else:
    subcategorization.customize_verb_case(mylang, climb_verbs, ch)

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
    raising = verb.get('raising')
    i = val.find(',')
    dir_inv = ''
    if i != -1:
      val = val[:i]
      dir_inv = 'dir-inv-'

    if val == 'trans':
      tivity = 'trans'
    elif val == 'intrans':
      tivity = 'intrans'
    elif val.find('-') != -1 and not vcross:
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
        elif c[2] == 'vor' or c[2] == 'voor':
          a_case = case.canon_to_abbr(c[0], cases)
          b_case = case.canon_to_abbr(c[1], cases)
          o_case = case.canon_to_abbr(c[2], cases)
          tivity = a_case + '-' + b_case + '-' + o_case + '-part-trans'
          create_particle_lex_entry(mylang, lexicon, o_case, climb_verbs)
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          b_case = case.canon_to_abbr(c[1], cases)
          o_case = case.canon_to_abbr(c[2], cases)
          tivity = a_case + '-' + b_case + '-' + o_case + '-ditrans'
    elif val.find('-') != -1:
      tivity = val
      vparts = val.split('-')
      if not raising:
        if len(vparts) == 2:
          tivity += '-trans'
        elif len(vparts) == 3:
          tivity += '-ditrans'
        elif len(vparts) == 4:
          tivity += '-3arg'
        elif len(vparts) == 5:
          tivity += '-4arg'
    else:
      if not vcross:
        s_case = case.canon_to_abbr(val, cases)
      else:
        s_case = val
      tivity = s_case + '-intrans'
    if not vcross:
      tivity += 'itive'
    stype = dir_inv + tivity + '-verb-lex'
    vtype = name + '-verb-lex'

    mylang.add(vtype + ' := ' + stype + '.')
    climb_verbs.add(vtype + ' := ' + stype + '.')

    if aux_s:
      mylang.add(vtype + ' := ' + aux_s + '-only-verb-lex.')
      climb_verbs.add(vtype + ' := ' + aux_s + '-only-verb-lex.')

    if o_drop_default:
      if val == 'trans' or '-' in val:
        c = val.split('-')
        if not 'obj' in opt_heads:
          mylang.add(vtype + ' := \
                     [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT \
                                           ' + o_drop_default + ' ].')
          climb_verbs.add(vtype + ' := \
                     [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT \
                                           ' + o_drop_default + ' ].')
        if len(c) == 3:
          if c[2] == 'vor' or c[2] == 'voor':
            mylang.add(vtype + ' := \
                         [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ], [ OPT - ] > ].')
            climb_verbs.add(vtype + ' := \
                         [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ], [ OPT - ] > ].')
          elif not 'obj2' in opt_heads:
            if not c[1] == 'refl':
              mylang.add(vtype + ' := \
                     [ ARG-ST < [ ], [ ], \
                                    [ OPT ' + o_drop_default + ' ] > ].')
              climb_verbs.add(vtype + ' := \
                     [ ARG-ST < [ ], [ ], \
                                    [ OPT ' + o_drop_default + ' ] > ].')
            else:
              mylang.add(vtype + ' := \
                     [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ], \
                                      [ OPT ' + o_drop_default + ' ] > ].')
              climb_verbs.add(vtype + ' := \
                     [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ], \
                                      [ OPT ' + o_drop_default + ' ] > ].')  
###
# scomp-spec
# does the verb take questions, affirmatives or both as a complement?
#
    sf = verb.get('compl-sf')
    if sf and not vcross:
      mylang.add(vtype + ' := \
          [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.SF ' \
           + sf + ' ].')
      climb_verbs.add(vtype + ' := \
          [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.SF ' \
           + sf + ' ].')

    features.customize_feature_values(mylang, ch, hierarchies, verb, vtype, 'verb', None, cases, climbfile = climb_verbs)

    stems = verb.get('stem', [])
    stems.extend(verb.get('bistem', []))

    for stem in stems:
      orthstr = orth_encode(stem.get('orth'))
      pred = stem.get('pred')
      id = stem.get('name')
      id = id.replace(' ','_')
      typedef = \
        TDLencode(id) + ' := ' + vtype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)
      climb_verbs.add(typedef, section='lexicon')
      climb_verbs.set_section('mylang')
# Returns the verb type for lexical/main verbs.
def main_or_verb(ch):
  if ch.get('has-aux') == 'yes':
    if ch.get('verb-morph-exception') == 'yes':
      return 'gen-main-verb-lex'
    else:
      return 'main-verb-lex'
  else:
    return 'verb-lex'


def create_particle_lex_entry(mylang, lexicon, o_case, climb_verbs):
  mylang.add(o_case + '-verbal-particle-lex := verbal-particle-lex & \
              [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + o_case + '-part ].')
  climb_verbs.add(o_case + '-verbal-particle-lex := verbal-particle-lex & \
              [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + o_case + '-part ].')
  typedef = \
    TDLencode(o_case) +  ' := ' + o_case + '-verbal-particle-lex & \
                    [ STEM < "' + o_case + '" > ].'
  lexicon.add(typedef)
  climb_verbs.add(typedef, section='lexicon')
  climb_verbs.set_section('mylang')


def customize_copula(mylang, climb_cop, ch, lexicon, hierarchies):
  #add copula supertype
  #copula
  if ch.get('has-cop') == 'yes':
    if ch.get('rel-clause') == 'yes':
      mylang.add('+nvjrp :+ [ PRD bool ].')
      climb_cop.add('+nvjrp :+ [ PRD bool ].')
    else:
      mylang.add('+njrp :+ [ PRD bool ].')
      climb_cop.add('+njrp :+ [ PRD bool ].')
  
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
      climb_cop.add('copula-verb-lex := ' + type_name + ' & trans-first-arg-raising-lex-item-2.')
      mylang.add('loc-copula-verb-lex := ' + type_name + \
                    ' & trans-first-arg-raising-lex-item & norm-sem-lex-item & \
  [ SYNSEM [ LOCAL [ CAT.VAL.COMPS.FIRST.LOCAL [ CAT.HEAD +rp, \
						 CONT.HOOK [ INDEX #ind, \
							     XARG #xarg ] ], \
		     CONT.HOOK [ INDEX #ind ] ], \
	     LKEYS.KEYREL.ARG1 #xarg ] ].')

      climb_cop.add('loc-copula-verb-lex := ' + type_name + \
                    ' & trans-first-arg-raising-lex-item & norm-sem-lex-item & \
  [ SYNSEM [ LOCAL [ CAT.VAL.COMPS.FIRST.LOCAL [ CAT.HEAD +rp, \
						 CONT.HOOK [ INDEX #ind, \
							     XARG #xarg ] ], \
		     CONT.HOOK [ INDEX #ind ] ], \
	     LKEYS.KEYREL.ARG1 #xarg ] ].')
    else:
      type_name = 'copula-verb-lex'
      mylang.add('copula-verb-lex := trans-first-arg-raising-lex-item-2.')
      climb_cop.add('copula-verb-lex := trans-first-arg-raising-lex-item-2.')
      
    mylang.add('copula-verb-lex := \
                 [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD +jp ].')
    climb_cop.add('copula-verb-lex := \
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

    climb_cop.add(type_name + ' := verb-lex & \
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
      climb_cop.add(type_name + ' := [ SYNSEM.LOCAL.CAT.MC na-or-- ].' )

    if ch.get('has-aux') == 'yes':
      mylang.add(type_name + ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
      climb_cop.add(type_name + ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')


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
      climb_cop.add(userstypename + ' := copula-verb-lex.')
    else:
      mylang.add(userstypename + ' := loc-copula-verb-lex.')
      climb_cop.add(userstypename + ' := loc-copula-verb-lex.')
    if subjcase:
      mylang.add(userstypename + ' := \
                       [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + subjcase + ' ].')
      climb_cop.add(userstypename + ' := \
                       [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + subjcase + ' ].')

##object drop on copula just doesn't make sense, prohibiting it for noew
    o_drop_default = ''
    if ch.get('obj-drop') == 'obj-drop-lex':
      mylang.add(userstypename + ' := \
                       [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT - ].')
      climb_cop.add(userstypename + ' := \
                       [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT - ].')
    
    if aux_s:
      mylang.add(userstypename + ' := ' + aux_s + '-only-verb-lex.')
      climb_cop.add(userstypename + ' := ' + aux_s + '-only-verb-lex.')

    features.customize_feature_values(mylang, ch, hierarchies, cop, userstypename, 'cop', climbfile = climb_cop)
    add_copula_to_lexicon(userstypename, cop, lexicon, climb_cop)

def add_copula_to_lexicon(userstypename, cop, lexicon, climb_cop):
  for stem in cop.get('stem',[]):
    orth = stem.get('orth')
    orthstr = orth_encode(orth)
    id = stem.get('name')
    id = id.replace(' ','_')
    typedef = TDLencode(id) + ' := ' + userstypename + ' & \
                       [ STEM < "' + orthstr + '" > ].'
    lexicon.add(typedef)
    climb_cop.add(typedef, section='lexicon')
  
    if cop.get('loc') == 'on':
      pred = stem.get('pred')
      typedef = TDLencode(id) + \
                    ' := [ SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef, merge=True)
      climb_cop.add(typedef, merge=True,section='lexicon')

def customize_determiners(mylang, ch, lexicon, climb_lex, hierarchies):

  # Lexical type for determiners, if the language has any:
  if ch.get('has-dets') == 'yes':
    comment = \
      ';;; Determiners\n' + \
      ';;; SPEC is non-empty, and already specified by basic-determiner-lex.'
    mylang.add_literal(comment)
    climb_lex.add_literal(comment)

    typedef = \
      'determiner-lex := basic-determiner-lex & zero-arg-nonslash & \
          [ SYNSEM.LOCAL.CAT.VAL [ SPR < >, \
                                   COMPS < >, \
                                   SUBJ < > ]].'
    mylang.add(typedef)
    climb_lex.add(typedef)
    
    mylang.add('determiner-lex := non-mod-lex-item.')
    climb_lex.add('determiner-lex := non-mod-lex-item.')
    # also not to be used as modifiers
    mylang.add('determiner-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].')
    climb_lex.add('determiner-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].')    
    
    if ch.get('pronoun_possessive') == 'yes':
      add_basic_possessive_pron_det(mylang, climb_lex)

    if ch.get('rel-clause') == 'yes':
      create_rel_determiner(mylang, climb_lex)
      mylang.add('determiner-lex := non-rel-lex-item.')
      climb_lex.add('determiner-lex := non-rel-lex-item.')
  # Determiners
  if 'det' in ch:
    lexicon.add_literal(';;; Determiners')
    climb_lex.add_literal(';;; Determiners', section='lexicon')

  for det in ch.get('det',[]):
    name = get_name(det)
    wh = det.get('wh')    
    rel = det.get('rel')
    poss = det.get('poss')

    if wh == 'yes':
      stype = 'wh-determiner-lex'
    elif rel == 'yes':
      stype = 'rel-determiner-lex'
    elif poss == 'yes':
      stype = 'pronominal-poss_det-lex'
    else:
      stype = 'determiner-lex'
    dtype = name + '-determiner-lex'

    mylang.add(dtype + ' := ' + stype + '.')
    climb_lex.add(dtype + ' := ' + stype + '.')

    features.customize_feature_values(mylang, ch, hierarchies, det, dtype, 'det', climbfile=climb_lex)

    for stem in det.get('stem',[]):
      orthstr = orth_encode(stem.get('orth'))
      pred = stem.get('pred')
      id = stem.get('name')
      id = id.replace(' ','_')
      typedef = \
        TDLencode(id) + ' := ' + dtype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)
      climb_lex.add(typedef,section='lexicon')


def add_basic_possessive_pron_det(mylang, climb_lex):
  typedef_1 = \
   '''basic-poss-determiner-lex := norm-hook-lex-item &
  [ SYNSEM [ LOCAL [ CAT [ HEAD det,
			   VAL [ SPEC < [ LOCAL.CONT.HOOK [ INDEX #ind,
							      LTOP #larg ] ] >,
				 SUBJ < >,
				 COMPS < >,
				 SPR < > ] ],
		     CONT [ HCONS <! qeq &
				   [ HARG #harg,
				     LARG #larg ], [ ] !>,
			    RELS <! relation,
                                      arg12-ev-relation & 
                                      [ PRED "_poss_rel",
                                        ARG1 #index ], [ ], [ ] !>,
			    HOOK.INDEX #index ] ],
	     LKEYS.KEYREL quant-relation &
		   [ ARG0 #ind,
		     RSTR #harg ] ] ].'''
  mylang.add(typedef_1)
  climb_lex.add(typedef_1)
  typedef_2 = \
  '''pronominal-poss_det-lex := basic-poss-determiner-lex & 
  [ SYNSEM [ LOCAL [ CAT.VAL.SPEC < [ LOCAL.CONT.HOOK.LTOP #hand ] >,
		     CONT [ RELS <! relation,
                                  [ LBL #hand,
                                    ARG2 #ind ],
                                  [ PRED "_pronoun_q_rel",
                                    RSTR #rhand,
                                    ARG0 #ind ],
				  [ PRED "pron_rel",
                                    LBL #prohand,
                                    ARG0 #ind ] !>,
                            HCONS <! qeq, qeq & [ HARG #rhand,
                                                  LARG #prohand ] !> ] ],
	     NON-LOCAL [ QUE 0-dlist,
			 REL 0-dlist ] ] ].'''
  mylang.add(typedef_2)
  climb_lex.add(typedef_2)

def create_rel_determiner(mylang, climb_lex):
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
  climb_lex.add(basic_rel_det, comment)
  
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
  climb_lex.add(rel_det)


def customize_adjectives(mylang, ch, lexicon, rules, climb_lex, hierarchies):

   # Lexical type for adjectives, if the language has any:
  if ch.get('has-adj') == 'yes':
    comment = \
      ';;; Adjectives\n' 
    mylang.add_literal(comment)
    mylang.add('scopal-mod-adj-lex := basic-scopal-mod-adj-lex.')
    mylang.add('int-mod-adj-lex := basic-int-mod-adj-lex.')
    mylang.add('basic-adjective-lex :+ basic-zero-arg.')
    climb_lex.add_literal(comment)
    climb_lex.add('scopal-mod-adj-lex := basic-scopal-mod-adj-lex.')
    climb_lex.add('int-mod-adj-lex := basic-int-mod-adj-lex.')
    climb_lex.add('basic-adjective-lex :+ basic-zero-arg.')
   #2011-11-07 Fixing semantics of adjectives
    mylang.add('int-mod-adj-lex := [ SYNSEM [ LOCAL.CONT.HOOK.XARG #xarg, \
                                              LKEYS.KEYREL.ARG1 #xarg ] ].')
    climb_lex.add('int-mod-adj-lex := [ SYNSEM [ LOCAL.CONT.HOOK.XARG #xarg, \
                                              LKEYS.KEYREL.ARG1 #xarg ] ].')

  
    if ch.get('comp-adj') == 'yes':
      comp_adj = \
      '''
      basic-compare-adj-lex := basic-comparative-lex &
      [ SYNSEM.LOCAL.CAT.HEAD adj & [ MOD < [ LOCAL intersective-mod ] > ] ].
      '''
      mylang.add(comp_adj)
      climb_lex.add(comp_adj)
    if ch.get('indep-adj') == 'yes':
      adj_to_noun_phrase = \
      '''independent_adjective_phrase := basic-unary-phrase & phrasal &
  [ SYNSEM [ LOCAL [ CAT [ HEAD noun &
                                [ MOD < > ],
                           VAL [ SUBJ < >,
                                 SPR < synsem &
                                       [ LOCAL [ CAT [ VAL [ SPR < >,
                                                           COMPS < > ],
                                                 HEAD det ],
                                                 AGR #index ],
                                         NON-LOCAL.REL 0-dlist,
                                         OPT - ], ... >,
                                 COMPS < >,
                                 SPEC < > ] ],
                     AGR #agr ],
             NON-LOCAL [ QUE 0-dlist,
                         REL 0-dlist ] ],
    ARGS < [ SYNSEM 
	      [ LOCAL 
		 [ CAT [ HEAD adj &
			      [ MOD < synsem & [ LOCAL.AGR #agr &
							   [ PNG #png ] ] > ],
			 VAL [ SUBJ < >,
			       COMPS < > ] ],
		   CONT.HOOK [ LTOP #nhand,
			       XARG #index  ] ] ] ] >,
    C-CONT [ HOOK [ LTOP #nhand,
                    INDEX #index & [ PNG #png ] ],
	     RELS.LIST < [ LBL #nhand,
                           ARG0 #index ], ... > ] ].
      '''
      mylang.add(adj_to_noun_phrase, section='phrases')
      climb_lex.add(adj_to_noun_phrase, comment='section=\'phrases\'')
      rules.add('ind_adjective := independent_adjective_phrase.') 
      climb_lex.add('ind_adjective := independent_adjective_phrase.') 
      if ch.get('n_spec_spr') == 'yes':
        mylang.add('independent_adjective_phrase := \
           [ SYNSEM.LOCAL.CAT.HEAD mass_cnt_noun ].')
        climb_lex.add('independent_adjective_phrase := \
           [ SYNSEM.LOCAL.CAT.HEAD mass_cnt_noun ].')
    if ch.get('rel-clause') == 'yes':
      mylang.add('basic-adjective-lex :+ non-rel-lex-item.',section='addenda')
      climb_lex.add('basic-adjective-lex :+ non-rel-lex-item.',comment='section=\'addenda\'')
    if ch.get('verb-cluster') == 'yes':
      mylang.add('scopal-mod-adj-lex := no-cluster-lex-item.')
      mylang.add('int-mod-adj-lex := no-cluster-lex-item.')
      climb_lex.add('scopal-mod-adj-lex := no-cluster-lex-item.')
      climb_lex.add('int-mod-adj-lex := no-cluster-lex-item.')   


    if ch.get('strength-marking') == 'double':
      mylang.add('+njdo :+ [ STRONG bool ].', section='addenda')
      climb_lex.add('+njdo :+ [ STRONG bool ].', comment='section=\'addenda\'')
    elif ch.get('strength-marking') == 'triple':
      mylang.add('+njdo :+ [ STRONG luk ].', section='addenda')
      climb_lex.add('+njdo :+ [ STRONG luk ].', comment='section=\'addenda\'')
  
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
      if ch.get('cp-at-np') == 'yes':
        mylang.add('+njc :+ [ CASE case].', section='addenda')
        climb_lex.add('+njc :+ [ CASE case].', comment='section=\'addenda\'')
      else:
        mylang.add('+nj :+ [ CASE case].', section='addenda')
        climb_lex.add('+nj :+ [ CASE case].', comment='section=\'addenda\'')
    elif ch.get('case-marking') != 'none' or ch.get('real-case-marking') != 'none': 
      if ch.get('cp-at-np') == 'yes':
        mylang.add('+nc :+ [ CASE case].', section='addenda')
        climb_lex.add('+nc :+ [ CASE case].', comment='section=\'addenda\'')
      else:
        mylang.add('noun :+ [ CASE case ].', section='addenda')
        climb_lex.add('noun :+ [ CASE case ].', comment='section=\'addenda\'')
      
 # Adjectives
  if 'adj' in ch:
    lexicon.add_literal(';;; Adjectives')
    climb_lex.add_literal(';;; Adjectives',section='lexicon')

  for adj in ch.get('adj',[]):
    name = get_name(adj)

    stype = '-mod-adj-lex'
    if adj.get('kind') == 'int':
      stype = 'int' + stype
    elif adj.get('kind') == 'comp':
      stype = 'basic-compare-adj-lex'
    else:
      stype = 'scopal' + stype
    atype = name + '-adjective-lex'
    
    mylang.add(atype + ' := ' + stype + ' & \
      [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT [ HEAD noun, \
                                                  VAL.SPR <[ ]> ] ] > ].')
    climb_lex.add(atype + ' := ' + stype + ' & \
      [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT [ HEAD noun, \
                                                  VAL.SPR <[ ]> ] ] > ].')

    arg_str = adj.get('arg-str')
    val = ''
    if arg_str == 'none':
      val = 'VAL [ SUBJ < >, \
                   COMPS < >, \
                   SPR < >, \
                   SPEC < > ]'
    elif arg_str == 'acc':
      val = 'VAL [ SUBJ < >, \
                   COMPS < [ LOCAL.CAT.HEAD noun & [ CASE acc ] ]>, \
                   SPR < >, \
                   SPEC < > ]'
    elif arg_str == 'dat':
      val = 'VAL [ SUBJ < >, \
                   COMPS < [ LOCAL.CAT.HEAD noun & [ CASE dat ] ]>, \
                   SPR < >, \
                   SPEC < > ]'
    elif arg_str == 'adp':
      if ch.get('verb-cross-classification') == 'yes':
        val = 'VAL [ SUBJ < >, \
                   COMPS < [ LOCAL.CAT.HEAD adp & [ FORM #pform ] ]>, \
                   SPR < >, \
                   SPEC < > ]'
        mylang.add(atype + ' := [ SYNSEM.LKEYS.KEY-ADP #pform ].')
        climb_lex.add(atype + ' := [ SYNSEM.LKEYS.KEY-ADP #pform ].')
      else:
        val = 'VAL [ SUBJ < >, \
                   COMPS < [ LOCAL.CAT.HEAD adp ]>, \
                   SPR < >, \
                   SPEC < > ]'
 
    if val:
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.' + val + ' ].')
      climb_lex.add(atype + ' := [ SYNSEM.LOCAL.CAT.' + val + ' ].')
      

    if case_agr and not ll:
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.HEAD [ CASE #case, \
                            MOD < [ LOCAL.CAT.HEAD.CASE #case ] > ] ].')
      climb_lex.add(atype + ' := [ SYNSEM.LOCAL.CAT.HEAD [ CASE #case, \
                            MOD < [ LOCAL.CAT.HEAD.CASE #case ] > ] ].')

    if strength_agr and not ll:
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.HEAD [ STRONG #strength, \
                            MOD < [ LOCAL.CAT.HEAD.STRONG #strength ] > ] ].')
      climb_lex.add(atype + ' := [ SYNSEM.LOCAL.CAT.HEAD [ STRONG #strength, \
                            MOD < [ LOCAL.CAT.HEAD.STRONG #strength ] > ] ].')

    features.customize_feature_values(mylang, ch, hierarchies, adj, atype, 'adj', climbfile= climb_lex)

    for stem in adj.get('stem',[]):
      orth = stem.get('orth')
      orthstr = orth_encode(orth)
      pred = stem.get('pred')
      id = stem.get('name')
      id = id.replace(' ','_')
      typedef = \
        TDLencode(id) + ' := ' + atype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)
      climb_lex.add(typedef, section='lexicon')


def customize_numbers(ch, mylang, lexicon, lrules, climb_num, hierarchies):
  basic_type = \
  '''basic-number-adjective-lex := basic-int-mod-adj-lex &
    [ SYNSEM [ LOCAL.CAT.VAL [ SUBJ < >,
                               COMPS < >,
                               SPR < > ],
               NON-LOCAL [ SLASH 0-dlist,
                           QUE 0-dlist,
                           REL 0-dlist ] ] ].'''
  mylang.add(basic_type)
  climb_num.add(basic_type)
  numb = ch.get('numb',[])
  sub_t1 = '''norm-number-adjective-lex := basic-number-adjective-lex &
          [ SYNSEM.LKEYS.KEYREL number-relation ].'''
  sub_t2 = '''ord-number-adjective-lex := basic-number-adjective-lex &
          [ SYNSEM.LKEYS.KEYREL ord-relation ].'''
  mylang.add(sub_t1)
  mylang.add(sub_t2)
  climb_num.add(sub_t1)
  climb_num.add(sub_t2)
  for n in numb:
    nname = n.get('name') + '-lex'
    if n.get('det') == 'yes':
      lex_rule_type = \
      '''numb-det-lex-rule := same-non-local-lex-rule &
			      same-modified-lex-rule &
			      same-light-lex-rule &
                              same-val-lex-rule & 
     [ SYNSEM.LOCAL.CAT [ HEAD det,
                          VAL.SPEC.FIRST #item & 
                                       [ LOCAL.CONT.HOOK [ INDEX #index,
                                                           LTOP #larg] ] ],
       DTR basic-number-adjective-lex &
           [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST #item ],
       C-CONT [ HOOK.INDEX #index,
                RELS <! quant-relation &
		   [ PRED "_def_q_rel",
		     ARG0 #index,
		     RSTR #harg ] !>,
	       HCONS <! qeq & 
		    [ HARG #harg,
		      LARG #larg ] !> ] ].'''

      mylang.add(lex_rule_type)
      climb_num.add(lex_rule_type)
      lrules.add('numb-det-lrule := numb-det-lex-rule.')
      climb_num.add('numb-det-lrule := numb-det-lex-rule.',section='lrules')
    
    if n.get('ord') == 'yes':
      stype = 'norm-number-adjective-lex'
    else:
      stype = 'ord-number-adjective-lex'

#    for feat in n.get('feat',[]):
#      if feat.get('head') == 'mod':
#        path = '[ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG.'
#      path += feat.get('name').upper()
#      path += ' ' + feat.get('value')
    mylang.add(nname + ' := ' + stype + '.')
    climb_num.add(nname + ' := ' + stype + '.')
    features.customize_feature_values(mylang, ch, hierarchies, n, nname, 'adj', climbfile=climb_num)

    for stem in n.get('stem',[]):
      orth = stem.get('orth')
      orthstr = orth_encode(orth)
      pred = stem.get('pred')
      id = stem.get('name')
      id = id.replace(' ','_')
      typedef = \
        TDLencode(id) + ' := ' + nname + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.CARG "' + pred + '" ].'
      lexicon.add(typedef)
      climb_num.add(typedef,section='lexicon')

def customize_adverbs(mylang, ch, lexicon, climb_lex):

   # Lexical type for adverbs, if the language has any:
  if ch.get('has-adv') == 'yes':
    comment = \
      ';;; Adverbs\n'
    mylang.add_literal(comment)
    mylang.add('scopal-adverb-lex := basic-scopal-adverb-lex.')
    mylang.add('int-adverb-lex := basic-int-adverb-lex & \
                [ SYNSEM [ LOCAL.CONT.HOOK.XARG #arg1, \
                           LKEYS.KEYREL.ARG1 #arg1 ] ].')
    mylang.add('basic-adverb-lex :+ [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].')
    climb_lex.add_literal(comment)
    climb_lex.add('scopal-adverb-lex := basic-scopal-adverb-lex.')
    climb_lex.add('int-adverb-lex := basic-int-adverb-lex & \
                [ SYNSEM [ LOCAL.CONT.HOOK.XARG #arg1, \
                           LKEYS.KEYREL.ARG1 #arg1 ] ].')
    climb_lex.add('basic-adverb-lex :+ [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].')
    if ch.get('adv-argst') == 'yes':
      mylang.add('int-mod-with-one-arg-lex := intersective-mod-lex & \
                    [ SYNSEM.LKEYS.KEYREL.ARG2 #arg2, \
                      ARG-ST < [ LOCAL.CONT.HOOK.INDEX #arg2 ] > ].')
      climb_lex.add('int-mod-with-one-arg-lex := intersective-mod-lex & \
                    [ SYNSEM.LKEYS.KEYREL.ARG2 #arg2, \
                      ARG-ST < [ LOCAL.CONT.HOOK.INDEX #arg2 ] > ].')

    if ch.get('rel-clause') == 'yes':
      mylang.add('basic-adverb-lex :+ non-rel-lex-item.',section='addenda')
      climb_lex.add('basic-adverb-lex :+ non-rel-lex-item.',comment='section=\'addenda\'')
    if ch.get('verb-cluster') == 'yes':
      mylang.add('scopal-adverb-lex := no-cluster-lex-item.')
      mylang.add('int-adverb-lex := no-cluster-lex-item.') 
      climb_lex.add('scopal-adverb-lex := no-cluster-lex-item.')
      climb_lex.add('int-adverb-lex := no-cluster-lex-item.')   

 # Adverbs
  if 'adv' in ch:
    lexicon.add_literal(';;; Adverbs')
    climb_lex.add_literal(';;; Adverbs', section='lexicon')

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
      if 'ad' in modh:
        mod_head += 'j'
        mod_head += 'r'
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
      climb_lex.add(atype + ' := ' + stype + ' & \
        [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.HEAD ' + mod_head + ' ] > ].')
    else:
      mylang.add(atype + ' := \
        [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.HEAD ' + mod_head + ' ] > ].')
      climb_lex.add(atype + ' := \
        [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.HEAD ' + mod_head + ' ] > ].')
    
    if mod_val:
      mylang.add(atype + ' :=  \
      [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL ' + mod_val + ' ] > ].')
      climb_lex.add(atype + ' :=  \
      [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL ' + mod_val + ' ] > ].')

    if order == 'pre':
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
      climb_lex.add(atype + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
    elif order == 'post':
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
      climb_lex.add(atype + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')

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
                                         MC -, \
                                         VAL [ SUBJ < [ ] >, \
                                               COMPS < >, \
                                               SPR < >, \
                                               SPEC < > ] ], \
                             OPT -, \
                             NON-LOCAL.SLASH 0-dlist ] >, \
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
        climb_lex.add('clausal-arg-mod-lex-item := basic-one-arg & \
                    [ ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] >, \
                      SYNSEM [ LOCAL.CONT.HCONS <! [ ], qeq & [ HARG #harg, \
                                                           LARG #larg ] !>, \
                               LKEYS.KEYREL [ ARG2 #harg ] ] ].')
        climb_lex.add('scopal-mod-with-cl-arg-lex := clausal-arg-mod-lex-item & \
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
        climb_lex.add('clausal-arg-int-mod-lex-item := basic-one-arg & \
                    [ ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] >, \
                      SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg, \
                                                           LARG #larg ] !>, \
                               LKEYS.KEYREL [ ARG2 #harg ] ] ].')
       
        climb_lex.add('int-mod-clausal-arg-lex := clausal-arg-int-mod-lex-item & \
                     [ SYNSEM [ LOCAL.CAT.HEAD.MOD < [ LOCAL intersective-mod & \
					             [ CONT.HOOK.INDEX #ind ]] >, \
                                LKEYS.KEYREL.ARG1 #ind ] ].')
    if val:
      mylang.add(atype + ' := [ SYNSEM.LOCAL.CAT.' + val + ' ].')
      climb_lex.add(atype + ' := [ SYNSEM.LOCAL.CAT.' + val + ' ].')
####Germanic specific  
    if arg_str == 'zuinf':
      mylang.add(atype + ' := int-mod-with-one-arg-lex & \
        [ SYNSEM.LOCAL.CAT [ HEAD.MOD < [ LOCAL.CONT.HOOK.XARG #xarg ] >, \
                             VAL.COMPS < #comp & \
                                        [ LOCAL.CONT.HOOK.XARG #xarg ] > ], \
          ARGS < #comp > ] ].')   
      climb_lex.add(atype + ' := int-mod-with-one-arg-lex & \
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
      climb_lex.add(atype + ' := scopal-mod-with-cl-arg-lex & basic-adverb-lex & \
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
      orthstr = orth_encode(orth)
      pred = stem.get('pred')
      id = stem.get('name')
      id = id.replace(' ','_')
      typedef = \
        TDLencode(id) + ' := ' + atype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)
      climb_lex.add(typedef,section='lexicon')


def customize_adpositions(ch, mylang, lexicon, climb_lex, hierarchies):
  cases = case.case_names(ch)
  if ch.get('has-adp'):
    s_name = create_adposition_supertypes(ch, mylang, climb_lex)
    
    default = ch.get('adp-default')
    exception = ch.get('adp-default-exception')  
    for adp in ch.get('adp',[]):
      set_order = False
      sname = s_name
      incl = ''
      if adp.get('incl') == 'yes':
        incl = True
      if incl:
        sname = 'compl-incl-adp-lex-item'
      if adp.get('det-incl') == 'yes':
        sname = 'int-adp-integrated-det-lex-item'
      name = adp.get('name') + '-adp-lex-item'
      kind = adp.get('kind')
      form = adp.get('form')
      circum = adp.get('circum')
      rel = adp.get('relative-cl') 
### based on Dutch (and absence in German) relative adposition is always compl-incl
      if rel == 'yes' and incl:
        mylang.add('relative-adposition-lex := ' + sname + ' & \
              [ SYNSEM [ LOCAL.CONT.RELS.LIST.FIRST.ARG2 #arg2, \
                         NON-LOCAL.REL 1-dlist & <! [ INDEX #arg2 ] !> ] ].')
        climb_lex.add('relative-adposition-lex := ' + sname + ' & \
              [ SYNSEM [ LOCAL.CONT.RELS.LIST.FIRST.ARG2 #arg2, \
                         NON-LOCAL.REL 1-dlist & <! [ INDEX #arg2 ] !> ] ].')   
        sname = 'relative-adposition-lex' 
        create_adp_cross_classification(ch, mylang, climb_lex, sname)   

      if kind == 'mod':
        mod = adp.get('mod')
        if kind in exception and mod in exception:
          set_order = True
        if 'incl' in sname or circum == 'yes':
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
        elif circum == 'yes': 
          sname = 'circump-lex-item'
          create_adp_cross_classification(ch, mylang, climb_lex, sname)
 
        if adp.get('sprundersp') == 'on':
          mylang.add(name + ' := general-int-adp-lex-item & prep-lex-item.')
          climb_lex.add(name + ' := general-int-adp-lex-item & prep-lex-item.')
          if mod == 'noun':
            mylang.add(name + ' := \
               [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST [ LOCAL.CAT.HEAD noun, \
                                                   LIGHT + ] ].')
            climb_lex.add(name + ' := \
               [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST [ LOCAL.CAT.HEAD noun, \
                                                   LIGHT + ] ].')
          elif kind == 'prd':
            mylang.add(name + ' := [ SYNSEM [ LOCAL.CONT.HOOK.XARG #arg1, \
                                     LKEYS.KEYREL.ARG1 #arg1 ] ].')
            climb_lex.add(name + ' := [ SYNSEM [ LOCAL.CONT.HOOK.XARG #arg1, \
                                     LKEYS.KEYREL.ARG1 #arg1 ] ].')
        else:
          mylang.add(name + ' := ' + mod + '-' + sname + '.')
          climb_lex.add(name + ' := ' + mod + '-' + sname + '.')
          #used to be part of supertype, but inclusives can also be args
          if incl:
            mylang.add(name + ' := ' + 'intersective-mod-lex.')
            climb_lex.add(name + ' := ' + 'intersective-mod-lex.')
        order = adp.get('order')
        if order == 'post':
          mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
          climb_lex.add(name + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
        elif order == 'pre':
          mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
          climb_lex.add(name + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
        if ch.get('has-cop') == 'yes':
          mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.HEAD.PRD - ].')
          climb_lex.add(name + ' := [ SYNSEM.LOCAL.CAT.HEAD.PRD - ].')
      elif kind == 'prd':
        mylang.add(name + ' := prd-' + sname + '& \
          [ SYNSEM.LOCAL.CAT [ HEAD.PRD +, \
                               VC - ] ].' )
        climb_lex.add(name + ' := prd-' + sname + '& \
          [ SYNSEM.LOCAL.CAT [ HEAD.PRD +, \
                               VC - ] ].' )

      features.customize_feature_values(mylang, ch, hierarchies, adp, name, 'adp', climbfile = climb_lex)
#     for feat in adp.get('feat',[]):
#      if feat.get('name') == 'case':
#          constr = 'LOCAL.CAT.HEAD.CASE '
#          value = case.canon_to_abbr(feat.get('value'), cases)
#          value += ' ] >'
#        if feat.get('head') == 'comp':
#          path = 'SYNSEM.LOCAL.CAT.VAL.COMPS < [ '
#          
#        typedef = name + ' := [ ' + path + constr + value + ' ].'
#        mylang.add(typedef)
      if form:
        sf = ''
        if ch.get('nachfeld') == 'yes' and 'pform' in ch.get('nf-forms'):
          sf += 'nf-'
        sf += 'form'
        mylang.add('pform := ' + sf + '.',section='features')
        mylang.add(form + ' := pform.',section='features')
        mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')
        climb_lex.add('pform := ' + sf + '.',section='features')
        climb_lex.add(form + ' := pform.',section='features')
        climb_lex.add(name + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')
       

      for stem in adp.get('stem',[]):
        orth = stem.get('orth')
        orthstr = orth_encode(orth)
        pred = stem.get('pred')
        id = stem.get('name')
        id = id.replace(' ','_')
        tname = TDLencode(id)
        typedef = \
            tname + ' := ' + name + ' & \
                   [ STEM < "' + orthstr + '" >, \
                     SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
        lexicon.add(typedef)
        climb_lex.add(typedef,section='lexicon')
        if circum:
          pform = '< "' + stem.get('prt-form') + '">'
          lexicon.add(tname + ' := [ SYNSEM.LKEYS.KEY-PART ' + pform + ' ].')
          climb_lex.add(tname + ' := [ SYNSEM.LKEYS.KEY-PART ' + pform + ' ].',section='lexicon')


def create_adposition_supertypes(ch, mylang, climb_lex):
###probably not universal

  if ch.get('obj-drop'):
    mylang.add('basic-adposition-lex :+ \
                  [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].',section='addenda')
    climb_lex.add('basic-adposition-lex :+ \
                  [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].',section='addenda')
###general introduced because 'als' (as) in German may combine with N as well as NP

  mylang.add('basic-adposition-lex :+ basic-one-arg.')
  climb_lex.add('basic-adposition-lex :+ basic-one-arg.')
  s_name = 'general-int-adp-lex-item'
  comp = '[ LOCAL.CAT [ HEAD noun, \
                        VAL [ COMPS < >, \
                              SUBJ < >,\
                              SPEC < > ] ] ]'
  mylang.add(s_name + ' := basic-int-mod-adposition-lex & \
                [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                         COMPS < ' + comp + ' > \
                                         SPR < >, \
                                         SPEC < > ] ].')
  mylang.add('int-adp-lex-item := general-int-adp-lex-item & \
         [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.VAL.SPR < > ].')  
  climb_lex.add(s_name + ' := basic-int-mod-adposition-lex & \
                [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                         COMPS < ' + comp + ' > \
                                         SPR < >, \
                                         SPEC < > ] ].')
  climb_lex.add('int-adp-lex-item := general-int-adp-lex-item & \
         [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.VAL.SPR < > ].')  
###adds ARG2, co-indexed with comp  
  mylang.add(s_name + ' := [ SYNSEM [ LKEYS.KEYREL.ARG2 #arg2, \
                  LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX #arg2 ] ].')
  climb_lex.add(s_name + ' := [ SYNSEM [ LKEYS.KEYREL.ARG2 #arg2, \
                  LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX #arg2 ] ].')

  if ch.get('circumpositions') == 'yes':
    comment = '''KEY-PART based on Cramer: way to introduce selected particle's form in lexicon'''
    mylang.add('lexkeys :+ [ KEY-PART list ].',comment=comment,section='addenda')
    comment += '\n ;section=\'addenda\''
    climb_lex.add('lexkeys :+ [ KEY-PART list ].',comment=comment)

    basic_circp = '''basic-circumposition-lex := single-rel-lex-item &
  [ SYNSEM [ LOCAL.CAT [ HEAD adp,
    	     	         VAL.COMPS < [ LOCAL.CONT.HOOK.INDEX #ind ], [ ] > ],
	     LKEYS.KEYREL arg12-ev-relation &
	     		  [ ARG2 #ind ]]].'''
    mylang.add(basic_circp)
    climb_lex.add(basic_circp)
    mylang.add('basic-int-mod-circumposition-lex := intersective-mod-lex & \
                                                     basic-circumposition-lex.')
    climb_lex.add('basic-int-mod-circumposition-lex := intersective-mod-lex & \
                                                     basic-circumposition-lex.')
    circ_name = 'int-circump-lex-item'
    comp2 = '[ LOCAL.CAT [ HEAD verb & [ PART-FORM #prtform ], \
                        VAL [ SPR < >, \
                              COMPS < >, \
                              SUBJ < >,\
                              SPEC < > ] ], \
               OPT - ]'
    mylang.add(circ_name + ' := basic-int-mod-circumposition-lex & \
                [ SYNSEM [ LOCAL.CAT.VAL [ SUBJ < >, \
                                         COMPS < ' + comp + ', ' + comp2 + ' > \
                                         SPR < >, \
                                         SPEC < > ], \
                           LKEYS.KEY-PART #prtform ] ].')
    climb_lex.add(circ_name + ' := basic-int-mod-circumposition-lex & \
                [ SYNSEM [ LOCAL.CAT.VAL [ SUBJ < >, \
                                         COMPS < ' + comp + ', ' + comp2 + ' > \
                                         SPR < >, \
                                         SPEC < > ], \
                           LKEYS.KEY-PART #prtform ] ].')
  
###adds ARG2, co-indexed with comp  
    mylang.add(circ_name + ' := [ SYNSEM [ LKEYS.KEYREL.ARG2 #arg2, \
                  LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX #arg2 ] ].')
    climb_lex.add(circ_name + ' := [ SYNSEM [ LKEYS.KEYREL.ARG2 #arg2, \
                  LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX #arg2 ] ].')

  create_adp_cross_classification(ch, mylang, climb_lex, s_name)
###FOR CONTRACTIONS OF PREPOSITION AND PRONOMINAL COMPLEMENT
###e.g. German "darauf", Dutch "daarop" (on that)
  if ch.get('comp-incl-adp') == 'yes':
    comment = 'introducing adpositions that contain their pronominal \
               complement. They cannot inherit from basic-adp-lex which is a \
               one-rel-lex-item, because it needs to introduce the semantics \
               of its complement.'
####CAREFUL REMOVING INT FROM COMPL-INCL: MUST BE ADDED ELSEWHERE!
    sname2 = 'compl-incl-adp-lex-item' 
    mylang.add(sname2 + ' := norm-hook-lex-item & no-cluster-lex-item & \
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
    climb_lex.add(sname2 + ' := norm-hook-lex-item & no-cluster-lex-item & \
                  [ SYNSEM.LOCAL.CAT [ HEAD adp, \
                                       VAL [ SUBJ < >, \
                                             SPR < >, \
                                             SPEC < >, \
                                             COMPS < > ] ] ].', comment)
    climb_lex.add(sname2 + ' := \
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

    create_adp_cross_classification(ch, mylang, climb_lex, sname2)

  if need_marking_adposition(ch):
    m_adp = \
     '''basic-marking-only-adp-lex := basic-one-arg & 
          [ ARG-ST < #comps &
                      [ LOCAL.CAT [ VAL.SPR < >,
                                    HEAD noun ],
	                            OPT - ] >,
            SYNSEM [ LOCAL.CAT [ VAL [ SPR < >,
                                       SUBJ < >,
                                       COMPS < #comps >,
                                       SPEC < > ],
                                 HEAD adp &
                                        [ MOD < > ] ],
	             NON-LOCAL.SLASH 0-dlist ] ].'''
    mylang.add(m_adp)
    climb_lex.add(m_adp)
    if needs_head_final(ch):
      mylang.add('basic-marking-only-adp-lex := [ SYNSEM.LOCAL.CAT.HEADFINAL - ].')
      climb_lex.add('basic-marking-only-adp-lex := [ SYNSEM.LOCAL.CAT.HEADFINAL - ].')

###For German vom (von + dem) and im (in + dem)
  if ch.get('det-incl-adp') == 'yes':
    sname3 = 'int-adp-integrated-det-lex'
    typedef = \
    '''int-adp-integrated-det-lex-item := no-cluster-lex-item & 
                                         norm-hook-lex-item & basic-one-arg &
  [ SYNSEM [ LKEYS.KEYREL arg12-ev-relation & [ ARG1 #arg1,
						ARG2 #arg2 ],
             NON-LOCAL [ REL 0-dlist,
                         QUE 0-dlist ],
             LOCAL [ CAT [ HEAD adp & [ MOD < [ LOCAL intersective-mod &
                                              [ CONT.HOOK.INDEX #arg1 ] ] > ],
                         VAL [ SUBJ < >,
                               SPR < >,
                               SPEC < >,
                               COMPS < #comp & [ LOCAL [ CAT [ HEAD noun,
                                      VAL [ SPR < [ OPT -,
						    NON-LOCAL.SLASH 0-dlist ] >,
                                                           COMPS < >,
                                                           SUBJ < >,
                                                           SPEC < > ] ],
                                            CONT.HOOK [ INDEX #arg2,
							LTOP #larg ] ] ] > ] ],
                     CONT [ HCONS <! qeq & [ HARG #harg,
                                             LARG #larg ] !>,
                            RELS <! relation, quant-relation &
                                                 [ PRED "_def_q_rel",
                                                   ARG0 #arg2,
                                                   RSTR #harg ] !> ] ] ],
    ARG-ST < #comp > ].'''

    mylang.add(typedef)
    climb_lex.add(typedef)
#    create_adp_cross_classification(ch, mylang, sname3)
    t2 = \
      '''int-prep-integr-det-lex-item := int-adp-integrated-det-lex-item &
               [ SYNSEM.LOCAL.CAT.HEADFINAL - ].'''
    t3 = \
      '''noun-int-adp-integrated-det-lex-item := int-prep-integr-det-lex-item &
                    [ SYNSEM.LOCAL.CAT.HEAD [ MOD.FIRST [ LOCAL.CAT.HEAD noun,
                                                          LIGHT + ],
                                              PRD - ] ].'''
    t4 = \
      '''verb-int-prep-integrated-det-lex-item := int-prep-integr-det-lex-item &
               [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.HEAD verb ].'''

    if needs_head_final(ch):
      mylang.add(t2)
      climb_lex.add(t2)

    mylang.add(t3)
    mylang.add(t4)
    climb_lex.add(t3)
    climb_lex.add(t4)
  return s_name


def customize_particles(ch, mylang, lexicon, climb_lex):

  comment = 'For now, we make particles verbal types (though only particles that are verbal complements can occur in the verbal cluster).'
  super_type = '''basic-verbal-particle-lex := norm-zero-arg &
  [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ < >,
                             COMPS < >,
                             SPR < >,
                             SPEC < > ],
                       HEAD.PART-FORM #prt-form ],
    STEM #prt-form ].'''
  mylang.add('verb :+ [ PART-FORM list ].',section='features')
  mylang.add(super_type, comment)
  mylang.add('verbal-particle-lex := basic-verbal-particle-lex & verb-lex.')
  mylang.add('adp-particle-lex := basic-verbal-particle-lex & \
            [ SYNSEM.LOCAL.CAT.HEAD verb & [ MOD < > ] ].')
  climb_lex.add('verb :+ [ PART-FORM list ].',comment='section=\'features\'')
  climb_lex.add(super_type, comment)
  climb_lex.add('verbal-particle-lex := basic-verbal-particle-lex & verb-lex.')
  climb_lex.add('adp-particle-lex := basic-verbal-particle-lex & \
            [ SYNSEM.LOCAL.CAT.HEAD verb & [ MOD < > ] ].')
 # mylang.add('part-form := form.')

  for part in ch.get('part',[]):
    name = part.get('name')
    hd = part.get('head') 
    mylang.add(name + '-lex := ' + hd + '-particle-lex.')
    climb_lex.add(name + '-lex := ' + hd + '-particle-lex.')
    if part.get('vcluster') == 'no':
      mylang.add(name + '-lex := no-cluster-lex-item.')
      climb_lex.add(name + '-lex := no-cluster-lex-item.')

    for stem in part.get('stem', []):
##only using orth: no multiwords for particles (for now?)
      orth = stem.get('orth')
# OLD ANALYSIS: NOW, PART GETS ITS STEM AS FORM
#
#      form = orth + '-prt'
#      part_id_name = name + '-' + orth + '-lex'
#      mylang.add(form + ' := part-form.')
#      mylang.add(part_id_name + ' := ' + name + '-lex & \
#                   [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')
      
      orthstr = orth_encode(orth)
      orth = orth.replace(' ','_')
      typedef = \
            TDLencode(orth) + ' := ' + name + '-lex & \
                   [ STEM < "' + orthstr + '" > ].'
      lexicon.add(typedef)
      climb_lex.add(typedef,section='lexicon')
      
def need_marking_adposition(ch):
  needed = False
  if ch.get('comparatives') == 'yes' and ch.get('comparative-comp-head') == 'adp':
      needed = True
  elif ch.get('passivization') == 'yes':  
    passive = ch.get('pass',[])
    for p in passive:
      dsubj_mark = p.get('dem-subj-mark')
      if 'adp' in dsubj_mark:
        needed = True

  return needed  

def needs_head_final(ch):
  needed = False
  if ch.get('ldd') == 'yes':
    needed = True
  elif ch.get('vc-analysis') == 'aux-rule' or ch.get('aux-comp-order') == 'both':
    needed = True
  elif ch.get('adp-order') == 'both':
    needed = True

  return needed


def create_adp_cross_classification(ch, mylang, climb_lex, sname):
  
  if ch.get('adp-order') == 'both':
    mylang.add('cat :+ [ HEADFINAL bool ].')
    climb_lex.add('cat :+ [ HEADFINAL bool ].')
    mylang.add('prep-lex-item := basic-adposition-lex & \
              [ SYNSEM.LOCAL.CAT.HEADFINAL - ].')
    mylang.add('postp-lex-item := basic-adposition-lex & \
              [ SYNSEM.LOCAL.CAT.HEADFINAL + ].')
    climb_lex.add('prep-lex-item := basic-adposition-lex & \
              [ SYNSEM.LOCAL.CAT.HEADFINAL - ].')
    climb_lex.add('postp-lex-item := basic-adposition-lex & \
              [ SYNSEM.LOCAL.CAT.HEADFINAL + ].')
  default = ch.get('adp-default')
  exception = ch.get('adp-default-exception')  

  if ch.get('verb-cluster') == 'yes':
    mylang.add(sname + ' := no-cluster-lex-item.')
    climb_lex.add(sname + ' := no-cluster-lex-item.')

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
        climb_lex.add(type_n + ' := ' + sname + ' & \
               [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.HEAD '+ head +' ].') 
        
        for feat in mod.get('feat',[]):
          if feat.get('name') == 'light':
            constr = 'LIGHT ' + feat.get('value')
          mylang.add(type_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.' + constr + ' ].')
          climb_lex.add(type_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.' + constr + ' ].')
 
        if not 'incl' in type_n:
          if both_orders and not 'circum' in type_n:
            type_npre = type_n.replace('adp', 'prep')
            type_npost = type_n.replace('adp', 'postp')
            mylang.add(type_npre + ' := prep-lex-item & ' + type_n + '.')
            mylang.add(type_npost + ' := postp-lex-item & ' + type_n + '.')
            climb_lex.add(type_npre + ' := prep-lex-item & ' + type_n + '.')
            climb_lex.add(type_npost + ' := postp-lex-item & ' + type_n + '.')
          elif default:
            mylang.add(type_n + ' := ' + default + '-lex-item.')
            climb_lex.add(type_n + ' := ' + default + '-lex-item.')

    elif spadp.get('kind') == 'prd':
      type_n = 'prd-' + sname
      mylang.add(type_n + ' := ' + sname + ' & \
               [ SYNSEM [ LOCAL.CONT.HOOK.XARG #arg1, \
                          LKEYS.KEYREL.ARG1 #arg1 ] ].')
      climb_lex.add(type_n + ' := ' + sname + ' & \
               [ SYNSEM [ LOCAL.CONT.HOOK.XARG #arg1, \
                          LKEYS.KEYREL.ARG1 #arg1 ] ].')
      if not 'incl' in type_n:
        if 'prd' in exception:
          type_npre = type_n.replace('adp', 'prep')
          type_npost = type_n.replace('adp', 'postp')
          mylang.add(type_npre + ' := prep-lex-item & ' + type_n + '.')
          mylang.add(type_npost + ' := postp-lex-item & ' + type_n + '.')
          climb_lex.add(type_npre + ' := prep-lex-item & ' + type_n + '.')
          climb_lex.add(type_npost + ' := postp-lex-item & ' + type_n + '.')
        elif default:
          mylang.add(type_n + ' := ' + default + '-lex-item.')
          climb_lex.add(type_n + ' := ' + default + '-lex-item.')

       
def customize_complementizers(ch, mylang, lexicon, climb_lex):
  if ch.get('has-compl') == 'yes':
  ###create section in lexicon
    lexicon.add_literal(';;; Complementizers')
    climb_lex.add_literal(';;; Complementizers')
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
    climb_lex.add('complementizer-lex-item := raise-sem-lex-item & \
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
    if ch.get('cp-at-np') == 'yes':
      cases = ch.get('cp-cases')
      mylang.add('complementizer-lex-item := \
                  [ SYNSEM.LOCAL.CAT.HEAD.CASE ' + cases + ' ].')
      climb_lex.add('complementizer-lex-item := \
                  [ SYNSEM.LOCAL.CAT.HEAD.CASE ' + cases + ' ].')
    for compl in ch.get('compl',[]):
      sf = compl.get('sf')
      my_type = compl.get('name')
      mylang.add(my_type + ' := complementizer-lex-item & \
         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ' + sf + ' ].')
      climb_lex.add(my_type + ' := complementizer-lex-item & \
         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ' + sf + ' ].')
      
      for stem in compl.get('stem'):
        orth = stem.get('orth')
        orthstr = orth_encode(orth)
        orth = orth.replace(' ','_')
        typedef = \
            TDLencode(orth) + ' := ' + my_type + ' & \
                   [ STEM < "' + orthstr + '" > ].'
        lexicon.add(typedef)
        climb_lex.add(typedef, section='lexicon')


def customize_misc_lex(ch, lexicon, climb_lex):

  #lexicon.add_literal(';;; Other')

  # Question particle
  if ch.get('q-part'):
    orth = ch.get('q-part-orth')
    orthstr = orth_encode(orth)
    orth = orth.replace(' ','_')
    typedef = \
      TDLencode(orth) + ' := qpart-lex-item & \
                   [ STEM < "' + orthstr + '" > ].'
    lexicon.add(typedef)
    climb_lex.add(typedef, section='lexicon')

def customize_nouns(mylang, ch, lexicon, hierarchies, climb_nouns):
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
  climb_nouns.add(typedef) 
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
    climb_nouns.add(typedef1)
    climb_nouns.add(typedef2)
  # Assuming that most nouns typically do not modify
  # until compounds have been added
 
  mylang.add('noun-lex := general-noun-lex & \
         [ SYNSEM.LOCAL.CAT [ HEAD.MOD < >, \
                              VAL.SPR < #spr & [ LOCAL.CAT.HEAD det ] > ], \
           ARG-ST < #spr > ].')
  climb_nouns.add('noun-lex := general-noun-lex & \
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
    climb_nouns.add('compound-local := local.')
    climb_nouns.add('compound-allowing-noun-lex := basic-noun-lex & \
                 [ SYNSEM.LOCAL compound-local ].')
    climb_nouns.add('compound-noun-lex := compound-allowing-noun-lex & \
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
    climb_nouns.add(typedef, comment)
    if ch.get('v2-analysis') == 'mc':
      mylang.add('non-sem-noun-lex := [ SYNSEM.NON-LOCAL.SLASH 0-dlist & \
                                                               [ LIST < > ] ].')
      climb_nouns.add('non-sem-noun-lex := [ SYNSEM.NON-LOCAL.SLASH 0-dlist & \
                                                               [ LIST < > ] ].')
    if ch.get('explitives') == 'yes':
      expltype = \
           'expl-noun-lex := non-sem-noun-lex & \
             [ SYNSEM [ LOCAL [ CONT.HOOK.INDEX expl-ind, \
                                CAT.HEAD.CASE nom ], \
                        NON-LOCAL.REL 0-dlist ] ].' 
      mylang.add(expltype)
      climb_nouns.add(expltype)
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
      climb_nouns.add(relprn)
      if ch.get('wh-rel') or ch.get('non-wh-rel'):
        mylang.add('wh-rel-pronoun-lex := rel-pronoun-lex & \
                     [ INFLECTED infl-wh ].')
        mylang.add('non-wh-rel-pronoun-lex := rel-pronoun-lex & \
             [ INFLECTED infl-non-wh ].')
        climb_nouns.add('wh-rel-pronoun-lex := rel-pronoun-lex & \
                     [ INFLECTED infl-wh ].')
        climb_nouns.add('non-wh-rel-pronoun-lex := rel-pronoun-lex & \
             [ INFLECTED infl-non-wh ].')
      if refl:
        mylang.add('rel-local := rel-non-refl-local.',section='features')
        climb_nouns.add('rel-local := rel-non-refl-local.',section='features')
      else:
        mylang.add('rel-local := local.',section='features')
        climb_nouns.add('rel-local := local.',section='features')
    if refl:
      refl_prn = \
       '''reflexive-noun-lex := non-sem-noun-lex &
            [ SYNSEM.LOCAL refl-local ].'''
      mylang.add(refl_prn)
      climb_nouns.add(refl_prn)
      if ch.get('wh-questions') == 'yes':
        mylang.add('reflexive-noun-lex := non-wh-or-rel-lex-item.')
        climb_nouns.add('reflexive-noun-lex := non-wh-or-rel-lex-item.')

      mylang.add('refl-local := local.', 'Distinguishing reflexive nouns from non-reflexive nouns.', section='features')
      climb_nouns.add('refl-local := local.', 'Distinguishing reflexive nouns from non-reflexive nouns.', section='features')
      if ch.get('rel-clause') == 'yes':
        mylang.add('rel-non-refl-local := local.', section='features')
        mylang.add('non-refl-local := rel-non-refl-local.') 
        climb_nouns.add('rel-non-refl-local := local.', section='features')
        climb_nouns.add('non-refl-local := rel-non-refl-local.') 
      else:
        mylang.add('non-refl-local := local.', section='features')   
        climb_nouns.add('non-refl-local := local.', section='features')   
      mylang.add('non-reflexive-noun-lex := basic-noun-lex & \
                              [ SYNSEM.LOCAL non-refl-local ].')
      mylang.add('noun-lex := non-reflexive-noun-lex.')   
      climb_nouns.add('non-reflexive-noun-lex := basic-noun-lex & \
                              [ SYNSEM.LOCAL non-refl-local ].')
      climb_nouns.add('noun-lex := non-reflexive-noun-lex.')
  mylang.add('non-rel-lex-item := lex-item & \
                                        [ SYNSEM.NON-LOCAL.REL 0-dlist ].')
  climb_nouns.add('non-rel-lex-item := lex-item & \
                                        [ SYNSEM.NON-LOCAL.REL 0-dlist ].')
  if ch.get('wh-questions') == 'yes':
    mylang.add('non-wh-or-rel-lex-item := non-rel-lex-item & non-wh-lex-item.')
    climb_nouns.add('non-wh-or-rel-lex-item := non-rel-lex-item & non-wh-lex-item.')
  
  if ch.get('mod-noun') == 'yes':
    mylang.add('mod-noun-lex := general-noun-lex & \
                [ SYNSEM.LOCAL.CAT [ HEAD.MOD < [ ] >, \
                                  VAL.SPR < #spr & [ LOCAL.CAT.HEAD det ] > ], \
                  ARG-ST < #spr > ].')
    climb_nouns.add('mod-noun-lex := general-noun-lex & \
                [ SYNSEM.LOCAL.CAT [ HEAD.MOD < [ ] >, \
                                  VAL.SPR < #spr & [ LOCAL.CAT.HEAD det ] > ], \
                  ARG-ST < #spr > ].')
    if refl:
      mylang.add('mod-noun-lex := non-reflexive-noun-lex.')
      climb_nouns.add('mod-noun-lex := non-reflexive-noun-lex.')

###creating basic type for nouns that don't take specifier, but have meaning
###for them included
  spr_incl = []
  if ch.get('n_spec_spr') == 'yes':
    mylang.add('spr_incl_noun := noun.')
    mylang.add('mass_cnt_noun := noun.')
    climb_nouns.add('spr_incl_noun := noun.')
    climb_nouns.add('mass_cnt_noun := noun.')
    type_n = '''basic-spr-incl-noun-lex := norm-hook-lex-item &
                 [ SYNSEM [ LOCAL [ CONT [ RELS <! relation, #altkey &
                                                          [ ARG0 #index ] !>,
                                           HOOK.INDEX #index ],
                                    CAT [ HEAD spr_incl_noun & [ MOD < > ],
                                          VAL [ SUBJ < >,
                                                SPR <  >,
                                                COMPS < >,
			                        SPEC < > ] ] ],
	                    NON-LOCAL.QUE 0-dlist,
                            LKEYS.ALTKEYREL relation & #altkey ] ].'''
    mylang.add(type_n)
    climb_nouns.add(type_n)

    if 'pronoun' in ch.get('spr-incl-kind'):
      spr_incl.append('pronoun')
      pro_type = '''basic-pronoun-lex := basic-spr-incl-noun-lex & \
                     [ SYNSEM.LOCAL.CONT [ RELS.LIST [ FIRST.LBL #nhand,
                                              REST.FIRST [ PRED "_pron_q_rel",
                                                  RSTR #rhand ] ],
                                       HCONS <! qeq & [ HARG #rhand,
                                                       LARG #nhand ] !> ] ].'''
      mylang.add(pro_type)
      climb_nouns.add(pro_type)
    if 'indef' in ch.get('spr-incl-kind'):
      spr_incl.append('indef')
      indef_type = '''basic-indef-noun-lex := basic-spr-incl-noun-lex & \
                     [ SYNSEM.LOCAL.CONT [ RELS.LIST [ FIRST.LBL #nhand,
                                              REST.FIRST [ PRED "_indef_q_rel",
                                                  RSTR #rhand ] ],
                                       HCONS <! qeq & [ HARG #rhand,
                                                       LARG #nhand ] !> ] ].'''
      mylang.add(indef_type)
      climb_nouns.add(indef_type)
    if 'neg' in ch.get('spr-incl-kind'):
      spr_incl.append('neg')
      neg_type = '''basic-neg-noun-lex := basic-spr-incl-noun-lex & \
                     [ SYNSEM.LOCAL.CONT [ RELS.LIST [ FIRST.LBL #nhand,
                                              REST.FIRST [ PRED "_kein_q_rel",
                                                  RSTR #rhand ] ],
                                       HCONS <! qeq & [ HARG #rhand,
                                                       LARG #nhand ] !> ] ].'''
      mylang.add(neg_type)
      climb_nouns.add(neg_type)
    if 'every' in ch.get('spr-incl-kind'):
      spr_incl.append('every')
      indef_type = '''basic-every-noun-lex := basic-spr-incl-noun-lex & \
                     [ SYNSEM.LOCAL.CONT [ RELS.LIST [ FIRST.LBL #nhand,
                                              REST.FIRST [ PRED "_jed_q_rel",
                                                  RSTR #rhand ] ],
                                       HCONS <! qeq & [ HARG #rhand,
                                                       LARG #nhand ] !> ] ].'''
      mylang.add(indef_type)
      climb_nouns.add(indef_type)
    if 'ander' in ch.get('spr-incl-kind'):
      spr_incl.append('ander')
      ander_type = '''basic-ander-noun-lex := basic-spr-incl-noun-lex & \
                     [ SYNSEM.LOCAL.CONT [ RELS.LIST [ FIRST.LBL #nhand,
                                              REST.FIRST [ PRED "_ander_q_rel",
                                                  RSTR #rhand ] ],
                                       HCONS <! qeq & [ HARG #rhand,
                                                       LARG #nhand ] !> ] ].'''
      mylang.add(ander_type)
      climb_nouns.add(ander_type)
  if ch.get('verb-cluster') == 'yes':
    mylang.add('general-noun-lex := no-cluster-lex-item.')
    climb_nouns.add('general-noun-lex := no-cluster-lex-item.')

  if singlentype:
    if seen['obl']:
      typedef = 'noun-lex := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
      mylang.add(typedef)
      climb_nouns.add(typedef)
    elif seen['imp']:
      typedef = 'noun-lex := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
      mylang.add(typedef)
      climb_nouns.add(typedef)
  else:
    if seen['obl']:
      typedef = \
        'obl-spr-noun-lex := noun-lex & \
           [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
      mylang.add(typedef)
      climb_nouns.add(typedef)

    if seen['imp']:
      typedef = \
          'no-spr-noun-lex := noun-lex & \
             [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
      mylang.add(typedef)
      climb_nouns.add(typedef)
      if ch.get('wh-det') == 'on':
        mylang.add('no-spr-noun-lex := non-wh-or-rel-lex-item.')
        climb_nouns.add('no-spr-noun-lex := non-wh-or-rel-lex-item.')

  if seen['imp'] and ch.get('has-dets') == 'yes':
    mylang.add(
      'head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.OPT - ].',
      'Nouns which cannot take specifiers mark their SPR requirement\n' +
      'as OPT +.  Making the non-head daughter OPT - in this rule\n' +
      'keeps such nouns out.')
    climb_nouns.add(
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
      climb_nouns.add('noun :+ [ CASE case ].', section='addenda')

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
###

  for noun in ch.get('noun',[]):
    name = get_name(noun)
    det = noun.get('det')
    wh = noun.get('wh')
    rel = noun.get('rel-pn')
    sub_rel = noun.get('sub-rel')
    expl = noun.get('expl')
    compound = noun.get('compound')
    pers_n = noun.get('pers-name')
    arg_st = ''
    kind = noun.get('kind')
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
      if sub_rel:
        stype = sub_rel + '-rel-pronoun-lex'
    elif kind == 'pronoun':
      stype = 'basic-pronoun-lex'
    elif kind == 'indef':
      stype = 'basic-indef-noun-lex'
    elif kind == 'neg':
      stype = 'basic-neg-noun-lex'
    elif kind == 'every':
      stype = 'basic-every-noun-lex'
    elif kind == 'ander':
      stype = 'basic-ander-noun-lex'
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
        climb_nouns.add(ntype + ':= [ SYNSEM.LOCAL.CAT.VAL.SPR < #spr >, \
                               ARG-ST < #spr & [ LOCAL.CAT.HEAD det ], [ ]> ].')
###BROKEN: optional determiner doesn't allow any specifier
###FIXED, misunderstanding on working of 'OPT': should make comment for grammar
###in customization system
        if det == 'imp': 
          mylang.add(ntype + ':= [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].')
          climb_nouns.add(ntype + ':= [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].')
      mylang.add(ntype + ' := [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
                                   ARG-ST < [ ], \
                                           #comps & \
                                   [ LOCAL [ CAT [ VAL [ COMPS < >, \
                                                         SPR < >, \
                                                         SPEC < > ] ] ], \
	                            OPT + ] > ].')
      climb_nouns.add(ntype + ' := [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
                                   ARG-ST < [ ], \
                                           #comps & \
                                   [ LOCAL [ CAT [ VAL [ COMPS < >, \
                                                         SPR < >, \
                                                         SPEC < > ] ] ], \
	                            OPT + ] > ].')
      if not arg_st == 'zuinf':
        mylang.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CAT.VAL.SUBJ < > ] > ].')
        climb_nouns.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CAT.VAL.SUBJ < > ] > ].')
      if 'comp' in arg_st:
        mylang.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CAT.HEAD comp ] > ].')
        climb_nouns.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CAT.HEAD comp ] > ].')
        if arg_st == 'scomp-prop':
           mylang.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CONT.HOOK.INDEX.SF prop ] > ].')
           climb_nouns.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CONT.HOOK.INDEX.SF prop ] > ].')
        elif arg_st == 'qcomp': 
           mylang.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CONT.HOOK.INDEX.SF ques, \
                            NON-LOCAL.SLASH 0-dlist ] > ].')
           climb_nouns.add(ntype + ' := [ ARG-ST < [ ], \
                          [ LOCAL.CONT.HOOK.INDEX.SF ques, \
                            NON-LOCAL.SLASH 0-dlist ] > ].')
      elif arg_st == 'adp':
        if ch.get('verb-cross-classification') == 'yes':
          mylang.add(ntype + ' := [ ARG-ST < [ ], \
                                [ LOCAL.CAT.HEAD adp & \
                                   [ MOD < [ LOCAL.CAT.HEAD noun ] >, \
                                     FORM #pform ] ] >, \
                                 SYNSEM.LKEYS.KEY-ADP #pform ].')
          climb_nouns.add(ntype + ' := [ ARG-ST < [ ], \
                                [ LOCAL.CAT.HEAD adp & \
                                   [ MOD < [ LOCAL.CAT.HEAD noun ] >, \
                                     FORM #pform ] ] >, \
                                 SYNSEM.LKEYS.KEY-ADP #pform ].')
        else:
          mylang.add(ntype + ' := [ ARG-ST < [ ], \
                                [ LOCAL.CAT.HEAD adp & \
                                   [ MOD < [ LOCAL.CAT.HEAD noun ] > ] ] > ].')
          climb_nouns.add(ntype + ' := [ ARG-ST < [ ], \
                                [ LOCAL.CAT.HEAD adp & \
                                   [ MOD < [ LOCAL.CAT.HEAD noun ] > ] ] > ].')
      elif arg_st == 'zuinf':
        mylang.add(ntype + ' := [ ARG-ST < [ ], \
                                           [ LOCAL [ CAT [ HEAD verb & [ FORM zuinf ], \
                                                           VAL.SUBJ < [ ] > ], \
                                                     CONT.HOOK.XARG #xarg ] ] >, \
                                   SYNSEM.LOCAL.CONT.HOOK.XARG #xarg ].')
        climb_nouns.add(ntype + ' := [ ARG-ST < [ ], \
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
      climb_nouns.add(ntype + ' := intersective-mod-lex & \
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
      climb_nouns.add(ntype + ' := ' + stype + '.')

      if pers_n:
        mylang.add(ntype + ' := [ SYNSEM.LKEYS.KEYREL named-relation ].')
        climb_nouns.add(ntype + ' := [ SYNSEM.LKEYS.KEYREL named-relation ].')

    if ch.get('n_spec_spr') == 'yes':
      if kind in spr_incl:
        n_head = 'spr_incl_noun'
      else:
        n_head = 'mass_cnt_noun'
      if not expl:
        mylang.add(ntype + ' := [ SYNSEM.LOCAL.CAT.HEAD ' + n_head + ' ].')
        climb_nouns.add(ntype + ' := [ SYNSEM.LOCAL.CAT.HEAD ' + n_head + ' ].')

    features.customize_feature_values(mylang, ch, hierarchies, noun, ntype, 'noun', climbfile=climb_nouns)

    

    for stem in noun.get('stem', []):
      orthstr = orth_encode(stem.get('orth'))
      pred = stem.get('pred')
      id = stem.get('name')
      id = id.replace(' ','_')
      if expl == 'yes':
        typedef = TDLencode(id) + ' := expl-noun-lex & \
                    [ STEM < "' + orthstr + '" > ].'
      elif rel == 'yes' or n_refl == 'obl':
        typedef = TDLencode(id) + ' :=  ' + ntype + ' & \
                    [ STEM < "' + orthstr + '" > ].'
      elif pers_n:
        typedef = TDLencode(id) + ' := ' + ntype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.CARG "' + pred + '" ].'
      else:
        typedef = TDLencode(id) + ' := ' + ntype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)
      climb_nouns.add(typedef, section='lexicon')

######################################################################
# create_wh_phrases()
# Create basic types and supertypes to get wh-words to work
# Specific subtypes are created in associated libraries
#

def create_wh_phrases(mylang, climb_wh, ch):
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
  climb_wh.add(basic_wh_type)

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
    climb_wh.add(bwlnsl)
    wal = 'wh-adverb-lex := basic-wh-loc-non-sp-lex & \
           [ SYNSEM [ LOCAL.CAT [ HEAD +rp, \
                                VAL [ SUBJ < >, \
			              COMPS < >, \
			              SPEC < >, \
			              SPR < > ] ], \
                      NON-LOCAL.SLASH 0-dlist ] ].'
    mylang.add(wal)
    climb_wh.add(wal)

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
    climb_wh.add(bwlsimple)
 
###future work: cross-classification between wh- and rel- words  
    wh_noun = \
      'wh-noun-lex := basic-wh-simple-sem-lex & non-rel-lex-item & \
                 [ SYNSEM [ LOCAL.CAT.HEAD noun & [ MOD < > ], \
                            LKEYS.KEYREL noun-relation ] ].'
    mylang.add(wh_noun)
    climb_wh.add(wh_noun)
  if ch.get('wh-det') == 'on':
    wh_det = \
     'wh-determiner-lex := basic-determiner-lex & zero-arg-nonslash & \
       [ SYNSEM [ LOCAL.CAT.VAL [ SUBJ < >, \
                                  COMPS < >, \
                                  SPR < > ], \
                  NON-LOCAL.QUE 1-dlist ] ].'
    mylang.add(wh_det)
    climb_wh.add(wh_det)
    if ch.get('rel-clause') == 'yes':
      mylang.add('wh-determiner-lex := non-rel-lex-item.')
      climb_wh.add('wh-determiner-lex := non-rel-lex-item.')
  mylang.add('non-wh-lex-item := lex-item & [ SYNSEM.NON-LOCAL.QUE 0-dlist].')
  mylang.add('basic-adjective-lex :+ non-wh-lex-item.')
  mylang.add('basic-adverb-lex :+ non-wh-lex-item.')
  mylang.add('basic-adposition-lex :+ \
             [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comp & [ LOCAL.CAT.VAL.SPR < > ] >, \
               ARG-ST < #comp > ].')
  mylang.add('determiner-lex := non-wh-lex-item.') 
  climb_wh.add('non-wh-lex-item := lex-item & [ SYNSEM.NON-LOCAL.QUE 0-dlist].')
  climb_wh.add('basic-adjective-lex :+ non-wh-lex-item.')
  climb_wh.add('basic-adverb-lex :+ non-wh-lex-item.')
  climb_wh.add('basic-adposition-lex :+ \
             [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comp & [ ] >, \
               ARG-ST < #comp > ].')
  climb_wh.add('determiner-lex := non-wh-lex-item.') 


######################################################################
# customize_lexicon()
#   Create the type definitions associated with the user's test
#   lexicon.



def customize_lexicon(mylang, ch, lexicon, hierarchies, lrules, rules, climb_files):

  climb_lex = climb_files.get('lexical_items')
  climb_lex.set_section('mylang')
  comment = '''Type assigning empty mod list. Added to basic types for nouns, verbs and determiners.'''
  mylang.add('non-mod-lex-item := lex-item & \
               [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].',comment)

  climb_lex.add('non-mod-lex-item := lex-item & \
               [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].',comment)

  mylang.set_section('nounlex')
  climb_nouns = climb_files.get('nouns')
  climb_nouns.set_section('mylang')
  customize_nouns(mylang, ch, lexicon, hierarchies, climb_nouns)

  mylang.set_section('otherlex')
  climb_case = climb_files.get('case')
  to_cfv = case.customize_case_adpositions(mylang, lexicon, climb_case, ch)
  climb_case.set_section('lexicon')
  features.process_cfv_list(mylang, ch, hierarchies, to_cfv, tdlfile=lexicon, climbfile=climb_case)
  climb_case.set_section('mylang')

  mylang.set_section('verblex')
  climb_verbs = climb_files.get('verbs')
  climb_verbs.set_section('mylang')
  customize_verbs(mylang, ch, lexicon, hierarchies, climb_verbs)
  ####CLIMB PAUZE
  if ch.get('has-aux') == 'yes':
    mylang.set_section('auxlex')
    climb_aux = climb_files.get('aux')
    climb_aux.set_section('mylang')
    auxiliaries.customize_auxiliaries(mylang, climb_aux, ch, lexicon, hierarchies)
  if ch.get('has-cop') == 'yes':
    climb_cop = climb_files.get('cop')
    climb_cop.set_section('mylang')
    customize_copula(mylang, climb_cop, ch, lexicon, hierarchies)
  
  mylang.set_section('otherlex')
  if ch.get('wh-questions') == 'yes':
    climb_wh = climb_files.get('wh')
    climb_wh.set_section('mylang')
    create_wh_phrases(mylang, climb_wh, ch)  
  if ch.get('numbers') == 'yes':
    customize_numbers(ch, mylang, lexicon, lrules, climb_lex, hierarchies)

  customize_determiners(mylang, ch, lexicon, climb_lex, hierarchies)
  customize_adjectives(mylang, ch, lexicon, rules, climb_lex, hierarchies)
  customize_adverbs(mylang, ch, lexicon, climb_lex)
  customize_adpositions(ch, mylang, lexicon, climb_lex, hierarchies)
  if ch.get('verbal-particles') == 'yes' or ch.get('circumpositions') == 'yes':
    customize_particles(ch, mylang, lexicon, climb_lex)
  customize_complementizers(ch, mylang, lexicon, climb_lex) 
  customize_misc_lex(ch, lexicon, climb_lex)
