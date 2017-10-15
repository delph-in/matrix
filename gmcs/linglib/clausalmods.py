from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy

######################################################################
# Clausal Modifiers
#   Create the type definitions associated with the user's choices
#   about clasual modification.

######################################################################


def add_subord_lex(mylang, lexicon, cms):
  """
  add the type definition for the lexical item to mylang
  and the lexical entries to lexicon
  """
  mylang.set_section('addenda')
  mylang.add('head :+ [ INIT bool ].')

  # [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL scopal-mod &\
	# 				      [ CAT [ HEAD verb,\
	# 					      VAL [ SUBJ < >,\
	# 						    SPR < >,\
	# 						    COMPS < > ]],\
	# 					CONT.HOOK [ LTOP #mod,\
	# 						    INDEX #index ]]] >,\
	# 		   VAL.COMPS < [ LOCAL [ CAT [ VAL [ SUBJ < >,\
	# 						      SPR < > ]]]] > ],\
	# 	     CONT [ HCONS <! qeq & \
	# 			 [ HARG #h1,\
	# 			   LARG #mod ] !>,\
	# 		    HOOK.INDEX #index ]],\
	#      LKEYS.KEYREL [ ARG1 #h1 ]]].\\')
  # add lexical type. if nomminalization is on, restrict the complement to be a nominalized verb otherwise, verb
  if cms.get('nominalization') == 'on':
    mylang.set_section('noun-lex')
    mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
    mylang.set_section('subordlex')
    mylang.add('scopal-mod-with-comp-lex := single-rel-lex-item & norm-ltop-lex-item &\
      [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL scopal-mod &\
               [CAT[HEAD verb,\
    VAL[SUBJ < >,\
        SPR < >,\
        COMPS < >]],\
    CONT.HOOK[LTOP  # mod,\
    INDEX  # index ]]] >,\
    VAL.COMPS < [LOCAL[CAT[HEAD noun,\
                 VAL[SUBJ < >,\
                     SPR < >,\
                     COMPS < >]],\
    CONT.HOOK.INDEX  # h2 ]] > ],\
    CONT[HCONS <! qeq &\
                  [HARG  # h1,\
                   LARG  # mod ] !>,\
                   HOOK.INDEX  # index ]],\
                   LKEYS.KEYREL[ARG0 event,\
                   ARG1  # h1,\
                   ARG2  # h2 ]]].')
    mylang.add('subord-lex-item := scopal-mod-with-comp-lex &\
        [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ < >,\
      			     SPR < > ],\
          ARG-ST < [ LOCAL.CAT [ HEAD noun & [ NMZ + ],\
                               MC - ]] > ].\\')
  else:
    mylang.set_section('subordlex')
    mylang.add('scopal-mod-with-comp-lex := single-rel-lex-item & norm-ltop-lex-item &\
            [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL scopal-mod &\
					      [ CAT [ HEAD verb,\
						      VAL [ SUBJ < >,\
							    SPR < >,\
							    COMPS < > ]],\
						CONT.HOOK [ LTOP #mod,\
							    INDEX #index ]]] >,\
			   VAL.COMPS < [ LOCAL [ CAT [ HEAD verb,\
							VAL [ SUBJ < >,\
							      SPR < >,\
							      COMPS < > ]],\
						  CONT.HOOK.LTOP #comps ]] > ],\
		     CONT [ HCONS <! qeq & \
				 [ HARG #h1,\
				   LARG #mod ],\
				 qeq &\
				 [ HARG #h2,\
				   LARG #comps ] !>,\
			    HOOK.INDEX #index ]],\
	     LKEYS.KEYREL [ ARG1 #h1,\
			    ARG2 #h2 ]]].')
    mylang.add('subord-lex-item := scopal-mod-with-comp-lex &\
    [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ < >,\
                                SPR < >,\
                                COMPS <  #comps > ]]],\
      ARG - ST <  #comps &\
                  [LOCAL.CAT[ HEAD verb,\
                              MC -]] > ].')

  pos = cms.get('position')
  subpos = cms.get('subposition')
  lextype = ''
  constraints = ''
  if subpos == 'before':
      lextype += 'clause-init'
      constraints += '[ SYNSEM.LOCAL.CAT [ HEAD.INIT +'
  elif subpos == 'after':
      lextype += 'clause-final'
      constraints += '[ SYNSEM.LOCAL.CAT [ HEAD.INIT -'
  if pos == 'before':
      lextype += '-prehead'
      constraints += ', POSTHEAD -'
  elif pos == 'after':
      lextype += '-posthead'
      constraints += ', POSTHEAD +'
  if cms.get('specialmorph') == 'on':
      for feat in cms.get('feat'):
          lextype += '-' + feat.get('value')
          #is this a problem? can the feature be in not head?
          constraints += (', SYNSEM.LOCAL.CAT.HEAD.' + feat.get('name') + '.' + feat.get('value'))
  lextype += '-subord-lex-item'
  constraints += ' ]].'
  print(lextype)
  print(constraints)
  mylang.add(lextype + ' := subord-lex-item &' + constraints)
  for freemorph in cms.get('freemorph'):
      orth = freemorph.get('orth')
      orthstr = orth_encode(orth)
      pred = freemorph.get('pred')
      lexicon.add(orthstr + ':= ' + lextype + ' &\
                              [ STEM < "' + orthstr + '" >,\
                           SYNSEM.LKEYS.KEYREL.PRED "' + pred + '"].\\')


def add_subord_phrasal_types(mylang, rules, cms):
  """
  Add the phrase type definitions
  """
  pos = cms.get('position')
  subpos = cms.get('subposition')
  mylang.set_section('phrases')
  if pos == 'before':
    rules.add('adj-head-scop := adj-head-scop-phrase.')
  elif pos == 'after':
    rules.add('head-adj-scop := head-adj-scop-phrase.')
  elif pos == 'either':
    rules.add('adj-head-scop := adj-head-scop-phrase.')
    rules.add('head-adj-scop := head-adj-scop-phrase.')
  mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial &\
  [HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT + ].')
  mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final&\
      [HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT - ].')
  rules.add('head-comp := head-comp-phrase.')
  rules.add('comp-head := comp-head-phrase.')
  #rules.add('subj-head := subj-head-phrase.')

def add_subordinator_pair_to_lexicon(lexicon, matrixtype, subordtype, pair):
  """
  Adds each member of a subordinator pair to the lexicon
  """
  if matrixtype == 'adv':
    lexicon.add(pair.get('matrixorth') + ':= subpair-adv-lex-item &\
  [ STEM < "' + pair.get('matrixorth') + '" >,\
    SYNSEM.LKEYS.KEYREL.PRED "' + pair.get('matrixpred') + '" ].')
  #elif matrixtype == 'comp'

  if subordtype == 'comp':
    lexicon.add(pair.get('subordorth') + ':= subpair-adv-lex-item &\
     [ STEM < "' + pair.get('subordorth') + '" >,\
       SYNSEM.LKEYS.KEYREL.PRED "' + pair.get('subordpred') + '" ].')
  #elif subordtype == 'adv':

def create_subpair_feature(mylang, morphpair):
  """
  adds the subpair feature to canonical synsem as well as adding constraints
  to phrase types to pass it and mc up
  """
  mylang.set_section('addenda')
  mylang.add('canonical-synsem :+ [ SUBPAIR subpair ].')
  mylang.add('basic-head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.MC #mc,\
    HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
  mylang.add('basic-head-mod-phrase-simple :+    [ SYNSEM.LOCAL.CAT.MC #mc,\
      NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.MOD < [ SUBPAIR #subpair ] >,\
				      MC #mc ],\
      HEAD-DTR.SYNSEM.SUBPAIR #subpair ].')

  mylang.add('basic-head-opt-subj-phrase :+ [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
  mylang.add('basic-head-spec-phrase-super :+ [ SYNSEM.SUBPAIR #subpair,\
				  HEAD-DTR.SYNSEM.SUBPAIR #subpair,\
				  NON-HEAD-DTR.SYNSEM [ SUBPAIR #pair,\
							LOCAL.CAT.VAL [ SPEC < [ SUBPAIR #pair ] > ]]].')
  mylang.add('basic-head-opt-comp-phrase :+ [ SYNSEM.SUBPAIR #subpair,\
				HEAD-DTR.SYNSEM.SUBPAIR #subpair ].')
  mylang.add('basic-head-opt-subj-phrase :+ [ SYNSEM.SUBPAIR #subpair,\
				HEAD-DTR.SYNSEM.SUBPAIR #subpair ].')
  mylang.add('adj-head-scop-phrase :+ [ SYNSEM.SUBPAIR #subpair,\
			  NON-HEAD-DTR.SYNSEM.SUBPAIR #subpair ].')
  mylang.add('adj-head-phrase :+ [ SYNSEM.SUBPAIR #subpair,\
		     NON-HEAD-DTR.SYNSEM.SUBPAIR #subpair ].')

  mylang.set_section('features')
  #mylang.add(';;; Subordinator Pair Features')
  mylang.add('subpair := *top*.')
  mylang.add('nopair := subpair.')
  for pair in morphpair:
    subpair = pair.get('subordpred')
    mylang.add(subpair + ' := subpair.')

# def add_pair_lex_items(mylang):
#   """
#   Adds the lexical types for subordinator pairs
#   """

def customize_clausalmods(mylang, ch, lexicon, rules, irules):
  """
  The main clausal modifier customization routine
  """
  mylang.add('+nvcdmo :+ [ MOD < > ].')

  for cms in ch.get('cms'):
    cmsnum = str(cms.iter_num())

    subord = cms.get('subordinator')


    if subord == 'free':
      add_subord_lex(mylang, lexicon, cms)
      add_subord_phrasal_types(mylang, rules, cms)

    if subord == 'pair':
      matrixtype = cms.get('matrixtype')
      subordtype = cms.get('subordtype')
      create_subpair_feature(mylang, cms.get('morphpair'))
      for pair in cms.get('morphpair'):
        add_subordinator_pair_to_lexicon(lexicon, matrixtype, subordtype, pair)
      #add_pair_lex_items()
      #add_pair_phrasal_types()