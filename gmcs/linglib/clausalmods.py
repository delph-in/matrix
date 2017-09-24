from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy

######################################################################
# Clausal Modifiers
#   Create the type definitions associated with the user's choices
#   about clasual modification.

######################################################################

def add_free_subordinator_to_lexicon(lexicon, freemorph, pos, subpos):
  """
  Add free subordinators to the lexicon
  """

  orth = freemorph.get('orth')
  orthstr = orth_encode(orth)
  pred = freemorph.get('pred')
  #todo add all options
  if pos == 'before':
    if subpos == 'before':
      lextype = 'clause-init-prehead-subord-lex-item'
    elif subpos == 'after':
      lextype = 'clause-final-prehead-subord-lex-item'
  elif pos == 'after':
    if subpos == 'before':
      lextype = 'clause-init-posthead-subord-lex-item'
    elif subpos == 'after':
      lextype = 'clause-final-posthead-subord-lex-item'
  elif pos == 'either':
    if subpos == 'before':
      lextype = 'clause-init-subord-lex-item'
    elif subpos == 'after':
      lextype = 'clause-final-subord-lex-item'

  lexicon.add(orthstr + ':= ' +lextype + ' &\
                        [ STEM < "' + orthstr + '" >,\
                     SYNSEM.LKEYS.KEYREL.PRED "' + pred + '"].\\')

def add_subord_lex_item(mylang, pos, subpos):
  """
  add the type definition for the lexical item to mylang
  """
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
			    ARG2 #h2 ]]].\\')
  mylang.add('subord-lex-item := scopal-mod-with-comp-lex &\
  [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ < >,\
			     SPR < >,\
			     COMPS < #comps > ]],\
    ARG-ST < #comps &\
    	     [ LOCAL.CAT [ MC - ]] > ].\\')

  if pos == 'before':
    if subpos == 'before':
      mylang.add('clause-init-prehead-subord-lex-item := subord-lex-item &\
      [ SYNSEM.LOCAL.CAT.HEAD.INIT +,\
      SYNSEM.LOCAL.CAT.POSTHEAD - ].\\')
    elif subpos == 'after':
      mylang.add('clause-final-prehead-subord-lex-item := subord-lex-item &\
            [ SYNSEM.LOCAL.CAT.HEAD.INIT - ],\
      SYNSEM.LOCAL.CAT.POSTHEAD - ].\\')
  elif pos == 'after':
    if subpos == 'before':
      mylang.add('clause-init-posthead-subord-lex-item := subord-lex-item &\
      [ SYNSEM.LOCAL.CAT.HEAD.INIT +,\
      SYNSEM.LOCAL.CAT.POSTHEAD + ].\\')
    elif subpos == 'after':
      mylang.add('clause-final-posthead-subord-lex-item := subord-lex-item &\
            [ SYNSEM.LOCAL.CAT.HEAD.INIT -,\
      SYNSEM.LOCAL.CAT.POSTHEAD + ].\\')
  elif pos == 'either':
    if subpos == 'before':
      mylang.add('clause-init-subord-lex-item := subord-lex-item &\
      [ SYNSEM.LOCAL.CAT.HEAD.INIT + ].\\')
    elif subpos == 'after':
      mylang.add('clause-final-subord-lex-item := subord-lex-item &\
            [ SYNSEM.LOCAL.CAT.HEAD.INIT - ].\\')

  mylang.set_section('addenda')
  mylang.add('head :+ [ INIT bool ].')

def add_subord_phrasal_types(mylang, rules, pos, subpos):
  """
  Add the phrase type definitions
  """
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
  if subpos == 'before':
    rules.add('head-comp := head-comp-phrase.')
  elif subpos == 'after':
    rules.add('comp-head := comp-head-phrase.')

# def add_subordinator_pair_to_lexicon(lexicon, matrixtype, subordtype, pair):
#   """
#   Adds each member of a subordinator pair to the lexicon
#   """
#   if matrixtype == 'adv':
#     lexicon.add(matrixorth + ':= subpair-adv-lex-item &\
#   [ STEM < "' + matrixorth '" >,\
#     SYNSEM.LKEYS.KEYREL.PRED "' + pred '" ].')
  #elif matrixtype == 'comp'

  #if subordtype == 'comp':
   # lexicon.add()

def customize_clausalmods(mylang, ch, lexicon, rules, irules):
  """
  The main clausal modifier customization routine
  """
  mylang.add('+nvcdmo :+ [ MOD < > ].')

  for cms in ch.get('cms'):
    cmsnum = str(cms.iter_num())

    pos = cms.get('position')
    subord = cms.get('subordinator')


    if subord == 'free':
      subpos = cms.get('subposition')
      for subord in cms.get('freemorph'):
        add_free_subordinator_to_lexicon(lexicon, subord, pos, subpos)
      add_subord_lex_item(mylang, pos, subpos)
      add_subord_phrasal_types(mylang, rules, pos, subpos)

    if subord == 'pair':
      matrixtype = cms.get('matrixtype')
      subordtype = cms.gete('subordtype')
      # for pair in cms.get('morphpair'):
      #   add subordinator_pair_to_lexicon(lexicon, matrixtype, subordtype, pair)
      # add_pair_lex_items()
      # add_pair_phrasal_types()