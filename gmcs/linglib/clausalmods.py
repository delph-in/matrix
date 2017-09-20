from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy

######################################################################
# Clausal Modifiers
#   Create the type definitions associated with the user's choices
#   about clasual modification.

######################################################################

def add_free_subordinator_to_lexicon(lexicon, freemorph):
  """
  Add free subordinators to the lexicon
  """

  for word in orth.get('freemorph'):
    orth = word.get('orth')
    orthstr = orth_encode(orth)
    pred = word.get('pred')
    predstr = pred_encode(pred)
    lexicon.add(orthstr + ':= subord-lex &\
                          [ STEM < "' + orthstr + '" >,\
                        SYNSEM.LKEYS.KEYREL.PRED "' + predstr + '",\
                        CFORM "' + cmsnum + '" ].')

def add_subord_lex_item(mylang, subpos):
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
  if subpos == 'before':
    mylang.add('clause-init-subord-lex-item := subord-lex-item &\
  [ SYNSEM.LOCAL.CAT.HEAD.INIT + ].\\')
  elif subpos == 'after':
    mylang.add('clause-init-subord-lex-item := subord-lex-item &\
        [ SYNSEM.LOCAL.CAT.HEAD.INIT - ].\\')
  mylang.set_section('addenda')
  mylang.add('head :+ [ INIT bool ].')


def customize_clausalmods(mylang, ch, lexicon, rules, irules):
  """
  The main clausal modifier customization routine
  """
  mylang.set_section('clausalmods')

  for cms in ch.get('cms'):
    cmsnum = str(cms.iter_num())

    pos = cms.get('position')
    subord = cms.get('subordinator')
    subpos = cms.get('subposition')
    freemorph = cms.get('freemorph')

    if subord == 'free':
      #add_free_subordinator_to_lexicon(lexicon, freemorph)
      add_subord_lex_item(mylang, subpos)

