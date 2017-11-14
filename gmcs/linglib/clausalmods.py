from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy

######################################################################
# Clausal Modifiers
#   Create the type definitions associated with the user's choices
#   about clasual modification.

######################################################################


def add_subord_lex(mylang, lexicon, cms, ch):
  """
  add the type definition for the lexical item to mylang
  and the lexical entries to lexicon
  """
  mylang.set_section('addenda')
  mylang.add('head :+ [ INIT bool ].')

  if cms.get('subordinator-type') == 'head':
    if cms.get('nominalization') == 'on':
      mylang.set_section('noun-lex')
      mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
      mylang.set_section('subordlex')
      nom_strategy = cms.get('nominalization_strategy')
      for ns in ch.get('ns'):
        if ns.get('name') == nom_strategy:
          nmzRel = ns.get('nmzRel')
      if nmzRel == 'no':
        mylang.add('scopal-mod-with-nominalized-comp-no-rel-lex := single-rel-lex-item & norm-ltop-lex-item &\
        [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL scopal-mod &\
                 [CAT [HEAD verb,\
      VAL [SUBJ < >,\
          SPR < >,\
          COMPS < >]],\
      CONT.HOOK[LTOP  #mod,\
      INDEX  #index ]]] >,\
      VAL.COMPS < [LOCAL[CAT[HEAD noun,\
                   VAL[SUBJ < >,\
                       SPR < >,\
                       COMPS < >]],\
      CONT.HOOK.LTOP  #comp ]] > ],\
      CONT[HCONS <! qeq &\
                    [ HARG  #h1,\
                     LARG  #mod ],\
                    qeq &\
                    [ HARG  #h2,\
                     LARG  #comp ] !>,\
                     HOOK.INDEX  #index ]],\
                     LKEYS.KEYREL[ARG0 event,\
                     ARG1  #h1,\
                     ARG2  #h2 ]]].')

        mylang.add('nom-no-rel-subord-lex-item := scopal-mod-with-nominalized-comp-no-rel-lex &\
                [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >,\
                                   SPR < > ,\
        			   COMPS < [ LOCAL [ CAT [ HEAD noun &\
                                        			[ NMZ + ],\
                                   			MC - ] ] ] > ]].')
      else:
        mylang.add('scopal-mod-with-nominalized-comp-lex := single-rel-lex-item & norm-ltop-lex-item &\
      [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL scopal-mod &\
               [CAT [HEAD verb,\
    VAL [SUBJ < >,\
        SPR < >,\
        COMPS < >]],\
    CONT.HOOK[LTOP  #mod,\
    INDEX  #index ]]] >,\
    VAL.COMPS < [LOCAL[CAT[HEAD noun,\
                 VAL[SUBJ < >,\
                     SPR < >,\
                     COMPS < >]],\
    CONT.HOOK.INDEX  #comp ]] > ],\
    CONT[HCONS <! qeq &\
                  [HARG  #h1,\
                   LARG  #mod ] !>,\
                   HOOK.INDEX  #index ]],\
                   LKEYS.KEYREL[ARG0 event,\
                   ARG1  #h1,\
                   ARG2  #comp ]]].')

        mylang.add('nom-subord-lex-item := scopal-mod-with-nominalized-comp-lex &\
                [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >,\
                                   SPR < > ,\
        			   COMPS < [ LOCAL [ CAT [ HEAD noun &\
                                        			[ NMZ + ],\
                                   			MC - ] ] ] > ]].')
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
      ARG-ST <  #comps &\
                  [LOCAL.CAT[ HEAD verb,\
                              MC -]] > ].')

  elif cms.get('subordinator-type') == 'adverb':
    mylang.set_section('subordlex')
    if cms.get('nominalization') == 'on':
      mylang.add('intersective-mod-nominalized-subord-lex-item := no-rels-hcons-lex-item &\
        [ SYNSEM [ LOCAL [ CAT [ VAL [ COMPS < >,\
      				 SPR < >,\
      				 SUBJ < > ],\
      			   HEAD.MOD < [ LOCAL intersective-mod &\
                                                    [ CAT [ HEAD noun &\
                                        			[ NMZ + ] ] ] ] > ] ] ]].')
    else:
      mylang.add('intersective-mod-subord-lex-item := no-rels-hcons-lex-item &\
  [ SYNSEM [ LOCAL [ CAT [ VAL [ COMPS < >,\
				 SPR < >,\
				 SUBJ < > ],\
			   HEAD.MOD < [ LOCAL intersective-mod &\
                                              [ CAT [ HEAD verb ] ] ] > ] ] ]].')

  lextype = []
  constraints = []
  pos = cms.get('position')
  subpos = cms.get('subposition')
  if subpos == 'before':
    lextype.append('clause-init')
    constraints.append('SYNSEM.LOCAL.CAT.HEAD.INIT +')
  elif subpos == 'after':
    lextype.append('clause-final')
    constraints.append('SYNSEM.LOCAL.CAT.HEAD.INIT -')
  if pos == 'before':
    lextype.append('prehead')
    constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD -')
  elif pos == 'after':
    lextype.append('posthead')
    constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD +')
  if cms.get('specialmorph') == 'on':
    for feat in cms.get('feat'):
          lextype.append(feat.get('value'))
          constraints.append('SYNSEM.LOCAL.CAT.HEAD.' + feat.get('name') + '.' + feat.get('value'))
  if cms.get('subordinator-type') == 'head':
    type = build_lex_type(lextype)
    #const = build_constraints(constraints)
    if cms.get('nominalization') == 'on':
      if nmzRel == 'no':
        type += '-nom-no-rel-subord-lex-item'
        mylang.add(type + ' := nom-no-rel-subord-lex-item & [ ' + constraints.pop() + ' ].')
        while constraints != []:
          mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
      else:
        type += '-nom-subord-lex-item'
        #mylang.add(type + ' := nom-subord-lex-item &' + const)
        mylang.add(type + ' := nom-subord-lex-item & [ ' + constraints.pop() + ' ].')
        while constraints != []:
          mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
    else:
      type += '-subord-lex-item'
      #mylang.add(type + ' := subord-lex-item & ' + const)
      mylang.add(type + ' := subord-lex-item & [ ' + constraints.pop() + ' ].')
      while constraints != []:
        mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
    for freemorph in cms.get('freemorph'):
      orth = freemorph.get('orth')
      orthstr = orth_encode(orth)
      pred = freemorph.get('pred')
      lexicon.add(orthstr + ':= ' + type + ' &\
                                  [ STEM < "' + orthstr + '" >,\
                               SYNSEM.LKEYS.KEYREL.PRED "' + pred + '"].\\')

  elif cms.get('subordinator-type') == 'adverb':
    for adverb in cms.get('freemorph'):
      pred = adverb.get('pred')
      value = pred.split('_')[0]
      lextype = [ value ] + lextype
      constraints.append('SYNSEM.SUBORDINATED ' + value)
      if cms.get('adverb-attaches') == 's':
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
      elif cms.get('adverb-attaches') == 'vp':
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
      if cms.get('adverb-attaches') == 'both':
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
      type = build_lex_type(lextype)
      #const = build_constraints(constraints)
      # if cms.get('nominalization') == 'on':
      #   nom_strategy = cms.get('nominalization_strategy')
      #   for ns in ch.get('ns'):
      #     if ns.get('name') == nom_strategy:
      #       nmzRel = ns.get('nmzRel')
      #   if nmzRel == 'no':
      #     type += '-nom-no-rel-adv-subord-lex-item'
      #     #mylang.add(type + ' := nom-no-rel-subord-lex-item &' + const) #todo this won't be the right type
      #     mylang.add(type + ' := nom-no-rel-adv-subord-lex-item & [ ' + constraints.pop() + ' ].')#todo this won't be the right type
      #     while constraints != []:
      #       mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
      #   else:
      #     type += '-nom-adv-subord-lex-item'
          #mylang.add(type + ' := nom-subord-lex-item &' + const) #todo this won't be the right type
      if cms.get('nominalization') == 'on':
        type += '-nom-adv-subord-lex-item'
        mylang.add(type + ' := intersective-mod-nominalized-subord-lex-item & [ ' + constraints.pop() + ' ].')
        while constraints != []:
          mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
      else:
        type += '-adv-subord-lex-item'
        #mylang.add(type + ' := intersective-mod-subord-lex-item & ' + const)
        if cms.get('nominalization') ==  'on':
          mylang.add(type + ' := intersective-mod-subord-lex-item & [ ' + constraints.pop() + ' ].')
        else:
          mylang.add(type + ' := intersective-mod-nominalized-subord-lex-item & [ ' + constraints.pop() + ' ].')
        while constraints != []:
          mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
      orth = adverb.get('orth')
      orthstr = orth_encode(orth)
      lexicon.add(orthstr + ':= ' + type + ' &\
                                          [ STEM < "' + orthstr + '" > ].\\')

def build_lex_type(lextype):
  type = ''
  type += lextype.pop()
  for s in lextype:
    type += '-' + s
  return type

# def build_constraints(constraints):
#   const = '[ '
#   if constraints != []:
#     const = const + constraints.pop()
#   for each in constraints:
#     const = const + ', ' + each
#   const = const + ' ].'
#   print(const)
#   return const


def add_subord_phrasal_types(mylang, rules, cms, ch):
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

  if cms.get('subordinator-type') == 'adverb':
    if cms.get('subposition') == 'before':
      rules.add('adj-head-int := adj-head-int-phrase.')
    elif cms.get('subposition') == 'after':
      rules.add('head-adj-int := head-adj-int-phrase.')
    else:
      rules.add('adj-head-int := adj-head-int-phrase.')
      rules.add('head-adj-int := head-adj-int-phrase.')

    if cms.get('nominalization') == 'on':
      nom_strategy = cms.get('nominalization_strategy')
      for ns in ch.get('ns'):
        if ns.get('name') == nom_strategy:
          nmzRel = ns.get('nmzRel')
      if nmzRel == 'no':
        supertype = 'adv-marked-nominalized-no-rel-subord-clause-phrase'
        mylang.add(supertype + ' := basic-unary-phrase &\
                  [ SYNSEM [ LOCAL [ CAT [ HEAD [ MOD < [ LOCAL scopal-mod &\
                						[ CAT [ HEAD verb,\
                							VAL [ SUBJ < >,\
                							      SPR < >,\
                							      COMPS < > ]],\
                						  CONT.HOOK [ LTOP #mcl,\
                								INDEX #index ]]] > ]]]],\
                    C-CONT [ RELS <! arg12-ev-relation &\
                		   [ ARG1 #mch,\
                		     ARG2 #sch ] !>,\
                	     HCONS <! qeq &\
                		    [ HARG #mch,\
                		      LARG #mcl ], \
                                qeq &\
                		    [ HARG #sch,\
                		      LARG #scl ] !>,\
                    		HOOK.INDEX #index ],\
                    ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD noun & \
                                                        [ NMZ + ],\
                                                  MC -,\
                				    VAL [ SUBJ < >,\
                					  SPR < >,\
                					  COMPS < > ]],\
                			    CONT.HOOK.LTOP #scl ],\
                                      NON-LOCAL [ REL 0-dlist ] ] ] > ].')
      else:
        supertype = 'adv-marked-nominalized-subord-clause-phrase'
        mylang.add(supertype + ' := basic-unary-phrase &\
          [ SYNSEM [ LOCAL [ CAT [ HEAD [ MOD < [ LOCAL scopal-mod &\
        						[ CAT [ HEAD verb,\
        							VAL [ SUBJ < >,\
        							      SPR < >,\
        							      COMPS < > ]],\
        						  CONT.HOOK [ LTOP #mcl,\
        								INDEX #index ]]] > ]]]],\
            C-CONT [ RELS <! arg12-ev-relation &\
        		   [ ARG1 #mch,\
        		     ARG2 #sch ] !>,\
        	     HCONS <! qeq &\
        		    [ HARG #mch,\
        		      LARG #mcl ],\
                          qeq &\
        		    [ HARG #sch,\
        		      LARG #scl ] !>,\
            		HOOK.INDEX #index ],\
            ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD noun & \
                                                [ NMZ + ],\
                                          MC -,\
        				    VAL [ SUBJ < >,\
        					  SPR < >,\
        					  COMPS < > ]],\
        			    CONT.HOOK.INDEX #scl ],\
                              NON-LOCAL [ REL 0-dlist ] ] ] > ].')
    else:
      supertype = 'adv-marked-subord-clause-phrase'
      mylang.add(supertype + ' := basic-unary-phrase &\
  [ SYNSEM [ LOCAL [ CAT [ HEAD [ MOD < [ LOCAL scopal-mod &\
						[ CAT [ HEAD verb,\
							VAL [ SUBJ < >,\
							      SPR < >,\
							      COMPS < > ]],\
						  CONT.HOOK [ LTOP #mcl,\
								INDEX #index ]]] > ]]]],\
    C-CONT [ RELS <! arg12-ev-relation &\
		   [ ARG1 #mch,\
		     ARG2 #sch ] !>,\
	     HCONS <! qeq &\
		    [ HARG #mch,\
		      LARG #mcl ],\
              qeq &\
		    [ HARG #sch,\
		      LARG #scl ] !>,\
    		HOOK.INDEX #index ],\
    ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD verb,\
				    VAL [ SUBJ < >,\
					  SPR < >,\
					  COMPS < > ]],\
			    CONT.HOOK.LTOP #scl ],\
                      NON-LOCAL [ REL 0-dlist ] ] ] > ].')
    for adverb in cms.get('freemorph'):
      pred = adverb.get('pred')
      value = pred.split('_')[0]
      lextype = value + '-modifying-clause-phrase'
      mylang.add(lextype + ' := ' + supertype + ' &\
  [ C-CONT.RELS <! [ PRED "' + pred + '" ] !>,\
    ARGS < [ SYNSEM.SUBORDINATED ' + value + ' ] > ].')
      rules.add(value + '-modifying-clause := ' + lextype + '.')

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

def create_subordinated_feature(mylang, roots, cms):
  """
  adds the SUBORDINATED feature to SYNSEM, adds the addenda to make sure
  it is tracked through the grammar, and appropriately constrains verb-lex
  """
  mylang.set_section('addenda')
  mylang.add('canonical-synsem :+ [ SUBORDINATED xsubord ].')
  mylang.add('xsubord := *top*.')
  mylang.add('none := xsubord.')
  for adverb in cms.get('freemorph'):
    pred = adverb.get('pred')
    value = pred.split('_')[0]
    mylang.add(value + ' := xsubord.')
  mylang.add('basic-head-subj-phrase :+\
  [ SYNSEM.SUBORDINATED #subord,\
    HEAD-DTR.SYNSEM.SUBORDINATED #subord ].')
  mylang.add('basic-head-comp-phrase :+\
  [ SYNSEM.SUBORDINATED #subord,\
    HEAD-DTR.SYNSEM.SUBORDINATED #subord ].')
  mylang.add('basic-head-opt-comp-phrase :+\
  [ SYNSEM.SUBORDINATED #subord,\
    HEAD-DTR.SYNSEM.SUBORDINATED #subord ].')
  mylang.add('basic-head-opt-subj-phrase :+\
  [ SYNSEM.SUBORDINATED #subord,\
    HEAD-DTR.SYNSEM.SUBORDINATED #subord ].')
  mylang.add('adj-head-phrase :+\
  [ SYNSEM.SUBORDINATED #subord,\
    NON-HEAD-DTR.SYNSEM.SUBORDINATED #subord ].')
  mylang.set_section('verb-lex')
  mylang.add('verb-lex := [ SYNSEM.SUBORDINATED none ].')
  mylang.add('same-subordinated-lex-rule := lex-rule &\
  [ SYNSEM.SUBORDIANTED #subord,\
    DTR.SYNSEM.SUBORDIANTED #subord ].')
  mylang.add('non-local-change-only-lex-rule := same-subordinated-lex-rule.')
  mylang.add('local-change-only-lex-rule := same-subordinated-lex-rule.')
  mylang.add('cont-change-only-lex-rule := same-subordinated-lex-rule.')
  mylang.add('cat-change-with-ccont-lex-rule := same-subordinated-lex-rule.')
  mylang.add('add-only-rule := same-subordinated-lex-rule.')
  roots.add('root := [ SYNSEM.SUBORDINATED none ].')

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

def customize_clausalmods(mylang, ch, lexicon, rules, roots):
  """
  The main clausal modifier customization routine
  """
  mylang.set_section ('addenda')
  mylang.add('+nvcdmo :+ [ MOD < > ].')

  for cms in ch.get('cms'):
    cmsnum = str(cms.iter_num())

    subord = cms.get('subordinator')


    if subord == 'free':
      if cms.get('subordinator-type') == 'adverb':
        create_subordinated_feature(mylang, roots, cms)
      add_subord_lex(mylang, lexicon, cms, ch)
      add_subord_phrasal_types(mylang, rules, cms, ch)

    if subord == 'pair':
      matrixtype = cms.get('matrixtype')
      subordtype = cms.get('subordtype')
      create_subpair_feature(mylang, cms.get('morphpair'))
      for pair in cms.get('morphpair'):
        add_subordinator_pair_to_lexicon(lexicon, matrixtype, subordtype, pair)
      #add_pair_lex_items()
      #add_pair_phrasal_types()