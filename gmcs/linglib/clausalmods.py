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
        if cms.get('subordinator') == 'pair':
          mylang.add('scopal-mod-with-nominalized-comp-no-rel-lex := [ SYNSEM [ SUBPAIR #subpair,\
                                      LOCAL.CAT.HEAD.MOD < [ SUBPAIR #subpair ] > ]].' )
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
        if cms.get('subordinator') == 'pair':
          mylang.add('scopal-mod-with-nominalized-comp-lex := [ SYNSEM [ SUBPAIR #subpair,\
                                      LOCAL.CAT.HEAD.MOD < [ SUBPAIR #subpair ] > ]].' )
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
      if cms.get('subordinator') == 'pair':
        mylang.add('scopal-mod-with-comp-lex := [ SYNSEM [ SUBPAIR #subpair,\
                                    LOCAL.CAT.HEAD.MOD < [ SUBPAIR #subpair ] > ]].')
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
  saved_constraints = constraints
  saved_lextype = lextype
  if cms.get('subordinator-type') == 'head':
    if cms.get('subordinator') == 'free':
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
      if cms.get('subordinator') == 'free':
        for freemorph in cms.get('freemorph'):
          orth = freemorph.get('orth')
          orthstr = orth_encode(orth)
          pred = freemorph.get('pred')
          lexicon.add(orthstr + ' := ' + type + ' &\
                                  [ STEM < "' + orthstr + '" >,\
                               SYNSEM.LKEYS.KEYREL.PRED "' + pred + '"].\\')
    elif cms.get('subordinator') == 'pair':
      for morphpair in cms.get('morphpair'):
        pred = morphpair.get('subordpred')
        value = pred.split('_')[0]
        lextype = [value] + lextype
        type = build_lex_type(lextype)
        constraints.append('SYNSEM.SUBPAIR ' + value)
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
        for morphpair in cms.get('morphpair'):
          orth = morphpair.get('subordorth')
          orthstr = orth_encode(orth)
          pred = morphpair.get('subordpred')
          lexicon.add(orthstr + ' := ' + type + ' &\
                                  [ STEM < "' + orthstr + '" >,\
                               SYNSEM.LKEYS.KEYREL.PRED "' + pred + '"].\\')

  elif cms.get('subordinator-type') == 'adverb':
    if cms.get('subordinator') == 'free':
      for adverb in cms.get('freemorph'):
        constraints = saved_constraints
        lextype = saved_lextype
        pred = adverb.get('pred')
        value = pred.split('_')[0]
        lextype = [ value ] + lextype
        constraints.append('SYNSEM.SUBORDINATED ' + value)
        if cms.get('subordinator') == 'pair':
          constraints.append('SYNSEM.SUBPAIR ' + value)
        if cms.get('adverb_attaches') == 's':
          constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
        elif cms.get('adverb_attaches') == 'vp':
          constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
        if cms.get('adverb_attaches') == 'both':
          constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
        type = build_lex_type(lextype)
    if cms.get('subordinator') == 'pair':
      for adverb in cms.get('morphpair'):
        constraints = saved_constraints
        lextype = saved_lextype
        pred = adverb.get('subordpred')
        value = pred.split('_')[0]
        lextype = [ value ] + lextype
        constraints.append('SYNSEM.SUBORDINATED ' + value)
        if cms.get('subordinator') == 'pair':
          constraints.append('SYNSEM.SUBPAIR ' + value)
        if cms.get('adverb_attaches') == 's':
          constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
        elif cms.get('adverb_attaches') == 'vp':
          constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
        if cms.get('adverb_attaches') == 'both':
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
        mylang.add(type + ' := intersective-mod-subord-lex-item & [ ' + constraints.pop() + ' ].')
        while constraints != []:
          mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
      orth = adverb.get('subordorth')
      orthstr = orth_encode(orth)
      lexicon.add(orthstr + ' := ' + type + ' & [ STEM < "' + orthstr + '" > ].\\')

def build_lex_type(lextype):
  type = ''
  type += lextype.pop()
  for s in lextype:
    type += '-' + s
  return type



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
  #do i even need the following?
  if ch.get('word-order') == 'free' or ch.get('word-order') == 'v2':
    mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial-head-nexus &\
          [HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT + ].')
    mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final-head-nexus&\
            [HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT - ].')
    rules.add('head-comp := head-comp-phrase.')
    rules.add('comp-head := comp-head-phrase.')
  else:
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
    if cms.get('subordinator') == 'pair':
      mylang.add(supertype + ' := [ SYNSEM [ SUBPAIR #subpair,\
                                  LOCAL.CAT.HEAD.MOD < [ SUBPAIR #subpair ] > ],\
                                  ARGS < [ SYNSEM.SUBPAIR #subpair ] > ].')
    if cms.get('subordinator') == 'free':
      for adverb in cms.get('freemorph'):
        pred = adverb.get('pred')
        value = pred.split('_')[0]
        lextype = value + '-modifying-clause-phrase'
        mylang.add(lextype + ' := ' + supertype + ' &\
  [ C-CONT.RELS <! [ PRED "' + pred + '" ] !>,\
    ARGS < [ SYNSEM.SUBORDINATED ' + value + ' ] > ].')
        rules.add(value + '-modifying-clause := ' + lextype + '.')
    elif cms.get('subordinator') == 'pair':
      for adverb in cms.get('morphpair'):
        pred = adverb.get('subordpred')
        value = pred.split('_')[0]
        lextype = value + '-modifying-clause-phrase'
        mylang.add(lextype + ' := ' + supertype + ' &\
  [ C-CONT.RELS <! [ PRED "' + pred + '" ] !>,\
    ARGS < [ SYNSEM.SUBORDINATED ' + value + ' ] > ].')
        rules.add(value + '-modifying-clause := ' + lextype + '.')

def add_subordinators_matrix_pair_to_lexicon(mylang, lexicon, cms, ch):
  """
  Adds the matrix member of a subordinator pair to the lexicon
  """
  mylang.add('intersective-mod-matrix-lex-item := no-rels-hcons-lex-item &\
    [ SYNSEM [ LOCAL [ CAT [ VAL [ COMPS < >,\
  				 SPR < >,\
  				 SUBJ < > ],\
  			   HEAD.MOD < [ LOCAL intersective-mod &\
                                                [ CAT [ HEAD verb ] ] ] > ] ] ]].')
  #if the subordinated feature is introduced by any of the strategies, we need to set it to none for this type
  for strategy in ch.get('cms'):
    if strategy.get('subordinator-type') == 'adverb':
      mylang.set_section('subordlex')
      mylang.add('intersective-mod-matrix-lex-item := [ SYNSEM.SUBORDINATED none ].')
  for adverb in cms.get('morphpair'):
    lextype = []
    constraints = []
    advpos = cms.get('matrix_subposition')
    if advpos == 'before':
      lextype.append('clause-init')
      constraints.append('SYNSEM.LOCAL.CAT.HEAD.INIT +')
    elif subpos == 'after':
      lextype.append('clause-final')
      constraints.append('SYNSEM.LOCAL.CAT.HEAD.INIT -')
    subordpred = adverb.get('subordpred')
    subpair = subordpred.split('_')[0]
    matrixpred = adverb.get('matrixpred')
    pred = matrixpred.split('_')[0]
    lextype = [ pred ] + lextype
    constraints.append('SYNSEM.SUBPAIR ' + subpair)
    if cms.get('matrix_adverb_attaches') == 's':
      constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
    elif cms.get('matrix_adverb_attaches') == 'vp':
      constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
    elif cms.get('matrix_adverb_attaches') == 'both':
      constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
    type = build_lex_type(lextype)
    type += '-pair-lex-item'
    #mylang.add(type + ' := intersective-mod-subord-lex-item & ' + const)
    mylang.add(type + ' := intersective-mod-matrix-lex-item & [ ' + constraints.pop() + ' ].')
    while constraints != []:
      mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
    orth = adverb.get('matrixorth')
    orthstr = orth_encode(orth)
    lexicon.add(orthstr + ' := ' + type + ' &\
                                          [ STEM < "' + orthstr + '" > ].\\')


def create_subordinated_feature(mylang, roots, cms):
  """
  adds the SUBORDINATED feature to SYNSEM, adds the addenda to make sure
  it is tracked through the grammar, and appropriately constrains verb-lex
  """
  mylang.set_section('addenda')
  mylang.add('canonical-synsem :+ [ SUBORDINATED xsubord ].')
  mylang.add('xsubord := *top*.')
  mylang.add('none := xsubord.')
  if cms.get('subordinator') == 'free':
    for adverb in cms.get('freemorph'):
      pred = adverb.get('pred')
      value = pred.split('_')[0]
      mylang.add(value + ' := xsubord.')
  elif cms.get('subordinator') == 'pair':
    for adverb in cms.get('morphpair'):
      pred = adverb.get('subordpred')
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
  mylang.add('basic-head-mod-phrase-simple :+\
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
  mylang.add('basic-head-subj-phrase :+\
    [ SYNSEM.SUBPAIR #subpair,\
      HEAD-DTR.SYNSEM.SUBPAIR #subpair ].')
  mylang.add('basic-head-comp-phrase :+\
    [ SYNSEM.SUBPAIR #subpair,\
      HEAD-DTR.SYNSEM.SUBPAIR #subpair ].')
  mylang.add('basic-head-mod-phrase-simple :+    [ SYNSEM [ SUBPAIR #subpair,\
                                                            LOCAL.CAT.MC #mc ],\
      NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.MOD < [ SUBPAIR #subpair ] >,\
				      MC #mc ] ].')

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

  mylang.set_section('verb-lex')
  mylang.add('verb-lex := [ SYNSEM.SUBPAIR nopair ].')

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

    if cms.get('subordinator-type') == 'adverb':
      create_subordinated_feature(mylang, roots, cms)

    if subord == 'free':
      add_subord_lex(mylang, lexicon, cms, ch)
      add_subord_phrasal_types(mylang, rules, cms, ch)

    if subord == 'pair':
      create_subpair_feature(mylang, cms.get('morphpair'))
      add_subord_lex(mylang, lexicon, cms, ch)
      add_subordinators_matrix_pair_to_lexicon(mylang, lexicon, cms,ch)
      add_subord_phrasal_types(mylang, rules, cms, ch)