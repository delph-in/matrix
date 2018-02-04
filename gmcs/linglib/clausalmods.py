from gmcs.utils import orth_encode,TDLencode

######################################################################
# Clausal Modifiers
#   Create the type definitions associated with the user's choices
#   about clasual modification.

######################################################################

def customize_clausalmods(mylang, ch, lexicon, rules, roots):
    """
    The main clausal modifier customization routine
    """
    if not 'cms' in ch:
        return None
    add_head_compement_rules(mylang, rules, ch)

    for cms in ch.get('cms'):
        subord = cms.get('subordinator')
        subtype = cms.get('subordinator-type')

        if subord == 'free' or subord == 'pair':
            add_head_modifier_phrases(mylang, rules, cms)
            if subtype == 'head':
                create_head_subordinator_basic_lex_type(mylang, ch, cms)
                create_head_subordinator_lexical_subtypes(mylang, lexicon, ch, cms)
            if subtype == 'adverb':
                create_subordinated_feature(mylang, roots, cms, ch)
                create_adverb_subordinator_basic_lex_type(mylang)
                create_adverb_subordinator_lexical_subtypes(mylang, lexicon, cms)
                add_non_branching_rules(mylang, rules, cms, ch)

        if subord == 'pair':
            create_subpair_feature(mylang, roots, cms.get('morphpair'), ch)
            add_subordinators_matrix_pair_to_lexicon(mylang, lexicon, cms,ch)

        if subord == 'none':
            add_morphological_subord_rel(mylang, cms, ch, rules)

def create_head_subordinator_basic_lex_type(mylang, ch, cms):
    """
    Create the basic lexical type if if the the subordinator is a head (or adposition)
    A different type is added based on whether the subordinate clause is verbal or nominalized
    and if the nominalized clause has a semantic nominalized (nmz) relation
    """
    nominalized, nmzRel, nom_strategy = is_nominalized(cms, ch)
    mylang.set_section('subordlex')
    mylang.add('adposition-subord-lex-item := single-rel-lex-item & norm-ltop-lex-item &\
            [ SYNSEM.LOCAL.CAT [ MC -,\
                                HEAD adp & [ MOD < [ LOCAL scopal-mod &\
                                                        [ CAT [ HEAD verb,\
                                                                VAL [ COMPS < > ]]]]>],\
                                VAL [ SUBJ < >,\
                                        SPR < >,\
                                        COMPS < [LOCAL.CAT [ MC -,\
                                                            VAL.COMPS < >]] > ]]].')

    if nominalized == 'yes':
        if nmzRel == 'no':
            mylang.add('subord-with-nominalized-comp-no-rel-lex := adposition-subord-lex-item &\
            [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL.CONT.HOOK [ LTOP  #mod,\
                                                INDEX  #index ]] >,\
                                    VAL.COMPS < [LOCAL[CAT[HEAD noun &\
                                            			[ NMZ + ],\
                                            			VAL.SPR < > ],\
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
        else:
            mylang.add('subord-with-nominalized-comp-lex := adposition-subord-lex-item &\
            [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL.CONT.HOOK[LTOP  #mod,\
                                                                    INDEX  #index ]] >,\
                                   VAL.COMPS < [LOCAL[CAT[HEAD noun &\
                                                                [ NMZ + ],\
                                                            VAL.SPR < > ],\
                                                    CONT.HOOK.INDEX  #comp ]] > ],\
                            CONT[HCONS <! qeq &\
                                  [HARG  #h1,\
                                   LARG  #mod ] !>,\
                               HOOK.INDEX  #index ]],\
                       LKEYS.KEYREL[ARG0 event,\
                                   ARG1  #h1,\
                                    ARG2  #comp ]]].')

    else:
        mylang.add('subord-with-verbal-comp-lex := adposition-subord-lex-item &\
                [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL [ CAT [ HEAD verb ],\
                                    						   CONT.HOOK [ LTOP #mod,\
    	                                    						    INDEX #index ]]] >,\
                        			   VAL.COMPS < [ LOCAL [ CAT [ HEAD verb],\
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

def create_adverb_subordinator_basic_lex_type(mylang):
    """
    Create the basic subordinator lexical type if the subordinator is an adverb
    """
    mylang.set_section('subordlex')
    mylang.add('adverb-subord-lex-item := no-rels-hcons-lex-item &\
      [ SYNSEM [ LOCAL [ CAT [ VAL [ SUBJ < >,\
                                    SPR < >,\
                                    COMPS < > ],\
                                HEAD adv & [ MOD < [ SUBORDINATED none,\
						LOCAL intersective-mod &\
                                                  [ CAT [ MC -,\
                                                          HEAD verb ] ] ] > ] ] ]]].')

def create_head_subordinator_lexical_subtypes(mylang, lexicon, ch, cms):
    """
    Create the lexical subtype for the adposition/head subordinator with constraints for
    the clausal mod's position (before/after a vp/s), subordinator's position (clause initial
    or final), morphological constraints, subject sharing. Then add each subordinator to the
    lexicon.
    """
    lextype = []
    constraints = []
    pos = cms.get('position')
    subpos = cms.get('subposition')
    attach = cms.get('modifier-attach')
    nominalized, nmzRel, nom_strategy = is_nominalized(cms, ch)
    #constraints for the subordinator's position in the clause
    if subpos == 'before':
        lextype.append('clause-init')
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.INIT +')
    elif subpos == 'after':
        lextype.append('clause-final')
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.INIT -')
    #constraints for the clausal modifier with respect to the matrix clause
    if pos == 'before':
        lextype.append('prehead')
        constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD -')
    elif pos == 'after':
        lextype.append('posthead')
        constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD +')
    if attach == 's':
        lextype.append('s-attach')
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL.SUBJ < > ] >')
    elif attach == 'vp':
        lextype.append('vp-attach')
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL.SUBJ < [ ] > ] >')
    #add special morphology constraints
    lextype, constraints = add_morphological_constraints(lextype, constraints, cms, 'head')
    #add constraints if subject is shared between clauses
    if cms.get('shared-subj') == 'on':
        lextype.append('shared-subject')
        constraints.append('SYNSEM.LOCAL.CAT [ HEAD.MOD < [ LOCAL [ CONT.HOOK.XARG #xarg ]] >,\
                    VAL.COMPS < [ LOCAL [ CAT [ VAL [ SUBJ < unexpressed > ]],\
    	  		    CONT.HOOK.XARG #xarg ]]  > ]')
    else:
        constraints.append('SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.VAL.SUBJ < > ] >')
    # for free subordinators, add each of the constraints enumerated above to the lexical type
    # (with the appropriate subertype based on whether the clausal mod is nominalized)
    if cms.get('subordinator') == 'free':
        if has_subpairs(ch) == True:
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR nopair ] >')
        type = build_type_name(lextype)
        if nominalized == 'yes':
            if nmzRel == 'no':
                type += '-nom-no-rel-subord-lex-item'
                mylang.add(type + ' := subord-with-nominalized-comp-no-rel-lex & [ ' + constraints.pop() + ' ].')
                while constraints != []:
                    mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
            else:
                type += '-nom-subord-lex-item'
                mylang.add(type + ' := subord-with-nominalized-comp-lex & [ ' + constraints.pop() + ' ].')
                while constraints != []:
                    mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
        else:
            type += '-subord-lex-item'
            mylang.add(type + ' := subord-with-verbal-comp-lex & [ ' + constraints.pop() + ' ].')
            while constraints != []:
                mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
        #add each subordinator to the lexicon
        for freemorph in cms.get('freemorph'):
            add_to_lexicon(freemorph, type, '', lexicon)

     # for pair subordinators, add each of the constraints enumerated above to the lexical type
    # (with the appropriate subertype based on whether the clausal mod is nominalized). A separate
    # lexical type is required for each pair, so that the SUBPAIR feature can be constrained
    elif cms.get('subordinator') == 'pair':
        for morphpair in cms.get('morphpair'):
            pred = morphpair.get('subordpred')
            value = shortform_pred(pred)
            lextype = [value] + lextype
            type = build_type_name(lextype)
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR ' + value + ' ] >')
            if nominalized == 'yes':
                if nmzRel == 'no':
                    type += '-nom-no-rel-subord-lex-item'
                    mylang.add(type + ' := subord-with-nominalized-comp-no-rel-lex & [ ' + constraints.pop() + ' ].')
                    while constraints != []:
                        mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
                else:
                    type += '-nom-subord-lex-item'
                    mylang.add(type + ' := subord-with-nominalized-comp-lex & [ ' + constraints.pop() + ' ].')
                    while constraints != []:
                        mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
            else:
                type += '-subord-lex-item'
                mylang.add(type + ' := subord-with-verbal-comp-lex & [ ' + constraints.pop() + ' ].')
                while constraints != []:
                    mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
            #add each subordinator to lexicon
            for morphpair in cms.get('morphpair'):
                add_to_lexicon(morphpair, type, 'subord', lexicon)

def create_adverb_subordinator_lexical_subtypes(mylang, lexicon, cms):
    """
    Create the lexical subtype for the adverb subordinator with constraints for
    subordinator's position (before/after a vp/s), morphological constraints,
    and the SUBORDINATED feature. Then add each to lexicon.
    """
    lextype = []
    constraints = []
    pos = cms.get('position')
    subpos = cms.get('subposition')
    #add constraints for subordinator position
    if subpos == 'before':
        lextype.append('clause-init')
        constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD -')
    elif subpos == 'after':
        lextype.append('clause-final')
        constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD +')
    if pos == 'before':
        lextype.append('prehead')
    elif pos == 'after':
        lextype.append('posthead')
    #add morphological constraints
    lextype, constraints = add_morphological_constraints(lextype, constraints, cms, 'adverb')
    saved_constraints = constraints
    saved_lextype = lextype
    # each free adverb subordinator gets it's own lexical type with a special SUBORDINATED value so that the non-branching
    #rule can select it
    if cms.get('subordinator') == 'free':
        for adverb in cms.get('freemorph'):
            constraints = saved_constraints
            lextype = saved_lextype
            pred = adverb.get('pred')
            value = shortform_pred(pred)
            lextype = [value] + lextype
            constraints.append('SYNSEM.SUBORDINATED ' + value)
            #add constriants for the adverb's attaching to vp or s
            if cms.get('adverb-attach') == 's':
                if cms.get('shared-subj') == 'on':
                    constraints.append('SYNSEM.LOCAL [ CONT.HOOK.XARG #xarg,\
                                                       CAT.HEAD.MOD < [ LOCAL [ CONT.HOOK.XARG #xarg,\
                                                                              CAT.VAL [ SUBJ < unexpressed >,\
                                                                                        COMPS < > ]]] > ]')
                else:
                    constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
            elif cms.get('adverb-attach') == 'vp':
                constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
            if cms.get('adverb-attach') == 'both':
                constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
            type = build_type_name(lextype)
            type += '-adv-subord-lex-item'
            mylang.add(type + ' := adverb-subord-lex-item & [ ' + constraints.pop() + ' ].')
            while constraints != []:
                mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
            orth = adverb.get('orth')
            orthstr = orth_encode(orth)
            name = TDLencode(adverb.get('name'))
            lexicon.add(name + ' := ' + type + ' & [ STEM < "' + orthstr + '" > ].')
    # each adverb gets it's own lexical type with a special SUBORDINATED value so that the non-branching
    # rule can select it and so that the SUBPAIR feature can be added
    elif cms.get('subordinator') == 'pair':
        for adverb in cms.get('morphpair'):
            constraints = saved_constraints
            lextype = saved_lextype
            pred = adverb.get('subordpred')
            value = shortform_pred(pred)
            lextype = [value] + lextype
            constraints.append('SYNSEM.SUBORDINATED ' + value)
            constraints.append('SYNSEM.LOCAL.CAT.SUBPAIR ' + value)
            # add constriants for the adverb's attaching to vp or s
            if cms.get('adverb-attach') == 's':
                if cms.get('shared-subj') == 'on':
                    constraints.append('SYNSEM.LOCAL [ CONT.HOOK.XARG #xarg,\
                                                       CAT.HEAD.MOD < [ LOCAL [ CONT.HOOK.XARG #xarg,\
                                                                              CAT.VAL [ SUBJ < unexpressed >,\
                                                                                        COMPS < > ]]] > ]')
                else:
                    constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
            elif cms.get('adverb-attach') == 'vp':
                constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
            if cms.get('adverb-attach') == 'both':
                constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
            type = build_type_name(lextype)
            type += '-adv-subord-lex-item'
            mylang.add(type + ' := adverb-subord-lex-item & [ ' + constraints.pop() + ' ].')
            while constraints != []:
                mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
            orth = adverb.get('subordorth')
            orthstr = orth_encode(orth)
            name = TDLencode(adverb.get('subordname'))
            lexicon.add(name + ' := ' + type + ' & [ STEM < "' + orthstr + '" > ].')


def add_head_modifier_phrases(mylang, rules, cms):
    """
    Add the appropriate head-modifier rules for the the clausal modifier's attachment to the
    matrix clause and for adverb subordinators attachment in the subord clause
    """
    mylang.set_section('addenda')
    mylang.add('basic-head-mod-phrase-simple :+ [ SYNSEM.LOCAL.CAT.MC #mc,\
      HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    # head mod rules for clausal mod attachment
    pos = cms.get('position')
    if pos == 'before':
        rules.add('adj-head-scop := adj-head-scop-phrase.')
    elif pos == 'after':
        rules.add('head-adj-scop := head-adj-scop-phrase.')
    elif pos == 'either':
        rules.add('adj-head-scop := adj-head-scop-phrase.')
        rules.add('head-adj-scop := head-adj-scop-phrase.')

    # head mod rules for adverb attachment
    if cms.get('subordinator-type') == 'adverb':
        if cms.get('subposition') == 'before':
            rules.add('adj-head-int := adj-head-int-phrase.')
        elif cms.get('subposition') == 'after':
            rules.add('head-adj-int := head-adj-int-phrase.')
        else:
            rules.add('adj-head-int := adj-head-int-phrase.')
            rules.add('head-adj-int := head-adj-int-phrase.')

def add_non_branching_rules(mylang, rules, cms, ch):
    """
    Create the non-branching rules for adverb subordinators. Add constraints for
    the clausal mod's attachment to the matrix clause (before/after a vp/s) and subject
    sharing. Each subordinator needs it's own rule subtype to add the subordinator
    """
    # First create the supertype
    mylang.add('adv-marked-subord-clause-phrase := unary-phrase &\
                [ SYNSEM [ LOCAL [ CAT [ MC -,\
                              VAL [ SPR < >,\
                              COMPS < >,\
				SPEC < >,\
                                    SUBJ #subj ],\
                              HEAD adp & [ MOD < [ LOCAL scopal-mod &\
    						[ CAT [ HEAD verb,\
    							VAL [ SPR < >,\
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
                ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD verb &\
                                              [ MOD < > ],\
    				    VAL [ SUBJ #subj,\
    				        SPR < >,\
    					  COMPS < >,\
					SPEC < > ]],\
    			    CONT.HOOK.LTOP #scl ] ] ] > ].')
    if cms.get('subordinator') == 'pair':
        mylang.add('adv-marked-subord-clause-phrase := [ SYNSEM.LOCAL.CAT.SUBPAIR nopair,\
                                        SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR #subpair ] >,\
                                      ARGS < [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair ] > ].')
    pos = cms.get('position')
    if cms.get('subordinator') == 'free':
        morpheme = 'freemorph'
        predication = 'pred'
    if cms.get('subordinator') == 'pair':
        morpheme = 'morphpair'
        predication = 'subordpred'
    # then add a rule for each subordinator
    for adverb in cms.get(morpheme):
        pred = adverb.get(predication)
        value = shortform_pred(pred)
        type = value + '-modifying-clause-phrase'
        mylang.add(type + ' := adv-marked-subord-clause-phrase &\
                    [ C-CONT.RELS <! [ PRED "' + pred + '" ] !>,\
                        ARGS < [ SYNSEM.SUBORDINATED ' + value + ' ] > ].')
        if pos == 'before':
            mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
        elif pos == 'after':
            mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
        if cms.get('shared-subj') == 'on':
            mylang.add(type + ' := [ SYNSEM.LOCAL.CAT [ HEAD.MOD < [ LOCAL [ CONT.HOOK.XARG #xarg ]] > ],\
                                     ARGS < [ SYNSEM.LOCAL [ CONT.HOOK.XARG #xarg,\
                                                  CAT [ VAL [ SUBJ < unexpressed > ]]]] >].')
        else:
            mylang.add(type + ' := [ ARGS < [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ < > ]]]] >].')

        if cms.get('modifier-attach') == 's':
            mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [LOCAL.CAT.VAL[SUBJ < >]] > ].')
        elif cms.get('modifier-attach') == 'vp':
            mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [LOCAL.CAT.VAL[SUBJ < [ ] >]] > ].')
        if cms.get('subordinator') == 'pair':
            mylang.add(type + ' := [ ARGS < [ SYNSEM.SUBORDINATED ' + value + ' ] > ].')
        if cms.get('subordinator') != 'pair' and has_subpairs(ch) == True:
            mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR nopair ] > ].')
        rules.add(value + '-modifying-clause := ' + type + '.')


def add_subordinators_matrix_pair_to_lexicon(mylang, lexicon, cms, ch):
    """
    Adds the matrix adverb of a subordinator pair to the lexicon, including constriants for it's
    attachment (before/after a vp/s). Add each to lexicon.
    """
    mylang.set_section('subordlex')
    mylang.add('subord-pair-matrix-lex-item := basic-adverb-lex &\
    [ SYNSEM [ LOCAL [ CAT [ VAL [ SUBJ < >,\
                              SPR < >,\
                              COMPS < > ],\
                        HEAD.MOD < [ LOCAL scopal-mod & [ CAT [ MC +,\
                                                HEAD verb ],\
                                            CONT.HOOK.LTOP #mod ]] > ],\
                      CONT.HCONS <! qeq &\
                                    [ HARG #h1,\
                                      LARG #mod ] !> ],\
                LKEYS.KEYREL.ARG1 #h1 ]].')
    #if the subordinated feature is introduced by any of the strategies, we need to set it to none for this type
    for strategy in ch.get('cms'):
        if strategy.get('subordinator-type') == 'adverb':
            mylang.set_section('subordlex')
            mylang.add('subord-pair-matrix-lex-item := [ SYNSEM.SUBORDINATED none ].')
    for adverb in cms.get('morphpair'):
        lextype = []
        constraints = []
        advpos = cms.get('matrix-subposition')
        if advpos == 'before':
            lextype.append('clause-init')
            constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD -')
        elif advpos == 'after':
            lextype.append('clause-final')
            constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD +')
        subordpred = adverb.get('subordpred')
        if subordpred.split('_')[0] == '':
            subpair = subordpred.split('_')[1]
        else:
            subpair = subordpred.split('_')[0]
        matrixpred = adverb.get('matrixpred')
        pred = shortform_pred(matrixpred)
        lextype = [ pred ] + lextype
        constraints.append('SYNSEM.LOCAL.CAT.SUBPAIR ' + subpair)
        if cms.get('matrix-adverb-attach') == 's':
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
        elif cms.get('matrix-adverb-attach') == 'vp':
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
        elif cms.get('matrix-adverb-attach') == 'both':
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
        type = build_type_name(lextype)
        type += '-pair-lex-item'
        mylang.add(type + ' := subord-pair-matrix-lex-item & [ ' + constraints.pop() + ' ].')
        while constraints != []:
            mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
        add_to_lexicon(adverb, type, 'matrix', lexicon)

def add_morphological_subord_rel(mylang, cms, ch, rules):
    """
    Create a non-branching rule that puts in a subord_rel when no free subordinator is present.
    Add constraints for clausal mod attachment (before/after a vp/s) special morphology and
    subject sharing.
    """
    nominalized, nmzRel, nom_strategy = is_nominalized(cms, ch)
    constraints = []
    lextype = []
    pos = cms.get('position')
    if pos == 'before':
        lextype.append('prehead')
        constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD -')
    elif pos == 'after':
        lextype.append('posthead')
        constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD +')
    lextype, constraints = add_morphological_constraints(lextype, constraints, cms, 'phrase')
    if has_subpairs(ch) == True:
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR nopair ] >')
    mylang.set_section('phrases')
    if cms.get('shared-subj') == 'on':
        lextype.append('shared-subject')
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL [ CONT.HOOK.XARG #xarg ]] >,\
    ARGS < [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ < unexpressed > ]],\
	  		    CONT.HOOK.XARG #xarg ]]  >')
    else:
        constraints.append('ARGS < [ SYNSEM.LOCAL.CAT.VAL.SUBJ < > ] >')
    # The supertype depends on if the clause in nominalized, and if it's nominalized, if it has
    # a semantic nominalized predication
    if nominalized == 'yes':
        mylang.set_section('phrases')
        for ns in ch.get('ns'):
            if ns.get('name') == nom_strategy:
                nmzRel = ns.get('nmzRel')
        if nmzRel == 'no':
            lextype.append('nmz-no-rel')
            supertype = 'no-subordinator-nominalized-norel-subord-clause-phrase'
            mylang.add(supertype + ' := unary-phrase &\
          [ SYNSEM [ LOCAL [ CAT [ MC -,\
                                  VAL [ SUBJ #subj,\
                                        SPR < >,\
                                        COMPS < > ],\
                                  HEAD adp & [ MOD < [ LOCAL scopal-mod &\
        						[ CAT [ HEAD verb,\
        							VAL [ SUBJ < >,\
        							      COMPS < > ]],\
        						  CONT.HOOK [ LTOP #mcl,\
        								INDEX #index ]]] > ]]]],\
            C-CONT [ RELS <! [ ARG1 #mch,\
        		     ARG2 #sch ] !>,\
        	     HCONS <! qeq &\
        		    [ HARG #mch,\
        		      LARG #mcl ],\
                      qeq &\
        		    [ HARG #sch,\
        		      LARG #scl ] !>,\
            		HOOK.INDEX #index ],\
            ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD noun &\
                                                    [ NMZ + ],\
        				    MC na-or-+,\
        				    VAL [ SUBJ #subj,\
        				        SPR < >,\
        					  COMPS < > ]],\
        			    CONT.HOOK.LTOP #scl ] ] ] > ].')
        else:
            lextype.append('nmz')
            supertype = 'no-subordinator-nominalized-subord-clause-phrase'
            mylang.add(supertype + ' := unary-phrase &\
            [ SYNSEM [ LOCAL [ CAT [ MC -,\
                                    VAL [ SUBJ #subj,\
                                          SPR < >,\
                                          COMPS < > ],\
                                    HEAD adp & [ MOD < [ LOCAL scopal-mod &\
          						[ CAT [ HEAD verb,\
          							VAL [ SUBJ < >,\
          							      COMPS < > ]],\
          						  CONT.HOOK [ LTOP #mcl,\
          								INDEX #index ]]] > ]]]],\
              C-CONT [ RELS <! [ ARG1 #mch,\
          		     ARG2 #scl ] !>,\
          	     HCONS <! qeq &\
          		    [ HARG #mch,\
          		      LARG #mcl ] !>,\
              		HOOK.INDEX #index ],\
              ARGS < [ SYNSEM [ LOCAL [ CONT.HOOK.INDEX #scl,\
                                        CAT [ HEAD noun &\
                                                    [ NMZ + ],\
          				    MC na-or-+,\
          				    VAL [ SUBJ #subj,\
          				        SPR < >,\
          					  COMPS < > ]] ] ] ] > ].')
    else:
        supertype = 'morphological-subord-clause-phrase'
        mylang.add(supertype + ' := unary-phrase &\
    [ SYNSEM [ LOCAL [ CAT [ MC -,\
                            VAL [ SUBJ #subj,\
                                  SPR < >,\
                                  COMPS < > ],\
                            HEAD adp & [ MOD < [ LOCAL scopal-mod &\
  						[ CAT [ HEAD verb,\
  							VAL [ SUBJ < >,\
  							      SPR < >,\
  							      COMPS < > ]],\
  						  CONT.HOOK [ LTOP #mcl,\
  								INDEX #index ]]] > ]]]],\
      C-CONT [ RELS <! [ ARG1 #mch,\
  		     ARG2 #sch ] !>,\
  	     HCONS <! qeq &\
  		    [ HARG #mch,\
  		      LARG #mcl ],\
                qeq &\
  		    [ HARG #sch,\
  		      LARG #scl ] !>,\
      		HOOK.INDEX #index ],\
      ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD verb,\
  				    MC na-or-+,\
  				    VAL [ SUBJ #subj,\
  				    SPR < >,\
  					  COMPS < > ]],\
  			    CONT.HOOK.LTOP #scl ] ] ] > ].')
    type = build_type_name(lextype)
    pred = cms.get('pred')
    if pred == '':
        pred = '_subord_rel'
    type = type + '-modifying-clause-phrase'
    rules.add(type + ' := ' + type + '.')
    mylang.add(type + ' := ' + supertype + '.')
    while constraints != []:
        mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
    mylang.add(type + ' := [ C-CONT.RELS <! [ PRED "' + pred + '" ] !> ].')
    mylang.set_section('addenda')
    mylang.add('basic-head-mod-phrase-simple :+ [ SYNSEM.LOCAL.CAT.MC #mc,\
      HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
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

def create_subordinated_feature(mylang, roots, cms, ch):
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
            value = shortform_pred(pred)
            mylang.add(value + ' := xsubord.')
    elif cms.get('subordinator') == 'pair':
        for adverb in cms.get('morphpair'):
            pred = adverb.get('subordpred')
            value = shortform_pred(pred)
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
    mylang.add('head-adj-phrase :+\
      [ SYNSEM.SUBORDINATED #subord,\
        NON-HEAD-DTR.SYNSEM.SUBORDINATED #subord ].')
    if 'cs' in ch:
        mylang.add('coord-phrase :+ [ SYNSEM.SUBORDINATED #subord,\
				    LCOORD-DTR.SYNSEM.SUBORDINATED #subord,\
				    RCOORD-DTR.SYNSEM.SUBORDINATED #subord ].')
    mylang.set_section('verb-lex')
    mylang.add('verb-lex := [ SYNSEM.SUBORDINATED none ].')
    mylang.set_section('lexrules')
    mylang.add('same-subordinated-lex-rule := lex-rule &\
    [ SYNSEM.SUBORDINATED #subord,\
    DTR.SYNSEM.SUBORDINATED #subord ].')
    mylang.set_section('addenda')
    mylang.add('non-local-change-only-lex-rule :+ same-subordinated-lex-rule.')
    mylang.add('local-change-only-lex-rule :+ same-subordinated-lex-rule.')
    mylang.add('cont-change-only-lex-rule :+ same-subordinated-lex-rule.')
    mylang.add('cat-change-with-ccont-lex-rule :+ same-subordinated-lex-rule.')
    mylang.add('add-only-rule :+ same-subordinated-lex-rule.')
    roots.add('root := [ SYNSEM.SUBORDINATED none ].')

def create_subpair_feature(mylang, roots, morphpair, ch):
    """
    adds the subpair feature to canonical synsem as well as adding constraints
    to phrase types to pass it and mc up
    """
    mylang.set_section('addenda')
    mylang.add('cat :+ [ SUBPAIR subpair ].')
    #can i comment this out?? it might cause problems witht he german analysis
    mylang.add('basic-head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.MC #mc,\
    HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    mylang.add('basic-head-subj-phrase :+\
    [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
      HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair ].')
    mylang.add('basic-head-comp-phrase :+\
    [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
      HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair ].')
    mylang.add('scopal-mod-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR #subpair ] >,\
                                        HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair ].')

    mylang.add('basic-head-opt-subj-phrase :+ [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
    mylang.add('basic-head-spec-phrase-super :+ [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
				  HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
				  NON-HEAD-DTR.SYNSEM [ LOCAL.CAT.SUBPAIR #pair,\
							LOCAL.CAT.VAL [ SPEC < [ LOCAL.CAT.SUBPAIR #pair ] > ]]].')
    mylang.add('basic-head-opt-comp-phrase :+ [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
				HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair ].')
    mylang.add('basic-head-opt-subj-phrase :+ [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
				HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair ].')
    mylang.add('basic-head-mod-phrase-simple :+ [ SYNSEM.LOCAL.CAT.MC #mc,\
		     HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    mylang.add('basic-head-mod-phrase-simple :+ [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
  		     NON-HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair].')
    if 'cs' in ch:
        mylang.add('coord-phrase :+ [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
				    LCOORD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
				    RCOORD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair ].')
    mylang.set_section('features')
    mylang.add('subpair := *top*.')
    mylang.add('nopair := subpair.')
    for pair in morphpair:
        subpair = pair.get('subordpred')
        if subpair.split('_')[0] == '':
            value = subpair.split('_')[1]
        else:
            value = subpair.split('_')[0]
        mylang.add(value + ' := subpair.')

    mylang.set_section('verb-lex')
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.SUBPAIR nopair ].')
    roots.add('root := [ SYNSEM.LOCAL.CAT.SUBPAIR nopair ].')

def has_subpairs(ch):
    """
    Returns true if the grammar will have subordinator pairs and false otherwise
    """
    subpair = False
    for cms in ch.get('cms'):
        if cms.get('subordinator') == 'pair':
            subpair = True
    return subpair

def add_head_compement_rules(mylang, rules, ch):
    """
    Add appropriate head-complement rules for word order.
    """
    mylang.set_section('addenda')
    mylang.add('head :+ [ INIT bool ].')
    subpos = []
    for cms in ch.get('cms'):
        if cms.get('subordinator-type') == 'head':
            subpos.append(cms.get('subposition'))
    wo = ch.get('word-order')
    mylang.set_section('phrases')
    if not cms.get('subord-word-order'):
        if wo == 'sov' or wo == 'osv' or wo == 'ovs' or wo == 'v-final':
            mylang.add('comp-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT - ].')
            if 'before' in subpos:
                mylang.add('adp-head-comp-phrase := basic-head-1st-comp-phrase & head-initial &\
                    [ SYNSEM.LOCAL.CAT.MC #mc,\
                    HEAD-DTR.SYNSEM.LOCAL.CAT [ MC #mc,\
                                            HEAD adp & [ INIT + ] ]].')
                rules.add('adp-head-comp := adp-head-comp-phrase.')
        elif wo == 'svo' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
            mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT + ].')
            if 'after' in subpos:
                mylang.add('adp-comp-head-phrase := basic-head-1st-comp-phrase & head-final &\
                    [ SYNSEM.LOCAL.CAT.MC #mc,\
                    HEAD-DTR.SYNSEM.LOCAL.CAT [ MC #mc,\
                                            HEAD adp & [ INIT - ] ]].')
                rules.add('adp-comp-head := adp-comp-head-phrase.')
        elif wo == 'free' or wo == 'v2':
            mylang.add('comp-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT - ].')
            mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT + ].')
    if ch.get('subord-word-order') == 'vfinal':
        if 'before' in subpos:
            mylang.add('adp-head-comp-phrase := basic-head-1st-comp-phrase & head-initial &\
                                [ SYNSEM.LOCAL.CAT.MC #mc,\
                                HEAD-DTR.SYNSEM.LOCAL.CAT [ MC #mc,\
                                                        HEAD adp & [ INIT + ] ]].')
            rules.add('adp-head-comp := adp-head-comp-phrase.')

def add_morphological_constraints(lextype, constraints, cms, type):
    """
    Loop through morphologial constraints and add them to the the constraints
    list at the appropriate path.
    """
    for feat in cms.get('feat'):
        if feat.get('name') != 'nominalization':
            lextype.append(feat.get('value'))
            if feat.get('name') == 'mood' or feat.get('name') == 'aspect':
                path = 'LOCAL.CONT.HOOK.INDEX.E.'
            else:
                path = 'LOCAL.CAT.HEAD.'
            constraints.append('SYNSEM.' + path + feat.get('name').upper() + ' #feat')
            if type == 'head':
                constraints.append('SYNSEM.LOCAL.CAT.VAL.COMPS < [ ' + path + feat.get('name').upper()\
                                   + ' #feat & ' + feat.get('value') + ' ] >')
            elif type == 'adverb':
                constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ ' + path + feat.get('name').upper()\
                                   + ' #feat & ' + feat.get('value') + ' ] >')
            elif type == 'phrase':
                constraints.append('ARGS < [ SYNSEM.' + path + feat.get('name').upper()\
                                   + ' #feat & ' + feat.get('value') + ' ] >')
    return lextype, constraints

def build_type_name(lextype):
    """
    From a list of words that need to be included in a type name, build the type name
    """
    type = ''
    type += lextype.pop()
    for s in lextype:
        type += '-' + s
    return type

def is_nominalized(cms, ch):
    """
    Find out if nominalization is among the special morphology features, and return the name of
    the strategy and if there is an nmzRel
    """
    nominalized = 'no'
    nom_strategy = ''
    nmzRel = ''
    for feat in cms.get('feat'):
        if feat.get('name') == 'nominalization':
            nominalized = 'yes'
            nom_strategy = feat.get('value')
    for ns in ch.get('ns'):
        if ns.get('name') == nom_strategy:
            nmzRel = ns.get('nmzRel')

    return nominalized, nmzRel, nom_strategy

def shortform_pred(pred):
    """
    break the predication into a short name that can be used to distinguish between
    different subordinators
    """
    if pred.split('_')[0] == '':
        value = pred.split('_')[1]
    else:
        value = pred.split('_')[0]
    return value

def add_to_lexicon(morphtype, typename, type, lexicon):
    """
    add the subordinator or adverb to lexicon
    """
    orth = morphtype.get(type + 'orth')
    orthstr = orth_encode(orth)
    pred = morphtype.get(type + 'pred')
    name = TDLencode(morphtype.get(type + 'name'))
    lexicon.add(name + ' := ' + typename + ' &\
                      [ STEM < "' + orthstr + '" >,\
                   SYNSEM.LKEYS.KEYREL.PRED "' + pred + '"].')

def get_subord_stemids(ch, stemids):
    """
    A function called by insert_ids() in lexical_items.py to
    check for name-space-collisions
    """
    for cms in ch.get('cms'):
        for freemorph in cms.get('freemorph'):
            orth = freemorph.get('orth')
            if orth in stemids.keys():
                stemids[orth] += 1
            else:
                stemids[orth] = 1
        for morphpair in cms.get('morphpair'):
            subordorth = morphpair.get('subordorth')
            matrixorth = morphpair.get('matrixorth')
            if subordorth in stemids.keys():
                stemids[subordorth] += 1
            else:
                stemids[subordorth] = 1
            if matrixorth in stemids.keys():
                stemids[matrixorth] += 1
            else:
                stemids[matrixorth] = 1
    return stemids

def add_subord_name(ch, stemids, stemidcounters):
    """
    A function called by insert_ids() in lexical_items.py to
    create a "name" for each subordinator in choices, preventing
    name-space-collisions
    """
    for cms in ch.get('cms'):
        for freemorph in cms.get('freemorph'):
            orth = freemorph.get('orth')
            if stemids[orth] == 1:
                ch[freemorph.full_key + '_name'] = orth
            elif orth not in stemidcounters:
                stemidcounters[orth] = 1
                ch[freemorph.full_key + '_name'] = orth + '_1'
            else:
                stemidcounters[orth] += 1
                ch[freemorph.full_key + '_name'] = orth + '_' + str(stemidcounters[orth])
        for morphpair in cms.get('morphpair'):
            subordorth = morphpair.get('subordorth')
            matrixorth = morphpair.get('matrixorth')
            #first look at the subordinator
            if stemids[subordorth] == 1:
                ch[morphpair.full_key + '_subordname'] = subordorth
            elif subordorth not in stemidcounters:
                stemidcounters[subordorth] = 1
                ch[morphpair.full_key + '_subordname'] = subordorth + '_1'
            else:
                stemidcounters[subordorth] += 1
                ch[morphpair.full_key + '_subordname'] = subordorth + '_' + str(stemidcounters[subordorth])
            #then look at the matrix adverb
            if stemids[matrixorth] == 1:
                ch[morphpair.full_key + '_matrixname'] = matrixorth
            elif matrixorth not in stemidcounters:
                stemidcounters[matrixorth] = 1
                ch[morphpair.full_key + '_matrixname'] = matrixorth + '_1'
            else:
                stemidcounters[subordorth] += 1
                ch[morphpair.full_key + '_matrixname'] = matrixorth + '_' + str(stemidcounters[matrixorth])

