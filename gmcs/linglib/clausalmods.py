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
    nominalized, nom_strategy = is_nominalized(cms)

    if cms.get('subordinator-type') == 'head':
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
            mylang.set_section('noun-lex')
            mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
            mylang.set_section('subordlex')
            for ns in ch.get('ns'):
                if ns.get('name') == nom_strategy:
                    nmzRel = ns.get('nmzRel')
            if nmzRel == 'no':
                mylang.add('subord-with-nominalized-comp-no-rel-lex := adposition-subord-lex-item &\
        [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ CONT.HOOK [ LTOP  #mod,\
                                            INDEX  #index ]] >,\
                                VAL.COMPS < [LOCAL[CAT[HEAD noun &\
                                        			[ NMZ + ]],\
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

                # mylang.add('subord-with-nominalized-comp-no-rel-lex :=\
                # [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >,\
                #                    SPR < > ,\
        			#    COMPS < [ LOCAL [ CAT [ HEAD noun &\
                #                         			[ NMZ + ],\
                #                    			MC - ] ] ] > ]].')
                if cms.get('subordinator') == 'pair':
                    mylang.add('subord-with-nominalized-comp-no-rel-lex := [ SYNSEM [ LOCAL.CAT.SUBPAIR #subpair,\
                                      LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR #subpair ] > ]].' )
            else:
                mylang.add('subord-with-nominalized-comp-lex := adposition-subord-lex-item &\
        [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL.CONT.HOOK[LTOP  #mod,\
                                                                INDEX  #index ]] >,\
                               VAL.COMPS < [LOCAL[CAT[HEAD noun],\
                                                CONT.HOOK.INDEX  #comp ]] > ],\
                        CONT[HCONS <! qeq &\
                              [HARG  #h1,\
                               LARG  #mod ] !>,\
                           HOOK.INDEX  #index ]],\
                   LKEYS.KEYREL[ARG0 event,\
                               ARG1  #h1,\
                                ARG2  #comp ]]].')

                # mylang.add('nom-subord-lex-item := subord-with-nominalized-comp-lex &\
                # [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >,\
                #                    SPR < > ,\
        			#    COMPS < [ LOCAL [ CAT [ HEAD noun &\
                #                         			[ NMZ + ],\
                #                    			MC - ] ] ] > ]].')
                if cms.get('subordinator') == 'pair':
                    mylang.add('scopal-mod-with-nominalized-comp-lex := [ SYNSEM [ LOCAL.CAT.SUBPAIR #subpair,\
                                      LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR #subpair ] > ]].' )
        else:
            mylang.set_section('subordlex')
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
            if cms.get('subordinator') == 'pair':
                mylang.add('scopal-mod-with-comp-lex := [ SYNSEM [ LOCAL.CAT.SUBPAIR #subpair,\
                                    LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR #subpair ] > ]].')
    #         mylang.add('subord-lex-item := scopal-mod-with-comp-lex &\
    # [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >,\
    #                             COMPS <  #comps > ]]],\
    #   ARG-ST <  #comps &\
    #               [LOCAL.CAT[ HEAD verb,\
    #                           MC -]] > ].')

    elif cms.get('subordinator-type') == 'adverb':
        mylang.set_section('subordlex')
        mylang.add('adverb-subord-lex-item := no-rels-hcons-lex-item &\
  [ SYNSEM [ LOCAL [ CAT [ HEAD.MOD < [ LOCAL intersective-mod &\
                                              [ CAT [ MC -,\
                                                      HEAD verb ] ] ] > ] ] ]].')

    lextype = []
    constraints = []
    pos = cms.get('position')
    subpos = cms.get('subposition')
    attach = cms.get('modifier_attachment')
    if cms.get('subordinator-type') == 'head':
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
        if attach == 's':
            lextype.append('s-attach')
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL.SUBJ < > ] >')
        elif attach == 'vp':
            lextype.append('vp-attach')
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL.SUBJ < [ ] > ] >')
    elif cms.get('subordinator-type') == 'adverb':
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

    if cms.get('specialmorph') == 'on':
        lextype, constraints = add_morphological_constraints(lextype, constraints, cms, 'lextype')

    saved_constraints = constraints
    saved_lextype = lextype
    if cms.get('subordinator-type') == 'head':
        if cms.get('shared-subj') == 'on':
            lextype.append('shared-subject')
            constraints.append('SYNSEM.LOCAL.CAT [ HEAD.MOD < [ LOCAL [ CONT.HOOK.XARG #xarg ]] >,\
                        VAL.COMPS < [ LOCAL [ CAT [ VAL [ SUBJ < unexpressed > ]],\
        	  		    CONT.HOOK.XARG #xarg ]]  > ]')
        else:
            constraints.append('SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.VAL.SUBJ < > ] >')

        if cms.get('subordinator') == 'free':
            type = build_lex_type(lextype)
            #const = build_constraints(constraints)
            if nominalized == 'yes':
                if nmzRel == 'no':
                    type += '-nom-no-rel-subord-lex-item'
                    mylang.add(type + ' := subord-with-nominalized-comp-no-rel-lex & [ ' + constraints.pop() + ' ].')
                    while constraints != []:
                        mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
                else:
                    type += '-nom-subord-lex-item'
                    #mylang.add(type + ' := nom-subord-lex-item &' + const)
                    mylang.add(type + ' := subord-with-nominalized-comp-lex & [ ' + constraints.pop() + ' ].')
                    while constraints != []:
                        mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
            else:
                type += '-subord-lex-item'
                #mylang.add(type + ' := subord-lex-item & ' + const)
                mylang.add(type + ' := subord-with-verbal-comp-lex & [ ' + constraints.pop() + ' ].')
                while constraints != []:
                    mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
            if cms.get('subordinator') == 'free':
                for freemorph in cms.get('freemorph'):
                    orth = freemorph.get('orth')
                    orthstr = orth_encode(orth)
                    pred = freemorph.get('pred')
                    lexicon.add(orthstr + ' := ' + type + ' &\
                                  [ STEM < "' + orthstr + '" >,\
                               SYNSEM.LKEYS.KEYREL.PRED "' + pred + '"].')
        elif cms.get('subordinator') == 'pair':
            for morphpair in cms.get('morphpair'):
                pred = morphpair.get('subordpred')
                value = shortform_pred(pred)
                lextype = [value] + lextype
                type = build_lex_type(lextype)
                constraints.append('SYNSEM.LOCAL.CAT.SUBPAIR ' + value)
                #const = build_constraints(constraints)
                if nominalized == 'yes':
                    if nmzRel == 'no':
                        type += '-nom-no-rel-subord-lex-item'
                        mylang.add(type + ' := subord-with-nominalized-comp-no-rel-lex & [ ' + constraints.pop() + ' ].')
                        while constraints != []:
                            mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
                    else:
                        type += '-nom-subord-lex-item'
                        #mylang.add(type + ' := nom-subord-lex-item &' + const)
                        mylang.add(type + ' := subord-with-nominalized-comp-lex & [ ' + constraints.pop() + ' ].')
                        while constraints != []:
                            mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
                else:
                    type += '-subord-lex-item'
                    #mylang.add(type + ' := subord-lex-item & ' + const)
                    mylang.add(type + ' := subord-with-verbal-comp-lex & [ ' + constraints.pop() + ' ].')
                    while constraints != []:
                        mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
                for morphpair in cms.get('morphpair'):
                    orth = morphpair.get('subordorth')
                    orthstr = orth_encode(orth)
                    pred = morphpair.get('subordpred')
                    lexicon.add(orthstr + ' := ' + type + ' &\
                                  [ STEM < "' + orthstr + '" >,\
                               SYNSEM.LKEYS.KEYREL.PRED "' + pred + '"].')

    elif cms.get('subordinator-type') == 'adverb':
        if cms.get('subordinator') == 'free':
            for adverb in cms.get('freemorph'):
                constraints = saved_constraints
                lextype = saved_lextype
                pred = adverb.get('pred')
                value = shortform_pred(pred)
                lextype = [ value ] + lextype
                constraints.append('SYNSEM.SUBORDINATED ' + value)
                if cms.get('subordinator') == 'pair':
                    constraints.append('SYNSEM.LOCAL.CAT.SUBPAIR ' + value)
                if cms.get('adverb_attaches') == 's':
                    if cms.get('shared-subj') == 'on':
                        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < unexpressed >, COMPS < > ]] >')
                    else:
                        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
                elif cms.get('adverb_attaches') == 'vp':
                    constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
                if cms.get('adverb_attaches') == 'both':
                    constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
                type = build_lex_type(lextype)
                type += '-adv-subord-lex-item'
                mylang.add(type + ' := adverb-subord-lex-item & [ ' + constraints.pop() + ' ].')
                while constraints != []:
                    mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
                orth = adverb.get('orth')
                orthstr = orth_encode(orth)
                lexicon.add(orthstr + ' := ' + type + ' & [ STEM < "' + orthstr + '" > ].')
        elif cms.get('subordinator') == 'pair':
            for adverb in cms.get('morphpair'):
                constraints = saved_constraints
                lextype = saved_lextype
                pred = adverb.get('subordpred')
                value = shortform_pred(pred)
                lextype = [ value ] + lextype
                constraints.append('SYNSEM.SUBORDINATED ' + value)
                constraints.append('SYNSEM.LOCAL.CAT.SUBPAIR ' + value)
                if cms.get('adverb_attaches') == 's':
                    constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
                elif cms.get('adverb_attaches') == 'vp':
                    constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
                if cms.get('adverb_attaches') == 'both':
                    constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
                type = build_lex_type(lextype)
                type += '-adv-subord-lex-item'
                mylang.add(type + ' := adverb-subord-lex-item & [ ' + constraints.pop() + ' ].')
                while constraints != []:
                    mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
                orth = adverb.get('subordorth')
                orthstr = orth_encode(orth)
                lexicon.add(orthstr + ' := ' + type + ' & [ STEM < "' + orthstr + '" > ].')



def add_subord_phrasal_types(mylang, rules, cms, ch):
    """
    Add the phrase type definitions
    """
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
        print('its an adverb')
        if cms.get('subposition') == 'before':
            rules.add('adj-head-int := adj-head-int-phrase.')
        elif cms.get('subposition') == 'after':
            rules.add('head-adj-int := head-adj-int-phrase.')
        else:
            rules.add('adj-head-int := adj-head-int-phrase.')
            rules.add('head-adj-int := head-adj-int-phrase.')

        supertype = 'adv-marked-subord-clause-phrase'
        mylang.add(supertype + ' := basic-unary-phrase &\
  [ SYNSEM [ LOCAL [ CAT [ MC -,\
                          VAL.SUBJ #subj,\
                          HEAD [ MOD < [ LOCAL scopal-mod &\
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
				    VAL [ SUBJ #subj,\
				      SPR < >,\
					  COMPS < > ]],\
			    CONT.HOOK.LTOP #scl ],\
                      NON-LOCAL [ REL 0-dlist ] ] ] > ].')
        if cms.get('subordinator') == 'pair':
            mylang.add(supertype + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.SUBPAIR #subpair ] >,\
                                  ARGS < [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair ] > ].')
        pos = cms.get('position')
        attach =cms.get('modifier_attachment')
        if cms.get('subordinator') == 'free':
            for adverb in cms.get('freemorph'):
                pred = adverb.get('pred')
                value = shortform_pred(pred)
                type = value + '-modifying-clause-phrase'
                mylang.add(type + ' := ' + supertype + ' &\
  [ C-CONT.RELS <! [ PRED "' + pred + '" ] !>,\
    ARGS < [ SYNSEM.SUBORDINATED ' + value + ' ] > ].')
                if pos == 'before':
                    mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
                elif pos == 'after':
                    mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
                if attach == 's':
                    mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL.SUBJ < > ] > ].')
                elif attach == 'vp':
                    mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL.SUBJ < [ ] > ] > ].')
                rules.add(value + '-modifying-clause := ' + type + '.')
        elif cms.get('subordinator') == 'pair':
            for adverb in cms.get('morphpair'):
                pred = adverb.get('subordpred')
                value = shortform_pred(pred)
                type = value + '-modifying-clause-phrase'
                mylang.add(type + ' := ' + supertype + ' &\
  [ C-CONT.RELS <! [ PRED "' + pred + '" ] !>,\
    ARGS < [ SYNSEM.SUBORDINATED ' + value + ' ] > ].')
                if pos == 'before':
                    mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
                elif pos == 'after':
                    mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
                if attach == 's':
                    mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL.SUBJ < > ] > ].')
                elif attach == 'vp':
                    mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL.SUBJ < [ ] > ] > ].')
                rules.add(value + '-modifying-clause := ' + type + '.')
        if cms.get('shared-subj') == 'on':
            mylang.add(type + ' := [ SYNSEM.LOCAL.CAT [ HEAD.MOD < [ LOCAL [ CONT.HOOK.XARG #xarg ]] > ],\
                         ARGS < [ SYNSEM.LOCAL [ CONT.HOOK.XARG #xarg,\
                                      CAT [ VAL [ SUBJ < unexpressed > ]]]] >].')
        else:
            mylang.add(type + ' := [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.VAL.SUBJ < > ] >].')

    if cms.get('subordinator-type') == 'head':
        wo = ch.get('word-order')
        if wo == 'sov' or wo == 'osv' or wo == 'ovs' or wo == 'v-final' or wo  == 'v2':
            mylang.set_section('phrases')
            mylang.add('adp-head-comp-phrase := basic-head-1st-comp-phrase & head-initial &\
                [ SYNSEM.LOCAL.CAT.MC #mc,\
                  HEAD-DTR.SYNSEM.LOCAL.CAT [ MC #mc,\
                                                HEAD adp ]].')
            rules.add('adp-head-comp := adp-head-comp-phrase.')

def add_subordinators_matrix_pair_to_lexicon(mylang, lexicon, cms, ch):
    """
    Adds the matrix member of a subordinator pair to the lexicon
    """
    mylang.add('scopal-mod-matrix-lex-item := basic-adverb-lex &\
    [ SYNSEM [ LOCAL [ CAT [ VAL [ SUBJ < >,\
                              SPR < >,\
                              COMPS < > ],\
                        HEAD.MOD < [ LOCAL scopal-mod & [ CAT [ SUBPAIR nopair,\
                                                MC +,\
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
            mylang.add('scopal-mod-matrix-lex-item := [ SYNSEM.SUBORDINATED none ].')
    for adverb in cms.get('morphpair'):
        lextype = []
        constraints = []
        advpos = cms.get('matrix_subposition')
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
        if cms.get('matrix_adverb_attaches') == 's':
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < >, COMPS < > ]] >')
        elif cms.get('matrix_adverb_attaches') == 'vp':
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ SUBJ < [ ] >, COMPS < > ]] >')
        elif cms.get('matrix_adverb_attaches') == 'both':
            constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.VAL [ COMPS < > ]] >')
        type = build_lex_type(lextype)
        type += '-pair-lex-item'
        #mylang.add(type + ' := adverb-subord-lex-item & ' + const)
        mylang.add(type + ' := scopal-mod-matrix-lex-item & [ ' + constraints.pop() + ' ].')
        while constraints != []:
            mylang.add(type + ' := [ ' + constraints.pop() + ' ].')
        orth = adverb.get('matrixorth')
        orthstr = orth_encode(orth)
        lexicon.add(orthstr + ' := ' + type + ' &\
                                          [ STEM < "' + orthstr + '" >,\
                               SYNSEM.LKEYS.KEYREL.PRED "' + matrixpred + '"].')


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
    #mylang.add('basic-head-mod-phrase-simple :+    [ SYNSEM [ LOCAL.CAT [ SUBPAIR #subpair ]],\
    #   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ SUBPAIR #subpair ] ].')
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
    #mylang.add('adj-head-scop-phrase :+ [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
    #		  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair ].')
    mylang.add('basic-head-mod-phrase-simple :+ [ SYNSEM.LOCAL.CAT.MC #mc,\
		     HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    mylang.add('adj-head-int-phrase :+ [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
  		     NON-HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair].')
    mylang.add('head-adj-int-phrase :+ [ SYNSEM.LOCAL.CAT.SUBPAIR #subpair,\
  		     NON-HEAD-DTR.SYNSEM.LOCAL.CAT.SUBPAIR #subpair].')

    mylang.set_section('features')
    #mylang.add(';;; Subordinator Pair Features')
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

def add_morphological_constraints(lextype, constraints, cms, type):
    for feat in cms.get('feat'):
        if type == 'lextype':
            if feat.get('value') != 'nominalization':
                #lextype.append(feat.get('name'))
                lextype.append(feat.get('value'))
                constraints.append('SYNSEM.LOCAL.CAT [ HEAD.' + feat.get('name') + ' #feat,\
        VAL.COMPS < [ LOCAL.CAT.HEAD.' + feat.get('name') + ' #feat & ' + feat.get('value') + ' ] > ]')
        elif type == 'phrase':
            if feat.get('value') != 'nominalization':
                #lextype.append(feat.get('name'))
                lextype.append(feat.get('value'))
                constraints.append('SYNSEM.LOCAL.CAT [ HEAD.' + feat.get('name') + ' #feat ],\
        ARGS < [ SYNSEM.LOCAL.CAT.HEAD.' + feat.get('name') + ' #feat & ' + feat.get('value') + ' ] >')
    return lextype, constraints

def build_lex_type(lextype):
    type = ''
    type += lextype.pop()
    for s in lextype:
        type += '-' + s
    return type

def is_nominalized(cms):
    nominalized = 'no'
    nom_strategy = ''
    for feat in cms.get('feat'):
        if feat.get('value') == 'nominalization':
            nominalized = 'yes'
            nom_strategy = feat.get('name')
    return nominalized, nom_strategy

def shortform_pred(pred):
    if pred.split('_')[0] == '':
        value = pred.split('_')[1]
    else:
        value = pred.split('_')[0]
    return value

def add_morphological_subord_rel(mylang, cms, ch, rules):
    #todo- make the rel a variable and add subord rel if there is no rel
    """
    adds a non-branching rule that puts in a subord_rel (when no other semantic meaning
    is added via a subordinator) and accounts for special morphology, subject raising, and
    nominalization
    """
    nominalized, nom_strategy = is_nominalized(cms)
    constraints = []
    lextype = []
    pos = cms.get('position')
    if pos == 'before':
        lextype.append('prehead')
        constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD -')
    elif pos == 'after':
        lextype.append('posthead')
        constraints.append('SYNSEM.LOCAL.CAT.POSTHEAD +')
    if cms.get('specialmorph') == 'on':
        lextype, constraints = add_morphological_constraints(lextype, constraints, cms, 'phrase')
    mylang.set_section('phrases')
    if cms.get('shared-subj') == 'on':
        lextype.append('shared-subject')
        constraints.append('SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL [ CONT.HOOK.XARG #xarg ]] >,\
    ARGS < [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ < unexpressed > ]],\
	  		    CONT.HOOK.XARG #xarg ]]  >')
    else:
        constraints.append('ARGS < [ SYNSEM.LOCAL.CAT.VAL.SUBJ < > ] >')
    if nominalized == 'yes':
        mylang.set_section('noun-lex')
        mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
        mylang.set_section('phrases')
        for ns in ch.get('ns'):
            if ns.get('name') == nom_strategy:
                nmzRel = ns.get('nmzRel')
        if nmzRel == 'no':
            lextype.append('nmz-no-rel')
            supertype = 'semantically-empty-nominalized-norel-subord-clause-phrase'
            mylang.add(supertype + ' := basic-unary-phrase &\
          [ SYNSEM [ LOCAL [ CAT [ MC -,\
                                  VAL.SUBJ #subj,\
                                  HEAD [ MOD < [ LOCAL scopal-mod &\
        						[ CAT [ HEAD verb,\
        							VAL [ SUBJ < >,\
        							      SPR < >,\
        							      COMPS < > ]],\
        						  CONT.HOOK [ LTOP #mcl,\
        								INDEX #index ]]] > ]]]],\
            C-CONT [ RELS <! [ PRED "_subord_rel" ] &\
        		   [ ARG1 #mch,\
        		     ARG2 #sch ] !>,\
        	     HCONS <! qeq &\
        		    [ HARG #mch,\
        		      LARG #mcl ],\
                      qeq &\
        		    [ HARG #sch,\
        		      LARG #scl ] !>,\
            		HOOK.INDEX #index ],\
            ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD noun,\
        				    VAL [ SUBJ #subj,\
        				        SPR < >,\
        					  COMPS < > ]],\
        			    CONT.HOOK.LTOP #scl ],\
                              NON-LOCAL [ REL 0-dlist ] ] ] > ].')
        else:
            lextype.append('nmz')
            supertype = 'semantically-empty-nominalized-subord-clause-phrase'
            mylang.add(supertype + ' := basic-unary-phrase &\
            [ SYNSEM [ LOCAL [ CAT [ MC -,\
                                    VAL.SUBJ #subj,\
                                    HEAD [ MOD < [ LOCAL scopal-mod &\
          						[ CAT [ HEAD verb,\
          							VAL [ SUBJ #subj,\
          							      SPR < >,\
          							      COMPS < > ]],\
          						  CONT.HOOK [ LTOP #mcl,\
          								INDEX #index ]]] > ]]]],\
              C-CONT [ RELS <! [ PRED "_subord_rel" ] &\
          		   [ ARG1 #mch,\
          		     ARG2 #scl ] !>,\
          	     HCONS <! qeq &\
          		    [ HARG #mch,\
          		      LARG #mcl ] !>,\
              		HOOK.INDEX #index ],\
              ARGS < [ SYNSEM [ LOCAL [ CONT.HOOK.INDEX #scl,\
                                        CAT [ HEAD noun,\
          				    VAL [ SUBJ #subj,\
          				        SPR < >,\
          					  COMPS < > ]] ],\
                                NON-LOCAL [ REL 0-dlist ] ] ] > ].')
    else:
        supertype = 'morphological-subord-clause-phrase'
        mylang.add(supertype + ' := basic-unary-phrase &\
    [ SYNSEM [ LOCAL [ CAT [ MC -,\
                            VAL.SUBJ #subj,\
                            HEAD [ MOD < [ LOCAL scopal-mod &\
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
  				    VAL [ SUBJ #subj,\
  				    SPR < >,\
  					  COMPS < > ]],\
  			    CONT.HOOK.LTOP #scl ],\
                        NON-LOCAL [ REL 0-dlist ] ] ] > ].')
    type = build_lex_type(lextype)
    pred = cms.get('pred')
    # if pred != '':
    #   value = shortform_pred(pred)
    #   type = value + '-' + type
    # else:
    #   pred = '_subord_rel'
    if pred == '':
        pred = '_subord_rel'
    rules.add(type + '-modifying-clause := ' + type + '-modifying-clause-phrase.')
    type = type + '-modifying-clause-phrase'
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

        if subord == 'none':
            add_morphological_subord_rel(mylang, cms, ch, rules)