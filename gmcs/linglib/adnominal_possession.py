import gmcs.tdl
from gmcs.linglib.word_order import customize_major_constituent_order
from gmcs.linglib.morphotactics import all_position_classes
###################################################################
# Atoms of a possessive strategy:
#
# The rule that combines possessor and possessum
#     Sometimes must be added separately
#     Sometimes already there
#
# For each affix:
# Lexical rule type
#
# For each non-affix:
# Lexical entries + rules to attach these words to their heads
#
###################################################################


POSS_REL = '''arg12-ev-relation & [ PRED "poss_rel", \
                                    LBL #lbl, \
                                    ARG1 #possessum, \
                                    ARG2 #possessor ] '''

POSSESSUM_EXIST_REL = '''quant-relation & [ PRED "exist_q_rel", \
                                            ARG0 #possessum, \
                                            RSTR #harg ]'''

POSSESSOR_RULE=' :=\
                  [ SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,\
                                             COMPS #comps,\
                                             SUBJ #subj ] ],\
                    DTR.SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,\
                                                 COMPS #comps,\
                                                 SUBJ #subj ], \
                                       CONT.HOOK #hook & [ INDEX #possessor ] ] ] ].'

POSSESSUM_RULE=' :=\
                  [ SYNSEM.LOCAL [ CAT.VAL [ SPEC #spec,\
                                             SUBJ #subj ] ],\
                    DTR.SYNSEM.LOCAL [ CAT.VAL [ SPEC #spec,\
                                                 SUBJ #subj ], \
                                       CONT.HOOK #hook ] ] ].'

# PRIMARY FUNCTION
def customize_adnominal_possession(mylang,ch,rules,irules,lexicon):
    for strat in ch.get('poss-strat',[]):
        customize_rules(strat,mylang,ch,rules)
        if strat.get('possessor-bound')=='possessor-affix' or strat.get('possessum-bound')=='possessum-affix':
            customize_irules(strat,mylang,ch,irules)
        if strat.get('possessor-bound')=='possessor-non-affix' or strat.get('possessum-bound')=='possessum-non-affix':
            customize_lexicon(strat,mylang,ch,lexicon)

# SECONDARY FUNCTIONS
def customize_rules(strat,mylang,ch,rules):
# TODO: deal with free word order
    """
    Adds the necessary phrase rule to combine possessor and possessum
    If rule already exists (head-comp case), then make sure its order is correct.
    """
    phrase_rule=""
    strat_order=strat.get('order')
    rule_added=False
    mylang.set_section('phrases')
    # Add a head-compositional variant of head-spec if possessor = spec
    if strat.get('mod-spec')=='spec':
        phrase_rule="head-spec-poss-phrase"
        # Note: added the constraint on head type so that this would never do the work of attaching determiners to nouns
        # (Found helpful in scenario: possessor marking adposition, spec-like attachment.
        # Not sure if this is too little constriction -- only testing with mini english so far (12/22/2017)
        mylang.add(phrase_rule + ' :=  basic-head-spec-phrase-super & [  NON-HEAD-DTR.SYNSEM [ LOCAL.CAT.HEAD +nvjrpcmo,\
                                                                                               OPT - ],\
                                                                         HEAD-DTR.SYNSEM.LOCAL.CONT.HOOK #hook ,\
                                                                         C-CONT.HOOK #hook ].')
        rule_added=True
    # Add either head-mod or head-comp if possessor = mod
    # Exception: no rule added if preexistent head-comps has correct order
    elif strat.get('mod-spec')=='mod':
        if strat.get('mark-loc')=='possessum-marking' or 'both-marking':
            phrase_rule="head-comp-poss-phrase"
            # Check if the existing head-comp rule has the correct order; 
            # if not, add a new rule with correct order that only applies to poss-phrases.
            head_comp_order=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']
            if head_comp_order=='head-comp':
                head_comp_order='head-initial'
            else:
                head_comp_order='head-final'
            if head_comp_order!=strat_order:
                mylang.add(phrase_rule +' := basic-head-1st-comp-phrase &\
                                           [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD noun,\
                                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD noun  ].')
                rule_added=True
        else:
            phrase_rule="head-mod-poss-phrase"
            mylang.add(phrase_rule+' := basic-head-mod-phrase-simple & head-compositional & \
                                        [ SYNSEM.LOCAL.CAT.VAL [ SPEC #spec ], \
                                          HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC #spec ] ].')
            rule_added=True
    # If a specialized poss phrase rule was added, require that the marked constituent be marked possessive.
    if rule_added:
        if strat.get('mark-loc')=='possessor-marking':
            mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive,\
                                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS possessor ].',merge=True)
        if strat.get('mark-loc')=='possessum-marking':
            mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS possessum,\
                                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive ].',merge=True)
        if strat.get('mark-loc')=='both-marking':
            mylang.add(phrase_rule+':= [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS possessor,\
                                         HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS possessum ].',merge=True)
    # Make non-possessive phrases reject possessive nouns:
    if ch.get('has-dets')=='yes':
#        mylang.add('head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD  +vjrpcdmo ].',merge=True)
        mylang.add('head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive ].',merge=True)
    adj_head=False
    head_adj=False
    for adj in ch.get('adj',[]):
        modpos=adj.get('modpos',[])
        if modpos=='before':
            head_adj=True
        elif modpos=='after':
            adj_head=True
        elif modpos=='either':
            head_adj=True
            adj_head=True
    for pc in ch.get('adj-pc',[]):
        for lrt in pc.get('lrt'):
            modpos=lrt.get('modpos')
            if modpos=='before':
                head_adj=True
            elif modpos=='after':
                adj_head=True
            elif modpos=='either':
                head_adj=True
                adj_head=True
#    if head_adj: mylang.add('head-adj-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD  +vjrpcdmo ].',merge=True)
#    if adj_head: mylang.add('adj-head-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD  +vjrpcdmo ].',merge=True)
    if head_adj: mylang.add('head-adj-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive ].',merge=True)
    if adj_head: mylang.add('adj-head-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive ].',merge=True)
    if phrase_rule=='head-comp-poss-phrase' and rule_added:
        head_comp_order=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']
        # TODO: add this to head-comp or comp-head depending on what exists in the lg.
        mylang.add(head_comp_order+'-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive ].')
    # If a specialized poss phrase rule was added, adds word order info to the phrase rule:
    if rule_added: 
        mylang.add(phrase_rule +' := '+strat.get('order')+'.',merge=True)
        # If a specialized poss phrase rule was added, adds rule to rules.tdl
        rules.add(phrase_rule.replace('-phrase','') + ':= '+phrase_rule+'. ' )
        # TODO: remove this:
        # Switch for if possessor is NP vs NOM.
        np_nom=strat.get('np-nom')
        if np_nom=='np':
            mylang.add(phrase_rule+' := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR olist ].',merge=True)
        elif np_nom=='nom':
            mylang.add(phrase_rule+' := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR 1-list ].',merge=True)



# NOTE: customize_irules pseudocode/code doesn't yet deal with situations where one marker is an affix and one isn't:
# NOTE: customize_irules and customize_lex pseudocode/code both don't handle agreement yet
def customize_irules(strat,mylang,ch,irules):
    #TODO: this method for retrieving the strategy name is garbage. Fix it.
    strat_name=strat.full_keys()[0].split("_")[0]
    strat_num=strat_name[-1]
    for pc in all_position_classes(ch):
        pc_key = pc.full_key
        pc_inputs = pc.get('inputs',[])
        idx = pc['lrt'].next_iter_num() if 'lrt' in pc else 1
        for lrt in pc.get('lrt',[]):
            for feat in lrt['feat']:
                # Go through the pc info till you find the strategy you're actually dealing with
                if strat_name in str(feat['name']):
                    # Then narrow down which kind of rule to add:
                    mod_spec=strat.get('mod-spec')
                    mark_loc=strat.get('mark-loc')
                    mylang.set_section('lexrules')


                    if mark_loc=='possessor-marking' or 'both-marking':
                        # Add the basic possessor rule defn:
                        possessor_rule_name = 'possessor-lex-rule-'+strat_num
                        if mod_spec=='spec':
                            mylang.add(possessor_rule_name+POSSESSOR_RULE)
                            mylang.add(possessor_rule_name+' := val-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ VAL [ SPEC.FIRST.LOCAL [ CAT [ HEAD noun ],\
                                                                                         CONT.HOOK [ INDEX #possessum & [ COG-ST uniq-id ],\
                                                                                                     LTOP #lbl ] ] ] ] ,\
                                             C-CONT [ HOOK #hook ,\
                                                      RELS <! '+ POSS_REL  +' , '+POSSESSUM_EXIST_REL+ ' !>, \
                                                                   HCONS <! qeq & [ HARG #harg, LARG #lbl ] !>, \
                                                                                                ICONS <! !>  ] ].',merge=True)
                        else: 
                            if mark_loc=='possessor-marking':
                                mylang.add(possessor_rule_name+POSSESSOR_RULE)
                                mylang.add(possessor_rule_name+' := head-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ HEAD.MOD.FIRST [ LOCAL [ CAT.HEAD noun, \
                                                                                         CONT.HOOK [ INDEX #possessum, \
                                                                                                     LTOP #lbl ] ], \
                                                                                 OPT - ], \
                                                                VAL [ SPEC #spec ] ] ,\
                                             C-CONT [ HOOK #hook ,\
                                                RELS <! '+POSS_REL+' !>,\
                                                HCONS <! !>, \
                                                ICONS <! !>  ], \
                                             DTR.SYNSEM.LOCAL.CAT.VAL.SPEC #spec  ].',merge=True)
                            elif mark_loc=='both-marking':
                                mylang.add(possessor_rule_name+' := add-only-no-ccont-rule &\
                                                                    [ SYNSEM.LOCAL [ CAT [ HEAD.POSS possessor,\
                                                                                           VAL #val ] ] ,\
                                                                      DTR.SYNSEM.LOCAL [ CAT.VAL #val ] ].')



                    if mark_loc=='possessum-marking' or 'both marking':
                        possessum_rule_name = 'possessum-lex-rule-'+strat_num
                        mylang.add(possessum_rule_name+POSSESSUM_RULE)
                        if mod_spec=='spec':
                            mylang.add(possessum_rule_name+':=  val-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ VAL [ COMPS #comps,\
                                                                      SPR < [ LOCAL [ CAT [ HEAD noun ] ] ] > ] ] ,\
                                             C-CONT [ HOOK #hook & [ INDEX.COG-ST uniq-id ],\
                                                      HCONS <! !>, \
                                                      ICONS <! !>  ],\
                                             DTR.SYNSEM.LOCAL [ CAT.VAL.COMPS #comps ] ].',merge=True)
                            if mark_loc=='possessum-marking':
                                mylang.add(possessum_rule_name+' := [ SYNSEM.LOCAL.CAT.VAL.SPR <[ LOCAL.CONT.HOOK [ INDEX #possessor ] ]>,\
                                                                      C-CONT [ HCONS <! qeq & [ HARG #harg, LARG #lbl ] !> ,\
                                                                               RELS <! '+POSS_REL+',\
                                                                                       '+POSSESSUM_EXIST_REL+' !> ],\
                                                                                       DTR.SYNSEM.LOCAL.CONT.HOOK [ INDEX #possessum,\
                                                                                                                    LTOP #lbl ] ].')
                            else:
                                mylang.add(possessum_rule_name+' := [ C-CONT.RELS <! !> ].')
                        if mod_spec=='mod':
                            # Should append things to COMPS list, not overwrite
                            mylang.add(possessum_rule_name+':= val-change-with-ccont-lex-rule & \
                                                       [ SYNSEM.LOCAL.CAT [ VAL [ SPR #spr, \
                                                                                  COMPS.FIRST.LOCAL [ CAT.HEAD noun, \
                                                                                                      CONT.HOOK [ INDEX #possessor ] ] ] ],\
                                                         C-CONT [ HOOK #hook ,\
                                                                  RELS <! '+POSS_REL+' !>,\
                                                                  HCONS <! !>,\
                                                                  ICONS <! !>  ],\
                                                         DTR.SYNSEM.LOCAL [ CAT.VAL.SPR #spr,\
                                                                            CONT.HOOK [ INDEX #possessum,\
                                                                                        LTOP #lbl ] ] ].',merge=True)
                            if mark_loc=='both-marking':
                                mylang.add(possessum_rule_name+' :=\
                                           [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD noun & [ POSS possessor ] ].')
 


def customize_lexicon(strat,mylang,ch,lexicon):
    strat_name=strat.full_keys()[0].split("_")[0]
    strat_num=strat_name[-1]
    mark_loc=strat.get('mark-loc')
    mod_spec=strat.get('mod-spec')    
    if mark_loc=='possessor-marking':
        orth=strat.get('possessor-orth')
        # UNTESTED:
        # TODO: check if already-existing phrase rules will work; if not add a head-comps rule that will only take possessive heads
        # Problem with this one: head-comps doesn't pass up SPEC value of head dtr.
        mylang.add('two-rel-adposition-lex := basic-icons-lex-item &\
  [ SYNSEM [ LOCAL [ CAT [ HEAD adp,\
                           VAL.COMPS < [ LOCAL [ CAT cat-sat,\
                                                 CONT.HOOK [ INDEX #ind,\
                                                             ICONS-KEY.IARG1 #clause ] ] ] > ],\
                     CONT.HOOK.CLAUSE-KEY #clause ],\
             LKEYS.KEYREL arg12-ev-relation & [ ARG2 #ind ] ] ].')
        if mod_spec=='spec':
            mylang.add('possessor-adp-lex := two-rel-adposition-lex &\
                                 [  SYNSEM.LOCAL [ CAT  [ HEAD.POSS possessor,\
                                                          VAL [ COMPS.FIRST.LOCAL [ CAT.HEAD noun,\
                                                                                   CONT.HOOK.INDEX #possessor ],\
                                                               SPEC.FIRST.LOCAL [ CONT.HOOK [ INDEX #possessum,\
                                                                                              LTOP #lbl ],\
                                                                                  CAT.VAL.SPR < [ ] > ] ] ],\
                                                  CONT [ RELS <! '+POSS_REL+',\
                                                                 '+POSSESSUM_EXIST_REL+' !>,\
                                                         HCONS <!  qeq & [ HARG #harg, LARG #lbl ] !>,\
                                                         ICONS <! !>   ] ] ].')
        if mod_spec=='mod':
            mylang.add('possessor-adp-lex := two-rel-adposition-lex &\
                                 [  SYNSEM.LOCAL [ CAT [ HEAD.POSS possessor,\
                                                         VAL.COMPS.FIRST.LOCAL [ CAT.HEAD noun,\
                                                                                 CONT.HOOK.INDEX #possessor ],\
                                                         HEAD.MOD.FIRST.LOCAL [ CONT.HOOK [ INDEX #possessum,\
                                                                                            LTOP #lbl ],\
                                                                                CAT.VAL.SPR < [ ] > ] ],\
                                                  CONT [ RELS <! arg12-ev-relation & [ PRED "poss_rel",\
                                                                                            ARG1 #possessum,\
                                                                                            ARG2 #possessor ] !>,\
                                                         HCONS <! !>,\
                                                         ICONS <! !>   ] ] ].')
        lexicon.add('possessor-adp-'+strat_num+' := possessor-adp-lex &\
                                                  [ STEM < "'+orth+'" >].')
    elif mark_loc=='possessum-marking':
        orth=strat.get('possessum-orth')
        if mod_spec=='spec':
            mylang.add('possessum-noun-lex := noun-lex &\
                                 [  SYNSEM.LOCAL [ CAT  [ HEAD.POSS possessum,\
                                                          VAL [ SPR.FIRST.LOCAL [ CAT.HEAD noun,\
                                                                                   CONT.HOOK.INDEX #possessor ],\
                                                               COMPS.FIRST.LOCAL [ CONT.HOOK [ INDEX #possessum,\
                                                                                              LTOP #lbl ],\
                                                                                  CAT.VAL.SPR < [ ] > ] ] ],\
                                                  CONT [ RELS <! '+POSS_REL+',\
                                                                 '+POSSESSUM_EXIST_REL+' !>,\
                                                         HCONS <!  qeq & [ HARG #harg, LARG #lbl ] !>,\
                                                         ICONS <! !>   ] ] ].')
        if mod_spec=='mod':
            mylang.add('possessum-noun-lex := noun-lex &\
                                 [  SYNSEM.LOCAL [ CAT [ HEAD.POSS possessum,\
                                                         VAL [ COMPS < [ LOCAL [ CONT.HOOK [ INDEX #possessum,\
                                                                                             LTOP #lbl ],\
                                                                                 CAT.VAL.SPR < [ ] > ] ] ] ],\
                                                                       [ LOCAL [ CAT.HEAD noun,\
                                                                                 CONT.HOOK.INDEX #possessor ] ] >,\
                                                  CONT [ RELS <! '+POSS_REL+',\
                                                                 '+POSSESSUM_EXIST_REL+' !>,\
                                                         HCONS <!  qeq & [ HARG #harg, LARG #lbl ] !>,\
                                                         ICONS <! !>   ] ] ].')

        lexicon.add('possessum-noun-'+strat_num+' := possessor-noun-lex &\
                                                  [ STEM < "'+orth+'" >].')

#     IF: either possessor or possessum mark is a separate word, add the correct lexical entry
#     Possessor-marker:
#        If the possessor is specifier-like:
#            ADD possessor-det-lex, with SPR<[possessor]>, SPEC<[possessum]>, carrying poss_rel
#             OR TRY:
#            ADD possessor-adp-lex, with COMPS<[possessor]>,SPEC<possessum]>
#        If the possessor is modifier-like:
#            ADD possessor-adp-lex, with COMPS<[possessor]>, MOD<[possessum]>
#     Possessum-marker:
#        If the possessor is specifier-like:
#             ADD possessum-noun-lex, with COMPS<[possessum]>, SPR<[possessor]>
#        If the possessor is modifier-like:
#             ADD possessum-noun-lex, with COMPS<[possessum][possessor]>
