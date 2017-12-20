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
                                       CONT.HOOK #hook & [ INDEX #possessor  ] ] ] ].'

POSSESSUM_RULE=' :=\
                  [ SYNSEM.LOCAL [ CAT.VAL [ SPEC #spec,\
                                             SUBJ #subj ] ],\
                    DTR.SYNSEM.LOCAL [ CAT.VAL [ SPEC #spec,\
                                                 SUBJ #subj ], \
                                       CONT.HOOK.INDEX #possessum  ] ] ].'

# PRIMARY FUNCTION
def customize_adnominal_possession(mylang,ch,rules,irules,lexicon):
    # TODO: add POSS feature and POSS.PNG to head feature here.
    for strat in ch.get('poss-strat',[]):
        customize_rules(strat,mylang,ch,rules)
        customize_irules(strat,mylang,ch,irules)
        #customize_lexicon(strat,mylang,ch,lexicon)

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
        mylang.add(phrase_rule + ' :=  basic-head-spec-phrase-super & [  NON-HEAD-DTR.SYNSEM [ OPT - ],\
                                                                         HEAD-DTR.SYNSEM.LOCAL.CONT.HOOK #hook ,\
                                                                         C-CONT.HOOK #hook ].')
        rule_added=True
    # Add either head-mod or head-comp if possessor = mod
    # Exception: no rule added if preexistent head-comps has correct order
    elif strat.get('mod-spec')=='mod':
        if strat.get('mark-loc')=='possessum-marking':
            phrase_rule="head-comp-poss-phrase"
            # Check if the existing head-comp rule has the correct order; 
            # if not, add a new rule with correct order that only applies to poss-phrases.
            head_comp_order=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']
            if head_comp_order=='head-comp':
                head_comp_order='head-initial'
            else:
                head_comp_order='head-final'
            if head_comp_order!=strat_order:
                mylang.add(phrase_rule +' := basic-head-comp-phrase.')
                rule_added=True
        else:
            phrase_rule="head-mod-poss-phrase"
            mylang.add(phrase_rule+' := basic-head-mod-phrase-simple & head-compositional & \
                                        [ SYNSEM.LOCAL.CAT.VAL [ SPEC #spec ], \
                                          HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC #spec].')
            rule_added=True
    # If a specialized poss phrase rule was added, require that the marked constituent be marked possessive.
    if strat.get('mark-loc')=='possessor-marking':
        mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive,\
                                     NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS possessive ].',merge=True)
    if strat.get('mark-loc')=='possessum-marking':
        mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS possessive,\
                                     NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive ].',merge=True)
    if strat.get('mark-loc')=='both-marking':
        mylang.add(phrase_rule+':= [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS possessive,\
                                     HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS possessive ].',merge=True)
    # Make non-possessive phrases reject possessive nouns:
    if ch.get('has-dets')=='yes':
        mylang.add('head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD  +vjrpcdmo ].',merge=True)
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
        print pc
        for lrt in pc.get('lrt'):
            modpos=lrt.get('modpos')
            if modpos=='before':
                head_adj=True
            elif modpos=='after':
                adj_head=True
            elif modpos=='either':
                head_adj=True
                adj_head=True
    if head_adj: mylang.add('head-adj-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD  +vjrpcdmo ].',merge=True)
    if adj_head: mylang.add('adj-head-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD  +vjrpcdmo ].',merge=True)
# TODO: limit head-comp phrase as well, when relevant.
#    if phrase_rule=='head-comp-poss-phrase' and rule_added:
#        mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSS nonpossessive ].')
    # If a specialized poss phrase rule was added, adds word order info to the phrase rule:
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
                    if mark_loc=='possessor-marking' or 'double-marking':
                        # Add the basic possessor rule defn:
                        possessor_rule_name = 'possessor-lex-rule-'+strat_num
                        mylang.add(possessor_rule_name+POSSESSOR_RULE)
                        if mod_spec=='spec':
                            mylang.add(possessor_rule_name+' := val-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ VAL [ SPEC.FIRST.LOCAL [ CAT [ HEAD noun ],\
                                                                                         CONT.HOOK [ INDEX #possessum & [ COG-ST uniq-id ],\
                                                                                                     LTOP #lbl ] ] ] ] ,\
                                             C-CONT [ HOOK #hook ,\
                                                      RELS <! '+ POSS_REL  +' , '+POSSESSUM_EXIST_REL+ ' !>, \
                                                                   HCONS <! qeq & [ HARG #harg, LARG #lbl ] !>, \
                                                                                                ICONS <! !>  ] ].',merge=True)
                        else: 
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
                    if mark_loc=='possessum-marking' or 'double-marking':
                        # Don't add poss_rel yet -- only if its single-marking
                        possessum_rule_name = 'possessum-lex-rule-'+strat_num
                        mylang.add(possessum_rule_name+POSSESSUM_RULE)

# THESE TWO RULES ARE UNTESTED:
                        if mod_spec=='spec':
                            mylang.add(possessum_rule_name+':=  val-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ VAL [ COMPS #comps,\
                                                                      SPR < [ LOCAL [ CAT [ HEAD noun ],\
                                                                                        CONT.HOOK #hook & [ INDEX #possessor,\
                                                                                                            LTOP #lbl ] ] ] > ] ] ,\
                                             C-CONT [ HOOK #hook & [ INDEX.COG-ST uniq-id ],\
                                                      RELS <! '+ POSS_REL  +' ,\
                                                              '+ POSSESSUM_EXIST_REL+ ' !>, \
                                                                   HCONS <! qeq & [ HARG #harg, LARG #lbl ] !>, \
                                                                                                ICONS <! !>  ],\
                                                              DTR.SYNSEM.LOCAL [ CAT.VAL.COMPS #comps ] ] ].',merge=True)
                       
                        if mod_spec=='mod':
                            mylang.add(possessum_rule_name+':= val-change-with-ccont-lex-rule & \
                                                       [ SYNSEM.LOCAL.CAT [ HEAD.CASE gen,\
                                                                            VAL [ SPR #spr, \
                                                                                  COMPS.FIRST.LOCAL [ CAT.HEAD noun, \
                                                                                                      CONT.HOOK [ INDEX #possessor,\
                                                                                                                  LTOP #lbl ] ] ] ],\
                                                         C-CONT [ HOOK #hook ,\
                                                                  RELS <! '+POSS_REL+',\
                                                                          '+POSSESSUM_EXIST_REL+' !>, \
                                                                  HCONS <! qeq & [ HARG #harg, LARG #lbl ] !>,\
                                                                  ICONS <! !>  ],\
                                                          DTR.SYNSEM.LOCAL [ CAT.VAL [ SPR #spr ],\
                                                                             CONT.HOOK  #hook & [ INDEX #possessum ] ] ].',merge=True)
 




#     Possessum-marker:
#        If the possessor is specifier-like:
#             If single-marking:
#                ADD possessum-lex-rule with SPR<[possessor]>, carrying poss_rel
#             If double-marking:
#                ADD possessum-lex-rule with SPR<[possessor]>
#        If the possessum is modifier-like:
#             If single-marking:
#                ADD possessum-lex-rule with COMPS<[possessor]>, carrying poss_rel
#             If double-marking
#                ADD possessum-lex-rule with COMPS<[possessor]>
#                ADD a feature [HEAD.POSS +] to the possessor infl rule.




# TODO: figure out what happens with double marking
# TODO: add in where the poss_rel is

# def customize_lex(mylang,ch,lexicon):
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






# Not sure making an object from the choices file is really that helpful, 
# but here's one way you could do it if you wanted to:
#
# A strategy object contains:
#
# Primitives from questionnaire:
# (Try not to add too many obscuring layers between choices file and this)
# juxtaposition: boolean (for convenience -- could be encoded in other variables)
# specifier vs. mod
# NOM: bool
# order: head-first or head-final or free
# mark-loc: head, dep, both
# possessor-affix: boolean
# possessor-sep-word: left-attach, right-attach, none
# possessum-affix: boolean
# possessum-sep-word: left-attach, right-attach, none
#
# Deduced properties:
# rule-type: head-spec-hc, head-comps, head-mod
#     DEPENDS ON: spec-like, mark-loc
# add-lex-rules: bool
#     DEPENDS ON: possessor-affix, possessum-affix
# add-lex-entries: bool
#     DEPENDS ON: possessor-sep-word, possessum-sep-word
# add-phrasal-rules: bool
#     DEPENDS ON: order, rule-type (anything but head-comp guarantees a rule add), 
#     [order info from word-order page]
#
# TDL-adding functions:
# Lexical rules.
#     DEPENDS ON: mark-loc, possessor-affix, possessum-affix
#     [Also unsure of how this interfaces with the stuff on Morphology page]
# Lexical entries.
#     DEPENDS ON: mark-loc, possessor-sep-word, possessum-sep-word
# Phrasal rules for attaching possessor and possessum:
#     DEPENDS ON: order, rule-type, [info from word-order lib]





# Choice 1:
# Order of possessor and possessum
#
# Consequence: 
# Depending on the preexistent order of head-spec, head-mod, and head-comps,
# May have to create a specific version of the above that has the appropriate order:
#     Whatever rule is used in this strategy will have to be added and with the right constraints.
# If preexistent order of head-spec, head-mod, and head-comps is already correct,
# Do nothing.


# Choice 2:
# Modifier vs. specifier
#
# Consequence:
# If specifier, always add head-spec-hc.
#     Also, MANY OTHER CHOICES FALL OUT FROM THIS
# If modifier, add either head-mod or leave in head-comp.
#     Also, MANY OTHER CHOICES FALL OUT FROM THIS

# Choice 3:
# Possessor = NOM vs NP
# 
# Consequence:
# If NOM, then require the possessor (in head-mod or head-spec or head-comps)
# to be SPR 1-dlist. 
# If NP, then require the possessor to be SPR olist.

# Choice 4:
# Complicated stuff
