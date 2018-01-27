import gmcs.tdl
import gmcs.utils
from gmcs.utils import get_name
from gmcs.choices import ChoiceDict
from gmcs.linglib.word_order import customize_major_constituent_order
from gmcs.linglib.morphotactics import all_position_classes
from gmcs.linglib.features import customize_feature_values
from gmcs.linglib.lexical_items import adp_id, noun_id
###############################################################################################
# Parts of a possessive strategy:
#
# The rule that combines possessor and possessum: customize_poss_rules()
#     Sometimes must be added separately
#     Sometimes already there
#
# For each affix: customize_poss_irules()
#     Lexical rule type for possessor or possessum inflecting rules
#
# For each non-affix: customize_poss_lexicon()
#     Lexical entries 
#     Rules that combine these lexical items with possessor/possessum (customize_poss_rules())
#
################################################################################################

################################################################################################
## Maximally general typedefs for use in library                                             ###
################################################################################################ 


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

JUXTAPOSITION_RULE=' := [ SYNSEM.LOCAL.CAT [ HEAD #head,\
                                              VAL [ COMPS < >,\
                                                    SUBJ < > ] ],\
                           C-CONT [ HOOK #hook & [ INDEX #possessum ],\
                                    ICONS <! !>],\
                           HEAD-DTR.SYNSEM.LOCAL [ CAT [ HEAD #head & noun ],\
                                                   CONT.HOOK #hook & [ INDEX #possessum,\
                                                               LTOP #lbl ] ],\
                           NON-HEAD-DTR.SYNSEM.LOCAL [ CAT [ HEAD noun,\
                                                             VAL.SPR < > ],\
                                                       CONT.HOOK.INDEX #possessor ] ].'

TWO_REL_ADP='two-rel-adposition-lex := basic-icons-lex-item &\
  [ SYNSEM [ LOCAL [ CAT [ HEAD adp,\
                           VAL.COMPS < [ LOCAL [ CAT cat-sat,\
                                                 CONT.HOOK #hook & [ INDEX #ind,\
                                                             ICONS-KEY.IARG1 #clause ] ] ] > ],\
                     CONT.HOOK #hook & [ CLAUSE-KEY #clause ] ],\
             LKEYS.KEYREL arg12-ev-relation & [ ARG2 #ind ] ] ].'

POSSESSOR_ADP_LEX=':= two-rel-adposition-lex &\
                                 [  SYNSEM.LOCAL [ CAT  [ VAL [ SPR < >,\
                                                                COMPS.FIRST [ LOCAL.CAT.HEAD noun,\
                                                                              OPT - ] ] ],\
                                                  CONT [ ICONS <! !>   ] ] ].'

POSSESSUM_NOUN_LEX=':= basic-two-arg &\
                                   [ SYNSEM.LOCAL [ CAT [ HEAD noun ,\
                                                          VAL [ SPR < #spr >,\
                                                                COMPS < #comps & [ LOCAL [ CONT.HOOK #hook,\
                                                                                  CAT.VAL.SPR <[ ]> ] ] > ] ],\
                                                    CONT [ HOOK #hook,\
                                                           ICONS <! !> ] ],\
                                     ARG-ST < #spr, #comps > ].'

POSSESSOR_PRON_LEX=' := basic-one-arg &\
                        [ SYNSEM [ LOCAL [ CAT [ VAL [ SPR olist,\
                                                       COMPS olist,\
                                                       SUBJ olist ],\
                                                 HEAD noun ] ],\
                                   LKEYS.ALTKEYREL #altkeyrel & noun-relation &\
                                                         [ PRED "pron_rel",\
                                                           LBL #lbl2,\
                                                           ARG0 #possessor & [ COG-ST activ-or-more,\
                                                                               SPECI + ] ] ] ].'


##################################################################
## Primary function (called from customize.py)                 ###
################################################################## 
def customize_adnominal_possession(mylang,ch,rules,irules,lexicon,hierarchies):
    # Check if any possessive strategies or pronouns are defined.
    # If so, add the POSS head feature.
    for key in ch.full_keys():
        if 'poss-strat' in key or 'poss-pron' in key:
            customize_poss_addenda(mylang)
    customize_np_possession(mylang,ch,rules,irules,lexicon,hierarchies)
    customize_pronominal_possession(mylang,ch,rules,irules,lexicon,hierarchies)

#####################################################################################################
## Secondary functions (called by customize_adnominal_possession() or other secondary functions)  ###
#####################################################################################################

# Adds things to the addenda section that are necessary
# for any strategy
def customize_poss_addenda(mylang):
    mylang.add('head :+ [ POSSESSOR poss ].',section='addenda')
    mylang.add('cat :+ [ POSSESSUM poss ].',section='addenda')
    mylang.add('poss := *top* & [ POSS-AGR png ].',section='addenda')
    mylang.add('possessive := poss.',section='addenda')
    mylang.add('nonpossessive := poss.',section='addenda')
    mylang.add('possessor := possessive.',section='addenda')
    mylang.add('possessum := possessive.',section='addenda')
    mylang.add('basic-bare-np-phrase :+\
            [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive, \
                                          POSSESSUM nonpossessive ] ].',section='addenda')

# Calls customize_poss_rules, customize_poss_irules, and customize_poss_lexicon
# to build possessive strategies for cases where the possessor
# is a full NP
def customize_np_possession(mylang,ch,rules,irules,lexicon,hierarchies):
    for strat in ch.get('poss-strat',[]):
        strat_num=strat.full_keys()[0].split("_")[0][-1]
        mylang.add('possessive-'+str(strat_num)+' := possessive.',section='addenda')
        mylang.add('possessor-'+str(strat_num)+\
                    ' := possessor & possessive-'+str(strat_num)+'.',section='addenda')
        mylang.add('possessum-'+str(strat_num)+\
                    ' := possessum & possessive-'+str(strat_num)+'.',section='addenda')
        customize_poss_rules(strat,mylang,ch,rules)
        if strat.get('possessor-type')=='affix' or strat.get('possessum-type')=='affix':
            customize_poss_irules(strat,mylang,ch,irules,hierarchies)
        if strat.get('possessor-type')=='non-affix' or strat.get('possessum-type')=='non-affix':
            customize_poss_lexicon(strat,mylang,ch,lexicon,rules,hierarchies)
        # This part sets nouns to default nonpossessive behavior in non-affixal
        # grammars.
        if 'affix' not in (strat.get('possessor-type'), strat.get('possessum-type')):
            mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                         POSSESSUM nonpossessive ] ].')


# Calls customize_poss_rules, customize_poss_irules, and customize_poss_lexicon
# to build possessive strategies for cases where the possessor
# is a pronoun
def customize_pronominal_possession(mylang,ch,rules,irules,lexicon,hierarchies):
    for pron in ch.get('poss-pron',[]):
        pron_num=pron.full_keys()[0].split("_")[0][-1]
        mylang.add('possessive-pron-'+str(pron_num)+' := possessive.',section='addenda')
        mylang.add('possessor-pron-'+str(pron_num)+' :=\
                      possessor & '+'possessive-pron-'+str(pron_num)+'.',section='addenda')
        customize_poss_rules(pron,mylang,ch,rules)
        if pron.get('type')=='affix':
            customize_poss_irules(pron,mylang,ch,irules,hierarchies)
        if pron.get('type')=='non-affix':            
            customize_poss_lexicon(pron,mylang,ch,lexicon,rules,hierarchies)


# Adds phrase rules needed to join possessor and possessum,
# as well as rules needed to join possession marker and the 
# constituent it marks.
# Also adds constraints to non-possessive phrase rules to prevent
# them from allowing possessive words in incorrect places
def customize_poss_rules(strat,mylang,ch,rules):
    """
    Adds the necessary phrase rule to combine possessor and possessum
    If rule already exists (head-comp case), then make sure its order is correct.
    """
    # Define vars for all elements of strategy:
    strat_name=strat.full_keys()[0].split("_")[0]
    strat_num=strat_name[-1]
    strat_order=strat.get('order')
    mark_loc=strat.get('mark-loc')
    pron_strat=False
    pron_affix=False
    if 'poss-pron' in strat_name:
        pron_strat=True
        strat_num='pron-'+strat_num
        if strat.get('type')=='affix':
            pron_affix=True
    # Define var to keep track of major constituent word order
    head_comp_order=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']
    if head_comp_order=='head-comp':
        head_comp_order='head-initial'
    else:
        head_comp_order='head-final'
    # Add vars to keep track of what rules have been added:
    phrase_rule=""
    rule_added=False
    mylang.set_section('phrases')
    # Start adding rules:
    # If no marking exists, add one of two juxtaposition rules:
    if mark_loc=='neither' and not pron_strat:
        phrase_rule='poss-phrase'+'-'+strat_num
        mylang.add(phrase_rule+JUXTAPOSITION_RULE)
        if strat.get('mod-spec')=='spec':
            mylang.add(phrase_rule+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < >,\
                                          HEAD-DTR.SYNSEM.LOCAL [ CAT.VAL.SPR <[ ]>,\
                                                                  CONT.HOOK.INDEX.COG-ST uniq-id  ],\
                                          C-CONT [ RELS <! '+POSS_REL+',\
                                                           '+POSSESSUM_EXIST_REL+' !>,\
                                                   HCONS <! qeq & [ HARG #harg, LARG #lbl] !> ] ].')
        elif strat.get('mod-spec')=='mod':
            mylang.add(phrase_rule+' := [ SYNSEM.LOCAL.CAT.VAL.SPR #spr,\
                                          HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR #spr & <[ ]>,\
                                          C-CONT [ RELS <! '+POSS_REL+' !>,\
                                                   HCONS <! !> ] ].')
        rule_added=True
    # If possessor isn't an affix, add a phrase rule
    elif not pron_affix:
        # If possessor==spec, add a head-compositional variant of head-spec 
        if strat.get('mod-spec')=='spec':
            phrase_rule='head-spec-poss-phrase'+'-'+strat_num
            mylang.add(phrase_rule+' :=  basic-head-spec-phrase-super & [  NON-HEAD-DTR.SYNSEM [ LOCAL.CAT [ VAL [ SPR < >,\
                                                                                                                   COMPS < > ],\
                                                                                                               HEAD +nvjrpcmo ],\
                                                                                                   OPT - ],\
                                                                           HEAD-DTR.SYNSEM.LOCAL.CONT.HOOK #hook ,\
                                                                           C-CONT.HOOK #hook ].')
            rule_added=True
        # If possessor==mod, add either head-mod or head-comp
        # Exception: no rule added if preexistent head-comps has correct order
        elif strat.get('mod-spec')=='mod':
            if strat.get('mark-loc')=='possessum' or strat.get('mark-loc')=='both':# and not pron_strat:
                phrase_rule="head-comp-poss-phrase"+'-'+strat_num
                # Check if the existing head-comp rule has the correct order; 
                # if not, add a new rule with correct order that only applies to poss-phrases.
                if head_comp_order!=strat_order:
                    mylang.add(phrase_rule +' := basic-head-1st-comp-phrase &\
                                           [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +np,\
                                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.SPR <>,\
                                                                             HEAD +np ] ].')
                    rule_added=True
            else:
                phrase_rule="head-mod-poss-phrase"+'-'+strat_num
                mylang.add(phrase_rule+' := basic-head-mod-phrase-simple & head-compositional & \
                                        [ SYNSEM.LOCAL.CAT [ HEAD +np,\
                                                             VAL [ SPEC #spec ] ], \
                                          HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +np,\
                                                                      VAL.SPEC #spec ] ] ].')

                rule_added=True

    # If a specialized (non-juxtaposition) poss phrase rule was added, require that the marked constituent be [ POSS possessive ]
    if rule_added: # and not phrase_rule=='head-comp-poss-phrase'+'-'+strat_num:
        if not (phrase_rule=='head-comp-poss-phrase-'+strat_num):# and (possessor_mark_order==order or possessum_mark_order==order)):
            if strat.get('mark-loc')=='possessor' or pron_strat:
                mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM nonpossessive,\
                                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ].',merge=True)
            if strat.get('mark-loc')=='possessum':
                mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM possessum-'+strat_num+',\
                                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR nonpossessive ].',merge=True)
            if strat.get('mark-loc')=='both':
                mylang.add(phrase_rule+':= [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+',\
                                             HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM possessum-'+strat_num+' ].',merge=True)
        # Make head-comp-poss accept only possessive phrases. 
        elif phrase_rule=='head-comp-poss-phrase-'+strat_num:
            if (mark_loc=='possessum' or mark_loc=='both') and strat.get('possessum-type')=='affix':
                mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT [ POSSESSUM possessum ] ].')
        # Make non-possessive phrases reject possessive nouns:
        if ch.get('has-dets')=='yes':
            mylang.add('head-spec-phrase :=\
                              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.SPR <>,\
                                                                HEAD +vjrcdmo &\
                                                                  [ POSSESSOR nonpossessive ] ] ].',merge=True)

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
        if head_adj: mylang.add('head-adj-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR nonpossessive ].',merge=True)
        if adj_head: mylang.add('adj-head-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR nonpossessive ].',merge=True)
        if phrase_rule=='head-comp-poss-phrase'+'-'+strat_num: # and rule_added and strat.get('possessum-type')!='non-affix':
            head_comp_order=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']
            mylang.add(head_comp_order+'-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR nonpossessive,\
                                                         HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL [ SPR <>,\
                                                                                           SPEC <> ] ] ].')
    if rule_added:
        # If a specialized poss phrase rule was added:
        #     Adds rule to rules.tdl
        #     Add word order info to rule
        if strat_order=='either':
            mylang.add(phrase_rule+'-head-initial := head-initial & '+phrase_rule+'.')
            mylang.add(phrase_rule+'-head-final := head-final & '+phrase_rule+'.')
            rules.add(phrase_rule.replace('-phrase','')+'-head-initial := '+phrase_rule+'-head-initial.'  )
            rules.add(phrase_rule.replace('-phrase','')+'-head-final := '+phrase_rule+'-head-final. ' )
        else:
            mylang.add(phrase_rule +' := '+strat.get('order')+'.',merge=True)
            rules.add(phrase_rule.replace('-phrase','') + ':= '+phrase_rule+'. ' )

# Adds inflectional rules (or adds constraints to inflectional rules added in
# morphotactics.py) that create possessive forms
def customize_poss_irules(strat,mylang,ch,irules,hierarchies):
    strat_name=strat.full_keys()[0].split("_")[0]
    strat_num=strat_name[-1]
    if 'poss-pron' in strat_name:
        pron_strat=True
    else:
        pron_strat=False
    mark_loc=strat.get('mark-loc')
    mod_spec=strat.get('mod-spec')
    possessor_type=strat.get('possessor-type')
    possessum_type=strat.get('possessum-type')
    for pc in all_position_classes(ch):
        for lrt in pc.get('lrt',[]):
            for feat in lrt['feat']:
                if not pron_strat:
                    # Go through the position class (pc) info till you find the strategy you're actually dealing with
                    if strat_name==str(feat['name']) and feat['value']!='nonpossessive':
                        # Add possessor-inflecting rules:
                        mylang.set_section('lexrules')
                        if (mark_loc=='possessor' or mark_loc=='both') and possessor_type=='affix' and feat['value']=='possessor':
                            # Add the basic possessor rule defn:
                            possessor_rule_name ='possessor-lex-rule-'+strat_num
                            if mod_spec=='spec':
                                agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                                mylang.add(possessor_rule_name+POSSESSOR_RULE)
                                if mark_loc=='possessor':
                                    mylang.add(possessor_rule_name+' := val-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                                VAL [ SPEC.FIRST.LOCAL [ CAT [ HEAD noun ],\
                                                                                         CONT.HOOK [ INDEX #possessum & [ COG-ST uniq-id ],\
                                                                                                     LTOP #lbl ] ] ] ] ,\
                                             C-CONT [ HOOK #hook ,\
                                                      RELS <! '+ POSS_REL  +' , '+POSSESSUM_EXIST_REL+ ' !>, \
                                                                   HCONS <! qeq & [ HARG #harg, LARG #lbl ] !>, \
                                                                                                ICONS <! !>  ] ].',merge=True)

                                if mark_loc=='both':
                                    mylang.add(possessor_rule_name+' := val-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                                VAL [ SPEC.FIRST.LOCAL [ CAT [ HEAD noun ] ] ] ] ,\
                                             C-CONT [ HOOK #hook & [ INDEX #possessor ],\
                                                      RELS <!  !>, \
                                                      HCONS <! !>, \
                                                      ICONS <! !>  ] ].',merge=True)
                            elif mod_spec=='mod':
                                if mark_loc=='possessor':
                                    agr_prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                                    mylang.add(possessor_rule_name+POSSESSOR_RULE)
                                    mylang.add(possessor_rule_name+' := head-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ HEAD [ MOD.FIRST [ LOCAL [ CAT [ VAL.SPR <[]>,\
                                                                                               HEAD +np ], \
                                                                                         CONT.HOOK [ INDEX #possessum, \
                                                                                                     LTOP #lbl ] ], \
                                                                                 OPT - ],\
                                                                       POSSESSOR possessor-'+strat_num+' ], \
                                                                VAL [ SPEC #spec ] ] ,\
                                             C-CONT [ HOOK #hook ,\
                                                RELS <! '+POSS_REL+' !>,\
                                                HCONS <! !>, \
                                                ICONS <! !>  ], \
                                             DTR.SYNSEM.LOCAL.CAT.VAL.SPEC #spec  ].',merge=True)
                                elif mark_loc=='both':
                                    mylang.add(possessor_rule_name+' := add-only-no-ccont-rule &\
                                                                    [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                                                           VAL #val ] ] ,\
                                                                      DTR.SYNSEM.LOCAL [ CAT.VAL #val ] ].')
                        # If an agreement strategy is indicated, identify POSS-AGR with PNG of possessum
                        if strat.get('possessor-affix-agr')=='agree':
                            # Note: we don't do this in the mod-like both-marking scenario -- possessor is a COMP and has no access to 
                            # possessum. In this scenario, the identification must be done on the possessum inflection.
                            if not (mark_loc=='both' and mod_spec=='mod'):
                                mylang.add(possessor_rule_name+' := [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                                                               '+agr_prefix+' #png ].')
                        # Add possessum-inflecting rules
                        if (mark_loc=='possessum' or mark_loc=='both') and possessum_type=='affix' and feat['value']=='possessum':
                            possessum_rule_name ='possessum-lex-rule-'+strat_num
                            mylang.add(possessum_rule_name+POSSESSUM_RULE)
                            if mod_spec=='spec':
                                agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                                mylang.add(possessum_rule_name+':=  val-change-with-ccont-lex-rule & \
                                           [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                                                VAL [ COMPS #comps,\
                                                                      SPR < [ LOCAL [ CAT [ HEAD +np ] ] ] > ] ] ,\
                                             C-CONT [ HOOK #hook & [ INDEX.COG-ST uniq-id ],\
                                                      HCONS <! !>, \
                                                      ICONS <! !>  ],\
                                             DTR.SYNSEM.LOCAL [ CAT.VAL.COMPS #comps ] ].',merge=True)
                                mylang.add(possessum_rule_name+' := [ SYNSEM.LOCAL.CAT.VAL.SPR <[ LOCAL.CONT.HOOK [ INDEX #possessor ] ]>,\
                                                                      C-CONT [ HCONS <! qeq & [ HARG #harg, LARG #lbl ] !> ,\
                                                                               RELS <! '+POSS_REL+',\
                                                                                       '+POSSESSUM_EXIST_REL+' !> ],\
                                                                                       DTR.SYNSEM.LOCAL.CONT.HOOK [ INDEX #possessum,\
                                                                                                                    LTOP #lbl ] ].')
                                if mark_loc=='both':
                                    mylang.add(possessum_rule_name+' :=\
                                       [ SYNSEM.LOCAL.CAT.VAL.SPR < [ LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ] > ].')
                            if mod_spec=='mod':
                                # Added OPT - to the complement to try to correct overzealous head-comp application. Didn't work.
                                agr_prefix='SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                                mylang.add(possessum_rule_name+':= val-change-with-ccont-lex-rule & \
                                                       [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                                                            VAL [ SPR #spr, \
                                                                                  COMPS.FIRST [ LOCAL [ CAT cat-sat & [ HEAD +np ], \
                                                                                                        CONT.HOOK [ INDEX #possessor ] ],\
                                                                                                OPT - ] ] ],\
                                                         C-CONT [ HOOK #hook ,\
                                                                  RELS <! '+POSS_REL+' !>,\
                                                                  HCONS <! !>,\
                                                                  ICONS <! !>  ],\
                                                         DTR.SYNSEM.LOCAL [ CAT.VAL.SPR #spr,\
                                                                            CONT.HOOK [ INDEX #possessum,\
                                                                                        LTOP #lbl ] ] ].',merge=True)
                                if mark_loc=='both':
                                    mylang.add(possessum_rule_name+' :=\
                                           [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD +np & [ POSSESSOR possessor-'+strat_num+' ] ].')
                            if strat.get('possessum-affix-agr')=='agree':
                                mylang.add(possessum_rule_name+' := [ SYNSEM.LOCAL.CAT.POSSESSUM.POSS-AGR #png,\
                                                             '+agr_prefix+' #png ].')
                            # Note: in the mutual agreement, double marking mod-like scenario, the possessor is a COMP.
                            # Therefore, it has no access to the possessum's PNG info. When the possessor agrees with 
                            # the possessum, therefore, all agreement must be done in the possessum-inflecting rule:
                            if mark_loc=='both' and mod_spec=='mod' and strat.get('possessor-affix-agr')=='agree':
                                if possessum_type=='affix':
                                    mylang.add(possessum_rule_name+' :=\
                                         [ SYNSEM.LOCAL [ CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #poss-png,\
                                                          CONT.HOOK.INDEX.PNG #poss-png ] ].')
                    elif strat_name==str(feat['name']) and feat['value']=='nonpossessive':
                        if (mark_loc=='possessum' or mark_loc=='both') and mod_spec=='spec':
                            # This should keep normal head-spec constructions from letting noun phrases act as determiners:
                            mylang.add(get_name(lrt)+'-lex-rule := [ SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CAT.HEAD det ].')
                elif pron_strat:
                    if strat_name in str(feat['name']) and feat['value']!='minus':
                        mylang.add(get_name(lrt)+'-lex-rule :=\
                                      [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR possessive-pron-'+strat_num+',\
                                        DTR.SYNSEM.LOCAL.CONT.HOOK #hook & [ INDEX #possessum & [ COG-ST uniq+fam+act ],\
                                                                             LTOP #lbl],\
                                        C-CONT.HOOK #hook ].')
                        if mod_spec=='spec':
                            mylang.add(get_name(lrt)+'-lex-rule := \
                                                                   [ SYNSEM.LOCAL.CAT.VAL [ SPR < >,\
                                                                                            SPEC #spec,\
                                                                                            SUBJ #subj,\
                                                                                            COMPS #comps ],\
                                                                     C-CONT [ RELS  <! noun-relation &\
                                                                                     [ PRED "pron_rel",\
                                                                                       LBL #lbl2,\
                                                                                       ARG0 #possessor & [ COG-ST activ-or-more,\
                                                                                                           SPECI + ] ],\
                                                                                     '+POSSESSUM_EXIST_REL+',\
                                                                                     '+POSS_REL+',\
                                                                                     quant-relation &\
                                                                                     [ PRED "exist_q_rel",\
                                                                                       ARG0 #possessor,\
                                                                                       RSTR #harg2 ] !>,\
                                                                             HCONS <! qeq & [ HARG #harg,\
                                                                                             LARG #lbl ],\
                                                                                     qeq & [ HARG #harg2,\
                                                                                             LARG #lbl2 ] !> ],\
                                                                     DTR.SYNSEM.LOCAL.CAT.VAL [ SPEC #spec,\
                                                                                                SUBJ #subj,\
                                                                                                COMPS #comps] ].')
                        elif mod_spec=='mod':
                            mylang.add(get_name(lrt)+'-lex-rule := \
                                                                   [ SYNSEM.LOCAL.CAT.VAL #val,\
                                                                     DTR.SYNSEM.LOCAL.CAT.VAL #val,\
                                                                     C-CONT [ RELS  <! noun-relation &\
                                                                                       [ PRED "pron_rel",\
                                                                                         LBL #lbl2,\
                                                                                         ARG0 #possessor & [ COG-ST activ-or-more,\
                                                                                                             SPECI + ] ],\
                                                                                       '+POSS_REL+',\
                                                                                       quant-relation &\
                                                                                       [ PRED "exist_q_rel",\
                                                                                         ARG0 #possessor,\
                                                                                         RSTR #harg2 ] !>,\
                                                                            HCONS <! qeq & [ HARG #harg2,\
                                                                                             LARG #lbl2 ] !> ] ].')



# Adds lexical items for possession markers and possessor pronouns.
# All needed phrase rules added in customize_poss_rules() above.
def customize_poss_lexicon(strat,mylang,ch,lexicon,rules,hierarchies):
    # Define vars for all elements of strategy:
    strat_name=strat.full_keys()[0].split("_")[0]
    strat_num=strat_name[-1]
    mark_loc=strat.get('mark-loc')
    mod_spec=strat.get('mod-spec')    
    possessor_type=strat.get('possessor-type')
    possessum_type=strat.get('possessum-type')
    if 'poss-pron' in strat_name:
        pron_strat=True
    else:
        pron_strat=False
    if not pron_strat:
        if (mark_loc=='possessor' or mark_loc=='both') and possessor_type=='non-affix':
            mylang.set_section('otherlex')
            mylang.add(TWO_REL_ADP)
            mylang.add('possessor-adp-lex-'+strat_num+' '+POSSESSOR_ADP_LEX)
            if mod_spec=='spec':
                mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [  SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                       POSSESSUM nonpossessive,\
                                                       VAL [ SPEC.FIRST.LOCAL.CAT.VAL.SPR < [ ] > ] ] ].')
                if mark_loc=='possessor':
                    mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [  SYNSEM.LOCAL [ CAT  [ VAL [ COMPS.FIRST.LOCAL [ CONT.HOOK.INDEX #possessor ],\
                                                                SPEC.FIRST.LOCAL [ CONT.HOOK [ INDEX #possessum,\
                                                                                               LTOP #lbl ] ] ] ],\
                                                  CONT [ RELS <! '+POSS_REL+',\
                                                                 '+POSSESSUM_EXIST_REL+' !>,\
                                                         HCONS <!  qeq & [ HARG #harg, LARG #lbl ] !> ] ] ].')
                elif mark_loc=='both':
                    mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [  SYNSEM.LOCAL [ CONT [ RELS <! !>,\
                                                          HCONS <! !> ] ] ].')
            if mod_spec=='mod':
                mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT.HEAD [ POSSESSOR possessor-'+strat_num+',\
                                                             MOD.FIRST.LOCAL.CAT.VAL.SPR < [ ] > ],\
                                                  CONT [ HCONS <! !> ] ] ] .')
                if mark_loc=='possessor':
                    mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT [ VAL [ COMPS.FIRST.LOCAL [ CONT.HOOK.INDEX #possessor ] ],\
                                                         HEAD.MOD.FIRST.LOCAL [ CONT.HOOK [ INDEX #possessum,\
                                                                                            LTOP #lbl ] ] ],\
                                                  CONT [ RELS <! '+POSS_REL+' !> ] ] ].')
                elif mark_loc=='both':
                    mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                        POSSESSUM nonpossessive, ]\
                                                  CONT.RELS <! !> ] ] .')
            # TODO: these lex items don't follow nomenclature conventions yet:
            if strat.get('possessor-agr')=='non-agree':
                orth=strat.get('possessor-orth')
                lexicon.add('possessor-adp-'+strat_num+' := possessor-adp-lex-'+strat_num+' &\
                                                      [ STEM < "'+orth+'" >].')
            elif strat.get('possessor-agr')=='agree':
                for form in strat.get('possessor-form'):
                    if mod_spec=='spec':
                        prefix='SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                    elif mod_spec=='mod':
                        if mark_loc=='possessor':
                            prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                        elif mark_loc=='both':
                            prefix='SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                    orth=form.get('agr-orth')
                    adp_type=adp_id(form)
                    mylang.add(adp_type+' := possessor-adp-lex-'+strat_num+' &\
                                        [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                        '+prefix+' #png ].')
                    customize_feature_values(mylang,ch,hierarchies,form,adp_type,'poss-marker')
                    orth=form.get('agr-orth')
                    lexicon.add(adp_type.replace('-lex','')+' := '+adp_type+' &\
                                         [ STEM < "'+orth+'" > ].')


        if (mark_loc=='possessum' or mark_loc=='both') and possessum_type=='non-affix':
            mylang.set_section('nounlex')
            if mod_spec=='spec':
                mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX)
                mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL [ CAT [ POSSESSUM possessum-'+strat_num+',\
                                                                         VAL [ SPR < #spr & [ LOCAL [ CONT.HOOK.INDEX #possessor ] ] >,\
                                                                              COMPS < #comps & [ LOCAL [ CONT.HOOK [ INDEX #possessum,\
                                                                                                                     LTOP #lbl ] ] ] > ] ],\
                                                                CONT [ RELS <! '+POSS_REL+',\
                                                                               '+POSSESSUM_EXIST_REL+' !>,\
                                                                               HCONS <! qeq & [HARG #harg, LARG #lbl] !> ] ] ].',merge=True)
                if mark_loc=='possessum':
                        mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ LOCAL.CAT.HEAD noun ] > ].')                    
                if mark_loc=='both':
                    if possessor_type=='affix':
                        mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ LOCAL.CAT.HEAD noun ] > ].')
                    else:
                        mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ LOCAL.CAT.HEAD adp ] > ].')
            if strat.get('possessum-agr')=='non-agree':
                orth=strat.get('possessum-orth')
                lexicon.add('possessum-noun-'+strat_num+' := possessum-noun-lex-'+strat_num+' &\
                                                  [ STEM < "'+orth+'" >].')
            elif strat.get('possessum-agr')=='agree':
                for form in strat.get('possessum-form'):
                    if mod_spec=='spec':
                        prefix='SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                    elif mod_spec=='mod':
                        prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                    noun_type=noun_id(form)
                    mylang.add(noun_type+' := possessum-noun-lex-'+strat_num+' &\
                                        [ SYNSEM.LOCAL.CAT.POSSESSUM.POSS-AGR #png,\
                                        '+prefix+' #png ].')
                    customize_feature_values(mylang,ch,hierarchies,form,noun_type,'poss-marker')
                    orth=form.get('agr-orth')
                    lexicon.add(noun_type.replace('-lex','')+' := '+noun_type+' &\
                                         [ STEM < "'+orth+'" > ].')
    elif pron_strat:
        noun_type=noun_id(strat)
        if strat.get('agr')=='agree':
            agr=True
        else:
            agr=False
        mylang.set_section('nounlex')
        mylang.add(noun_type+POSSESSOR_PRON_LEX)
        if mod_spec=='spec':
            agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            mylang.add(noun_type+' :=\
                        [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-pron-'+strat_num+',\
                                               VAL.SPEC.FIRST.LOCAL.CONT.HOOK [ INDEX #possessum & [ COG-ST uniq+fam+act ],\
                                                                          LTOP #lbl ] ],\
                                         CONT [ RELS  <! '+POSSESSUM_EXIST_REL+',\
                                                         '+POSS_REL+',\
                                                           quant-relation &\
                                                           [ PRED "exist_q_rel",\
                                                             ARG0 #possessor,\
                                                             RSTR #harg2 ],\
                                                           #altkeyrel !>,\
                                                  HCONS <! qeq & [ HARG #harg,\
                                                                   LARG #lbl ],\
                                                           qeq & [ HARG #harg2,\
                                                                   LARG #lbl2 ] !> ] ] ].')
        elif mod_spec=='mod':
            agr_prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            mylang.add(noun_type+' :=\
                        [ SYNSEM.LOCAL [ CAT.HEAD [ POSSESSOR possessor-pron-'+strat_num+',\
                                                    MOD.FIRST.LOCAL [ CAT.VAL.SPR <[]>,\
                                                                      CONT.HOOK [ INDEX #possessum,\
                                                                                  LTOP #lbl ] ] ],\
                                         CONT [ RELS  <!  '+POSS_REL+',\
                                                           quant-relation &\
                                                           [ PRED "exist_q_rel",\
                                                             ARG0 #possessor,\
                                                             RSTR #harg2 ],\
                                                           #altkeyrel !>,\
                                                  HCONS <! qeq & [ HARG #harg2,\
                                                                   LARG #lbl2 ] !> ] ] ].')
        if agr: 
            mylang.add(noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                              '+agr_prefix+' #png ].')
        for instance in strat.get('instance'):
            orth=instance.get('orth')
            instance_name=noun_id(instance)
            mylang.add(instance_name+' := '+noun_type+'.')
            customize_feature_values(mylang,ch,hierarchies,instance,instance_name,'noun')
            lexicon.add(instance_name.replace('-lex','')+' := '+instance_name+' &\
                                                          [ STEM < "'+orth+'" > ].')

            instance_tmp={}                
            for key in instance.keys():
                # Relabel the inherent features as something else ('skip') 
                # Relabel the agreement features as simply features ('feat')
                # Then call customize_feature_values() with the 'poss-marker' setting
                # so that the agreement features are added at POSS.POSS-AGR instead of HOOK.INDEX.PNG
                new_key=key.replace('feat','skip')
                new_key=new_key.replace('agr-skip','feat')
                instance_tmp[new_key]=instance.get(key)
            # TODO: Figure out how to cast instance_tmp from a dict to a ChoiceDict so that no future
            #  developers have to deal with this mess in features.py
            customize_feature_values(mylang,ch,hierarchies,instance_tmp,instance_name,'poss-marker')


