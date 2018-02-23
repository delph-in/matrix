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

POSSESSOR_NON_SPEC_RULE=' :=\
                  [ SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,\
                                             COMPS #comps,\
                                             SUBJ #subj ] ],\
                    DTR.SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,\
                                                 COMPS #comps,\
                                                 SUBJ #subj ], \
                                       CONT.HOOK #hook & [ INDEX #possessor ] ] ] ].'

POSSESSOR_SPEC_RULE=' :=\
                  [ SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,\
                                             COMPS #comps,\
                                             SUBJ #subj ] ],\
                    DTR.SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,\
                                                 COMPS #comps,\
                                                 SUBJ #subj  ] ] ].'


POSSESSUM_NON_SPEC_RULE=' :=\
                  [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR #poss & nonpossessive,\
                                         VAL [ SPEC #spec,\
                                             SUBJ #subj ] ] ],\
                    DTR.SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR #poss,\
                                             VAL [ SPEC #spec,\
                                                 SUBJ #subj ] ], \
                                       CONT.HOOK #hook ] ] ].'

POSSESSUM_SPEC_RULE=' :=\
                  [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR #poss & nonpossessive,\
                                         VAL [ SPEC #spec,\
                                             SUBJ #subj ] ] ],\
                    DTR.SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR #poss,\
                                             VAL [ SPEC #spec,\
                                                 SUBJ #subj ] ] ] ] ].'


JUXTAPOSITION_RULE=' := [ SYNSEM.LOCAL.CAT [ HEAD #head,\
                                              VAL [ COMPS < >,\
                                                    SUBJ < > ] ],\
                           C-CONT [ HOOK #hook & [ INDEX #possessum ],\
                                    ICONS <! !>],\
                           HEAD-DTR.SYNSEM.LOCAL [ CAT [ HEAD #head & noun & [ PRON - ] ],\
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
                                 [  SYNSEM.LOCAL [ CAT  [ VAL [ SPEC < >,\
                                                                SUBJ < >,\
                                                                SPR < >,\
                                                                COMPS.FIRST [ LOCAL.CAT [ HEAD noun,\
                                                                                          VAL.SPR < > ],\
                                                                              OPT - ] ] ],\
                                                  CONT [ ICONS <! !>   ] ] ].'

#POSSESSUM_NOUN_LEX=':= basic-two-arg &\
#                                   [ SYNSEM.LOCAL [ CAT [ HEAD #head & noun ,\
#                                                          VAL [ SUBJ < >,\
#                                                                SPR < #spr & [ LOCAL [ CAT.VAL.SPR < >,\
#                                                                                       CONT.HOOK.INDEX #possessor ] ] >,\
#                                                                COMPS < #comps & [ LOCAL [ CONT.HOOK #hook & [ INDEX #possessum,\
#                                                                                                               LTOP #lbl ],\
#                                                                                           CAT [ VAL.SPR <[ ]>,\
#                                                                                                 HEAD #head & [ PRON - ] ] ] ] > ] ],\
#                                                    CONT [ RELS <! '+POSS_REL+',\
#                                                                               '+POSSESSUM_EXIST_REL+' !>,\
#                                                           HCONS <! qeq & [ HARG #harg, LARG #lbl] !>,\
#                                                           HOOK #hook,\
#                                                           ICONS <! !> ] ],\
#                                     ARG-ST < #spr, #comps > ].'

POSSESSUM_NOUN_LEX=':= basic-one-arg &\
                                   [ SYNSEM.LOCAL [ CAT [ HEAD #head & noun ,\
                                                          VAL [ SUBJ < >,\
                                                                SPR < [ ] >,\
                                                                COMPS < #comps & [ LOCAL [ CONT.HOOK #hook,\
                                                                                           CAT [ VAL.SPR <[ ]>,\
                                                                                                 HEAD #head & [ PRON - ] ] ] ] > ] ],\
                                                    CONT [ RELS <! !>,\
                                                           HCONS <! !>,\
                                                           HOOK #hook,\
                                                           ICONS <! !> ] ],\
                                     ARG-ST < #comps > ].'



# TODO: change one-arg to being added by the logic section, since the modifier-version is zero-arg.
POSSESSOR_PRON_LEX=' := basic-one-arg &\
                        [ SYNSEM [ LOCAL [ CONT.HOOK [ INDEX #possessor,\
                                                       LTOP #ltop ],\
                                           CAT [ VAL [ SPR < [ LOCAL.CAT.HEAD det,\
                                                               OPT + ] >,\
                                                       COMPS olist,\
                                                       SUBJ olist ],\
                                                 HEAD noun & [ PRON + ] ] ],\
                                   LKEYS.ALTKEYREL #altkeyrel & noun-relation &\
                                                         [ PRED "pron_rel",\
                                                           LBL #ltop,\
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
            customize_poss_addenda(mylang,ch)

    customize_np_possession(mylang,ch,rules,irules,lexicon,hierarchies)

    customize_pronominal_possession(mylang,ch,rules,irules,lexicon,hierarchies)

#####################################################################################################
## Secondary functions (called by customize_adnominal_possession() or other secondary functions)  ###
#####################################################################################################

"""
Adds things to the addenda section that are necessary
for any strategy
"""
def customize_poss_addenda(mylang,ch):
    mylang.add('head :+ [ POSSESSOR poss ].',section='addenda')
    mylang.add('cat :+ [ POSSESSUM poss ].',section='addenda')
    mylang.add('poss := *top* & [ POSS-AGR png ].',section='addenda')
    mylang.add('possessive := poss.',section='addenda')
    mylang.add('nonpossessive := poss.',section='addenda')
    mylang.add('possessor := possessive.',section='addenda')
    mylang.add('possessum := possessive.',section='addenda')
    mylang.add('basic-bare-np-phrase :+ [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR #possessor,\
                                                             POSSESSUM #possessum],\
                                          HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR #possessor,\
                                                             POSSESSUM #possessum] ].',section='addenda')
    # Set nouns to default nonpossessive behavior if no affixal strategies exist:
    poss_strat_types=set()
    for strat in ch.get('poss-strat'):
        poss_strat_types.add(strat.get('possessor-type'))
        poss_strat_types.add(strat.get('possessum-type'))
        poss_strat_types.add(strat.get('possessum-mark-type'))
    if 'affix' not in poss_strat_types:
        mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                     POSSESSUM nonpossessive ] ].',section='nounlex')



"""
Calls customize_poss_rules, customize_poss_irules, and customize_poss_lexicon
to build possessive strategies for cases where the possessor is a full NP
"""
def customize_np_possession(mylang,ch,rules,irules,lexicon,hierarchies):
    for strat in ch.get('poss-strat',[]):

        strat_num=strat.full_keys()[0].split("_")[0][-1]

        # Add subtypes of POSSESSOR and POSSESSUM features for this strategy
        mylang.add('possessive-'+str(strat_num)+' := possessive.',section='addenda')
        mylang.add('possessor-'+str(strat_num)+\
                    ' := possessor & possessive-'+str(strat_num)+'.',section='addenda')
        mylang.add('possessum-'+str(strat_num)+\
                    ' := possessum & possessive-'+str(strat_num)+'.',section='addenda')

        # Add phrase rules:
        customize_poss_rules(strat,mylang,ch,rules,hierarchies)

        # Add inflectional rules:
        if strat.get('possessor-type')=='affix' or strat.get('possessum-type')=='affix':
            customize_poss_irules(strat,mylang,ch,irules,hierarchies)

        # Add lexical items:
        if strat.get('possessor-type')=='non-affix' or strat.get('possessum-type')=='non-affix':
            customize_poss_lexicon(strat,mylang,ch,lexicon,rules,hierarchies)


"""
Calls customize_poss_rules, customize_poss_irules, and customize_poss_lexicon
to build possessive strategies for cases where the possessor is a pronoun
"""
def customize_pronominal_possession(mylang,ch,rules,irules,lexicon,hierarchies):

    for pron in ch.get('poss-pron',[]):

        pron_num=pron.full_keys()[0].split("_")[0][-1]

        # Add subtypes of POSSESSOR feature for this strategy:
        mylang.add('possessive-pron-'+str(pron_num)+' := possessive.',section='addenda')
        mylang.add('possessor-pron-'+str(pron_num)+' :=\
                      possessor & '+'possessive-pron-'+str(pron_num)+'.',section='addenda')
        if pron.get('possessum-mark')=='yes':
            mylang.add('possessum-pron-'+str(pron_num)+' :=\
                      possessum & '+'possessive-pron-'+str(pron_num)+'.',section='addenda')

        # Add phrase rules:        
        customize_poss_rules(pron,mylang,ch,rules,hierarchies)

        # Add inflectional rules:
        if pron.get('type')=='affix':
            customize_poss_irules(pron,mylang,ch,irules,hierarchies)

        # Add lexical rules:
        if pron.get('type')=='non-affix':            
            customize_poss_lexicon(pron,mylang,ch,lexicon,rules,hierarchies)


#########################################################################################
# Add phrase rules
#########################################################################################


"""
Add the necessary phrase rule to combine possessor and possessum
If rule already exists (head-comp case), then make sure its order is correct.
Also add constraints to non-possessive phrase rules to prevent
them from allowing possessive words in incorrect places
"""
def customize_poss_rules(strat,mylang,ch,rules,hierarchies):

    # Define vars for all elements of strategy:
    strat_name=strat.full_keys()[0].split("_")[0]
    strat_num=strat_name[-1]
    strat_order=strat.get('order')
    mark_loc=strat.get('mark-loc')
    pron_allowed=True if strat.get('pronoun-allow')=='yes' else False
    adj_rule=False
    spec_rule=False
    # Set flags for pronouns 
    if 'poss-pron' in strat_name:
        pron_strat=True
        strat_num='pron-'+strat_num
        if strat.get('type')=='affix':
            pron_affix=True
        else:
            pron_affix=False
    else:
        pron_strat=False
        pron_affix=False

    # Define var to keep track of major constituent word order
    head_comp_order=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']
    if head_comp_order=='head-comp':
        head_comp_order='head-initial'
    else:
        head_comp_order='head-final'

    # Add vars to keep track of what rules have been added:
    phrase_rule=""
    rule_added=False

    # Start adding rules:
    mylang.set_section('phrases')

    # If no marking exists, add one of two juxtaposition rules:
    if mark_loc=='neither' and not pron_strat:
        
        # Add general juxtaposition rule:
        rule_added=True
        phrase_rule='poss-phrase-'+strat_num
        mylang.add(phrase_rule+JUXTAPOSITION_RULE)
        
        # Add constraints to general juxtaposition rule
        if strat.get('mod-spec')=='spec':
            mylang.add(phrase_rule+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < >,\
                                          HEAD-DTR.SYNSEM.LOCAL [ CAT [ VAL.SPR <[ ]> ],\
                                                                  CONT.HOOK.INDEX.COG-ST uniq-id  ],\
                                          C-CONT [ RELS <! '+POSS_REL+',\
                                                           '+POSSESSUM_EXIST_REL+' !>,\
                                                   HCONS <! qeq & [ HARG #harg, LARG #lbl] !> ] ].')

        elif strat.get('mod-spec')=='mod':
            mylang.add(phrase_rule+' := [ SYNSEM.LOCAL.CAT.VAL.SPR #spr,\
                                          HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.SPR #spr & <[ ]>],\
                                          C-CONT [ RELS <! '+POSS_REL+' !>,\
                                                   HCONS <! !> ] ].')
        
        #If both word orders are allowed, then you should add a type to this rule to inherit from:
        if strat_order=='either': 
            mylang.add(phrase_rule+' := binary-headed-phrase.')

        # Add any feature constraints to possessor in juxt construction
        if strat.get('feat'):
            customize_feature_values(mylang,ch,hierarchies,strat,phrase_rule,'juxt-rule')

        if not pron_allowed:
            mylang.add(phrase_rule+' := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.PRON - ] ].')

    # If possessor isn't an affix pronoun, add a phrase rule
    elif not pron_affix:

        # If possessor==spec, add a head-compositional variant of head-spec 
        if strat.get('mod-spec')=='spec':

            mylang.add('head-spec-phrase := basic-head-spec-phrase &\
                    [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                      HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')
                
            
#            rule_added=True
#            phrase_rule='head-spec-poss-phrase'+'-'+strat_num
#            mylang.add(phrase_rule+' :=  basic-head-spec-phrase-super & [  NON-HEAD-DTR.SYNSEM [ LOCAL.CAT [ VAL [ SPEC < [ LOCAL.CAT.POSSESSUM #poss ] >,\
#                                                                                                                   SPR < >,\
#                                                                                                                   COMPS < > ],\
#                                                                                                               HEAD +nvjrpcmo ],\
#                                                                                                   OPT - ],\
#                                                                           HEAD-DTR.SYNSEM.LOCAL [ CAT.POSSESSUM #poss,\
#                                                                                                   CONT.HOOK #hook ] ,\
#                                                                           C-CONT.HOOK #hook ].')

            spec_rule=True

            mylang.add('poss-unary-phrase-'+strat_num+' := basic-unary-phrase & \
              [ SYNSEM.LOCAL [ CONT.HOOK #hook,\
                               CAT [ HEAD det & [ POSSESSOR nonpossessive ],\
  	                 	   VAL [ SPR < >,\
                                         COMPS #comps,\
			                 SUBJ #subj,\
		     	                 SPEC < [ LOCAL [ CAT [ VAL.COMPS < > ,\
                                                                HEAD +np & [ PRON - ] ],\
		     	      	                          CONT.HOOK #hook & [ INDEX #possessum & [ COG-ST uniq-id ],\
				      		      	                      LTOP #lbl ] ] ] > ] ] ],\
             C-CONT [ RELS <! arg12-ev-relation & [ PRED "poss_rel", \
                                                      LBL #lbl, \
                                                      ARG1 #possessum, \
                                                      ARG2 #possessor ],\
  		                quant-relation & [ PRED "exist_q_rel", \
                                                   ARG0 #possessum, \
                                                   RSTR #harg ] !>,\
	                HCONS <! qeq & [ HARG #harg, LARG #lbl ]!>,\
                        ICONS <! !>   ],\
                ARGS < [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >,\
       	       		      	                    COMPS #comps & olist,\
				                    SUBJ #subj,\
                                                    SPEC < > ],\
      	   		      	              HEAD +np ],\
  			                CONT.HOOK.INDEX #possessor ] ] > ].')


            rules.add('poss-unary-'+strat_num+' := poss-unary-phrase-'+strat_num+'.')

        # If possessor==mod, add either head-mod or head-comp
        # Exception: no rule added if preexistent head-comps has correct order
        elif strat.get('mod-spec')=='mod':

            # Add head-comp rule if necessary
            if strat.get('mark-loc')=='possessum' or strat.get('mark-loc')=='both':

                phrase_rule="head-comp-poss-phrase"+'-'+strat_num

                # Check if the existing head-comp rule has the correct order; 
                # if not, add a new rule with correct order that only applies to poss-phrases.
                if head_comp_order!=strat_order:

                    rule_added=True

                    mylang.add(phrase_rule +' := basic-head-1st-comp-phrase &\
                                           [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +np,\
                                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.SPR <>,\
                                                                             HEAD +np ] ].')

            # Add head-mod rule
            else:
                adj_rule=True
                mylang.add('possessum-mod-rule := basic-head-mod-phrase-simple & [\
                                    NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.MOD.FIRST.LOCAL.CAT.POSSESSUM #poss ],\
                                    HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss ].')
                if strat_order=='head-initial':
                    mylang.add('head-adj-int-phrase :+ possessum-mod-rule.')
                    rules.add('head-adj-int := head-adj-int-phrase.')
                elif strat_order=='head-final':
                    mylang.add('adj-head-int-phrase :+ possessum-mod-rule.')
                    rules.add('adj-head-int := adj-head-int-phrase.')
                else:
                    mylang.add('head-adj-int-phrase :+ possessum-mod-rule.')
                    mylang.add('adj-head-int-phrase :+ possessum-mod-rule.')
                    rules.add('head-adj-int := head-adj-int-phrase.')
                    rules.add('adj-head-int := adj-head-int-phrase.')

                rule_added=True
#                phrase_rule="head-mod-poss-phrase"+'-'+strat_num

                # Note: adding SPR <> constraint to non head dtr so that possessor pronouns are 
                # forced to go through bare-np
#                mylang.add(phrase_rule+' := basic-head-mod-phrase-simple & head-compositional & \
#                                        [ SYNSEM.LOCAL.CAT [ HEAD +np,\
#                                                             VAL [ SPEC #spec ] ], \
#                                          HEAD-DTR.SYNSEM.LOCAL.CAT [ POSSESSUM #poss,\
#                                                                      HEAD +np,\
#                                                                      VAL.SPEC #spec ] ,\
#                                          NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +np & [ MOD.FIRST.LOCAL.CAT.POSSESSUM #poss ],\
#                                                                          VAL.SPR < > ] ].')
                

    # If a specialized (non-juxtaposition) poss phrase rule was added, require that the marked constituent be [ POSS possessive ]
    if rule_added and mark_loc!='neither': 
        
        # Make head-mod-poss and head-spec-poss accept only possessive phrases
        if not (phrase_rule=='head-comp-poss-phrase-'+strat_num or adj_rule or spec_rule):
            if strat.get('mark-loc')=='possessor':
                mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT [ POSSESSUM nonpossessive ],\
                                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+' ] ].',merge=True)
            if strat.get('mark-loc')=='possessum':
                mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ],\
                                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive ] ].',merge=True)
            if strat.get('mark-loc')=='both':
                mylang.add(phrase_rule+':= [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+' ],\
                                             HEAD-DTR.SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ] ].',merge=True)
            if pron_strat:
                if strat.get('possessum-mark')=='yes':
                    mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ],\
                                                 NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+' ] ].',merge=True)
                else:
                    mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT [ POSSESSUM nonpossessive ],\
                                                 NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+' ] ].',merge=True)


        # Make head-comp-poss accept only possessive phrases. 
        elif phrase_rule=='head-comp-poss-phrase-'+strat_num:

            if (mark_loc=='possessum' or mark_loc=='both') and strat.get('possessum-type')=='affix':
                mylang.add(phrase_rule+':= [ HEAD-DTR.SYNSEM.LOCAL.CAT [ POSSESSUM possessum ] ].')

        # Make non-possessive phrases reject possessive nouns:

#        # Make head-spec reject possessive nouns:
#        if ch.get('has-dets')=='yes':
#            mylang.add('head-spec-phrase :=\
#                              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.SPR <>,\
#                                                                HEAD +vjrcdmo &\
#                                                                  [ POSSESSOR nonpossessive ] ] ].',merge=True)

        # Determine word order for adjectives and make adjective phrases reject possessive nouns
#        adj_pos=''
#        for adj in ch.get('adj',[]):
#            mod_wo=adj.get('modpos',[])
#            if mod_wo=='before':
#                adj_pos='head-adj'
#            elif mod_wo=='after':
#                adj_pos='adj-head'
#            elif mod_wo=='either':
#                adj_pos='either'
#        for pc in ch.get('adj-pc',[]):
#            for lrt in pc.get('lrt'):
#                mod_wo=lrt.get('modpos')
#                if mod_wo=='before':
#                    adj_pos='head-adj'
#                elif mod_wo=='after':
#                    adj_pos='adj-head'
#                elif mod_wo=='either':
#                    adj_pos='either'
        clmod_pos=''
        for cms in ch.get('cms',[]):
            # If this is the first strategy, then just store its info
            if clmod_pos=='':
                if cms.get('position')=='after':
                    clmod_pos='head-adj'
                elif cms.get('position')=='before':
                    clmod_pos='adj-head'
                elif cms.get('position')=='either':
                    clmod_pos='either'

            # If this isn't the first strategy, check if you need to
            # update from one order to two orders
            if clmod_pos=='adj-head':
                if cms.get('position')=='after' or cms.get('position')=='either':
                    clmod_pos='either'
            if clmod_pos=='head-adj':
                if cms.get('position')=='before' or cms.get('position')=='either':
                    clmod_pos='either'

#        if adj_pos=='head-adj' or adj_pos=='either':
#            mylang.add('head-adj-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
#                                                                                 POSSESSUM nonpossessive  ] ].',section='addenda')
#        if adj_pos=='adj-head' or adj_pos=='either':
#            mylang.add('adj-head-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
#                                                                                 POSSESSUM nonpossessive ] ].',section='addenda')
        if clmod_pos=='head-adj' or clmod_pos=='either':
            mylang.add('head-adj-scop-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                                                 POSSESSUM nonpossessive  ] ].',section='addenda')
        if clmod_pos=='adj-head' or clmod_pos=='either':
            mylang.add('adj-head-scop-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                                                 POSSESSUM nonpossessive  ] ].',section='addenda')
        if phrase_rule=='head-comp-poss-phrase'+'-'+strat_num: 
            head_comp_order=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']
            mylang.add(head_comp_order+'-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR nonpossessive,\
                                                         HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL [ SPR <>,\
                                                                                           SPEC <> ] ] ].')

    if rule_added and (not adj_rule) and (not spec_rule):
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


#########################################################################################
# Add inflectional rules
#########################################################################################

# Adds inflectional rules (or adds constraints to inflectional rules added in
# morphotactics.py) that create possessive forms
def customize_poss_irules(strat,mylang,ch,irules,hierarchies):

    # Define vars for all elements of strategy:
    strat_name=strat.full_keys()[0].split("_")[0]
    strat_num=strat_name[-1]
    mark_loc=strat.get('mark-loc')
    mod_spec=strat.get('mod-spec')
    possessor_type=strat.get('possessor-type')
    possessum_type=strat.get('possessum-type')

    # Set flag for pronouns
    if 'poss-pron' in strat_name:
        pron_strat=True
    else:
        pron_strat=False

    # Go through the position class (pc) info till you find the strategy you're actually dealing with
    for pc in all_position_classes(ch):
        for lrt in pc.get('lrt',[]):
            for feat in lrt['feat']:

                # For non-pronoun strategies:
                if not pron_strat:

                    if strat_name==str(feat['name']) and feat['value']!='nonpossessive':

                        mylang.set_section('lexrules')

                        # Add possessor-inflecting rules:
                        if (mark_loc=='possessor' or mark_loc=='both') and possessor_type=='affix' and feat['value']=='possessor':

                            customize_possessor_irules(strat,mylang,ch,strat_num,mod_spec,mark_loc,hierarchies)

                        # Add possessum-inflecting rules
                        if (mark_loc=='possessum' or mark_loc=='both') and possessum_type=='affix' and feat['value']=='possessum':

                            customize_possessum_irules(strat,mylang,ch,strat_num,mod_spec,mark_loc,possessum_type,hierarchies)

                # Add irules for pronoun strategies:
                elif pron_strat:
                    
                    customize_possessor_pron_irules(strat,mylang,ch,strat_name,strat_num,feat,lrt,mod_spec,hierarchies)

def customize_possessor_irules(strat,mylang,ch,strat_num,mod_spec,mark_loc,hierarchies):
    
    # Add the basic possessor rule defn:
    possessor_rule_name ='possessor-lex-rule-'+strat_num
    
    # Add constraints to possessor rule for spec version
    if mod_spec=='spec':
        agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        mylang.add(possessor_rule_name+POSSESSOR_SPEC_RULE)

        # Add constraints to spec version for single marking
        if mark_loc=='possessor' or mark_loc=='both':
#        if mark_loc=='possessor':
#            mylang.add(possessor_rule_name+' := val-change-with-ccont-lex-rule & \
#            [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
#                                 VAL [ SPEC.FIRST.LOCAL [ CAT [ HEAD noun & [ PRON - ] ],\
#                                                          CONT.HOOK [ INDEX #possessum & [ COG-ST uniq-id ],\
#                                                                      LTOP #lbl ] ] ] ] ,\
#              C-CONT [ HOOK #hook ,\
#                       RELS <! '+ POSS_REL  +' , '+POSSESSUM_EXIST_REL+ ' !>, \
#                       HCONS <! qeq & [ HARG #harg, LARG #lbl ] !>, \
#                       ICONS <! !>  ] ].',merge=True)

            mylang.add(possessor_rule_name+' := cat-change-only-lex-rule & \
            [ SYNSEM.LOCAL.CAT [ VAL #val,\
                                 HEAD.POSSESSOR possessor-'+strat_num+' ] ,\
              DTR.SYNSEM.LOCAL.CAT.VAL #val,\
              C-CONT [ RELS <! !>, \
                       HCONS <! !>, \
                       ICONS <! !>  ] ].',merge=True)

            mylang.add('poss-unary-phrase-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD [ POSSESSOR possessor-'+strat_num+' ] ] > ].')

        # Add constraints to spec version for double marking
#        if mark_loc=='both':
#            mylang.add(possessor_rule_name+' := val-change-with-ccont-lex-rule & \
#                                           [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
#                                                                VAL [ SPEC.FIRST.LOCAL [ CAT [ HEAD noun & [ PRON - ] ] ] ] ] ,\
#                                             C-CONT [ HOOK #hook & [ INDEX #possessor ],\
#                                                      RELS <!  !>, \
#                                                      HCONS <! !>, \
#                                                      ICONS <! !>  ] ].',merge=True)


    # Add constraints to possessor rule for mod version
    elif mod_spec=='mod':

        # Add constraints to mod version for single marking
        if mark_loc=='possessor':
            agr_prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            mylang.add(possessor_rule_name+POSSESSOR_NON_SPEC_RULE)
            mylang.add(possessor_rule_name+' := head-change-with-ccont-lex-rule & \
                        [ SYNSEM.LOCAL.CAT [ HEAD [ MOD.FIRST [ LOCAL [ CAT [ VAL.SPR <[]>,\
                                                                        HEAD +np & [ PRON - ] ], \
                                                                        CONT.HOOK [ INDEX #possessum, \
                                                                                    LTOP #lbl ] ], \
                                                                OPT - ],\
                                                    POSSESSOR possessor-'+strat_num+' ], \
                                             VAL [ SPEC #spec ] ] ,\
                          C-CONT [ HOOK #hook,\
                                   RELS <! '+POSS_REL+' !>,\
                                   HCONS <! !>, \
                                   ICONS <! !>  ], \
                          DTR.SYNSEM.LOCAL.CAT.VAL.SPEC #spec  ].',merge=True)

        # Add constraints to mod version for double marking
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



def customize_possessum_irules(strat,mylang,ch,strat_num,mod_spec,mark_loc,possessum_type,hierarchies):
    
    # Add general possessum-marking rule:
    possessum_rule_name ='possessum-lex-rule-'+strat_num

                            
    # Add constraints to possessor rule for spec version                            
    if mod_spec=='spec':

        mylang.add(possessum_rule_name+POSSESSUM_SPEC_RULE)

        agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        
        # Add constraints in for single marking and double marking:
        if mark_loc=='possessum' or mark_loc=='both':

#            mylang.add(possessum_rule_name+':=  val-change-with-ccont-lex-rule & \
#                         [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
#                                              VAL [ COMPS #comps,\
#                                                    SPR < [ LOCAL [ CONT.HOOK #hook & [ INDEX #possessor ],\
#                                                                    CAT [ VAL.SPR < >,\
#                                                                          HEAD +np ] ] ] > ] ] ,\
#                            C-CONT [ HOOK #hook & [ INDEX.COG-ST uniq-id ],\
#                                                    HCONS <! qeq & [HARG #harg, LARG #lbl ] !>, \
#                                                    ICONS <! !>,\
#                                                    RELS <! '+POSS_REL+',\
#                                                            '+POSSESSUM_EXIST_REL+' !> ],\
#                            DTR.SYNSEM.LOCAL [ CONT.HOOK #hook & [ INDEX #possessum,\
#                                                                   LTOP #lbl],\
#                                               CAT [ HEAD.PRON -,\
#                                                     VAL.COMPS #comps ] ] ].',merge=True)
#        if mark_loc=='both':

            mylang.add(possessum_rule_name+':=  cat-change-only-lex-rule & \
                         [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                              VAL #val  ] ,\
                            C-CONT [ HCONS <! !>, \
                                     ICONS <! !>,\
                                     RELS <! !> ],\
                            DTR.SYNSEM.LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                     HEAD.PRON - ,\
                                                     VAL #val ] ] ].',merge=True)

            mylang.add('poss-unary-phrase-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ] ].')

            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            instance_tmp={}
            if strat.get('possessor-feat'):
                for key in strat.keys():
                    new_key=key.replace('feat','skip')
                    new_key=new_key.replace('possessor-skip','feat')
                    instance_tmp[new_key]=strat.get(key)
                customize_feature_values(mylang,ch,hierarchies,instance_tmp,possessum_rule_name,'possessum-spec-mark')
                                    
        # Add constraints to spec version just for double marking
#        if mark_loc=='both':
#            mylang.add(possessum_rule_name+' :=\
#            [ SYNSEM.LOCAL.CAT.VAL.SPR < [ LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ] > ,\
#              DTR.SYNSEM.LOCAL.CAT.HEAD.PRON - ].')
        
        # Add constraints to spec version for case where possessum is marked 
        # and possessor = pronoun
        if mark_loc=='possessum-with-pron':
            mylang.add(possessum_rule_name+':=  cat-change-only-lex-rule & \
                         [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                              VAL [ COMPS #comps,\
                                                    SPR < [ LOCAL [ CAT [ VAL.SPR < >,\
                                                                          HEAD noun ] ] ] > ] ] ,\
                            C-CONT.HOOK #hook,\
                            DTR.SYNSEM.LOCAL [ CAT [ HEAD.PRON -,\
                                                     VAL.COMPS #comps ] ] ].',merge=True)
            

    # Add constraints to possessor rule for mod version
    if mod_spec=='mod':

        agr_prefix='SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        mylang.add(possessum_rule_name+POSSESSUM_NON_SPEC_RULE)

        if mark_loc=='possessum' or mark_loc=='both':
            mylang.add(possessum_rule_name+':= val-change-with-ccont-lex-rule & \
                                                       [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                                                            VAL [ SPR #spr, \
                                                                                  COMPS.FIRST [ LOCAL [ CAT cat-sat & [ HEAD +np ], \
                                                                                                        CONT.HOOK [ INDEX #possessor ] ] ] ] ],\
                                                         C-CONT [ HOOK #hook ,\
                                                                  RELS <! '+POSS_REL+' !>,\
                                                                  HCONS <! !>,\
                                                                  ICONS <! !>  ],\
                                                         DTR.SYNSEM.LOCAL [ CAT [ HEAD.PRON -,\
                                                                                  VAL.SPR #spr ],\
                                                                            CONT.HOOK [ INDEX #possessum,\
                                                                                        LTOP #lbl ] ] ].',merge=True)

            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            instance_tmp={}
            if strat.get('possessor-feat'):
                for key in strat.keys():
                    new_key=key.replace('feat','skip')
                    new_key=new_key.replace('possessor-skip','feat')
                    instance_tmp[new_key]=strat.get(key)
                customize_feature_values(mylang,ch,hierarchies,instance_tmp,possessum_rule_name,'possessum-mod-mark')

        # Add constraints to spec version for double marking
        if mark_loc=='both':
            mylang.add(possessum_rule_name+' :=\
               [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD +np & [ POSSESSOR possessor-'+strat_num+' ] ].')

        # Add constraints to spec version for case where possessum is marked 
        # and possessor = pronoun
        if mark_loc=='possessum-with-pron':
            mylang.add(possessum_rule_name+':= cat-change-only-lex-rule & \
                                                       [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                                                            VAL [ SPR #spr ] ],\
                                                         C-CONT [ HOOK #hook ,\
                                                                  RELS <! !>,\
                                                                  HCONS <! !>,\
                                                                  ICONS <! !>  ],\
                                                         DTR.SYNSEM.LOCAL [ CAT [ HEAD.PRON -,\
                                                                                  VAL.SPR #spr ] ] ].',merge=True)

    # Add agreement features to the possessum affix
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
                            
# EKN 2018-01-29 This was in here to constrain non-possessive nouns, but was breaking the pronoun cases.
# Taking it out doesn't cause any tests to break, but I'm leaving it commented out while still in active 
# development
#                    elif strat_name==str(feat['name']) and feat['value']=='nonpossessive':
#                        if (mark_loc=='possessum' or mark_loc=='both') and mod_spec=='spec':
                            # This should keep normal head-spec constructions from letting noun phrases act as determiners:
#                            mylang.add(get_name(lrt)+'-lex-rule := [ SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CAT.HEAD det ].')



def customize_possessor_pron_irules(strat,mylang,ch,strat_name,strat_num,feat,lrt,mod_spec,hierarchies):

    if strat_name in str(feat['name']) and feat['value']!='minus':

        # Add general version of pronoun affix rule:
        mylang.add(get_name(lrt)+'-lex-rule :=\
           [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR possessive-pron-'+strat_num+',\
             DTR.SYNSEM.LOCAL.CONT.HOOK #hook & [ INDEX #possessum & [ COG-ST uniq+fam+act ],\
                                                  LTOP #lbl],\
              C-CONT.HOOK #hook ].')
   
        # Add constraints to pronoun affix rule for spec version
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
                DTR.SYNSEM.LOCAL.CAT [ HEAD.PRON -,\
                                       VAL [ SPEC #spec,\
                                             SUBJ #subj,\
                                             COMPS #comps] ] ].')
            
        # Add constraints to pronoun affix rule for mod version
        elif mod_spec=='mod':
            mylang.add(get_name(lrt)+'-lex-rule := \
               [ SYNSEM.LOCAL.CAT.VAL #val,\
                 DTR.SYNSEM.LOCAL.CAT [ HEAD.PRON -,\
                                        VAL #val ],\
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


#########################################################################################
# Add lexical items
#########################################################################################

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
    pron_allowed=True if strat.get('pronoun-allow')=='yes' else False

    if 'poss-pron' in strat_name:
        pron_strat=True
    else:
        pron_strat=False

    # Add lexical items other than poss pronouns:
    if not pron_strat:

        # Add possessor-marking adpositons:
        if (mark_loc=='possessor' or mark_loc=='both') and possessor_type=='non-affix':
            customize_possessor_lexicon(strat,mylang,ch,lexicon,strat_name,strat_num,mod_spec,mark_loc,pron_allowed,hierarchies)

        # Add possessum-marking nouns:
        if (mark_loc=='possessum' or mark_loc=='both') and possessum_type=='non-affix':
            customize_possessum_lexicon(strat,mylang,ch,lexicon,strat_name,strat_num,mod_spec,mark_loc,pron_allowed,possessor_type,hierarchies)

    elif pron_strat:
        customize_possessor_pron_lexicon(strat,mylang,ch,lexicon,strat_name,strat_num,mod_spec,hierarchies)
        



def customize_possessor_lexicon(strat,mylang,ch,lexicon,strat_name,strat_num,mod_spec,mark_loc,pron_allowed,hierarchies):
            
    # Add most general defn of possessor-marking adp:
    mylang.set_section('otherlex')
    mylang.add(TWO_REL_ADP)
    mylang.add('possessor-adp-lex-'+strat_num+' '+POSSESSOR_ADP_LEX)
    
    # Optionally block PRON + items from being possessors
    if not pron_allowed:
        mylang.add('possessor-adp-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.PRON - ].')

    # Add constraints to possessor adp for spec version
    if mod_spec=='spec':

#        mylang.add('possessor-adp-lex-'+strat_num+' := \
#                                 [  SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
#                                                       POSSESSUM nonpossessive,\
#                                                       VAL [ SPEC.FIRST.LOCAL.CAT [ VAL.SPR < [ ] >,\
#                                                                                    HEAD.PRON - ] ] ] ].')

        mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [  SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                       POSSESSUM nonpossessive ] ].')
        mylang.add('possessor-adp-lex-'+strat_num+' := \
                [  SYNSEM.LOCAL [ CONT [ RELS <! !>,\
                                         HCONS <! !> ] ] ].')

        mylang.add('poss-unary-phrase-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD [ POSSESSOR possessor-'+strat_num+' ] ] > ].')

        # Add constraints to spec version for single-marking:
#        if mark_loc=='possessor':
#            mylang.add('possessor-adp-lex-'+strat_num+' := \
#                                 [  SYNSEM.LOCAL [ CAT  [ VAL [ COMPS.FIRST.LOCAL [ CONT.HOOK.INDEX #possessor ],\
#                                                                SPEC.FIRST.LOCAL [ CONT.HOOK [ INDEX #possessum,\
#                                                                                               LTOP #lbl ] ] ] ],\
#                                                  CONT [ RELS <! '+POSS_REL+',\
#                                                                 '+POSSESSUM_EXIST_REL+' !>,\
#                                                         HCONS <!  qeq & [ HARG #harg, LARG #lbl ] !> ] ] ].')
#
#        # Add constraints to spec version for double-marking:
#        elif mark_loc=='both':
#            mylang.add('possessor-adp-lex-'+strat_num+' := \
#                                 [  SYNSEM.LOCAL [ CONT [ RELS <! !>,\
#                                                          HCONS <! !> ] ] ].')

    # Add constraints to possessor adp for mod version
    if mod_spec=='mod':
        mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT.HEAD [ POSSESSOR possessor-'+strat_num+',\
                                                             MOD.FIRST.LOCAL [ CAT [ HEAD.PRON -,\
                                                                                     VAL.SPR < [ ] > ] ] ],\
                                                  CONT [ HCONS <! !> ] ] ] .')

        # Add constraints to mod version for single-marking:
        if mark_loc=='possessor':
            mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT [ VAL [ COMPS.FIRST.LOCAL [ CONT.HOOK.INDEX #possessor ] ],\
                                                         HEAD.MOD.FIRST.LOCAL [ CONT.HOOK [ INDEX #possessum,\
                                                                                            LTOP #lbl ] ] ],\
                                                  CONT [ RELS <! '+POSS_REL+' !> ] ] ].')

        # Add constraints to mod version for double-marking:
        elif mark_loc=='both':
            mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                        POSSESSUM nonpossessive, ]\
                                                  CONT.RELS <! !> ] ] .')

    # Add agreement features to the possessor adp if appropriate:
    # TODO: these lex items don't follow nomenclature conventions yet:
    if strat.get('possessor-agr')=='non-agree':
        
        # Add non-agreeing adp to lexicon:
        orth=strat.get('possessor-orth')
        lexicon.add('possessor-adp-'+strat_num+' := possessor-adp-lex-'+strat_num+' &\
                                                      [ STEM < "'+orth+'" >].')

    # Add agreeing adps to mylang and lexicon:
    elif strat.get('possessor-agr')=='agree':

        # Set feature path for agreement features
        for form in strat.get('possessor-form'):
            if mod_spec=='spec':
                agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            elif mod_spec=='mod':
                if mark_loc=='possessor':
                    agr_prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                elif mark_loc=='both':
                    agr_prefix='SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

            # Add agreeing forms to mylang
            orth=form.get('agr-orth')
            adp_type=adp_id(form)
            mylang.add(adp_type+' := possessor-adp-lex-'+strat_num+' &\
                                        [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                        '+agr_prefix+' #png ].')

            customize_feature_values(mylang,ch,hierarchies,form,adp_type,'poss-marker')

            # Add agreeing adps to lexicon:
            orth=form.get('agr-orth')
            lexicon.add(adp_type.replace('-lex','')+' := '+adp_type+' &\
                                         [ STEM < "'+orth+'" > ].')

    # Add any necessary constraints to the complement of the possessor-marking word:
    instance_tmp={}
    if strat.get('dep-comp-feat'):
        for key in strat.keys():
            new_key=key.replace('feat','skip')
            new_key=new_key.replace('dep-comp-skip','feat')
            instance_tmp[new_key]=strat.get(key)
        customize_feature_values(mylang,ch,hierarchies,instance_tmp,'possessor-adp-lex-'+strat_num,'poss-adp-comp')


def customize_possessum_lexicon(strat,mylang,ch,lexicon,strat_name,strat_num,mod_spec,mark_loc,pron_allowed,possessor_type,hierarchies):
    mylang.set_section('nounlex')
    
    # Add spec-version (only version in this case)
    if mod_spec=='spec':

        mylang.add('poss-unary-phrase-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ] ].')

        # Add constraints to spec version for single marking
        if mark_loc=='possessum':

            mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX)
            mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL [ CAT [ HEAD [ POSSESSOR nonpossessive ],\
                                                                                       POSSESSUM possessum-'+strat_num+' ] ] ].',merge=True)

#            mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ LOCAL.CAT.HEAD noun ] > ].')                           

            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            if strat.get('possessor-feat'):
                instance_tmp={}
                if strat.get('possessor-feat'):
                    for key in strat.keys():
                        new_key=key.replace('feat','skip')
                        new_key=new_key.replace('possessor-skip','feat')
                        instance_tmp[new_key]=strat.get(key)
                    customize_feature_values(mylang,ch,hierarchies,instance_tmp,'possessum-noun-lex-'+strat_num,'possessum-spec-mark')

        # Add constraints to spec version for double marking
        if mark_loc=='both':

            mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX)
            mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL [ CAT [ HEAD [ POSSESSOR nonpossessive ],\
                                                                                       POSSESSUM possessum-'+strat_num+' ] ] ].',merge=True)

#            if possessor_type=='affix':
#                mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ LOCAL.CAT.HEAD noun ] > ].')

#            else:
#                mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ LOCAL.CAT.HEAD adp ] > ].')
                
        if mark_loc=='possessum-with-pron':
            # TODO: generalize typedef in header so that this doesn't need to be declared separately
            POSSESSUM_NOUN_LEX_W_PRON=':= basic-two-arg &\
                                   [ SYNSEM.LOCAL [ CAT [ HEAD #head & noun ,\
                                                          VAL [ SUBJ < >,\
                                                                SPR < #spr & [ LOCAL [ CAT.VAL.SPR < > ] ] >,\
                                                                COMPS < #comps & [ LOCAL [ CONT.HOOK #hook,\
                                                                                           CAT [ VAL.SPR <[ ]>,\
                                                                                                 HEAD #head & [ PRON - ] ] ] ] > ] ],\
                                                    CONT [ RELS <! !>,\
                                                           HCONS <! !>,\
                                                           HOOK #hook,\
                                                           ICONS <! !> ] ],\
                                     ARG-ST < #spr, #comps > ].'

            mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX_W_PRON)
            mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL [ CAT [ HEAD [ POSSESSOR nonpossessive ],\
                                                                                          POSSESSUM possessum-'+strat_num+' ] ] ].',merge=True)

    # Add agreement features where appropriate
    if strat.get('possessum-agr')=='non-agree':
        orth=strat.get('possessum-orth')
        # Add single, non-agreeing form to lexicon
        lexicon.add('possessum-noun-'+strat_num+' := possessum-noun-lex-'+strat_num+' &\
                                                  [ STEM < "'+orth+'" >].')
            
    elif strat.get('possessum-agr')=='agree':
        for form in strat.get('possessum-form'):
            # Set feature paths to agreement feats:
            if mod_spec=='spec':
                prefix='SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            elif mod_spec=='mod':
                prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            noun_type=noun_id(form)

            # Add appropriate agreeing forms to mylang:
            mylang.add(noun_type+' := possessum-noun-lex-'+strat_num+' &\
                          [ SYNSEM.LOCAL.CAT.POSSESSUM.POSS-AGR #png,\
                                '+prefix+' #png ].')
            customize_feature_values(mylang,ch,hierarchies,form,noun_type,'poss-marker')
            orth=form.get('agr-orth')
            # Add appropriate number of agreeing forms to lexicon
            lexicon.add(noun_type.replace('-lex','')+' := '+noun_type+' &\
                                         [ STEM < "'+orth+'" > ].')


def customize_possessor_pron_lexicon(strat,mylang,ch,lexicon,strat_name,strat_num,mod_spec,hierarchies):

    # Set vars for pron strat:
    noun_type=noun_id(strat)
    if strat.get('agr')=='agree':
        agr=True
    else:
        agr=False

    # Add general form of pronoun:
    mylang.set_section('nounlex')
    mylang.add(noun_type+POSSESSOR_PRON_LEX)

    # Add constraints for spec version:
    if mod_spec=='spec':
        agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
#        mylang.add(noun_type+' := \
#                        [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-pron-'+strat_num+',\
#                                               VAL.SPEC < #spec & [ OPT -,\
#                                                                LOCAL [ CAT.HEAD.PRON -,\
#                                                                        CONT.HOOK [ INDEX #possessum & [ COG-ST uniq+fam+act ],\
#                                                                                    LTOP #lbl ] ] ] > ],\
#                                         CONT [ RELS  <! '+POSSESSUM_EXIST_REL+',\
#                                                         '+POSS_REL+',\
#                                                           #altkeyrel !>,\
#                                                  HCONS <! qeq & [ HARG #harg,\
#                                                                   LARG #lbl ] !> ] ],\
#                          ARG-ST < #spec > ].')
 
        mylang.add(noun_type+' := \
                        [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-pron-'+strat_num+',\
                                               VAL.SPEC < > ],\
                                         CONT [ RELS  <! #altkeyrel !>,\
                                                  HCONS <! !> ] ] ].')
           
    # Add constraints for mod version
    elif mod_spec=='mod':
        agr_prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        mylang.add(noun_type+' := \
                        [ SYNSEM.LOCAL [ CAT.HEAD [ POSSESSOR possessor-pron-'+strat_num+',\
                                                    MOD < [ OPT -,\
                                                            LOCAL [ CAT [ HEAD.PRON -,\
                                                                          VAL.SPR < [ ] > ],\
                                                                    CONT.HOOK [ INDEX #possessum,\
                                                                                LTOP #lbl ] ] ] > ],\
                                         CONT [ RELS  <!  '+POSS_REL+',\
                                                           #altkeyrel !>,\
                                                HCONS <! !> ] ] ].')

    if agr: 
        mylang.add(noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                              '+agr_prefix+' #png ].')


    # Add forms to lexicon.tdl:
    for pron_inst in strat.get('instance'):
        orth=pron_inst.get('orth')
        instance_name=noun_id(pron_inst)
        mylang.add(instance_name+' := '+noun_type+'.')
        customize_feature_values(mylang,ch,hierarchies,pron_inst,instance_name,'noun')
        lexicon.add(instance_name.replace('-lex','')+' := '+instance_name+' &\
                                                          [ STEM < "'+orth+'" > ].')

        # Add agr features where appropriate
        instance_tmp={}                
        for key in pron_inst.keys():
            # Relabel the inherent features as something else ('skip') 
            # Relabel the agreement features as simply features ('feat')
            # Then call customize_feature_values() with the 'poss-marker' setting
            # so that the agreement features are added at POSS.POSS-AGR instead of HOOK.INDEX.PNG
            new_key=key.replace('feat','skip')
            new_key=new_key.replace('agr-skip','feat')
            instance_tmp[new_key]=pron_inst.get(key)
        # TODO: Figure out how to cast instance_tmp from a dict to a ChoiceDict so that no future
        #  developers have to deal with this mess in features.py
        customize_feature_values(mylang,ch,hierarchies,instance_tmp,instance_name,'poss-marker')

    # Add any necessary markings to the possessum:
    if strat.get('possessum-mark')=='yes':

        # Make possessor pron to req a marked possessum:
        if mod_spec=='mod':
            mylang.add(noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.POSSESSUM possessum-pron-'+strat_num+'] > ].')
            

        
        # Add affixal markings:
        if strat.get('possessum-mark-type')=='affix':

            customize_possessum_irules(strat,mylang,ch,'pron-'+strat_num,mod_spec,'possessum-with-pron','affix',hierarchies)

            if strat.get('possessum-mark-affix-agr')=='agree':
                if mod_spec=='spec':
                            mylang.add(noun_type+' := \
                              [ SYNSEM.LOCAL [ CONT.HOOK.INDEX.PNG #png,\
                                               CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png ] ].')
                if mod_spec=='mod':
                            mylang.add(noun_type+' := \
                              [ SYNSEM.LOCAL [ CONT.HOOK.INDEX.PNG #png,\
                                               CAT.HEAD.MOD.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png ] ].')
                
        if strat.get('possessum-mark-type')=='non-affix':
            
            customize_possessum_lexicon(strat,mylang,ch,lexicon,strat_name,'pron-'+strat_num,mod_spec,'possessum-with-pron',True,'non-affix',hierarchies)
