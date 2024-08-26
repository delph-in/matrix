import gmcs.tdl
from gmcs.lib import TDLHierarchy
import gmcs.utils
from gmcs.utils import get_name
from gmcs.choices import ChoiceDict
from gmcs.linglib.word_order import customize_major_constituent_order, customize_nmz_clause_word_order
from gmcs.linglib.morphotactics import all_position_classes, add_nonpossessive_behavior
from gmcs.linglib.features import customize_feature_values
from gmcs.linglib.lexical_items import adp_id, noun_id
from gmcs.linglib import lexbase
from gmcs.linglib.nominalized_clauses import need_specialized_head_spec, get_nmz_clause_wo, needs_anc_wo_feat

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

POSSESSUM_ID_HS = ' [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                    HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].'

POSSESSUM_RULE = ' :=\
                  [ SYNSEM.LOCAL.CAT.HEAD [ MOD < >,\
                                            POSSESSOR nonpossessive ] ].'


JUXTAPOSITION_RULE = ':= [ SYNSEM.LOCAL [ CAT [ HEAD #head,\
                                              VAL [ SUBJ < >, \
                                                    SPEC < > ] ] ],\
                           HEAD-DTR.SYNSEM.LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                         HEAD #head & noun & [ POSSESSOR nonpossessive,\
                                                                               PRON - ] ] ],\
                           NON-HEAD-DTR.SYNSEM.LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                             HEAD noun & [ POSSESSOR nonpossessive ],\
                                                             VAL.SPR < > ]]].'


TWO_REL_ADP = 'two-rel-adposition-lex := basic-icons-lex-item &\
  [ SYNSEM [ LOCAL [ CAT [ HEAD adp,\
                           VAL.COMPS < [ LOCAL [ CAT cat-sat,\
                                                 CONT.HOOK #hook & [ INDEX #ind,\
                                                             ICONS-KEY.IARG1 #clause ] ] ] > ],\
                     CONT.HOOK #hook & [ CLAUSE-KEY #clause ] ],\
             LKEYS.KEYREL arg12-ev-relation & [ ARG2 #ind ] ] ].'

POSSESSOR_ADP_LEX = ':= two-rel-adposition-lex &\
                                 [  SYNSEM.LOCAL [ CAT [ VAL [ SPEC < >,\
                                                               SUBJ < >,\
                                                               SPR < >,\
                                                               COMPS.FIRST [ LOCAL.CAT [ HEAD noun ,\
                                                                                          VAL.SPR < > ],\
                                                                              OPT - ] ] ],\
                                                  CONT.ICONS.LIST < > ] ].'

POSSESSUM_NOUN_LEX = ':= non-local-none-lex-item &\
                                   [ SYNSEM.LOCAL [ CAT [ HEAD #head & noun ,\
                                                          VAL [ SUBJ < >,\
                                                                SPR < [ LOCAL.CAT.HEAD det ] >,\
                                                                COMPS < #comps & [ LOCAL [ CONT.HOOK #hook,\
                                                                                           CAT [ POSSESSUM nonpossessive,\
                                                                                                 VAL.SPR <[ ]>,\
                                                                                                 HEAD #head & [ PRON - ,\
                                                                                                                POSSESSOR nonpossessive ] ] ] ] > ] ],\
                                                    CONT [ RELS.LIST < >,\
                                                           HCONS.LIST < >,\
                                                           HOOK #hook,\
                                                           ICONS.LIST < > ] ],\
                                     ARG-ST < #comps & [OPT -] > ].'

POSSESSUM_NOUN_LEX_W_PRON = ':= non-local-none-lex-item &\
                                   [ SYNSEM.LOCAL [ CAT [ HEAD #head & noun ,\
                                                          VAL [ SUBJ < >,\
                                                                SPR < #spr & [ LOCAL [ CAT [ VAL.SPR < > ] ] ] >,\
                                                                COMPS < #comps & [ LOCAL [ CONT.HOOK #hook,\
                                                                                           CAT [ VAL.SPR <[ ]>,\
                                                                                                 HEAD #head & [ PRON - ] ] ] ] > ] ],\
                                                    CONT [ RELS.LIST < >,\
                                                           HCONS.LIST < >,\
                                                           HOOK #hook,\
                                                           ICONS.LIST < > ] ],\
                                     ARG-ST < #spr, #comps > ].'

# TODO: change one-arg to being added by the logic section, since the modifier-version is zero-arg.
POSSESSOR_PRON_LEX = ' := non-local-none-lex-item &\
                        [ SYNSEM [ LOCAL [ CONT.HOOK [ INDEX #possessor,\
                                                       LTOP #ltop ],\
                                           CAT [ VAL [ SPR < [ LOCAL.CAT.HEAD det,\
                                                               OPT + ] >,\
                                                       COMPS olist,\
                                                       SUBJ olist, SPEC < > ],\
                                                 HEAD noun & [ PRON + ] ] ],\
                                   LKEYS.ALTKEYREL #altkeyrel & noun-relation &\
                                                         [ PRED "pron_rel",\
                                                           LBL #ltop,\
                                                           ARG0 #possessor & [ COG-ST activ-or-more,\
                                                                               SPECI + ] ] ] ].'

NON_POSS_LEX_ITEM = '[ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                     POSSESSUM nonpossessive ] ].'

POSS_UNARY = ' := basic-unary-phrase & \
              [ SYNSEM [ NON-LOCAL #nonloc \
                            LOCAL [CAT [ HEAD det & [ MOD < >, POSSESSOR possessor ],\
  	                 	   VAL [ SPR < >,\
                                         COMPS < >,\
			                 SUBJ < >,\
		     	                 SPEC < [ LOCAL [ CAT [ VAL.COMPS < > ,\
                                                                HEAD noun & [ PRON - ] ] ] ] > ] ] ] ],\
                ARGS < [ SYNSEM [ LOCAL [ CAT [ VAL [ SPR < >,\
       	       		      	                    COMPS < >,\
				                    SUBJ < >,\
                                                    SPEC < > ],\
      	   		      	              HEAD +np & [MOD < >] ] ], \
                                  NON-LOCAL #nonloc ] ] > ].'


##################################################################
## Primary function (called from customize.py)                 ###
##################################################################
def customize_adnominal_possession(mylang, ch, rules, irules, lexicon, hierarchies):
    # Check if any possessive strategies or pronouns are defined.
    # If so, add the POSS head feature.
    for key in ch.full_keys():
        if 'poss-strat' in key or 'poss-pron' in key:
            customize_poss_addenda(mylang, ch)

    customize_np_possession(mylang, ch, rules, irules, lexicon, hierarchies)

    customize_pronominal_possession(
        mylang, ch, rules, irules, lexicon, hierarchies)

#####################################################################################################
## Secondary functions (called by customize_adnominal_possession() or other secondary functions)  ###
#####################################################################################################


"""
Adds things to the addenda section that are necessary
for any strategy
"""


def customize_poss_addenda(mylang, ch):
    mylang.add('head :+ [ POSSESSOR poss ].', section='addenda')
    mylang.add('cat :+ [ POSSESSUM poss ].', section='addenda')
    mylang.add('poss := *top* & [ POSS-AGR png ].', section='addenda')
    mylang.add('basic-bare-np-phrase :+ [ SYNSEM.LOCAL.CAT [ VAL.SPEC < >,\
                                                             HEAD #head,\
                                                             POSSESSUM #possessum],\
                                          HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD #head,\
                                                             POSSESSUM #possessum & nonpossessive ] ].', section='addenda')
    mylang.add('basic-head-1st-comp-phrase :+ [ \
                  SYNSEM.LOCAL.CAT.POSSESSUM #poss,\
                  HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss ].', section='addenda')

    # Set nouns to default nonpossessive behavior if no affixal strategies exist:
    poss_strat_types = set()
    for strat in ch.get('poss-strat'):
        poss_strat_types.add(strat.get('possessor-type'))
        poss_strat_types.add(strat.get('possessum-type'))
        poss_strat_types.add(strat.get('possessum-mark-type'))
    if 'affix' not in poss_strat_types:
        mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                     POSSESSUM nonpossessive ] ].', section='nounlex')
    else:
        #If affixal strategies do exist, set any nouns that do not serve as input to a possessive rule
        #to have default nonpossessive behavior
        add_nonpossessive_behavior(ch, mylang)
    mylang.add('basic-determiner-lex :+ ' +
                NON_POSS_LEX_ITEM, section='addenda')



def customize_poss_hier(mylang, strat_num):
    hier = TDLHierarchy('possession')
    hier.add('possessive', 'poss', 'supertype for possessive features')
    hier.add('nonpossessive', 'poss', 'type for nonpossessive')
    hier.add('possessor', 'possessive', '')
    hier.add('possessum', 'possessive', '')
    hier.add('possessive-'+str(strat_num), 'possessive',
             'supertype for strategy '+str(strat_num))
    hier.add('possessor-'+str(strat_num),
             'possessor & possessive-'+str(strat_num), '')
    hier.add('possessum-'+str(strat_num),
             'possessum & possessive-'+str(strat_num), '')
    hier.save(mylang)




"""
Calls customize_poss_rules, customize_poss_irules, and customize_poss_lexicon
to build possessive strategies for cases where the possessor is a full NP
"""


def customize_np_possession(mylang, ch, rules, irules, lexicon, hierarchies):
    for strat in ch.get('poss-strat', []):

        # Add subtypes of POSSESSOR and POSSESSUM features for this strategy
        strat_num = strat.full_keys()[0].split("_")[0][-1]
        customize_poss_hier(mylang, strat_num)

        # Add phrase rules:
        customize_poss_rules(strat, mylang, ch, rules, hierarchies)

        # Add inflectional rules:
        if strat.get('possessor-type') == 'affix' or strat.get('possessum-type') == 'affix':

            customize_poss_irules(
                strat, mylang, ch, irules, hierarchies, rules)

        # Add lexical items:
        if strat.get('possessor-type') == 'non-affix' or strat.get('possessum-type') == 'non-affix':
            customize_poss_lexicon(
                strat, mylang, ch, lexicon, rules, hierarchies)


"""
Calls customize_poss_rules, customize_poss_irules, and customize_poss_lexicon
to build possessive strategies for cases where the possessor is a pronoun
"""


def customize_pronominal_possession(mylang, ch, rules, irules, lexicon, hierarchies):

    for pron in ch.get('poss-pron', []):

        # Add possessive features:
        pron_num = pron.full_keys()[0].split("_")[0][-1]
        customize_poss_hier(mylang, 'pron-'+pron_num)

        # Add phrase rules:
        customize_poss_rules(pron, mylang, ch, rules, hierarchies)

        # Add inflectional rules:
        if pron.get('type') == 'affix':
            customize_poss_irules(pron, mylang, ch, irules, hierarchies, rules)

        # Add lexical rules:
        if pron.get('type') == 'non-affix':
            customize_poss_lexicon(
                pron, mylang, ch, lexicon, rules, hierarchies)


#########################################################################################
# Add phrase rules
#########################################################################################

"""
 Helper function to determine if you'll need to
 manipulate the order of head-comp rules:
"""


def check_hc_order_manip(ch, strat, hc):

    # Order of major poss phrase
    strat_order = strat.get('order')
    # Order of minor phrase consisting of mark + possessor/um
    dep_mark_order = strat.get('possessor-mark-order')
    head_mark_order = strat.get('possessum-mark-order')
    mark_loc = strat.get('mark-loc')
    mod_spec = strat.get('mod-spec')
    # Order of head-comps that already exists:
    if hc == 'head-comp':
        head_comp_order = 'head-initial'
        default_init = '+'
    elif hc == 'comp-head':
        head_comp_order = 'head-final'
        default_init = '-'
    else:
        head_comp_order = 'either'
        default_init = 'either'

    default_nmz_init = None
    nmz_hc = None
    nmz_head_comp_order = None
    if ch.get('ns'):
        if 'same-word-order' not in ch or ch.get('same-word-order') == 'yes':
            default_nmz_init = default_init
            nmz_hc = hc
            nmz_head_comp_order = head_comp_order
        elif ch.get('same-word-order') == 'no':
            nmz_word_order = get_nmz_clause_wo(ch)
            if nmz_word_order in ['sov', 'osv', 'ovs', 'v-final']:
                nmz_hc = 'comp-head'
            elif nmz_word_order in ['svo', 'vos', 'vso', 'v-initial']:
                nmz_hc = 'head-comp'

        if nmz_hc == 'head-comp':
            nmz_head_comp_order = 'head-initial'
            default_nmz_init = '+'
        elif nmz_hc == 'comp-head':
            nmz_head_comp_order = 'head-final'
            default_nmz_init = '-'
        else:
            nmz_head_comp_order = 'either'
            default_nmz_init = 'either'
        
    # Figure out if you need to add anything to manipulate the order head-comp:
    # Either if poss phrase is a head-comp, or if markers are non-affixal (which
    # are always joined by head-comp).
    order_manip = False
    if head_comp_order != strat_order:
        if (mark_loc == 'possessum' or mark_loc == 'both') and mod_spec == 'mod':
            order_manip = True
    if strat.get('possessor-type') == 'non-affix':
        if dep_mark_order:
            if dep_mark_order != head_comp_order:
                order_manip = True
        if strat.get('possessum-type') == 'non-affix':
            if head_mark_order:
                if dep_mark_order != head_mark_order:
                    order_manip = True
    if strat.get('possessum-type') == 'non-affix':
        if head_mark_order:
            if head_mark_order != head_comp_order:
                order_manip = True
    # Checks if the possessum-marking with pronoun case reqs order manipulation
    if strat.get('possessum-mark') == 'yes':
        if strat.get('possessum-mark-type') == 'non-affix':
            if head_mark_order != strat_order or head_mark_order != head_comp_order:
                order_manip = True

    return order_manip, default_init, head_comp_order, default_nmz_init, nmz_head_comp_order

"""
Checks to see if there are any nominalization strategies that use 
the possessive strategies and returns a list of the strategies along
with their semantic types.

arguments: ch = choices file
returns: a list of tuples contains (poss_strategy_name, semantic type)
semantic type can be either 'both', 'verb-only'. 'noun-only'

"""

def check_nom_strats(ch):
    anc_strat = set()
    for strat in ch.get('nmz_poss_strat'):
        anc_strat.add((strat.get('name'), ch.get('non_sent_sem')))
    return anc_strat

def handle_juxt_word_order(mylang, rules, strat, anc_strat, phrase_rule, anc_phrase_rule, noun_phrase_rule):
    if strat.get('order') == 'either':
        mylang.add(noun_phrase_rule+'-head-initial := head-initial & '+ noun_phrase_rule+'.')
        mylang.add(noun_phrase_rule+'-head-final := head-final & '+ noun_phrase_rule+'.')
        rules.add(noun_phrase_rule.replace('-phrase', '') + '-head-initial := '+noun_phrase_rule+'-head-initial.')
        rules.add(noun_phrase_rule.replace('-phrase', '') + '-head-final := '+noun_phrase_rule+'-head-final. ')
        if anc_strat is not None:
                #head-initial and head-final should both be added to these rules in the word-order library
                #since the 'either' option in the adnom-poss library should always coincide with
                #free or v2 word order in nominalized clauses
                mylang.add(anc_phrase_rule+'-head-initial := '+ anc_phrase_rule + '.')
                mylang.add(anc_phrase_rule+'-head-final := ' + anc_phrase_rule + '.')
                rules.add(anc_phrase_rule.replace('-phrase', '') + '-head-initial := '+anc_phrase_rule+'-head-initial.')
                rules.add(anc_phrase_rule.replace('-phrase', '') + '-head-final := '+anc_phrase_rule+'-head-final. ')
    else:
        mylang.add(phrase_rule + ' := '+strat.get('order') +'.', merge=True)
        rules.add(noun_phrase_rule.replace('-phrase', '') + ':= '+noun_phrase_rule+'. ')
        if anc_strat is not None:
            rules.add(anc_phrase_rule.replace('-phrase', '') + ':= '+anc_phrase_rule+'. ') 
            

def add_juxt_semantics(mylang, phrase_rule, noun_phrase_rule, anc_phrase_rule, anc_strat, nmz_wo, has_nmz):
        #add semantic constraints common to both the noun and anc versions of the poss-phrase rule

        common_constraints = '[HEAD-DTR.SYNSEM.LOCAL [ CONT.HOOK #hook & [LTOP #lbl,\
                                                                                 INDEX #possessum & [COG-ST uniq-id]]],\
                                            C-CONT [ HCONS.LIST < qeq & [ HARG #harg, LARG #lbl]  >,\
                                                     RELS.LIST < '+POSSESSUM_EXIST_REL+' > \
                                                     HOOK #hook,\
                                                     ICONS.LIST < > ]].'

        mylang.add(noun_phrase_rule +' := ' + common_constraints)
    
    
        #Add constraints specific to non-derived noun phrases
	#RELS.LIST. (first item on rels list will be the same on sub/supertype)
        mylang.add(noun_phrase_rule + ' := ' + phrase_rule + ' & [ SYNSEM.LOCAL.CAT.VAL.COMPS < >,\
                                                                    NON-HEAD-DTR [SYNSEM.LOCAL [ CAT.VAL.COMPS < >, \
                                                                                                 CONT.HOOK.INDEX #possessor ]] ,\
                                                                    C-CONT [ RELS.LIST < [], '+POSS_REL+' > ]].')
        if anc_strat is None:
            if has_nmz:
                 mylang.add(noun_phrase_rule+ ' := [HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
        else:
            mylang.add(anc_phrase_rule + ' := ' + common_constraints)
            #Syntactic constraints specific to the anc version of the poss-phrase rule
            mylang.add(anc_phrase_rule+ ' := ' + phrase_rule + ' & [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ +].')
            mylang.add(noun_phrase_rule+ ' := [HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')

            mylang.add(anc_phrase_rule+ ' := ' + phrase_rule + ' & [ SYNSEM.LOCAL.CAT [ VAL.COMPS #comps],\
                                                                     HEAD-DTR.SYNSEM.LOCAL [ CAT.VAL [SPR <[LOCAL.CAT.HEAD.POSSESSOR possessive]>,\
                                                                                                      COMPS #comps]]].')
            if nmz_wo in ['ovs','vos', 'sov','svo']:
                mylang.add('anc-' + phrase_rule + ':= [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS & null ].')

            sem_type = anc_strat[1]
            #Add semantic constraints based on whether "noun-only", "verb-only", or "both" semantics was selected
            if sem_type == "noun-only":
                mylang.add(anc_phrase_rule+ ' := ' + phrase_rule + ' & [ NON-HEAD-DTR [SYNSEM.LOCAL.CONT.HOOK.INDEX #possessor],\
                                                                         C-CONT [RELS.LIST < [], '+POSS_REL+' >, ] ].')
            elif sem_type == "verb-only":
                mylang.add(anc_phrase_rule+' := ' + phrase_rule + ' & [ HEAD-DTR.SYNSEM.LOCAL [CAT.VAL.SPR < [LOCAL.CONT.HOOK.INDEX #subj] >],\
                                                                        NON-HEAD-DTR [SYNSEM.LOCAL.CONT.HOOK.INDEX #subj]].')
            elif sem_type == "both":
                mylang.add(anc_phrase_rule+' := ' + phrase_rule + ' & [ HEAD-DTR.SYNSEM.LOCAL [CAT.VAL.SPR < [LOCAL.CONT.HOOK.INDEX #possessor] >],\
                                                                        NON-HEAD-DTR [SYNSEM.LOCAL.CONT.HOOK.INDEX #possessor],\
                                                                        C-CONT [RELS.LIST < [], '+POSS_REL+' >]].')
        

def handle_poss_unary_word_order(mylang, rules, strat, head_spec_order, order_mismatch, anc_strat):
    if strat.get('order') == 'head-initial':
            hs_phrase_name = 'head-spec'
    elif strat.get('order') == 'head-final':
            hs_phrase_name = 'spec-head'

    anc_head_compositional = 'head-compositional &\
                        [ SYNSEM.LOCAL.CAT.HEAD.NMZ +, \
                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR possessive ].'
    
    general_non_head_compositional = 'basic-head-spec-phrase & [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < >].'

    regular_noun_non_head_compositional= '[SYNSEM.LOCAL.CAT.HEAD.NMZ -].'

    anc_non_head_compositional= '[SYNSEM.LOCAL.CAT.HEAD.NMZ +, \
                                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR nonpossessive].'

    if strat.get('order') == 'either':


        #Regular non-head compositional version of the head-spec rule
        mylang.add('non-head-comp-head-spec-phrase := head-spec-phrase &' + general_non_head_compositional)
        mylang.add('non-head-comp-spec-head-phrase := spec-head-phrase &' + general_non_head_compositional)
        
        mylang.add('noun-head-spec-phrase := non-head-comp-head-spec-phrase &' + regular_noun_non_head_compositional)
        mylang.add('noun-spec-head-phrase := non-head-comp-spec-head-phrase &' + regular_noun_non_head_compositional)

        rules.add('noun-head-spec:= noun-head-spec-phrase.')
        rules.add('noun-spec-head:= noun-spec-head-phrase.')

        if anc_strat is not None:
            mylang.add('anc-head-spec-phrase := head-spec-phrase &' + anc_head_compositional)
            mylang.add('anc-spec-head-phrase := spec-head-phrase &' + anc_head_compositional)
            rules.add('anc-head-spec:= anc-head-spec-phrase.')
            rules.add('anc-spec-head:= anc-spec-head-phrase.')

            #Language has determiners
            if head_spec_order == 'head-initial':
                mylang.add('det-anc-head-spec-phrase := non-head-comp-head-spec-phrase &' + anc_non_head_compositional)
                rules.add('det-anc-head-spec:= det-anc-head-spec-phrase.')
            elif head_spec_order == 'head-final':
                mylang.add('det-anc-spec-head-phrase := non-head-comp-spec-head-phrase &' + anc_non_head_compositional)
                rules.add('det-anc-spec-head:= det-anc-spec-head-phrase.')
    

    #Order_mismatch is true whenever noun-det and possessum-possessor word order are different 
    #and noun-det is not none (the language has determiners).
    #In this case another head-spec rule needs to be added to allow for the difference in word order
    #between nouns and determiners and nouns and possessors
    elif order_mismatch:

        #Regular non-head compositional version of the head-spec rule
        mylang.add('non-head-comp-head-spec-phrase := head-spec-phrase &' + general_non_head_compositional)
        mylang.add('non-head-comp-spec-head-phrase := spec-head-phrase &' + general_non_head_compositional)
        
        mylang.add('noun-head-spec-phrase := non-head-comp-head-spec-phrase &' + regular_noun_non_head_compositional)
        mylang.add('noun-spec-head-phrase := non-head-comp-spec-head-phrase &' + regular_noun_non_head_compositional)

        rules.add('noun-head-spec:= noun-head-spec-phrase.')
        rules.add('noun-spec-head:= noun-spec-head-phrase.')

        if anc_strat is not None:
            mylang.add('anc-'+ hs_phrase_name + '-phrase := ' + hs_phrase_name + '-phrase &' + anc_head_compositional)
            rules.add('anc-' + hs_phrase_name + ':= anc-' + hs_phrase_name + '-phrase.')
            
            #Language has determiners
            if head_spec_order == 'head-initial':
                mylang.add('det-anc-head-spec-phrase := non-head-comp-head-spec-phrase &' + anc_non_head_compositional)
                rules.add('det-anc-head-spec:= det-anc-head-spec-phrase.')
            elif head_spec_order == 'head-final':
                mylang.add('det-anc-spec-head-phrase := non-head-comp-spec-head-phrase &' + anc_non_head_compositional)
                rules.add('det-anc-spec-head:= det-anc-spec-head-phrase.')

    #noun-det and noun-possessor order is either the same or noun-det order is none
    else:

        mylang.add('non-head-comp-' + hs_phrase_name + '-phrase := ' + hs_phrase_name + '-phrase &' + general_non_head_compositional)
        mylang.add('noun-' + hs_phrase_name + '-phrase := non-head-comp-' + hs_phrase_name + '-phrase & [ SYNSEM.LOCAL.CAT.HEAD.NMZ -].')
        rules.add('noun-' + hs_phrase_name  + ':= noun-' + hs_phrase_name + '-phrase.')
        if anc_strat is not None:
            mylang.add('anc-' + hs_phrase_name + '-phrase := ' + hs_phrase_name + '-phrase &' + anc_head_compositional)
            rules.add('anc-' + hs_phrase_name + ' := anc-' + hs_phrase_name + '-phrase.')
                
            #Language has determiners
            if head_spec_order != 'none':
                mylang.add('det-anc-' + hs_phrase_name + '-phrase := non-head-comp-' + hs_phrase_name + '-phrase & [ SYNSEM.LOCAL.CAT.HEAD.NMZ +, \
                                                                                                                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.POSSESSOR nonpossessive].')
                rules.add('det-anc-' + hs_phrase_name  + ':= det-anc-' + hs_phrase_name + '-phrase.')
        
def add_poss_unary_semantics(mylang, rules, anc_strat, phrase_rule, noun_phrase_rule, anc_phrase_rule, has_nmz):
    #add semantic constraints common to both the noun and anc versions of the poss-unary phrase rule

    common_constraints = '[SYNSEM [LOCAL [CAT.VAL.SPEC < [ LOCAL.CONT.HOOK [ INDEX #possessum & [ COG-ST uniq-id ], \
				      		      	                      LTOP #lbl ] ] >]], \
                                              C-CONT [ HCONS.LIST < qeq & [ HARG #harg, LARG #lbl ] >, \
                                                       RELS.LIST < '+POSSESSUM_EXIST_REL+' > \
                                                       ICONS.LIST < >   ] ].'
    mylang.add(noun_phrase_rule+' := ' + common_constraints)


    #Add constraints specific to non-derived noun phrases
    mylang.add(noun_phrase_rule + ':= ' + phrase_rule + ' & [SYNSEM [LOCAL [CONT.HOOK #hook,\
                                                                            CAT.VAL.SPEC < [ LOCAL.CONT.HOOK #hook] >]],\
                                              C-CONT [ RELS.LIST < [], '+POSS_REL+' > ],\
                                              ARGS < [ SYNSEM [ LOCAL [ CONT.HOOK.INDEX #possessor ]]] > ].')
    rules.add(noun_phrase_rule.replace('-phrase', '') +' := ' + noun_phrase_rule + '.')

    if anc_strat is None:
        if has_nmz:
            mylang.add(noun_phrase_rule + ':= ' + phrase_rule + ' & [SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.HEAD.NMZ -] > ].')
    else:
        mylang.add(anc_phrase_rule+' := ' + common_constraints)
        rules.add(anc_phrase_rule.replace('-phrase', '') + ' := ' + anc_phrase_rule + '.')
        sem_type = anc_strat[1]
        #Add semantic constraints based on whether "noun-only", "verb-only", or "both" semantics was selected
        if sem_type == "noun-only":
            mylang.add(anc_phrase_rule+' := ' + phrase_rule + ' & [C-CONT [ RELS.LIST < [], '+POSS_REL+' >],\
                                                                   ARGS < [ SYNSEM [ LOCAL [ CONT.HOOK.INDEX #possessor ]]] > ].')
        elif sem_type == "verb-only":
            mylang.add(anc_phrase_rule+' := ' + phrase_rule + ' & [SYNSEM [LOCAL [CONT.HOOK.INDEX #subj]],\
                                                              ARGS < [ SYNSEM [ LOCAL [ CONT.HOOK.INDEX #subj ]]] > ].')
        elif sem_type == "both":
            mylang.add(anc_phrase_rule+' := ' + phrase_rule + ' & [SYNSEM [LOCAL [CONT.HOOK.INDEX #possessor]] ,\
                                                    C-CONT [ RELS.LIST < [], '+POSS_REL+' >],\
                                                    ARGS < [ SYNSEM [ LOCAL [ CONT.HOOK.INDEX #possessor ]]] > ].')
            
        mylang.add(anc_phrase_rule + ':= ' + phrase_rule + ' & [SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.HEAD.NMZ +] > ].')
        mylang.add(noun_phrase_rule + ':= ' + phrase_rule + ' & [SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.HEAD.NMZ -] > ].')
       


def add_spec_semantics(ch, mylang, strat, anc_strat, phrase_rule, poss_strat, order_mismatch, rules, head_spec_order, nmz_wo, has_nmz):
    noun_phrase_rule = "noun-" + phrase_rule
    anc_phrase_rule = "anc-" + phrase_rule
    if poss_strat == "juxt":
        add_juxt_semantics(mylang, phrase_rule, noun_phrase_rule, anc_phrase_rule, anc_strat, nmz_wo, has_nmz)
        handle_juxt_word_order(mylang, rules, strat, anc_strat, phrase_rule, anc_phrase_rule, noun_phrase_rule)
    elif poss_strat == "poss_unary":
        add_poss_unary_semantics(mylang, rules, anc_strat, phrase_rule, noun_phrase_rule, anc_phrase_rule, has_nmz)
        if need_specialized_head_spec(ch):
            handle_poss_unary_word_order(mylang, rules, strat, head_spec_order, order_mismatch, anc_strat)
            
"""
Add the necessary phrase rule to combine possessor and possessum
If rule already exists (head-comp case), then make sure its order is correct.
Also add constraints to non-possessive phrase rules to prevent
them from allowing possessive words in incorrect places
"""


def customize_poss_rules(strat, mylang, ch, rules, hierarchies):

    # Define vars for all elements of strategy:
    strat_name = strat.full_keys()[0].split("_")[0]
    strat_num = strat_name[-1]
    mark_loc = strat.get('mark-loc')
    mod_spec = strat.get('mod-spec')
    pron_allowed = True if strat.get('pronoun-allow') == 'yes' else False
    strat_order = strat.get('order')
    head_mark_order = strat.get('possessum-mark-order')
    dep_mark_order = strat.get('possessor-mark-order')
    if not head_mark_order:
        head_mark_order = strat_order
    if not dep_mark_order:
        dep_mark_order = strat_order
    adj_rule = False
    spec_rule = False
    # Set flags for pronouns
    if 'poss-pron' in strat_name:
        pron_strat = True
        strat_num = 'pron-'+strat_num
        if strat.get('type') == 'affix':
            pron_affix = True
        else:
            pron_affix = False
    else:
        pron_strat = False
        pron_affix = False

    # Add vars to keep track of what rules have been added:
    phrase_rule = ""
    rule_added = False

    # Add vars to keep track of ordering of various types of phrases:
    strat_order = strat.get('order')

    # Start adding rules:
    mylang.set_section('phrases')

    anc_strats =  check_nom_strats(ch)
    anc_strat = None
    for item in anc_strats:
         if strat_name == item[0]:
             anc_strat = item

    nmz_wo = get_nmz_clause_wo(ch)

    # If no marking exists, add one of two juxtaposition rules:
    if mark_loc == 'neither' and not pron_strat:

        # Add general juxtaposition rule:
        rule_added = True
        phrase_rule = 'poss-phrase-'+strat_num
        mylang.add(phrase_rule+JUXTAPOSITION_RULE)
        # Added binary-nonloc-phrase supertype
        mylang.add(phrase_rule + ' := binary-nonloc-phrase.')
        

        # Add constraints to general juxtaposition rule
        if strat.get('mod-spec') == 'spec':
            mylang.add(phrase_rule+' := [ SYNSEM.LOCAL.CAT.VAL.SPR < >, \
                                          HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR <[ ]>].')
        elif strat.get('mod-spec') == 'mod':
            mylang.add(phrase_rule+' := [ SYNSEM.LOCAL.CAT.VAL [COMPS < >, \
                                                                SPR #spr],\
                                          HEAD-DTR.SYNSEM.LOCAL [ CAT [VAL.SPR #spr & <[ ]>],\
                                                                  CONT.HOOK #hook & [ INDEX #possessum,\
                                                                                      LTOP #lbl ]],\
                                          NON-HEAD-DTR [SYNSEM.LOCAL.CONT.HOOK.INDEX #possessor],\
                                          C-CONT [ HOOK #hook & [ INDEX #possessum ],\
                                                   ICONS.LIST < >,\
                                                   RELS.LIST < '+POSS_REL+' >,\
                                                   HCONS.LIST < > ] ].')
            # Add order variation and add rules to rules.tdl:
            if strat_order == 'either':
                mylang.add(
                    phrase_rule+'-head-initial := head-initial & '+phrase_rule+'.')
                mylang.add(phrase_rule+'-head-final := head-final & ' +
                            phrase_rule+'.')
                rules.add(phrase_rule.replace('-phrase', '') +
                            '-head-initial := '+phrase_rule+'-head-initial.')
                rules.add(phrase_rule.replace('-phrase', '') +
                            '-head-final := '+phrase_rule+'-head-final. ')
            else:
                mylang.add(phrase_rule + ' := '+strat.get('order')+'.', merge=True)
                rules.add(phrase_rule.replace(
                    '-phrase', '') + ':= '+phrase_rule+'. ')

        # If both word orders are allowed, then you should add a type to this rule to inherit from:
        if strat_order == 'either':
            mylang.add(phrase_rule+' := binary-headed-phrase.')
        # Add any feature constraints to possessor in juxt construction
        if strat.get('feat'):
            customize_feature_values(
                mylang, ch, hierarchies, strat, phrase_rule, 'poss-juxt-rule')

        if not pron_allowed:
            mylang.add(
                phrase_rule+' := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.PRON - ] ].')

        if strat.get('mod-spec') == 'spec':
            #phrase_rule = 'poss-phrase-'+strat_num
            if anc_strats:
                has_nmz = True
            else:
                has_nmz = False
            add_spec_semantics(ch, mylang, strat, anc_strat, phrase_rule, "juxt", None, rules, None, nmz_wo, has_nmz)
            #Customize the word order of action nominal constructions which mark one argument with a possessive strategy
            if anc_strat is not None:
                customize_nmz_clause_word_order(mylang, ch, rules, nmz_wo, phrase_rule)

    # If possessor isn't an affix pronoun, add a phrase rule
    elif not pron_affix:

        # Check if the existing head-comp rules include the correct order for poss;
        # if not, add a new rule with correct order. Add the INIT feature so that
        # poss head-comp order can be distinguished from the general order.
        hc = customize_major_constituent_order(
            ch.get('word-order'), mylang, ch, rules)['hc']
        order_manip, default_init, head_comp_order, default_nmz_init, nmz_head_comp_order = check_hc_order_manip(
            ch, strat, hc)
        if order_manip:
            # In order to play nice with the wo library, you have to
            # not inherit directly from head-initial and head-final
            # in cases where there's already a head-initial/final-head-nexus
            # rule that a head-comp rule might be inheriting from.
            if ch.get('has-aux') and ch.get('word-order') == 'free':
                hi = 'head-initial-head-nexus'
                hf = 'head-final-head-nexus'
            else:
                hi = 'head-initial'
                hf = 'head-final'

            mylang.add('head :+ [ INIT bool ].', section='addenda')
            init_min = '  [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT - ]'
            init_plus = '  [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT + ]'
            # If the order of head-comps outside this lib is head-initial:
            if head_comp_order == 'head-initial':
                # Add new rule:
                mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & ' +
                           hf+' & '+init_min+'.', section='phrases')
                rules.add('comp-head := comp-head-phrase.')
                # Add INIT to old rule:
                mylang.add('head-comp-phrase := '+init_plus+'.')
                if (ch.get('word-order') == 'free' or ch.get('word-order') == 'v2'):
                    mylang.add('head-comp-phrase-2 := '+init_plus+'.')
                    mylang.add('comp-head-phrase-2 := '+init_min+'.')
            # If the order of head-comps outside this lib is head-final:
            elif head_comp_order == 'head-final':
                # Add new rule:
                mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & '+hi+' &\
                                     '+init_plus+'.', section='phrases')
                rules.add('head-comp := head-comp-phrase.')
                # Add INIT to old rule:
                mylang.add('comp-head-phrase := '+init_min+'.')
                if (ch.get('word-order') == 'free' or ch.get('word-order') == 'v2'):
                    mylang.add('head-comp-phrase-2 := '+init_plus+'.')
                    mylang.add('comp-head-phrase-2 := '+init_min+'.')

            # If general order of head-comps is more restricted, add the correct default INIT
            # value for non-poss lexical items:
            if head_comp_order != 'either':
                for pos in ['tverb', 'aux', 'det', 'cop']:
                    if ch.get(pos) or pos in ['tverb']:
                        name = lexbase.LEXICAL_SUPERTYPES[pos]
                        mylang.add(
                            name + ' := [ SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_init + ' ].', merge=True)
            if ch.get('ns') and nmz_head_comp_order != 'either':
                if default_nmz_init == default_init:
                    mylang.add('anc-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_init + ' ].', merge=True)
                else:
                    for ns in ch.get('ns'):
                        nmz_type = ns.get('nmz_type')
                        if nmz_type == 'sentential' or nmz_type == 'alt-sent':
                            mylang.add('sentential-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_init + ' ].', merge=True)
                        elif nmz_type == 'all-comps':
                             mylang.add('comps-anc-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_init + ' ].', merge=True)
                        elif nmz_type == 'poss-acc':
                            mylang.add('trans-poss-acc-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_nmz_init + ' ].', merge=True)
                        elif nmz_type == 'erg-poss':
                            mylang.add('trans-erg-poss-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_nmz_init + ' ].', merge=True)
                        elif nmz_type == 'nominal':
                            mylang.add('trans-nominal-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_nmz_init + ' ].', merge=True)




        if strat.get('mod-spec') == 'spec':

            # If possessives care about order, then add the correct SPEC_INIT value to nouns.
            # NB: possessors are the only nouns that act as specifiers, so this'll be added
            # directly to the noun supertype, rather than on the possessor lex rules items.
            # Otherwise leave nouns unconstrainted for SPEC_INIT, and they'll go through both:
            mylang.add('head :+ [ SPEC-INIT bool ].', section='addenda')
            if strat_order != 'either':
                spec_init = '+' if strat_order == 'head-final' else '-'
                mylang.add('poss-unary-phrase-'+strat_num +
                           ' := [ SYNSEM.LOCAL.CAT.HEAD.SPEC-INIT ' + spec_init + ' ].', section='phrases')
                # LTX 2022-04-28: Fix issue #598:
                # The line above does not assign a supertype for poss-unary-phrase until
                # the customized_possessor_irules() function is called (i.e., only
                # if this possessive strategy is used in morphology).
                # Therefore, the supertype should be defined even though it's not instantiated
                # in the morphology as below:
                mylang.add('poss-unary-phrase-'+strat_num +
                           ' := poss-unary-phrase.')

            # Check if you need to add any head-spec rules
            head_spec_order = ch.get('noun-det-order')
            if head_spec_order == 'noun-det':
                head_spec_order = 'head-initial'
                default_spec_init = '-'
            elif head_spec_order == 'det-noun':
                head_spec_order = 'head-final'
                default_spec_init = '+'
            else:
                head_spec_order = 'none'

            # Add possessum identification to any preexisting head-spec rule:
            if head_spec_order == 'head-initial':
                mylang.add('head-spec-phrase := '+POSSESSUM_ID_HS)
            elif head_spec_order == 'head-final':
                mylang.add('spec-head-phrase := '+POSSESSUM_ID_HS)

            if strat_order == 'head-initial':
                hs_phrase_name = 'head-spec'
            else:
                hs_phrase_name = 'spec-head'

            has_sent_nmz_rel_strats = False

            for ns in ch.get('ns'):
                if (ns.get('nmz_type') == 'sentential' or   ns.get('nmz_type') == 'alt-sent') and ns.get('nmzRel') == 'yes':
                    has_sent_nmz_rel_strats = True
                    break
            if head_spec_order != strat_order:

                # If no head-spec rule, then just add the correct one(s) here:
                if head_spec_order == 'none':
                    if strat_order != 'either':
                        mylang.add(hs_phrase_name + '-phrase := '+strat_order+' & \
                                [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                                  HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')
                        if not need_specialized_head_spec(ch):
                            mylang.add(hs_phrase_name + '-phrase := basic-head-spec-phrase.')
                            rules.add(hs_phrase_name + ' := ' + hs_phrase_name + '-phrase.')
                        else:
                            mylang.add(hs_phrase_name + '-phrase := basic-head-spec-phrase-super.')
                    else:
                        if not need_specialized_head_spec(ch):
                            mylang.add('spec-head-phrase := basic-head-spec-phrase & head-final.')
                            mylang.add('head-spec-phrase := basic-head-spec-phrase & head-initial.')
                            rules.add('spec-head := spec-head-phrase.')
                            rules.add('head-spec := head-spec-phrase.')
                        else:
                            mylang.add('spec-head-phrase := basic-head-spec-phrase-super.')
                            mylang.add('head-spec-phrase := basic-head-spec-phrase-super.')
                            if nmz_wo not in ['free', 'v2']:
                                mylang.add('spec-head-phrase := head-final.')
                                mylang.add('head-spec-phrase := head-initial.')
                            else:
                                mylang.add('non-head-comp-spec-head-phrase := head-final.')
                                mylang.add('non-head-comp-head-spec-phrase := head-initial.')

                        mylang.add('spec-head-phrase := \
                              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                                HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')
                        mylang.add('head-spec-phrase :=  \
                              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                                HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')


                # If a head-spec rule exists, check its order and adjust accordingly:
                else:
                    # Add the correct default SPEC-INIT value for non-poss lexical items:

                    #commenting out the below line since its always true
                    #head_spec_order only takes the values 'det-noun', 'noun-det', and 'none'
                    #if head_spec_order != 'either':
                    for pos in ['tverb', 'aux', 'det', 'cop']:
                        if ch.get(pos) or pos in ['tverb']:
                            name = lexbase.LEXICAL_SUPERTYPES[pos]
                            mylang.add(
                                name + ' := [ SYNSEM.LOCAL.CAT.HEAD.SPEC-INIT ' + default_spec_init + ' ].', merge=True)

                    # If head-initial rule exists, add head-final and add SPEC-INIT feats to both:
                    if head_spec_order == 'head-initial':
                        if anc_strat is None:
                            mylang.add('spec-head-phrase := basic-head-spec-phrase & head-final.')
                            rules.add('spec-head := spec-head-phrase.')
                        else:
                            mylang.add('spec-head-phrase := basic-head-spec-phrase-super.')
                            if nmz_wo not in ['free', 'wo']:
                                mylang.add('spec-head-phrase := head-final.')
                            else:
                                mylang.add('non-head-comp-spec-head-phrase := head-final.')

                        mylang.add(
                            'head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.SPEC-INIT - ].')
                        mylang.add('spec-head-phrase := \
                                    [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.SPEC-INIT +,\
                                                                      VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ] ,\
                                      HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')


                    # If head-final rule exists, add head-initial and add SPEC-INIT feats to both:
                    if head_spec_order == 'head-final':
                        if anc_strat is None:
                            mylang.add('head-spec-phrase := basic-head-spec-phrase & head-initial.')
                            rules.add('head-spec := head-spec-phrase.')
                        else:
                            mylang.add('head-spec-phrase := basic-head-spec-phrase-super.')
                            if nmz_wo not in ['free', 'wo']:
                                mylang.add('head-spec-phrase := head-initial.')
                            else:
                                mylang.add('non-head-comp-head-spec-phrase:= head-initial.')
                                
                        mylang.add(
                            'spec-head-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.SPEC-INIT + ].')
                        mylang.add('head-spec-phrase :=\
                                         [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.SPEC-INIT -,\
                                                                           VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ] ,\
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')

            mylang.add('poss-unary-phrase'+POSS_UNARY)
            if has_sent_nmz_rel_strats:
                mylang.add('poss-unary-phrase := [ARGS < [SYNSEM.LOCAL [CONT.HOOK.INDEX ref-ind]] >] .')
            if head_spec_order != strat_order and head_spec_order != "none":
                order_mismatch = True
            else:
                order_mismatch = False
	    #phrase_rule = 'poss-unary-phrase-'+strat_num
            if anc_strats:
                has_nmz = True
            else:
                has_nmz = False
            add_spec_semantics(ch, mylang, strat, anc_strat, 'poss-unary-phrase-'+strat_num, "poss_unary", order_mismatch, rules,head_spec_order, nmz_wo, has_nmz)
            #Customize the word order of action nominal constructions which mark one argument with a possessive strategy
            if anc_strat is not None:
                customize_nmz_clause_word_order(mylang, ch, rules, nmz_wo, hs_phrase_name)


        elif strat.get('mod-spec') == 'mod':

            #            if strat.get('mark-loc')=='possessum' or strat.get('mark-loc')=='both':
            #                pass

            # Add head-mod rule
            if strat.get('mark-loc') != 'possessum' and strat.get('mark-loc') != 'both':
                mylang.add('possessum-mod-rule := basic-head-mod-phrase-simple & [\
                                    NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.MOD.FIRST.LOCAL.CAT.POSSESSUM #poss ],\
                                    HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss ].')
                if strat_order == 'head-initial':
                    mylang.add('head-adj-int-phrase :+ possessum-mod-rule.')
                    rules.add('head-adj-int := head-adj-int-phrase.')
                elif strat_order == 'head-final':
                    mylang.add('adj-head-int-phrase :+ possessum-mod-rule.')
                    rules.add('adj-head-int := adj-head-int-phrase.')
                else:
                    mylang.add('head-adj-int-phrase :+ possessum-mod-rule.')
                    mylang.add('adj-head-int-phrase :+ possessum-mod-rule.')
                    rules.add('head-adj-int := head-adj-int-phrase.')
                    rules.add('adj-head-int := adj-head-int-phrase.')

                clmod_pos = ''
                for cms in ch.get('cms', []):
                    # If this is the first strategy, then just store its info
                    if clmod_pos == '':
                        if cms.get('position') == 'after':
                            clmod_pos = 'head-adj'
                        elif cms.get('position') == 'before':
                            clmod_pos = 'adj-head'
                        elif cms.get('position') == 'either':
                            clmod_pos = 'either'

                    # If this isn't the first strategy, check if you need to
                    # update from one order to two orders
                    if clmod_pos == 'adj-head':
                        if cms.get('position') == 'after' or cms.get('position') == 'either':
                            clmod_pos = 'either'
                    if clmod_pos == 'head-adj':
                        if cms.get('position') == 'before' or cms.get('position') == 'either':
                            clmod_pos = 'either'

                if clmod_pos == 'head-adj' or clmod_pos == 'either':
                    mylang.add('head-adj-scop-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                                                 POSSESSUM nonpossessive  ] ].', section='addenda')
                if clmod_pos == 'adj-head' or clmod_pos == 'either':
                    mylang.add('adj-head-scop-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                                                 POSSESSUM nonpossessive  ] ].', section='addenda')

#########################################################################################
# Add inflectional rules
#########################################################################################

# Adds inflectional rules (or adds constraints to inflectional rules added in
# morphotactics.py) that create possessive forms


def customize_poss_irules(strat, mylang, ch, irules, hierarchies, rules):

    # Define vars for all elements of strategy:
    strat_name = strat.full_keys()[0].split("_")[0]
    strat_num = strat_name[-1]
    mark_loc = strat.get('mark-loc')
    mod_spec = strat.get('mod-spec')
    possessor_type = strat.get('possessor-type')
    possessum_type = strat.get('possessum-type')

    # Set flag for pronouns
    if 'poss-pron' in strat_name:
        pron_strat = True
    else:
        pron_strat = False

    # Go through the position class (pc) info till you find the strategy you're actually dealing with
    for pc in all_position_classes(ch):
        for lrt in pc.get('lrt', []):
            for feat in lrt['feat']:
                # For non-pronoun strategies:
                if not pron_strat:

                    if strat_name == str(feat['name']) and feat['value'] != 'nonpossessive':

                        mylang.set_section('lexrules')

                        # Add possessor-inflecting rules:
                        if (mark_loc == 'possessor' or mark_loc == 'both') and possessor_type == 'affix' and feat['value'] == 'possessor':

                            customize_possessor_irules(
                                strat, mylang, rules, ch, strat_num, mod_spec, mark_loc, hierarchies)

                        # Add possessum-inflecting rules
                        if (mark_loc == 'possessum' or mark_loc == 'both') and possessum_type == 'affix' and feat['value'] == 'possessum':

                            customize_possessum_irules(
                                strat, mylang, rules, ch, strat_num, mod_spec, mark_loc, possessum_type, hierarchies)

                # Add irules for pronoun strategies:
                elif pron_strat:
                    anc_strats = check_nom_strats(ch)
                    if anc_strats:
                        only_pron_strat = True
                        for name, sem in anc_strats:
                            if 'poss-strat' in name:
                                only_pron_strat = False
                                break
                        if only_pron_strat:
                            customize_nmz_clause_word_order(mylang, ch, rules, get_nmz_clause_wo(ch), '')
                    customize_possessor_pron_irules(
                        mylang, strat_name, feat, lrt, mod_spec, anc_strats, strat_num)


def customize_possessor_irules(strat, mylang, rules, ch, strat_num, mod_spec, mark_loc, hierarchies):

    case = True if ch.get('case-marking') != 'none' else False

    # Add the basic possessor rule defn:
    possessor_rule_name = 'possessor-lex-rule-'+strat_num

    # Add case constraints if case exists:
    if case:

        mylang.add('poss-case := case.', section='addenda')
        mylang.add(possessor_rule_name +
                   ' := [ SYNSEM.LOCAL.CAT.HEAD.CASE poss-case ].')

    # Add constraints to possessor rule for spec version
    if mod_spec == 'spec':
        agr_prefix = 'SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

        # Add constraints to spec version for single marking
        if mark_loc == 'possessor' or mark_loc == 'both':

            mylang.add(possessor_rule_name+' := head-change-only-lex-rule & \
            [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+' ] ].', merge=True)

            mylang.add('poss-unary-phrase-'+strat_num+' := poss-unary-phrase &\
                               [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD [ POSSESSOR possessor-'+strat_num+' ] ] > ].')

        # Add case constraints if case exists:
#        if case:
#            mylang.add('poss-case := case.',section='addenda')
#            mylang.add(possessor_rule_name+' := [ SYNSEM.LOCAL.CAT.HEAD.CASE poss-case ].')

        # If the possessor is the only marked constituent, forbid marking on the possessum:
        if mark_loc == 'possessor':

            mylang.add('poss-unary-phrase-'+strat_num +
                       ' := poss-unary-phrase & [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM nonpossessive ] ].')

    # Add constraints to possessor rule for mod version
    elif mod_spec == 'mod':

        # Set posthead flag on possessors acting as mods
        if strat.get('order') == 'head-initial':
            ph = '+'
        elif strat.get('order') == 'head-final':
            ph = '-'
        else:
            ph = 'bool'
        # Add constraints to mod version for single marking
        if mark_loc == 'possessor':
            agr_prefix = 'SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

            mylang.add(possessor_rule_name+' := head-change-with-ccont-lex-rule & \
                        [ SYNSEM.LOCAL.CAT [ POSTHEAD '+ph+',\
                                             HEAD [ MOD.FIRST [ LOCAL [ CAT [ VAL.SPR <[]>,\
                                                                        HEAD +np & [ PRON - ] ], \
                                                                        CONT.HOOK [ INDEX #possessum, \
                                                                                    LTOP #lbl ] ], \
                                                                OPT - ],\
                                                    POSSESSOR possessor-'+strat_num+' ], \
                                             VAL #val ] ,\
                          C-CONT [ HOOK #hook,\
                                   RELS.LIST < '+POSS_REL+' >,\
                                   HCONS.LIST < >, \
                                   ICONS.LIST < >  ], \
                          DTR.SYNSEM.LOCAL [ CONT.HOOK #hook & [ INDEX #possessor ],\
                                             CAT.VAL #val  ] ].', merge=True)

        # Add constraints to mod version for double marking
        elif mark_loc == 'both':

            mylang.add(possessor_rule_name+' := add-only-no-ccont-rule &\
                              [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                     VAL #val ] ] ,\
                                DTR.SYNSEM.LOCAL [ CAT.VAL #val ] ].')

    # If an agreement strategy is indicated, identify POSS-AGR with PNG of possessum

    if strat.get('possessor-affix-agr') == 'agree':

        # Note: we don't do this in the mod-like both-marking scenario -- possessor is a COMP and has no access to
        # possessum. In this scenario, the identification must be done on the possessum inflection.
        if not (mark_loc == 'both' and mod_spec == 'mod'):
            if mod_spec == 'mod':
                mylang.add(possessor_rule_name+' := [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                                                               '+agr_prefix+' #png ].')
            elif mod_spec == 'spec':
                mylang.add('poss-unary-phrase-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png ] >,\
                                                                                      '+agr_prefix+' #png ].', section='phrases')


def customize_possessum_irules(strat, mylang, rules, ch, strat_num, mod_spec, mark_loc, possessum_type, hierarchies):

    # Add general possessum-marking rule:
    possessum_rule_name = 'possessum-lex-rule-'+strat_num

    # Add constraints to possessor rule for spec version
    if mod_spec == 'spec':

        mylang.add(possessum_rule_name+POSSESSUM_RULE)

        agr_prefix = 'SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

        # Add constraints in for single marking and double marking:
        if mark_loc == 'possessum' or mark_loc == 'both':

            mylang.add(possessum_rule_name+':=  cat-change-only-lex-rule & \
                         [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                              VAL #val & [ SPEC < >,\
                                                           SPR < [ LOCAL.CAT [ VAL.SPR < >,\
                                                                               HEAD.POSSESSOR possessor-'+strat_num+' ] ] > ]  ] ,\
                            C-CONT [ HCONS.LIST < >, \
                                     ICONS.LIST < >,\
                                     RELS.LIST < > ],\
                            DTR.SYNSEM.LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                     HEAD.PRON -,\
                                                     VAL #val ] ] ].', merge=True)

            mylang.add('poss-unary-phrase-'+strat_num +
                       ' := poss-unary-phrase & [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ] ].', section='phrases')

            # If possessor isn't marked, don't let it be possessive
            if mark_loc == 'possessum':
                mylang.add('poss-unary-phrase-'+strat_num+' := poss-unary-phrase &\
                               [ ARGS < [ SYNSEM.LOCAL.CAT [ HEAD [ POSSESSOR nonpossessive ],\
                                                             POSSESSUM nonpossessive ] ] > ] .')


            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            instance_tmp = {}
            if strat.get('possessor-feat'):
                for key in list(strat.keys()):
                    new_key = key.replace('feat', 'skip')
                    new_key = new_key.replace('possessor-skip', 'feat')
                    instance_tmp[new_key] = strat.get(key)
                customize_feature_values(
                    mylang, ch, hierarchies, instance_tmp, 'poss-unary-phrase-'+strat_num, 'possessum-spec-mark')

        # Add constraints to spec version for case where possessum is marked
        # and possessor = pronoun
        if mark_loc == 'possessum-with-pron':
            mylang.add(possessum_rule_name+':=  cat-change-only-lex-rule & \
                         [ SYNSEM.LOCAL.CAT [ HEAD #head,\
                                              POSSESSUM possessum-'+strat_num+',\
                                              VAL [ SPEC #spec, COMPS #comps,\
                                                    SPR < [ LOCAL [ CAT [ VAL.SPR < >,\
                                                                          HEAD +nd ] ] ] > ] ] ,\
                            DTR.SYNSEM.LOCAL [ CAT [ HEAD #head & [ PRON - ],\
                                                     VAL [ COMPS #comps, SPEC #spec ] ] ] ].', merge=True)

    # Add constraints to possessor rule for mod version
    if mod_spec == 'mod':

        agr_prefix = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        mylang.add(possessum_rule_name+POSSESSUM_RULE)

        if mark_loc == 'possessum' or mark_loc == 'both':
            mylang.add(possessum_rule_name+':= val-change-with-ccont-lex-rule & \
                            [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                                 VAL [ SPEC #spec, SPR #spr, \
                                                       COMPS.FIRST [ OPT - ,\
                                                                     LOCAL [ CAT cat-sat & [ HEAD +np,\
                                                                                             VAL.SPR < > ], \
                                                                             CONT.HOOK [ INDEX #possessor ] ] ] ] ],\
                              C-CONT [ HOOK #hook ,\
                                       RELS.LIST < '+POSS_REL+' >,\
                                       HCONS.LIST < >,\
                                       ICONS.LIST < >  ],\
                              DTR.SYNSEM.LOCAL [ CAT [ HEAD.PRON -,\
                                                       VAL [ SPR #spr, SPEC #spec ] ],\
                                                 CONT.HOOK #hook & [ INDEX #possessum,\
                                                                     LTOP #lbl ] ] ].', merge=True)

            # Check if you need to add INIT to the possessum to keep it from going through wrong ordered head-comps
            hc = customize_major_constituent_order(
                ch.get('word-order'), mylang, ch, rules)['hc']
            order_manip, default_init, head_comp_order,_,_ = check_hc_order_manip(
                ch, strat, hc)
            if order_manip:
                init = '+' if strat.get('order') == 'head-initial' else '-'
                if strat.get('order') != 'either':
                    mylang.add(possessum_rule_name +
                               ' := [ SYNSEM.LOCAL.CAT.HEAD.INIT '+init+' ].')

            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            instance_tmp = {}
            if strat.get('possessor-feat'):
                for key in list(strat.keys()):
                    new_key = key.replace('feat', 'skip')
                    new_key = new_key.replace('possessor-skip', 'feat')
                    instance_tmp[new_key] = strat.get(key)
                customize_feature_values(
                    mylang, ch, hierarchies, instance_tmp, possessum_rule_name, 'possessum-mod-mark')

        # Add constraints to mod version for double marking
        if mark_loc == 'both':
            mylang.add(possessum_rule_name+' :=\
               [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD +np & [ POSSESSOR possessor-'+strat_num+' ] ].')

        # Add constraints to spec version for case where possessum is marked
        # and possessor = pronoun
        if mark_loc == 'possessum-with-pron':
            mylang.add(possessum_rule_name+':= cat-change-only-lex-rule & \
                                                       [ SYNSEM.LOCAL.CAT [ HEAD #head,\
                                                                            POSSESSUM possessum-'+strat_num+',\
                                                                            VAL [ SPEC #spec, SPR #spr ] ],\
                                                         C-CONT [ HOOK #hook ,\
                                                                  RELS.LIST < >,\
                                                                  HCONS.LIST < >,\
                                                                  ICONS.LIST < > ],\
                                                         DTR.SYNSEM.LOCAL [ CONT.HOOK #hook,\
                                                                            CAT [ HEAD #head & [ PRON - ],\
                                                                                  VAL [ SPR #spr, SPEC #spec ] ] ] ].', merge=True)

    # Add agreement features to the possessum affix
    if strat.get('possessum-affix-agr') == 'agree':
        if mod_spec == 'mod':
            mylang.add(possessum_rule_name+' := [ SYNSEM.LOCAL.CAT.POSSESSUM.POSS-AGR #png,\
                                                             '+agr_prefix+' #png ].')
        elif mod_spec == 'spec':
            mylang.add('poss-unary-phrase-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png-um ] >,\
                                                             SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png-um ].', section='phrases')

    # Note: in the mutual agreement, double marking mod-like scenario, the possessor is a COMP.
    # Therefore, it has no access to the possessum's PNG info. When the possessor agrees with
    # the possessum, therefore, all agreement must be done in the possessum-inflecting rule:
    if mark_loc == 'both' and mod_spec == 'mod' and strat.get('possessor-affix-agr') == 'agree':
        if possessum_type == 'affix':
            mylang.add(possessum_rule_name+' :=\
            [ SYNSEM.LOCAL [ CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #poss-png,\
                                                          CONT.HOOK.INDEX.PNG #poss-png ] ].')


def customize_possessor_pron_irules(mylang, strat_name, feat, lrt, mod_spec, anc_strats, strat_num):
    has_anc = False
    anc_strat = False

    if len(anc_strats) != 0:
        has_anc = True
        for item in anc_strats:
            if item[0] == strat_name:
                sem_type = item[1]

    if feat['value'] == 'ANC':
        anc_strat = True

    spec_lrt_syn_constraints = ' := \
                            [SYNSEM.LOCAL.CAT [HEAD #head,\
                               VAL [ SPR < >,\
                                     SPEC #spec,\
                                     SUBJ #subj,\
                                     COMPS #comps]],\
             DTR.SYNSEM.LOCAL [CAT [HEAD #head & [ PRON - ],\
                                     VAL [ SPEC #spec & < >,\
                                             SUBJ #subj,\
                                             COMPS #comps]]]].'
    
    spec_lrt_sem_constraints_noun_only = ' := \
               [ C-CONT [ HOOK #hook,\
                          RELS.LIST  < noun-relation &\
                                [ PRED "pron_rel",\
                                  LBL #lbl2,\
                                  ARG0 #possessor & [ COG-ST activ-or-more,\
                                                      SPECI + ] ],\
                                '+POSSESSUM_EXIST_REL+',\
                                '+POSS_REL+',\
                                quant-relation &\
                               [ PRED "exist_q_rel",\
                                 ARG0 #possessor,\
                                 RSTR #harg2 ] >,\
                          HCONS.LIST < qeq & [ HARG #harg,\
                                           LARG #lbl ],\
                                   qeq & [ HARG #harg2,\
                                           LARG #lbl2 ] > ],\
                 DTR.SYNSEM.LOCAL [ CONT.HOOK #hook & [ INDEX #possessum & [ COG-ST activ-or-more ],\
                                                                             LTOP #lbl ] ] ].'
    spec_lrt_sem_constraints_both = ' := \
               [ C-CONT [ HOOK #hook,\
                          RELS.LIST  < noun-relation &\
                                [ PRED "pron_rel",\
                                  LBL #lbl2,\
                                  ARG0 #possessor & [ COG-ST activ-or-more,\
                                                      SPECI + ] ],\
                                '+POSSESSUM_EXIST_REL+',\
                                '+POSS_REL+',\
                                quant-relation &\
                               [ PRED "exist_q_rel",\
                                 ARG0 #possessor,\
                                 RSTR #harg2 ] >,\
                          HCONS.LIST < qeq & [ HARG #harg,\
                                           LARG #lbl ],\
                                   qeq & [ HARG #harg2,\
                                           LARG #lbl2 ] > ],\
                 DTR.SYNSEM.LOCAL[ CAT.VAL.SPR < [LOCAL.CONT.HOOK.INDEX #possessor] >,\
                                   CONT.HOOK #hook & [ INDEX #possessum & [ COG-ST activ-or-more ],\
                                                                            LTOP #lbl ] ] ].'
    spec_lrt_sem_constraints_verb_only = ' := \
               [ C-CONT [ HOOK #hook,\
                          RELS.LIST  < noun-relation &\
                                [ PRED "pron_rel",\
                                  LBL #lbl2,\
                                  ARG0 #possessor & [ COG-ST activ-or-more,\
                                                      SPECI + ] ],\
                                '+POSSESSUM_EXIST_REL+',\
                                quant-relation &\
                               [ PRED "exist_q_rel",\
                                 ARG0 #possessor,\
                                 RSTR #harg2 ] >,\
                          HCONS.LIST < qeq & [ HARG #harg,\
                                           LARG #lbl ],\
                                   qeq & [ HARG #harg2,\
                                           LARG #lbl2 ] > ],\
                 DTR.SYNSEM.LOCAL[ CAT.VAL.SPR < [LOCAL.CONT.HOOK.INDEX #possessor] >,\
                                   CONT.HOOK #hook & [ INDEX #possessum & [ COG-ST activ-or-more ],\
                                                                            LTOP #lbl ] ] ].'
                 
    if strat_name in str(feat['name']) and feat['value'] != 'minus':
        # Add constraints to pronoun affix rule for spec version
        if mod_spec == 'spec':
            if not anc_strat:
                #Add all syntactic constraints
                mylang.add(get_name(lrt)+ '-lex-rule '+ spec_lrt_syn_constraints)

                #Add semantic constraints for non-derived nouns
                mylang.add(get_name(lrt)+'-lex-rule ' + spec_lrt_sem_constraints_noun_only)

                if has_anc:
                    mylang.add(get_name(lrt)+'-lex-rule := \
               [ DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
                    
            else:
                #Add all syntactic constraints
                mylang.add(get_name(lrt) + '-lex-rule '+ spec_lrt_syn_constraints)
                mylang.add(get_name(lrt)+ '-lex-rule '+ ':= \
               [ DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ + ].' )
                
                if sem_type == 'noun-only':
                    mylang.add(get_name(lrt) + '-lex-rule '+ spec_lrt_sem_constraints_noun_only)
                elif sem_type == 'both':
                    mylang.add(get_name(lrt)+  '-lex-rule '+ spec_lrt_sem_constraints_both)
                elif sem_type == 'verb-only':
                    mylang.add(get_name(lrt)+ '-lex-rule '+ spec_lrt_sem_constraints_verb_only)

        # Add constraints to pronoun affix rule for mod version
        elif mod_spec == 'mod':

            # TODO: change COG-ST on pron to activ-or-more, and adjust any tests as needed.

            mylang.add(get_name(lrt)+'-lex-rule :=\
              [ SYNSEM.LOCAL.CAT.HEAD #head ,\
                DTR.SYNSEM.LOCAL [ CAT.HEAD #head ,\
                                CONT.HOOK #hook & [ INDEX #possessum & [ COG-ST activ-or-more ],\
                                                  LTOP #lbl] ],\
              C-CONT.HOOK #hook ].')
            mylang.add(get_name(lrt)+'-lex-rule := \
               [ SYNSEM.LOCAL.CAT.VAL #val,\
                 DTR.SYNSEM.LOCAL.CAT [ HEAD.PRON -,\
                                        VAL #val ],\
                 C-CONT [ RELS.LIST  < noun-relation &\
                                  [ PRED "pron_rel",\
                                    LBL #lbl2,\
                                    ARG0 #possessor & [ COG-ST activ-or-more,\
                                                        SPECI + ] ],\
                                  '+POSS_REL+',\
                                  quant-relation &\
                                 [ PRED "exist_q_rel",\
                                   ARG0 #possessor,\
                                   RSTR #harg2 ] >,\
                          HCONS.LIST < qeq & [ HARG #harg2,\
                                           LARG #lbl2 ] > ] ].')


#########################################################################################
# Add lexical items
#########################################################################################

# Adds lexical items for possession markers and possessor pronouns.
# All needed phrase rules added in customize_poss_rules() above.
def customize_poss_lexicon(strat, mylang, ch, lexicon, rules, hierarchies):

    # Define vars for all elements of strategy:
    strat_name = strat.full_keys()[0].split("_")[0]
    strat_num = strat_name[-1]
    mark_loc = strat.get('mark-loc')
    mod_spec = strat.get('mod-spec')
    possessor_type = strat.get('possessor-type')
    possessum_type = strat.get('possessum-type')
    pron_allowed = True if strat.get('pronoun-allow') == 'yes' else False

    if 'poss-pron' in strat_name:
        pron_strat = True
    else:
        pron_strat = False

    # Add lexical items other than poss pronouns:
    if not pron_strat:

        # Add possessor-marking adpositons:
        if (mark_loc == 'possessor' or mark_loc == 'both') and possessor_type == 'non-affix':

            customize_possessor_lexicon(strat, mylang, ch, lexicon, strat_name,
                                        strat_num, mod_spec, mark_loc, pron_allowed, hierarchies, rules)

        # Add possessum-marking nouns:
        if (mark_loc == 'possessum' or mark_loc == 'both') and possessum_type == 'non-affix':

            customize_possessum_lexicon(strat, mylang, ch, lexicon, strat_name, strat_num,
                                        mod_spec, mark_loc, pron_allowed, possessor_type, hierarchies, rules)

    elif pron_strat:

        customize_possessor_pron_lexicon(
            strat, mylang, ch, lexicon, strat_name, strat_num, mod_spec, hierarchies, rules)


def customize_possessor_lexicon(strat, mylang, ch, lexicon, strat_name, strat_num, mod_spec, mark_loc, pron_allowed, hierarchies, rules):

    case = True if ch.get('case-marking') != 'none' else False

    # Add most general defn of possessor-marking adp:
    mylang.set_section('otherlex')
    mylang.add(TWO_REL_ADP)
    mylang.add('possessor-adp-lex-'+strat_num+' '+POSSESSOR_ADP_LEX)

    # Make sure no other adps are going to show up where possessor adp should:
    mylang.add('basic-adposition-lex :+ [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                             POSSESSUM nonpossessive ] ].')

    # Check if ordering info needs to be added to adp
    hc = customize_major_constituent_order(
        ch.get('word-order'), mylang, ch, rules)['hc']
    order_manip, default_init, head_comp_order,_,_ = check_hc_order_manip(
        ch, strat, hc)
    if order_manip:
        marker_order = strat.get('possessor-mark-order')
        init = 'bool'
        if marker_order == 'either':
            init = 'bool'
        elif marker_order == 'head-initial':
            init = '+'
        elif marker_order == 'head-final':
            init = '-'

        mylang.add('possessor-adp-lex-'+strat_num +
                   ' := [ SYNSEM.LOCAL.CAT.HEAD.INIT '+init+' ].')

    # Optionally block PRON + items from being possessors
    if not pron_allowed:
        mylang.add('possessor-adp-lex-'+strat_num +
                   ' := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.PRON - ].')

    # Add constraints to possessor adp for spec version
    if mod_spec == 'spec':

        mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [  SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                       POSSESSUM nonpossessive ] ].')
        mylang.add('possessor-adp-lex-'+strat_num+' := \
                [  SYNSEM.LOCAL [ CAT.HEAD.MOD < >,\
                                  CONT [ RELS.LIST < >,\
                                         HCONS.LIST < > ] ] ].')

        mylang.add('poss-unary-phrase-'+strat_num +
                   ' := poss-unary-phrase & [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD [ POSSESSOR possessor-'+strat_num+' ] ] > ].', section='phrases')

        # If the possessor is the only marked constituent, forbid marking on the possessum:
        if mark_loc == 'possessor':
            mylang.add('poss-unary-phrase-'+strat_num +
                       ' := [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM nonpossessive ] ].', section='phrases')

    # Add constraints to possessor adp for mod version
    if mod_spec == 'mod':

        # Set posthead flag on possessors acting as mods
        if strat.get('order') == 'head-initial':
            ph = '+'
        elif strat.get('order') == 'head-final':
            ph = '-'
        else:
            ph = 'bool'

        mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT [ POSTHEAD '+ph+',\
                                                        HEAD [ POSSESSOR possessor-'+strat_num+',\
                                                             MOD.FIRST.LOCAL [ CAT [ HEAD noun & [ PRON - ],\
                                                                                     VAL.SPR < [ ] > ] ] ] ],\
                                                  CONT.HCONS.LIST < > ] ] .')

        # Add constraints to mod version for single-marking:
        if mark_loc == 'possessor':
            mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT [ VAL [ COMPS.FIRST.LOCAL [ CONT.HOOK.INDEX #possessor ] ],\
                                                         HEAD.MOD.FIRST.LOCAL [ CAT.HEAD noun, '
                                                      '                         CONT.HOOK [ INDEX #possessum,\
                                                                                            LTOP #lbl ] ] ],\
                                                  CONT [ RELS.LIST < '+POSS_REL+' > ] ] ].')

        # Add constraints to mod version for double-marking:
        elif mark_loc == 'both':
            mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                        POSSESSUM nonpossessive, ]\
                                                  CONT.RELS.LIST < > ] ] .')

    if case:

        mylang.add('+np :+ [ CASE case ].', section='addenda')
        mylang.add('poss-case := case.', section='addenda')

        mylang.add('possessor-adp-lex-'+strat_num +
                   ' := [ SYNSEM.LOCAL.CAT.HEAD.CASE poss-case ].')

    # Add agreement features to the possessor adp if appropriate:
    # TODO: these lex items don't follow nomenclature conventions yet:
    if strat.get('possessor-agr') == 'non-agree':

        # Add non-agreeing adp to lexicon:
        orth = strat.get('possessor-orth')
        lexicon.add('possessor-adp-'+strat_num+' := possessor-adp-lex-'+strat_num+' &\
                                                      [ STEM < "'+orth+'" >].')

    # Add agreeing adps to mylang and lexicon:
    elif strat.get('possessor-agr') == 'agree':

        # Set feature path for agreement features
        for form in strat.get('possessor-form'):
            if mod_spec == 'spec':
                agr_prefix = 'SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            elif mod_spec == 'mod':
                if mark_loc == 'possessor':
                    agr_prefix = 'SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
                elif mark_loc == 'both':
                    agr_prefix = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

            # Add agreeing forms to mylang
            orth = form.get('agr-orth')
            adp_type = adp_id(form)

            # If mod, agreement constraint goes on adp
            if mod_spec == 'mod':
                mylang.add(adp_type+' := possessor-adp-lex-'+strat_num+' &\
                                        [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                        '+agr_prefix+' #png ].')

                customize_feature_values(
                    mylang, ch, hierarchies, form, adp_type, 'possessor-marker')

            # If spec, agreement constraint goes on poss-unary-phrase:
            elif mod_spec == 'spec':
                mylang.add('poss-unary-phrase-'+strat_num+' := \
                          [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png-or,\
                            ARGS < [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png-or ] > ].', section='phrases')
                mylang.add(adp_type+' := possessor-adp-lex-'+strat_num+'.')
                customize_feature_values(
                    mylang, ch, hierarchies, form, adp_type, 'possessor-marker')

            # Add agreeing adps to lexicon:
            orth = form.get('agr-orth')
            lexicon.add(adp_type.replace('-lex', '')+' := '+adp_type+' &\
                                         [ STEM < "'+orth+'" > ].')

    # Add any necessary constraints to the complement of the possessor-marking word:
    instance_tmp = {}
    if strat.get('dep-comp-feat'):
        for key in list(strat.keys()):
            new_key = key.replace('feat', 'skip')
            new_key = new_key.replace('dep-comp-skip', 'feat')
            instance_tmp[new_key] = strat.get(key)
        customize_feature_values(
            mylang, ch, hierarchies, instance_tmp, 'possessor-adp-lex-'+strat_num, 'poss-adp-comp')
        
    #If a language has nominalized verbs, set possessor-adp-lex to have the same NMZ value as its complement.
    if ch.get('ns', ''):
        mylang.add('possessor-adp-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT [ HEAD.NMZ #nmz \
                                                                VAL.COMPS < [LOCAL.CAT.HEAD.NMZ #nmz] >]].')
        if needs_anc_wo_feat(ch):
                 mylang.add('possessor-adp-lex-'+strat_num+ ' := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO - ].')


def customize_possessum_lexicon(strat, mylang, ch, lexicon, strat_name, strat_num, mod_spec, mark_loc, pron_allowed, possessor_type, hierarchies, rules):
    mylang.set_section('nounlex')

    # Check if ordering info needs to be added to adp
    init = 'bool'
    hc = customize_major_constituent_order(
        ch.get('word-order'), mylang, ch, rules)['hc']
    order_manip, default_init, head_comp_order,_,_ = check_hc_order_manip(
        ch, strat, hc)
    if order_manip:
        marker_order = strat.get('possessum-mark-order')
        if marker_order == 'either':
            init = 'bool'
        elif marker_order == 'head-initial':
            init = '+'
        elif marker_order == 'head-final':
            init = '-'

    # Add spec-version (only version in this case)
    if mod_spec == 'spec':

        mylang.add('poss-unary-phrase-'+strat_num +
                   ' := poss-unary-phrase & [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ] ].', section='phrases')

        # Add constraints to spec version for single marking
        if mark_loc == 'possessum':

            mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX)
            mylang.add('possessum-noun-lex-'+strat_num+' := \
                          [ SYNSEM.LOCAL [ CAT [ VAL [ SPEC < >, SPR < [ LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ] > ],\
                                           HEAD [ POSSESSOR nonpossessive ],\
                                           POSSESSUM possessum-'+strat_num+' ] ] ].', merge=True)

            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            if strat.get('possessor-feat'):  # TODO change to checking if possessor is marked
                instance_tmp = {}
                if strat.get('possessor-feat'):
                    for key in list(strat.keys()):
                        new_key = key.replace('feat', 'skip')
                        new_key = new_key.replace('possessor-skip', 'feat')
                        instance_tmp[new_key] = strat.get(key)
                    customize_feature_values(
                        mylang, ch, hierarchies, instance_tmp, 'poss-unary-phrase-'+strat_num, 'possessum-spec-mark')

        # Add constraints to spec version for double marking
        if mark_loc == 'both':

            mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX)
            mylang.add('possessum-noun-lex-'+strat_num+' := \
                                             [ SYNSEM.LOCAL [ CAT [ VAL [ SPEC < >, SPR < [ LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ] > ],\
                                                                    HEAD [ POSSESSOR nonpossessive ],\
                                                                    POSSESSUM possessum-'+strat_num+' ] ] ].', merge=True)

        if mark_loc == 'possessum-with-pron':

            mylang.add('possessum-noun-lex-'+strat_num +
                       ' '+POSSESSUM_NOUN_LEX_W_PRON)
            mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL [ CAT [ VAL [ SPEC < >, SPR < [ LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ] > ],\
                                                                                   HEAD [ POSSESSOR nonpossessive ],\
                                                                                          POSSESSUM possessum-'+strat_num+' ] ] ].', merge=True)
        if order_manip:

            mylang.add('possessum-noun-lex-'+strat_num +
                       ' := [ SYNSEM.LOCAL.CAT.HEAD.INIT '+init+' ].')

        #If a language has nominalized verbs, the possessum noun marker should be NMZ -, ADV-MOD - just like all other nouns
        if ch.get('ns', ''):
            mylang.add('possessum-noun-lex-'+strat_num + ' := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
            if ch.get('adv', ''):
                mylang.add('possessum-noun-lex-'+strat_num +' := [ SYNSEM.LOCAL.CAT.HEAD.ADV-MOD - ].')
            if needs_anc_wo_feat(ch):
                 mylang.add( 'possessum-noun-lex-'+strat_num + ' := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO - ].')
                
        

    if mod_spec == 'mod':

        mylang.add('head :+ [ INIT bool ].', section='addenda')

        possessor_constr = '& [ POSSESSOR possessor-' + \
            strat_num+' ]' if mark_loc == 'both' else ''

        if mark_loc != 'possessum-with-pron':

            mylang.add('possessum-noun-lex-'+strat_num+' := non-local-none-lex-item &\
                          [ SYNSEM.LOCAL [ CAT [ POSSESSUM possessum-'+strat_num+',\
                                                 HEAD noun & [ POSSESSOR nonpossessive,\
                                                               INIT '+init+' ],\
                                                 VAL [ SPEC < >, COMPS < #possessum-comp & [ OPT -,\
                                                                                   LOCAL [ CONT.HOOK #hook &\
                                                                                                   [ INDEX #possessum,\
                                                                                                     LTOP #lbl ]  ,\
                                                                                          CAT [ VAL.SPR #spr & < [ ] >,\
                                                                                                HEAD noun & [ POSSESSOR nonpossessive ],\
                                                                                                POSSESSUM nonpossessive ] ] ],\
                                                             #possessor-comp & [ OPT -,\
                                                                                 LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                                                               VAL [ SPR <  >, COMPS < > ],\
                                                                                               HEAD +np '+possessor_constr+' ],\
                                                                                         CONT.HOOK.INDEX #possessor ] ] >,\
                                                      SPR #spr ] ],\
                                           CONT [ HOOK #hook,\
                                                  RELS.LIST < '+POSS_REL+' >,\
                                                  HCONS.LIST < >,\
                                                  ICONS.LIST < >  ] ],\
                            ARG-ST < #possessum-comp, #possessor-comp > ].')
        else:

            # When the possessum is marked and the possessor = pron, then the pron is the modifier of the possessum:
            mylang.add('possessum-noun-lex-'+strat_num+' := non-local-none-lex-item &\
                          [ SYNSEM.LOCAL [ CAT [ POSSESSUM possessum-'+strat_num+',\
                                                 HEAD noun & [ POSSESSOR nonpossessive,\
                                                              INIT '+init+' ],\
                                                 VAL [ SPEC < >, COMPS < #possessum-comp & [ OPT -,\
                                                                                   LOCAL [ CONT.HOOK #hook  & [ INDEX #possessum ] ,\
                                                                                           CAT [ VAL.SPR #spr & < [ ] >,\
                                                                                                HEAD noun & [ POSSESSOR nonpossessive ],\
                                                                                                POSSESSUM nonpossessive ] ] ] >,\
                                                      SPR #spr ] ],\
                                           CONT [ HOOK #hook & [ INDEX #possessum ] ,\
                                                  RELS.LIST < >,\
                                                  HCONS.LIST < >,\
                                                  ICONS.LIST < >  ] ],\
                            ARG-ST < #possessum-comp > ].')
        # Add any feature constraints to the possessor (only if the possessor is unmarked)
        instance_tmp = {}
        if strat.get('possessor-feat'):
            for key in list(strat.keys()):
                new_key = key.replace('feat', 'skip')
                new_key = new_key.replace('possessor-skip', 'feat')
                instance_tmp[new_key] = strat.get(key)
            customize_feature_values(mylang, ch, hierarchies, instance_tmp,
                                     'possessum-noun-lex-'+strat_num, 'possessum-mod-mark2')
            
  
    # Add agreement features where appropriate
    if strat.get('possessum-agr') == 'non-agree':
        orth = strat.get('possessum-orth')
        # Add single, non-agreeing form to lexicon
        lexicon.add('possessum-noun-'+strat_num+' := possessum-noun-lex-'+strat_num+' &\
                                                  [ STEM < "'+orth+'" >].')

    elif strat.get('possessum-agr') == 'agree':
        for form in strat.get('possessum-form'):
            # Set feature paths to agreement feats:
            if mod_spec == 'spec':
                prefix = 'SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            elif mod_spec == 'mod':
                prefix = 'SYNSEM.LOCAL.CAT.VAL.COMPS.REST.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            noun_type = noun_id(form)

            if mod_spec == 'mod':

                # If mod and appearing w pronoun, then pronoun handles agr
                if mark_loc == 'possessum-with-pron':
                    pron_type = noun_id(strat)
                    mylang.add(pron_type+' :=\
                              [ SYNSEM.LOCAL [ CAT.HEAD.MOD.FIRST.LOCAL.CAT [ HEAD noun, POSSESSUM.POSS-AGR #head-png ],\
                                               CONT.HOOK.INDEX.PNG #head-png ] ].')
                    mylang.add(
                        noun_type+' := possessum-noun-lex-'+strat_num+'.')

                else:
                    # If mod and not w pronoun, then agreeing happens on possessum marker:
                    mylang.add(noun_type+' := possessum-noun-lex-'+strat_num+' &\
                          [ SYNSEM.LOCAL.CAT.POSSESSUM.POSS-AGR #png,\
                                '+prefix+' #png ].')

            # If spec, then agreeing happens on poss-unary-rule:
            elif mod_spec == 'spec':
                mylang.add(noun_type+' := possessum-noun-lex-'+strat_num+'.')
                mylang.add('poss-unary-phrase-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png-um ] >,\
                                                                 SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png-um ].', section='phrases')

            customize_feature_values(
                mylang, ch, hierarchies, form, noun_type, 'possessum-marker')

            orth = form.get('agr-orth')
            # Add appropriate number of agreeing forms to lexicon
            lexicon.add(noun_type.replace('-lex', '')+' := '+noun_type+' &\
                                         [ STEM < "'+orth+'" > ].')


def customize_possessor_pron_lexicon(strat, mylang, ch, lexicon, strat_name, strat_num, mod_spec, hierarchies, rules):

    case = True if ch.get('case-marking') != 'none' else False

    # Set vars for pron strat:
    noun_type = noun_id(strat)
    agr = True if strat.get('agr') == 'agree' else False

    # Add general form of pronoun:
    mylang.set_section('nounlex')
    mylang.add(noun_type+POSSESSOR_PRON_LEX)

    #If a language has nominalized verbs, possessive pronouns should be NMZ -, ADV-MOD - just like all other nouns
    if ch.get('ns', ''):
        mylang.add(noun_type +' := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
        if ch.get('adv', ''):
            mylang.add(noun_type +' := [ SYNSEM.LOCAL.CAT.HEAD.ADV-MOD - ].')

    # Add constraints for spec version:
    if mod_spec == 'spec':
        agr_prefix = 'SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

        mylang.add(noun_type+' := \
                        [ SYNSEM.LOCAL [ CAT [ HEAD [ MOD < >, POSSESSOR possessor-pron-'+strat_num+'],\
                                               VAL.SPEC < > ],\
                                         CONT [ RELS.LIST  < #altkeyrel >,\
                                                  HCONS.LIST < > ] ] ].')

        mylang.add('poss-unary-phrase-pron-'+strat_num +
                   ' := poss-unary-phrase & [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD [ POSSESSOR possessor-pron-'+strat_num+' ] ] > ].', section='phrases')

        if agr:
            mylang.add('poss-unary-phrase-pron-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png ] >,\
                                                           '+agr_prefix+' #png ].', section='phrases')

    # Add constraints for mod version
    elif mod_spec == 'mod':

        # Set posthead flag on possessors acting as mods
        if strat.get('order') == 'head-initial':
            ph = '+'
        elif strat.get('order') == 'head-final':
            ph = '-'
        else:
            ph = 'bool'

        agr_prefix = 'SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        mylang.add(noun_type+' := \
                        [ SYNSEM.LOCAL [ CAT [ POSTHEAD '+ph+',\
                                               HEAD [ POSSESSOR possessor-pron-'+strat_num+',\
                                                    MOD < [ OPT -,\
                                                            LOCAL [ CAT [ HEAD noun & [ PRON - ],\
                                                                          VAL [ SPR < [ ] >,\
                                                                                COMPS < > ] ],\
                                                                    CONT.HOOK [ INDEX #possessum,\
                                                                                LTOP #lbl ] ] ] > ] ],\
                                         CONT [ RELS.LIST  <  '+POSS_REL+',\
                                                           #altkeyrel >,\
                                                HCONS.LIST < > ] ] ].')

        if agr:
            mylang.add(noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                              '+agr_prefix+' #png ].')

    if case:

        mylang.add('poss-case := case.', section='addenda')
        mylang.add(noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.CASE poss-case ].')

    # Add forms to lexicon.tdl:
    for pron_inst in strat.get('instance'):
        orth = pron_inst.get('orth')
        instance_name = noun_id(pron_inst)
        mylang.add(instance_name+' := '+noun_type+'.')
        customize_feature_values(
            mylang, ch, hierarchies, pron_inst, instance_name, 'noun')
        lexicon.add(instance_name.replace('-lex', '')+' := '+instance_name+' &\
                                                          [ STEM < "'+orth+'" > ].')

        # Add agr features where appropriate
        instance_tmp = {}
        # Add PNG agr features:
        for key in list(pron_inst.keys()):
            # Relabel the inherent features as something else ('skip')
            # Relabel the agreement features as simply features ('feat')
            # Then call customize_feature_values() with the 'poss-marker' setting
            # so that the agreement features are added at POSS.POSS-AGR instead of HOOK.INDEX.PNG

            new_key = key.replace('feat', 'skip')
            new_key = new_key.replace('agr-skip', 'feat')

            instance_tmp[new_key] = pron_inst.get(key)

        # TODO: Figure out how to cast instance_tmp from a dict to a ChoiceDict so that no future
        #  developers have to deal with this mess in features.py
        customize_feature_values(
            mylang, ch, hierarchies, instance_tmp, instance_name, 'possessor-marker')

        # Add non-PNG features (only enabled for mod-like prons):
        instance_tmp = {}
        for key in list(pron_inst.keys()):
            new_key = key.replace('feat', 'skip')
            new_key = new_key.replace('non-png-skip', 'feat')
            instance_tmp[new_key] = pron_inst.get(key)

        if strat.get('mod-spec') == 'mod':
            customize_feature_values(
                mylang, ch, hierarchies, instance_tmp, instance_name, 'poss-pron-mod')

    # Add any necessary markings to the possessum:
    if strat.get('possessum-mark') == 'yes':

        # Make possessor pron req a marked possessum:
        if mod_spec == 'spec':
            mylang.add('poss-unary-phrase-pron-'+strat_num +
                       ' := [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM possessum-pron-'+strat_num+' ].', section='phrases')

        if mod_spec == 'mod':
            mylang.add(
                noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT [ HEAD noun, POSSESSUM possessum-pron-'+strat_num+'] ] > ].')

        # Add affixal markings:
        if strat.get('possessum-mark-type') == 'affix':

            customize_possessum_irules(strat, mylang, rules, ch, 'pron-' +
                                       strat_num, mod_spec, 'possessum-with-pron', 'affix', hierarchies)

            if strat.get('possessum-mark-affix-agr') == 'agree':
                if mod_spec == 'spec':

                    mylang.add('poss-unary-phrase-pron-'+strat_num+' := \
                              [ ARGS < [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png ] >,\
                                SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png ].', section='phrases')

                if mod_spec == 'mod':
                    mylang.add(noun_type+' := \
                              [ SYNSEM.LOCAL [ CONT.HOOK.INDEX.PNG #png,\
                                               CAT.HEAD.MOD.FIRST.LOCAL.CAT [ HEAD noun, POSSESSUM.POSS-AGR #png ] ] ].')

        if strat.get('possessum-mark-type') == 'non-affix':

            customize_possessum_lexicon(strat, mylang, ch, lexicon, strat_name, 'pron-'+strat_num,
                                        mod_spec, 'possessum-with-pron', True, 'non-affix', hierarchies, rules)
    else:
        # If the possessor is the only marked constituent in a spec construction, forbid marking on the possessum:
        if mod_spec == 'spec':
            mylang.add('poss-unary-phrase-pron-'+strat_num +
                       ' := [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM nonpossessive ] ].', section='phrases')
