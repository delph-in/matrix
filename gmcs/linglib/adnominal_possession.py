import gmcs.tdl
from gmcs.lib import TDLHierarchy
import gmcs.utils
from gmcs.utils import get_name
from gmcs.choices import ChoiceDict
from gmcs.linglib.word_order import customize_major_constituent_order
from gmcs.linglib.morphotactics import all_position_classes
from gmcs.linglib.features import customize_feature_values
from gmcs.linglib.lexical_items import adp_id, noun_id
from gmcs.linglib import lexbase

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

POSSESSUM_ID_HS=' [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                    HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].'

POSSESSUM_RULE=' :=\
                  [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR nonpossessive ].'


JUXTAPOSITION_RULE=' := [ SYNSEM.LOCAL [ CAT [ HEAD #head,\
                                              VAL [ COMPS < >,\
                                                    SUBJ < >, \
                                                    SPEC < > ] ] ],\
                           C-CONT [ HOOK #hook & [ INDEX #possessum ],\
                                    ICONS <! !>],\
                           HEAD-DTR.SYNSEM.LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                         HEAD #head & noun & [ POSSESSOR nonpossessive,\
                                                                               PRON - ] ],\
                                                   CONT.HOOK #hook & [ INDEX #possessum,\
                                                               LTOP #lbl ] ],\
                           NON-HEAD-DTR.SYNSEM.LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                             HEAD noun & [ POSSESSOR nonpossessive ],\
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
                                 [  SYNSEM.LOCAL [ CAT [ VAL [ SPEC < >,\
                                                               SUBJ < >,\
                                                               SPR < >,\
                                                               COMPS.FIRST [ LOCAL.CAT [ HEAD noun ,\
                                                                                          VAL.SPR < > ],\
                                                                              OPT - ] ] ],\
                                                  CONT [ ICONS <! !>   ] ] ].'

POSSESSUM_NOUN_LEX=':= basic-one-arg &\
                                   [ SYNSEM.LOCAL [ CAT [ HEAD #head & noun ,\
                                                          VAL [ SUBJ < >,\
                                                                SPR < [ LOCAL.CAT.HEAD det ] >,\
                                                                COMPS < #comps & [ LOCAL [ CONT.HOOK #hook,\
                                                                                           CAT [ POSSESSUM nonpossessive,\
                                                                                                 VAL.SPR <[ ]>,\
                                                                                                 HEAD #head & [ PRON - ,\
                                                                                                                POSSESSOR nonpossessive ] ] ] ] > ] ],\
                                                    CONT [ RELS <! !>,\
                                                           HCONS <! !>,\
                                                           HOOK #hook,\
                                                           ICONS <! !> ] ],\
                                     ARG-ST < #comps > ].'

POSSESSUM_NOUN_LEX_W_PRON=':= basic-two-arg &\
                                   [ SYNSEM.LOCAL [ CAT [ HEAD #head & noun ,\
                                                          VAL [ SUBJ < >,\
                                                                SPR < #spr & [ LOCAL [ CAT [ VAL.SPR < > ] ] ] >,\
                                                                COMPS < #comps & [ LOCAL [ CONT.HOOK #hook,\
                                                                                           CAT [ VAL.SPR <[ ]>,\
                                                                                                 HEAD #head & [ PRON - ] ] ] ] > ] ],\
                                                    CONT [ RELS <! !>,\
                                                           HCONS <! !>,\
                                                           HOOK #hook,\
                                                           ICONS <! !> ] ],\
                                     ARG-ST < #spr, #comps > ].'

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

NON_POSS_LEX_ITEM = '[ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                     POSSESSUM nonpossessive ] ].'

POSS_UNARY = ' := basic-unary-phrase & \
              [ SYNSEM.LOCAL [ COORD -,\
                               CONT.HOOK #hook,\
                               CAT [ HEAD det & [ POSSESSOR possessor ],\
  	                 	   VAL [ SPR < >,\
                                         COMPS < >,\
			                 SUBJ < >,\
		     	                 SPEC < [ LOCAL [ CAT [ VAL.COMPS < > ,\
                                                                HEAD noun & [ PRON - ] ],\
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
                ARGS < [ SYNSEM.LOCAL [ COORD -,\
                                        CAT [ VAL [ SPR < >,\
       	       		      	                    COMPS < >,\
				                    SUBJ < >,\
                                                    SPEC < > ],\
      	   		      	              HEAD +np ],\
  			                CONT.HOOK.INDEX #possessor ] ] > ].'



##################################################################
## Primary function (called from customize.py)                 ###
################################################################## 
def customize_adnominal_possession(mylang,ch,rules,irules,lexicon,hierarchies):
    # Check if any possessive strategies or pronouns are defined.
    # If so, add the POSS head feature.
    if 'poss-strat' in ch or 'poss-pron' in ch:
        customize_poss_addenda(mylang, ch)

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
    mylang.add('basic-bare-np-phrase :+ [ SYNSEM.LOCAL.CAT [ VAL.SPEC < >,\
                                                             HEAD #head,\
                                                             POSSESSUM #possessum],\
                                          HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD #head,\
                                                             POSSESSUM #possessum & nonpossessive ] ].',section='addenda')
    mylang.add('basic-head-1st-comp-phrase :+ [ \
                  SYNSEM.LOCAL.CAT.POSSESSUM #poss,\
                  HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss ].',section='addenda')


    # Set nouns to default nonpossessive behavior if no affixal strategies exist:
    poss_strat_types=set()
    for strat in ch.get('poss-strat'):
        poss_strat_types.add(strat.get('possessor-type'))
        poss_strat_types.add(strat.get('possessum-type'))
        poss_strat_types.add(strat.get('possessum-mark-type'))
    if 'affix' not in poss_strat_types:
        mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                     POSSESSUM nonpossessive ] ].',section='nounlex')


def customize_poss_hier(mylang,strat_num):
    hier = TDLHierarchy('possession')
    hier.add('possessive','poss','supertype for possessive features')
    hier.add('nonpossessive','poss','type for nonpossessive')
    hier.add('possessor','possessive','')
    hier.add('possessum','possessive','')
    hier.add('possessive-'+str(strat_num),'possessive','supertype for strategy '+str(strat_num))
    hier.add('possessor-'+str(strat_num),'possessor & possessive-'+str(strat_num),'')
    hier.add('possessum-'+str(strat_num),'possessum & possessive-'+str(strat_num),'')
    hier.save(mylang)


"""
Calls customize_poss_rules, customize_poss_irules, and customize_poss_lexicon
to build possessive strategies for cases where the possessor is a full NP
"""
def customize_np_possession(mylang,ch,rules,irules,lexicon,hierarchies):
    for strat_num, strat in enumerate(ch.get('poss-strat', []), 1):
        strat_num = str(strat_num)

        # Add subtypes of POSSESSOR and POSSESSUM features for this strategy
        customize_poss_hier(mylang,strat_num)

        # Add phrase rules:
        customize_poss_rules(strat,mylang,ch,rules,hierarchies,strat_num,False)

        # Add inflectional rules:
        if strat.get('possessor-type')=='affix' or strat.get('possessum-type')=='affix':

            customize_poss_irules(strat,mylang,ch,irules,hierarchies,rules,strat_num,False)

        # Add lexical items:
        if strat.get('possessor-type')=='non-affix' or strat.get('possessum-type')=='non-affix':
            customize_poss_lexicon(strat,mylang,ch,lexicon,rules,hierarchies,strat_num,False)


"""
Calls customize_poss_rules, customize_poss_irules, and customize_poss_lexicon
to build possessive strategies for cases where the possessor is a pronoun
"""
def customize_pronominal_possession(mylang,ch,rules,irules,lexicon,hierarchies):

    for pron_num, pron in enumerate(ch.get('poss-pron', []), 1):
        pron_num = str(pron_num)

        # Add possessive features:
        customize_poss_hier(mylang,'pron-'+pron_num)

        # Add phrase rules:        
        customize_poss_rules(pron,mylang,ch,rules,hierarchies,pron_num,True)

        # Add inflectional rules:
        if pron.get('type')=='affix':
            customize_poss_irules(pron,mylang,ch,irules,hierarchies,rules,pron_num,True)

        # Add lexical rules:
        if pron.get('type')=='non-affix':            
            customize_poss_lexicon(pron,mylang,ch,lexicon,rules,hierarchies,pron_num,True)


#########################################################################################
# Add phrase rules
#########################################################################################

"""
 Helper function to determine if you'll need to
 manipulate the order of head-comp rules:
"""
def check_hc_order_manip(strat,hc):

    # Order of major poss phrase
    strat_order=strat.get('order')
    # Order of minor phrase consisting of mark + possessor/um 
    dep_mark_order=strat.get('possessor-mark-order')
    head_mark_order=strat.get('possessum-mark-order')
    mark_loc=strat.get('mark-loc')
    mod_spec=strat.get('mod-spec')
    # Order of head-comps that already exists:
    if hc=='head-comp':
        head_comp_order='head-initial'
        default_init='+'
    elif hc=='comp-head':
        head_comp_order='head-final'
        default_init='-'
    else:
        head_comp_order='either'
        default_init='either'

    # Figure out if you need to add anything to manipulate the order head-comp:
    # Either if poss phrase is a head-comp, or if markers are non-affixal (which
    # are always joined by head-comp).
    order_manip=False
    if head_comp_order != strat_order:
        if (mark_loc=='possessum' or mark_loc=='both') and mod_spec=='mod':
            order_manip=True
    if strat.get('possessor-type')=='non-affix':
        if dep_mark_order:
            if dep_mark_order != head_comp_order:
                order_manip=True
        if strat.get('possessum-type')=='non-affix':
            if head_mark_order:
                if dep_mark_order != head_mark_order:
                    order_manip=True
    if strat.get('possessum-type')=='non-affix':
        if head_mark_order:
            if head_mark_order != head_comp_order:
                order_manip=True
    # Checks if the possessum-marking with pronoun case reqs order manipulation
    if strat.get('possessum-mark')=='yes':
        if strat.get('possessum-mark-type')=='non-affix':
            if head_mark_order != strat_order or head_mark_order != head_comp_order:
                order_manip=True

    return order_manip,default_init,head_comp_order


"""
Add the necessary phrase rule to combine possessor and possessum
If rule already exists (head-comp case), then make sure its order is correct.
Also add constraints to non-possessive phrase rules to prevent
them from allowing possessive words in incorrect places
"""
def customize_poss_rules(strat,mylang,ch,rules,hierarchies,strat_num,pron_strat):

    # Define vars for all elements of strategy:
    mark_loc=strat.get('mark-loc')
    mod_spec=strat.get('mod-spec')
    pron_allowed=True if strat.get('pronoun-allow')=='yes' else False
    strat_order=strat.get('order')
    head_mark_order=strat.get('possessum-mark-order')
    dep_mark_order=strat.get('possessor-mark-order')
    if not head_mark_order:
        head_mark_order=strat_order
    if not dep_mark_order:
        dep_mark_order=strat_order
    adj_rule=False
    spec_rule=False
    # Set flags for pronouns 
    if pron_strat:
        strat_num='pron-'+strat_num
        if strat.get('type')=='affix':
            pron_affix=True
        else:
            pron_affix=False
    else:
        pron_affix=False

    # Add vars to keep track of what rules have been added:
    phrase_rule=""
    rule_added=False

    # Add vars to keep track of ordering of various types of phrases:
    strat_order=strat.get('order')

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

        # Add order variation and add rules to rules.tdl:
        if strat_order=='either':
            mylang.add(phrase_rule+'-head-initial := head-initial & '+phrase_rule+'.')
            mylang.add(phrase_rule+'-head-final := head-final & '+phrase_rule+'.')
            rules.add(phrase_rule.replace('-phrase','')+'-head-initial := '+phrase_rule+'-head-initial.'  )
            rules.add(phrase_rule.replace('-phrase','')+'-head-final := '+phrase_rule+'-head-final. ' )
        else:
            mylang.add(phrase_rule +' := '+strat.get('order')+'.',merge=True)
            rules.add(phrase_rule.replace('-phrase','') + ':= '+phrase_rule+'. ' )

        
        #If both word orders are allowed, then you should add a type to this rule to inherit from:
        if strat_order=='either': 
            mylang.add(phrase_rule+' := binary-headed-phrase.')

        # Add any feature constraints to possessor in juxt construction
        if strat.get('feat'):
            customize_feature_values(mylang,ch,hierarchies,strat,phrase_rule,'poss-juxt-rule')

        if not pron_allowed:
            mylang.add(phrase_rule+' := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.PRON - ] ].')

    # If possessor isn't an affix pronoun, add a phrase rule
    elif not pron_affix:

        # Check if the existing head-comp rules include the correct order for poss; 
        # if not, add a new rule with correct order. Add the INIT feature so that
        # poss head-comp order can be distinguished from the general order.
        hc=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']
        order_manip,default_init,head_comp_order=check_hc_order_manip(strat,hc)
        if order_manip:
            # In order to play nice with the wo library, you have to 
            # not inherit directly from head-initial and head-final 
            # in cases where there's already a head-initial/final-head-nexus
            # rule that a head-comp rule might be inheriting from.
            if ch.get('has-aux') and ch.get('word-order')=='free':
                hi='head-initial-head-nexus'
                hf='head-final-head-nexus'
            else:
                hi='head-initial'
                hf='head-final'

            mylang.add('head :+ [ INIT bool ].', section='addenda')
            init_min = '  [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT - ]' 
            init_plus = '  [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT + ]'
            # If the order of head-comps outside this lib is head-initial:
            if head_comp_order=='head-initial':
                # Add new rule:
                mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & '+hf+' & '+init_min+'.',section='phrases')
                rules.add('comp-head := comp-head-phrase.')
                # Add INIT to old rule:
                mylang.add('head-comp-phrase := '+init_plus+'.')
                if (ch.get('word-order')=='free' or ch.get('word-order')=='v2'): 
                    mylang.add('head-comp-phrase-2 := '+init_plus+'.')
                    mylang.add('comp-head-phrase-2 := '+init_min+'.')
            # If the order of head-comps outside this lib is head-final:
            elif head_comp_order=='head-final':
                # Add new rule:
                mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & '+hi+' &\
                                     '+init_plus+'.',section='phrases')
                rules.add('head-comp := head-comp-phrase.')
                # Add INIT to old rule:
                mylang.add('comp-head-phrase := '+init_min+'.')
                if (ch.get('word-order')=='free' or ch.get('word-order')=='v2'): 
                    mylang.add('head-comp-phrase-2 := '+init_plus+'.')
                    mylang.add('comp-head-phrase-2 := '+init_min+'.')

            # If general order of head-comps is more restricted, add the correct default INIT 
            # value for non-poss lexical items:
            if head_comp_order!='either':
                for pos in ['tverb','aux','det','cop']:
                    if ch.get(pos) or pos in ['tverb']:
                        name = lexbase.LEXICAL_SUPERTYPES[pos]
                        mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_init + ' ].', merge=True)

        if strat.get('mod-spec')=='spec':

            # If possessives care about order, then add the correct SPEC_INIT value to nouns. 
            # NB: possessors are the only nouns that act as specifiers, so this'll be added
            # directly to the noun supertype, rather than on the possessor lex rules items.
            # Otherwise leave nouns unconstrainted for SPEC_INIT, and they'll go through both:
            mylang.add('head :+ [ SPEC-INIT bool ].', section='addenda')
            if strat_order!='either':
                spec_init='+' if strat_order=='head-final' else '-'
                mylang.add('poss-unary-phrase-'+strat_num+' := [ SYNSEM.LOCAL.CAT.HEAD.SPEC-INIT ' + spec_init + ' ].',section='phrases')

            # Check if you need to add any head-spec rules
            head_spec_order=ch.get('noun-det-order')
            if head_spec_order=='noun-det':
                head_spec_order='head-initial'
                default_spec_init='-'
            elif head_spec_order=='det-noun':
                head_spec_order='head-final'
                default_spec_init='+'
            else:
                head_spec_order='none'
    
            # Add possessum identification to any preexisting head-spec rule:
            mylang.add('head-spec-phrase := '+POSSESSUM_ID_HS)

            if head_spec_order!=strat_order:

                # If no head-spec rule, then just add the correct one(s) here:
                if head_spec_order=='none':
                    if strat_order!='either':
                        mylang.add('head-spec-phrase := '+strat_order+' &  basic-head-spec-phrase &\
                              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                                HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')
                        rules.add('head-spec := head-spec-phrase.')
                    else:
                        mylang.add('head-spec-phrase := head-final &  basic-head-spec-phrase &\
                              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                                HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')
                        rules.add('head-spec := head-spec-phrase.')
                        mylang.add('head-spec-phrase-2 := head-initial & basic-head-spec-phrase &\
                              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ,\
                                HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')
                        rules.add('head-spec-2 := head-spec-phrase-2.')

                # If a head-spec rule exists, check its order and adjust accordingly:
                else:

                    # Add the correct default SPEC-INIT value for non-poss lexical items:
                    if head_spec_order!='either':
                        for pos in ['tverb','aux','det','cop']:
                            if ch.get(pos) or pos in ['tverb']:
                                name = lexbase.LEXICAL_SUPERTYPES[pos]
                                mylang.add(name + ' := [ SYNSEM.LOCAL.CAT.HEAD.SPEC-INIT ' + default_spec_init + ' ].', merge=True)

                    # If head-initial rule exists, add head-final and add SPEC-INIT feats to both:
                    if head_spec_order=='head-initial':
                        mylang.add('head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.SPEC-INIT - ].')
                        mylang.add('head-spec-phrase-2 := head-final &  basic-head-spec-phrase &\
                                    [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.SPEC-INIT +,\
                                                                      VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ] ,\
                                      HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')

                        rules.add('head-spec-2 := head-spec-phrase-2.')
                    # If head-final rule exists, add head-initial and add SPEC-INIT feats to both:
                    if head_spec_order=='head-final':
                        mylang.add('head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.SPEC-INIT + ].')
                        mylang.add('head-spec-phrase-2 := head-initial & basic-head-spec-phrase &\
                                         [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.SPEC-INIT -,\
                                                                           VAL.SPEC < [ LOCAL.CAT.POSSESSUM #poss ] > ] ,\
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.POSSESSUM #poss  ].')
                        rules.add('head-spec-2 := head-spec-phrase-2.')
                    
            mylang.add('poss-unary-phrase'+POSS_UNARY)
            mylang.add('basic-determiner-lex :+ '+NON_POSS_LEX_ITEM, section = 'addenda')
            
        elif strat.get('mod-spec')=='mod':

#            if strat.get('mark-loc')=='possessum' or strat.get('mark-loc')=='both':
#                pass

            # Add head-mod rule
            if strat.get('mark-loc')!='possessum' and strat.get('mark-loc')!='both':
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

                if clmod_pos=='head-adj' or clmod_pos=='either':
                    mylang.add('head-adj-scop-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                                                 POSSESSUM nonpossessive  ] ].',section='addenda')
                if clmod_pos=='adj-head' or clmod_pos=='either':
                    mylang.add('adj-head-scop-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive,\
                                                                                 POSSESSUM nonpossessive  ] ].',section='addenda')

#########################################################################################
# Add inflectional rules
#########################################################################################

# Adds inflectional rules (or adds constraints to inflectional rules added in
# morphotactics.py) that create possessive forms
def customize_poss_irules(strat,mylang,ch,irules,hierarchies,rules,strat_num,pron_strat):

    # Define vars for all elements of strategy:
    strat_name=('poss-pron' if pron_strat else 'poss-strat') + strat_num
    mark_loc=strat.get('mark-loc')
    mod_spec=strat.get('mod-spec')
    possessor_type=strat.get('possessor-type')
    possessum_type=strat.get('possessum-type')

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

                            customize_possessor_irules(strat,mylang,rules,ch,strat_num,mod_spec,mark_loc,hierarchies)

                        # Add possessum-inflecting rules
                        if (mark_loc=='possessum' or mark_loc=='both') and possessum_type=='affix' and feat['value']=='possessum':

                            customize_possessum_irules(strat,mylang,rules,ch,strat_num,mod_spec,mark_loc,possessum_type,hierarchies)

                # Add irules for pronoun strategies:
                elif pron_strat:
                    
                    customize_possessor_pron_irules(strat,mylang,ch,strat_name,feat,lrt,mod_spec,hierarchies)

def customize_possessor_irules(strat,mylang,rules,ch,strat_num,mod_spec,mark_loc,hierarchies):

    # Add the basic possessor rule defn:
    possessor_rule_name ='possessor-lex-rule-'+strat_num

    # Add case constraints if case exists:
    if ch.get('case-marking')!='none':
        
        mylang.add('poss-case := case.',section='addenda')
        mylang.add(possessor_rule_name+' := [ SYNSEM.LOCAL.CAT.HEAD.CASE poss-case ].')
    
    # Add constraints to possessor rule for spec version
    if mod_spec=='spec':
        agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

        # Add constraints to spec version for single marking
        if mark_loc=='possessor' or mark_loc=='both':

            mylang.add(possessor_rule_name+' := head-change-only-lex-rule & \
            [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+' ] ].',merge=True)

            mylang.add('poss-unary-phrase-'+strat_num+' := poss-unary-phrase &\
                               [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD [ POSSESSOR possessor-'+strat_num+' ] ] > ].')
            rules.add('poss-unary-'+strat_num+' := poss-unary-phrase-'+strat_num+'.')
        
        # Add case constraints if case exists:
#        if case:
#            mylang.add('poss-case := case.',section='addenda')
#            mylang.add(possessor_rule_name+' := [ SYNSEM.LOCAL.CAT.HEAD.CASE poss-case ].')

        # If the possessor is the only marked constituent, forbid marking on the possessum:
        if mark_loc=='possessor':

            mylang.add('poss-unary-phrase-'+strat_num+' := poss-unary-phrase & [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM nonpossessive ] ].')

    # Add constraints to possessor rule for mod version
    elif mod_spec=='mod':
        
        # Set posthead flag on possessors acting as mods
        if strat.get('order')=='head-initial':
            ph='+'
        elif strat.get('order')=='head-final':
            ph='-'
        else:
            ph='bool'
        # Add constraints to mod version for single marking
        if mark_loc=='possessor':
            agr_prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

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
                                   RELS <! '+POSS_REL+' !>,\
                                   HCONS <! !>, \
                                   ICONS <! !>  ], \
                          DTR.SYNSEM.LOCAL [ CONT.HOOK #hook & [ INDEX #possessor ],\
                                             CAT.VAL #val  ] ].',merge=True)

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
            if mod_spec=='mod':
                mylang.add(possessor_rule_name+' := [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                                                               '+agr_prefix+' #png ].')
            elif mod_spec=='spec':
                mylang.add('poss-unary-phrase-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png ] >,\
                                                                                      '+agr_prefix+' #png ].',section='phrases')



def customize_possessum_irules(strat,mylang,rules,ch,strat_num,mod_spec,mark_loc,possessum_type,hierarchies):

    # Add general possessum-marking rule:
    possessum_rule_name ='possessum-lex-rule-'+strat_num
                            
    # Add constraints to possessor rule for spec version                            
    if mod_spec=='spec':

        mylang.add(possessum_rule_name+POSSESSUM_RULE)

        agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        
        # Add constraints in for single marking and double marking:
        if mark_loc=='possessum' or mark_loc=='both':

            mylang.add(possessum_rule_name+':=  cat-change-only-lex-rule & \
                         [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                              VAL #val & [ SPEC < >,\
                                                           SPR < [ LOCAL.CAT [ VAL.SPR < >,\
                                                                               HEAD.POSSESSOR possessor-'+strat_num+' ] ] > ]  ] ,\
                            C-CONT [ HCONS <! !>, \
                                     ICONS <! !>,\
                                     RELS <! !> ],\
                            DTR.SYNSEM.LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                     HEAD.PRON -,\
                                                     VAL #val ] ] ].',merge=True)

            mylang.add('poss-unary-phrase-'+strat_num+' := poss-unary-phrase & [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ] ].',section='phrases')

            # If possessor isn't marked, don't let it be possessive
            if mark_loc=='possessum': 
                mylang.add('poss-unary-phrase-'+strat_num+' := poss-unary-phrase &\
                               [ ARGS < [ SYNSEM.LOCAL.CAT [ HEAD [ POSSESSOR nonpossessive ],\
                                                             POSSESSUM nonpossessive ] ] > ] .')

            rules.add('poss-unary-'+strat_num+' := poss-unary-phrase-'+strat_num+'.')

            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            instance_tmp={}
            if strat.get('possessor-feat'):
                for key in list(strat.keys()):
                    new_key=key.replace('feat','skip')
                    new_key=new_key.replace('possessor-skip','feat')
                    instance_tmp[new_key]=strat.get(key)
                customize_feature_values(mylang,ch,hierarchies,instance_tmp,possessum_rule_name,'possessum-spec-mark')
                                            
        # Add constraints to spec version for case where possessum is marked 
        # and possessor = pronoun
        if mark_loc=='possessum-with-pron':
            mylang.add(possessum_rule_name+':=  cat-change-only-lex-rule & \
                         [ SYNSEM.LOCAL.CAT [ HEAD #head,\
                                              POSSESSUM possessum-'+strat_num+',\
                                              VAL [ COMPS #comps,\
                                                    SPR < [ LOCAL [ CAT [ VAL.SPR < >,\
                                                                          HEAD +nd ] ] ] > ] ] ,\
                            DTR.SYNSEM.LOCAL [ CAT [ HEAD #head & [ PRON - ],\
                                                     VAL.COMPS #comps ] ] ].',merge=True)
            

    # Add constraints to possessor rule for mod version
    if mod_spec=='mod':

        agr_prefix='SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        mylang.add(possessum_rule_name+POSSESSUM_RULE)

        if mark_loc=='possessum' or mark_loc=='both':
            mylang.add(possessum_rule_name+':= val-change-with-ccont-lex-rule & \
                            [ SYNSEM.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+',\
                                                 VAL [ SPR #spr, \
                                                       COMPS <[ OPT - ,\
                                                                     LOCAL [ CAT cat-sat & [ HEAD +np,\
                                                                                             VAL.SPR < > ], \
                                                                             CONT.HOOK [ INDEX #possessor ] ] ]> ] ],\
                              C-CONT [ HOOK #hook ,\
                                       RELS <! '+POSS_REL+' !>,\
                                       HCONS <! !>,\
                                       ICONS <! !>  ],\
                              DTR.SYNSEM.LOCAL [ CAT [ HEAD.PRON -,\
                                                       VAL.SPR #spr ],\
                                                 CONT.HOOK #hook & [ INDEX #possessum,\
                                                                     LTOP #lbl ] ] ].',merge=True)
            
            # Check if you need to add INIT to the possessum to keep it from going through wrong ordered head-comps
            hc=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']    
            order_manip,default_init,head_comp_order=check_hc_order_manip(strat,hc)
            if order_manip:
                init='+' if strat.get('order')=='head-initial' else '-'
                if strat.get('order')!='either':
                    mylang.add(possessum_rule_name+' := [ SYNSEM.LOCAL.CAT.HEAD.INIT '+init+' ].')
            

            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            instance_tmp={}
            if strat.get('possessor-feat'):
                for key in list(strat.keys()):
                    new_key=key.replace('feat','skip')
                    new_key=new_key.replace('possessor-skip','feat')
                    instance_tmp[new_key]=strat.get(key)
                customize_feature_values(mylang,ch,hierarchies,instance_tmp,possessum_rule_name,'possessum-mod-mark')

        # Add constraints to mod version for double marking
        if mark_loc=='both':
            mylang.add(possessum_rule_name+' :=\
               [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD +np & [ POSSESSOR possessor-'+strat_num+' ] ].')
        
        # Add constraints to spec version for case where possessum is marked 
        # and possessor = pronoun
        if mark_loc=='possessum-with-pron':
            mylang.add(possessum_rule_name+':= cat-change-only-lex-rule & \
                                                       [ SYNSEM.LOCAL.CAT [ HEAD #head,\
                                                                            POSSESSUM possessum-'+strat_num+',\
                                                                            VAL [ SPR #spr ] ],\
                                                         C-CONT [ HOOK #hook ,\
                                                                  RELS <! !>,\
                                                                  HCONS <! !>,\
                                                                  ICONS <! !>  ],\
                                                         DTR.SYNSEM.LOCAL [ CONT.HOOK #hook,\
                                                                            CAT [ HEAD #head & [ PRON - ],\
                                                                                  VAL.SPR #spr ] ] ].',merge=True)

    # Add agreement features to the possessum affix
    if strat.get('possessum-affix-agr')=='agree':
        if mod_spec=='mod':
            mylang.add(possessum_rule_name+' := [ SYNSEM.LOCAL.CAT.POSSESSUM.POSS-AGR #png,\
                                                             '+agr_prefix+' #png ].')
        elif mod_spec=='spec':
            mylang.add('poss-unary-phrase-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png-um ] >,\
                                                             SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png-um ].',section='phrases')

    # Note: in the mutual agreement, double marking mod-like scenario, the possessor is a COMP.
    # Therefore, it has no access to the possessum's PNG info. When the possessor agrees with 
    # the possessum, therefore, all agreement must be done in the possessum-inflecting rule:
    if mark_loc=='both' and mod_spec=='mod' and strat.get('possessor-affix-agr')=='agree':
        if possessum_type=='affix':
            mylang.add(possessum_rule_name+' :=\
            [ SYNSEM.LOCAL [ CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #poss-png,\
                                                          CONT.HOOK.INDEX.PNG #poss-png ] ].')
                            
def customize_possessor_pron_irules(strat,mylang,ch,strat_name,feat,lrt,mod_spec,hierarchies):

    if strat_name in str(feat['name']) and feat['value']!='minus':

        # TODO: change COG-ST on pron to activ-or-more, and adjust any tests as needed.

        mylang.add(get_name(lrt)+'-lex-rule :=\
           [ SYNSEM.LOCAL.CAT.HEAD #head ,\
             DTR.SYNSEM.LOCAL [ CAT.HEAD #head ,\
                                CONT.HOOK #hook & [ INDEX #possessum & [ COG-ST activ-or-more ],\
                                                  LTOP #lbl] ],\
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
                                       VAL [ SPEC #spec & < >,\
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
def customize_poss_lexicon(strat,mylang,ch,lexicon,rules,hierarchies,strat_num,pron_strat):

    # Define vars for all elements of strategy:
    mark_loc=strat.get('mark-loc')
    mod_spec=strat.get('mod-spec')    
    possessor_type=strat.get('possessor-type')
    possessum_type=strat.get('possessum-type')

    # Add lexical items other than poss pronouns:
    if not pron_strat:

        # Add possessor-marking adpositons:
        if (mark_loc=='possessor' or mark_loc=='both') and possessor_type=='non-affix':

            customize_possessor_lexicon(strat,mylang,ch,lexicon,strat_num,mod_spec,mark_loc,hierarchies,rules)

        # Add possessum-marking nouns:
        if (mark_loc=='possessum' or mark_loc=='both') and possessum_type=='non-affix':

            customize_possessum_lexicon(strat,mylang,ch,lexicon,strat_num,mod_spec,mark_loc,hierarchies,rules)

    elif pron_strat:

        customize_possessor_pron_lexicon(strat,mylang,ch,lexicon,strat_num,mod_spec,hierarchies,rules)


def customize_possessor_lexicon(strat,mylang,ch,lexicon,strat_num,mod_spec,mark_loc,hierarchies,rules):
    pron_allowed=True if strat.get('pronoun-allow')=='yes' else False

    # Add most general defn of possessor-marking adp:
    mylang.set_section('otherlex')
    mylang.add(TWO_REL_ADP)
    mylang.add('possessor-adp-lex-'+strat_num+' '+POSSESSOR_ADP_LEX)
    
    # Make sure no other adps are going to show up where possessor adp should:
    mylang.add('basic-adposition-lex :+ [ SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR nonpossessive ,\
                                                             POSSESSUM nonpossessive ] ].')

    # Check if ordering info needs to be added to adp
    hc=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']    
    order_manip,default_init,head_comp_order=check_hc_order_manip(strat,hc)
    if order_manip:
        marker_order=strat.get('possessor-mark-order')
        init='bool'
        if marker_order=='either':
            init='bool'
        elif marker_order=='head-initial':
            init='+'
        elif marker_order=='head-final':
            init='-'

        mylang.add('possessor-adp-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.HEAD.INIT '+init+' ].')
    
    # Optionally block PRON + items from being possessors
    if not pron_allowed:
        mylang.add('possessor-adp-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.PRON - ].')

    # Add constraints to possessor adp for spec version
    if mod_spec=='spec':

        mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [  SYNSEM.LOCAL.CAT [ HEAD.POSSESSOR possessor-'+strat_num+',\
                                                       POSSESSUM nonpossessive ] ].')
        mylang.add('possessor-adp-lex-'+strat_num+' := \
                [  SYNSEM.LOCAL [ CONT [ RELS <! !>,\
                                         HCONS <! !> ] ] ].')

        mylang.add('poss-unary-phrase-'+strat_num+' := poss-unary-phrase & [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD [ POSSESSOR possessor-'+strat_num+' ] ] > ].',section='phrases')
        rules.add('poss-unary-'+strat_num+' := poss-unary-phrase-'+strat_num+'.')

        # If the possessor is the only marked constituent, forbid marking on the possessum:
        if mark_loc=='possessor':
            mylang.add('poss-unary-phrase-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM nonpossessive ] ].',section='phrases')

    # Add constraints to possessor adp for mod version
    if mod_spec=='mod':
        
        # Set posthead flag on possessors acting as mods
        if strat.get('order')=='head-initial':
            ph='+'
        elif strat.get('order')=='head-final':
            ph='-'
        else:
            ph='bool'

        mylang.add('possessor-adp-lex-'+strat_num+' := \
                                 [ SYNSEM.LOCAL [ CAT [ POSTHEAD '+ph+',\
                                                        HEAD [ POSSESSOR possessor-'+strat_num+',\
                                                             MOD.FIRST.LOCAL [ CAT [ HEAD.PRON -,\
                                                                                     VAL.SPR < [ ] > ] ] ] ],\
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

    if ch.get('case-marking')!='none':
        
        mylang.add('+np :+ [ CASE case ].', section='addenda')
        mylang.add('poss-case := case.',section='addenda')
        
        mylang.add('possessor-adp-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.HEAD.CASE poss-case ].')

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
            
            # If mod, agreement constraint goes on adp
            if mod_spec=='mod':
                mylang.add(adp_type+' := possessor-adp-lex-'+strat_num+' &\
                                        [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                        '+agr_prefix+' #png ].')

                customize_feature_values(mylang,ch,hierarchies,form,adp_type,'possessor-marker')

            # If spec, agreement constraint goes on poss-unary-phrase:
            elif mod_spec=='spec':
                mylang.add('poss-unary-phrase-'+strat_num+' := \
                          [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png-or,\
                            ARGS < [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png-or ] > ].',section='phrases')
                mylang.add(adp_type+' := possessor-adp-lex-'+strat_num+'.')
                customize_feature_values(mylang,ch,hierarchies,form,adp_type,'possessor-marker')

            # Add agreeing adps to lexicon:
            orth=form.get('agr-orth')
            lexicon.add(adp_type.replace('-lex','')+' := '+adp_type+' &\
                                         [ STEM < "'+orth+'" > ].')

    # Add any necessary constraints to the complement of the possessor-marking word:
    instance_tmp={}
    if strat.get('dep-comp-feat'):
        for key in list(strat.keys()):
            new_key=key.replace('feat','skip')
            new_key=new_key.replace('dep-comp-skip','feat')
            instance_tmp[new_key]=strat.get(key)
        customize_feature_values(mylang,ch,hierarchies,instance_tmp,'possessor-adp-lex-'+strat_num,'poss-adp-comp')


def customize_possessum_lexicon(strat,mylang,ch,lexicon,strat_num,mod_spec,mark_loc,hierarchies,rules):
    mylang.set_section('nounlex')

    # Check if ordering info needs to be added to adp
    init='bool'
    hc=customize_major_constituent_order(ch.get('word-order'),mylang,ch,rules)['hc']    
    order_manip,default_init,head_comp_order=check_hc_order_manip(strat,hc)
    if order_manip:
        marker_order=strat.get('possessum-mark-order')
        if marker_order=='either':
            init='bool'
        elif marker_order=='head-initial':
            init='+'
        elif marker_order=='head-final':
            init='-'

    # Add spec-version (only version in this case)
    if mod_spec=='spec':


        mylang.add('poss-unary-phrase-'+strat_num+' := poss-unary-phrase & [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM possessum-'+strat_num+' ] ].',section='phrases')
        rules.add('poss-unary-'+strat_num+' := poss-unary-phrase-'+strat_num+'.')

        # Add constraints to spec version for single marking
        if mark_loc=='possessum':

            mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX)
            mylang.add('possessum-noun-lex-'+strat_num+' := \
                          [ SYNSEM.LOCAL [ CAT [ VAL.SPR < [ LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ] >,\
                                           HEAD [ POSSESSOR nonpossessive ],\
                                           POSSESSUM possessum-'+strat_num+' ] ] ].',merge=True)

            # Add any feature constraints to the possessor (only if the possessor is unmarked)
            if strat.get('possessor-feat'): #TODO change to checking if possessor is marked
                instance_tmp={}
                if strat.get('possessor-feat'):
                    for key in list(strat.keys()):
                        new_key=key.replace('feat','skip')
                        new_key=new_key.replace('possessor-skip','feat')
                        instance_tmp[new_key]=strat.get(key)
                    customize_feature_values(mylang,ch,hierarchies,instance_tmp,'poss-unary-phrase-'+strat_num,'possessum-spec-mark')

        # Add constraints to spec version for double marking
        if mark_loc=='both':

            mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX)
            mylang.add('possessum-noun-lex-'+strat_num+' := \
                                             [ SYNSEM.LOCAL [ CAT [ VAL.SPR < [ LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ] >,\
                                                                    HEAD [ POSSESSOR nonpossessive ],\
                                                                    POSSESSUM possessum-'+strat_num+' ] ] ].',merge=True)
                
        if mark_loc=='possessum-with-pron':

            mylang.add('possessum-noun-lex-'+strat_num+' '+POSSESSUM_NOUN_LEX_W_PRON)
            mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL [ CAT [ VAL.SPR < [ LOCAL.CAT.HEAD.POSSESSOR possessor-'+strat_num+' ] >,\
                                                                                   HEAD [ POSSESSOR nonpossessive ],\
                                                                                          POSSESSUM possessum-'+strat_num+' ] ] ].',merge=True)
        if order_manip:

            mylang.add('possessum-noun-lex-'+strat_num+' := [ SYNSEM.LOCAL.CAT.HEAD.INIT '+init+' ].')


    if mod_spec=='mod':

        mylang.add('head :+ [ INIT bool ].', section='addenda')

        possessor_constr='& [ POSSESSOR possessor-'+strat_num+' ]' if mark_loc=='both' else ''

        if mark_loc!='possessum-with-pron':
        
            mylang.add('possessum-noun-lex-'+strat_num+' := basic-two-arg &\
                          [ SYNSEM.LOCAL [ CAT [ POSSESSUM possessum-'+strat_num+',\
                                                 HEAD noun & [ POSSESSOR nonpossessive,\
                                                               INIT '+init+' ],\
                                                 VAL [ COMPS < #possessum-comp & [ OPT -,\
                                                                                   LOCAL [ CONT.HOOK #hook &\
                                                                                                   [ INDEX #possessum,\
                                                                                                     LTOP #lbl ]  ,\
                                                                                          CAT [ VAL.SPR #spr & < [ ] >,\
                                                                                                HEAD noun & [ POSSESSOR nonpossessive ],\
                                                                                                POSSESSUM nonpossessive ] ] ],\
                                                             #possessor-comp & [ OPT -,\
                                                                                 LOCAL [ CAT [ POSSESSUM nonpossessive,\
                                                                                               VAL.SPR <  >,\
                                                                                               HEAD +np '+possessor_constr+' ],\
                                                                                         CONT.HOOK.INDEX #possessor ] ] >,\
                                                      SPR #spr ] ],\
                                           CONT [ HOOK #hook,\
                                                  RELS <! '+POSS_REL+' !>,\
                                                  HCONS <! !>,\
                                                  ICONS <! !>  ] ],\
                            ARG-ST < #possessum-comp, #possessor-comp > ].')
        else:

            # When the possessum is marked and the possessor = pron, then the pron is the modifier of the possessum:
            mylang.add('possessum-noun-lex-'+strat_num+' := basic-one-arg &\
                          [ SYNSEM.LOCAL [ CAT [ POSSESSUM possessum-'+strat_num+',\
                                                 HEAD noun & [ POSSESSOR nonpossessive,\
                                                              INIT '+init+' ],\
                                                 VAL [ COMPS < #possessum-comp & [ OPT -,\
                                                                                   LOCAL [ CONT.HOOK #hook  & [ INDEX #possessum ] ,\
                                                                                           CAT [ VAL.SPR #spr & < [ ] >,\
                                                                                                HEAD noun & [ POSSESSOR nonpossessive ],\
                                                                                                POSSESSUM nonpossessive ] ] ] >,\
                                                      SPR #spr ] ],\
                                           CONT [ HOOK #hook & [ INDEX #possessum ] ,\
                                                  RELS <! !>,\
                                                  HCONS <! !>,\
                                                  ICONS <! !>  ] ],\
                            ARG-ST < #possessum-comp > ].')
        # Add any feature constraints to the possessor (only if the possessor is unmarked)
        instance_tmp={}
        if strat.get('possessor-feat'):
            for key in list(strat.keys()):
                new_key=key.replace('feat','skip')
                new_key=new_key.replace('possessor-skip','feat')
                instance_tmp[new_key]=strat.get(key)
            customize_feature_values(mylang,ch,hierarchies,instance_tmp,'possessum-noun-lex-'+strat_num,'possessum-mod-mark2')


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
                prefix='SYNSEM.LOCAL.CAT.VAL.COMPS.REST.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
            noun_type=noun_id(form)


            if mod_spec=='mod':

                # If mod and appearing w pronoun, then pronoun handles agr
                if mark_loc=='possessum-with-pron':
                    pron_type=noun_id(strat)
                    mylang.add(pron_type+' :=\
                              [ SYNSEM.LOCAL [ CAT.HEAD.MOD.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #head-png,\
                                               CONT.HOOK.INDEX.PNG #head-png ] ].')
                    mylang.add(noun_type+' := possessum-noun-lex-'+strat_num+'.')
                    
                else:
                    # If mod and not w pronoun, then agreeing happens on possessum marker:                
                    mylang.add(noun_type+' := possessum-noun-lex-'+strat_num+' &\
                          [ SYNSEM.LOCAL.CAT.POSSESSUM.POSS-AGR #png,\
                                '+prefix+' #png ].')

            # If spec, then agreeing happens on poss-unary-rule:
            elif mod_spec=='spec':
                mylang.add(noun_type+' := possessum-noun-lex-'+strat_num+'.')
                mylang.add('poss-unary-phrase-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png-um ] >,\
                                                                 SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png-um ].',section='phrases')

            customize_feature_values(mylang,ch,hierarchies,form,noun_type,'possessum-marker')

            orth=form.get('agr-orth')
            # Add appropriate number of agreeing forms to lexicon
            lexicon.add(noun_type.replace('-lex','')+' := '+noun_type+' &\
                                         [ STEM < "'+orth+'" > ].')


def customize_possessor_pron_lexicon(strat,mylang,ch,lexicon,strat_num,mod_spec,hierarchies,rules):

    # Set vars for pron strat:
    noun_type=noun_id(strat)
    agr=True if strat.get('agr')=='agree' else False

    # Add general form of pronoun:
    mylang.set_section('nounlex')
    mylang.add(noun_type+POSSESSOR_PRON_LEX)

    # Add constraints for spec version:
    if mod_spec=='spec':
        agr_prefix='SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'

        mylang.add(noun_type+' := \
                        [ SYNSEM.LOCAL [ CAT [ HEAD.POSSESSOR possessor-pron-'+strat_num+',\
                                               VAL.SPEC < > ],\
                                         CONT [ RELS  <! #altkeyrel !>,\
                                                  HCONS <! !> ] ] ].')

           
        mylang.add('poss-unary-phrase-pron-'+strat_num+' := poss-unary-phrase & [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD [ POSSESSOR possessor-pron-'+strat_num+' ] ] > ].',section='phrases')
        rules.add('poss-unary-pron-'+strat_num+' := poss-unary-phrase-pron-'+strat_num+'.')

        if agr: 
            mylang.add('poss-unary-phrase-pron-'+strat_num+' := [ ARGS < [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png ] >,\
                                                           '+agr_prefix+' #png ].',section='phrases')


    # Add constraints for mod version
    elif mod_spec=='mod':

        # Set posthead flag on possessors acting as mods
        if strat.get('order')=='head-initial':
            ph='+'
        elif strat.get('order')=='head-final':
            ph='-'
        else:
            ph='bool'

        agr_prefix='SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG'
        mylang.add(noun_type+' := \
                        [ SYNSEM.LOCAL [ CAT [ POSTHEAD '+ph+',\
                                               HEAD [ POSSESSOR possessor-pron-'+strat_num+',\
                                                    MOD < [ OPT -,\
                                                            LOCAL [ CAT [ HEAD.PRON -,\
                                                                          VAL [ SPR < [ ] >,\
                                                                                COMPS < > ] ],\
                                                                    CONT.HOOK [ INDEX #possessum,\
                                                                                LTOP #lbl ] ] ] > ] ],\
                                         CONT [ RELS  <!  '+POSS_REL+',\
                                                           #altkeyrel !>,\
                                                HCONS <! !> ] ] ].')

        if agr: 
            mylang.add(noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR #png,\
                                              '+agr_prefix+' #png ].')

    if ch.get('case-marking')!='none':
        mylang.add('poss-case := case.',section='addenda')
        mylang.add(noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.CASE poss-case ].')

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
        # Add PNG agr features:
        for key in list(pron_inst.keys()):
            # Relabel the inherent features as something else ('skip') 
            # Relabel the agreement features as simply features ('feat')
            # Then call customize_feature_values() with the 'poss-marker' setting
            # so that the agreement features are added at POSS.POSS-AGR instead of HOOK.INDEX.PNG
            
            new_key=key.replace('feat','skip')
            new_key=new_key.replace('agr-skip','feat')

            instance_tmp[new_key]=pron_inst.get(key)

        # TODO: Figure out how to cast instance_tmp from a dict to a ChoiceDict so that no future
        #  developers have to deal with this mess in features.py
        customize_feature_values(mylang,ch,hierarchies,instance_tmp,instance_name,'possessor-marker')

        # Add non-PNG features (only enabled for mod-like prons):
        instance_tmp={}
        for key in list(pron_inst.keys()):
            
            new_key=key.replace('feat','skip')
            new_key=new_key.replace('non-png-skip','feat')
            instance_tmp[new_key]=pron_inst.get(key)

        if strat.get('mod-spec')=='mod':
            customize_feature_values(mylang,ch,hierarchies,instance_tmp,instance_name,'poss-pron-mod')

    # Add any necessary markings to the possessum:
    if strat.get('possessum-mark')=='yes':

        # Make possessor pron req a marked possessum:
        if mod_spec=='spec':
            mylang.add('poss-unary-phrase-pron-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM possessum-pron-'+strat_num+' ].',section='phrases')

        if mod_spec=='mod':
            mylang.add(noun_type+' := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.POSSESSUM possessum-pron-'+strat_num+'] > ].')
      
        # Add affixal markings:
        if strat.get('possessum-mark-type')=='affix':
            
            customize_possessum_irules(strat,mylang,rules,ch,'pron-'+strat_num,mod_spec,'possessum-with-pron','affix',hierarchies)

            if strat.get('possessum-mark-affix-agr')=='agree':
                if mod_spec=='spec':

                    mylang.add('poss-unary-phrase-pron-'+strat_num+' := \
                              [ ARGS < [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png ] >,\
                                SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png ].',section='phrases')

                if mod_spec=='mod':
                    mylang.add(noun_type+' := \
                              [ SYNSEM.LOCAL [ CONT.HOOK.INDEX.PNG #png,\
                                               CAT.HEAD.MOD.FIRST.LOCAL.CAT.POSSESSUM.POSS-AGR #png ] ].')
                
        if strat.get('possessum-mark-type')=='non-affix':
            
            customize_possessum_lexicon(strat,mylang,ch,lexicon,'pron-'+strat_num,mod_spec,'possessum-with-pron',hierarchies,rules)
    else:
        # If the possessor is the only marked constituent in a spec construction, forbid marking on the possessum:
        if mod_spec=='spec':
            mylang.add('poss-unary-phrase-pron-'+strat_num+' := [ SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT [ POSSESSUM nonpossessive ] ].',section='phrases')

