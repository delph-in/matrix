from gmcs.utils import get_name,TDLencode, orth_encode

from gmcs import constants
from gmcs.linglib import lexbase

######################################################################
# Clausal Complements
#   Create the type definitions associated with the user's choices
#   about clasual complements.

######################################################################

# Constants (specific to this module)
COMPS = 'comps' # choice name for clausal complement strategies
COMP = 'comp' # reserved head name for complementizers; should be a constant on some other page?
              # Also, the name for the choice for complementizer of a clausal complement strategy.
EXTRA = 'clause-pos-extra' # Choice name for extraposed complement
SAME = 'clause-pos-same' # Choice name for default noun position complement

BEF = 'comp-pos-before' # Choice name for complementizer attaching before embedded clause
AFT = 'comp-pos-after' # Choice name for complementizer attaching after embedded clause

COMPLEX = 'complex' # TDL file section name for complementizer lexical items

COMP_LEX_ITEM = 'comp-lex-item'

COMP_LEX_ITEM_DEF = COMP_LEX_ITEM + ' := raise-sem-lex-item & basic-one-arg &\
      [ SYNSEM.LOCAL.CAT [ HEAD comp &\
                                [ MOD < > ],\
                           VAL [ SPR < >,\
                                 SUBJ < >,\
                                 COMPS < #comps > ] ],\
        ARG-ST < #comps & \
                 [ LOCAL.CAT [ HEAD verb, MC -,\
                               VAL [ SUBJ < >,\
                                     COMPS < > ] ] ] > ].'

FORM = 'FORM' # FORM feature name
FORM_PATH = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD' # FORM feature path

# Note: the below lists do not include V2 or free.
OV = ['sov', 'ovs', 'osv', 'v-final']
VFINAL = ['sov','osv','v-final']
VO = ['svo', 'vos', 'vso', 'v-initial']

CLAUSALCOMP = 'clausal'
COMPLEMENTIZER = 'stem' # Choices key for choices pertaining
                                  # to the complementizer defined for
                                  # a particular complementation strategy.

# Error messages:
EXTRA_VO = 'The only supporded word orders for extraposed complements are: SOV, VOS, OVS, OSV, v-final.'
SAME_OR_EXTRA = 'Please choose whether the clausal complement takes the same position as noun ' \
                        'complements or is extraposed to the end of the clause ' \
                        '(the latter valid only for strict OV orders).'
WO_WARNING = 'You chose a flexible word order; note that the order will indeed be flexible, ' \
             'including within the embedded clause.'

#### Methods ###

'''
Main function which will be called by customize.py.
Should fully cover all the customization needed for
what was specified on the Clausal Complements subpage
of the Questionnaire.
'''
def customize_clausalcomps(mylang,ch,lexicon,rules):
    if not COMPS in ch:
        return
    # Note: clausal verb type will be added by lexical_items.py.
    have_comp = add_complementizers_to_lexicon(lexicon,ch)
    add_types_to_grammar(mylang,ch,rules,have_comp)

def add_complementizers_to_lexicon(lexicon,ch):
    lexicon.add_literal(';;; Complementizers')
    have_comp = False
    for comp_strategy in ch[COMPS]:
        id = comp_strategy.full_key
        stype = id + '-' + COMP_LEX_ITEM
        #TODO: Perhaps turn complementizers into full-blown
        # lexical items and then can call insert_ids() from lexical_items.py instead
        # This would need to be done via redesigning the questionnaire a little.
        for complementizer in comp_strategy[COMPLEMENTIZER]:
            orth = orth_encode(complementizer.get('orth'))
            name = TDLencode(complementizer.get('name'))
            typedef = name + ' := ' + stype + ' & \
                          [ STEM < "' + orth + '" > ].'
            lexicon.add(typedef)
            have_comp = True
    return have_comp

def add_types_to_grammar(mylang,ch,rules,have_complementizer):
    if have_complementizer:
        mylang.set_section(COMPLEX)
        add_complementizer_supertype(mylang)
    wo = ch.get(constants.WORD_ORDER)
    init = use_init(ch, mylang, wo)
    extra = extra_needed(ch,mylang)
    for cs in ch.get(COMPS):
        clausalverb = find_clausalverb_typename(ch,cs)
        customize_clausal_verb(clausalverb,mylang,ch,cs,extra)
        typename = add_complementizer_subtype(cs, mylang,ch,extra) if cs[COMP] else None
        if wo in OV or wo in VO:
            general, additional = determine_head_comp_rule_type(ch.get(constants.WORD_ORDER),cs)
            customize_order(ch, cs, mylang, rules, typename, init,general,additional)
            if need_customize_hs(wo,cs):
                constrain_head_subj_rules(cs,mylang,rules,ch)
            if extra:
                constrain_for_extra(wo,general, additional, cs, mylang)
        elif wo == 'free':
            constrain_complementizer(wo,cs,mylang,typename)


def constrain_for_extra(wo,general, additional, cs, mylang):
    if cs[EXTRA] and additional_hcr_needed(cs, wo):
        mylang.add(additional + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.EXTRA + ].', merge=True)
        mylang.add(general + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.EXTRA - ].', merge=True)


def is_more_flexible_order(wo,ccs):
    """
    @param ch: choices
    @return: True if the word order in complex sentences
    subsumes the basic WO but not restricts it.
    E.g. If in a SOV order both OV and VO is allowed for clausal complements.
    Or if complementizers attach both before and after clause.
    If e.g. OV order is forbidden for clausal complements, must return False.
    """
    if not wo in OV and not wo in VO:
        return False
    if not ccs[COMP] == 'oblig':
        return False
    if wo in OV:
        #Either ccomps or compl. cannot use normal HCR.
        strict_extra_bef = not ccs[AFT] or not ccs[SAME]
        #complementizers can't use additional HCR.
        extra_strict_aft = ccs[AFT] and not ccs[BEF] and ccs[EXTRA]
        #ccomps can't use additional HCR.
        noextra_bef = ccs[BEF] and not ccs[EXTRA]
        restricted = strict_extra_bef or extra_strict_aft or noextra_bef
        if restricted:
            return False
    if wo in VO:
        strict_extra_aft = not ccs[BEF] or not ccs[SAME]
        extra_strict_bef = (ccs[BEF] and not ccs[AFT] and ccs[EXTRA])
        noextra_bef = ccs[AFT] and not ccs[EXTRA]
        restricted = strict_extra_aft or extra_strict_bef or noextra_bef
        if restricted:
            return False
    return True


def constrain_complementizer(wo,cs,mylang,typename):
    if not wo == 'free':
        raise Exception("This function is for free word order only.")
    if cs[COMP]:
        path = 'SYNSEM.LOCAL.CAT.HEAD'
        if cs[BEF] and not cs[AFT]:
            init_val = '+'
            default_init_val = '-'
            my_phrase = 'head-comp'
            other_phrase = 'comp-head'
        elif cs[AFT] and not cs[BEF]:
            init_val = '-'
            default_init_val = '+'
            my_phrase = 'comp-head'
            other_phrase = 'head-comp'
        if not init_val or not my_phrase or not other_phrase or not default_init_val:
            raise Exception('Illegal combination of choises for complementizer position.')
        constrain_lexitem_for_feature(typename,path,'INIT',init_val,mylang)
        mylang.add(my_phrase + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT ' + init_val + ' ].',
                   merge=True)
        mylang.add(other_phrase + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT ' + default_init_val + ' ].',
                   merge=True)

def use_init(ch, mylang, wo):
    init = False
    if wo in OV or wo in VO or wo == 'free':
        for cs in ch.get(COMPS):
            is_flex = is_more_flexible_order(wo,cs)
            init = init_needed(wo, cs, mylang, is_flex)
            if init:
                break
    return init

def add_complementizer_supertype(mylang):
    mylang.add(COMP_LEX_ITEM_DEF, section=COMPLEX)

def add_complementizer_subtype(cs, mylang,ch,extra):
    id = cs.full_key
    typename = id + '-' + COMP_LEX_ITEM
    mylang.add(typename + ' := ' + COMP_LEX_ITEM + '.', section=COMPLEX)
    constrain_for_features(typename,cs,mylang,'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.',ch,is_nominalized_complement(cs))
    if cs['cformvalue']:
        constrain_lexitem_for_feature(typename,'SYNSEM.LOCAL.CAT.HEAD','FORM',cs['cformvalue'],mylang)
    if extra:
        if cs[EXTRA] and not cs[SAME]:
            mylang.add(typename + ':= [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.EXTRA + ] > ].',merge=True)
        elif cs[SAME] and not cs[EXTRA]:
            mylang.add(typename + ':= [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.EXTRA - ] > ].',merge=True)
    return typename

'''
Add and modify head-complement rules depending
on what kind of word order variations clausal complements
exhibit.
General and additional are default and new head-comp rule
(determined simply by the word order).
For example, if the order is OV, the general rule will
be comp-head, and the additional will be head-comp,
to accommodate non-default orders.
Typename is the name of the complementizer involved in this
complementation strategy.
cs is the complementation strategy.
init tells if the INIT feature is needed or not. The value must
be true if INIT feature will be used in at least one of
the complementation strategies in this grammar.
'''
def customize_order(ch, cs, mylang, rules, typename, init, general, additional):
    wo = ch.get(constants.WORD_ORDER)
    init_gen, init_add = which_init(general,additional)
    is_flex = is_more_flexible_order(wo,cs)
    #if is_flex and not init:
    constrain_lex_items_using_headtypes(ch,cs,typename,mylang)
    #else:
    constrain_lex_items(ch,cs,typename,init_add,init_gen,mylang,init)
    if need_customize_hc(wo,cs):
        if additional_hcr_needed(cs,wo):
            #if is_flex and not init:
            constrain_head_comp_rules_headtype(mylang,rules,additional,cs,ch)
            #else:
            constrain_head_comp_rules(mylang,rules,init,general,additional,cs,ch)
        add_special_complementizer_HCR(additional, cs, general, mylang, rules, wo,is_flex)

def need_customize_hc(wo,cs):
    return (wo in ['vos', 'v-initial', 'sov', 'v-final', 'osv', 'ovs'] and cs[EXTRA]) \
           or (wo in OV and cs[BEF]) \
           or (wo in VO and cs[AFT])

def need_customize_hs(wo,cs):
    return wo in ['vos'] and cs[EXTRA]

def constrain_head_subj_rules(cs,mylang,rules,ch):
    mylang.add('head-subj-ccomp-phrase := decl-head-subj-phrase & head-initial & '\
               '[ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.EXTRA + ] > ].',section='phrases')
    constrain_for_features('head-subj-ccomp-phrase',cs,mylang,
                           'HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.',ch,is_nominalized_complement(cs))
    rules.add('head-subj-ccomp := head-subj-ccomp-phrase.')
    mylang.add('head-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].',merge=True)

'''
An additional HCR will *not* be needed if:
The matrix order is VO and clausal complements are not extraposed,
and there is not a complementizer or
there is a complementizer but it can only use the normal HCR.
Note that this relies that for some situations, complementizer_head_comp_needed
will be called separately! (Which is bad of course and should be rewritten).
'''
def additional_hcr_needed(cs,wo):
    if wo in ['vos'] and cs[EXTRA] and cs[SAME] and cs[BEF] and cs[AFT]:
      return False #Because additional HSR instead
    if wo == 'v-initial' and cs[EXTRA] and cs[SAME] and not cs[AFT]:
      return False
    if wo == 'v-initial' and cs[SAME] and cs[BEF] and cs[AFT]:
      return False
    if wo in OV and not cs[EXTRA] and not cs[BEF]:
        return False
    if wo in VO and not cs[EXTRA] and not cs[AFT]:
        return False
    return True

#This assumes WO is in ['v-initial','vos','v-final'].
# This is an interesting function which should ideally be merged with
# additional_hcr_needed somehow, in the higher logic of the library.
# I think additional HEAD comp HCR is needed only in a couple cases.
def complementizer_comp_head_needed(wo,cs):
    if not wo in ['v-initial','vos','v-final']:
        return False
    if wo in ['vos'] and cs[EXTRA] and cs[SAME] and cs[BEF] and not cs[AFT]:
      return False
    if wo  == 'v-initial' and cs[EXTRA] and not cs[AFT]:
        return False
    if wo == 'vos' and cs[EXTRA] and cs[AFT]:
        return True
    if wo == 'v-final' and cs[EXTRA] and not cs[SAME] and cs[AFT]:
        return True
    if not wo == 'v-final' and cs[SAME] and cs[COMP] and (cs[EXTRA] or cs[AFT]):
        return True
    return False

def which_init(general, additional):
    supertype_gen = 'head-initial' if general.startswith(constants.HEAD_COMP) else 'head-final'
    supertype_add = 'head-initial' if additional.startswith(constants.HEAD_COMP) else 'head-final'
    init_general = '+' if supertype_gen == 'head-initial' else '-'
    init_add = '+' if supertype_add == 'head-initial' else '-'
    return (init_general,init_add)

'''
If an additional head-comp rule is needed, it may also need constraints
with respect to its head or the INIT feature. The default rule will
also need to be constrained with respect to INIT, if INIT is used in
the additional rule.
'''
def constrain_head_comp_rules(mylang,rules,init,general,additional,cs,ch):
    wo = ch.get(constants.WORD_ORDER)
    supertype = 'head-initial' if additional.startswith(constants.HEAD_COMP) else 'head-final'
    init_gen, init_add = which_init(general,additional)
    mylang.add(additional + '-phrase := basic-head-1st-comp-phrase & ' + supertype + '.'
           ,section = 'phrases',merge=True)
    rules.add(additional + ' := ' + additional + '-phrase.')
    if is_nominalized_complement(cs):
        mylang.add(additional + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ + ].'
               ,merge=True)
        if not cs[SAME]:
            mylang.add(general + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
    if init:
        mylang.add(additional + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT ' + init_add + ' ].',
                   merge=True)
        mylang.add(general + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT ' + init_gen + ' ].',
                   merge=True)
    constrain_for_features(additional + '-phrase', cs, mylang,
                           'NON-HEAD-DTR.SYNSEM.',ch,is_nominalized_complement(cs))
    if need_low_subj_attachment(wo,cs,additional):
            enforce_low_subj(additional,mylang)

def constrain_head_comp_rules_headtype(mylang,rules,additional,cs,ch):
    wo = ch.get(constants.WORD_ORDER)
    supertype = 'head-initial' if additional.startswith(constants.HEAD_COMP) else 'head-final'
    head = determine_head(ch.get(constants.WORD_ORDER),cs)
    mylang.add(additional + '-phrase := basic-head-1st-comp-phrase & ' + supertype + '.'
           ,section = 'phrases',merge=True)
    rules.add(additional + ' := ' + additional + '-phrase.')
    if is_nominalized_complement(cs):
        mylang.add(additional + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ + ].'
               ,merge=True)
    mylang.add(additional + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
                   merge=True)
    constrain_for_features(additional + '-phrase', cs, mylang,
                           'NON-HEAD-DTR.SYNSEM.',ch,is_nominalized_complement(cs))
    if need_low_subj_attachment(wo,cs,additional):
            enforce_low_subj(additional,mylang)

def constrain_for_features(typename,choice,mylang,path_prefix,ch,is_nmz):
    for f in choice['feat']:
        path = 'LOCAL.CAT.HEAD.'
        if nominalized_comps(ch) and not is_nmz:
            mylang.add(typename + ' := [ ' + path_prefix + path + 'NMZ - ].',merge=True)
        if f['name'] != 'nominalization':
            if f['name'] == 'mood' or f['name'] == 'aspect':
                path = 'LOCAL.CONT.HOOK.INDEX.E.'
            else:
                path = 'LOCAL.CAT.HEAD.'
            mylang.add(typename + ' := '
                                    '[ ' + path_prefix + path + f['name'].upper() + ' '
                       + f['value'] + ' ].', merge=True)
        else:
            path = 'LOCAL.CAT.HEAD.'
            mylang.add(typename + ' := [ ' + path_prefix + path + 'NMZ + ].',merge=True)


def need_low_subj_attachment(wo,cs,additional):
    if not additional_hcr_needed(cs,wo):
        return False
    relevant_order = wo in ['ovs', 'osv', 'v-initial','vos','v-final']
    is_head_fin = additional.startswith(constants.COMP_HEAD)
    vin = wo in ['v-initial','vos']
    return ((relevant_order and cs[EXTRA])) or ((vin and cs[AFT]) and not (vin and is_head_fin))

def enforce_low_subj(phrase_name,mylang):
    mylang.add(phrase_name + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].',
        section='phrases',merge=True)


#TODO: This isn't really special cases. This is EXTRA feature handling,
# plus adding SUBJ <> in some cases,
# plus an actual special case(?) with complementizer.
def add_special_complementizer_HCR(additional, cs, general, mylang, rules, wo,is_more_flex):
    if complementizer_comp_head_needed(wo,cs) and not additional.startswith(constants.COMP_HEAD):
        # V-final and VOS will need and additional (to additional) HCR in some cases,
        # for the complementizer to be able to attach to an extraposed complement.
        # The situation should be symmetric for v-initial but we aren't yet supporting
        # extraposition to the beginning of the clause, so this doesn't fully come up.
        name = 'comp-head' if not general == 'comp-head' else 'comp-head-compl'
        if is_more_flex:
            mylang.add(name + '-phrase := basic-head-1st-comp-phrase & head-final '\
                   '& [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD comp ].',section='phrases')
        else:
            mylang.add(name + '-phrase := basic-head-1st-comp-phrase & head-final '\
                   '& [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INIT - ].',section='phrases')
        if not cs[SAME]:
            mylang.add(name + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.EXTRA + ].',
                       merge=True)
        rules.add(name + ' := ' + name + '-phrase.')

def determine_clausal_verb_comp_head(cs):
    head = ''
    if cs[COMP]:
        if cs[COMP] == 'oblig':
            head = 'comp'
        elif cs[COMP] == 'opt':
            head = '+vc'
    else:
        head = 'noun' if is_nominalized_complement(cs) else 'verb'
    return head


def find_clausalverb_typename(ch,cs):
    for v in ch.get(constants.VERB):
        if v.get(constants.VALENCE).endswith(cs.full_key):
            return get_name(v) + '-verb-lex'


def constrain_transitive_verb(head,cs):
    return head == 'verb' \
           or (head == '+vc' and cs[EXTRA]
               and (not cs[SAME] or cs[COMP] == 'opt'))


def constrain_lex_items_using_headtypes(ch,cs,comptype, mylang):
    wo = ch.get(constants.WORD_ORDER)
    path = 'SYNSEM.LOCAL.CAT'
    head = determine_head(wo,cs)
    if cs[COMP]:
        if wo in VO:
            if cs[AFT] and not cs[BEF]:
                constrain_lexitem_for_feature(comptype,path,'HEAD',head,mylang)
        elif wo in OV:
            if cs[BEF] and not cs[AFT]:
                constrain_lexitem_for_feature(comptype,path,'HEAD',head,mylang)
    if comptype and nominalized_comps(ch) and not is_nominalized_complement(cs):
        mylang.add(comptype + ':= [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.NMZ - ] > ].',merge=True)


def constrain_lex_items(ch,cs,comptype, init_value, default_init_value,mylang,init):
    wo = ch.get(constants.WORD_ORDER)
    clausalverb = find_clausalverb_typename(ch,cs)
    path = 'SYNSEM.LOCAL.CAT.HEAD'
    if init:
        if cs[COMP]:
            if wo in VO:
                if cs[AFT] and not cs[BEF]:
                    constrain_lexitem_for_feature(comptype,path,'INIT',init_value,mylang)
                elif cs[BEF] and not cs[AFT]:
                    constrain_lexitem_for_feature(comptype,path,'INIT',default_init_value,mylang)
            elif wo in OV:
                if cs[BEF] and not cs[AFT]:
                    constrain_lexitem_for_feature(comptype,path,'INIT',init_value,mylang)
                elif cs[AFT] and not cs[BEF]:
                    constrain_lexitem_for_feature(comptype,path,'INIT',default_init_value,mylang)
        if wo in OV:
            if cs[EXTRA] and not cs[SAME]:
                constrain_lexitem_for_feature(clausalverb,path,'INIT',init_value,mylang)
            elif cs[SAME] and not cs[EXTRA]:
                constrain_lexitem_for_feature(clausalverb,path,'INIT',default_init_value,mylang)
        for pos in ['tverb','aux','det','cop']:
            if ch.get(pos) or pos in ['tverb']:
                name = lexbase.LEXICAL_SUPERTYPES[pos]
                mylang.add(name + ' := [ ' + path + '.INIT ' + default_init_value + ' ].'
                        , merge=True)
    if comptype and nominalized_comps(ch) and not is_nominalized_complement(cs):
        mylang.add(comptype + ':= [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.NMZ - ] > ].',merge=True)


def constrain_lexitem_for_feature(typename, feature_path, feature_name, feature_value,mylang):
    mylang.add( typename + ' := [ ' + feature_path + '.' + feature_name.upper() + ' ' + feature_value + ' ]. ',
                            merge=True)

'''
Determine whether the head of the additional head-comp rule
should be constrained to just verbs, just complementizers, or both.
'''
def determine_head(wo,cs):
    head = None
    if not cs[COMP]:
        head = 'verb'
    elif wo in OV:
        if cs[BEF]:
            if cs[EXTRA]:
                head = '+vc'
            elif cs[SAME]:
                head = 'comp'
        elif cs[AFT]:
            if cs[EXTRA]:
                head = 'verb'
    elif wo in VO:
        if cs[AFT]:
            if cs[EXTRA]:
                head = '+vc'
            elif cs[SAME]:
                head = 'comp'
        elif cs[BEF]:
            if cs[EXTRA]:
                head = '+vc'
    return head

'''
Determine which head-complement rule is the generally applicable one
and which one would be the secondary one, applicable only to complementizers
and/or clausal complement verbs.
'''
def determine_head_comp_rule_type(wo,cs):
    if wo == 'v2' or wo == 'free':
        # Note: it is possible that not much is needed here, as v2 and free are very flexible
        raise Exception('Currently only supporting strict VO/OV orders, but not V2 or free.')
    if wo=='v-initial' or wo == 'vos' and cs[EXTRA]:
            return(constants.HEAD_COMP, 'head-comp-ccomp')
    return (constants.HEAD_COMP, constants.COMP_HEAD) if wo in VO \
        else (constants.COMP_HEAD,constants.HEAD_COMP)


'''
Given word order and clausal complement choices,
determine whether the INIT feature will be used,
for this particular clausal complement strategy.
Note that once the INIT feaure has been used for
one strategy, you will need to keep it in mind
for all of them, so you will need to stop
calling this function once it returns True.
'''

def init_needed(wo, cs,mylang,is_flex):
    if is_flex:
        return False
    res = False
    if cs[COMP]:
        if wo in OV:
            if cs[COMP] == 'opt' and cs[EXTRA]:
                res = True
            elif cs[BEF]:
                if not cs[AFT]: # complementizer before clause only
                    res = True
                else: # complementizer both before and after clause
                    res = (cs[SAME] and not cs[EXTRA]) \
                          or (cs[EXTRA]and not cs[SAME])
            elif cs[AFT]:
                res = cs[EXTRA] == constants.ON
        elif wo in VO:
            res = (cs[AFT] == constants.ON)
        elif wo == 'free':
            res = (cs[AFT] and not cs[BEF]) or (cs[BEF] and not cs[AFT])
    else:
         res = wo in OV and cs[EXTRA] == 'on'
    if res:
        mylang.add('head :+ [ INIT bool ].', section='addenda')
    return res

def extra_needed(ch,mylang):
    res = ch.get(constants.WORD_ORDER) in ['v-initial','vos','v-final'] \
           and len([cs for cs in ch[COMPS] if cs[EXTRA]]) > 0
    if res:
        mylang.add('head :+ [ EXTRA bool ].', section='addenda')
        mylang.add('transitive-verb-lex := [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.EXTRA - ] > ].'
                       ,merge=True)
    return res

'''
Add clausal verb supertype to the grammar.
'''
# Note: this function is currently called from within lexical_items.py.
# It is possible that that call should be moved to this module.

def add_clausalcomp_verb_supertype(ch, mainorverbtype,mylang):
    head = ch.case_head()
    typedef = CLAUSALCOMP + '-verb-lex := ' + mainorverbtype + '&\
      [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >,\
        ARG-ST < [ LOCAL.CAT.HEAD ' + head + ' ],\
                 #comps &\
                 [ LOCAL.CAT.VAL [ SPR < >, COMPS < >, SUBJ < > ] ] > ].'
    mylang.add(typedef,section='verblex')


def is_nominalized_complement(cs):
    return 'nominalization' in [ f['name'] for f in cs['feat'] ]

def customize_clausal_verb(clausalverb,mylang,ch,cs,extra):
    if not cs['cformvalue']:
        constrain_for_features(clausalverb,cs,mylang,
                               'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.',ch,is_nominalized_complement(cs))
    else:
        mylang.add(clausalverb + ' := [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.FORM '\
                   + cs['cformvalue'] + ' ] > ].'
                    , merge=True)
    supertype = clausalverb_supertype(ch, cs)
    mylang.add(clausalverb +' := ' + supertype + '.',merge=True)
    if extra:
        val = None
        if cs[EXTRA] and not cs[SAME]:
            val = '+'
        elif cs[SAME] and not cs[EXTRA]:
            val = '-'
        if val:
            mylang.add(clausalverb + ' := [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.EXTRA '+ val + ' ] > ].'
                       , merge=True)


'''
Semantically non-empty nominalization requires that
the clausal verb takes nominalization_rel's ARG0
directly as its ARG2. In other cases, it wants
the embedded verb's handle.
'''
def clausalverb_supertype(ch, cs):
    supertype = None
    for f in cs['feat']:
        if f['name'] == 'nominalization':
            for ns in ch['ns']:
                if ns['name'] == f['value']:
                    if ns['nmzRel'] == 'yes' or ns['level'] in ['mid','low']:
                        supertype = 'transitive-lex-item'
    if not supertype:
        supertype = 'clausal-second-arg-trans-lex-item'
    return supertype


# This is currently called by lexical_items.py
def update_verb_lextype(ch,verb, vtype):
    suffix = ''
    head = ''
    val = verb.get(constants.VALENCE)
    for ccs in ch.get(COMPS):
        if val.endswith(ccs.full_key):
            suffix = val
            head = determine_clausal_verb_comp_head(ccs)
    if suffix:
        name = vtype[0:vtype.find('verb-lex')-1]
        #rest = 'clausal-verb-lex'
        rest = 'verb-lex'
        vtype = name + '-' + rest
    return vtype,head

def nonempty_nmz(cs,ch):
    for f in cs['feat']:
        if f['name'] == 'nominalization':
            for ns in ch['ns']:
                if ns['name'] == f['value']:
                    if ns['nmzRel'] == 'yes' or ns['level'] in ['mid','low']:
                        return True
    return False

def extraposed_comps(ch):
    return len([css for css in ch.get('comps') if css['clause-pos-extra']]) > 0

def nominalized_comps(ch):
    for ccs in ch.get(COMPS):
        for f in ccs['feat']:
            if f['name'] == 'nominalization':
                return True

def validate(ch,vr):
    if not ch.get(COMPS):
        pass
    matches = {}
    wo = ch.get(constants.WORD_ORDER)
    for v in ch.get('verb'):
        if get_name(v) == 'clausal':
            vr.err("The word 'clausal' is reserved; please use another name.")
    for ccs in ch.get(COMPS):
        if wo in ['free','v2']:
            vr.warn(ccs.full_key + '_'+ SAME,WO_WARNING)
        matches[ccs.full_key] = None
        for vb in ch.get('verb'):
            val = vb['valence']
            if val.endswith(ccs.full_key):
                matches[ccs.full_key] = vb.full_key
        for m in matches:
            if not matches[m]:
                vr.err(ccs.full_key + '_' + SAME,
                       'You did not enter any verbs in the Lexicon to go with this complementation strategy.')
        if not (ccs[EXTRA] or ccs[SAME]):
            vr.err(ccs.full_key + '_' + SAME, SAME_OR_EXTRA)
        if ccs[EXTRA]:
            if wo in ['free','v2','svo','vso']:
                vr.err(ccs.full_key + '_' + EXTRA,EXTRA_VO)
        for f in ccs['feat']:
            feat = find_in_other_features(f['name'],ch)
            if feat:
                if feat['type'] and feat['type'] == 'index':
                    vr.err(f.full_key + '_name','Custom semantic features are not supported here.')
        if ccs['cformvalue'] and not ccs[COMP] == 'oblig':
            vr.err(ccs.full_key + '_' + COMP,
                   'FORM on complementizers is only supported with obligatory complementizers.')
        if ccs['complementizer'] and not (ccs[COMP] == 'oblig' or ccs[COMP] == 'opt'):
            vr.err(ccs.full_key + '_' + COMP,
                   'Please choose whether the complementizer is obligatory or optional.')

def find_in_other_features(name,ch):
    for f in ch.get('feature'):
        if f['name'] == name:
            return f
    return None