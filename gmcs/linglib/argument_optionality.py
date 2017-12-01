from gmcs.linglib import morphotactics
from gmcs.linglib import features
from gmcs.utils import get_name

# SS 2009-06-07 added check to see if a const rule which changes
# the COMPS of the mother to OPT - is needed.  The code assumes
# that a given position class will have the same co-occurrence
# restrictions for all lexical rule types. i.e. if one LRT is
# not permitted to appear with an overt argument but is required
# with a dropped argument, all the other LRTs in this PC will
# have the same restrictions.  This is necessary because the
# const rule that is generated will change the value of the
# COMPS of the mother OPT - for all items which are not marked
# by one of LRTs in this PC.
#
# SS 2009-06-07 Now adding capability for when the marker is not
# permitted with a dropped argument and is required (or
# optional) overt argument.  This is done by increasing the
# subrules count just like above.  The subrules created are
# different.

######################################################################
# customize_arg_op()
#   Create phrase-structure and lexical rules associated with user's
#   choices on argument optionality page.

def customize_arg_op(mylang, ch, rules, hierarchies):
    """ Create the lexical types, lexical, rules and phrase structure
        rules to allow argument dropping"""

    if 'scale' in ch and (ch.get('subj-drop')or ch.get('obj-drop')):
        mylang.add('dir-inv-scale := unexpressed-reg')

    mylang.set_section('verblex')
    ##Adding potential fix for integrating argument optionality and direct-inverse

    #Figure out the constraints on subject dropping and write the
    #appropriate types to mylang.tdl or rules.tdl

    if ch.get('subj-drop') == 'subj-drop-all' and not (ch.get('subj-con') == 'subj-con-some'):
        rules.add('decl-head-opt-subj := decl-head-opt-subj-phrase.')
    if ch.get('subj-drop') == 'subj-drop-lex' and not (ch.get('subj-con') == 'subj-con-some'):
        rules.add('decl-head-opt-subj := decl-head-opt-subj-phrase.')
        mylang.add('no-subj-drop-verb-lex := verb-lex &\
                         [SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].')
        mylang.add('subj-drop-verb-lex := verb-lex.')


    #Figure out the constraints on object dropping and write the
    #appropriate types to mylang.tdl or rules.tdl
    if ch.get('obj-drop')=='obj-drop-all':
        rules.add('basic-head-opt-comp := basic-head-opt-comp-phrase.')

    if ch.get('obj-drop') == 'obj-drop-lex':
        rules.add('basic-head-opt-comp := basic-head-opt-comp-phrase.')
        mylang.add('no-obj-drop-verb-lex := transitive-verb-lex &\
                        [SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].')
        mylang.add('obj-drop-verb-lex := transitive-verb-lex.')

    if ch.get('subj-drop') == 'subj-drop-lex' and ch.get('obj-drop') == 'obj-drop-lex':
        mylang.add('subj-drop-only-verb-lex := subj-drop-verb-lex & no-obj-drop-verb-lex.')
        mylang.add('obj-drop-only-verb-lex := obj-drop-verb-lex & no-subj-drop-verb-lex.')
        mylang.add('subj-obj-drop-verb-lex := subj-drop-verb-lex & obj-drop-verb-lex.')
        mylang.add('no-drop-verb-lex := no-subj-drop-verb-lex & no-obj-drop-verb-lex.')

    mylang.set_section('phrases')

    #Create phrase-structure rules for each context
    for context in ch.get('context'):
        name = 'context' + str(context.iter_num())
        ptype = name + '-decl-head-opt-subj-phrase'
        features.customize_feature_values(mylang, ch, hierarchies, context, ptype, 'con')
        mylang.add(ptype + ':= decl-head-opt-subj-phrase.')
        rules.add(name + '-decl-head-opt-subj := '+ name + '-decl-head-opt-subj-phrase.')

    #Trying to get co-occurrence of marker dropping to work

    if (ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and (ch.get('subj-mark-drop')== 'subj-mark-drop-opt'or ch.get('subj-mark-drop')=='subj-mark-drop-req')):
        mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

    if ((ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-not' and ch.get('obj-mark-drop') == 'obj-mark-drop-req') or ((ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt' and ch.get('obj-mark-drop') == 'obj-mark-drop-req'))):
        mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

    if ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-not' and ch.get('obj-mark-drop') == 'obj-mark-drop-opt' :
        mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

    if ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-req' and ch.get('obj-mark-drop') == 'obj-mark-drop-not' :
        mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

    if ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt' and ch.get('obj-mark-drop') == 'obj-mark-drop-not' :
        mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

    if ch.get('obj-mark-drop')== 'obj-mark-drop-opt' and ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-req':
        mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

    if ch.get('subj-mark-drop')== 'subj-mark-drop-opt' and ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-req':
        mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

    if ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and ch.get('subj-mark-drop') == 'subj-mark-drop-opt' :
        mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

    if ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-req' and ch.get('subj-mark-drop') == 'subj-mark-drop-not' :
        mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

    if ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-opt' and ch.get('subj-mark-drop') == 'subj-mark-drop-not' :
        mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

    #def customize_subj_phrase(phrase)
    #Trying to get the subject/object marker co-occurrence to work out
    #if (ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and (ch.get('subj-mark-drop')== 'subj-mark-drop-opt'or ch.get('subj-mark-drop')=='subj-mark-drop-req')):
    # mylang.add(phrase + ':= [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT +].', merge = True)


def add_lexrules(choices):
    '''
    '''
    for pc in morphotactics.all_position_classes(choices):
        pc_key = pc.full_key
        idx = pc['lrt'].next_iter_num() if 'lrt' in pc else 1
        for lrt in pc.get('lrt',[]):
            overt = [f for f in lrt.get('feat',[]) if f['name']=='overt-arg']
            dropped = [f for f in lrt.get('feat',[]) if f['name']=='dropped-arg']
            need_lex_rule = need_no_drop_rule('obj-mark', choices) or \
                            need_no_drop_rule('subj-mark', choices)
            # overt-arg morphs should be the index of the next available
            if overt:
                feat = overt[0]
                # convert overt-arg features to OPT
                if feat['value'] == 'not-permitted':
                    feat['name'] = 'OPT'
                    feat['value'] = 'plus'
                # only create a lexical rule if necessary
                if need_lex_rule:
                    key = pc.full_key + '_lrt' + str(idx)
                    name = get_name(pc) + '-no-drop'
                    choices[key + '_name'] = name
                    choices[key + '_feat1_name'] = 'OPT'
                    choices[key + '_feat1_value'] = 'minus'
                    choices[key + '_feat1_head'] = feat['head']
                    choices[key + '_lri1_inflecting'] = 'no'
                    choices[key + '_lri1_orth'] = ''
            # dropped-arg morphs should be the index of the next available + 1
            if dropped:
                feat = dropped[0]
                # convert dropped-arg features to OPT
                if feat['value'] == 'not-permitted':
                    feat['name'] = 'OPT'
                    feat['value'] = 'minus'
                # only create a lexical rule if necessary
                if need_lex_rule:
                    key = pc.full_key + '_lrt' + str(idx + 1)
                    name = get_name(pc) + '-drop'
                    choices[key + '_name'] = name
                    choices[key + '_feat1_name'] = 'OPT'
                    choices[key + '_feat1_value'] = 'plus'
                    choices[key + '_feat1_head'] = feat['head']
                    choices[key + '_lri1_inflecting'] = 'no'
                    choices[key + '_lri1_orth'] = ''

def need_no_drop_rule(obj_subj, choices):
    '''
    Return True if the unordered values of the pair
    ((obj|subj)-mark-drop, (obj|subj)-mark-no-drop) is a valid pattern
    for needing a separate morpheme.
    '''
    patterns = (set(['req','not']), set(['req','opt']))
    if set([choices[obj_subj + '-drop'].split('-')[-1],
            choices[obj_subj + '-no-drop'].split('-')[-1]]) in patterns:
        return True
    return False

##################
### VALIDATION ###
##################

def validate(choices):
    # add validation tests specific to this module
    pass
