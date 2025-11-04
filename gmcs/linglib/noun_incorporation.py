from gmcs.choices import ChoicesFile
from gmcs.choices import ChoicesFile, ChoiceDict
from gmcs.utils import get_name


""" def customize_ni(ch, mylang):
# If noun incorporation, add to mylanguage.tdl
    if ch.get("noun_incorp", False):
        mylang.add('''incorporated_noun-lex-rule := add-only-rule & infl-lex-rule & NI-valence-rule-dtr &
                    [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CONT.HOOK [ INDEX #ind,
                                                                        LTOP #larg ] ] >,
                        C-CONT [ HCONS.LIST < qeq & 
                                                [ HARG #harg, 
                                                    LARG #larg ] >,
                                  RELS.LIST < noun-relation &
                                        [ ARG0 #ind,
                                            LBL #larg ],
                                            quant-relation & 
                                        [ PRED "exist_q_rel",
                                            ARG0 #ind,
                                            RSTR #harg ] > ] ].''',
                    comment='Noun Incorporation',
                    section='lexrules')
        
        mylang.add('''NI-valence-lex-rule-super := add-ccont-val-change-only-lex-rule & const-lex-rule &
                   [ SYNSEM.LOCAL.CAT.VAL [ SPEC < >,
                                            SPR < > ] ].''',
                    section='lexrules')
        
        #mylang.add('''NI-valence-rule-dtr := word-or-lexrule''',
                   #section='lexrules') # I want to get this in intermediate types
        
        if ch.get("promotion", False):
            mylang.add('''promote-lex-rule := NI-valence-lex-rule-super & 
                        [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, 
                                                COMPS < [ LOCAL [ CAT [ NCORP-MOD -,
                                                                        VAL.SPR < >,
                                                                        HEAD noun ],
                                                                CONT.HOOK.INDEX #arg2 ] ] > ],
                        DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj,
                                                    COMPS < [ LOCAL.CONT.HOOK.INDEX #arg1 ] > ],
                        C-CONT.RELS.LIST < arg12-ev-relation & 
                                            [ PRED "poss_rel",
                                            ARG1 #arg1,
                                            ARG2 #arg2 ] > ].''',
                        section='lexrules')
        if ch.get("reduction", False):
            mylang.add('''reduction-lex-rule := NI-valence-lex-rule-super & 
                       [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj,
                                                COMPS < > ],
                       DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ #subj ].''',
                       section='lexrules') """

basic_noun_incorp_def = ''':= \
                    [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CONT.HOOK [ INDEX #ind,\
                                                                        LTOP #larg ] ] >,\
                    C-CONT [ HCONS.LIST < qeq &\
                                            [ HARG #harg, \
                                                LARG #larg ] >,\
                                RELS.LIST < noun-relation &\
                                            [ ARG0 #ind,\
                                            LBL #larg ],\
                                            quant-relation &\
                                                [ PRED "exist_q_rel",\
                                                    ARG0 #ind,\
                                                    RSTR #harg ] > ] ]. '''  

# OPT - going in here, testing to see if this works for all the grammars
# 9/24/25 making LBL of the poss_rel be the same as the LBL of the possessum
# 11/5/25 copying up head value of comps so that new comp behaves how the old one should have
PROMOTION_POSS = ':= \
                    [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                             COMPS < [ OPT -, \
                                                       LOCAL [ CAT [ NCORP-MOD #nc-mod,\
                                                                     VAL.SPR < >,\
                                                                     HEAD noun & \
                                                                        #head ],\
                                                               CONT.HOOK.INDEX #arg2 ] ] > ],\
                        DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                                   COMPS  < [ LOCAL [ CAT [ NCORP-MOD #nc-mod,\
                                                                            HEAD #head ], \
                                                                      CONT.HOOK [ INDEX #arg1, \
                                                                                  LTOP #lbl ] ] ] > ], \
                        C-CONT.RELS.LIST < arg12-ev-relation & \
                                            [ PRED "poss_rel", \
                                              LBL #lbl, \
                                              ARG1 #arg1, \
                                              ARG2 #arg2 ] > ].'

PROMOTION_OBLIQUE = ':= \
                        [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                                COMPS < [ OPT -, \
                                                          LOCAL [ CAT [ NCORP-MOD -,\
                                                                        VAL.SPR < >,\
                                                                        HEAD noun & \
                                                                            #head ],\
                                                                CONT.HOOK.INDEX #arg2 ] ] > ],\
                        DTR.SYNSEM.LOCAL [ CAT.VAL [ COMPS < [ LOCAL.CAT.HEAD #head ] >, \
                                                    SUBJ #subj ], \
                                           CONT.HOOK.INDEX #arg1 ], \
                        C-CONT.RELS.LIST < arg12-ev-relation & \
                                            [ ARG1 #arg1, \
                                             ARG2 #arg2 ] > ].'

INTRANS_REDUCTION_RULE = ':= val-change-only-lex-rule & \
             [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < [ LOCAL [ CONT.HOOK #hook, \
                                                       CAT [ NCORP-MOD #nc-mod, \
                                                             VAL #val, \
                                                             HEAD +np ] ] ] >,\
                                      COMPS < > ],\
             DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL [ CONT.HOOK #hook, \
                                                       CAT [ NCORP-MOD #nc-mod, \
                                                             VAL #val ] ] ] > ] ].'

TRANS_REDUCTION_RULE = ':= no-ccont-lex-rule & \
                       [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                                COMPS #comps & \
                                                    [ FIRST.OPT + ] ], \
                         DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                                    COMPS #comps ] ].'

NI_VALENCE = ':= \
            [ SYNSEM.LOCAL.CAT.VAL [ SPEC < >, \
                                    SPR < > ] ].'

# separating double and strand rules for different forbid constraints
DOUBLE_RULE = ':= \
            [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                     COMPS < [ OPT -, \
                                               LOCAL [ CAT [ NCORP-MOD +, \
                                                             HEAD noun ],\
                                                       CONT.HOOK [ LTOP #ltop, \
                                                                   INDEX #ind ] ] ] > ], \
              DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                         COMPS < [ LOCAL.CONT.HOOK [ LTOP #ltop, \
                                                                     INDEX #ind ] ] > ], \
              C-CONT [ RELS.LIST < >, \
                       HCONS.LIST < >, \
                       ICONS.LIST < > ] ].'

STRAND_RULE = ':= \
            [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                     COMPS < [ OPT -, \
                                               LOCAL [ CAT [ NCORP-MOD +, \
                                                             HEAD adj ], \
                                                       CONT.HOOK [ LTOP #ltop, \
                                                                   INDEX #ind ] ] ] > ], \
              DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                         COMPS < [ LOCAL.CONT.HOOK [ LTOP #ltop, \
                                                                     INDEX #ind ] ] > ], \
              C-CONT [ RELS.LIST < >, \
                       HCONS.LIST < >, \
                       ICONS.LIST < > ] ].'

LEX_ITEM = 'lex-item :+ [ SYNSEM.LOCAL.CAT [ NCORP-MOD -, \
                                             VAL [ SUBJ nc-list, \
                                                   COMPS nc-list, \
                                                   SPR nc-list, \
                                                   SPEC nc-list ] ] ].'

TYPE_MOD_PHRASE = 'type-ni-mod-phrase := unary-nonloc-phrase & head-only & \
                      [ SYNSEM.LOCAL.CAT [ WH #wh, \
                                           VAL #val, \
                                           NCORP-MOD + ], \
                        HEAD-DTR.SYNSEM.LOCAL [ CAT [ HEAD noun, \
                                                      WH #wh, \
                                                      VAL #val & \
                                                          [ SPR < > ], \
                                                      NCORP-MOD - ], \
                                                CONT.HOOK.INDEX #arg2 ], \
                        C-CONT [ RELS.LIST < arg12-ev-relation & \
                                            [ PRED "type_rel", \
                                              LBL #lbl, \
                                              ARG1 #ind, \
                                              ARG2 #arg2 ] >, \
                                 ICONS.LIST < >, \
                                 HCONS.LIST < >, \
                                 HOOK [ LTOP #lbl, \
                                        INDEX #ind ] ] ].'

ADJ_MOD_PHRASE = 'adj-ni-mod-phrase := unary-nonloc-phrase & head-only & \
                      [ SYNSEM.LOCAL.CAT [ WH #wh, \
                                           VAL #val, \
                                           NCORP-MOD + ], \
                        HEAD-DTR.SYNSEM.LOCAL [ CAT [ HEAD adj & \
                                                           [ MOD.FIRST.LOCAL.CONT.HOOK.INDEX #arg1 ],  \
                                                      WH #wh, \
                                                      VAL #val, \
                                                      NCORP-MOD - ], \
                                                CONT.HOOK.LTOP #ltop ], \
                        C-CONT [ RELS.LIST < >, \
                                 ICONS.LIST < >, \
                                 HCONS.LIST < >, \
                                 HOOK [ LTOP #ltop, \
                                        INDEX #arg1 ] ] ].'

BARE_NP = ':= \
        [ SYNSEM.LOCAL.CAT.NCORP-MOD - \
          HEAD-DTR.SYNSEM.LOCAL.CAT.NCORP-MOD - ].'

HEAD_COMMENT = "This rule identifies the HEAD value of the element on the daughter's COMPS list with that of the element on the mother's COMPS list, even though these two element themselves are not identified. The reasoning for this identification is to ensure that elements promoted to object position as a result of noun incorporation behave as the incorporated noun would have if it had existed in an unincorporated position."
             
            
def add_lexrules(ch):
    for pc in ch['verb-pc']:
        for lrt in pc['is-lrt']:
            idx = ch[pc.full_key[:-1]].next_iter_num()
            key = 'verb-pc' + str(idx)
            ch[key + '_name'] = 'NI-valence'
            ch[key + '_order'] = pc['order']
            ch[key + '_inputs'] = pc.full_key

            ch[key + '_require1_others'] = pc.full_key
            ch[pc.full_key + '_require1_others'] = key
            # need to eventually handle the index here

            # make ghost pc the input to whatever the IN pc used to be input to
            for pc_inp in ch['verb-pc']:
                if pc.full_key in pc_inp['inputs'] and pc_inp.full_key != key:
                    pc_inp['inputs'] = ', '.join([i for i in pc_inp['inputs'].split(', ') if i != pc.full_key] + [key])


            for ni_type in ['promote-poss', 'promote-obl', 'reduce', 'double-noun', 'strand-mod']:
                if ch.get(ni_type) == 'on':
                    if ch[key + '_lrt']:
                        idx = ch[key + '_lrt'].next_iter_num() # i think this will be a problem
                    else:
                        idx = 1
                    lrt_key = key + '_lrt' + str(idx)
                    ch[lrt_key + '_name'] = ni_type
                    ch[lrt_key + '_lri_inflecting'] = 'no'

                    #check for forbid constraints
                    for forbid in ch.get(f'{ni_type.split("-")[0]}-forbid', []):
                        string, value = str(forbid).removeprefix(f'{ni_type.split("-")[0]}-').split('=')
                        ch[lrt_key + '_' + string] = value



def customize_noun_incorporation(ch, mylang):
    if ch.get('noun-incorp') == 'on':
        for vpc in ch['verb-pc']:
            for lrt in vpc['is-lrt']:
                #print(lrt.identifier())
                #lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + 
                                                #['add-only-rule']) # not sure which rule needs to be added here
                mylang.add(get_name(vpc)+ '-lex-rule-super ' + basic_noun_incorp_def,
                           merge=True, section='lexrules')
                
