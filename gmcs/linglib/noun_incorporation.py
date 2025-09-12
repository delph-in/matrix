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

PROMOTION_RULE = ':= \
                    [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                                COMPS < [ LOCAL [ CAT [ NCORP-MOD -,\
                                                                        VAL.SPR < >,\
                                                                        HEAD noun ],\
                                                                CONT.HOOK.INDEX #arg2 ] ] > ],\
                        DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj,\
                                                    COMPS < [ LOCAL.CONT.HOOK.INDEX #arg1 ] > ],\
                        C-CONT.RELS.LIST < arg12-ev-relation & \
                                            [ PRED "poss_rel",\
                                            ARG1 #arg1,\
                                            ARG2 #arg2 ] > ].'

REDUCTION_RULE = ':= no-ccont-lex-rule & \
             [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj,\
                                     COMPS < > ],\
             DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ #subj ].'

NI_VALENCE = ':= \
            [ SYNSEM.LOCAL.CAT.VAL [ SPEC < >, \
                                    SPR < > ] ].'

DOUBLE_RULE = ':= \
            [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                     COMPS < [ LOCAL [ CAT.NCORP-MOD +, \
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


            for kind in ['promote', 'reduce', 'double']:
                if ch.get(kind) == 'on':
                    if ch[key + '_lrt']:
                        idx = ch[key + '_lrt'].next_iter_num() # i think this will be a problem
                    else:
                        idx = 1
                    lrt_key = key + '_lrt' + str(idx)
                    ch[lrt_key + '_name'] = kind
                    ch[lrt_key + '_lri_inflecting'] = 'no'


def customize_noun_incorporation(ch, mylang):
    if ch.get('noun-incorp') == 'on':
        for vpc in ch['verb-pc']:
            for lrt in vpc['is-lrt']:
                #print(lrt.identifier())
                #lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + 
                                                #['add-only-rule']) # not sure which rule needs to be added here
                mylang.add(get_name(vpc)+ '-lex-rule-super ' + basic_noun_incorp_def,
                           merge=True, section='lexrules')
                
