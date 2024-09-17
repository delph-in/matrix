from gmcs.lib import TDLHierarchy
from gmcs.constants import ON, YES, INTRANSITIVE, TRANSITIVE

###########################################
### CONSTANTS (specific to this module) ###
###########################################

LVC_TYPE = 'lvc'
LV_NONE_TYPE = 'lv-none'
LV_ALL_TYPE = 'lv-all'

COVERB_VERB = 'verb'
COVERB_NOUN = 'noun'

COVERB_NOUN_ITEM = 'coverb-' + COVERB_NOUN + '-lex := basic-noun-lex & \
    [ SYNSEM.LOCAL.CAT [ HEAD.MOD < >, \
                        VAL [ COMPS < >, \
                            SPR < #spr & [ LOCAL.CAT.HEAD det ] > ] ], \
    ARG-ST < #spr > ].'

COVERB_INTRANS_VERB_ITEM = 'coverb-' + INTRANSITIVE + '-' + COVERB_VERB + '-lex := intransitive-verb-lex.'

COVERB_TRANS_VERB_ITEM = 'coverb-' + TRANSITIVE + '-' + COVERB_VERB + '-lex := transitive-verb-lex.'

LV_ITEM = 'lv-lex := [ SYNSEM [ LOCAL [ CAT [ HEAD.LVC ' + LV_NONE_TYPE + ', \
                             VAL.COMPS.FIRST #comps ], \
                       CONT.HOOK [ CLAUSE-KEY #clause, \
                                   LTOP #ltop ] ], \
               LKEYS.KEYREL.ARG1 #ind1 ], \
       ARG-ST [ FIRST.LOCAL [ CAT cat-sat, \
                              CONT.HOOK [ INDEX ref-ind & #ind1, \
                                          ICONS-KEY.IARG1 #clause ] ], \
                REST.FIRST #comps & [ LOCAL [ CAT cat-sat, \
                                              CONT.HOOK [ ICONS-KEY.IARG1 #clause, \
                                                          XARG #ind1, \
                                                          LTOP #ltop ] ] ] ] ].'

LV_NOUN_ITEM = COVERB_NOUN + '-lv-lex := lv-lex & \
    [ SYNSEM [ LOCAL.CAT.VAL.COMPS.FIRST #comps, \
               LKEYS.KEYREL.ARG2 #ind2 ], \
      ARG-ST.REST.FIRST #comps & [ LOCAL [ CAT cat-sat & [ VAL.SPR < > ], \
                                           CONT.HOOK.INDEX ref-ind & #ind2 ] ] ].'

LV_VERB_ITEM = COVERB_VERB + '-lv-lex := lv-lex & \
    [ SYNSEM [ LOCAL.CAT.VAL.COMPS.FIRST #comps, \
               LKEYS.KEYREL.ARG2 #ind2 ], \
      ARG-ST.REST.FIRST #comps & [ LOCAL.CONT.HOOK.INDEX event & #ind2 ] ].'

LV_IT_ITEM = INTRANSITIVE + '-lv-lex := non-mod-lex-item.'

LV_TR_ITEM = TRANSITIVE + '-lv-lex := non-mod-lex-item & non-local-none-no-hcons & basic-icons-lex-item & \
    [ SYNSEM [ LOCAL [ CAT.VAL [ COMPS < [], [ LOCAL [ CAT cat-sat & [ VAL.SPR < > ], \
                                                        CONT.HOOK [ INDEX ref-ind & #ind3, \
                                                                    ICONS-KEY.IARG1 #clause ] ] ] > ], \
                       CONT.HOOK.CLAUSE-KEY #clause ], \
                LKEYS.KEYREL.ARG3 #ind3, \
                LIGHT + ] ].'

LV_IT_NOUN_ITEM = INTRANSITIVE + '-' + COVERB_NOUN + '-lv-lex := ' + COVERB_NOUN + '-lv-lex & ' + INTRANSITIVE + '-lv-lex.'

LV_IT_VERB_ITEM = INTRANSITIVE + '-' + COVERB_VERB + '-lv-lex := ' + COVERB_VERB + '-lv-lex & ' + INTRANSITIVE + '-lv-lex.'

LV_TR_NOUN_ITEM = TRANSITIVE + '-' + COVERB_NOUN + '-lv-lex := ' + COVERB_NOUN + '-lv-lex & ' + TRANSITIVE + '-lv-lex.'

LV_TR_VERB_ITEM = TRANSITIVE + '-' + COVERB_VERB + '-lv-lex := ' + COVERB_VERB + '-lv-lex & ' + TRANSITIVE + '-lv-lex.'


#################################################################
# customize_light_verb()
#   Create the type definitions associated with the user's
#   choices about light verbs.

def init_light_verb_hierarchy(ch, hierarchies):
    if ch.get('coverb-n') == ON or ch.get('coverb-v') == ON :
        hier = TDLHierarchy(LVC_TYPE)

        hier.add(LV_NONE_TYPE, LVC_TYPE)
        hier.add(LV_ALL_TYPE, LVC_TYPE)

        if ch.get('lvc-it') == ON:
            hier.add(LV_ALL_TYPE + '-' + INTRANSITIVE, LV_ALL_TYPE)
        if ch.get('lvc-tr') == ON:
            hier.add(LV_ALL_TYPE + '-' + TRANSITIVE, LV_ALL_TYPE)

        for lv in ch.light_verbs():
            hier.add('lv-' + lv[0], lv[1])

        if not hier.is_empty():
            hierarchies[hier.name] = hier


def customize_light_verb(mylang, hierarchies):
    if LVC_TYPE in hierarchies:
        mylang.add('+nv :+ [ LVC lvc ].', section='addenda')
        hierarchies[LVC_TYPE].save(mylang)

#################################################################


###################################################
### Primary function (called from customize.py) ###
###################################################

def customize_lvc(mylang, ch, rules):
    """
    Customize light verb constructions.
    """
    if ch.get('coverb-n') == ON or ch.get('coverb-v') == ON :
        create_lvc_phrase_types(mylang, ch, rules)


########################
### Helper functions ###
########################

def create_lvc_phrase_types(mylang, ch, rules):
    """
    Create/modify phrasal types for light verb constructions.
    """
    mylang.add_literal(';;; LVC-related phrasal types', section='phrases')

    wo = ch.get('word-order')
    lvc_wo = ch.get('lvc-word-order')
    head_type_suffix = ''

    # head-comp
    if wo in ['svo', 'vos', 'vso', 'v-initial']:
        # prevents LVCs from combining using this rule
        mylang.add('head-comp-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC ' + LV_NONE_TYPE + ' ].', section='phrases')

    # comp-head
    if wo in ['sov', 'osv', 'ovs', 'v-final']:
        # prevents LVCs from combining using this rule
        mylang.add('comp-head-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC ' + LV_NONE_TYPE + ' ].', section='phrases')

    # head-comp and comp-head
    if wo in ['free', 'v2']:
        head_type_suffix = '-head-nexus'
        # prevents LVCs from combining using these rules
        mylang.add('head-comp-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC ' + LV_NONE_TYPE + ' ].', section='phrases')
        mylang.add('comp-head-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC ' + LV_NONE_TYPE + ' ].', section='phrases')
        mylang.add('head-comp-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC ' + LV_NONE_TYPE + ' ].', section='phrases')
        mylang.add('comp-head-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC ' + LV_NONE_TYPE + ' ].', section='phrases')

    # coverb after light verb
    if lvc_wo == 'lv-cv':
        add_lvc_phrase(mylang, ch, True, False, head_type_suffix, rules)
    # coverb before light verb
    elif lvc_wo == 'cv-lv':
        add_lvc_phrase(mylang, ch, False, True, head_type_suffix, rules)
    # coverb before/after light verb
    elif lvc_wo == 'both':
        add_lvc_phrase(mylang, ch, True, True, head_type_suffix, rules)


def add_lvc_phrase(mylang, ch, lv_cv, cv_lv, head_type_suffix, rules):
    """
    Add LVC phrasal types based on the order of the light verb and
    coverb within the LVC. Add LVC rule instances.
    """
    if lv_cv:
        rules.add('head-comp-lvc := head-comp-phrase-lvc.')

        # allows for formation of LVC
        mylang.add('head-comp-phrase-lvc := basic-head-1st-comp-phrase & head-initial' + head_type_suffix + ' & \
            [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.LVC ' + LV_ALL_TYPE + ' ].', section='phrases')

    if cv_lv:
        rules.add('comp-head-lvc := comp-head-phrase-lvc.')

        # allows for combination of coverb + light verb first
        mylang.add('head-final-lvc := head-final & \
            [ SYNSEM.ATTACH lmod ].', section='phrases')

        # prevents object from combining w/ coverb in "subj obj coverb lv" 
        # before coverb + lv have combined
        mylang.add('decl-head-subj-phrase :+ \
            [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC ' + LV_NONE_TYPE + ' ].', section='phrases')

        # allows for formation of LVC
        mylang.add('comp-head-phrase-lvc := basic-head-1st-comp-phrase & head-final-lvc & \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.LVC ' + LV_ALL_TYPE + ' ].', section='phrases')

    if ch.get('coverb-n') == ON:
        if ch.get('lvc-noun-cv-dep') == YES:
            # coverb takes noun-normal dependents
            pass
        else:
            # coverb doesn't take noun-normal dependents
            mylang.add('lv-' + COVERB_NOUN + '-lex := \
                [ SYNSEM [ LOCAL.CAT.VAL.COMPS.FIRST #comps ], \
                    ARG-ST.REST.FIRST #comps & [ LIGHT + ] ].', section='phrases')

    if ch.get('coverb-v') == ON:
        if ch.get('lvc-verb-cv-dep') == YES:
            # coverb takes verb-normal dependents
            pass
        else:
            # coverb doesn't take verb-normal dependents
            mylang.add('lv-' + COVERB_VERB + '-lex := \
                [ SYNSEM [ LOCAL.CAT.VAL.COMPS.FIRST #comps ], \
                    ARG-ST.REST.FIRST #comps & [ LIGHT + ] ].', section='phrases')

    if ch.get('lvc-adjacent') == YES:
        # coverb must be immediately adjacent to light verb
        if lv_cv:
            mylang.add('head-comp-phrase-lvc := [ HEAD-DTR.SYNSEM.LIGHT + ].', section='phrases')
        if cv_lv:
            mylang.add('comp-head-phrase-lvc := [ HEAD-DTR.SYNSEM.LIGHT + ].', section='phrases')
    else:
        # coverb doesn't have to be immediately adjacent to light verb
        pass

