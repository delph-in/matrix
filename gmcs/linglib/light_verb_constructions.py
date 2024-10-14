# imports for typing
from __future__ import annotations      # makes it so you don't need to wrap
                                        # types like ValidationResult in quotes
from typing import Dict, TYPE_CHECKING
from gmcs.lib import TDLHierarchy
from gmcs.tdl import TDLfile
from gmcs.choices import ChoicesFile
# to avoid circular import errors
if TYPE_CHECKING:
    from gmcs.validate import ValidationResult

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


#############################
### LVC feature functions ###
#############################

def init_light_verb_hierarchy(ch: ChoicesFile, hierarchies: Dict[str, TDLHierarchy]):
    """
    Initialize hiearchy for LVC feature.
    """
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


def customize_light_verb(mylang: TDLfile, hierarchies: Dict[str, TDLHierarchy]):
    """
    Create type definition allowing LVC feature.
    """
    if LVC_TYPE in hierarchies:
        mylang.add('+nv :+ [ LVC lvc ].', section='addenda')
        hierarchies[LVC_TYPE].save(mylang)


###############################
### Customization functions ###
###############################

def customize_lvc(ch: ChoicesFile, mylang: TDLfile, rules: TDLfile):
    """
    Customize light verb constructions.
    """
    if ch.get('coverb-n') == ON or ch.get('coverb-v') == ON :
        create_lvc_phrase_types(ch, mylang, rules)


def create_lvc_phrase_types(ch: ChoicesFile, mylang: TDLfile, rules: TDLfile):
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
        add_lvc_phrase(ch, mylang, rules, True, False, head_type_suffix)
    # coverb before light verb
    elif lvc_wo == 'cv-lv':
        add_lvc_phrase(ch, mylang, rules, False, True, head_type_suffix)
    # coverb before/after light verb
    elif lvc_wo == 'both':
        add_lvc_phrase(ch, mylang, rules, True, True, head_type_suffix)


def add_lvc_phrase(ch: ChoicesFile, mylang: TDLfile, rules: TDLfile, lv_cv: bool, cv_lv: bool, head_type_suffix: str):
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


########################
### Helper functions ###
########################

# Used in lexicon.py
def interpret_lv_valence(valence: str) -> str:
    """
    Return the canonical valence name (e.g. lv-iverb, lv-tverb) given the
    valence for a light verb as defined in a choices file.
    """
    if valence == 'coverb-1comp':
        return 'lv-tverb'
    else:
        return 'lv-iverb'


# Used in lexicon.py
def interpret_cv_valence(valence: str) -> str:
    """
    Return the canonical valence name (e.g. lv-iverb, lv-tverb) given the
    valence for a light verb as defined in a choices file.
    """
    if valence == 'coverb-1comp':
        return 'cv-tverb'
    else:
        return 'cv-iverb'


# Used in morphotactics.py
def fix_coverb_pc_inputs(pc_inputs, ch):
    """
    Return pc inputs containing correct input name(s) for coverbs.
    """
    new_pc_inputs = {}

    for pc in pc_inputs:
        new_pc_inputs[pc] = set()
        for inp in pc_inputs[pc]:
            cv_type = ch.get(inp).get('coverb-type')
            if cv_type == 'cv-only':
                # no regular; yes coverb
                new_pc_inputs[pc].add(inp + '-coverb')
            elif cv_type == 'cv-opt':
                # yes regular; yes coverb
                new_pc_inputs[pc].add(inp)
                new_pc_inputs[pc].add(inp + '-coverb')
            else:
                # yes regular; no coverb
                new_pc_inputs[pc].add(inp)

    return new_pc_inputs

############################
### Validation functions ###
############################

def validate_lvc(ch: ChoicesFile, vr: ValidationResult):
    """
    Validate the user's choices about light verb constructions.
    """
    # if any LVC options are selected, make sure coverb-n/coverb-v is selected
    valid = True
    for option in ['lvc-word-order', 'lvc-adjacent', 'lvc-it', 'lvc-tr']:
        if ch.get(option):
            if not ch.get('coverb-n') and not ch.get('coverb-v'):
                valid = False
    if not valid:
        msg = 'If a light verb construction option has been selected, you ' + \
              'must specify that either noun or verb coverbs are allowed.'
        vr.err('coverb-n', msg)

    # make sure whether dependents can be picked up is selected
    if ch.get('coverb-n') == ON:
        if not ch.get('lvc-noun-cv-dep'):
            msg = 'If noun coverbs are allowed, you must specify ' + \
                  'whether they can pick up dependents.'
            vr.err('lvc-noun-cv-dep', msg)
    if ch.get('coverb-v') == ON:
        if not ch.get('lvc-verb-cv-dep'):
            msg = 'If verb coverbs are allowed, you must specify ' + \
                  'whether they can pick up dependents.'
            vr.err('lvc-verb-cv-dep', msg)
    
    # if whether dependents can be pick up is selected, make sure coverb-n/coverb-v is selected
    if ch.get('lvc-noun-cv-dep'):
        if not ch.get('coverb-n'):
            msg = 'If you specify whether noun coverbs can pick up ' + \
                  'dependents, you must specify that noun coverbs are allowed.'
            vr.err('coverb-n', msg)
    if ch.get('lvc-verb-cv-dep'):
        if not ch.get('coverb-v'):
            msg = 'If you specify whether verb coverbs can pick up ' + \
                  'dependents, you must specify that verb coverbs are allowed.'
            vr.err('coverb-v', msg)

    # if coverbs are allowed, make sure all LVC options have a selection
    if ch.get('coverb-n') == ON or ch.get('coverb-v') == ON:
        if not ch.get('lvc-word-order'):
            msg = 'If coverbs are allowed, you must specify the ' + \
                'word order within an LVC.'
            vr.err('lvc-word-order', msg)
        if not ch.get('lvc-adjacent'):
            msg = 'If coverbs are allowed, you must specify whether ' + \
                  'a coverb is immediately adjacent to a light verb.'
            vr.err('lvc-adjacentr', msg)
        if not ch.get('lvc-it') and not ch.get('lvc-tr'):
            msg = 'If coverbs are allowed, you must specify at least ' + \
                  'one possible valence option.'
            vr.err('lvc-it', msg)