from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.utils import get_name
from gmcs.lib import TDLHierarchy
from gmcs.constants import ORTH, ON, YES
from gmcs.tdl import TDLparse

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

# all verb coverbs default to intransitive
COVERB_VERB_ITEM = 'coverb-' + COVERB_VERB + '-lex := intransitive-verb-lex.'

# bardi has inflected flags, need to figure out how to add that
# bardi inherits from various rule-dtr types
LV_ITEM = 'lv-lex := verb-lex & \
    [ SYNSEM [ LOCAL [ CAT [ HEAD.LVC lv-none, \
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

LV_NOUN_ITEM = 'lv-' + COVERB_NOUN + '-lex := lv-lex & \
    [ SYNSEM [ LOCAL.CAT.VAL.COMPS.FIRST #comps, \
               LKEYS.KEYREL.ARG2 #ind2 ], \
      ARG-ST.REST.FIRST #comps & [ LOCAL [ CAT cat-sat & [ VAL.SPR < > ], \
                                           CONT.HOOK.INDEX ref-ind & #ind2 ] ] ].'

LV_VERB_ITEM = 'lv-' + COVERB_VERB + '-lex := lv-lex & \
    [ SYNSEM [ LOCAL.CAT.VAL.COMPS.FIRST #comps, \
               LKEYS.KEYREL.ARG2 #ind2 ], \
      ARG-ST.REST.FIRST #comps & [ LOCAL.CONT.HOOK.INDEX event & #ind2 ] ].'

# bardi has inflected flags, need to figure out how to add them
LV_IT_ITEM = 'lv-it-lex := tr-min-rule-dtr & \
    [ SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.LOCAL.CAT.HEAD.CASE abs ].'

# bardi has inflected flags, need to figure out how to add them
LV_TR_ITEM = 'lv-tr-lex := non-local-none-no-hcons & basic-icons-lex-item & tr-aug-rule-dtr & tr-min-rule-dtr & \
    [ SYNSEM [ LOCAL [ CAT.VAL [ SUBJ.FIRST.LOCAL.CAT.HEAD.CASE erg, \
                                 COMPS < [], [ LOCAL [ CAT cat-sat & [ HEAD.CASE not-erg, \
                                                                       VAL.SPR < > ], \
                                                        CONT.HOOK [ INDEX ref-ind & #ind3, \
                                                                    ICONS-KEY.IARG1 #clause ] ] ] > ], \
                       CONT.HOOK.CLAUSE-KEY #clause ], \
                LKEYS.KEYREL.ARG3 #ind3, \
                LIGHT + ] ].'

LV_IT_NOUN_ITEM = 'lv-it-' + COVERB_NOUN + '-lex := lv-' + COVERB_NOUN + '-lex & lv-it-lex.'

LV_IT_VERB_ITEM = 'lv-it-' + COVERB_VERB + '-lex := lv-' + COVERB_VERB + '-lex & lv-it-lex.'

LV_TR_NOUN_ITEM = 'lv-tr-' + COVERB_NOUN + '-lex := lv-' + COVERB_NOUN + '-lex & lv-tr-lex.'

LV_TR_VERB_ITEM = 'lv-tr-' + COVERB_VERB + '-lex := lv-' + COVERB_VERB + '-lex & lv-tr-lex.'

######################################################################
# customize_light_verb()
#   Create the type definitions associated with the user's choices
#   about light verbs.

def init_light_verb_hierarchy(ch, hierarchies):
    if ch.get('coverb-n') == ON or ch.get('coverb-v') == ON :
        hier = TDLHierarchy(LVC_TYPE)

        hier.add(LV_NONE_TYPE, LVC_TYPE)

        if ch.get('lvc-it') == ON:
            hier.add(LV_ALL_TYPE + '-it', LVC_TYPE)
        if ch.get('lvc-tr') == ON:
            hier.add(LV_ALL_TYPE + '-tr', LVC_TYPE)

        for lv in ch.light_verbs():
            hier.add('lv-' + lv[0], lv[1])

        if not hier.is_empty():
            hierarchies[hier.name] = hier


def customize_light_verb(mylang, hierarchies):
    if LVC_TYPE in hierarchies:
        mylang.add('+nv :+ [ LVC lvc ].', section='addenda')
        hierarchies[LVC_TYPE].save(mylang)

######################################################################

def customize_lvc(mylang, ch, hierarchies, rules):
    if ch.get('coverb-n') == ON or ch.get('coverb-v') == ON :
        # Make sure regular nouns (non-coverbs) can't be used in LVC constructions
        mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.LVC lv-none ].')

        # Create lexical types for all the defined light verb types
        create_lv_lex_types(mylang, ch, hierarchies)

        # Create lexical types for all the defined coverb types
        create_coverb_lex_types(mylang, ch, hierarchies)

        # Create/modify phrasal types for lvcs
        create_lvc_phrase_types(mylang, ch, hierarchies, rules)

        print("---added LVCs to mylang")

def create_lvc_phrase_types(mylang, ch, hierarchies, rules):
    mylang.add_literal(';;; LVC-related phrasal types', section='phrases')

    wo = ch.get('word-order')
    head_type_suffix = ''

    # head-comp
    if wo in ['svo', 'vos', 'vso', 'v-initial']:
        # prevents LVCs from combining using this rule
        mylang.add('head-comp-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC lv-none ].', section='phrases')

    # comp-head
    if wo in ['sov', 'osv', 'ovs', 'v-final']:
        # prevents LVCs from combining using this rule
        mylang.add('comp-head-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC lv-none ].', section='phrases')

    # head-comp and comp-head
    if wo in ['free', 'v2']:
        head_type_suffix = '-head-nexus'
        # prevents LVCs from combining using these rules
        mylang.add('head-comp-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC lv-none ].', section='phrases')
        mylang.add('comp-head-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC lv-none ].', section='phrases')
        mylang.add('head-comp-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC lv-none ].', section='phrases')
        mylang.add('comp-head-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC lv-none ].', section='phrases')

    lvc_wo = ch.get('lvc-word-order')

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
    if lv_cv:
        rules.add('head-comp-lvc := head-comp-phrase-lvc.')

        # allows for formation of LVC
        mylang.add('head-comp-phrase-lvc := basic-head-1st-comp-phrase & head-initial' + head_type_suffix + ' & \
            [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.LVC lv-all ].', section='phrases')

    if cv_lv:
        rules.add('comp-head-lvc := comp-head-phrase-lvc.')

        # allows for combination of coverb + light verb first
        mylang.add('head-final-lvc := head-final & \
            [ SYNSEM.ATTACH lmod ].', section='phrases')

        # prevents object from combining w/ coverb in "subj obj coverb lv" 
        # before coverb + lv have combined
        mylang.add('decl-head-subj-phrase :+ \
            [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.LVC lv-none ].', section='phrases')

        # allows for formation of LVC
        mylang.add('comp-head-phrase-lvc := basic-head-1st-comp-phrase & head-final-lvc & \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.LVC lv-all ].', section='phrases')

    if ch.get('coverb-n') == ON:
        if ch.get('lvc-noun-cv-dep') == 'yes':
            # coverb takes noun-normal dependents
            pass
        elif ch.get('lvc-noun-cv-dep') == 'no':
            # coverb doesn't take noun-normal dependents
            mylang.add('lv-' + COVERB_NOUN + '-lex := \
                [ SYNSEM [ LOCAL.CAT.VAL.COMPS.FIRST #comps ], \
                    ARG-ST.REST.FIRST #comps & [ LIGHT + ] ].', section='phrases')

    if ch.get('coverb-v') == ON:
        if ch.get('lvc-verb-cv-dep') == 'yes':
            # coverb takes verb-normal dependents
            pass
        elif ch.get('lvc-verb-cv-dep') == 'no':
            # coverb doesn't take verb-normal dependents
            mylang.add('lv-' + COVERB_VERB + '-lex := \
                [ SYNSEM [ LOCAL.CAT.VAL.COMPS.FIRST #comps ], \
                    ARG-ST.REST.FIRST #comps & [ LIGHT + ] ].', section='phrases')

    if ch.get('lvc-adjacent') == 'yes':
        # coverb must be immediately adjacent to light verb
        if lv_cv:
            mylang.add('head-comp-phrase-lvc := [ HEAD-DTR.SYNSEM.LIGHT + ].', section='phrases')
        if cv_lv:
            mylang.add('comp-head-phrase-lvc := [ HEAD-DTR.SYNSEM.LIGHT + ].', section='phrases')
    elif ch.get('lvc-adjacent') == 'no':
        # coverb doesn't have to be immediately adjacent to light verb
        pass

def lv_id(item, cv_pos, val_type, with_name=True):
    """Return the identifier for a light verb lexical item."""

    identifier = 'lv-'

    if val_type == 'coverb-only':
        identifier += 'it-'
    elif val_type == 'coverb-1comp':
        identifier += 'tr-'
    
    identifier += cv_pos + '-lex'

    if with_name:
        return get_name(item) + '-' + identifier
    else:
        return identifier

# for lexical_items.py
def customize_light_verbs(mylang, ch, lexicon, hierarchies):
    if ch.get('coverb-n') == ON or ch.get('coverb-v') == ON :
        # Lexical entries
        lexicon.add_literal(';;; Light Verbs')

        # Create lexical entries for all the defined light verb types
        create_lv_lex_entries(ch, lexicon)

        print('---added LVs to lexicon')

def create_lv_lex_entries(ch, lexicon):
    """
    Create light verb lexical entries and add them to the lexicon.
    """
    lv_name_set = set() # keep track of which lvs have been added to the lexicon

    for lv in ch.get('lv'):
        # prevent duplicates
        # only add lv to lexicon if the lv name has not yet been added
        # (just in case, since this should be handled via validation)
        if get_name(lv) not in lv_name_set:
            cv_poses = lv.get('cv-type')
            val_types = lv.get('valence')

            # print(lv.full_key, lv.get('name'))

            for cv_pos in cv_poses.split(', '):
                for val_type in val_types.split(', '):
                    lv_type = lv_id(lv, cv_pos, val_type)

                    for stem in lv.get('stem'):
                        orthstr = orth_encode(stem.get(ORTH))
                        pred = stem.get('pred')
                        typedef = TDLencode(orthstr) + ' := ' + lv_type + ' & \
                            [ STEM < "' + orthstr + '" >, \
                              SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
                        lexicon.add(typedef, merge=False)
        
        lv_name_set.add(get_name(lv))

def create_lv_lex_types(mylang, ch, hierarchies):
    mylang.add_literal(';;; Light Verbs', section='otherlex')

    mylang.add(LV_ITEM, section='otherlex')

    if ch.get('lvc-it') == ON:
        mylang.add(LV_IT_ITEM, section='otherlex')
    
    if ch.get('lvc-tr') == ON:
        mylang.add(LV_TR_ITEM, section='otherlex')

    if ch.get('coverb-n') == ON:
        mylang.add(LV_NOUN_ITEM, section='otherlex')

    if ch.get('coverb-v') == ON:
        mylang.add(LV_VERB_ITEM, section='otherlex')
    
    if ch.get('lvc-it') == ON and ch.get('coverb-n') == ON:
        mylang.add(LV_IT_NOUN_ITEM, section='otherlex')

    if ch.get('lvc-it') == ON and ch.get('coverb-v') == ON:
        mylang.add(LV_IT_VERB_ITEM, section='otherlex')
    
    if ch.get('lvc-tr') == ON and ch.get('coverb-n') == ON:
        mylang.add(LV_TR_NOUN_ITEM, section='otherlex')

    if ch.get('lvc-tr') == ON and ch.get('coverb-v') == ON:
        mylang.add(LV_TR_VERB_ITEM, section='otherlex')
    
    for lv in ch.get('lv'):
        name = lv.get('name')
        val_types = lv.get('valence').split(', ')
        cv_poses = lv.get('cv-type').split(', ')

        for val_type in val_types:
            for cv_pos in cv_poses:
                typedef = lv_id(lv, cv_pos, val_type) + ' := ' + lv_id(lv, cv_pos, val_type, False) + ' & \
                    [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.LVC ' + TDLencode(name) + ' ].'
                mylang.add(typedef, section='otherlex')

def coverb_id(cv_pos, lv_type):
    """Return the identifier for a coverb lexical item."""

    return 'coverb-' + cv_pos + '-' + lv_type + '-lex'

def customize_coverbs(mylang, ch, lexicon, hierarchies):
    if ch.get('coverb-n') == ON or ch.get('coverb-v') == ON :
        # Lexical entries
        lexicon.add_literal(';;; Coverbs')

        if ch.get('coverb-n') == ON:
            # Create lexical entries for all the defined coverb types
            lexicon.add_literal('; Noun Coverbs')
            cv_names_to_types = create_coverb_lex_entries(ch, lexicon, COVERB_NOUN, hierarchies)
            cleanup_coverb_lex_entries(lexicon, COVERB_NOUN, hierarchies, cv_names_to_types)

        if ch.get('coverb-v') == ON :
            lexicon.add_literal('; Verb Coverbs')
            cv_names_to_types = create_coverb_lex_entries(ch, lexicon, COVERB_VERB, hierarchies)
            cleanup_coverb_lex_entries(lexicon, COVERB_VERB, hierarchies, cv_names_to_types)
        
        print('---added coverbs to lexicon')

def create_coverb_lex_types(mylang, ch, hierarchies):
    """
    Create coverb lexical types and add them to mylang.tdl.
    """
    mylang.add_literal(';;; Coverbs', section='otherlex')

    if ch.get('coverb-n') == ON:
        mylang.add_literal('; Noun Coverbs', section='otherlex')

        # need to add OPT +/- to coverb-noun-lex
        #   see customize_nouns() in lexical_items.py to see how
        #   current choices file requires determiners so add this later
        #   when have choices file that doesn't allow determiners
        mylang.add(COVERB_NOUN_ITEM, section='otherlex')
        add_coverb_lex_type(mylang, ch, hierarchies, COVERB_NOUN)

    if ch.get('coverb-v') == ON:
        mylang.add_literal('; Verb Coverbs', section='otherlex')
        mylang.add(COVERB_VERB_ITEM, section='otherlex')
        add_coverb_lex_type(mylang, ch, hierarchies, COVERB_VERB)

def add_coverb_lex_type(mylang, ch, hierarchies, cv_pos):
    """
    Create coverb lexical types for each LVC type and add them to mylang.tdl.
        Only add if the LV type can take the cover part of speech
    """
    lvs_for_pos = set()
    for lv in ch.get('lv'):
        name = lv.get('name')
        cv_poses_from_ch = lv.get('cv-type').split(', ')
        if cv_pos in cv_poses_from_ch:
            lvs_for_pos.add('lv-' + name)

    for lv_type in hierarchies[LVC_TYPE].hierarchy:
        typedef = 'coverb-' + cv_pos + '-lex-' + lv_type[0] + ' := coverb-' + cv_pos + '-lex & \
            [ SYNSEM.LOCAL.CAT.HEAD.LVC ' + lv_type[0] + ' ].'

        if lv_type[0] == LV_ALL_TYPE:
            mylang.add(typedef, section='otherlex')
        elif not lv_type[0] == LV_NONE_TYPE:
            if hierarchies[LVC_TYPE].subtypes[lv_type[0]]:
                subtypes = hierarchies[LVC_TYPE].subtypes[lv_type[0]]
                is_in_lvs = False
                for subtype in subtypes:
                    if subtype in lvs_for_pos:
                        is_in_lvs = True
                if is_in_lvs:
                    mylang.add(typedef, section='otherlex')
            elif lv_type[0] in lvs_for_pos:
                    mylang.add(typedef, section='otherlex')

def create_coverb_lex_entries(ch, lexicon, cv_pos, hierarchies):
    """
    Create coverb lexical entries and add them to the lexicon.

    Returns a dictionary mapping coverb lexical type names to the set of
        lexical types associated with it.
    """
    cv_names_to_types = {}

    for cv in ch.get(cv_pos):
        if cv.get('coverb-type'):
            for stem in cv.get('stem'):
                lv_type_set = set() # keep track of which lv types have been added to the lexicon

                orthstr = orth_encode(stem.get(ORTH))
                pred = stem.get('pred')
                name = stem.get('name')

                for cv_lvs in cv.get('lv'):
                    if name not in cv_names_to_types:
                        cv_names_to_types[name] = set()

                    val_types = cv_lvs.get('valence')

                    if cv_lvs.get('lvs') == 'all-lv':
                        for val_type in val_types.split(', '):
                            cv_lv_name = ''
                            if val_type == 'coverb-only':
                                cv_lv_name = LV_ALL_TYPE + '-it'
                            if val_type == 'coverb-1comp':
                                cv_lv_name = LV_ALL_TYPE + '-tr'

                            coverb_type = coverb_id(cv_pos, cv_lv_name)
                            typedef = TDLencode(name) + ' := ' + coverb_type + ' & \
                                [ STEM < "' + orthstr + '" >, \
                                SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
                            lexicon.add(typedef, merge=False)

                        cv_names_to_types[name].add(coverb_type)
                    else:
                        for val_type in val_types.split(', '):
                            val_type_suffix = ''
                            if val_type == 'coverb-only':
                                val_type_suffix = '-it'
                            elif val_type == 'coverb-1comp':
                                val_type_suffix = '-tr'

                            lv_type_list = []

                            for cv_lv in cv_lvs.get('lvs').split(', '):
                                # Find lv name from lv full key
                                for lv in ch.get('lv'):
                                    if cv_lv == lv.full_key:
                                        lv_type_list.append('lv-' + lv.get('name') + val_type_suffix)

                            cv_lv_name = hierarchies[LVC_TYPE].get_type_covering(lv_type_list)

                            # prevent duplicates
                            # only add coverb to lexicon if the lv type has not yet been added
                            if cv_lv_name not in lv_type_set:
                                coverb_type = coverb_id(cv_pos, cv_lv_name)
                                typedef = TDLencode(name) + ' := ' + coverb_type + ' & \
                                    [ STEM < "' + orthstr + '" >, \
                                    SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
                                lexicon.add(typedef, merge=False)

                                cv_names_to_types[name].add(coverb_type)

                            lv_type_set.add(cv_lv_name)
    return cv_names_to_types

def cleanup_coverb_lex_entries(lexicon, cv_pos, hierarchies, cv_names_to_types):
    """
    For all supertypes associated with a lexical entry, if the subtypes for that
        supertype have lexical entries, remove them.
    """
    for supertype, subtypes in hierarchies[LVC_TYPE].subtypes.items():
        if not (supertype == LVC_TYPE) and subtypes:
            for name in cv_names_to_types:
                if coverb_id(cv_pos, supertype) in cv_names_to_types[name]:
                    search_cv_subtypes(lexicon, cv_pos, hierarchies, subtypes, name)

def search_cv_subtypes(lexicon, cv_pos, hierarchies, curr_subtypes, name):
    """
    Recursively go through subtypes of subtypes and remove them if necessary.
    """
    for curr_subtype in curr_subtypes:
        if curr_subtype in hierarchies[LVC_TYPE].subtypes:

            subtype_types = {coverb_id(cv_pos, curr_subtype)}
            lexicon.remove_subtypes(name, subtype_types)

            new_subtypes = hierarchies[LVC_TYPE].subtypes[curr_subtype]

            search_cv_subtypes(lexicon, cv_pos, hierarchies, new_subtypes, name)

