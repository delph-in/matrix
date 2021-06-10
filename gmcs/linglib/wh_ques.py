'''
Module to support the Constituent (Wh-) Questions library.
email olzama@uw.edu with both constituent and polar questions about the library.
'''

from gmcs.constants import MTRX_FR_OPT, MTRX_FRONT, NO_MULTI, \
    SINGLE, MULTI, SG_OBLIG, ALL_OBLIG, EMBED_INSITU, ON, WH_INFL, \
    IN_SITU, NONE_OBLIG
from gmcs.utils import get_name, TDLencode, orth_encode

from gmcs.feature_type_use import USED_TYPES

'''
CONSTANTS
'''

WH_Q_PHR_NO_OR_SG_OBLIG_MULTI = '''wh-ques-phrase := 
[ SYNSEM.NON-LOCAL.QUE #que,
     HEAD-DTR.SYNSEM.NON-LOCAL.QUE #que ].'''

WH_Q_PHR = ''' wh-ques-phrase := basic-head-filler-phrase & interrogative-clause & head-final &
[ SYNSEM [ LOCAL.CAT [ WH.BOOL +, 
                       MC bool,
			           VAL #val,
			           HEAD verb ], 
			NON-LOCAL.QUE.LIST < > ],
     HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val & [ SUBJ < >,
					                        COMPS < > ],
     NON-HEAD-DTR.SYNSEM [ NON-LOCAL [ QUE.LIST < ref-ind >,
                                       SLASH.LIST < >,
                                       REL.LIST < > ],
                           LOCAL.CONT.HOOK.ICONS-KEY focus ] ].'''


MAIN_WHQ = '''main-wh-ques-phrase := wh-ques-phrase &
[ HEAD-DTR.SYNSEM.LOCAL.CAT.MC na-or-+,
    SYNSEM.LOCAL.CAT.MC + ].
'''

EMBED_WHQ = '''embed-wh-ques-phrase := wh-ques-phrase &
[ HEAD-DTR.SYNSEM.LOCAL.CAT.MC -,
    SYNSEM.LOCAL.CAT.MC - ].'''

EX_COMP = '''extracted-comp-phrase := basic-extracted-comp-phrase.'''

EX_SUBJ = '''extracted-subj-phrase := basic-extracted-subj-phrase &
[ SYNSEM.LOCAL.CAT.HEAD verb ].'''

IN_SITU_PHRASE = '''insitu-int-cl := interrogative-clause & head-only &
[ SYNSEM [ MODIFIED hasmod,
             LOCAL.CAT [ VAL #val,
       MC bool ],
       NON-LOCAL [ SLASH.LIST < >, QUE.LIST < >, REL.LIST < > ] ],
    C-CONT [ RELS.LIST < >,
       HCONS.LIST < > ],
    HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD verb,
          VAL #val & 
            [ SUBJ < >,
              COMPS < > ] ],
          NON-LOCAL [ SLASH.LIST < >,
          REL.LIST < >,
          QUE.LIST < ref-ind, ... > ] ] ].'''

EX_DET_PHRASE = '''extracted-det-phrase := basic-extracted-arg-phrase & head-compositional &
  [ SYNSEM [ LOCAL #specloc & local &
                   [ CAT.VAL [ SUBJ < >,
                               COMPS < >,
                               SPR < >,
                               SPEC < > ] ],
             NON-LOCAL.SLASH.APPEND < #slash,
                                      [ LIST < #local > ] > ],
    HEAD-DTR.SYNSEM [ LOCAL [ CAT [ HEAD noun,
                                    VAL.SPR < gap &
                                          [ LOCAL #local & local &
                                                  [ CAT [ HEAD det,
                                                          VAL.SPEC.FIRST.LOCAL #specloc ],
                                                    CONT.HOOK #hook ] ] > ] ],
                            NON-LOCAL.SLASH #slash ],
    C-CONT [ RELS.LIST < >,
             HCONS.LIST < >,
             ICONS.LIST < >,
             HOOK #hook ] ].'''

BASIC_FILLER_SG = '''basic-filler-phrase :+ [ SYNSEM.NON-LOCAL.SLASH.LIST < >,
                                           ARGS < [ SYNSEM [ LOCAL #slash,
                                                             NON-LOCAL.SLASH.LIST < > ] ], 
                                                    [ SYNSEM.NON-LOCAL.SLASH.LIST < #slash > ] > ]. '''

FIRST_FILLER = '''1st-head-filler-phrase := basic-filler-phrase & head-compositional &
[  SYNSEM [ NON-LOCAL [ SLASH.LIST #slash, REL.LIST < >, QUE.LIST < > ] ],
     ARGS < [ SYNSEM.LOCAL #local & [ CAT.HEAD +nrpd ] ],
	   [ SYNSEM.NON-LOCAL [ SLASH.LIST < #local . #slash > ] ] > ].'''

SEC_FILLER = '''2nd-head-filler-phrase := binary-phrase & phrasal & head-compositional &
[ SYNSEM.NON-LOCAL.SLASH.LIST < #firstarg . #otherargs >,
    ARGS < [ SYNSEM.LOCAL #local ],
     [ SYNSEM.NON-LOCAL [ SLASH.LIST [ FIRST #firstarg, REST < #local . #otherargs > ],
          REL.LIST < > ] ] > ].'''

NC_SUBJ_HEAD = '''subj-head-nc-phrase := decl-head-subj-phrase & head-final &
[ SYNSEM.LOCAL.CAT [ MC - ],
    HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < > ] ].

'''


# This function should be finished when/if WH feature is moved from matrix.tdl to customization
def customize_wh_feature(mylang, ch):
    mylang.add('cat :+ [ WH and-or ].')
    if ch.get(MTRX_FRONT):
        mylang.add('wh-ques-phrase := [ SYNSEM.LOCAL.CAT.WH.BOOL + ].')
        if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
            mylang.add(
                'norm-adposition-lex := [ SYNSEM.LOCAL.CAT.WH.BOOL + ].')


def customize_wh_ques(mylang, ch, rules, roots):
    if (not ch.get(MTRX_FRONT)) and ch.get(WH_INFL) != 'on':
        # If there are no wh-questions, need to put the default
        # constraints to establish the semantic links between
        # the filler and the gap and the extracted subject and the verb:
        mylang.add(BASIC_FILLER_SG, section='phrases')
        mylang.add(EX_SUBJ, section='phrases')
        mylang.add('''clause :+ [ SYNSEM.NON-LOCAL.QUE.LIST < > ]. ''')

    # Either no fronting at all or single fronting
    if (not ch.get(MTRX_FRONT)) or ch.get(MTRX_FRONT) == SINGLE:
        if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
            mylang.add(
                '''adj-head-int-phrase :+ [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH.LIST < > ].''', section='addenda')

    if (not ch.get(MTRX_FRONT) and ch.get(WH_INFL) != ON):
        mylang.add('''clause :+ [ SYNSEM.NON-LOCAL.QUE.LIST < > ]. ''')
        if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
            mylang.add(
                '''head-adj-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < > ]. ''', section='addenda')

    if ch.get(NO_MULTI) == ON:
        if ch.get(MTRX_FRONT) == SINGLE:
            mylang.add(
                '''wh-ques-phrase := [ HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < > ].''')
        if ch.get(MTRX_FRONT) == IN_SITU or \
                (ch.get(MTRX_FRONT) == SINGLE and ch.get(MTRX_FR_OPT) == NONE_OBLIG):
            mylang.add(
                '''insitu-int-cl := [ HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < [ ] > ].''')
        if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
            mylang.add(
                '''wh-adverb-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.WH.BOOL - ] > ].''')

    mylang.add_literal(
        ';;; Wh-question-related phrasal types', section='phrases')

    if ch.get(MTRX_FRONT) in [SINGLE, MULTI]:
        mylang.add_literal('''; Do not allow extracting "And Kim"''')
        mylang.add('''basic-head-filler-phrase :+
   [ ARGS < [ SYNSEM.LOCAL.COORD - ], [ SYNSEM.LOCAL.COORD - ] > ].''')
        mylang.add(WH_Q_PHR, section='phrases')
        if not ch.get('wh-inv-matrix') == ON:
            mylang.add(
                'wh-ques-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC na-or-+ ].')
        else:
            if not ch.get('wh-inv-embed') == ON:
                mylang.add(MAIN_WHQ)
                mylang.add(EMBED_WHQ)
            else:
                mylang.add(
                    'wh-ques-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC na-or-+ ].')
        mylang.add_literal('; Complement extraction', section='phrases')
        mylang.add(EX_COMP)
        rules.add('ex-comp := extracted-comp-phrase.')
        mylang.add_literal('; Adjunct extraction', section='phrases')
        rules.add('ex-adj := basic-extracted-adj-phrase.')
        # Free probably shouldn't belong here? check
        if ch.get('word-order') in ['vos', 'svo', 'sov', 'free']:
            if ch.get('pied-pip-adp') != 'on' or ch.get('oblig-pied-pip-adp') == ON:
                mylang.add(
                    'extracted-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ cons ].', merge=True)
            mylang.add(
                'extracted-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].', merge=True)
        elif ch.get('word-order') in ['vso', 'osv', 'ovs']:
            mylang.add(
                'extracted-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].', merge=True)

    if ch.get(MTRX_FRONT) == SINGLE:
        # With single fronting, can restrict SLASH to one element at most
        mylang.add(BASIC_FILLER_SG, section='phrases')
        mylang.add_literal('; Subject extraction')
        mylang.add(EX_SUBJ)
        rules.add('ex-subj := extracted-subj-phrase.')
        if not ch.get('wh-inv-matrix') == ON:
            rules.add('wh-ques := wh-ques-phrase.')
        else:
            if not ch.get('wh-inv-embed') == ON:
                rules.add('main-whq := main-wh-ques-phrase.')
                rules.add('embed-whq := embed-wh-ques-phrase.')
            else:
                rules.add('wh-ques := wh-ques-phrase.')

        if ch.get(MTRX_FR_OPT) == SG_OBLIG:
            mylang.add(
                '''head-adj-int-phrase :+ [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ clist, COMPS clist ] ].''')

        if ch.get('wh-inv-matrix') == ON:
            if not ch.get('wh-inv-notsubj') == ON:
                mylang.add(
                    'wh-ques-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].')
            else:
                mylang.add(
                    'extracted-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
            if not ch.get('wh-inv-embed') == ON:
                mylang.add('subj-head-phrase := [ SYNSEM.LOCAL.CAT.MC na-or-+,'
                           'HEAD-DTR.SYNSEM.NON-LOCAL [ QUE.LIST < >, SLASH.LIST < > ] ].')
                mylang.add(
                    'adj-head-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.NON-LOCAL [ QUE.LIST < > ] ].')
                mylang.add(NC_SUBJ_HEAD, section='phrases')
                rules.add('nc-subjh := subj-head-nc-phrase.')
            mylang.add('basic-extracted-adj-phrase :+ '
                       '[ SYNSEM.NON-LOCAL.SLASH.LIST < [ CAT.HEAD.MOD < [ LOCAL.CAT.HEAD [ INV - ] ] > ] >, '
                       '  HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')

    if ch.get(MTRX_FRONT) in [MULTI]:
        mylang.add(EX_SUBJ, section='phrases')
        mylang.add('extracted-subj-phrase := [ HEAD-DTR.SYNSEM.L-QUE - ].')
        rules.add('ex-subj := extracted-subj-phrase.')
        mylang.add(
            'wh-ques-phrase := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH.LIST < [], ... > ].')
        mylang.add(FIRST_FILLER)
        # prevent adjunct extraction, as it will be done out of head-subj
        if ch.get(MTRX_FR_OPT) == 'none-oblig':
            mylang.add('1st-head-filler-phrase := [ SYNSEM.MODIFIED hasmod ].')
        mylang.add('wh-ques-phrase := 1st-head-filler-phrase.')
        rules.add('wh-ques := wh-ques-phrase.')
        if ch.get(MTRX_FR_OPT) == ALL_OBLIG:
            mylang.add(WH_Q_PHR_NO_OR_SG_OBLIG_MULTI)  # QUE.LIST is empty
        # Rule out structural ambiguity for sentences like "Who sleeps where?"
        if ch.get('word-order') in ['svo', 'sov', 'osv']:
            mylang.add('''head-adj-int-phrase :+ [ 
             LOCAL.CAT.VAL [ SUBJ clist, COMPS clist ] ] ].''', section='addenda')
        if ch.get('word-order') == 'free':
            mylang.add(
                '''adj-head-int-phrase :+ [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ clist ].''', merge=True)
            mylang.add(
                '''head-adj-int-phrase :+ [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ clist ].''', merge=True)

    if (ch.get(MTRX_FRONT) in [SINGLE] and ch.get(MTRX_FR_OPT) == 'none-oblig'):
        if ch.get('word-order') in ['ovs', 'vos', 'vso']:
            mylang.add(
                '''adj-head-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < > ].''', section='addenda')

    # If fronting is optional, need to use the peripheral feature to rule out ambiguity.
    if (ch.get(MTRX_FRONT) == SINGLE) \
            or (ch.get(MTRX_FRONT) == MULTI and not ch.get(MTRX_FR_OPT) == ALL_OBLIG):
        mylang.add(
            'phrase-or-lexrule :+ [ SYNSEM.L-QUE #lque, ARGS.FIRST.SYNSEM.L-QUE #lque ].')

    # If the fronting isn't obligatory or if only one question phrase
    # is obligatorily fronted, need also in-situ rules:
    if (ch.get(MTRX_FRONT) == SINGLE and not ch.get(MTRX_FR_OPT) == SG_OBLIG) \
            or ch.get(MTRX_FRONT) == IN_SITU \
            or (ch.get(MTRX_FRONT) == MULTI and not ch.get(MTRX_FR_OPT) == ALL_OBLIG) \
            or ch.get(WH_INFL) == ON:
        mylang.add_literal(
            '; In-situ interrogative clause.', section='phrases')
        mylang.add(IN_SITU_PHRASE)
        rules.add('in-situ-ques := insitu-int-cl.')
        if ch.get('q-part-order') == 'second':
            mylang.add('''insitu-int-cl := 
            [ SYNSEM.NON-LOCAL.YNQ #ynq, 
              HEAD-DTR.SYNSEM.NON-LOCAL.YNQ #ynq ].''')

        if not ch.get(MTRX_FRONT) == IN_SITU:
            if ch.get(EMBED_INSITU) == ON:
                mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.WH.BOOL + ].')
            else:
                mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.WH.BOOL - ].')
        else:
            mylang.add('''insitu-int-cl := [ SYNSEM.LOCAL.CAT.WH.BOOL + ].''')

        if ch.get(MTRX_FRONT) in [SINGLE, MULTI]:
            mylang.add('insitu-int-cl := [ HEAD-DTR.SYNSEM.L-QUE - ].')

        # For non-free word orders, need to rule out structural ambiguity:
        if ch.get('word-order') in ['svo', 'sov'] \
                and not (ch.get(MTRX_FRONT) == IN_SITU
                         or ch.get(WH_INFL) == ON):
            mylang.add(
                'subj-head-phrase := [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < > ].')

    # Obligatory pied piping of both nouns and adpositions is the default.
    # If there is no pied piping or it is optional, additional extraction rules are needed.
    if ch.get(MTRX_FRONT) in [SINGLE, MULTI] and len(ch.get('det', [])) > 0:
        if (not ch.get('pied-pip') == ON or (ch.get('pied-pip') == ON
                                             and not (ch.get('oblig-pied-pip-noun') == ON))):
            mylang.add_literal('; If there is no obligatory pied-piping, determiners '
                               'can be extracted separately:', section='phrasal')
            if USED_TYPES['qdet']:
                mylang.add(EX_DET_PHRASE, section='phrases')
            rules.add('ex-det := extracted-det-phrase.')
        # The following would rule out "Which royal house did you see a member of?"
        # if ch.get('pied-pip-adp') == 'on' and not ch.get('oblig-pied-pip-adp') == ON:
            #mylang.add('extracted-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +vp ].')
            # if ch.get('word-order') in ['vos', 'vso', 'ovs','v-initial']:
            #mylang.add('head-spec-phrase := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH.LIST < > ].')
        # else:
        #    mylang.add('extracted-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD verb ].')

        if ch.get('oblig-pied-pip-adp') == ON:
            mylang.add(
                'norm-adposition-lex := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.NON-LOCAL.SLASH.LIST < > ].')

    if ch.get('q-part') == ON:
        if ch.get(MTRX_FRONT) == IN_SITU:
            mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.WH.BOOL + ].')
            if len(ch.get('q-particle')) == 1:
                # This is 1 and not 0 because the Choices len method is overriden; see Choices.py
                qpart = ch.get('q-particle')[1]
                if qpart['wh'] == 'oblig':
                    mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.MC - ].')
            elif ch.has_diverse_ques_particles():
                mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.MC - ].')


def add_lexrules(choices):
    from gmcs.linglib.morphotactics import all_position_classes
    '''
    '''
    for pc in all_position_classes(choices):
        pc_key = pc.full_key
        idx = pc['lrt'].next_iter_num() if 'lrt' in pc else 1
        for lrt in pc.get('lrt', []):
            for feat in lrt['feat']:
                if feat['name'] == 'question' and feat['value'] == 'wh':
                    key = pc_key + '_lrt' + str(idx)
                    name = 'wh-pseudo'
                    choices[key + '_name'] = name
                    choices[key + '_feat1_name'] = 'question'
                    choices[key + '_feat1_value'] = 'wh-pseudo'
                    choices[key + '_feat1_head'] = 'verb'
                    lris = lrt.get('lri', [])
                    i = 1
                    for lri in lris:
                        choices[key + '_lri' +
                                str(i) + '_orth'] = lri['orth']
                        choices[key + '_lri' +
                                str(i) + '_inflecting'] = lri['inflecting']
                        i += 1
                    break
