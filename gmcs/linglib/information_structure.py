from gmcs.linglib import features
from gmcs.utils import get_name
from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

g_tdls = []


#focus-pos;topic-fisrt;c-focus;c-focus-pos : '1st;2nd;3rd'
g_pos_types = {
    ';;;clause-initial' : ['c-focus-pos|contrast-focus', '', '', ''],
    ';;;clause-final' : ['c-focus-pos|contrast-focus', '', '', ''],
    ';;;preverbal' : ['c-focus-pos|contrast-focus', '', '', ''],
    ';;;postverbal' : ['c-focus-pos|contrast-focus', '', '', ''],
    ';on;;' : ['topic-first|topic', '', '', ''],
    ';on;;clause-initial' : ['topic-first|topic;c-focus-pos|contrast-focus', '', '', ''],
    ';on;;clause-final' : ['topic-first|topic;c-focus-pos|contrast-focus', '', '', ''],
    ';on;;preverbal' : ['topic-first|topic;c-focus-pos|contrast-focus', '', '', ''],
    ';on;;postverbal' : ['topic-first|topic;c-focus-pos|contrast-focus', '', '', ''],
    'clause-initial;;;' : ['focus-pos|semantic-focus', '', '', ''],
    'clause-initial;;;clause-initial' : ['focus-pos|focus', '', '', ''],
    'clause-initial;;;clause-final' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'clause-initial;;;preverbal' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'clause-initial;;;postverbal' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'clause-initial;;on;' : ['focus-pos|focus', '', '', ''],
    'clause-initial;on;;' : ['focus-pos|focus-or-topic', '', '', ''],
    'clause-initial;on;;clause-initial' : ['focus-pos|focus-or-topic', '', '', ''],
    'clause-initial;on;;clause-final' : ['focus-pos|focus-or-topic;c-focus-pos|contrast-focus', '', '', ''],
    'clause-initial;on;;preverbal' : ['focus-pos|focus-or-topic;c-focus-pos|contrast-focus', '', '', ''],
    'clause-initial;on;;postverbal' : ['focus-pos|focus-or-topic;c-focus-pos|contrast-focus', '', '', ''],
    'clause-initial;on;on;' : ['focus-pos|focus-or-topic', '', '', ''],
    'clause-final;;;' : ['focus-pos|semantic-focus', '', '', ''],
    'clause-final;;;clause-initial' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'clause-final;;;clause-final' : ['focus-pos|focus', '', '', ''],
    'clause-final;;;preverbal' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'clause-final;;;postverbal' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'clause-final;;on;' : ['focus-pos|focus', '', '', ''],
    'clause-final;on;;' : ['focus-pos|semantic-focus;topic-first|topic', 'non-topic', 'non-focus', ''],
    'clause-final;on;;clause-initial' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', '', '', ''],
    'clause-final;on;;clause-final' : ['focus-pos|focus;topic-first|topic', 'non-topic', 'non-focus', ''],
    'clause-final;on;;preverbal' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', 'non-topic', 'non-focus',  ''],
    'clause-final;on;;postverbal' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', 'non-topic', 'non-focus',  ''],
    'clause-final;on;on;' : ['focus-pos|focus;topic-first|topic', 'non-topic', 'non-focus', ''],
    'preverbal;;;' : ['focus-pos|semantic-focus', '', '', ''],
    'preverbal;;;clause-initial' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'preverbal;;;clause-final' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'preverbal;;;preverbal' : ['focus-pos|focus', '', '', ''],
    'preverbal;;;postverbal' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'preverbal;;on;' : ['focus-pos|focus', '', '', ''],
    'preverbal;on;;' : ['focus-pos|semantic-focus;topic-first|topic', '', 'non-focus', ''],
    'preverbal;on;;clause-initial' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', '', 'non-focus', ''],
    'preverbal;on;;clause-final' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', '', 'non-focus', 'non-topic'],
    'preverbal;on;;preverbal' : ['focus-pos|focus;topic-first|topic', '', 'non-focus', ''],
    'preverbal;on;;postverbal' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', '', 'non-focus', ''],
    'preverbal;on;on;' : ['focus-pos|focus;topic-first|topic', '', 'non-focus', ''],
    'postverbal;;;' : ['focus-pos|semantic-focus', '', '', ''],
    'postverbal;;;clause-initial' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'postverbal;;;clause-final' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'postverbal;;;preverbal' : ['focus-pos|semantic-focus;c-focus-pos|contrast-focus', '', '', ''],
    'postverbal;;;postverbal' : ['focus-pos|focus', '', '', ''],
    'postverbal;;on;' : ['focus-pos|focus', '', '', ''],
    'postverbal;on;;' : ['focus-pos|semantic-focus;topic-first|topic', '', 'non-focus', ''],
    'postverbal;on;;clause-initial' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', '', 'non-focus', ''],
    'postverbal;on;;clause-final' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', '', 'non-focus', ''],
    'postverbal;on;;preverbal' : ['focus-pos|semantic-focus;topic-first|topic;c-focus-pos|contrast-focus', '', 'non-focus', ''],
    'postverbal;on;;postverbal' : ['focus-pos|focus;topic-first|topic', '', 'non-focus', ''],
    'postverbal;on;on;' : ['focus-pos|focus;topic-first|topic', '', 'non-focus', '']
}

g_types = [ 'basic-head-1st-comp-phrase',
            'head-comp-phrase',
            'head-comp-nmc-phrase',
            'head-nf-comp-phrase',
            'nf-comp-head-phrase',
            'comp-head-phrase',
            'comp-head-nmc-phrase',

            'decl-head-subj-phrase',
            'subj-head-phrase',
            'subj-head-nmc-phrase',
            'head-subj-phrase',
            'head-subj-nmc-phrase',
            'head-nf-subj-phrase',
            'nf-subj-head-phrase',

            'extracted-subj-phrase',

            'head-periph-subj-phrase',
            'periph-subj-head-phrase',
            'head-periph-comp-phrase',
            'periph-comp-head-phrase',

            'infostr-dislocated-phrase',
            'infostr-filler-head-phrase',
            'infostr-head-filler-phrase',

            'head-nf-subj-phrase-super',
            'head-nf-comp-phrase-super',
            'narrow-focused-phrase',
            'head-subj-phrase-2',
            'head-comp-phrase-2',
            'head-comp-phrase-3',
            'comp-head-phrase'
            '']

narrow_focused_phrase = """
narrow-focused-phrase := head-only & 
  [ C-CONT [ HOOK #hook,
             RELS <! !>,
             HCONS <! !>,
             ICONS <! focus-or-topic & #icons &
                      [ IARG2 #target ] !> ],
    SYNSEM [ LIGHT -,
             INFOSTR-FLAG +,
             LOCAL [ CAT.VAL [ SPR < >, SUBJ < >, COMPS < >, SPEC < > ],
		     CONT.HOOK [ INDEX #target,
                                 ICONS-KEY #icons ] ] ],
    HEAD-DTR.SYNSEM [ LIGHT -,
                      INFOSTR-FLAG -,
                      LOCAL [ CAT.HEAD noun,
                              CONT [ HOOK #hook,
                                     ICONS <!  !> ] ] ] ].
"""

subj_head_nmc_phrase = """
subj-head-nmc-phrase := basic-head-subj-nmc-phrase & head-final & 
 [ SYNSEM.LOCAL.CAT.MC -,
   HEAD-DTR.SYNSEM.NON-LOCAL.SLASH [ LIST < [ CONT.HOOK.ICONS-KEY $ ] > ] ].
"""
head_subj_nmc_phrase = """
head-subj-nmc-phrase := basic-head-subj-nmc-phrase & head-initial &
 [ SYNSEM.LOCAL.CAT.MC -,
   HEAD-DTR.SYNSEM.NON-LOCAL.SLASH [ LIST < [ CONT.HOOK.ICONS-KEY $ ] > ] ].
"""

head_subj_phrase_initial = """
head-subj-phrase := decl-head-subj-phrase & head-initial & narrow-focus &
 [ SYNSEM.L-PERIPH +, 
   HEAD-DTR.SYNSEM [ L-PERIPH +,
                     LOCAL.CONT.HOOK.ICONS-KEY $ ] ].
"""

head_subj_phrase_initial_wo_comps = """
head-subj-phrase := decl-head-subj-phrase & head-initial & narrow-focus &
 [ SYNSEM.L-PERIPH +, 
   HEAD-DTR.SYNSEM [ L-PERIPH +,
                     LOCAL [ CAT.VAL.COMPS < >,
                             CONT.HOOK.ICONS-KEY $ ] ] ].
"""


head_subj_phrase_final = """
head-subj-phrase := decl-head-subj-phrase & head-initial & narrow-focus &
 [ SYNSEM.R-PERIPH +, 
   HEAD-DTR.SYNSEM [ R-PERIPH na-or--,
                     LOCAL.CAT.VAL.COMPS < > ],
   NON-HEAD-DTR.SYNSEM [ R-PERIPH +,
                         LOCAL.CONT.HOOK.ICONS-KEY $ ] ].
"""

head_comp_nmc_phrase = """
head-comp-nmc-phrase := basic-head-comp-nmc-phrase & head-initial & 
  [ SYNSEM [ R-PERIPH -
             LOCAL.CAT.VAL.COMPS #comps ],
    HEAD-DTR.SYNSEM [ LOCAL.CAT.VAL.COMPS < #synsem . #comps >,
		      NON-LOCAL.SLASH [ LIST < [ CONT.HOOK.ICONS-KEY $ ] > ] ], 
    NON-HEAD-DTR.SYNSEM #synsem ].
"""

comp_head_phrase = """
comp-head-phrase := basic-head-1st-comp-phrase & head-final & narrow-focus &
 [ SYNSEM.R-PERIPH +,
   HEAD-DTR.SYNSEM [ LIGHT +, 
                     LOCAL.CONT.HOOK.ICONS-KEY $ ] ].
"""

comp_head_nmc_phrase = """
comp-head-nmc-phrase := basic-head-comp-nmc-phrase & head-final & 
  [ SYNSEM [ R-PERIPH -
             LOCAL.CAT.VAL.COMPS #comps ],
    HEAD-DTR.SYNSEM [ LOCAL.CAT.VAL.COMPS < #synsem . #comps >,
		      NON-LOCAL.SLASH [ LIST < [ CONT.HOOK.ICONS-KEY $ ] > ] ], 
    NON-HEAD-DTR.SYNSEM #synsem ].
"""

head_nf_comp_phrase = """
head-nf-comp-phrase := head-nf-comp-phrase-super & head-initial & 
 [ HEAD-DTR.SYNSEM.LIGHT +,
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

head_nf_subj_phrase = """
head-nf-subj-phrase := head-nf-subj-phrase-super & head-initial & 
  [ HEAD-DTR.SYNSEM.LIGHT +,
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

head_nf_comp_phrase_v2 = """
head-nf-comp-phrase := head-nf-comp-phrase-super & head-initial-head-nexus & 
 [ HEAD-DTR.SYNSEM.LIGHT +,
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

head_nf_subj_phrase_v2 = """
head-nf-subj-phrase := head-nf-subj-phrase-super & head-initial-head-nexus & 
  [ HEAD-DTR.SYNSEM.LIGHT +,
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

infostr_dislocated_phrase = """
infostr-dislocated-phrase := narrow-focus &
  [ SYNSEM.LOCAL.CAT [ MC +, VAL.SUBJ < > ],
    C-CONT [ RELS <! !>, 
             HCONS <! !>,
             ICONS <! info-str & #icons & [ IARG1 #clause, IARG2 #index ] !> ],
    HEAD-DTR.SYNSEM.LOCAL [ CAT [ MC -, HEAD verb ],
                            CONT.HOOK [ INDEX #clause,
                                        CLAUSE-KEY #clause ] ],
    NON-HEAD-DTR.SYNSEM [ LIGHT -,
                          LOCAL [ CAT.HEAD +np,
                                  CONT.HOOK [ INDEX #index,
                                              ICONS-KEY #icons ] ] ] ].
"""

infostr_filler_head_phrase = """
infostr-filler-head-phrase := basic-head-filler-phrase & infostr-dislocated-phrase & head-final &
 [ SYNSEM.L-PERIPH +,
   HEAD-DTR.SYNSEM [ L-PERIPH -,
                     LOCAL.CAT.VAL.SUBJ < > ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CONT.HOOK.ICONS-KEY $ ].
"""

infostr_head_filler_phrase = """
infostr-head-filler-phrase := nc-head-filler-phrase & infostr-dislocated-phrase & head-initial &
 [ SYNSEM.R-PERIPH +,
   HEAD-DTR.SYNSEM [ R-PERIPH -,
             	     LOCAL.CAT.VAL.SUBJ olist ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CONT.HOOK.ICONS-KEY $ ].
"""

head_nf_comp_phrase_super = """
head-nf-comp-phrase-super := basic-head-comp-phrase & narrow-focus &
  [ SYNSEM.LOCAL.CAT.VAL.COMPS #comps,
    HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < #synsem . #comps >,
    NON-HEAD-DTR.SYNSEM #synsem & [ INFOSTR-FLAG + ] ].
"""

head_nf_subj_phrase_super = """
head-nf-subj-phrase-super := basic-head-subj-phrase & declarative-clause & narrow-focus &
  [ NON-HEAD-DTR.SYNSEM.INFOSTR-FLAG + ].
"""

# [MC -]: deleting it from all nf rules
nf_comp_head_phrase_old = """
nf-comp-head-phrase := head-nf-comp-phrase-super & head-final &
 [ SYNSEM.LOCAL.CAT.MC -, 
   HEAD-DTR.SYNSEM [ LIGHT +,
         	     LOCAL.CAT [ MC -, VAL.SUBJ < [ ] > ],
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

nf_comp_head_phrase = """
nf-comp-head-phrase := head-nf-comp-phrase-super & head-final &
 [ HEAD-DTR.SYNSEM.LIGHT +,
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

nf_comp_head_phrase_v2 = """
nf-comp-head-phrase := head-nf-comp-phrase-super & head-final-head-nexus &
 [ SYNSEM [ L-PERIPH +, INFOSTR-FLAG + ],
   HEAD-DTR.SYNSEM.LIGHT +,
   NON-HEAD-DTR.SYNSEM [ L-PERIPH +,
                         LOCAL [ CAT.HEAD +np,
		                 CONT.HOOK.ICONS-KEY $ ] ] ]. 
"""

nf_subj_head_phrase_v2 = """
nf-subj-head-phrase := head-nf-subj-phrase-super & head-final-head-nexus & 
  [ SYNSEM [ L-PERIPH +, INFOSTR-FLAG + ],
    HEAD-DTR.SYNSEM.LIGHT +,
   NON-HEAD-DTR.SYNSEM [ L-PERIPH +,
                         LOCAL [ CAT.HEAD +np,
		                 CONT.HOOK.ICONS-KEY $ ] ] ]. 
"""

head_nf_subj_phrase_w_comps = """
head-nf-subj-phrase := head-nf-subj-phrase-super & head-initial &
  [ HEAD-DTR.SYNSEM [ LIGHT +,
		      LOCAL.CAT.VAL.COMPS.FIRST [ ] ], 
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

nf_subj_head_phrase = """
nf-subj-head-phrase := head-nf-subj-phrase-super & head-final &
  [ HEAD-DTR.SYNSEM [ LIGHT +,
		      LOCAL.CAT.VAL.COMPS.FIRST [ ] ], 
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

nf_subj_head_phrase_simple = """
nf-subj-head-phrase := head-nf-subj-phrase-super & head-final & 
  [ HEAD-DTR.SYNSEM.LIGHT +,
    NON-HEAD-DTR.SYNSEM.LOCAL.CONT.HOOK.ICONS-KEY $ ].
"""

nf_comp_head_phrase_aux = """
nf-comp-head-phrase := head-nf-comp-phrase-super & head-final & 
 [ HEAD-DTR.SYNSEM [ LIGHT +,
         	     LOCAL.CAT.HEAD.AUX - ],
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

head_nf_comp_phrase_aux = """
head-nf-comp-phrase := head-nf-comp-phrase-super & head-initial &
 [ HEAD-DTR.SYNSEM [ LIGHT +,
         	     LOCAL.CAT.HEAD.AUX - ],
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

head_nf_subj_phrase_aux = """
head-nf-subj-phrase := head-nf-subj-phrase-super & head-initial & 
  [ HEAD-DTR.SYNSEM [ LIGHT +,
                      LOCAL.CAT.HEAD.AUX - ],
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

head_nf_subj_phrase_w_comps_aux = """
head-nf-subj-phrase := head-nf-subj-phrase-super & head-initial & 
  [ HEAD-DTR.SYNSEM [ LIGHT +,
		      LOCAL.CAT [ HEAD.AUX -,
                                  VAL.COMPS.FIRST [ ] ] ], 
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

nf_subj_head_phrase_aux = """
nf-subj-head-phrase := head-nf-subj-phrase-super & head-final &
  [ HEAD-DTR.SYNSEM [ LIGHT +,
                      LOCAL.CAT [ HEAD.AUX -,
				  VAL.COMPS.FIRST [ ] ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD +np,
		               CONT.HOOK.ICONS-KEY $ ] ]. 
"""

nf_subj_head_phrase_simple_aux = """
nf-subj-head-phrase := head-nf-subj-phrase-super & head-final & 
  [ HEAD-DTR.SYNSEM [ LIGHT +,
                      LOCAL.CAT.HEAD.AUX - ],
    NON-HEAD-DTR.SYNSEM.LOCAL.CONT.HOOK.ICONS-KEY $ ].
"""


def add_ph_types(mylang, ph_types):
    for t in g_types:
        if t == '' or ph_types[t] == '':
            continue
        mylang.add(ph_types[t], '', section='phrases')


def add_ph_rules(rules, ph_rules):
    for t in g_types:
        if t == '' or ph_rules[t] == '':
            continue
        rule = ph_rules[t].strip()
        rule += ' := '
        rule += t.strip()
        rule += '.'
        rules.add(rule)


def init_ph_types():
    ph_types = {}
    for t in g_types:
        ph_types[t] = ''
    return ph_types


def init_ph_rules():
    return init_ph_types()


def get_irule(irule_name, affix_type, orth, type_name, affix_cnt):
    irule = irule_name.strip() + '-affix-' + str(affix_cnt)
    irule += ' :=\n%' + affix_type.strip() + ' '
    irule += '(* ' + orth.strip() + ')\n' + type_name.strip() + '.'
    return irule


def add_lextypes(mylang, tdl):
    if tdl not in g_tdls:
        mylang.add(tdl, merge = True, section='otherlex')
        g_tdls.append(tdl)


def customize_information_structure_pos_once(mylang, ch, rules, infostr_type, infostr, flr):
    if ch.get(infostr_type) == '': #focus-pos, topic-first, ...
        return

    #   nf_comp_head_phrase = nf_comp_head_phrase_aux
    #   head_nf_comp_phrase = head_nf_comp_phrase_aux
    #   head_nf_subj_phrase = head_nf_subj_phrase_aux
    #   head_nf_subj_phrase_w_comps = head_nf_subj_phrase_w_comps_aux
    #   nf_subj_head_phrase = nf_subj_head_phrase_aux
    #   nf_subj_head_phrase_simple = nf_subj_head_phrase_simple_aux

    infostr_in_flr = 'focus-or-topic'
    if flr != '':
        infostr_in_flr = flr

    ph_types = init_ph_types()
    ph_rules = init_ph_rules()

    wo = ch.get('word-order')
    pos = ch.get(infostr_type)

    if pos in [ 'preverbal', 'postverbal' ] or wo == 'v2':
        mylang.add_literal('lex-or-phrase-synsem :+ [ INFOSTR-FLAG luk ].', '', section='addenda')
        mylang.add_literal('decl-head-subj-phrase :+ [ SYNSEM.INFOSTR-FLAG -, NON-HEAD-DTR.SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        mylang.add_literal('basic-head-1st-comp-phrase :+ [ SYNSEM.INFOSTR-FLAG -, NON-HEAD-DTR.SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        mylang.add_literal('basic-head-2nd-comp-phrase :+ [ SYNSEM.INFOSTR-FLAG -, NON-HEAD-DTR.SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        mylang.add_literal('basic-head-opt-subj-phrase :+ [ SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        mylang.add_literal('basic-head-opt-comp-phrase :+ [ SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        mylang.add_literal('basic-head-mod-phrase-simple :+ [ SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        mylang.add_literal('basic-head-spec-phrase :+ [ SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        mylang.add_literal('basic-bare-np-phrase :+ [ SYNSEM.INFOSTR-FLAG -, HEAD-DTR.SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        #mylang.add_literal('basic-binary-headed-phrase :+ [ SYNSEM.INFOSTR-FLAG - ].', '', section='addenda')
        if pos != 'clause-initial':
            ph_types['head-nf-subj-phrase-super'] = head_nf_subj_phrase_super
        ph_types['head-nf-comp-phrase-super'] = head_nf_comp_phrase_super
        ph_types['narrow-focused-phrase'] = narrow_focused_phrase
        ph_rules['narrow-focused-phrase'] = 'narrow-focused-phrase'

    if wo in ['sov', 'svo', 'osv', 'ovs', 'vso', 'vos']:
        if pos == 'clause-initial' or infostr_type == 'topic-first':
            if wo == 'sov':
                ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.L-PERIPH -, HEAD-DTR.SYNSEM.L-PERIPH - ].'
                ph_types['subj-head-phrase'] = 'subj-head-phrase := [ SYNSEM [ LOCAL.CAT.MC +, NON-LOCAL.SLASH 0-dlist ], HEAD-DTR.SYNSEM.L-PERIPH - ].'
                ph_types['subj-head-nmc-phrase'] = subj_head_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['subj-head-nmc-phrase'] = 'subj-head-nmc'
                # if infostr_type != 'topic-first':
                #   ph_types['head-subj-phrase'] = head_subj_phrase_initial.replace('$', infostr)
                #   ph_rules['head-subj-phrase'] = 'head-subj'
                #   ph_types['head-comp-phrase'] = """head-comp-phrase := basic-head-1st-comp-phrase & head-initial &
                #                                   [ SYNSEM [ L-PERIPH +, LOCAL.CAT [ HC-LIGHT -, VAL.SUBJ < > ] ],
                #                                     HEAD-DTR.SYNSEM.L-PERIPH + ]."""
                #   ph_rules['head-comp-phrase'] = 'head-comp'
            elif wo == 'svo':
                ph_types['basic-head-1st-comp-phrase'] = 'basic-head-1st-comp-phrase :+ [ SYNSEM.L-PERIPH - ].'
                ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
                ph_types['subj-head-phrase'] = 'subj-head-phrase := [ SYNSEM [ LOCAL.CAT.MC +, NON-LOCAL.SLASH 0-dlist ], HEAD-DTR.SYNSEM.L-PERIPH - ].'
                ph_types['subj-head-nmc-phrase'] = subj_head_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['subj-head-nmc-phrase'] = 'subj-head-nmc'
                # if infostr_type != 'topic-first':
                #   ph_types['head-subj-phrase'] = head_subj_phrase_initial.replace('$', infostr)
                #   ph_rules['head-subj-phrase'] = 'head-subj'
            elif wo in ['vso', 'vos']:
                ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
                ph_types['head-subj-phrase'] = 'head-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].'
                ph_types['head-comp-nmc-phrase'] = head_comp_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['head-comp-nmc-phrase'] = 'head-comp-nmc'
                tdl = """extracted-subj-phrase := basic-extracted-subj-phrase &
                 [ C-CONT [ RELS <! !>, HCONS <! !>, ICONS <! !> ],
                   HEAD-DTR.SYNSEM [ L-PERIPH -,
                                     LOCAL.CAT.VAL.COMPS < >,
                                     NON-LOCAL.SLASH.LIST < [ CONT.HOOK.ICONS-KEY $ ] > ] ]."""
                ph_types['extracted-subj-phrase'] = tdl.replace('$', infostr_in_flr)
                ph_rules['extracted-subj-phrase'] = 'extracted-subj'
                if wo == 'vso':
                    ph_types['head-subj-nmc-phrase'] = head_subj_nmc_phrase.replace('$', infostr_in_flr)
                    ph_rules['head-subj-nmc-phrase'] = 'head-subj-nmc'
            elif wo == 'osv':
                ph_types['comp-head-nmc-phrase'] = comp_head_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['comp-head-nmc-phrase'] = 'comp-head-nmc'
                # if infostr_type != 'topic-first':
                #   ph_types['head-subj-phrase'] = head_subj_phrase_initial_wo_comps.replace('$', infostr)
                #   ph_rules['head-subj-phrase'] = 'head-subj'
            elif wo == 'ovs':
                tdl = """extracted-subj-phrase := basic-extracted-subj-phrase &
                 [ C-CONT [ RELS <! !>, HCONS <! !>, ICONS <! !> ],
                   HEAD-DTR.SYNSEM [ L-PERIPH -,
                                     LOCAL.CAT.VAL.COMPS < >,
                                     NON-LOCAL.SLASH.LIST < [ CONT.HOOK.ICONS-KEY $ ] > ] ]."""
                ph_types['extracted-subj-phrase'] = tdl.replace('$', infostr_in_flr)
                ph_rules['extracted-subj-phrase'] = 'extracted-subj'
                # if infostr_type != 'topic-first':
                #   ph_types['head-comp-phrase'] = """head-comp-phrase := basic-head-1st-comp-phrase & head-initial &
                #                                   [ SYNSEM [ L-PERIPH +, LOCAL.CAT.HC-LIGHT - ],
                #                                     HEAD-DTR.SYNSEM.L-PERIPH + ]."""
                #   ph_rules['head-comp-phrase'] = 'head-comp'
            else:
                pass

            ph_types['infostr-dislocated-phrase'] = infostr_dislocated_phrase
            ph_types['infostr-filler-head-phrase'] = infostr_filler_head_phrase.replace('$', infostr)
            ph_rules['infostr-filler-head-phrase'] = 'flr-head'

        elif pos == 'clause-final':
            if wo == 'sov':
                ph_types['basic-head-1st-comp-phrase'] = 'basic-head-1st-comp-phrase :+ [ HEAD-DTR.SYNSEM.R-PERIPH - ].'
                ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM [ LOCAL.CAT.HC-LIGHT -, NON-LOCAL.SLASH 0-dlist ] ].'
                ph_types['comp-head-nmc-phrase'] = comp_head_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['comp-head-nmc-phrase'] = 'comp-head-nmc'
                ph_types['subj-head-nmc-phrase'] = subj_head_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['subj-head-nmc-phrase'] = 'subj-head-nmc'
                ph_types['subj-head-phrase'] = 'subj-head-phrase := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 0-dlist ].'
                tdl = """extracted-subj-phrase := basic-extracted-subj-phrase &
                 [ C-CONT [ RELS <! !>, HCONS <! !>, ICONS <! !> ],
                   HEAD-DTR.SYNSEM [ R-PERIPH -,
                                     LOCAL.CAT.VAL.COMPS < >,
                                     NON-LOCAL.SLASH.LIST < [ CONT.HOOK.ICONS-KEY $ ] > ] ]."""
                ph_types['extracted-subj-phrase'] = tdl.replace('$', infostr_in_flr)
                ph_rules['extracted-subj-phrase'] = 'extracted-subj'
            elif wo == 'svo':
                ph_types['basic-head-1st-comp-phrase'] = 'basic-head-1st-comp-phrase :+ [ HEAD-DTR.SYNSEM.R-PERIPH - ].'
                ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.NON-LOCAL.SLASH 0-dlist, HEAD-DTR.SYNSEM.R-PERIPH - ].'
                ph_types['head-comp-nmc-phrase'] = head_comp_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['head-comp-nmc-phrase'] = 'head-comp-nmc'
                #ph_types['comp-head-phrase'] = comp_head_phrase.replace('$', infostr)
                #ph_rules['comp-head-phrase'] = 'comp-head'
                ph_types['subj-head-phrase'] = 'subj-head-phrase := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 0-dlist ].'
                tdl = """extracted-subj-phrase := basic-extracted-subj-phrase &
                 [ C-CONT [ RELS <! !>, HCONS <! !>, ICONS <! !> ],
                   HEAD-DTR.SYNSEM [ R-PERIPH -,
                                     LOCAL.CAT.VAL.COMPS < >,
                                     NON-LOCAL.SLASH.LIST < [ CONT.HOOK.ICONS-KEY $ ] > ] ]."""
                ph_types['extracted-subj-phrase'] = tdl.replace('$', infostr_in_flr)
                ph_rules['extracted-subj-phrase'] = 'extracted-subj'
            elif wo == 'vso':
                ph_types['basic-head-1st-comp-phrase'] = 'basic-head-1st-comp-phrase :+ [ HEAD-DTR.SYNSEM.R-PERIPH - ].'
                #tdl = """subj-head-phrase := decl-head-subj-phrase & head-final & narrow-focus &
                #         [ SYNSEM.R-PERIPH +,
                #           HEAD-DTR.SYNSEM [ R-PERIPH +,
                #                             LOCAL.CONT.HOOK.ICONS-KEY $,
                #                             NON-LOCAL.SLASH 0-dlist ] ]."""
                #ph_types['subj-head-phrase'] = tdl.replace('$', infostr)
                #ph_rules['subj-head-phrase'] = 'subj-head'
                ph_types['head-comp-nmc-phrase'] = head_comp_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['head-comp-nmc-phrase'] = 'head-comp-nmc'
            elif wo == 'vos':
                ph_types['basic-head-1st-comp-phrase'] = 'basic-head-1st-comp-phrase :+ [ HEAD-DTR.SYNSEM.R-PERIPH - ].'
                ph_types['head-subj-nmc-phrase'] = head_subj_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['head-subj-nmc-phrase'] = 'head-subj-nmc'
                ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].'
            elif wo == 'osv':
                ph_types['basic-head-1st-comp-phrase'] = 'basic-head-1st-comp-phrase :+ [ HEAD-DTR.SYNSEM.R-PERIPH - ].'
                ph_types['subj-head-nmc-phrase'] = subj_head_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['subj-head-nmc-phrase'] = 'subj-head-nmc'
                ph_types['comp-head-nmc-phrase'] = comp_head_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['comp-head-nmc-phrase'] = 'comp-head-nmc'
                #ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].'
            elif wo == 'ovs':
                #tdl = """subj-head-phrase := decl-head-subj-phrase & head-final & narrow-focus &
                #         [ SYNSEM.R-PERIPH +,
                #           HEAD-DTR.SYNSEM [ R-PERIPH +,
                #                             LOCAL.CONT.HOOK.ICONS-KEY $,
                #                             NON-LOCAL.SLASH 0-dlist ] ]."""
                #ph_types['subj-head-phrase'] = tdl.replace('$', infostr)
                #ph_rules['subj-head-phrase'] = 'subj-head'
                ph_types['head-subj-nmc-phrase'] = head_subj_nmc_phrase.replace('$', infostr_in_flr)
                ph_rules['head-subj-nmc-phrase'] = 'head-subj-nmc'
                ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM [ R-PERIPH -, NON-LOCAL.SLASH 0-dlist ] ].'

            else:
                pass

            ph_types['infostr-dislocated-phrase'] = infostr_dislocated_phrase
            ph_types['infostr-head-filler-phrase'] = infostr_head_filler_phrase.replace('$', infostr)
            ph_rules['infostr-head-filler-phrase'] = 'head-flr'

        elif pos == 'preverbal':
            if wo == 'sov':
                ph_types['nf-subj-head-phrase'] = nf_subj_head_phrase.replace('$', infostr)
                ph_rules['nf-subj-head-phrase'] = 'nf-subj-head'
                ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            elif wo == 'svo':
                ph_types['nf-comp-head-phrase'] = nf_comp_head_phrase.replace('$', infostr)
                ph_rules['nf-comp-head-phrase'] = 'nf-comp-head'
            elif wo in ['vso', 'vos']:
                ph_types['nf-subj-head-phrase'] = nf_subj_head_phrase_simple.replace('$', infostr)
                ph_rules['nf-subj-head-phrase'] = 'nf-subj-head'
                ph_types['nf-comp-head-phrase'] = nf_comp_head_phrase.replace('$', infostr)
                ph_rules['nf-comp-head-phrase'] = 'nf-comp-head'
                if wo == 'vos':
                    ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            elif wo == 'osv':
                ph_types['nf-comp-head-phrase'] = nf_comp_head_phrase.replace('$', infostr)
                ph_rules['nf-comp-head-phrase'] = 'nf-comp-head'
            elif wo == 'ovs':
                ph_types['nf-subj-head-phrase'] = nf_subj_head_phrase_simple.replace('$', infostr)
                ph_rules['nf-subj-head-phrase'] = 'nf-subj-head'
                ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            else:
                pass

        elif pos == 'postverbal':
            if wo == 'sov':
                if ch.get('has-aux').strip() == 'yes':
                    ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase_aux.replace('$', infostr)
                    ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase_aux.replace('$', infostr)
                else:
                    ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase.replace('$', infostr)
                    ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase.replace('$', infostr)
                ph_rules['head-nf-subj-phrase'] = 'head-nf-subj'
                ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
                ph_rules['head-nf-comp-phrase'] = 'head-nf-comp'
            elif wo == 'svo':
                if ch.get('has-aux').strip() == 'yes':
                    ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase_aux.replace('$', infostr)
                else:
                    ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase.replace('$', infostr)
                ph_rules['head-nf-subj-phrase'] = 'head-nf-subj'
                ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            elif wo == 'vso':
                if ch.get('has-aux').strip() == 'yes':
                    ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase_aux.replace('$', infostr)
                else:
                    ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase.replace('$', infostr)
                ph_rules['head-nf-comp-phrase'] = 'head-nf-comp'
            elif wo == 'vos':
                if ch.get('has-aux').strip() == 'yes':
                    ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase_w_comps_aux.replace('$', infostr)
                else:
                    ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase_w_comps.replace('$', infostr)
                ph_rules['head-nf-subj-phrase'] = 'head-nf-subj'
                ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            elif wo == 'osv':
                #need to use both? check them
                if ch.get('has-aux').strip() == 'yes':
                    ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase_aux.replace('$', infostr)
                    ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase_aux.replace('$', infostr)
                else:
                    ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase.replace('$', infostr)
                    ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase.replace('$', infostr)
                ph_rules['head-nf-subj-phrase'] = 'head-nf-subj'

                ph_rules['head-nf-comp-phrase'] = 'head-nf-comp'
            elif wo == 'ovs':
                if ch.get('has-aux').strip() == 'yes':
                    ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase_aux.replace('$', infostr)
                else:
                    ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase.replace('$', infostr)
                ph_rules['head-nf-comp-phrase'] = 'head-nf-comp'
            else:
                pass

    elif wo == 'v-final':
        if pos == 'clause-initial' or infostr_type == 'topic-first':
            ph_types['subj-head-phrase'] = 'subj-head-phrase := [ SYNSEM.L-PERIPH na, HEAD-DTR.SYNSEM.L-PERIPH na-or-- ].'
            ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.L-PERIPH na, HEAD-DTR.SYNSEM.L-PERIPH na-or-- ].'
            # if infostr_type != 'topic-first':
            #   tdl = """head-comp-phrase := basic-head-1st-comp-phrase & head-initial &
            #          [ SYNSEM [ L-PERIPH +,
            #                     LOCAL [ CAT.HC-LIGHT -,
            #                             CONT.HOOK.ICONS-KEY $  ] ],
            #            HEAD-DTR.SYNSEM.L-PERIPH + ]."""
            #   ph_types['head-comp-phrase'] = tdl.replace('$', infostr)
            #   ph_rules['head-comp-phrase'] = 'head-comp'
            #   ph_types['head-subj-phrase'] = head_subj_phrase_initial.replace('$', infostr)
            #   ph_rules['head-subj-phrase'] = 'head-subj'
        elif pos == 'clause-final':
            # tdl = """head-comp-phrase := basic-head-1st-comp-phrase & head-initial & narrow-focus &
            #                               [ SYNSEM [ R-PERIPH +,
            #                                          LOCAL.CAT.HC-LIGHT - ],
            #                                 NON-HEAD-DTR.SYNSEM [ R-PERIPH +,
            #                                                       LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            # ph_types['head-comp-phrase'] = tdl.replace('$', infostr)
            # ph_rules['head-comp-phrase'] = 'head-comp'
            # ph_types['head-subj-phrase'] = head_subj_phrase_final.replace('$', infostr)
            # ph_rules['head-subj-phrase'] = 'head-subj'
            # ph_types['subj-head-phrase'] = 'subj-head-phrase := [ SYNSEM.R-PERIPH na, HEAD-DTR.SYNSEM.R-PERIPH na-or-- ].'
            # ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.R-PERIPH na, HEAD-DTR.SYNSEM.R-PERIPH na-or-- ].'
            pass
        elif pos == 'preverbal':
            #Todo THIS
            #There is no constraint for this type. (i.e. in the basic word order: S[O]V / O[S]V )
            pass
        elif pos == 'postverbal':
            # ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase.replace('$', infostr)
            # ph_rules['head-nf-comp-phrase'] = 'head-nf-comp'
            # ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase.replace('$', infostr)
            # ph_rules['head-nf-subj-phrase'] = 'head-nf-subj'
            # ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            pass
        else:
            pass
    elif wo == 'v-initial':
        if pos == 'clause-initial' or infostr_type == 'topic-first':
            ph_types['head-subj-phrase'] = 'head-subj-phrase := [ HEAD-DTR.SYNSEM.L-PERIPH na-or-- ].'
            ph_types['head-comp-phrase'] = 'head-comp-phrase := [ HEAD-DTR.SYNSEM.L-PERIPH na-or-- ].'
            # if infostr_type != 'topic-first':
            #   tdl = """comp-head-phrase := basic-head-1st-comp-phrase & head-final & narrow-focus &
            #          [ SYNSEM [ L-PERIPH +,
            #                     LOCAL.CAT.HC-LIGHT - ],
            #            HEAD-DTR.SYNSEM.L-PERIPH -,
            #            NON-HEAD-DTR.SYNSEM [ L-PERIPH +,
            #                                  LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            #   ph_types['comp-head-phrase'] = tdl.replace('$', infostr)
            #   ph_rules['comp-head-phrase'] = 'comp-head'
            #   tdl = """subj-head-phrase := decl-head-subj-phrase & head-final & narrow-focus &
            #          [ SYNSEM [ L-PERIPH +,
            #                     LOCAL.CAT.HC-LIGHT - ],
            #            HEAD-DTR.SYNSEM.L-PERIPH -,
            #            NON-HEAD-DTR.SYNSEM [ L-PERIPH +,
            #                                  LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            #   ph_types['subj-head-phrase'] = tdl.replace('$', infostr)
            #   ph_rules['subj-head-phrase'] = 'subj-head'
        elif pos == 'clause-final':
            # tdl = """comp-head-phrase := basic-head-1st-comp-phrase & head-final &
            #          [ SYNSEM.R-PERIPH +,
            #            HEAD-DTR.SYNSEM [ R-PERIPH +,
            #                              LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            # ph_types['comp-head-phrase'] = tdl.replace('$', infostr)
            # ph_rules['comp-head-phrase'] = 'comp-head'
            # tdl = """subj-head-phrase := decl-head-subj-phrase & head-final &
            #          [ SYNSEM.R-PERIPH +,
            #            HEAD-DTR.SYNSEM [ R-PERIPH +,
            #                              LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            # ph_types['subj-head-phrase'] = tdl.replace('$', infostr)
            # ph_rules['subj-head-phrase'] = 'subj-head'
            ph_types['head-subj-phrase'] = 'head-subj-phrase := [ SYNSEM.R-PERIPH na, HEAD-DTR.SYNSEM.R-PERIPH na-or-- ].'
            ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.R-PERIPH na, HEAD-DTR.SYNSEM.R-PERIPH na-or-- ].'
        elif pos == 'preverbal':
            # ph_types['nf-comp-head-phrase'] = nf_comp_head_phrase.replace('$', infostr)
            # ph_rules['nf-comp-head-phrase'] = 'nf-comp-head'
            # ph_types['nf-subj-head-phrase'] = nf_subj_head_phrase_simple.replace('$', infostr)
            # ph_rules['nf-subj-head-phrase'] = 'nf-subj-head'
            # ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            pass
        elif pos == 'postverbal':
            #Todo THIS
            #There is no constraint for this type. (i.e. in the basic word order: S[O]V / O[S]V )
            pass
        else:
            pass
    elif wo == 'free':
        # future research
        pass
        if pos == 'clause-initial' or infostr_type == 'topic-first':
            ph_types['head-subj-phrase'] = """head-subj-phrase :=
                                        [ SYNSEM.L-PERIPH #periph,
                                          HEAD-DTR.SYNSEM.L-PERIPH #periph ].."""
            tdl = """head-periph-subj-phrase := decl-head-subj-phrase & head-initial-head-nexus &
                [ SYNSEM.L-PERIPH +,
                  HEAD-DTR.SYNSEM [ L-PERIPH +,
                                    LOCAL [ CAT.VAL.COMPS < [], ... >,
                                            CONT.HOOK.ICONS-KEY $ ] ] ]."""
            ph_types['head-periph-subj-phrase'] = tdl.replace('$', infostr)
            ph_rules['head-periph-subj-phrase'] = 'head-periph-subj'

            ph_types['subj-head-phrase'] = """subj-head-phrase :=
                                         [ SYNSEM.L-PERIPH -,
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < [], ... >,
                                           NON-HEAD-DTR.SYNSEM.L-PERIPH - ]."""
            tdl = """periph-subj-head-phrase := decl-head-subj-phrase & head-final-head-nexus &
                [ SYNSEM.L-PERIPH +,
                  HEAD-DTR.SYNSEM [ L-PERIPH -,
                                    LOCAL.CAT.VAL.COMPS < > ],
                  NON-HEAD-DTR.SYNSEM [ L-PERIPH +,
                                        LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            ph_types['periph-subj-head-phrase'] = tdl.replace('$', infostr)
            ph_rules['periph-subj-head-phrase'] = 'periph-subj-head'

            ph_types['head-comp-phrase'] = """head-comp-phrase :=
                                         [ SYNSEM.L-PERIPH #periph,
                                           HEAD-DTR.SYNSEM.L-PERIPH #periph ]."""
            tdl = """head-periph-comp-phrase := basic-head-1st-comp-phrase & head-initial-head-nexus &
                [ SYNSEM.L-PERIPH +,
                  HEAD-DTR.SYNSEM [ L-PERIPH +,
                                    LOCAL [ CAT.VAL.SUBJ.FIRST [ ],
				            CONT.HOOK.ICONS-KEY $ ] ] ]."""
            ph_types['head-periph-comp-phrase'] = tdl.replace('$', infostr)
            ph_rules['head-periph-comp-phrase'] = 'head-periph-comp'

            ph_types['comp-head-phrase'] = """comp-head-phrase :=
                                        [ SYNSEM.L-PERIPH -,
                                          HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < [], ... >,
                                          NON-HEAD-DTR.SYNSEM.L-PERIPH - ]."""
            tdl = """periph-comp-head-phrase := basic-head-1st-comp-phrase & head-final-head-nexus &
                [ SYNSEM.L-PERIPH +,
                  HEAD-DTR.SYNSEM [ L-PERIPH -, 
                                    LOCAL.CAT.VAL.SUBJ < > ],
                  NON-HEAD-DTR.SYNSEM [ L-PERIPH +,
					LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            ph_types['periph-comp-head-phrase'] = tdl.replace('$', infostr)
            ph_rules['periph-comp-head-phrase'] = 'periph-comp-head'

        elif pos == 'clause-final':
            ph_types['head-subj-phrase'] = """head-subj-phrase :=
                                         [ SYNSEM.R-PERIPH -,
                                           HEAD-DTR.SYNSEM [ R-PERIPH na-or--,
					                     LOCAL.CAT.VAL.COMPS.FIRST [ ] ] ]."""
            tdl = """head-periph-subj-phrase := decl-head-subj-phrase & head-initial-head-nexus &
                [ SYNSEM.R-PERIPH +,
                  HEAD-DTR.SYNSEM.R-PERIPH -,
                  NON-HEAD-DTR.SYNSEM [ R-PERIPH +,
                                        LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            ph_types['head-periph-subj-phrase'] = tdl.replace('$', infostr)
            ph_rules['head-periph-subj-phrase'] = 'head-periph-subj'

            ph_types['subj-head-phrase'] = """subj-head-phrase :=
                                         [ SYNSEM.R-PERIPH +,
                                           HEAD-DTR.SYNSEM [ R-PERIPH na-or-+,
                                                             LOCAL.CAT.MKG fc-only ] ]."""
            tdl = """periph-subj-head-phrase := decl-head-subj-phrase & head-final-head-nexus &
               [ SYNSEM.R-PERIPH +,
                 HEAD-DTR.SYNSEM [ R-PERIPH +,
	                           LOCAL [ CAT.VAL.COMPS < [], ... >,
                                           CONT.HOOK.ICONS-KEY $ ] ] ]."""
            ph_types['periph-subj-head-phrase'] = tdl.replace('$', infostr)
            ph_rules['periph-subj-head-phrase'] = 'periph-subj-head'

            ph_types['head-comp-phrase'] = """head-comp-phrase :=
                                        [ SYNSEM.R-PERIPH -,
                                          HEAD-DTR.SYNSEM [ R-PERIPH na-or--,
					                    LOCAL.CAT.VAL.SUBJ.FIRST [ ] ] ]."""
            tdl = """head-periph-comp-phrase := basic-head-1st-comp-phrase & head-initial-head-nexus &
                 [ SYNSEM.R-PERIPH +,
                   HEAD-DTR.SYNSEM.R-PERIPH -, 
                   NON-HEAD-DTR.SYNSEM [ R-PERIPH +,
                                         LOCAL.CONT.HOOK.ICONS-KEY $ ] ]."""
            ph_types['head-periph-comp-phrase'] = tdl.replace('$', infostr)
            ph_rules['head-periph-comp-phrase'] = 'head-periph-comp'

            ph_types['comp-head-phrase'] = """comp-head-phrase :=
                                        [ SYNSEM.R-PERIPH +,
                                          HEAD-DTR.SYNSEM [ R-PERIPH na-or-+,
                                                            LOCAL.CAT.MKG fc-only ] ]."""
            tdl = """periph-comp-head-phrase := basic-head-1st-comp-phrase & head-final-head-nexus &
               [ SYNSEM.R-PERIPH +,
                 HEAD-DTR.SYNSEM [ R-PERIPH +,
                                   LOCAL [ CAT.VAL.SUBJ.FIRST [ ],
                                           CONT.HOOK.ICONS-KEY $ ] ] ]."""
            ph_types['periph-comp-head-phrase'] = tdl.replace('$', infostr)
            ph_rules['periph-comp-head-phrase'] = 'periph-comp-head'
        elif pos == 'preverbal':
            ph_types['subj-head-phrase'] = 'subj-head-phrase := [ HEAD-DTR.SYNSEM.LIGHT - ].'
            ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT -, HEAD-DTR.SYNSEM.LIGHT - ].'
            ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            ph_types['nf-subj-head-phrase'] = nf_subj_head_phrase_simple.replace('$', infostr)
            ph_rules['nf-subj-head-phrase'] = 'nf-subj-head'
            ph_types['nf-comp-head-phrase'] = nf_comp_head_phrase.replace('$', infostr)
            ph_rules['nf-comp-head-phrase'] = 'nf-comp-head'
        elif pos == 'postverbal':
            ph_types['comp-head-phrase'] = 'comp-head-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].'
            ph_types['head-subj-phrase'] = 'head-subj-phrase := [ HEAD-DTR.SYNSEM.LIGHT - ].'
            ph_types['head-comp-phrase'] = 'head-comp-phrase := [ SYNSEM.LOCAL.CAT.HC-LIGHT -, HEAD-DTR.SYNSEM.LIGHT - ].'
            ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase.replace('$', infostr)
            ph_rules['head-nf-subj-phrase'] = 'head-nf-subj'
            ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase.replace('$', infostr)
            ph_rules['head-nf-comp-phrase'] = 'head-nf-comp'
        else:
            pass
    elif wo == 'v2':
        if pos == 'clause-initial' or pos == 'preverbal' or infostr_type == 'topic-first':
            ph_types['nf-comp-head-phrase'] = nf_comp_head_phrase_v2.replace('$', infostr)
            ph_rules['nf-comp-head-phrase'] = 'nf-comp-head'
            ph_types['head-nf-subj-phrase-super'] = head_nf_subj_phrase_super
            ph_types['nf-subj-head-phrase'] = nf_subj_head_phrase_v2.replace('$', infostr)
            ph_rules['nf-subj-head-phrase'] = 'nf-subj-head'
            tdl = """head-subj-phrase-2 := basic-head-subj-phrase & declarative-clause & head-initial &
               [ HEAD-DTR.SYNSEM [ INFOSTR-FLAG +,
                                   LIGHT -,   
                                   LOCAL.CAT.VAL.COMPS < > ],
                 NON-HEAD-DTR.SYNSEM.INFOSTR-FLAG - ]."""
            ph_types['head-subj-phrase-2'] = tdl
            ph_rules['head-subj-phrase-2'] = 'head-subj-2'
            tdl = """head-comp-phrase-3 := basic-head-1st-comp-phrase & head-initial &
               [ HEAD-DTR.SYNSEM [ INFOSTR-FLAG +,
                                   LIGHT -,
                                   LOCAL.CAT.VAL.SUBJ < > ],
                 NON-HEAD-DTR.SYNSEM.INFOSTR-FLAG - ]."""
            ph_types['head-comp-phrase-3'] = tdl
            ph_rules['head-comp-phrase-3'] = 'head-comp-3'
            ph_types['head-comp-phrase'] = "head-comp-phrase := [ SYNSEM.LIGHT - ]."
            ph_types['head-comp-phrase-2'] = "head-comp-phrase-2 := [ SYNSEM.LIGHT - ]."
        elif pos == 'clause-final':
            #there is no this type of language!
            pass
        elif pos == 'postverbal':
            ph_types['head-nf-subj-phrase'] = head_nf_subj_phrase_v2.replace('$', infostr)
            ph_rules['head-nf-subj-phrase'] = 'head-nf-subj'
            ph_types['head-nf-comp-phrase'] = head_nf_comp_phrase_v2.replace('$', infostr)
            ph_rules['head-nf-comp-phrase'] = 'head-nf-comp'
            ph_types['comp-head-phrase'] = "comp-head-phrase := [ SYNSEM.LIGHT - ]."
            ph_types['comp-head-phrase-2'] = "comp-head-phrase := [ SYNSEM.LIGHT - ]."
        else:
            pass
    else:
        pass

    add_ph_types(mylang, ph_types)
    add_ph_rules(rules, ph_rules)


def customize_information_structure_pos(mylang, ch, rules):
    pos_type = ch.get('focus-pos').strip() + ';' + ch.get('topic-first').strip()  + ';'
    pos_type += ch.get('c-focus').strip() + ';' + ch.get('c-focus-pos').strip()
    if pos_type in g_pos_types.keys():
        pos = g_pos_types[pos_type][0].strip().split(';')
        for i in range(0,len(pos)):
            infostr = pos[i].strip().split('|')
            if len(infostr) == 2:
                customize_information_structure_pos_once(mylang, ch, rules, infostr[0].strip(), infostr[1].strip(), g_pos_types[pos_type][i+1])


def customize_information_structure_marker(mylang, ch, rules, irules, lexicon, trigger, marker, infostr):
    if len(ch.get(marker)) < 1:
        return

    mkg = 'mkg'
    l_periph = 'luk'
    if marker in ['focus-marker', 'c-focus-marker']:
        mkg = 'fc'
    elif marker in ['topic-marker', 'c-topic-marker']:
        mkg = 'tp'
        if marker == 'topic-marker' and ch.get('topic-first').strip() != '':
            l_periph = '+'

    for m in ch.get(marker):
        _type = m['type'].strip()
        _pos = m['pos'].strip()
        _cat = m['cat'].strip()
        _orth = m['orth'].strip()
        _head = ''
        _light = 'luk'
        if _cat == 'nouns':
            _head = 'noun'
            _light = '-'
        elif _cat == 'verbs':
            _head = 'verb'
            _light = '+'
        else: #both
            _head = '+nv'

        if _type == 'modifier':
            tdl = """infostr-marking-mod-lex := no-rels-hcons-lex-item & one-icons-lex-item &
               [ SYNSEM.LOCAL [ CAT [ HEAD adv & [ MOD < [ LIGHT $
                                                           LOCAL [ CAT.MKG [ FC na-or--, TP na-or-- ],
                                                                   CONT.HOOK [ INDEX #target,
                                                                               ICONS-KEY #icons] ] ] > ],
                                      VAL [ SUBJ < >, COMPS < >, SPR < >, SPEC < > ] ],
                                CONT.ICONS <! #icons & [ IARG2 #target ] !> ] ]."""

            tdl = tdl.replace('$', _light)
            add_lextypes(mylang, tdl)
            modifier_lex = infostr + '-marking-mod-lex'
            tdl = modifier_lex + ' := infostr-marking-mod-lex & '
            tdl += '[ SYNSEM.LOCAL.CAT [ MKG ' + mkg + ', HEAD.MOD < [ L-PERIPH ' + l_periph + ', \
                                                                 LOCAL [ CAT.HEAD ' + _head + ', \
                                                                         CONT.HOOK.ICONS-KEY ' + infostr + ' ] ] > ] ].'
            add_lextypes(mylang, tdl)

            tdl = rule = ''
            if _pos == 'before':
                rule = 'adj-head-int := adj-head-int-phrase.'
                rules.add(rule)
            elif _pos == 'after':
                rule = 'head-adj-int := head-adj-int-phrase.'
                rules.add(rule)
            else: #both
                rule = 'adj-head-int := adj-head-int-phrase.'
                rules.add(rule)
                rule = 'head-adj-int := head-adj-int-phrase.'
                rules.add(rule)

            orth = orth_encode(_orth)
            if _head == '+nv':
                modifier1 = TDLencode(_orth + '-marker-n')
                modifier2 = TDLencode(_orth + '-marker-v')
                tdl1 = modifier1 + ' := ' + modifier_lex + ' & \
                        [ STEM < "' + orth + '" >, SYNSEM.LOCAL.CAT.HEAD.MOD < [ LIGHT -, LOCAL.CAT.HEAD noun ] > ].'
                tdl2 = modifier2 + ' := ' + modifier_lex + ' & \
                        [ STEM < "' + orth + '" >, SYNSEM.LOCAL.CAT.HEAD.MOD < [ LIGHT +, LOCAL.CAT.HEAD verb ] > ].'
                lexicon.add_literal(';;; Modifiers of expressing information structure')
                lexicon.add(tdl1)
                lexicon.add(tdl2)
                grdef1 = modifier1 +'_gr := arg0e_gtr & \
                        [ CONTEXT [ RELS <! [ PRED "non_existing_rel" ] !> ], \
                          FLAGS.TRIGGER "' + modifier1 + '" ].'
                grdef2 = modifier2 +'_gr := arg0e_gtr & \
                        [ CONTEXT [ RELS <! [ PRED "non_existing_rel" ] !> ], \
                          FLAGS.TRIGGER "' + modifier2 + '" ].'
                trigger.add(grdef1)
                trigger.add(grdef2)
            else:
                modifier = TDLencode(_orth + '-marker')
                tdl = modifier
                tdl += ' := ' + modifier_lex + ' & \
                        [ STEM < "' + orth + '" > ].'
                lexicon.add_literal(';;; Modifiers of expressing information structure')
                lexicon.add(tdl)
                grdef = modifier +'_gr := arg0e_gtr & \
                        [ CONTEXT [ RELS <! [ PRED "non_existing_rel" ] !> ], \
                          FLAGS.TRIGGER "' + modifier + '" ].'
                trigger.add(grdef)

        else:#affix or adp
            pass


def customize_infostr_adpositions(mylang, lexicon, trigger, ch):
    to_cfv = []

    comment = \
        ';;; Information structural adpositions\n' + \
        ';;; Information structural adpositions are constrained not to\n' + \
        ';;; be modifiers.'
    mylang.add_literal(comment)

    typedef = \
        'infostr-marking-adp-lex := basic-one-arg & raise-sem-lex-item & one-icons-lex-item & \
        [ SYNSEM.LOCAL [ CAT [ HEAD adp & [ MOD < > ], \
                               VAL [ SPR < >, \
                                     SUBJ < >, \
                                     COMPS < #comps & [ LOCAL.CONT.HOOK.INDEX #target ] >, \
                                     SPEC < > ] ], \
                         CONT [ HOOK.ICONS-KEY #icons, \
			        ICONS <! #icons & [ IARG2 #target ] !> ] ],\
          ARG-ST < #comps & [ LOCAL.CAT [ HEAD noun, \
                                          VAL.SPR < > ] ] > ].'
    mylang.add(typedef)

    # checking whether language has both prepositions and postpositions
    bidirectional = False
    infostr_marking = False
    no_case_adp = True
    adporders = []

    for adp in ch.get('adp',[]):
        for feat in adp.get('feat', []):
            if feat['name'] == 'case' and (feat['value'] == case or case == ''):
                no_case_adp = False
            if feat['name'] == 'information-structure meaning':
                infostr_marking = True
        if no_case_adp and infostr_marking:
            adp_order = adp.get('order')
            if not adp_order in adporders:
                adporders.append(adp_order)

    if len(adporders) == 2:
        bidirectional = True
        mylang.add('infostr-marking-prep-lex := infostr-marking-adp-lex & \
               [ SYNSEM.LOCAL.CAT.HEADFINAL - ].')
        mylang.add('infostr-marking-postp-lex := infostr-marking-adp-lex & \
               [ SYNSEM.LOCAL.CAT.HEADFINAL + ].')

    # Lexical entries
    lexicon.add_literal(';;; Information structural adpositions')
    for adp in ch.get('adp',[]):
        infostr_marking = False
        no_case_adp = True
        for feat in adp.get('feat', []):
            if feat['name'] == 'case' and (feat['value'] == case or case == ''):
                no_case_adp = False
            if feat['name'] == 'information-structure meaning':
                infostr_marking = True
        if not no_case_adp or not infostr_marking:
            continue

        orth = orth_encode(adp.get('orth'))
        infix_tname = 'ad'
        if bidirectional:
            if adp.get('order') == 'before':
                infix_tname = 'pre'
            elif adp.get('order') == 'after':
                infix_tname = 'post'

        super_type = 'infostr-marking-' + infix_tname + 'p-lex'
        adp_type = TDLencode(orth + '-marker')
        typedef = \
            adp_type + ' := ' + super_type + ' & \
                        [ STEM < "' + orth + '" > ].'
        lexicon.add(typedef)

        grdef1 = adp_type +'_gr_1 := arg0e_gtr & \
                    [ CONTEXT [ RELS <!  [ ARG1 individual & #i ] !> ], \
                      FLAGS [ SUBSUME < #i >, TRIGGER "' + adp_type + '" ] ].'
        grdef2 = adp_type +'_gr_2 := arg0e_gtr & \
                      [ CONTEXT [ RELS <!  [ ARG2 individual & #i ] !> ], \
                        FLAGS [ SUBSUME < #i >, TRIGGER "' + adp_type + '" ] ].'
        grdef3 = adp_type +'_gr_3 := arg0e_gtr & \
                      [ CONTEXT [ RELS <!  [ ARG3 individual & #i ] !> ], \
                        FLAGS [ SUBSUME < #i >, TRIGGER "' + adp_type + '" ] ].'
        trigger.add(grdef1)
        trigger.add(grdef2)
        trigger.add(grdef3)

        to_cfv += [(adp.full_key, adp_type, 'adp')]

    return to_cfv

def customize_information_structure(mylang, ch, rules, irules, lexicon, trigger, hierarchies):
    #if topic is always initial:
    if ch.get('topic-first').strip() != '':
        mylang.add_literal('topic-comment :+ [ SYNSEM.L-PERIPH +, NON-HEAD-DTR.SYNSEM.L-PERIPH + ].', '', section='addenda')

    customize_information_structure_pos(mylang, ch, rules)
    customize_information_structure_marker(mylang, ch, rules, irules, lexicon, trigger, 'focus-marker', 'focus')
    customize_information_structure_marker(mylang, ch, rules, irules, lexicon, trigger, 'topic-marker', 'topic')
    customize_information_structure_marker(mylang, ch, rules, irules, lexicon, trigger, 'c-focus-marker', 'contrast-focus')
    customize_information_structure_marker(mylang, ch, rules, irules, lexicon, trigger, 'c-topic-marker', 'contrast-topic')

