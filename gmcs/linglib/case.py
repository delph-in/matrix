from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy
from gmcs.utils import get_name

######################################################################
# customize_case()
#   Create the type definitions associated with the user's choices
#   about case.

# case_names()
#   Create and return a list containing information about the cases
#   in the language described by the current choices.  This list consists
#   of tuples with three values:
#     [canonical name, friendly name, abbreviation]
def case_names(ch):
    # first, make two lists: the canonical and user-provided case names
    cm = ch.get('case-marking')
    canon = []
    user = []
    if cm == 'nom-acc':
        canon.append('nom')
        user.append(ch[cm + '-nom-case-name'])
        canon.append('acc')
        user.append(ch[cm + '-acc-case-name'])
    elif cm == 'erg-abs':
        canon.append('erg')
        user.append(ch[cm + '-erg-case-name'])
        canon.append('abs')
        user.append(ch[cm + '-abs-case-name'])
    elif cm == 'tripartite':
        canon.append('s_case')
        user.append(ch[cm + '-s-case-name'])
        canon.append('a_case')
        user.append(ch[cm + '-a-case-name'])
        canon.append('o_case')
        user.append(ch[cm + '-o-case-name'])
    elif cm in ['split-s']:
        canon.append('a_case')
        user.append(ch[cm + '-a-case-name'])
        canon.append('o_case')
        user.append(ch[cm + '-o-case-name'])
    elif cm in ['fluid-s']:
        canon.append('a_case+o_case')
        user.append('fluid')
        canon.append('a_case')
        user.append(ch[cm + '-a-case-name'])
        canon.append('o_case')
        user.append(ch[cm + '-o-case-name'])
    elif cm in ['split-n', 'split-v']:
        canon.append('nom')
        user.append(ch[cm + '-nom-case-name'])
        canon.append('acc')
        user.append(ch[cm + '-acc-case-name'])
        canon.append('erg')
        user.append(ch[cm + '-erg-case-name'])
        canon.append('abs')
        user.append(ch[cm + '-abs-case-name'])
    elif cm in ['focus']:
        canon.append('focus')
        user.append(ch[cm + '-focus-case-name'])
        canon.append('a_case')
        user.append(ch[cm + '-a-case-name'])
        canon.append('o_case')
        user.append(ch[cm + '-o-case-name'])

    # fill in any additional cases the user has specified
    for case in ch.get('case'):
        canon.append(case['name'])
        user.append(case['name'])

    # if possible without causing collisions, shorten the case names to
    # three-letter abbreviations; otherwise, just use the names as the
    # abbreviations
    abbrev = [ l[0:3] for l in user ]
    if len(set(abbrev)) != len(abbrev):
        abbrev = user

    return zip(canon, user, abbrev)

# Given the canonical (i.e. choices variable) name of a case, return
# its abbreviation from the list of cases, which should be created by
# calling case_names().  If there is no abbreviation, return the name.
def canon_to_abbr(name, cases):
    for c in cases:
        if c[0] == name:
            return c[2]
    return name

# Given the name of a case, return its abbreviation from the list of
# cases, which should be created by calling case_names().  If there
# is no abbreviation, return the name.
def name_to_abbr(name, cases):
    for c in cases:
        if c[1] == name:
            return c[2]
    return name

def init_case_hierarchy(ch, hierarchies):
    cm = ch.get('case-marking')
    cases = case_names(ch)

    hier = TDLHierarchy('case')

    # For most case patterns, just make a flat hierarchy.  For fluid-s,
    # split-n and split-v, however, a more articulated hierarchy is required.
    if cm in ['nom-acc', 'erg-abs', 'tripartite', 'split-s', 'focus']:
        for c in cases:
            hier.add(c[2], 'case', c[1])
    elif cm in ['fluid-s']:
        abbr = canon_to_abbr('a_case+o_case', cases)
        for c in cases:
            if c[0] in ['a_case', 'o_case']:
                hier.add(c[2], abbr, c[1])
            else:
                hier.add(c[2], 'case', c[1])
    elif cm in ['split-n', 'split-v']:
        nom_a = canon_to_abbr('nom', cases)
        acc_a = canon_to_abbr('acc', cases)
        erg_a = canon_to_abbr('erg', cases)
        abs_a = canon_to_abbr('abs', cases)
        if cm == 'split-v':
            for c in cases:
                hier.add(c[2], 'case', c[1])
        else:  # 'split-n':
            hier.add('a_case', 'case', 'transitive agent')
            hier.add('s_case', 'case', 'intransitive subject')
            hier.add('o_case', 'case', 'transitive patient')
            for c in cases:
                if c[2] == erg_a:
                    hier.add(c[2], 'a_case', c[1])
                elif c[2] == nom_a:
                    hier.add(c[2], 'a_case', c[1])
                    hier.add(c[2], 's_case', c[1])
                elif c[2] == abs_a:
                    hier.add(c[2], 's_case', c[1])
                    hier.add(c[2], 'o_case', c[1])
                elif c[2] == acc_a:
                    hier.add(c[2], 'o_case', c[1])
                else:
                    hier.add(c[2], 'case', c[1])

    if not hier.is_empty():
        hierarchies[hier.name] = hier


# customize_case_type()
#   Create a type for case

def customize_case_type(mylang, hierarchies):
    if 'case' in hierarchies:
        hierarchies['case'].save(mylang)

# customize_trigger_rules()
#   Create trigger rules for case-marking adpositions
def customize_trigger_rules(adp_type, trigger):

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

# customize_case_adpositions()
#   Create the appropriate types for case-marking adpositions
def customize_case_adpositions(mylang, lexicon, trigger, ch):
    cases = case_names(ch)
    #features = ch.features()
    to_cfv = []

    if ch.has_adp_case():
        comment = \
            ';;; Case-marking adpositions\n' + \
            ';;; Case marking adpositions are constrained not to\n' + \
            ';;; be modifiers.'
        mylang.add_literal(comment)

        mylang.add('+np :+ [ CASE case ].', section='addenda')

        typedef = \
            'case-marking-adp-lex := basic-one-arg & raise-sem-lex-item & \
                [ SYNSEM.LOCAL.CAT [ HEAD adp & [ CASE #case, MOD < > ], \
                                     VAL [ SPR < >, \
                                           SUBJ < >, \
                                           COMPS < #comps >, \
                                           SPEC < > ]], \
                  ARG-ST < #comps & [ LOCAL.CAT [ HEAD noun & [ CASE #case ], \
                                                  VAL.SPR < > ]] > ].'
        mylang.add(typedef)

        if ch.has_mixed_case():
            mylang.add('+np :+ [ CASE-MARKED bool ].', section='addenda')
            mylang.add(
                'case-marking-adp-lex := \
                 [ ARG-ST < [ LOCAL.CAT.HEAD.CASE-MARKED - ] > ].')


        # checking whether language has both prepositions and postpositions
        bidirectional = False
        adporders = []
        for adp in ch.get('adp',[]):
            adp_order = adp.get('order')
            if not adp_order in adporders:
                adporders.append(adp_order)
        if len(adporders) == 2:
            bidirectional = True
            mylang.add('case-marking-prep-lex := case-marking-adp-lex & \
               [ SYNSEM.LOCAL.CAT.HEADFINAL - ].')
            mylang.add('case-marking-postp-lex := case-marking-adp-lex & \
               [ SYNSEM.LOCAL.CAT.HEADFINAL + ].')


        # Lexical entries
        lexicon.add_literal(';;; Case-marking adpositions')

        for adp in ch.get('adp',[]):
            orth = orth_encode(adp.get('orth'))
            infix_tname = 'ad'
            if bidirectional:
                if adp.get('order') == 'before':
                    infix_tname = 'pre'
                elif adp.get('order') == 'after':
                    infix_tname = 'post'

            super_type = 'case-marking-' + infix_tname + 'p-lex'
            # figure out the abbreviation for the case this adp marks
            cn = ''
            abbr = ''
            for feat in adp.get('feat', []):
                if feat['name'] == 'case':
                    cn = feat['value']
                    break

            abbr = name_to_abbr(cn, cases)

            adp_type = TDLencode(abbr + '-marker')
            typedef = \
                adp_type + ' := ' + super_type + ' & \
                        [ STEM < "' + orth + '" > ].'
            lexicon.add(typedef)

            has_inforstr_feat = False
            for feat in adp.get('feat', []):
                if feat['name'] == "information-structure meaning":
                    has_inforstr_feat = True
                    typedef = \
                        adp_type + ' := [ SYNSEM.LOCAL [ CAT.VAL.COMPS < [ LOCAL.CONT.HOOK.INDEX #target ] >, \
                                           CONT [ HOOK.ICONS-KEY #icons, \
                                                  ICONS <! info-str & #icons & [ IARG2 #target ] !> ] ] ] ].'
                    lexicon.add(typedef)
                    break
            if not has_inforstr_feat:
                typedef = \
                    adp_type + ' := [ SYNSEM.LOCAL.CONT [ HOOK [ ICONS-KEY.IARG1 #clause, CLAUSE-KEY #clause ], ICONS <! !> ] ].'
                lexicon.add(typedef)

            if cn.strip() != '':
                customize_trigger_rules(adp_type, trigger)

            to_cfv += [(adp.full_key, adp_type, 'adp')]

    return to_cfv

def customize_case(mylang, ch, hierarchies):
    # first we need to peek in things like lex-rules for when something
    # has mixed case, and replace with the appropriate type covering
    from gmcs.linglib.lexbase import ALL_LEX_TYPES
    cases = case_names(ch)
    for x in ALL_LEX_TYPES:
        for lex_type in ch[x]:
            convert_mixed_case(lex_type, hierarchies, cases)
        for pc in ch[x + '-pc']:
            convert_mixed_case(pc, hierarchies, cases)
            for lrt in pc['lrt']:
                convert_mixed_case(lrt, hierarchies, cases)
    # now output the case hierarchies
    customize_case_type(mylang, hierarchies)

def convert_mixed_case(item, hierarchies, cases):
    for feat in item.get('feat',[]):
        if feat['name'] == 'case' and ',' in feat['value']:
            v = [canon_to_abbr(c, cases) for c in feat['value'].split(', ')]
            feat['value'] = hierarchies['case'].get_type_covering(v)

def add_lexrules(ch):
    # only need to add rules for mixed_case
    if not ch.has_mixed_case():
        return
    for pc in ch['noun-pc']:
        # TJT 2014-09-08: changing to set comprehension for speed
        #feature_names = {feat['name'] for lrt in pc['lrt'] for feat in lrt['feat']}
        feature_names = set()
        for lrt in pc['lrt']:
            for feat in lrt['feat']:
                feature_names.add(feat['name'])
        if 'case' in feature_names:
            for c in case_names(ch):
                if ch.has_adp_case(c[0]):
                    idx = ch[pc.full_key + '_lrt'].next_iter_num()
                    lrt_key = pc.full_key + '_lrt' + str(idx)
                    ch[lrt_key + '_name'] = get_name(pc) + '-synth-' + c[0]
                    ch[lrt_key + '_feat1_name'] = 'case'
                    ch[lrt_key + '_feat1_value'] = c[0]
                    ch[lrt_key + '_lri1_inflecting'] = 'no'
                    ch[lrt_key + '_lri1_orth'] = ''

def interpret_verb_valence(valence):
    '''
    Return the canonical valence name (e.g. iverb, tverb) given the
    valence for a verb as defined in a choices file.
    '''
    if valence == 'trans' or '-' in valence:
        return 'tverb'
    else:
        return 'iverb'



def customize_verb_case(mylang, ch):
    cm = ch.get('case-marking')
    cases = case_names(ch)

    # Pass through the list of case-marking patterns.  If a pattern is a
    # lexical pattern (i.e. the third item in the list is False), then
    # create and constrain the appropriate lexical type.  This type is a
    # subtype of either transitive-verb-lex or intransitive-verb-lex.
    #
    # Note: I specify ARG-ST.FIRST... below instead of ARG-ST < [], ...>
    # because TDLFile has trouble with merges and open-ended lists.
    # Which should get fixed...  - sfd

    # OZ: This currently also adds clausal types.

    for p in ch.patterns():
        rule_pattern = p[2]
        p = p[0].split(',')  # split off ',dirinv', if present
        dir_inv = ''
        clausal = ''
        if len(p) > 1:
            if p[1] == 'dirinv':
                dir_inv = 'dir-inv-'
            elif p[1].startswith('comps'):
                clausal = 'clausal-'
        if not rule_pattern:
            c = p[0].split('-')  # split 'agentcase-patientcase'
            if p[0] == 'trans' or len(c) > 1:  # transitive
                if p[0] == 'trans':
                    a_case = ''
                    o_case = ''
                    a_head = ch.case_head()
                    if not clausal:
                        o_head = ch.case_head()
                    else:
                        o_head = ''
                else:
                    a_case = canon_to_abbr(c[0], cases)
                    o_case = canon_to_abbr(c[1], cases)
                    a_head = ch.case_head(c[0])
                    if not clausal:
                        o_head = ch.case_head(c[1])
                    elif o_case:
                        o_head = ch.case_head(c[1])

                if a_case and o_case:
                    if not clausal:
                        t_type = dir_inv + a_case + '-' + o_case + '-transitive-verb-lex'
                    else:
                        t_type = clausal + a_case + '-' + o_case + '-verb-lex'
                else:
                    if not clausal:
                        t_type = dir_inv + 'transitive-verb-lex'
                    else:
                        t_type = clausal + 'verb-lex'
                if t_type != 'transitive-verb-lex' and t_type != 'clausal-verb-lex':
                    if not clausal:
                        mylang.add(t_type + ' := transitive-verb-lex.')
                    else:
                        mylang.add(t_type + ' := clausal-verb-lex.')
                # constrain the head of the agent/subject
                typedef = \
                    t_type + ' := \
          [ ARG-ST.FIRST.LOCAL.CAT.HEAD ' + a_head + ' ].'
                mylang.add(typedef)

                # constrain the case of the agent/subject
                if a_case:
                    typedef = \
                        t_type + ' := \
            [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + a_case + ' ].'
                    mylang.add(typedef)

                # constrain CASE-MARKING of the agent/subject, if appropriate
                if a_case and ch.has_mixed_case() and not ch.has_optadp_case(a_case):
                    typedef = \
                        t_type + ' := \
            [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE-MARKED + ] > ].'
                    mylang.add(typedef)

                # constrain the head of the patient/object
                if o_head:
                    typedef = \
                        t_type + ' := \
          [ ARG-ST < [ ], [ LOCAL.CAT.HEAD ' + o_head + ' ] > ].'
                    mylang.add(typedef)

                # constrain the case of the patient/object
                if o_case:
                    typedef = \
                        t_type + ' := \
            [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.CASE ' + o_case + ' ] > ].'
                    mylang.add(typedef)

                # constrain CASE-MARKING of the patient/object, if appropriate
                if o_case and ch.has_mixed_case() and not ch.has_optadp_case(o_case):
                    typedef = \
                        t_type + ' := \
            [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.CASE-MARKED + ] > ].'
                    mylang.add(typedef)
            else:     # intransitive or clausal with constrained subject
                if c[0] == 'intrans':
                    s_case = ''
                    s_head = ch.case_head()
                else:
                    s_case = canon_to_abbr(c[0], cases)
                    s_head = ch.case_head(c[0])

                if s_case:
                    if not clausal:
                        i_type = dir_inv + s_case + '-intransitive-verb-lex'
                    else:
                        i_type = clausal + s_case + '-verb-lex'
                else:
                    if not clausal:
                        i_type = dir_inv + 'intransitive-verb-lex'
                    else:
                        i_type = clausal + 'verb-lex'

                if i_type != 'intransitive-verb-lex' and i_type !='clausal-verb-lex':
                    if not clausal:
                        mylang.add(i_type + ' := intransitive-verb-lex.')
                    else:
                        mylang.add(i_type + ' := clausal-verb-lex.')

                # constrain the head of the subject
                typedef = \
                    i_type + ' := \
          [ ARG-ST.FIRST.LOCAL.CAT.HEAD ' + s_head + ' ].'
                mylang.add(typedef)

                # constrain the case of the subject
                if s_case:
                    typedef = \
                        i_type + ' := \
            [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + s_case + ' ].'
                    mylang.add(typedef)

                # constrain CASE-MARKING of the subject, if appropriate
                if s_case and ch.has_mixed_case() and not ch.has_optadp_case(s_case):
                    typedef = \
                        i_type + ' := \
            [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE-MARKED + ] > ].'
                    mylang.add(typedef)





##################
### VALIDATION ###
##################

def validate(choices, vr):
    cm = choices.get('case-marking')

    if not cm:
        vr.err('case-marking', 'You must specify if/how case is marked.')

    if cm in ('nom-acc', 'split-n', 'split-v'):
        validate_one_case(choices, vr, cm + '-nom')
        validate_one_case(choices, vr, cm + '-acc')
    if cm in ('erg-abs', 'split-n', 'split-v'):
        validate_one_case(choices, vr, cm + '-erg')
        validate_one_case(choices, vr, cm + '-abs')
    if cm in ('tripartite', 'split-s', 'fluid-s', 'focus'):
        validate_one_case(choices, vr, cm + '-a')
        validate_one_case(choices, vr, cm + '-o')
    if cm in ('tripartite'):
        validate_one_case(choices, vr, cm + '-s')
    if cm in ('focus'):
        validate_one_case(choices, vr, cm + '-focus')

    if cm == 'none' and 'case' in choices:
        for case in choices['case']:
            vr.err(case.full_key + '_name',
                   'You may not specify additional cases ' +
                   'if your language has no case marking.')

    if 'scale' in choices and not choices.get('scale-equal'):
        vr.err('scale-equal',
               'If you define a direct-inverse scale, ' +
               'you must say what direction the verb is ' +
               'when the agent and patient have equal rank.')

######################################################################
# validate_one_case(pre)
#   A helper function to validate the user's choices about one case.
#   pre is the first few characters of the associated choices names
#  (e.g. 'nom-acc-nom')

def validate_one_case(ch, vr, pre):
    if not ch.get(pre + '-case-name'):
        vr.err(pre + '-case-name', 'You must specify a name for every case.')

