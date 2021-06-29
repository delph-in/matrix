from collections import defaultdict

from gmcs import feature_type_use

from gmcs.utils import get_name
from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

from gmcs.linglib import lexbase
from gmcs.linglib import case
from gmcs.linglib import features
from gmcs.linglib import auxiliaries
from gmcs.linglib import information_structure
from gmcs.linglib import clausalcomps
from gmcs.linglib.parameters import determine_vcluster
from gmcs.linglib.lexbase import ALL_LEX_TYPES, LEXICAL_SUPERTYPES
from gmcs.linglib.lexicon import get_all_supertypes
from gmcs.linglib.clausalmods import get_subord_stemids
from gmcs.linglib.clausalmods import add_subord_name
from gmcs.feature_type_use import USED_TYPES

# helper functions


def verb_id(item):
    """Return the identifier for a verb lexical item."""
    if not item.full_key.startswith('qverb'):
        return get_name(item) + '-verb-lex'
    else:
        return get_name(item) + '-interrogative-verb-lex'


def noun_id(item):
    """Return the identifier for a noun lexical item."""
    return get_name(item) + '-noun-lex'


def adj_id(item):
    """Return the identifier for an adjective lexical item."""
    return get_name(item) + '-adj-lex'


def cop_id(item):
    """Return the identifier for an copula lexical item."""
    return get_name(item) + '-cop-lex'


def det_id(item):
    """Return the identifier for a determiner lexical item."""
    return get_name(item) + '-determiner-lex'


def adp_id(item):
    """Return the identifier for an adposition lexical item."""
    return get_name(item) + '-adp-lex'


def qpart_id(item):
    """Return the identifier for a question particle lexical item."""
    return get_name(item) + '-lex'

##########################################################
# insert_ids()


def insert_ids(ch):
    """
    Create a unique identifier for each lexical entry based
    on the stem value but allowing for separate lexical items
    with the same stem.  Store in the choices file object.
    """
    stemids = {}
    stemidcounters = {}
    postypes = ALL_LEX_TYPES

    for postype in postypes:
        for pos in ch.get(postype):
            # For ordinary stems, use the stem orthography itself
            # as the basis of the identifier.
            for stem in pos.get('stem'):
                orth = stem.get('orth')
                if orth in list(stemids.keys()):
                    stemids[orth] += 1
                else:
                    stemids[orth] = 1
            # For bistems, build the identifier out of the orthography
            # plus the affix, but store these in the same dictionary
            # to account for possible name-space collisions.
            for bistem in pos.get('bistem'):
                aff = bistem.get('aff')
                orth = bistem.get('orth')
                id = orth + '+' + aff

                if id in list(stemids.keys()):
                    stemids[id] += 1
                else:
                    stemids[id] = 1
    # KPH Subordinators are added outside the lexicon,
    # but should still be checked for possible name-space collisions
    # this should be a temporary/hacky fix- a bug has been filed
    stemids = get_subord_stemids(ch, stemids)

    # Now that stemids has the full count, go through and add
    # to the choices file object.

    for postype in postypes:
        for pos in ch.get(postype):
            for stem in pos.get('stem'):
                orth = stem.get('orth')
                if stemids[orth] == 1:
                    ch[stem.full_key + '_name'] = orth
                elif orth not in stemidcounters:
                    stemidcounters[orth] = 1
                    ch[stem.full_key + '_name'] = orth + '_1'
                else:
                    stemidcounters[orth] += 1
                    ch[stem.full_key + '_name'] = orth + \
                        '_' + str(stemidcounters[orth])
            for bistem in pos.get('bistem'):
                orth = bistem.get('orth') + '+' + bistem.get('aff')
                if stemids[orth] == 1:
                    ch[bistem.full_key + '_name'] = orth
                elif orth not in stemidcounters:
                    stemidcounters[orth] = 1
                    ch[bistem.full_key + '_name'] = orth + '_1'
                else:
                    stemidcounters[orth] += 1
                    ch[bistem.full_key + '_name'] = orth + \
                        '_' + str(stemidcounters[orth])
    # KPH Do the same for subordinators and complementizers
    add_subord_name(ch, stemids, stemidcounters)


##########################################################
# customize_verbs()

def customize_bipartite_stems(ch):
    """
    Users specify bipartite stems as roots + affixes in bipartite
    stem specifications plus position class for affix in lexical type.
    Take this information and add choices that create the lexical
    rules as well as the constraints that make sure that the two
    parts appear together.
    """
    # For each verb type
    for verb in ch.get('verb'):

        # Check whether there are bipartite stems
        bistems = verb.get('bistem')
        if bistems:
            # Find position class for affixes

            pcname = verb.get('bipartitepc')
            pc = None
            for vpc in ch.get('verb-pc'):
                if vpc.full_key == pcname:
                    pc = vpc

            # Make dictionary with affixes as keys and lists
            # of stems as values.  This will let us find out if
            # any verbs share same affix
            avpairs = {}
            for stem in bistems:
                aff = stem.get('aff')
                #orth = stem.get('orth')

                # Update affix-stem dictionary
                if aff in list(avpairs.keys()):
                    avpairs[aff].append(stem.full_key)
                else:
                    avpairs[aff] = [stem.full_key]

            # Get stem list again because I want access to the
            # info I've added since first initializing stems
            bistems = verb.get('bistem')

            for aff in list(avpairs.keys()):
                # Get iter number for lrts:
                if pc['lrt']:
                    iternum = str(pc['lrt'].next_iter_num())
                else:
                    iternum = '1'

                # Create lexical rules types and instances for each affix
                next_lrt_str = pc.full_key + '_lrt' + iternum
                ch[next_lrt_str + '_require1_others'] = ', '.join(avpairs[aff])
                ch[next_lrt_str + '_lri1_orth'] = aff
                ch[next_lrt_str + '_lri1_inflecting'] = 'yes'

                # Add requires constrains on stems
                for stemid in avpairs[aff]:
                    ch[stemid + '_require1_others'] = next_lrt_str


def customize_verbs(mylang, ch, lexicon, hierarchies):
    negmod = ch.get('neg-mod')
    negadv = ch.get('neg-adv')
    wo = ch.get('word-order')
    auxcomp = ch.get('aux-comp')
    auxorder = ch.get('aux-comp-order')
    # Do we need to constrain HC-LIGHT on verbs, to distinguish V from VP?
    hclight = (negadv == 'ind-adv' and negmod == 'v')
    hclightallverbs = False

    if ch.get('has-aux') == 'yes':
        # TODO: OZ 11-30-2017 Reconcile this with my VC stuff in word_order.py
        vc = determine_vcluster(auxcomp, auxorder, wo, ch)
        if wo == 'vso' or wo == 'osv':
            wo = 'req-hcl-vp'
        if auxcomp == 'v' and hclight != True:
            hclight = True
            if wo != 'free' or vc == True:
                hclightallverbs = True
        if auxcomp == 'vp' and wo == 'req-hcl-vp':
            hclightallverbs = True
    else:
        vc = False

    if wo == 'req-hcl-vp':
        wo = ch.get('word-order')

    # Lexical types for verbs
    # I'm adding the constraint to associate XARG with the
    # first ARG-ST element here (so raising auxiliaries work),
    # but perhaps this belongs in matrix.tdl?  Or maybe this
    # is another module/parameter (like, the external argument
    # might not be the first one?

    mainorverbtype = main_or_verb(ch)
    # The variable mainorverbtype is a type name for lexical/main (non-aux) verbs.
    # Note that the use of 'main' instead of 'lexical' is strictly for
    # coding clarity
    # If there are auxiliaries, non-aux verbs are 'main-verb-lex', and 'verb-lex'
    # includes both aux and lexical/main verbs.
    # If there are no auxiliaries then 'verb-lex' covers all verbs

    # Neither mainverbs or auxs should start out as modifiers (for now)
    # Assigning constraint to verb-lex

    if ch.get('has-aux') == 'yes':
        mylang.add('head :+ [ AUX bool ].', section='addenda')
        #mainorverbtype = 'main-verb-lex'

        # we need to know whether the auxiliaries form a vcluster

        auxcomp = ch.get('aux-comp')
        wo = ch.get('word-order')
        auxorder = ch.get('aux-comp-order')
        vcluster = determine_vcluster(auxcomp, auxorder, wo, ch)

        # OZ 2020-02-20 May need to push this down for question verbs.
        typedef = \
            'verb-lex := non-mod-lex-item & \
                       [ SYNSEM [ LOCAL.CAT.HEAD verb, L-QUE - ] ].'
        mylang.add(typedef)
        typedef = \
            'main-verb-lex := verb-lex & basic-verb-lex & \
                            [ SYNSEM [ LOCAL.CAT.HEAD.AUX -,' \
                                       'L-QUE - ] ].'
        mylang.add(typedef)
        typedef = \
            'aux-lex := verb-lex & basic-icons-lex-item & \
                      [ SYNSEM [ LOCAL.CAT.HEAD.AUX +,' \
                                'L-QUE - ] ].'
        mylang.add(typedef)

        if vcluster:
            mylang.add('main-verb-lex := [ SYNSEM.LOCAL.CAT.VC + ].')
            mylang.add('aux-lex := [ SYNSEM.LOCAL.CAT.VC - ].')
    else:
        #mainorverbtype = 'verb-lex'
        vcluster = False
        mylang.add('verb-lex := basic-verb-lex & non-mod-lex-item.')

    typedef = mainorverbtype + ' := basic-non-wh-word-lex &  \
       [ SYNSEM.LOCAL [ CAT [ VAL [ SPEC < >, \
                                  SUBJ < #subj > ] ], \
                        CONT.HOOK.XARG #xarg ], \
         ARG-ST < #subj & \
                  [ LOCAL [ CAT cat-sat & [ VAL [ SPR < >, \
                                      COMPS < > ] ], \
                            CONT.HOOK.INDEX #xarg ] ], ... > ].'
    mylang.add(typedef)

    if hclightallverbs:
        mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')
    elif hclight:
        comment = \
            ';;; If there are aspects of the syntax which pick out\n' + \
            ';;; lexical Vs (transitive or intransitive) such as V-attachment\n' + \
            ';;; of adverbs or argument composition auxiliaries which take V\n' + \
            ';;; complements, we need to distinguish (intranstive) V and VP.\n' + \
            ';;; To do so, we make use of a feature LIGHT.  Phrases are\n' + \
            ';;; generally [LIGHT -] with the exception of head-complement\n' + \
            ';;; phrases, which take their value for LIGHT from the head\'s\n' + \
            ';;; HC-LIGHT feature.  To make this work for us here, constraint\n' + \
            ';;; HC-LIGHT on verbs to be -.'
        #    mylang.add_literal(comment)
        mylang.add(mainorverbtype + ' := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')

    # intransitive verb lexical type
    typedef = \
        'intransitive-verb-lex := ' + mainorverbtype + ' & intransitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].'
    mylang.add(typedef)

    # transitive verb lexical type
    typedef = \
        'transitive-verb-lex := ' + mainorverbtype + ' & transitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
         ARG-ST < [ ], \
                  #comps & \
                  [ LOCAL [ CAT cat-sat & [ VAL [ SPR < >, \
                                      COMPS < > ] ] ] ] > ].'
    mylang.add(typedef)

    if ch.get(clausalcomps.COMPS):
        clausalcomps.add_clausalcomp_verb_supertype(ch, mainorverbtype, mylang)

    case.customize_verb_case(mylang, ch)

    # EKN 03-02-2018 Add [ CASE real-case ] to all args of verbs iff
    # the language has case and possessives:
    poss = True if ch.get('poss-strat') or ch.get('poss-pron') else False
    case_on = True if ch.get('case-marking') != 'none' else False
    if poss and case_on:
        real_case = '[ LOCAL.CAT.HEAD.CASE real-case ]'
        mylang.add('intransitive-verb-lex := [ ARG-ST < '+real_case+' > ].')
        mylang.add('transitive-verb-lex := [ ARG-ST < '+real_case+',\
                                                      '+real_case+' > ].')

    # Add constraints to choices to create lex rules for bipartite stems
    customize_bipartite_stems(ch)

    # Lexical entries
    lexicon.add_literal(';;; Verbs')

    # Now create the lexical entries for all the defined verb types
    cases = case.case_names(ch)
    for verb in ch.get('verb', []):
        create_verb_lex_type(cases, ch, hierarchies, lexicon, mylang, verb)
    if ch.get('wh-q-inter-verbs') == 'on':
        mylang.add(lexbase.ITRG_VB)
        for verb in ch.get('qverb', []):
            create_interrogative_verb_type(
                cases, ch, hierarchies, lexicon, mylang, verb)


def create_interrogative_verb_type(cases, ch, hierarchies, lexicon, mylang, verb):
    vtype = verb_id(verb)
    # clause-embedding verb's valence and its complement's head constraint:
    vtype, head = clausalcomps.update_verb_lextype(ch, verb, vtype)
    if verb['predtype'] in ['manner', 'loc']:
        mylang.add(vtype + lexbase.ITRG_FOUR_REL)
    elif verb['predtype'] == 'ref':
        mylang.add(vtype + lexbase.ITRG_THREE_REL)

    features.customize_feature_values(
        mylang, ch, hierarchies, verb, vtype, 'verb', None, cases)
    stems = verb.get('stem', [])
    stems.extend(verb.get('bistem', []))
    for stem in stems:
        add_itg_stem_to_lexicon(lexicon, stem, vtype, verb['predtype'])


def add_itg_stem_to_lexicon(lexicon, stem, stype, predtype):
    orthstr = orth_encode(stem.get('orth'))
    vpred = stem.get('verbpred')
    name = stem.get('name')
    typedef = \
        TDLencode(name) + ' := ' + stype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "_' + vpred + '_v_rel" ].'
    lexicon.add(typedef)
    npred = stem.get('nounpred')
    if predtype == 'ref':
        typedef = \
            TDLencode(
                name) + ' := [ SYNSEM.LOCAL.CONT.RELS.LIST < [], [], [ PRED "_' + npred + '_n_rel" ] > ] ].'
        lexicon.add(typedef, merge=True)
    elif predtype in ['manner', 'loc']:
        if predtype == 'manner':
            pred = 'manner_nonsp_rel'
        elif predtype == 'loc':
            pred = 'loc_nonsp_rel'
        typedef = \
            TDLencode(name) + ' := [ SYNSEM.LOCAL.CONT.RELS.LIST < [], [PRED "_' + pred + '"], ' \
            '[ PRED "_' + npred + '_n_rel" ], [] > ] ].'
        lexicon.add(typedef, merge=True)


def create_verb_lex_type(cases, ch, hierarchies, lexicon, mylang, verb):
    stypes = verb.get('supertypes').split(', ')
    stype_names = [verb_id(ch[st]) for st in stypes if st != '']
    vtype = verb_id(verb)
    construct_supertype_names(cases, ch, stype_names, verb)
    # clausal verb's valence and its complement's head constraint:
    vtype, head = clausalcomps.update_verb_lextype(ch, verb, vtype)
    if len(stype_names) == 0:
        mylang.add(vtype + ' := verb-lex .')
    else:
        mylang.add(vtype + ' := ' + ' & '.join(stype_names) + '.')
    if head:
        mylang.add(
            vtype + ' := [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD ' + head + ' ] > ].', merge=True)
    features.customize_feature_values(
        mylang, ch, hierarchies, verb, vtype, 'verb', None, cases)

    stems = verb.get('stem', [])
    stems.extend(verb.get('bistem', []))
    for stem in stems:
        add_stem_to_lexicon(lexicon, stem, vtype)


def add_stem_to_lexicon(lexicon, stem, stype):
    orthstr = orth_encode(stem.get('orth'))
    pred = stem.get('pred')
    name = stem.get('name')
    typedef = \
        TDLencode(name) + ' := ' + stype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
    lexicon.add(typedef)


def construct_supertype_names(cases, ch, stype_names, verb):
    val = verb.get('valence')
    if val:
        i = val.find(',')
        dir_inv = ''
        tivity = ''
        clausal = ''
        if i != -1:
            type = val.split(',')[1]
            if type == 'dirinv':
                dir_inv = 'dir-inv-'
            elif type.startswith('comps'):
                clausal = 'clausal-'
            val = val[:i]
        if val == 'trans' and not clausal:
            tivity = 'trans'
        elif val == 'intrans':
            tivity = 'intrans'
        elif val.find('-') != -1:
            c = val.split('-')
            a_case = case.canon_to_abbr(c[0], cases)
            o_case = case.canon_to_abbr(c[1], cases)
            tivity = a_case + '-' + o_case
            if not clausal:
                tivity = tivity + '-trans'
            else:
                tivity = tivity + '-'
        else:
            s_case = case.canon_to_abbr(val, cases)
            if not clausal:
                tivity += s_case
                tivity += '-intrans'
            else:
                # OZ 2018-01-03: This is awkward, has to do with the current format of the choices file...
                # We should not ideally end up here at all if we are not an intransitive verb...
                # The reason we end up here is some clause-embedding verbs will only have subject case specified.
                if not s_case == 'trans':
                    tivity += s_case
                    tivity += '-'
        if clausal:
            stype_names.append(clausal + tivity + 'verb-lex')
        elif (not dir_inv == '' or not tivity == ''):
            stype_names.append(dir_inv + tivity + 'itive-verb-lex')


# Returns the verb type for lexical/main verbs.
def main_or_verb(ch):
    if ch.get('has-aux') == 'yes':
        return 'main-verb-lex'
    else:
        return 'verb-lex'


def customize_determiners(mylang, ch, lexicon, hierarchies):
    from gmcs.constants import INTER, ON
    # Lexical type for determiners, if the language has any:
    if ch.get('has-dets') == 'yes':
        comment = \
            ';;; Determiners\n' + \
            ';;; SPEC is non-empty, and already specified by basic-determiner-lex.'
        mylang.add_literal(comment)

        # LLD 2016-04-04 changed basic-zero-arg to norm-zero-arg
        typedef = \
            'determiner-lex := basic-determiner-lex & basic-non-wh-word-lex & norm-zero-arg & \
                [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >, \
                                         COMPS < >, \
                                         SUBJ < > ]]].'
        mylang.add(typedef)

        mylang.add('determiner-lex := non-mod-lex-item.')

    # Determiners
    if 'det' in ch:
        lexicon.add_literal(';;; Determiners')

    for det in ch.get('det', []):
        if det[INTER] == ON:
            USED_TYPES['qdet'] = True
            mylang.add(lexbase.WH_DET)
            if ch.get('q-part-order') == 'second':
                mylang.add('''wh-determiner-lex := non-ynq-word.''')
            add_determiner(ch, det, 'wh-determiner-lex',
                           hierarchies, lexicon, mylang)
        else:
            add_determiner(ch, det, 'determiner-lex',
                           hierarchies, lexicon, mylang)


def add_determiner(ch, det, stype, hierarchies, lexicon, mylang):
    dtype = det_id(det)
    mylang.add(dtype + ' := ' + stype + '.')
    if stype == 'determiner-lex':
        mylang.add(dtype + ':= [ SYNSEM [ L-QUE -, ] ].')
    has_inforstr_feat = False
    for feat in det.get('feat', []):
        if feat['name'] == "information-structure meaning":
            has_inforstr_feat = True
            mylang.add(dtype + ' := infostr-marking-determiner-lex.')
            break
    if not has_inforstr_feat:
        if stype == 'determiner-lex':
            mylang.add(dtype + ' := no-icons-lex-item.')
        elif stype == 'wh-determiner-lex':
            mylang.add(dtype + ' := one-icons-lex-item.')

    features.customize_feature_values(
        mylang, ch, hierarchies, det, dtype, 'det')

    for stem in det.get('stem', []):
        orthstr = orth_encode(stem.get('orth'))
        pred = stem.get('pred')
        name = stem.get('name')
        typedef = \
            TDLencode(name) + ' := ' + dtype + ' & \
                    [ STEM < "' + orthstr + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
        lexicon.add(typedef)


def customize_misc_lex(ch, lexicon, trigger):

    # Question particle
    if ch.get('q-part') == 'on':
        for qpart in ch.get('q-particle'):
            parent = qpart_id(qpart)
            orth = qpart['orth']
            orthstr = orth_encode(orth)
            typedef = \
                TDLencode(orth) + ' := ' + parent + ' & \
                       [ STEM < "' + orthstr + '" > ].'
            lexicon.add(typedef)
            grdef = TDLencode(orth) + '_gr := generator_rule & \
                       [ CONTEXT [ RELS.LIST < [ ARG0.SF ques ] > ], \
                         FLAGS.TRIGGER "' + TDLencode(orth) + '" ].'
            trigger.add(grdef)


def customize_nouns(mylang, ch, lexicon, hierarchies):
    from gmcs.constants import INTER, ON, WH_PRO

    # EKN 2018-01-26 Adding a PRON feature to mark all pronouns:
    mylang.add('head :+ [ PRON bool ].', section='addenda')

    # Figure out which kinds of determiner-marking are in the language
    seen = {'obl': False, 'opt': False, 'imp': False}
    seenCount = 0

    for noun in ch.get('noun', []):
        det = noun.get('det')
        if not det == '' and not seen[det]:
            seen[det] = True
            seenCount += 1
        if noun.get(INTER):
            USED_TYPES['qpro'] = True
            mylang.add(lexbase.WH_PRONOUN)
    singlentype = (seenCount == 1)

    # Playing fast and loose with the meaning of OPT on SPR.  Using
    # OPT - to mean obligatory (as usual), OPT + to mean impossible (that's
    # weird), and leaving OPT unspecified for truly optional.  Hoping
    # this will work at least for LSA111 lab.

    # ERB 2006-11-28 Update: To make that weird use of OPT work, the
    # head-spec rule has to require [OPT -] on its non-head daughter.
    # Adding that just in case we add the no-spr-noun-lex type.

    typedef = \
        'noun-lex := basic-noun-lex & basic-non-wh-word-lex & non-local-none-lex-item & no-hcons-lex-item & \
           [ SYNSEM [ LOCAL [ CAT [ VAL [ SPR < #spr & [ LOCAL.CAT.HEAD det ] >, \
                                      COMPS < >, \
                                      SUBJ < >, \
                                      SPEC < > ] ] ] ], \
             ARG-ST < #spr > ].'

    # EKN 2017-12-18 In some languages, possessor nouns or
    # possessor-marking adpositions act as specifiers to nouns.
    # This checks to see which kinds of possessive strategies
    # have been defined, and makes the head type of the noun-lex's
    # SPR conditional on that. It also checks for cases where only
    # the possessum is marked, since in these cases, the possessor
    # noun must have a non-empty SPEC list even though it has gone
    # through no lexical rules.

    mylang.add(typedef)

    # Adding empty MOD on general definitiion for noun-lex
    mylang.add('noun-lex := non-mod-lex-item.')

    # singlentype means there's only one type of n in the hierarchy.
    if singlentype:
        if seen['obl']:
            typedef = 'noun-lex := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
            mylang.add(typedef)
        elif seen['imp']:
            typedef = 'noun-lex := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
            mylang.add(typedef)
    else:
        if seen['obl']:
            typedef = \
                'obl-spr-noun-lex := noun-lex & \
                   [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
            mylang.add(typedef)

        if seen['imp']:
            typedef = \
                'no-spr-noun-lex := noun-lex & \
                   [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
            mylang.add(typedef)
    # EKN 2018-02-02 Possessor pronouns are a type which cannot
    # take dets, but won't trigger 'imp' below. Adding a check
    # for them specifically:
    poss_prons = False
    for pron in ch.get('poss-pron', []):
        if pron.get('type') != 'affix':
            poss_prons = True
    if (seen['imp'] or poss_prons) and ch.get('has-dets') == 'yes':
        mylang.add(
            'head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.OPT - ].',
            'Nouns which cannot take specifiers mark their SPR requirement\n' +
            'as OPT +.  Making the non-head daughter OPT - in this rule\n' +
            'keeps such nouns out.')

    if ch.get('case-marking') != 'none':
        if not ch.has_adp_case() and not ch.has_det_case():
            mylang.add('noun :+ [ CASE case ].', section='addenda')

    # Add the lexical entries
    lexicon.add_literal(';;; Nouns')

    # make a hash of nountypes --> lists of children so that we
    # can stopdet on children
    children = defaultdict(dict)
    for noun in ch.get('noun', []):
        for p in noun.get('supertypes').split(', '):
            children[p][noun.full_key] = 1

    # make and populate a dictionary of stopdets, to avoid vacuous det supertypes
    # have to follow inheritance paths downwards from any nonempty det values
    stopdets = {}
    for noun in ch.get('noun', []):
        # if det is nonempty, child nouns shouldn't inherit det
        det = noun.get('det')
        if det != '':
            if noun.full_key in children:
                # there are children to stopdet on
                # recursively look for children
                parents = [noun.full_key]
                while (True):
                    next_parents = []
                    for p in parents:
                        if p in children:
                            for c in list(children[p].keys()):
                                stopdets[c] = True
                                if not c in next_parents:
                                    next_parents.append(c)
                    if len(next_parents) == 0:
                        break
                    else:
                        parents = next_parents

    for noun in ch.get('noun', []):
        ntype = noun_id(noun)
        det = noun.get('det')
        pron = noun.get('pron') == ON
        qpron = noun.get(INTER) == ON
        if noun.full_key in stopdets:
            det = ''

        stypes = noun.get('supertypes').split(', ')
        stype_names = [noun_id(ch[st]) for st in stypes if st != '']

        # if singlentype or det == 'opt':
        #  stype = 'noun-lex'
        if not singlentype and not qpron:
            if det == 'obl':
                stype_names.append('obl-spr-noun-lex')
            elif det == 'imp':
                stype_names.append('no-spr-noun-lex')
        if qpron:
            stype_names.append(WH_PRO)
        if len(stype_names) == 0:
            mylang.add(ntype + ' := noun-lex .')
        else:
            mylang.add(ntype + ' := ' + ' & '.join(stype_names) + '.')
        # EKN 2018-01-31 Adding PRON feature to individual types:
        if pron:
            mylang.add(ntype + ' := [ SYNSEM.LOCAL.CAT.HEAD.PRON + ].')
        features.customize_feature_values(
            mylang, ch, hierarchies, noun, ntype, 'noun')
        for stem in noun.get('stem', []):
            # consider instead using (should be the same effect):
            # add_stem_to_lexicon(lexicon, stem, ntype)
            orthstr = orth_encode(stem.get('orth'))
            pred = stem.get('pred')
            name = stem.get('name')
            typedef = TDLencode(name) + ' := ' + ntype + ' & \
                  [ STEM < "' + orthstr + '" >, \
                    SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
            lexicon.add(typedef)


def customize_adverbs(mylang, ch, lexicon):
    mylang.set_section('otherlex')
    mylang.add_literal(';;; Adverbs')
    lexicon.add_literal(';;; Adverbs')
    loc_adv_added = False
    manner_adv_added = False
    if ch.get('adv'):
        mylang.add(lexbase.ADV_ITEM)
    for adv in ch.get('adv'):
        stypes = []
        if adv['type'] == 'loc':
            if not loc_adv_added:
                mylang.add(lexbase.LOC_ADV_ITEM)
                loc_adv_added = True
            stypes.append('loc-adverb-lex-item')
        elif adv['type'] == 'manner':
            if not manner_adv_added:
                mylang.add(lexbase.MANNER_ADV_ITEM)
                manner_adv_added = True
            stypes.append('manner-adverb-lex-item')
        if adv['inter'] == 'on':
            stypes.append('wh-adverb-lex')
            mylang.add(lexbase.WH_ADV)
        else:
            stypes.append('adverb-lex')
            mylang.add(lexbase.ADV)
        supertypes = ' & '.join(stypes)
        typename = adv['name'] + '-' + 'adverb-lex'
        typedef = TDLencode(typename) + ' := ' + supertypes + '.'
        mylang.add(typedef)
        for stem in adv['stem']:
            add_stem_to_lexicon(lexicon, stem, typename)

# TJT 2014-05-05


def customize_adjs(mylang, ch, lexicon, hierarchies, rules):

    # Add basic adjective definition
    # OZ 2020-02-18 In fact, all adjectives must also inherit from either zero-norm-arg
    # or from non-local-none-lex-item. Otherwise the nonlocal values are underspecified.
    if ch.get('adj', []):
        mylang.add("adj-lex := basic-intersective-adjective-lex.")

    # Check which rules need to be added to rules.tdl
    adj_rules = {'adj_head': False, 'head_adj': False}
    # Check which types need to be added to mylanguage.tdl
    adj_type_base = ('pred_word', 'pred_lex', 'pred_only',
                     'attr_word', 'attr_lex', 'attr_only',
                     'stative_word', 'stative_lex', 'any_adj')
    # Convert into dictionary with False default values
    #adj_types = {item: False for item in adj_types}
    adj_types = dict()
    for item in adj_type_base:
        adj_types[item] = False

    # Lexical super types of different adjective types
    lst_map = {"both": "attr-adj-lex",
               "attr": "attr-only-adj-lex",
               "pred": "pred-only-adj-lex",
               "none": "adj-lex"}

    # Go through adjective position classes...
    for adj_pc in ch.get('adj-pc', []):
        # check if any have "any adj" as input
        if not adj_types['any_adj'] and 'adj' in adj_pc.get('inputs', []).split(', '):
            adj_types['any_adj'] = True
        # Additional checks for switching adjectives
        switching = adj_pc.get('switching', False)
        if switching:
            # For each switching adjective...
            for lrt in adj_pc.get('lrt', []):
                # Check its mode to get lexical types to add
                if not (adj_types['pred_lex'] and adj_types['attr_lex']):
                    adj_pc_mod = lrt.get('mod', '')
                    if adj_pc_mod:
                        # TJT 12-05-14: "both" lexical rule types are predicative, too!
                        if adj_pc_mod in ('pred', 'both'):
                            adj_types['pred_lex'] = True
                        # TJT 11-06-14: "both" lexical rule types are attributive
                        elif adj_pc_mod in ('attr', 'both'):
                            adj_types['attr_lex'] = True
                # Check modification direction to get rules to add
                if not (adj_rules['head_adj'] and adj_rules['adj_head']):
                    lrt_modpos = lrt.get('modpos', False)
                    if lrt_modpos:
                        if lrt_modpos == 'before':
                            adj_rules['head_adj'] = True
                        elif lrt_modpos == 'after':
                            adj_rules['adj_head'] = True
                        elif lrt_modpos == 'either':
                            adj_rules['head_adj'] = True
                            adj_rules['adj_head'] = True
                # Check predicative behavoir to get rules to add
                if not adj_types['stative_lex']:
                    # If not a copula complement
                    if not lrt.get('predcop', False):
                        adj_types['stative_lex'] = True

    # Add the lextypes to mylanguage.tdl
    for adj in ch.get('adj', []):
        # Reset values
        lst, posthead, subj, pred, adj_constraints, modunique = '', '', '', '', '', ''
        root = False

        # Get all supertypes to check for redundant specifications
        all_supertypes, pathToRoot = get_all_supertypes(adj, ch)
        if not pathToRoot:
            raise ValueError("No path to the root was found for type %s." % adj.get('name', '') +
                             "Please try validating your choices and compiling again.")
        # Keep track of redundancies
        supertype_redundancies = defaultdict(lambda: False)

        # Check pivots
        # Pivot on type of adjective
        mode = adj.get('mod', False)
        # TJT 2014-09-04: Commenting this out because one should be able
        # to define subtypes for purely morphological use
        # if not mode: continue
        # Pivot on modification direction
        modpos = adj.get('modpos', False)
        # Pivot on copula complementation
        predcop = adj.get('predcop', False)
        # Pivot on unique modification
        modunique = adj.get('modunique', False)

        # Optionally copula complement and adjectives that only agree
        # in only position must be unspecified at the lexical level
        if predcop == "opt":
            mode = "none"

        # Check for redundancies and collisions
        def_error = " ".join('''Collision found in supertype at %s!
                            Validate your choices file and try again.
                            Supertype: %s; Supertype Choice: %s
                            Type: %s; Type choice: %s'''.split()).strip()
        for supertype in all_supertypes:
            supertype_choice = ch.get(supertype, False)
            if supertype_choice:
                # Check mode
                if mode:
                    supertype_mode = supertype_choice.get('mode', False)
                    if supertype_mode:
                        if supertype_mode == "both" and mode in ('attr', 'pred'):
                            pass  # attr and pred unify with both
                        elif mode != supertype_mode:
                            raise ValueError(def_error % (
                                supertype, supertype_mode, adj.get('name', ''), mode))
                # Check modpos
                if modpos:
                    supertype_modpos = supertype_choice.get('modpos', False)
                    if supertype_modpos:
                        if modpos == supertype_modpos:
                            supertype_redundancies['modpos'] = True
                        elif supertype_modpos == 'either' and modpos in ('before', 'after'):
                            pass  # before and after unify with either
                        else:
                            raise ValueError(def_error % (
                                supertype, supertype_modpos, adj.get('name', ''), modpos))
                # Check modunique
                if modunique and supertype_choice.get('modunique', False):
                    supertype_redundancies['modunique'] = True
                # Check predcop
                if predcop:
                    supertype_predcop = supertype_choice.get('predcop', False)
                    if supertype_predcop:
                        if predcop == supertype_predcop:
                            supertype_redundancies['predcop'] = True
                        elif supertype_predcop == 'opt' and predcop in ('opt', 'obl'):
                            pass
                        else:
                            raise ValueError(def_error % (
                                supertype, supertype_predcop, adj.get('name', ''), predcop))

        # Calculate supertypes
        stypes = adj.get('supertypes').split(', ')
        stype_names = []
        if '' in stypes:  # Found root
            root = True
        # Set up root supertypes
        if root:
            stype_names = [lst_map[mode]]
        # Set up defined supertypes
        else:
            stype_names = [adj_id(ch[st]) for st in stypes if st]
            # Add pred-only or attr-only types
            if mode == 'pred' and predcop != 'opt':
                stype_names.append("pred-only-adj-lex")
            elif mode == 'attr':
                stype_names.append("attr-only-adj-lex")
        # Format supertypes
        stype_def = " & ".join(stype_names) or ""
        if stype_def:
            stype_def += " & "
        # Add pred-only and attr-only types if applicable
        if mode in ('attr', 'pred'):
            # Add proper type to mylanguage.tdl
            adj_types["%s_only" % mode] = True

        # For attributive adjectives...
        if mode in ("both", "attr"):
            # Set up proper rule for mylanguage.tdl
            adj_types['attr_word'] = True
            # Pivot on direction of modification
            if not supertype_redundancies['modpos']:
                if modpos == 'after':
                    posthead = 'POSTHEAD - '
                    adj_rules['adj_head'] = True
                elif modpos == 'before':
                    posthead = 'POSTHEAD + '
                    adj_rules['head_adj'] = True
                elif modpos == "either":
                    adj_rules['head_adj'] = True
                    adj_rules['adj_head'] = True
            # Set up unique modification if necessary
            if modunique:
                if not supertype_redundancies['modunique']:
                    modunique = 'MOD < [ MODIFIED notmod ] >'
                else:
                    modunique = ''

        if not supertype_redundancies['predcop']:
            # For predicative adjectives...
            if mode in ('both', 'pred'):
                # Set up proper rule for mylanguage.tdl
                # Pivot on copula complement
                if predcop == 'obl':
                    # Adjective only appears as copula complement
                    pred = '+'
                    subj = 'VAL.SUBJ < >'
                    if mode == 'pred':
                        adj_types['pred_word'] = True
                elif predcop == 'opt':
                    # Switching between copula complement and inflection
                    # See deffile.py for zero affixes added here
                    adj_types['stative_lex'] = True
                    adj_types['pred_lex'] = True
                elif predcop == 'imp':
                    # Adjective only appears as stative predicate
                    pred = '-'
                    adj_types['stative_word'] = True
                    adj_types['pred_word'] = True
                    # Add additional supertype
                    if root:
                        stype_def += 'stative-pred-adj-lex & '

        # Calculate HEAD value
        head = ''
        if pred and modunique:
            head = "HEAD [ PRD %s, %s ]" % (pred, modunique)
        elif pred:
            head = "HEAD.PRD %s" % pred
        elif modunique:
            head = "HEAD." + modunique

        # Only output constraints if defined
        if posthead or pred or subj or modunique:
            adj_constraints = "\n  [ SYNSEM.LOCAL.CAT [" + \
                "\n".join([posthead, subj, head]).strip(",\n") + '] ]'

        # Add lexical types to mylanguage.tdl
        atype = adj_id(adj)
        mylang.add(atype + ' := ' + lst + stype_def + adj_constraints + '.')

    # Add the proper lexical types to mylanguage.tdl
    # Add attributive adjective types
    if adj_types['attr_word']:
        mylang.add('''attr-adj-lex := adj-lex & intersective-mod-lex &
                    [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT [ HEAD noun,
                                                                VAL.SPR cons ] ] > ].''',
                   comment='Basic attributive adjective definition')
    if adj_types['attr_lex']:
        mylang.add('''attr-adj-lex-rule := add-only-no-ccont-rule &
                    [ SYNSEM [ LOCAL [ CAT.HEAD.MOD < [ LOCAL intersective-mod &
                                                        [ CONT.HOOK.INDEX #xarg,
                                                          CAT [ HEAD noun,
                                                                VAL.SPR cons ] ] ] >,
                                       CONT.HOOK.XARG #xarg ] ] ].''',
                   comment='Basic attributive adjective lexical rule definition',
                   section='lexrules')

    # Add attributive-only adjective types
    if adj_types['attr_only']:
        attr_only_map = {'attr_word':
                         {'type_name': 'attr-only-adj-lex := attr-adj-lex & ',
                          'section': ''},
                         'attr_lex':
                             {'type_name': 'attr-only-adj-lex-rule := attr-adj-lex-rule & ',
                              'section': 'lexrules'}}
        for sort in ('attr_word', 'attr_lex'):
            if adj_types[sort]:
                mylang.add('''%s [ SYNSEM.LOCAL.CAT [ HEAD.PRD -,
                                              VAL.SUBJ < > ] ].''' %
                           attr_only_map[sort]['type_name'],
                           section=attr_only_map[sort]['section'])

    # Add predicative-only adjective types
    if adj_types['pred_only']:
        if adj_types['pred_word']:
            mylang.add('''pred-only-adj-lex := adj-lex & no-mod-lex.''')
        if adj_types['pred_lex']:
            mylang.add(
                '''pred-only-adj-lex-rule := add-only-no-ccont-rule & no-mod-lex.''')

    # Add additional types
    # If there are stative predicates, add the proper rule and supertype
    pred_adj_map = {'stative_word':
                    {'supertype': 'stative-pred-adj-lex := adj-lex &',
                     'comment': 'Stative predicate adjective definition',
                     'section': ''},
                    'stative_lex':
                        {'supertype': 'stative-pred-lex-rule := add-only-no-ccont-rule & ',
                         'comment': 'Stative predicate adjective lexical rule definition',
                         'section': 'lexrules'}}
    # EKN 03-02-2018 Add [ CASE real-case ] to SUBJ of adj iff
    # the language has case and possessives:
    poss = True if ch.get('poss-strat') or ch.get('poss-pron') else False
    case_on = True if ch.get('case-marking') != 'none' else False
    if poss and case_on:
        pred_adj_definition = '''%s
          [ SYNSEM.LOCAL [ CAT.VAL.SUBJ < [ LOCAL [ CAT.HEAD.CASE real-case,
                                                    CONT.HOOK.INDEX #xarg,
  	   	   		    	                       CAT [ VAL [ SPR < >,
                                                                           COMPS < > ],
                                                                     HEAD noun ] ] ] >,
                          CONT.HOOK.XARG #xarg ] ].'''
    else:
        pred_adj_definition = '''%s
    [ SYNSEM.LOCAL [ CAT.VAL.SUBJ < [ LOCAL [ CONT.HOOK.INDEX #xarg,
  		   		    	                       CAT [ VAL [ SPR < >,
                                                           COMPS < > ],
                                                    HEAD noun ] ] ] >,
                     CONT.HOOK.XARG #xarg ] ].'''

    for form in ("stative_word", "stative_lex"):
        if adj_types[form]:
            mylang.add(pred_adj_definition % pred_adj_map[form]['supertype'],
                       comment=pred_adj_map[form]['comment'],
                       section=pred_adj_map[form]['section'])

    # If adjective incorporation, add to mylanguage.tdl
    if ch.get("adj_incorp", False):
        mylang.add('''adj_incorporation-lex-rule := add-only-rule &
                    [ C-CONT [ RELS.LIST < arg1-ev-relation &
                                       [ LBL #ltop,
		                                 ARG1 #index ] >,
	                           HOOK #hook ],
                      DTR.SYNSEM.LOCAL [ CAT.HEAD noun,
  		                                 CONT.HOOK #hook &
    			                                   [ LTOP #ltop,
			                                         INDEX #index ] ] ].''',
                   comment='Adjective Incorporation',
                   section='lexrules')

    # Add the proper syntactic rules to rules.tdl
    from gmcs.linglib.adverbs_adpositions import HEAD_ADJ, ADJ_HEAD
    if adj_rules['head_adj']:
        mylang.add(HEAD_ADJ, section='phrases')
        rules.add("head-adj := my-head-adj-phrase.")

    if adj_rules['adj_head']:
        mylang.add(ADJ_HEAD, section='phrases')
        rules.add("adj-head := my-adj-head-phrase.")

    # Add the lexical entries to lexicon.tdl
    lexicon.add_literal(';;; Adjectives')

    for adj in ch.get('adj', []):
        atype = adj_id(adj)

        # Automatically generate feature values based on choices
        features.customize_feature_values(
            mylang, ch, hierarchies, adj, atype, 'adj')

        for stem in adj.get('stem', []):
            typedef = TDLencode(stem.get('name')) + ' := ' + atype + ' & \n \
                [ STEM < "' + orth_encode(stem.get('orth')) + '" >, \
                SYNSEM.LKEYS.KEYREL.PRED "' + stem.get('pred') + '" ].'
            lexicon.add(typedef)

# TJT 2014-05-05


def customize_cops(mylang, ch, lexicon, hierarchies, trigger):

    if ch.get('cop', False):
        # Add PRD
        mylang.add('head :+ [ PRD bool ].', section='addenda')

        # Add copulas
        lexicon.add_literal(';;; Copulas')

        # Core definition
        mylang.add('''%s := basic-verb-lex-super & trans-first-arg-raising-lex-item-2 & non-mod-lex-item &
          [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < [ LOCAL [ CONT.HOOK.INDEX #xarg,
                                                      CAT cat-sat & [ VAL [ SPR < >,
                                                                  COMPS < > ],
                                                            HEAD noun ] ] ] >,
                                     COMPS < [ LOCAL.CAT cat-sat & [ HEAD.PRD +,
                                                           VAL [ SUBJ < >,
                                                                 COMPS < > ] ] ] >,
                                     SPEC < > ],
                           CONT.HOOK.XARG #xarg ] ].''' % LEXICAL_SUPERTYPES['cop'])

        # EKN 03-02-2018 Add [ CASE real-case ] to all subj of copula iff
        # the language has case and possessives:
        poss = True if ch.get('poss-strat') or ch.get('poss-pron') else False
        case_on = True if ch.get('case-marking') != 'none' else False
        if poss and case_on:
            mylang.add(
                '''%s := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE real-case ]  > ].''' % LEXICAL_SUPERTYPES['cop'])

        # only works for adj right now, change in future
        comment = '''Copula type taking adjectival complements.\nNeed to define more for additional complement types.'''
        mylang.add('''adj-comp-copula-verb-lex := %s &
                  [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD adj ].''' % LEXICAL_SUPERTYPES['cop'],
                   comment=comment)

        for cop in ch.get('cop', []):
            ctype = cop_id(cop)

            # Calculate supertypes
            stypes = cop.get('supertypes').split(', ')
            stype_def = ''
            if '' in stypes:  # Found root
                stype_def = 'adj-comp-copula-verb-lex & '  # Change for new complement types
            else:
                stype_names = [cop_id(ch[st])
                               for st in [_f for _f in stypes if _f]]
                stype_def = " & ".join(stype_names) or ""
                if stype_def:
                    stype_def += " & "

            features.customize_feature_values(
                mylang, ch, hierarchies, cop, ctype, 'cop')

            # Add the lexical types
            mylang.add(ctype + ' := ' + stype_def + '.')

            for stem in cop.get('stem'):
                orthstr = orth_encode(stem.get('orth'))
                name = stem.get('name')
                typedef = TDLencode(name) + ' := ' + ctype + ' & \
                        [ STEM < "' + orthstr + '" > ].'
                lexicon.add(typedef)

                # TODO: Add copula types to trigger.mtr


def customize_adpositions(mylang, lexicon, ch, hierarchies):
    mylang.add(lexbase.ADP_LEX)
    if ch.get('has-aux') == 'yes':
        mylang.add(
            'norm-adposition-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.HEAD.AUX - ] > ].')
    supertype = 'norm-adposition-lex'
    for adp in ch.get('normadp'):
        typename = adp.full_key + '-' + supertype
        mylang.add(typename + ' := ' + supertype + '.')
        for stem in adp['stem']:
            add_stem_to_lexicon(lexicon, stem, typename)
        features.customize_feature_values(
            mylang, ch, hierarchies, adp, typename, 'normadp')
        if not feature_type_use.USED_FEATURES['INIT']:
            feature_type_use.USED_FEATURES['INIT'] = True
            mylang.add('head :+ [ INIT bool ].', section='addenda')
        if adp['order'] == 'before':
            mylang.add(typename + ' := ' + supertype +
                       '& [ SYNSEM.LOCAL.CAT.HEAD.INIT + ].')
        else:
            mylang.add(typename + ' := ' + supertype +
                       '& [ SYNSEM.LOCAL.CAT.HEAD.INIT - ].')


######################################################################
# customize_lexicon()
#   Create the type definitions associated with the user's test
#   lexicon.

def customize_lexicon(mylang, ch, lexicon, trigger, hierarchies, rules):

    comment = '''Type assigning empty mod list. Added to basic types for nouns, verbs and determiners.'''
    mylang.add('non-mod-lex-item := lex-item & \
               [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].', comment)

    mylang.set_section('nounlex')
    customize_nouns(mylang, ch, lexicon, hierarchies)

    mylang.set_section('adjlex')
    customize_adjs(mylang, ch, lexicon, hierarchies, rules)

    mylang.set_section('otherlex')
    # Need to pick up other POS which inflect for case.
    case_pos = set()
    to_cfv = case.customize_case_adpositions(
        mylang, lexicon, trigger, ch, case_pos)

    features.process_cfv_list(mylang, ch, hierarchies, to_cfv, tdlfile=lexicon)

    # Specify CASE on determiners if needed:
    if ch.has_det_case():
        case_pos.add('det')
    if len(case_pos) == 2:
        mylang.add('+npd :+ [ CASE case ].', section='addenda')
    elif len(case_pos) == 1 and 'det' in case_pos:
        mylang.add('+nd :+ [ CASE case ].', section='addenda')
    elif len(case_pos) == 1 and 'adp' in case_pos:
        mylang.add('+np :+ [ CASE case ].', section='addenda')

    if ch.has_adp_only_infostr():
        to_cfv = information_structure.customize_infostr_adpositions(
            mylang, lexicon, trigger, ch)
        features.process_cfv_list(
            mylang, ch, hierarchies, to_cfv, tdlfile=lexicon)

    if ch.get('normadp'):
        customize_adpositions(mylang, lexicon, ch, hierarchies)

    mylang.set_section('verblex')
    customize_verbs(mylang, ch, lexicon, hierarchies)

    if ch.get('has-aux') == 'yes':
        mylang.set_section('auxlex')
        auxiliaries.customize_auxiliaries(
            mylang, ch, lexicon, trigger, hierarchies)

    mylang.set_section('coplex')
    customize_cops(mylang, ch, lexicon, hierarchies, trigger)

    mylang.set_section('otherlex')
    customize_determiners(mylang, ch, lexicon, hierarchies)
    customize_adverbs(mylang, ch, lexicon)
    customize_misc_lex(ch, lexicon, trigger)

# Used by the word order library, for different matrix-subordinate word order
# E.g. German: verbs in subordinate clauses cluster at the end of the clause.


def update_lex_items_vcluster(ch, mylang):
    mylang.add('cat :+ [ VC bool ].', merge=True, section='addenda')
    mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.VC - ].',
               merge=True, section='nounlex')
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.VC + ].',
               merge=True, section='verblex')
    if 'cms' in ch:
        for cms in ch.get('cms'):
            if cms.get('subordinator-type') == 'adverb':
                mylang.add(
                    'adverb-subord-lex-item := [ SYNSEM.LOCAL.CAT.VC - ].', merge=True, section='subordlex')
            if cms.get('subordinator-type') == 'head':
                mylang.add(
                    'adposition-subord-lex-item := [ SYNSEM.LOCAL.CAT.VC - ].', merge=True, section='subordlex')
    if 'comps' in ch:
        mylang.add(
            'complementizer-lex-item := [ SYNSEM.LOCAL.CAT.VC - ].', merge=True, section='complex')
    if 'has-adj' in ch:
        mylang.add('adj-lex := [ SYNSEM.LOCAL.CAT.VC - ].',
                   merge=True, section='adjlex')
