from collections import defaultdict

from gmcs.linglib import case
#from gmcs.linglib import lexical_items
from gmcs.utils import get_name
from gmcs.choices import ChoiceDict
from gmcs.linglib.lexbase import LexicalType, PositionClass
from gmcs.linglib.lexbase import ALL_LEX_TYPES
from gmcs.linglib.lexbase import LEXICAL_CATEGORIES
from gmcs.linglib.lexbase import LEXICAL_SUPERTYPES

def lexical_type_hierarchy(choices, lexical_supertype):
    if lexical_supertype not in LEXICAL_CATEGORIES:
        return None
    lth = PositionClass(key=lexical_supertype + '-pc',
                        name=get_lt_name(lexical_supertype, choices),
                        identifier_suffix='lex-super', lex_rule=False)
    lth.add_node(LexicalType(lexical_supertype,
                             get_lt_name(lexical_supertype, choices)))
    lts_to_add = [lexical_supertype]
    if lexical_supertype == 'verb':
        if choices['has-aux'] == 'yes':
            lth.add_node(LexicalType('aux', get_lt_name('aux', choices),
                                     parents={'verb':lth.nodes['verb']}))
            lth.add_node(LexicalType('mverb', get_lt_name('mverb', choices),
                                     parents={'verb':lth.nodes['verb']}))
            lts_to_add += ['aux']
        st = get_lexical_supertype('iverb', choices)
        lth.add_node(LexicalType('iverb', get_lt_name('iverb', choices),
                                 parents={st:lth.nodes[st]}))
        st = get_lexical_supertype('tverb', choices)
        lth.add_node(LexicalType('tverb', get_lt_name('tverb', choices),
                                 parents={st:lth.nodes[st]}))
    for lst in lts_to_add:
        for lt in choices[lst]:
            st = get_lexical_supertype(lt.full_key, choices)
            lth.add_node(LexicalType(lt.full_key, get_lt_name(lt.full_key, choices),
                                     parents={st:lth.nodes[st]}))
            # If we're dealing with a verb add nodes for all lexical entries
            # because bistems can give rise to flags that need to appear on
            # all verbs.
            if lexical_supertype == 'verb':
                bistems = choices[lt.full_key]['bistem'] or []
                stems = choices[lt.full_key]['stem'] or []
                stems.extend(bistems)
                for stem in stems:
                    lth.add_node(LexicalType(stem.full_key, stem['name'],
                                             parents={lt.full_key:lth.nodes[lt.full_key]},
                                             entry=True))
    return lth

def get_lexical_supertype(lt_key, choices):
    lexical_category = lt_key.rstrip('0123456789')
    if lexical_category in ('iverb','tverb') and choices['has-aux'] == 'yes':
        return 'mverb'
    elif lexical_category in ('aux','mverb','iverb','tverb'):
        return 'verb'
    elif lexical_category == 'verb':
        return case.interpret_verb_valence(choices[lt_key]['valence'])
    elif lexical_category in ('noun', 'det', 'adj', 'cop'): # TJT Added adj, cop, removed aux
        return lexical_category
    return None


def expand_lexical_supertype(st_key, choices):
    """
    Given a generic lexical supertype, like those defined in
    lexical_supertypes, return a list of all defined lexical types that
    fit in that pattern. For example, 'tverb' may return verbs with
    valence marked as nom-acc.
    """
    i_t = ['iverb','tverb']
    m = ['mverb']
    if st_key not in LEXICAL_SUPERTYPES: return []
    if (st_key == 'mverb' and choices['has-aux'] == 'yes') or \
            (st_key == 'verb' and choices['has-aux'] == 'no'):
        return [v.full_key for v in choices['verb']] + i_t
    elif st_key == 'verb' and choices['has-aux'] == 'yes':
        return [v.full_key for v in choices['verb'] + choices['aux']] + i_t + m
    elif st_key in ('iverb','tverb'):
        return [v.full_key for v in choices['verb']
                if case.interpret_verb_valence(v['valence']) == st_key]
    else:
        return [x.full_key for x in choices.get(st_key,[])]

def get_lexical_supertype_expansions(choices):
    """
    Return a dictionary mapping each supertype to its possible
    expansions as lexical types defined in the choices file.
    """
    return dict([(st, expand_lexical_supertype(st, choices))
                 for st in LEXICAL_SUPERTYPES])

def used_lexical_supertypes(choices):
    """
    Return a list of lexical supertypes (as defined in
    lexical_supertypes) that will actually be used in the grammar.
    """
    # TJT 2014-09-08: Changing this to set comprehension and adding "cop"
    # TJT 2014-12-19: Changing back to loop for older versions of python
    used = set([item for item in ('noun','aux','adj','det','cop') if item in choices])
    if 'verb' in choices:
        used.add('verb')
        used.update([case.interpret_verb_valence(v['valence'])
                     for v in choices['verb']])
        if choices['has-aux'] == 'yes':
            used.add('mverb')
    return used

def get_lexical_supertypes(lrt_key, choices):
    """
    Return the list of appropriate lexical types for the given rule
    type. There may be more than one for verbs ([tverb, mverb, verb]) or
    auxiliaries ([aux, verb]).
    """
    lexical_category = lrt_key.rstrip('0123456789')
    # first check if we are already dealing with a generic type
    if lexical_category == lrt_key:
        if lrt_key in ('iverb','tverb'):
            if choices['has-aux'] == 'yes': return ['mverb','verb']
            else: return ['verb']
        elif lrt_key =='aux': return ['verb']
        else: return []
    # otherwise we have a lexical type (e.g. noun1, verb2, etc)
    elif lexical_category in ('noun', 'det', 'adj', 'cop'): # TJT 2014-09-03: Adding cop, moving up
        return [lexical_category]
    elif lexical_category == 'verb':
        verb_type = case.interpret_verb_valence(choices[lrt_key]['valence'])
        return [verb_type] + get_lexical_supertypes(verb_type, choices)
    elif lexical_category == 'aux':
        return ['verb']
    return []

# TJT 2014-09-03
def get_all_supertypes(key, choices):
    """
    Return a list of all the supertypes of a given lexical type as defined by
    the user and whether the supertypes provide a path to the root. This does
    not include matrix internal types. Returns an empty list if no supertypes defined.
    """
    pathToRoot = False
    seen = set()
    supertypes = key.get('supertypes','').split(', ')
    if len(supertypes) == 1 and supertypes[0] == '':
        pathToRoot = True
        supertypes = []
    else:
        for supertype in supertypes:
            if supertype in seen: continue
            seen.add(supertype)
            if not choices.get(supertype,False): continue
            parents = choices.get(supertype).get('supertypes','').split(', ')
            if parents:
                if '' in parents:
                    pathToRoot = True
                else:
                    supertypes.extend(parents)
    return supertypes, pathToRoot

def get_lt_name(key, choices):
    if key in LEXICAL_SUPERTYPES:
        # lexical supertype-- pull out of lexical_supertypes, remove '-lex'
        return LEXICAL_SUPERTYPES[key].rsplit('-lex',1)[0]
    else:
        # defined lextype, name may or may not be defined
        name = get_name(choices[key])
        lex_st = LEXICAL_SUPERTYPES[key.strip('1234567890')]
        return '-'.join([name, lex_st.rsplit('-lex',1)[0]])

######################################################################
# validate_lexicon(ch, vr)
#   Validate the user's choices about the test lexicon.

def validate_lexicon(ch, vr):

    # Did they specify enough lexical entries?
    if 'noun' not in ch:
        mess = 'You should create at least one noun class.'
        vr.warn('noun1_stem1_orth', mess)

    # For verbs and verbal inflection, we need to prevent that index features are
    # assigned to verbs: making set of features that should not be assigned to
    # verbs

    # TJT 2014-09-02: Updating/moving this for adjectives: need to keep track of
    # both nominal and verbal constraints to constrain adjective agreement and
    # inflection properly

    # TJT 2014-12-19: Changing these set declarations to list declarations for
    # older versions of python
    index_features = set(['person', 'number','gender'])
    head_features = set(['tense', 'aspect', 'mood'])
    for feature in ch.get('feature'):
        if 'name' in feature:
            if feature.get('type') == 'index':
                index_features.add(feature.get('name'))
            elif feature.get('type') == 'head':
                head_features.add(feature.get('name'))

    # Nouns

    # check noun hierarchy for various properties
    # this is easier if we build some data objects

    # for each nountype,
    #  if it's got stems, it'll need to be put in the lexicon
    #    check that it has an answer for
    #      * has-dets
    #      * that its has-dets doesn't conflict with any parents
    #      * check its features
    #        ** see that they don't conflict with each other
    #      * see that the hierarchy won't cause the
    #        vacuous inheritance eg:
    #          a := noun
    #          b := a
    #          c := b & a

    # ntsts is a dict of nountype names->lists of supertypes
    # inherited_feats dict of nountype names->lists of inherited features
    ntsts = {}
    feats = {}
    inherited_feats = {}

    for noun in ch.get('noun'):
        sts = noun.get('supertypes').split(', ')
        undefined_sts = filter(lambda x: x not in ch, sts)
        if undefined_sts:
            vr.err(noun.full_key + '_supertypes',
                   "The following supertypes are not defined: "
                   "%s" % ', '.join(undefined_sts))
        ntsts[noun.full_key] = sts
        feats[noun.full_key] = {}
        for f in noun.get('feat'):
            feats[noun.full_key][f.get('name')]=f.get('value')

    # now we can figure out inherited features and write them down
    # also, print warnings about feature conflicts
    # also, figure out the det question for types with stems
    # also, every noun type needs a path to the root

    for n in ch.get('noun'):
        st_anc = [] # used to make sure we don't have an lkb err as
        # described in comments above
        root = False
        det = n.get('det')
        # det == '' is okay for now, check again after inheritance is computed
        seen = []
        paths= []
        # seed paths with the starting node and the first crop of sts
        for st in ntsts[n.full_key]:
            paths.append([n.full_key, st])
        parents = ntsts[n.full_key]
        inherited_feats[n.full_key]= {}
        while (True):
            next_parents = []
            for p in parents:
                if p == '':
                    root = True

                # get features from this parent and put em in inherited_feats
                else:
                    ptype = ch.get(p)
                    if not ptype: continue
                    for f in feats[p]:
                        # see if this feat conflicts with what we already know
                        if f in feats[n.full_key] and feats[p][f] != feats[n.full_key][f]:
                            # inherited feature conficts with self defined feature
                            vr.warn(n.full_key + '_feat', "The specification of "+f+"="+ \
                                    str(feats[n.full_key][f])+" may confict with the value"+ \
                                    " defined on the supertype "+ptype.get('name')+" ("+p+").",
                                    concat=False)
                        elif f in inherited_feats[n.full_key] and \
                                        feats[p][f] != inherited_feats[n.full_key][f]:
                            vr.warn(n.full_key + '_supertypes',
                                    "This inherited specification of "+f+"="+ \
                                    str(inherited_feats[n.full_key][f])+ \
                                    " may confict with the value defined on the supertype "+ \
                                    ptype.get('name')+" ("+p+").",concat=False)
                            inherited_feats[n.full_key][f] = "! "+ \
                                                             inherited_feats[n.full_key][f]+" && "+feats[p][f]
                        else:
                            inherited_feats[n.full_key][f] = feats[p][f]

                    # det question
                    pdet = ptype.get('det')
                    if pdet in ['imp','obl']:
                        if det == '':
                            det = pdet
                        elif det != pdet:
                            vr.err(n.full_key + '_det',
                                   "This noun type inherits a conflict in answer"+ \
                                   " to the determiner question.", concat=False)

                    # add sts to the next generation
                    to_be_seen = []
                    for r in paths:
                        if r[-1] == p: #this is the path to extend,
                            paths.remove(r)
                            for q in ntsts[p]: #go through all sts
                                # q is a st_anc of n
                                if not q in st_anc:
                                    st_anc.append(q)
                                if q in r:
                                    vr.err(n.full_key + '_supertypes', "This hierarchy "+
                                           "contains a cycle.  The type _"+q+"_ was found "+
                                           "at multiple points in the inheritance path: "+
                                           str(r + [q]))
                                else:
                                    new_path = r + [q]
                                    paths.append(new_path)
                                if (q != ''):
                                    if not (q in seen):
                                        next_parents.append(q)
                                        to_be_seen.append(q)
                                else:
                                    root = True
                    seen = seen + to_be_seen

            # if there aren't any next parents, we're done
            if len(next_parents) == 0:
                break

            # otherwise we go again
            parents = next_parents

        if(len(inherited_feats[n.full_key]) > 0):
            vr.info(n.full_key + '_feat', "inherited features are: "+
                    str(inherited_feats[n.full_key]))

        if not root:
            vr.err(n.full_key + '_supertypes',
                   "This noun type doesn't inherit from noun-lex or a descendent.")

        # Now check for the lkb err about vacuous inheritance using goodmami's
        #  method: find the intersection of supertypes and supertypes's ancestors
        for t in ntsts[n.full_key]:
            if t in st_anc:
                vr.err(n.full_key + '_supertypes', 'This noun hierarchy contains a '+
                       'redundant link that will result in an lkb error.  _'+t+
                       '_ is both an immediate supertype of'+n.full_key+' and also '+
                       'an ancestor of another supertype.')

        # Now we can check whether there's an answer to the question about
        # determiners?
        # but I only really care about types with stems (right??)
        s = n.get('stem', [])
        if len(s) != 0:
            if not det:
                mess = 'You must specify whether this noun takes a determiner.  '+ \
                       'Either on this type or on a supertype.'
                vr.err(n.full_key + '_det', mess)

        # If they said the noun takes an obligatory determiner, did they
        # say their language has determiners?
        if det == 'obl' and ch.get('has-dets') == 'no':
            mess = 'You defined a noun that obligatorily takes a determiner, ' + \
                   'but also said your language does not have determiners.'
            vr.err('has-dets', mess)
            vr.err(n.full_key + '_det', mess)

        # or if they said the noun takes an obligatory determiner, did they
        # provide any determiners?
        if det in ('obl', 'opt') and (not 'det' in ch):
            message_map = {'obl': ('obligatorily', ''),
                           'opt': ('optionally', 'with a determiner ') }
            mess = 'A noun defined as %s taking a determiner is unusable %s' + \
                   'without any determiners defined.'
            vr.warn(n.full_key + '_det', mess % message_map[det])

        for stem in s:
            orth = stem.get('orth')
            pred = stem.get('pred')

            # Did they give a spelling?
            if not orth:
                mess = 'You must specify a spelling for each noun you define.'
                vr.err(stem.full_key + '_orth', mess)

            # Did they give a predicate?
            if not pred:
                mess = 'You must specify a predicate for each noun you define.'
                vr.err(stem.full_key + '_pred', mess)

    # Verbs
    # check verb hierarchy for various properties
    # this is easier if we build some data objects

    # for each verb,
    #  if it's got stems, it'll need to be put in the lexicon
    #    check that it has an answer for
    #      * valence
    #      * that its has-dets doesn't conflict with any parents
    #      * check its features
    #        ** see that they don't conflict with each other
    #      * see that the hierarchy won't cause the
    #        vacuous inheritance eg:
    #          a := verb
    #          b := a
    #          c := b & a

    # vtsts is a dict of verbtype names->lists of supertypes
    # inherited_feats dict of verbtype names->lists of inherited features
    vtsts = {}
    feats = {}
    inherited_feats = {}
    for v in ch.get('verb'):
        vtsts[v.full_key] = v.get('supertypes').split(', ')
        feats[v.full_key] = {}
        for f in v.get('feat'):
            feats[v.full_key][f.get('name')]=f.get('value')

    seenTrans = False
    seenIntrans = False

    for v in ch.get('verb'):
        st_anc = [] # used to make sure we don't have an lkb err as
        # described in comments above
        root = False
        val = v.get('valence')
        # val can be null for now
        # check again after computing inheritance
        seen = []
        paths= []

        bistems = v.get('bistem', [])
        bipartitepc = v.get('bipartitepc')

        # seed paths with the starting node and the first crop of sts
        for st in vtsts[v.full_key]:
            paths.append([v.full_key, st])
        parents = vtsts[v.full_key]
        inherited_feats[v.full_key]= {}

        # now step up through the supertypes until we're done
        # compute inherited feats and check for clashes on val
        while(True):
            next_parents = []
            for p in parents:
                if p == '':
                    root = True

                # get features from this parent and put em in inherited_feats
                if p != '':
                    ptype = ch.get(p)
                    if not ptype: continue
                    for f in feats[p]:
                        # see if this feat conflicts with what we already know
                        if f in feats[v.full_key] and feats[p][f] != feats[v.full_key][f]:
                            # inherited feature conficts with self defined feature
                            vr.warn(v.full_key + '_feat', "The specification of "+f+"="+
                                    str(feats[v.full_key][f])+" may confict with the value "+
                                    "defined on the supertype "+ptype.get('name')+" ("+p+").",
                                    concat=False)
                        elif f in inherited_feats[v.full_key] and \
                                        feats[p][f] != inherited_feats[v.full_key][f]:
                            vr.warn(v.full_key + '_supertypes', "This inherited "+
                                    "specification of "+f+"="+
                                    str(inherited_feats[v.full_key][f])+
                                    " may confict with the value defined on the supertype "+
                                    ptype.get('name')+" ("+p+").",concat=False)
                            inherited_feats[v.full_key][f] = "! "+ \
                                                             inherited_feats[v.full_key][f]+" && "+feats[p][f]
                        else:
                            inherited_feats[v.full_key][f] = feats[p][f]

                    # val question
                    pval = ptype.get('valence')
                    if pval != '':
                        if val == '':
                            val = pval
                        elif val != pval:
                            vr.err(v.full_key + '_valence', "This verb type inherits a "+
                                   "conflict in answer to the question about argument "+
                                   "structure.",concat=False)

                    # add sts to the next generation
                    to_be_seen = []
                    for r in paths:
                        if r[-1] == p: #this is the path to extend,
                            paths.remove(r)
                            for q in vtsts[p]: #go through all sts
                                # q is a st_anc of n
                                if not q in st_anc:
                                    st_anc.append(q)
                                if q in r:
                                    vr.err(v.full_key + '_supertypes', "This hierarchy contains "+
                                           "a cycle.  The type _"+q+"_ was found at multiple "+
                                           "points in the inheritance path: "+str(r + [q]))
                                else:
                                    new_path = r + [q]
                                    paths.append(new_path)
                                if (q != ''):
                                    if not (q in seen):
                                        next_parents.append(q)
                                        to_be_seen.append(q)
                                else:
                                    root = True
                    seen = seen + to_be_seen

            # if there aren't any next parents, we're done
            if len(next_parents) == 0:
                break

            # otherwise we go again
            parents = next_parents

        # now check val
        # only care if it has stems
        s = v.get('stem', [])
        if len(s) != 0:
            if not val:
                mess = 'You must specify the argument structure of each verb you '+ \
                       'define.  Either on this type, or on a supertype.'
                vr.err(v.full_key + '_valence', mess)
            elif val[0:5] == 'trans' or '-' in val:
                seenTrans = True
            else:
                seenIntrans = True

        if bistems and not bipartitepc:
            mess = 'If you add bipartite stems to a class, you must specify a '+ \
                   'position class for the affix part of the stems.'
            vr.err(v.full_key + '_bipartitepc', mess)

        for stem in v.get('stem', []):
            orth = stem.get('orth')
            pred = stem.get('pred')

            if not orth:
                mess = 'You must specify a spelling for each verb you define.'
                vr.err(stem.full_key + '_orth', mess)

            if not pred:
                mess = 'You must specify a predicate for each verb you define.'
                vr.err(stem.full_key + '_pred', mess)

        for bistem in bistems:
            orth = bistem.get('orth')
            aff = bistem.get('aff')
            pred = bistem.get('pred')

            if not orth:
                mess = 'You must specify a spelling for each verb you define.'
                vr.err(bistem.full_key + '_orth', mess)

            if not aff:
                mess = 'You must specify a affix for each bipartite verb stem you '+ \
                       'define.'
                vr.err(bistem.full_key + '_aff', mess)

            if not pred:
                mess = 'You must specify a predicate for each verb you define.'
                vr.err(bistem.full_key + '_pred', mess)

    if not (seenTrans and seenIntrans):
        mess = 'You should create intransitive and transitive verb classes.'
        vr.warn('verb1_valence', mess)
        vr.warn('verb2_valence', mess)

    # Adjectives TJT 2014-08-25
    # First, gather switching adjective position classes' inputs
    adj_pc_switching_inputs = defaultdict(list)
    for adj_pc in ch.get('adj-pc',[]):
        if adj_pc.get('switching',''):
            inputs = adj_pc.get('inputs',[]).split(', ')
            if isinstance(inputs, basestring):
                adj_pc_switching_inputs[inputs].append(adj_pc)
            else:
                # Else, assume list
                for item in inputs:
                    adj_pc_switching_inputs[item].append(adj_pc)

    # TJT 2015-02-05: Check for conflicts between switching position classes
    # and their input lexical types
    for adj in adj_pc_switching_inputs:
        for pc in adj_pc_switching_inputs.get(adj):
            if not (ch.get(adj, ChoiceDict()).get('mod','') == 'none' or ch.get(adj, ChoiceDict()).get('predcop','') == 'opt'):
                vr.err(pc.full_key+'_name',
                       'This position class was created to enable behavior on ' + \
                       'an adjectival lexical type on the Lexicon page which is ' + \
                       'no longer configured to require this position class. You ' + \
                       'should either delete this position class or double check ' + \
                       'your types on the Lexicon page.')

    for adj in ch.get('adj',[]):
        mode = adj.get('mod','')

        # Name can't have illegal characters
        name = adj.get('name')
        if name:
            if " " in name:
                vr.err(adj.full_key + '_name',
                       'Type names cannot include the space character')

        # Supertypes must be defined
        supertypes = adj.get('supertypes','').split(', ')
        undefined_supertypes = filter(lambda x: x not in ch, supertypes)
        if undefined_supertypes:
            vr.err(adj.full_key + '_supertypes',
                   "The following supertypes are not defined: "
                   "%s" % ', '.join(undefined_supertypes))

        # Enforce feature specifications to unify with mode
        illegal_heads = {'attr':'subj', 'pred':'mod'}
        if mode in illegal_heads:
            name_map = {'attr':'attributive', 'pred':'predicative'}
            illegal_head = illegal_heads[mode]
            for feat in adj.get('feat',[]):
                if feat.get('head','') == illegal_head:
                    vr.err(feat.full_key+'_head',
                           'This head is not compatible with %s adjectives.' % name_map[mode])

        # Adjective agreement features on one adjective are exclusively set
        # through the Morphology page
        for feat in adj.get('feat',[]):
            if feat.get('head','') in illegal_heads.values():
                vr.err(feat.full_key+'_head',
                       'This head is available here to enable features on the Morphology ' + \
                       'page. If you are encountering this error, make sure you have ' + \
                       'JavaScript enabled, or try deleting this feature and checking the ' + \
                       'Morphology page.')

        # Mode specific adjective choices are disregarded without the proper mode
        if mode in ('pred', 'attr'):
            inverse_mode_name = {'pred':'attributively', 'attr':'predicatively'}[mode]
            message = 'This choice is only applicable to adjectives behaving ' + \
                      ('%s. This choice will be ignored, or you ' % inverse_mode_name) + \
                      'can change the adjective\'s behavoir above to enable this choice.'
            if mode != 'attr':
                if adj.get('modpos'):
                    vr.warn(adj.full_key+'_modpos', message)
                if adj.get('modunique'):
                    vr.warn(adj.full_key+'_modunique', message)
            if mode != 'pred':
                if adj.get('predcop'):
                    vr.warn(adj.full_key+'_predcop', message)

        # Adjectives defined as a copula complement are unusuable without a copula defined
        if adj.get('predcop','') in ('opt', 'obl') and not ch.get('cop',''):
            messages = {
                'opt':'optionally a copula complement is unusable as a copula complement',
                'obl':'obligatorily a copula complement is unusable' }
            message = messages[adj.get('predcop','')]
            vr.warn(adj.full_key+'_predcop',
                    'An adjective defined as %s without a copula ' % message + \
                    'defined on the Lexicon page.')

        ## Get path to root and supertypes to check
        supertypes, pathToRoot = get_all_supertypes(adj, ch)

        # Each adjective needs a path to root
        if not pathToRoot:
            vr.err(adj.full_key+'_supertypes',
                   'This adjective type doesn\'t inherit from adj-lex or a ' + \
                   'descendant, where a type inherits from adj-lex if it is' + \
                   'defined without a supertype.')

        # Check supertypes for inherited features and collisions
        inherited_choices = defaultdict(dict)
        name_map = {'mod':'adjective mode', 'modpos':'attributive modification direction',
                    'modunique':'unique modification', 'predcop':'copula complement'}
        for choice in name_map:
            adj_value = adj.get(choice,False)
            for supertype in supertypes:
                supertype_def = ch.get(supertype,False)
                if supertype_def:
                    supertype_value = supertype_def.get(choice,False)
                    if supertype_value:
                        # Type definitions must unify with their supertype's definitions
                        if adj_value:
                            if supertype_value != adj_value:
                                # Check for unifiable constraints
                                ## Both and predicative/attributive unify
                                if supertype_value == 'both' and adj_value in ('pred', 'attr'):
                                    continue
                                ## Either position and before/after unify
                                elif supertype_value == 'either' and adj_value in ('before', 'after'):
                                    continue
                                ## modunique underspecified can be the parent of modunique +
                                if supertype_def.get(choice,'missing') == 'missing' and adj.get(choice,'') == 'on':
                                    continue
                                # Found collision
                                vr.err(adj.full_key+'_supertypes',
                                       'This adjective definition clashes with its supertype ' + \
                                       ('%s on choice of %s. ' % (supertype, name_map[choice])) + \
                                       ('type choice: %s; supertype choice: %s.' % (adj.get(choice), supertype_value)))
                        # Keep track of inherited values
                        inherited_choices[choice][supertype_def.get('name')] = supertype_value
                        # else: if supertype doesn't make the choice, then the choice
                        # would not be required on its children, so no need to do anything here

        if not inherited_choices:
            # Adjectives must have a mode defined or a supertype
            if not mode:
                vr.err(adj.full_key+'_mod',
                       'Every adjective requires a choice of mode (attributive, ' + \
                       'predicative, or both)')

            # If mode 'unspecified', mode must be defined on a position class accepting
            # this as its input
            if mode == 'none':
                if adj.full_key not in adj_pc_switching_inputs:
                    vr.err(adj.full_key+'_mod',
                           'Unspecified adjectives must be specified for mode by ' + \
                           'some position class on the Morphology page enabled by using ' + \
                           'argument agreement choices on the feature section below.')

        # Print info boxes on inherited choices
        else:
            for choice in inherited_choices:
                vr.info('%s_%s' % (adj.full_key, choice),
                        'inherited choices are: %s' % (inherited_choices[choice]))

        # If no supertypes, check for required choices
        if not supertypes:
            if mode in ('both', 'attr'):
                if not adj.get('modpos'):
                    message = 'This choice is required for adjectives that can be attributive'
                    vr.err(adj.full_key+"_modpos", message)
            if mode in ('both', 'pred'):
                if not adj.get('predcop'):
                    message = 'This choice is required for adjectives that can be predicative'
                    vr.err(adj.full_key+"_predcop", message)

        # If adjective is optionally a copula complement, it must have an associated
        # switching position class defined on the Morphology page.
        if adj.get('predcop','') == "opt":
            if adj.full_key not in adj_pc_switching_inputs:
                vr.err(adj.full_key+'_predcop',
                       'Adjective types specified as optionally copula complement must ' + \
                       'be the input to a position class on the Morphology page ' + \
                       'that enables this functionality.')

        # if adj.get('mod','') == 'none':
        #   if adj.full_key not in adj_pc_switching_inputs:
        #     vr.err(adj.full_key+'_mod',
        #            'Adjective types with unspecified syntactic behavior must ' +\
        #            'be the input to a position class on the Morphology page ' +\
        #            'that enables this functionality.')

        # Check for clashes between inherited features and specified features
        inherited_feats = defaultdict(dict) # name -> 'value' -> value, 'specified on' -> head
        name_map = {'xarg':'Both positions', 'mod':'The modified noun',
                    'subj':'The subject', 'adj': 'The adjective'}
        for supertype in supertypes:
            supertype_def = ch.get(supertype,False)
            if not supertype_def: continue
            for feature in supertype_def.get('feat',[]):
                inherited_feats[feature.get('name')]['value'] = feature.get('value')
                head = feature.get('head')
                inherited_feats[feature.get('name')]['specified on'] = name_map[head] if head in name_map else head

        # Print info boxes on inherited features
        if len(inherited_feats) > 0:
            vr.info(adj.full_key + '_feat', "inherited features are: "+
                    str(dict(inherited_feats)))

        # Each stem needs a predicate / each predicate needs a stem
        message = 'Every stem requires both a spelling and predicate'
        for stem in adj.get('stem',''):
            orth = stem.get('orth',False)
            pred = stem.get('pred',False)
            if not orth:
                vr.err(stem.full_key+"_orth", message)
            if not pred:
                vr.err(stem.full_key+"_pred", message)

    # Auxiliaries
    aux_defined = 'aux' in ch
    if ch.get('has-aux') != 'yes':
        if aux_defined:
            mess = 'You have indicated that your language has no auxiliaries ' + \
                   'but have entered an auxiliary on the Lexicon page.'
            vr.err('has-aux', mess)

    if ch.get('has-aux') == 'yes':
        if not aux_defined:
            mess = 'You have indicated that your language has auxiliaries. ' + \
                   'You must define at least one auxiliary type.'
            vr.err('auxlabel', mess)

    comp = ch.get('aux-comp')
    for aux in ch.get('aux'):
        sem = aux.get('sem')
        pred = aux.get('pred')
        subj = aux.get('subj')

        if 'stem' not in aux:
            mess = 'You must specify a stem for each auxiliary type defined.'
            vr.err(aux.full_key + '_stem1_orth', mess)

        if not sem:
            mess = 'You must specify whether the auxiliary contributes a predicate.'
            vr.err(aux.full_key + '_sem', mess)

        if sem == 'add-pred':
            for feat in aux.get('feat', []):
                if feat.get('name') and not feat.get('value'):
                    mess = 'You must specify a value for this feature.'
                    vr.err(feat.full_key + '_value', mess)
                if feat.get('name') == 'evidential':
                    vr.warn(feat.full_key + '_name',
                        'You have specified an evidential and an added predicate on this auxiliary.' +\
                   'The evidential\'s semantics will overwrite the predicate you have specified.')
        for feat in aux.get('feat', []):
            if feat['name'] == 'evidential' and len(feat.get('value').split(',')) > 1:
                vr.err(feat.full_key + '_value',
                  'Choose only one evidential term.')

        if comp == 'vp' or comp == 'v':
            if not subj:
                mess = 'You must specify the subject type.'
                vr.err(aux.full_key + '_subj', mess)

        compform = 'no'
        for cf in aux.get('compfeature', []):
            name = cf.get('name')
            if name == 'form':
                compform = 'yes'
            if name and not cf.get('value'):
                mess = 'You must specify a value for this feature.'
                vr.err(cf.full_key + '_value', mess)
            if name == 'evidential':
                vr.err(cf.full_key + '_name', 'Evidentials should be specified as an auxiliary feature, not a complement feature.')

        if not compform == 'yes':
            mess = 'You must specify the form of the verb in the complement, ' + \
                   'i.e., the value of the complement feature FORM.'
            vr.err(aux.full_key + '_complabel', mess)

        for stem in aux.get('stem', []):
            if sem == 'add-pred' and not stem.get('pred'):
                mess = 'You have indicated that this type contributes a predicate. ' + \
                       'You must specify the predicate name.'
                vr.err(stem.full_key + '_pred', mess)
            if sem != 'add-pred' and stem.get('pred'):
                mess = 'You have specified a predicate but indicated ' + \
                       'that this type does not contribute a predicate.'
                vr.err(aux.full_key + '_sem', mess)
            if not stem.get('orth'):
                mess = 'You must specify a spelling for each auxiliary you define.'
                vr.err(stem.full_key + '_orth', mess)

    # TODO: Copulas: TJT 2014-08-25
    # Copulas
    for cop in ch.get('cop',[]):
        # Names can't have illegal characters
        name = cop.get('name',False)
        if name:
            if " " in name:
                vr.err(cop.full_key + '_name',
                       'Type names cannot include the space character.')

        # Supertypes must be defined
        immediate_supertypes = cop.get('supertypes','').split(', ')
        undefined_supertypes = filter(lambda st: st not in ch, immediate_supertypes)
        if undefined_supertypes:
            vr.err(adj.full_key+'_supertypes',
                   "The following supertypes are not defined: "
                   "%s." % ', '.join(undefined_supertypes))

        ## Get path to root and supertypes to check
        supertypes, pathToRoot = get_all_supertypes(cop, ch)
        inherited_choices = defaultdict(dict)

        # Check supertypes for inherited features and collisions
        name_map = {'comptype':'complement type'}
        for choice in name_map:
            cop_value = cop.get(choice,False)
            for supertype in supertypes:
                supertype_def = ch.get(supertype,False)
                if supertype_def:
                    supertype_value = supertype_def.get(choice,False)
                    if supertype_value:
                        # Type definitions must unify with their supertype's definitions
                        if cop_value:
                            if supertype_value != cop_value:
                                # Found collision
                                vr.err(cop.full_key+'_supertypes',
                                       'This adjective definition clashes with its supertype ' + \
                                       ('%s on choice of %s. ' % (supertype, name_map[choice])) + \
                                       ('type choice: %s; supertype choice: %s.' % (cop_value, supertype_value)))
                        # Keep track of inherited values
                        inherited_choices[choice][supertype_def.get('name')] = supertype_value

        # Copulas must have a complement type specified
        if not inherited_choices:
            if not cop.get('comptype',False):
                vr.err(cop.full_key+'_comptype',
                       'Every copula must have at least one complement type defined.')

        # Print infos on inherited choices
        else:
            for choice in inherited_choices:
                vr.info('%s_%s' % (cop.full_key, choice),
                        'inherited choices are: %s' % (inherited_choices[choice]))

        # Each Copula needs a path to root
        if not pathToRoot:
            vr.err(cop.full_key+'_supertypes',
                   'This copula type doesn\'t inherit from cop-lex or a ' + \
                   'descendant, where a type inherits from cop-lex if it is' + \
                   'defined without a supertype.')

            # TJT 2014-09-05: Verbs don't seem to do this, so not doing it
            # Supertype features cannot conflict with type features
        #    for stname in supertypes:
        #      supertype = ch.get(stname,False)
        #      if supertype:
        #        supertype_features = supertype.get('feats',[])
        #        for feat in supertype_features:

    # Determiners
    for det in ch.get('det',[]):
        for stem in det.get('stem', []):
            if not stem.get('orth'):
                mess = 'You must specify a spelling for each determiner you define.'
                vr.err(stem.full_key + '_orth', mess)

            if not stem.get('pred'):
                mess = 'You must specify a predicate for each determiner you define.'
                vr.err(stem.full_key + '_pred', mess)

    # Adpositions
    for adp in ch.get('adp'):
        if 'feat' not in adp:
            mess = 'You should specify a value for at least one feature (e.g., CASE).'
            vr.warn(adp.full_key + '_feat1_name', mess)

    # LLD 1-3-2016
    cases = []
    not_covered = []
    for p in ch.patterns():
        p = p[0].split(',')
        c = p[0].split('-')
        cases += [c[0], c[1]] if len(c) > 1 else [c[0]]
    cases.remove('trans')
    cases.remove('intrans')
    # if adpositions are not marked optional...
    if ch.has_adp_case() and not ch.has_optadp_case():
        for case in cases:
            # each case must be contributed by an adposition or noun case somewhere.
            if not (ch.has_adp_case(case) or ch.has_noun_case(case)):
                if case not in not_covered:
                    not_covered += [case]
        if len(not_covered) > 0:
            mess = 'You have case-marking adpositions marked non-optional, but not all ' + \
                   'core cases are contributed by adpositions, inflection, or lexical types. ' + \
                   'Please account for these cases: ' + ', '.join(not_covered) + '.'
            vr.err('adp1_opt', mess)



    # Features on all lexical types
    # TJT 2014-09-02: Adding adj and cop
    # TJT 2014-09-08: Changing to ALL_LEX_TYPES here to support future development
    for lextype in ALL_LEX_TYPES + ('adp',):
        for lt in ch.get(lextype):
            for feat in lt.get('feat', []):
                if not feat.get('name'):
                    mess = 'You must choose which feature you are specifying.'
                    vr.err(feat.full_key + '_name', mess)
                if not feat.get('value'):
                    mess = 'You must choose a value for each feature you specify.'
                    vr.err(feat.full_key + '_value', mess)

                if feat.get('name') == 'argument structure':
                    mess = 'The pseudo-feature "argument structure" is only ' + \
                           'appropriate for inflectional morphemes.  For verbs, ' + \
                           'please use the special argument structure drop-down; ' + \
                           'other lexical types do not yet support argument structure.'
                    vr.err(feat.full_key + '_name', mess)

                # TJT 2014-09-03: Adding adjectives and copulas here
                if lextype in ('verb', 'adj', 'cop'): # or lextype == 'aux': lap: 1/5/11 removed aux to get rid of overzealous validation
                    # The head must be defined
                    if not feat.get('head'):
                        mess = 'You must choose where the feature is specified.'
                        vr.err(feat.full_key + '_head', mess)
                    # Index features must be defined on some argument
                    elif feat.get('head') in ('verb', 'adj', 'cop'):
                        if feat.get('name') in index_features:
                            mess = 'This feature is associated with nouns. ' + \
                                   'Please select one of the NP options to use this feature.'
                            vr.err(feat.full_key + '_head', mess)
                    # Head features must be defined on the main predicate
                    elif feat.get('name') in head_features:
                        # Head features can be defined on adjective complements
                        # TODO: TJT 2014-09-15: Additional complement types will need to be more clever about this
                        if not feat.get('head') == 'comp':
                            predicate = {"verb":"verb", "adj":"adjective", "cop":"copula"}[lextype]
                            mess = 'This feature is associated with predicates. ' + \
                                   'Please select "the %s" to use this feature.' % predicate
                            vr.err(feat.full_key + '_head', mess)

                if not ch.has_dirinv() and feat.get('head') in ['higher', 'lower']:
                    mess = 'That choice is not available in languages ' + \
                           'without a direct-inverse scale.'
                    vr.err(feat.full_key + '_head', mess)

