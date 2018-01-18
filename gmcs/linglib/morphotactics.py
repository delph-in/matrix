from collections import defaultdict

from gmcs.linglib import lexicon
from gmcs.linglib.lexbase import (PositionClass, LexicalRuleType,
    #MorphotacticNode, LexicalType,
                                  LexicalRuleInstance,
                                  ALL_LEX_TYPES,
                                  LEXICAL_CATEGORIES,
                                  LEXICAL_SUPERTYPES,
                                  NON_ESSENTIAL_LEX_CATEGORIES)

from gmcs.lib import Hierarchy
from gmcs.utils import get_name
#from gmcs.utils import TDLencode

### Contents
# 1. Module Variables
# 2. Helper functions
# 3. Main logic functions
# 4. Output functions

## NOTES ON ABBREVIATIONS
# pc : position class
# lr : lexical rule
# lrt : lexical rule type
# lri : lexical rule instance
# lt : lexical type (e.g. noun1, verb1, etc.)
# le: lexical entry (i.e., stem on a lexical type)
# lst : lexical supertype (generic lts, like noun, verb, iverb, etc.)
# mn : morphotactic node (a pc, lrt, lt, or lst)

########################
### MODULE VARIABLES ###
########################

_mns = {}
_dtrs = set()
_infostr_lrt = []
_infostr_head = {}
_id_key_tbl = {}
_nonleaves = []
_supertypes = {}

########################
### HELPER FUNCTIONS ###
########################

def all_position_classes(choices):
    """ Yield each position class defined in the choices file. """
    for lt in ALL_LEX_TYPES:
        for pc in choices[lt + '-pc']:
            yield pc

def defined_lexrule_sts(lrt,pc):
    """
    Return the list of lexical rule supertypes, filtering out ones
    not defined in the pc (likely Matrix types)
    """
    sts = lrt.split_value('supertypes')
    to_return = []
    for lrt in pc['lrt']:
        if lrt.full_key in sts:
            to_return.append(lrt.full_key)
    return to_return
#  for st in lrt.split_value('supertypes'):
#    print st in [[l.full_key] for l in pc['lrt']]
#return [st for st in lrt.split_value('supertypes') if st in [[l.full_key] for l in pc['lrt']]]
#return [st for st in lrt.split_value('supertypes') if st in pc['lrt']]

def disjunctive_typename(mns):
    """
    Return a string that uses '-or-' as a delimiter to concatenate the
    names of all morphotactic nodes in mns.
    """
    return '-or-'.join(sorted([mn.name for mn in mns]))

def flag_name(flag_tuple):
    """
    Return the flag name for rule types in flag_tuple.
    """
    return disjunctive_typename(flag_tuple).upper() + '-FLAG'

def sequential(mn1, mn2):
    """
    Return True if the one of the MNs appears on the other's input.
    """
    return (mn1.precedes(mn2) or mn2.precedes(mn1))

def ordered_constraints(mn, constraint_type):
    """
    Return a list of constrained LRTs such that they are ordered
    according to their input order, or, if nonsequential, their keys.
    """
    ordered = []
    for c in mn.constraints.get(constraint_type, {}).values():
        loc = len(ordered)
        for i, o in enumerate(ordered):
            if c.precedes(o) \
                    or (not o.precedes(c) and c.key < o.key):
                loc = i
                break
        ordered.insert(loc, c)
    return ordered

def get_input_map(pch):
    """
    For the given position class hierarchy, return a map with the sets
    of all valid input rules/types as the keys and lists of position
    classes taking those inputs as the values.
    """
    inp_map = defaultdict(list)
    for pc in pch.nodes.values():
        i_s = tuple(sorted(pc.valid_inputs(), key=lambda x: x.tdl_order))
        if len(i_s) > 0:
            inp_map[i_s] += [pc]
    return inp_map

def get_vtype(stem, choices):
    """
    Helper function to look up verb type in choices file
    for a particular stem.
    """
    verb_prefix = stem.split('_')[0]
    verb = choices.get(verb_prefix)
    return get_name(verb) + '-verb-lex'


##########################
### MAIN LOGIC METHODS ###
##########################

def customize_inflection(choices, add_methods, mylang, irules, lrules, lextdl):
    """
    Process the information in the given choices file and add the rules
    and types necessary to model the inflectional system into the irules,
    lrules, and primary grammar files.
    """
    # first call other libraries' add_lexrules methods to see if they
    # have anything to add to the choices file before we begin
    # TODO: negation.py should be called later but should use
    # one of these add_methods
    for method in add_methods:
        method(choices)
    # now create the hierarchy
    pch = customize_lexical_rules(choices)
    # write_rules currently returns a list of items needing feature
    # customization. Hopefully we can find a better solution
    return write_rules(pch, mylang, irules, lrules, lextdl, choices)

def customize_lexical_rules(choices):
    """
    Interpret the PCs in a Choices file into a set of lexical rules.
    """
    # When customizing and outputting lexical rules, there are some things
    # we get for free from the choices file (features, orthographies,
    # lexical rule hierarchies) and some things we must infer (DTRs for
    # rules (intermediate types), possible inputs for rules, flags).
    # We must do some things in a certain order:
    #  1. create containers for explicitly defined PCs
    #  2. determine the constraints PCs place on each other
    #  3. find the unique input for each PC (and create intermediate rules)
    #      (all_inputs() depends on forward-looking require constraints)
    #  4. determine and create flags based on constraints
    pch = position_class_hierarchy(choices)
    interpret_constraints(choices)
    create_flags()
    calculate_supertypes(pch)
    return pch

### POSITION CLASSES AND LEXICAL RULE TYPES ###

def position_class_hierarchy(choices):
    """
    Create and return the data structures to hold the information
    regarding position classes and lexical types.
    """

    pch = Hierarchy()

    # Create PositionClasses for lexical types so they can take flags
    add_lexical_type_hierarchy(pch, choices)

    # We can't set parents until we have created all MN objects.
    pc_inputs = {}
    # Now create the actual position classes
    for i, pc in enumerate(all_position_classes(choices)):
        # these PCs are ChoiceDicts
        if len(pc.get('inputs', '')) > 0:
            pc_inputs[pc.full_key] = set(pc.get('inputs').split(', '))
        cur_pc = pch.add_node(PositionClass(pc.full_key, get_name(pc),
                                            order=pc.get('order')))
        cur_pc.tdl_order = i
        _mns[cur_pc.key] = cur_pc
        # If only one root LRT, and the PC or the LRT are unnamed, merge them
        if pc_lrt_mergeable(pc):
            pc_lrt_merge(cur_pc, pc)
        # Fill the lexical rule types with the information we know
        create_lexical_rule_types(cur_pc, pc)
    # now assign pc inputs
    for pc in pc_inputs:
        for inp in pc_inputs[pc]:
            if inp not in _mns and inp in LEXICAL_CATEGORIES:
                continue
            _mns[pc].relate(_mns[inp], 'parent')
    return pch

def add_lexical_type_hierarchy(pch, choices):
    for lex_cat in LEXICAL_CATEGORIES:
        if lex_cat not in choices: continue
        lth = lexicon.lexical_type_hierarchy(choices, lex_cat)
        _mns[lth.key] = lth
        _mns.update(lth.nodes)
        pch.add_node(lth)

def pc_lrt_mergeable(pc):
    """
    A pc is mergeable with its only daughter in case it only has
    one daughter.
    """
    return len([l for l in pc['lrt'] if not defined_lexrule_sts(l, pc)]) == 1

def pc_lrt_merge(cur_pc, pc):
    lrt = pc['lrt'].get_first()
    if pc.get('name','') == '' or lrt.get('name','') == '':
        name = pc.get('name') or lrt.get('name') or pc.full_key
        lrt['name'] = cur_pc.name = name
        cur_pc.identifier_suffix = 'lex-rule'

def create_lexical_rule_types(cur_pc, pc):
    lrt_parents = {}
    # TJT 2014-08-21 Check incorporated stems too
    all_lrts = (pc.get('lrt',[]), pc.get('is-lrt',[]))
    for lrts in all_lrts:
        for j, lrt in enumerate(lrts):
            mtx_supertypes = set()
            if 'supertypes' in lrt:
                lrt_parents[lrt.full_key] = set(defined_lexrule_sts(lrt,pc))
                mtx_supertypes = set(lrt.split_value('supertypes')).difference(
                    lrt_parents.get(lrt.full_key,set()))
            # default name uses name of PC with _lrtX
            if 'name' not in lrt:
                lrt['name'] = cur_pc.name + lrt.full_key.replace(cur_pc.key, '', 1)
            cur_lrt = create_lexical_rule_type(lrt, mtx_supertypes, cur_pc)
            # the ordering should only mess up if there are 100+ lrts
            cur_lrt.tdl_order = cur_pc.tdl_order + (0.01 * j)
            cur_pc.add_node(cur_lrt)
    for child in lrt_parents:
        for parent in lrt_parents[child]:
            cur_pc.relate_parent_child(_mns[parent], _mns[child])

def create_lexical_rule_type(lrt, mtx_supertypes, cur_pc):
    new_lrt = LexicalRuleType(lrt.full_key, get_name(lrt))
    _id_key_tbl[new_lrt.identifier()] = lrt.full_key
    for feat in lrt.get('feat'):
        if feat['name'] == 'evidential':
            new_lrt.evidential = feat['value']
        else:
            new_lrt.features[feat['name']] = {'value': feat['value'],
                                              'head': feat['head']}

    # CMC 2017-03-24 Populate valence change operations
    new_lrt.valchgops = lrt.get('valchg',[])

    # TJT 2014-08-27: For adjective position classes,
    # check for additional choices to copy to features
    # TJT 2014-11-06: Simplifying... just copy all choices
    if 'adj-pc' in lrt.full_key:
        for choice in ('modpos','predcop','mod'):
            lrt_choice = lrt.get(choice,False)
            if lrt_choice:
                new_lrt.features[choice] = lrt_choice
    # TJT 2014-08-21: Keep track of preds for adjective incorporation
    # using new LexicalRuleInstance class
    new_lrt.lris = [LexicalRuleInstance(lri['orth'], lri['pred']) \
                        if lri['inflecting'] == 'yes' else LexicalRuleInstance('')
                    for lri in lrt.get('lri',[])]
    # Fill out the obvious supertypes (later we'll finish)
    set_lexical_rule_supertypes(new_lrt, mtx_supertypes)
    _mns[new_lrt.key] = new_lrt
    return new_lrt

### CONSTRAINTS ###

def interpret_constraints(choices):
    convert_obligatoriness_to_req(choices)
    for mn in _mns.values():
        # don't bother if the morphotactic node is not defined in choices
        if mn.key not in choices \
                or not isinstance(choices[mn.key], dict): continue

        for req in choices[mn.key].get('require', []):
            others = dict([(o, _mns[o]) for o in req['others'].split(', ')])
            mn.disjunctive_flag_sets[tuple(sorted(others.keys()))] = others
            if all(o.precedes(mn) for o in others.values()):
                mn.constraints['req-bkwd'].update(others)
            elif all(mn.precedes(o) for o in others.values()):
                mn.constraints['req-fwd'].update(others)
                # we're not covering the case where others appear before
                # and after the current pc.
                # the case where it is neither followed by or follows other
                # should be covered in a validation test
        for fbd in choices[mn.key].get('forbid',[]):
            other = _mns[fbd['others']]
            # only forbid backwards. convert forwards forbids to backwards
            if other.precedes(mn):
                mn.constraints['forbid'][other.key] = other
            elif mn.precedes(other):
                other.constraints['forbid'][mn.key] = mn

def convert_obligatoriness_to_req(choices):
    """
    For all PCs marked as obligatory, add a "require" constraint for
    that PC on each of its basetypes.
    """
    for pc in all_position_classes(choices):
        if pc.get('obligatory','') == 'on':
            basetypes = [i for i in _mns[pc.full_key].input_span().values()
                         if len(i.inputs()) == 0]
            for bt in basetypes:
                bt.constraints['req-fwd'][pc.full_key] = _mns[pc.full_key]

### FLAGS ###

def create_flags():
    # these are values to be placed on flags:
    # tuple is ((SELF.MOTHER, SELF.DTR), (OTHER.MOTHER, OTHER.DTR))
    reqfwd  = (('-', None), ('+', None))
    reqbkwd = ((None, '+'), ('+', None))
    forbid  = ((None, 'na'), ('+', None))
    for mn in _mns.values():
        assign_flags(mn, reqfwd, minimal_flag_set(mn, 'req-fwd'))
        assign_flags(mn, reqbkwd, minimal_flag_set(mn, 'req-bkwd'))
        assign_flags(mn, forbid, minimal_flag_set(mn, 'forbid'))

def assign_flags(mn, flag_values, flag_groups):
    for flag_group in flag_groups:
        flag_tuple = tuple(sorted(flag_group.values(), key=lambda x: x.tdl_order))
        # first apply the value to the LR making the constraint
        if flag_values[0][1] is not None:
            mn.flags['in'][flag_tuple] = flag_values[0][1]
        if flag_values[0][0] is not None:
            mn.flags['out'][flag_tuple] = flag_values[0][0]
        # now apply the flag values to all objects of the flag
        for other in flag_group.values():
            if flag_values[1][1] is not None:
                other.flags['in'][flag_tuple] = flag_values[1][1]
            if flag_values[1][0] is not None:
                other.flags['out'][flag_tuple] = flag_values[1][0]
        # also set initial flag values for req-bkwd constraints
        if flag_values[0][1] == '+':
            basetypes = [i for i in mn.input_span().values() if len(i.inputs()) == 0]
            for bt in basetypes:
                set_req_bkwd_initial_flags(bt.pc, flag_tuple)

def minimal_flag_set(mn, constraint_type):
    """
    For a given lexical rule, use its set of constraints to find the
    minimal set of flags necessary to model the constraints.
    """
    all_flag_groups = []
    cs = ordered_constraints(mn, constraint_type)
    accounted_for = dict([(c.key, False) for c in cs])
    for c in cs:
        # a flag may have been accounted for by a disjunctive set. If so, skip.
        if accounted_for[c.key]: continue
        flag_group = {}
        # first add disjunctive sets
        for ds in mn.disjunctive_flag_sets.values():
            if c.key in ds:
                flag_group.update(ds)
        # nonseq are all nodes nonsequential with c (but may be with each other)
        nonseq = set([x for x in cs if not sequential(c, x)])
        # group only those items in nonseq that fulfill the following:
        # + are not preceded by an item that has not been accounted for
        # + are not accounted for themselves or are not followed by anything
        for x in nonseq:
            pre_x = set(x.input_span().values()).intersection(nonseq)
            if any([not accounted_for[y.key] for y in pre_x]) \
                    or (accounted_for[x.key] and any([x.precedes(y) for y in nonseq])):
                continue
            flag_group[x.key] = x
        # finally, account for flags in new flag group and store it
        if len(flag_group) != 0 and flag_group not in all_flag_groups:
            all_flag_groups += [flag_group]
            for x in flag_group.values():
                accounted_for[x.key] = True
    return all_flag_groups

def get_all_flags(out_or_in):
    flags = set()
    for mn in _mns.values():
        flags.update(set(mn.flags[out_or_in].keys()))
    return flags

def set_req_bkwd_initial_flags(lex_pc, flag_tuple):
    """
    Set the initial values for require-backward flags. Since these
    constraints need an initial value of na-or-- in order to work
    (otherwise + unifies with the default value of luk), we need to
    place these at the earliest possible spot (lexical types).
    """
    # the initial flag values are set on the minimal set of types by
    # first percolating the value down, then back up the hierarchy
    def validate_flag(x):
        if x.flags['out'].get(flag_tuple) != '+':
            x.flags['out'][flag_tuple] = 'na-or--'
    for root in lex_pc.roots():
        root.percolate_down(items=lambda x:x.flags['out'],
                            validate=lambda x: validate_flag(x))
    to_remove = defaultdict(set)
    for root in lex_pc.roots():
        root.percolate_up(items=lambda x: x.flags['out'], redundancies=to_remove)
    # don't forget to remove redundant items
    for child in to_remove.keys():
        for item in to_remove[child]:
            # recall common_items are dict items, so a (key, value) pair
            del child.flags['out'][item[0]]

### SUPERTYPES ###

# add possible supertypes here
# CMC 2017-05-16: Simplifying and clarifying these lists.
# Based on input from goodmami, the intent of these lists is
# * LEX_RULE_SUPERTYPES should be all supertypes that can be specified
#   by the customization system, i.e., if some part of the code here
#   can choose it as a supertype, it should go on this list.
# * ALL_LEX_RULE_SUPERTYPES should be the union of LEX_RULE_SUPERTYPES
#   and the three generic 'supertypes of last resort'.
# Changed the code to make the relationship explicit.
LEX_RULE_SUPERTYPES = ['cat-change-only-lex-rule',
                       'same-agr-lex-rule',
                       'cont-change-only-lex-rule',
                       'add-only-no-rels-hcons-rule',
                       'add-only-no-ccont-rule',
                       'val-and-cont-change-lex-rule',
                       'add-only-rule',
                       'same-head-lex-rule',
                       'val-change-only-lex-rule',
                       'head-change-only-lex-rule',
                       'cont-change-only-lex-rule',
                       'high-or-mid-nominalization-lex-rule',
                       'mid-nominalization-lex-rule',
                       'low-nmz-no-subjid-trans-lex-rule',
                       'low-nmz-no-subjid-compsid-lex-rule',
                       'low-nmz-subjid-trans-lex-rule',
                       'low-nmz-subjid-compsid-lex-rule']

ALL_LEX_RULE_SUPERTYPES = LEX_RULE_SUPERTYPES + ['infl-lex-rule',
                                                 'const-lex-rule',
                                                 'lex-rule']

# TJT 2014-12-19: Make these sets for performance
ALL_LEX_RULE_SUPERTYPES = set(ALL_LEX_RULE_SUPERTYPES)
LEX_RULE_SUPERTYPES = set(LEX_RULE_SUPERTYPES)

def set_lexical_rule_supertypes(lrt, mtx_supertypes):
    """
    Assign initial supertypes to lexical rule types. These can be inferred
    from the lexical rule instances and feature types.
    """
    lrt.supertypes.update(mtx_supertypes)
    # if there exists a non-empty lri, give it an infl supertype
    if len(lrt.lris) > 0:
        if any([len(lri) > 0 for lri in lrt.lris]):
            lrt.supertypes.add('infl-lex-rule')
        else:
            lrt.supertypes.add('const-lex-rule')

            # feature-based supertypes
            # JDC 04 June 2012 now negation supertypes are set in features.py
            #  if ('value', 'a') in lrt.features.get('negation',{}).items():
            #    lrt.supertypes.add('cont-change-only-lex-rule')
            # add other special cases here

def calculate_supertypes(pch):
    # calculate daughter types first, because we want to percolate them
    calculate_daughter_types(pch)
    for pc in pch.nodes.values():
        percolate_supertypes(pc)
        # in the case a lex-rule PC has no supertypes, give it a generic one
        if not any(st in ALL_LEX_RULE_SUPERTYPES for st in pc.supertypes):
            pc.supertypes.add('lex-rule')

def calculate_daughter_types(pch):
    inp_map = get_input_map(pch)
    for inp_set in inp_map:
        pcs = inp_map[inp_set]
        if len(inp_set) == 1:
            dtr_name = inp_set[0].identifier()
        # if there are multiple inputs, create an intermediate rule
        elif len(inp_set) > 1:
            dtr_name = disjunctive_typename(pcs) + '-rule-dtr'
            # each input should inherit from the intermediate type
            for inp in inp_set:
                inp.supertypes.add(dtr_name)
            # Add to the global daughter set so it gets written later
            _dtrs.add(dtr_name)
        for pc in pcs:
            pc.daughter_type = dtr_name

def percolate_supertypes(pc):
    # First percolate supertypes down to leaves. Before percolating them
    # back up, all must be pushed down, so this loop must be separate
    # from the next one.
    def validate_supertypes(x):
        if pc.is_lex_rule:
            if not any(st in LEX_RULE_SUPERTYPES for st in x.supertypes):
                # if str(pc.identifier()) in _infostr_pc.values():
                #   x.supertypes.add('add-only-no-rels-hcons-rule')
                if pc.has_incorporated_stems():
                    # TJT 2014-08-21: Incorporated Adjective lexical rule supertypes
                    x.supertypes.add('add-only-rule')
                    x.supertypes.add('adj_incorporation-lex-rule')
                elif pc.has_valchg_ops():
                    # CMC 2017-02-20: Valence-changing operations need
                    # less-constrained supertype
                    x.supertypes.add('val-change-with-ccont-lex-rule')
                elif pc.has_evidential():
                    # MTH 2017-11-13: Lexical rule types that contribute
                    # evidential semantics must be able to add a predicate
                    # in CCONT
                    x.supertypes.add('cont-change-only-lex-rule')
                else:
                    x.supertypes.add('add-only-no-ccont-rule')

    for r in pc.roots():
        r.percolate_down(items=lambda x: x.supertypes,
                         validate=lambda x: validate_supertypes(x))
    # percolate up also starts at the roots, since it's recursive
    to_remove = defaultdict(set)
    for r in pc.roots():
        r.percolate_up(items=lambda x: x.supertypes, redundancies=to_remove)
    # now we have to remove redundant items
    for child in to_remove.keys():
        child.supertypes.difference_update(to_remove[child])
    # since we don't use the pc node of lexical types, only do the
    # following if it is a lex rule
    if not pc.is_lex_rule: return
    # If there are supertypes common to all roots, then copy it up to
    # the PC. If not, we probably need to give the PC a generic type.
    # Also, the backup value for root_sts is to avoid a TypeError caused
    # by reducing over an empty sequence.
    common_sts = reduce(set.intersection,
                        [r.supertypes for r in pc.roots()] or [set()])
    if common_sts:
        # update the supertype sets
        pc.supertypes.update(common_sts)
        for r in pc.roots():
            r.supertypes.difference_update(common_sts)
    elif len(pc.supertypes) == 0:
        pc.supertypes.add('lex-rule')

######################
### OUTPUT METHODS ###
######################

def get_infostr_constraint(k, cur):
    if not _supertypes.has_key(k): return
    for st in _supertypes[k]:
        if _infostr_head.has_key(cur) and _infostr_head.has_key(st):
            for h in _infostr_head[st]:
                if h not in _infostr_head[cur]: _infostr_head[cur].append(h)
                get_infostr_constraint(st, k)

def get_infostr_constraints(choices):
    for i, pc in enumerate(all_position_classes(choices)):
        for j, lrt in enumerate(pc.get('lrt')):
            _supertypes[lrt.full_key] = lrt.split_value('supertypes')
            for st in _supertypes[lrt.full_key]:
                if st not in _nonleaves:
                    _nonleaves.append(st)
            if not _infostr_head.has_key(lrt.full_key):
                _infostr_head[lrt.full_key] = []
            for feat in lrt.get('feat'):
                if feat['name'] == "information-structure meaning":
                    if lrt.full_key not in _infostr_lrt:
                        _infostr_lrt.append(lrt.full_key)
                    if feat.get('head') in ['subj', 'obj', 'verb']:
                        _infostr_head[lrt.full_key].append(feat.get('head'))
    for i, pc in enumerate(all_position_classes(choices)):
        for j, lrt in enumerate(pc.get('lrt')):
            get_infostr_constraint(lrt.full_key, lrt.full_key)


def write_rules(pch, mylang, irules, lrules, lextdl, choices):
    # Set up irules.tdl
    irules.define_sections([['regular','Inflecting Lexical Rule Instances',False,False],
                            ['incorp','Incorporated Stem Lexical Rule Instances',False,False]])
    # Set up inflectional flags
    get_infostr_constraints(choices)
    all_flags = get_all_flags('out').union(get_all_flags('in'))
    write_inflected_avms(mylang, all_flags)
    mylang.set_section('lexrules')
    # First write any intermediate types (keep them together)
    write_intermediate_types(mylang)
    mylang.add_literal(';;; Lexical rule types')
    for pc in sorted(pch.nodes.values(), key=lambda x: x.tdl_order):
        # set the appropriate section
        mylang.set_section(get_section_from_pc(pc))
        # if it's a lexical type, just write flags and move on
        if not pc.is_lex_rule:
            write_pc_flags(mylang, lextdl, pc, all_flags, choices)
            for lt in pc.nodes.values():
                write_supertypes(mylang, lt.identifier(), lt.supertypes)
            continue
        # only lexical rules from this point
        write_supertypes(mylang, pc.identifier(), pc.supertypes)
        write_pc_flags(mylang, lextdl, pc, all_flags, choices)
        for lrt in sorted(pc.nodes.values(), key=lambda x: x.tdl_order):
            write_i_or_l_rules(irules, lrules, lrt, pc.order)
            # TJT 2014-08-27: Write adjective position class features
            write_pc_adj_syntactic_behavior(lrt, mylang, choices)
            # CMC 2017-03-28 Write valence change operations rules
            write_valence_change_behavior(lrt, mylang, choices)
            # MTH 2017-10-16 Write evidential behavior
            write_evidential_behavior(lrt, mylang, choices, pc.has_evidential())
            # CMC 2017-04-07 moved merged LRT/PCs handling to write_supertypes
            write_supertypes(mylang, lrt.identifier(), lrt.all_supertypes())
        write_daughter_types(mylang, pc)
    # features need to be written later
    return [(mn.key, mn.identifier(), mn.key.split('-')[0])
            for mn in _mns.values()
            if isinstance(mn, LexicalRuleType) and len(mn.features) > 0]

def write_intermediate_types(mylang):
    if _dtrs:
        mylang.add_literal(';;; Intermediate rule types')
        for dtr in _dtrs:
            mylang.add('''%(dtr)s := word-or-lexrule.''' % {'dtr': dtr}, one_line=True)

def get_section_from_pc(pc):
    """
    Given a PC, return the section in which its rules should be written.
    Note: needs to be updated with future POS types (adverbs, prepositions, etc.)
    """
    if not pc.is_lex_rule:
        # get section for lexical type
        if 'noun' in pc.key:
            return 'nounlex'
        elif 'verb' in pc.key:
            return 'verblex'
        elif 'adj' in pc.key:
            return 'adjlex'
        else:
            return 'otherlex'
    else:
        # get section for lexical rule
        if '-dir-inv' in pc.name:
            return 'dirinv'
        else:
            return 'lexrules'

def write_supertypes(mylang, identifier, supertypes=None):
    if supertypes is not None and len(supertypes) > 0:
        # CMC 2017-04-07 Handling for merged LRT/PCs: omit (same) identifier from list of supertypes written
        mylang.add('''%(id)s := %(sts)s.''' % \
                   {'id': identifier, 'sts': ' & '.join(sorted([st for st in supertypes if st != identifier]))})

def write_daughter_types(mylang, pc):
    """
    Find the proper value for each position class's DTR, creating
    intermediate rule types when necessary.
    """
    if pc.is_lex_rule:
        mylang.add('''%(id)s := [ DTR %(dtr)s ].''' % \
                   {'id':pc.identifier(), 'dtr': pc.daughter_type})

def write_inflected_avms(mylang, all_flags):
    mylang.set_section('addenda')
    for f in all_flags:
        flag = flag_name(f)
        mylang.add('''inflected :+ [%(flag)s luk].''' % {'flag': flag})
        mylang.add('''infl-satisfied :+ [%(flag)s na-or-+].''' % {'flag': flag})

def write_pc_flags(mylang, lextdl, pc, all_flags, choices):
    """
    Go down the PC hierarchy and write input and output flags. If no
    output flags have been written, copy up all flags. Otherwise, copy
    up the flags that don't occur as output flags.
    """
    if len(all_flags) == 0: return
    write_flags(mylang, pc)
    out_flags = set(pc.flags['out'].keys())
    to_copy = {}
    for mn in pc.roots():
        to_copy[mn.key] = write_mn_flags(mylang, lextdl, mn, out_flags, all_flags,
                                         choices)
    # for lex-rule PCs (not lexical types), write copy-up flags
    if pc.is_lex_rule:
        # first write copy-ups for the root nodes
        copied_flags = write_copy_up_flags(mylang, to_copy, all_flags)
        # then, if any remain, copy up on the pc (if a lexrule)
        to_copy = {pc.key: all_flags.difference(out_flags.union(copied_flags))}
        write_copy_up_flags(mylang, to_copy, all_flags, force_write=True)

def write_mn_flags(mylang, lextdl, mn, output_flags, all_flags, choices):
    if mn.instance:
        write_flags(lextdl, mn)
    else:
        write_flags(mylang, mn)
    to_copy = {}
    cur_output_flags = output_flags.union(set(mn.flags['out'].keys()))
    for sub_mn in mn.children().values():
        to_copy[sub_mn.key] = write_mn_flags(mylang, lextdl, sub_mn,
                                             cur_output_flags, all_flags, choices)
    copied_flags = write_copy_up_flags(mylang, to_copy, all_flags)
    return all_flags.difference(cur_output_flags).difference(copied_flags)

def write_flags(tdlfile, mn):
    if len(mn.flags['in']) + len(mn.flags['out']) == 0:
        return
    flag_strs = []
    if len(mn.flags['in']) > 0:
        flag_strs += ['DTR.INFLECTED [ ' + \
                      ', '.join(flag_name(flag) + ' ' + mn.flags['in'][flag]
                                for flag in mn.flags['in']) + ' ]']
    if len(mn.flags['out']) > 0:
        flag_strs += ['INFLECTED [ ' + \
                      ', '.join(flag_name(flag) + ' ' + mn.flags['out'][flag]
                                for flag in mn.flags['out']) + ' ]']
    tdl_str = mn.identifier() + ' := [ ' + ', '.join(flag_strs) + ' ].'
    tdlfile.add(tdl_str)

def write_copy_up_flags(mylang, to_copy, all_flags, force_write=False):
    copied_flags = set()
    if len(to_copy) == 0: return copied_flags
    common_flags = reduce(set.intersection, to_copy.values())
    for mn_key in to_copy:
        mn = _mns[mn_key]
        if mn.identifier_suffix in ('lex-super', 'lex', ''): continue
        # if all flags are common, none are copied here, and if the
        # difference contains all flags, just copy up the whole AVM.
        mn_copy_flags = to_copy[mn_key]
        if not force_write:
            mn_copy_flags.difference_update(common_flags)
        if mn_copy_flags == all_flags:
            mylang.add(mn.identifier() + ''' := [ INFLECTED #infl,
                                            DTR.INFLECTED #infl ].''')
        elif len(mn_copy_flags) > 0:
            flag_tags = [(flag_name(flag), disjunctive_typename(flag).lower())
                         for flag in mn_copy_flags]
            tdl_str = mn.identifier() + ' := [ ' + \
                      'INFLECTED [ ' + \
                      ', '.join(['%(flag)s #%(tag)s' % {'flag':ft[0], 'tag':ft[1]}
                                 for ft in flag_tags]) + ' ], ' + \
                      'DTR.INFLECTED [ ' + \
                      ', '.join(['%(flag)s #%(tag)s' % {'flag':ft[0], 'tag':ft[1]}
                                 for ft in flag_tags]) + ' ] ].'
            mylang.add(tdl_str)
        copied_flags.update(mn_copy_flags)
    return copied_flags

def write_i_or_l_rules(irules, lrules, lrt, order):
    if len(lrt.lris) == 0: return
    if any(len(lri) > 0 for lri in lrt.lris):
        # inflectional rules
        if order.lower() in ('prefix', 'before'):
            order = 'prefix'
        elif order.lower() in ('suffix', 'after'):
            order = 'suffix'
        # if there's only one LRI don't give the rule a number
        num = [''] if len(lrt.lris) == 1 else range(1, len(lrt.lris) + 1)
        for i, lri in enumerate(lrt.lris):
            # TJT 2014-08-20: Adding incorporated adjective stems
            # TJT 2014-12-21: Adding sections to irules.tdl
            if lri.pred:
                pred = " &\n  [ C-CONT.RELS.LIST.FIRST.PRED \"%s\" ]" % lri.pred
                section = "incorp"
            else:
                pred = ''
                section = "regular"
            rule = '\n'.join(['-'.join([lrt.name, order + str(num[i])]) + ' :=',
                              r'%' + order + ' (* ' + lri.name + ')',
                              lrt.identifier() + pred]) + '.'
            irules.add_literal(rule, section=section)
    else:
        # lexical rules # TJT 2014-12-21: cleaning this up
        lrt_id = lrt.identifier()
        lrules.add(lrt_id.rsplit('-rule',1)[0] + ' := ' + lrt_id + '.')

def write_evidential_behavior(lrt, mylang, choices, pc_evidential):
    EVIDENTIAL_LEX_RULE = '''evidential-lex-rule := cont-change-only-lex-rule &
  same-spr-lex-rule &
  same-spec-lex-rule &
[ C-CONT [ RELS <! event-relation &
           [ LBL #ltop,
           ARG0 event,
           ARG1 #harg ] !>,
       HCONS <! qeq & [ HARG #harg,
                LARG #larg ] !>,
       HOOK [ LTOP #ltop,
          INDEX #mainev,
          XARG #mainagent ] ],
  DTR.SYNSEM.LOCAL.CONT.HOOK [ LTOP #larg,
                          XARG #mainagent,
                          INDEX #mainev ] ].
'''
    if lrt.evidential:
        lrt.supertypes.add(lrt.evidential + '-evidential-lex-rule')
        prev_section = mylang.section
        mylang.set_section('lexrules')
        mylang.add(EVIDENTIAL_LEX_RULE)
        infl_evid_def = lrt.evidential + '''-evidential-lex-rule := evidential-lex-rule & 
        [ C-CONT.RELS <! [ PRED "ev_''' + lrt.evidential + '''_rel" ] !> ].
        '''
        mylang.add(infl_evid_def)
        mylang.set_section(prev_section)
    elif pc_evidential:
        lrt.supertypes.add("add-only-no-ccont-rule")

def write_valence_change_behavior(lrt, mylang, choices):
    from gmcs.linglib.valence_change import lexrule_name
    for op in lrt.valchgops:
        operation = op.get('operation','').lower()

        # NB: non-scopal always need same-cont added!
        if operation != 'subj-add':
            lrt.supertypes.add('same-cont-lex-rule')

        if operation == 'subj-rem':
            lrt.supertypes.add('local-change-only-lex-rule')
            mylang.add(lrt.identifier() + ' := ' + lexrule_name('subj-rem-op') + '.') #'''subj-rem-op-lex-rule.''')
        elif operation == 'obj-rem':
            lrt.supertypes.add('local-change-only-lex-rule')
            mylang.add(lrt.identifier() + ' := ' + lexrule_name('obj-rem-op') + '.') #obj-rem-op-lex-rule.''')
        elif operation == 'obj-add':
            if op['argpos'].lower() == 'pre':
                lrt.supertypes.add(lexrule_name('added-arg-applicative', 2, 3)) #'added-arg2of3-applicative-lex-rule')
                lrt.supertypes.add(lexrule_name('added-arg-head-type', 2, op['argtype'].lower()))
            else:
                lrt.supertypes.add(lexrule_name('added-arg-applicative', 3, 3)) #'added-arg3of3-applicative-lex-rule')
                lrt.supertypes.add(lexrule_name('added-arg-head-type', 3, op['argtype'].lower()))
            predname = op.get('predname','undef_pred')
            mylang.add(lrt.identifier() + ' := [ C-CONT.RELS <! [ PRED "' + predname +'" ] !> ].')
        elif operation.lower() == 'subj-add':
            lrt.supertypes.add('causative-lex-rule')
            predname = op.get('predname','causative_rel')
            mylang.add(lrt.identifier() + ' := [ C-CONT.RELS <! [ PRED "' + predname + '" ] !> ].')

def write_pc_adj_syntactic_behavior(lrt, mylang, choices):
    # TODO: Don't do this if a supertype is specified
    if 'mod' in lrt.features:
        if lrt.features['mod'] in ('both', 'attr'):
            # Basic attributive behavoir
            mylang.add(lrt.identifier() + " := attr-adj-lex-rule.")
            # Attributive only
            if lrt.features['mod'] == "attr":
                mylang.add(lrt.identifier() + ''' := attr-adj-lex-rule &
                     [ SYNSEM.LOCAL.CAT [ VAL.SUBJ < >,
                                          HEAD.PRD - ] ].''')
            # Modification direction
            modpos = choices.get(lrt.key+'_modpos','')
            # Options are adjective modifying nouns "before the adjective",
            # "after the adjective", or "either position"
            if modpos in ('before','after'):
                posthead = {'before':'+', 'after':'-'}[modpos]
                mylang.add(lrt.identifier() + (' := [ SYNSEM.LOCAL.CAT.POSTHEAD %s ].' % posthead))
        if lrt.features['mod'] in ('both', 'pred'):
            if lrt.features['mod'] == "pred":
                # Predicative only
                mylang.add(lrt.identifier() + " := [ SYNSEM.LOCAL.CAT.HEAD.MOD < > ].")
                #elif lrt.features['mod'] == "both":
                # Do nothing... gets PRD or stative predicate from below
            # TJT 2014-08-27: Making 'predcop' dependent on 'mod: pred or both'
            if 'predcop' in lrt.features:
                # This is the copula complement LRT
                mylang.add(lrt.identifier() + ''' := [ SYNSEM.LOCAL.CAT [ HEAD.PRD +
                                                                  VAL.SUBJ < > ] ].''')
            else:
                # This is the stative predicate LRT
                # This only fires if "mod" is ("both" or "pred") and "pred" not checked
                lrt.supertypes.add('stative-pred-lex-rule')
                # TJT: 2014-09-24: Stative predicate lexical rule is PRD -
                mylang.add(lrt.identifier() + ''' := [ SYNSEM.LOCAL.CAT.HEAD.PRD - ].''')

##################
### VALIDATION ###
##################

def validate(choices, vr):
    index_feats = choices.index_features()
    # TJT 2014-09-03: Generators are exhausted after use, so make this a list!
    all_pcs = list(all_position_classes(choices))
    warn_merged_pcs(all_pcs, vr)
    for pc in all_pcs:
        basic_pc_validation(choices, pc, vr)
        cooccurrence_validation(pc, choices, vr)
        hierarchy_validation(choices, pc, vr)
        # TJT 2014-09-04: Calculate switching inputs
        switching = pc.get('switching',False)
        pc_switching_inputs = set()
        if pc.get('switching',''):
            inputs = pc.get('inputs',[]).split(', ')
            if isinstance(inputs, basestring):
                pc_switching_inputs.add(inputs)
            else: # assume list
                pc_switching_inputs.update(inputs)
        for lrt in pc.get('lrt', []):
            lrt_validation(lrt, vr, index_feats, choices, inputs=pc_switching_inputs, switching=switching)
        # TJT 2014-08-21: Validate incorporated stems
        for lrt in pc.get('is-lrt', []):
            lrt_validation(lrt, vr, index_feats, choices, incorp=True, inputs=pc_switching_inputs, switching=switching)
    cycle_validation(choices, vr)

def basic_pc_validation(choices, pc, vr):
    # Lexical rule types need order and inputs specified
    if not 'order' in pc:
        vr.err(pc.full_key + '_order',
               'You must specify an order for every position class you define.')
    if not pc.get('inputs',''):
        # TJT 2014-09-01: Changing this to an error as the system crashes without an input
        vr.err(pc.full_key + '_inputs',
               #'A position class without any inputs is unusable unless you ' +\
               #'define inputs during hand-development of the grammar.')
               'Each position class must have at least one input defined.')
    else:
        # All user-defined inputs must be defined
        if any(inp not in choices and inp not in LEXICAL_SUPERTYPES
               for inp in pc.get('inputs','').split(', ')):
            vr.err(pc.full_key + '_inputs',
                   'Every lexical type, lexical rule type, or position class ' + \
                   'that serves as the input to a position class must be ' + \
                   'defined somewhere in the questionnaire.')
    # ALL inputs must be defined: if input is set to "Any X", and a POS type
    # of that X is not defined, the system fails but there is no validation

    # LLD 2015-12-09: modified to check for ALL_LEX_TYPES, so we check that verb,
    # aux, noun, etc. are defined if needed (not just det, adj, cop).
    #if any(inp in NON_ESSENTIAL_LEX_CATEGORIES  and inp not in choices
    #        for inp in pc.get('inputs','').split(', ')):
    if any(inp in ALL_LEX_TYPES and inp not in choices
           for inp in pc.get('inputs','').split(', ')):
        vr.err(pc.full_key + '_inputs',
               'You have specified morphology for a part of speech ' + \
               'that does not have any lexical types defined. You ' + \
               'can define lexical types on the Lexicon page.')
    # Check for 0 or 1 LRTs, and warn appropriately
    # TJT 2014-08-20: At least one LRT of IS-LRT type is required
    if ('lrt' not in pc or len(pc.get('lrt',[])) == 0) and \
            ('is-lrt' not in pc or len(pc.get('is-lrt',[])) == 0):
        vr.warn(pc.full_key + '_lrt', 'A position class without any defined ' + \
                'lexical rule types is unusable, though it can be later ' + \
                'defined by hand.')
    elif len(pc.get('lrt', [])) == 1:
        lrt = pc['lrt'].get_first()
        if lrt.get('name', '') == '' or pc.get('name', '') == '':
            # if the lrt has no name, it will be merged with its position class.
            # make sure it has no constraints
            for c in lrt.get('require', []) + lrt.get('forbid', []):
                vr.err(c.full_key + '_others', 'Solitary lexical rule types with ' + \
                       'no name will be merged with their position class, and ' + \
                       'therefore cannot themselves take constraints. Apply the ' + \
                       'constraints to the position class, instead.')
    # TJT 2014-09-18: PCs should be either incorporated stems or inflection...
    # this seems true from adjective typology survey, but might not be right
    # for future incorporated stems
    if 'is-lrt' in pc and 'lrt' in pc:
        vr.err(pc.full_key+'_lrt', 'Each position class should either have ' + \
               'incorporated stems or regular lexical rule types. If your ' + \
               'language has incorporated stems and lexical rules types ' + \
               'in a minimal pair, let the developers know!')

# TJT 2014-08-21: incorp argument for incorporated stem lexical rule validation
def lrt_validation(lrt, vr, index_feats, choices, incorp=False, inputs=set(), switching=False):
    # No supertype means it's a root type within a PC class (no longer an error)
    #if 'supertypes' not in lrt:
    #  vr.err(lrt.full_key + '_supertypes',
    #         'You must select a supertype for every lexical rule type.')
    # any features on an LR need a name and value (and head for verbs)
    for feat in lrt.get('feat', []):
        if 'name' not in feat:
            vr.err(feat.full_key + '_name',
                   'You must choose which feature you are specifying.')
        if 'value' not in feat:
            vr.err(feat.full_key + '_value',
                   'You must choose a value for each feature you specify.')
        # TJT 2014-08-22: check head for adjectives and incorporated stems
        if lrt.full_key.startswith('verb-pc') or \
                lrt.full_key.startswith('adj-pc') or \
                        'is-lrt' in lrt.full_key:
            if 'head' not in feat:
                vr.err(feat.full_key + '_head',
                       'You must choose where the feature is specified.')
            elif feat['head'] in ['higher', 'lower'] and not choices.get('scale'):
                vr.err(feat.full_key + '_head',
                       'To use higher/lower ranked NP, please define a scale on the direct-inverse page.')
            elif feat['head'] == 'verb' and feat.get('name','') in index_feats:
                vr.err(feat.full_key + '_head',
                       'This feature is associated with nouns, ' + \
                       'please select one of the NP options.')

        # MTH 2017-11-27: check to make sure that only one evidential value is selected
        if feat['name'] == 'evidential' and len(feat.get('value').split(',')) > 1:
            vr.err(feat.full_key + '_value',
                   'Choose only one evidential term.')

    # TJT 2015-02-02: Any given LRT should be either inflecting or non-inflecting
    inflecting_count = len(filter(None, [lri.get('inflecting')=="yes" for lri in lrt.get('lri',[])]))
    if inflecting_count not in (0, len(lrt.get('lri',[]))):
        vr.err(lrt.full_key + '_name',
               'Any given Lexical Rule Type should contain either inflecting Lexical Rule Instances ' + \
               'or non-inflecting Lexical Rule Instances.')
    orths = set()
    for lri in lrt.get('lri', []):
        orth = lri.get('orth', '')
        if lri['inflecting'] == 'yes' and orth == '':
            vr.err(lri.full_key + '_orth',
                   "If an instance's spelling is not selected as None, " + \
                   "it cannot be blank.")
        elif lri['inflecting'] == 'no' and len(orth) > 0:
            vr.warn(lri.full_key + '_orth',
                    "If an instance's spelling is selected as None, " + \
                    "any defined spelling will not be used.")
        if orth in orths:
            vr.err(lri.full_key + '_orth',
                   "This affix duplicates another, which is not allowed.")
        orths.add(orth)

    # CMC 2017-04-07: Valence-changing operations validateion
    for vchop in lrt.get('valchg',[]):
        optype = vchop.get('operation','')
        if not optype:
            vr.err(vchop.full_key+'_operation','A valence-changing lexical rule must specify an operation.')
        elif optype == 'obj-add':
            if vchop.get('argpos') not in ['pre','post']:
                vr.err(vchop.full_key+'_argpos','An argument can only be added at the front ' + \
                       'or end of the complements list.')
            if vchop.get('argtype') not in ['np','pp']:
                vr.warn(vchop.full_key+'_argtype','The type of the added argument ({}) is unconstrained.'.format(vchop.get('argtype','')))
            if not vchop.get('predname'):
                vr.warn(vchop.full_key+'_predname','The added predicated should have a name specified.')

    # TJT 2014-08-21: Incorporated Adjective validation
    if incorp:
        for lri in lrt.get('lri', []):
            pred = lri.get('pred', '')
            if not pred:
                vr.err(lri.full_key+'_pred',
                       "Each Incorporated Stem instance must have a pred " + \
                       "value associated with it. If you do not require a " + \
                       "pred value, use a regular lexical rule type + instances.")
            if pred[-len("_a_rel"):] != "_a_rel":
                vr.warn(lri.full_key+'_pred',
                        "The Customization System currently only supports " + \
                        "adjectival incorporated stems. Note that this pred " + \
                        "value (%s) has not been defined as a \"_a_rel\" stem." % pred)

    # TJT 2014-09-04: Swithing position class validation
    if switching:
        mode = lrt.get('mod','')
        modpos = lrt.get('modpos','')
        predcop = lrt.get('predcop','off')

        # Mode or some input must have mode defined
        if not mode:
            if not inputs:
                vr.err(lrt.full_key+'_mod',
                       'Every adjective position class or one of its inputs must ' + \
                       'define a syntactic behavoir.')

        # Mode must not clash with any of its inputs
        for key in inputs:
            input_def = choices.get(key,False)
            if input_def:
                # Check mode
                if mode != 'both':
                    input_mode = input_def.get('mod','')
                    # Only pred and attr conflict with each other
                    if input_mode in ("pred", "attr") and mode != input_mode:
                        vr.err(lrt.full_key+'_mod',
                               'This behavior conflicts with this lexical rule type\'s ' + \
                               'input %s on the input\'s behavoir' % (input_def.get('name','')))
                # Check modpos
                if mode in ('attr', 'both'):
                    if modpos and modpos != 'either':
                        input_modpos = input_def.get('modpos',False)
                        if input_modpos:
                            if modpos != input_modpos:
                                vr.err(lrt.full_key+'_modpos',
                                       'This modification direction conflicts with this ' + \
                                       'lexical rule type\'s input %s' % (input_def.get('name','')))
                # Check predcop
                if mode in ('pred', 'both'):
                    if input_def.get('mod','') in ('pred', 'both'):
                        predcop_map = {'on':'obl', 'off':'imp'} # Converge type names
                        input_predcop = input_def.get('predcop','off')
                        input_predcop = predcop_map[input_predcop] if input_predcop in predcop_map else input_predcop
                        if input_predcop and input_predcop != 'opt':
                            type_predcop = predcop_map[predcop] if predcop in predcop_map else predcop
                            if type_predcop != input_predcop:
                                vr.err(lrt.full_key+'_predcop',
                                       'This copula complementation choice conflicts with this ' + \
                                       'lexical rule type\'s input %s' % (input_def.get('name','')))

        # Applicable choices for each mode must be made
        if mode in ('attr', 'both'):
            if not modpos:
                vr.err(lrt.full_key+'_modpos',
                       'Every position class able to be attributive must have a ' + \
                       'modification direction defined.')

        # Mode specific adjective choices are disregarded without the proper mode
        if mode in ('pred', 'attr'):
            inverse_mode_name = {'pred':'attributively', 'attr':'predicatively'}[mode]
            message = 'This choice is only applicable to adjectives behaving ' + \
                      ('%s. This choice will be ignored, or you ' % inverse_mode_name) + \
                      'can change the adjective\'s behavoir to enable this choice above.'
            if mode != 'attr':
                if modpos:
                    vr.warn(lrt.full_key+'_modpos', message)
            if mode != 'pred':
                if predcop == "on":
                    vr.warn(lrt.full_key+'_predcop', message)

        # Adjectives defined as a copula complement are unusuable without a copula defined
        if mode in ('pred', 'both'):
            if predcop == "on":
                if not choices.get('cop',False):
                    vr.warn(lrt.full_key+'_predcop',
                            'An adjective defined as a copula complement is ' + \
                            'unusable without a copula defined on the Lexicon page.')

    #KPH Validation for case change on nominalization rules
    for lri in lrt.get('lri'):
        for feat in lri.get('feat'):
            if feat.get('name') == 'case':
                vr.warn(lrt.full_key + '_lri', 'If case change is specified on the object ' + \
                       'in a nominalization rule, the resulting lexical rule will only be ' + \
                       'compatible with transitive verbs. If this lexical rule should also ' + \
                       'be possible for intransitive verbs, create another lexical rule that ' + \
                        'requires intransitives verbs as the input.')

def hierarchy_validation(choices, pc, vr):
    # LLD 2015-11-22 Supertype LRTs should not have non-affixing LRIs if subtype
    # LRTs have spelling-changing LRIs.

    # sts_dict is a dict of lrt names->lists of supertypes of LRTs that have spelling-changing LRIs
    # has_no_affix_lri is a set of lrts with a "No Affix" lri
    # has_affix_lri is a set of lrts with at least one affixing lri
    sts_dict = {}
    has_no_affix_lri = set()
    has_affix_lri = set()

    for lrt in pc.get('lrt', []):
        for lri in lrt.get('lri', []):
            if lri:
                if lri['inflecting'] == 'no':
                    has_no_affix_lri.add(lrt.full_key)
                if lri['inflecting'] == 'yes':
                    has_affix_lri.add(lrt.full_key)
                    sts = lrt.get('supertypes', '').split(", ")
                    if sts:
                        sts_dict[lrt.full_key] = sts
    for lrt, sts in sts_dict.items():
        for st in sts:
            if st in has_no_affix_lri:
                has_no_affix_lri.remove(st)  # because we won't need to warn about this LRT again
                vr.err(st + '_lri1_inflecting',
                       "A lexical rule type should not contain a 'no affix' lexical rule instance if it " + \
                       "is a supertype to a lexical rule type that applies an affix.")
            if st in has_affix_lri:
                has_affix_lri.remove(st)  # because we won't need to warn about this LRT again
                vr.warn(st + '_lri1_inflecting',
                        "This lexical rule type has both instances and subtypes, which will lead to " + \
                        "greater ambiguity in realization (generation). If that was not your intention, " + \
                        "consider moving the affixing lexical rule instance to a subtype LRT.")

    # LLD 2015-12-09 Building hierarchy validation similar to what is in lexicon.py
    lrtsts = {}
    feats = {}
    inherited_feats = {}

    for lrt in pc.get('lrt', []):
        sts = lrt.get('supertypes', '').split(", ")
        if sts:
            lrtsts[lrt.full_key] = sts
        feats[lrt.full_key] = {}
        for f in lrt.get('feat'):
            feats[lrt.full_key][f.get('head') + " " + f.get('name')] = f.get('value')

    # now to figure out inherited features, check for cycles, check for hierarchy issues
    for lrt in pc.get('lrt', []):
        st_anc = [] #used to check for subsumption errors
        seen = []
        paths = []
        for st in lrtsts[lrt.full_key]:
            paths.append([lrt.full_key, st])
        parents = lrtsts[lrt.full_key]
        inherited_feats[lrt.full_key] = {}
        while (True):
            next_parents = []
            for p in parents:
                if p:
                    ptype = choices.get(p)
                    if not ptype: continue
                    for f in feats[p]:
                        # see if this feature conflicts with what we know
                        if f in feats[lrt.full_key] and feats[p][f] != feats[lrt.full_key][f]:
                            # inherited feature conflicts with self defined feature
                            vr.warn(lrt.full_key + '_feat', "The feature defined here, \'"+f+"="+ \
                                    str(feats[lrt.full_key][f])+"\', may confict with the value"+ \
                                    " defined on the supertype "+ptype.get('name')+" ("+p+"). "+ \
                                    "It is up to you to make sure that these values are compatible.",
                                    concat=False)
                        elif f in inherited_feats[lrt.full_key] and \
                                        feats[p][f] != inherited_feats[lrt.full_key][f]:
                            vr.warn(lrt.full_key + '_supertypes',
                                    "This inherited feature value, \'" +f+"="+ \
                                    str(inherited_feats[lrt.full_key][f])+ \
                                    "\', may conflict with the value defined on the supertype "+ \
                                    ptype.get('name')+" ("+p+"). "+ \
                                    "It is up to you to make sure that these values are compatible.",
                                    concat=False)
                            inherited_feats[lrt.full_key][f] = "! "+ \
                                                               inherited_feats[lrt.full_key][f]+" && "+feats[p][f]
                        else:
                            inherited_feats[lrt.full_key][f] = feats[p][f]

                    # add sts to the next generation
                    to_be_seen = []
                    for r in paths:
                        if r[-1] == p: #this is the path to extend,
                            paths.remove(r)
                            for q in lrtsts[p]: #go through all sts
                                # q is a st_anc of the lrt
                                if q not in st_anc:
                                    st_anc.append(q)
                                if q in r:
                                    vr.err(lrt.full_key + '_supertypes', "This hierarchy "+
                                           "contains a cycle. The type "+q+" was found "+
                                           "at multiple points in the inheritance path: "+
                                           str(r+[q]))
                                else:
                                    new_path = r + [q]
                                    paths.append(new_path)
                                if (q != ''):
                                    if not (q in seen):
                                        next_parents.append(q)
                                        to_be_seen.append(q)
                    seen = seen + to_be_seen

            # if there aren't any next parents, we're done
            if len(next_parents) == 0:
                break

            # otherwise we go again
            parents = next_parents

        # Now check for the lkb err about vacuous inheritance using goodmami's
        #  method: find the intersection of supertypes and supertypes's ancestors
        for t in lrtsts[lrt.full_key]:
            if t in st_anc:
                vr.err(lrt.full_key + '_supertypes', "This LRT hierarchy contains a "+
                       "redundant link that will result in an LKB error.  "+t+
                       " is both an immediate supertype of "+lrt.full_key+" and also "+
                       "an ancestor of another supertype.")


# check for a cycle in the inputs
def cycle_validation(choices, vr):
    try:
        pch = position_class_hierarchy(choices)
    except KeyError:
        return # there is probably another error that validation will pick up
    for pc in pch.nodes.values():
        cyclic_inps = set([i.key for i in pc.input_span().values()
                           if pc.precedes(i) and i.precedes(pc)])
        if len(cyclic_inps) > 0:
            vr.err(pc.key + '_inputs', 'The inputs of this position class might ' + \
                   'cause a cycle. Please review: ' + ', '.join(cyclic_inps))


def cooccurrence_validation(lrt, choices, vr):
    # if A constrains B, A must precede or be preceded by B
    pass
    #  + forbidding something required
    #     If A requires B, then any node in A's PC may forbid B's
    #      subtypes (but not B or B's supertypes)
    #     (e.g. A > B > C, A req B & C, B forbids C, no other paths)
    #  + reqs violating explicit inputs
    #     (e.g. A > B > C, A > C, A reqs B)

# TJT 2014-08-26: Warn about merging obligatory position classes
# with same inputs and positions
def warn_merged_pcs(all_pcs, vr):
    input_map = defaultdict(lambda: defaultdict(set))
    # Gather map of inputs to position classes
    for pc in all_pcs:
        pc_name = pc.full_key
        order = pc.get('order','') # prefix or suffix
        inputs = pc.get('inputs',[])
        # Not sure why inputs is a string instead of a list...
        if isinstance(inputs, basestring):
            input_map[inputs][order].add(pc_name)
        else:
            for inp in pc.get('inputs',[]):
                input_map[inp][order].add(pc_name)
                # Warn for each obligatory position class with equal inputs and orders
                #   pcs_to_be_merged = {pc for inp in input_map
                #                       for order in input_map[inp]
                #                       for pc in input_map[inp][order]
                #                       if len(input_map[inp][order]) > 1}
    # TJT 2014-12-19: Converting above set comprehension
    # to loop for older python versions
    pcs_to_be_merged = set()
    for inp in input_map:
        for order in input_map[inp]:
            if len(input_map[inp][order]) > 1:
                for pc in input_map[inp][order]:
                    pcs_to_be_merged.add(pc)
    for pc in pcs_to_be_merged:
        difference = pcs_to_be_merged.copy()
        difference.remove(pc)
        if len(difference) == 1:
            warningString = "another position class"
        else: warningString = "other position classes"
        differenceString = ", ".join(difference)
        vr.warn(pc+'_inputs', # putting this on inputs...
                "This position class has the same inputs and order " + \
                "as %s (%s). " % (warningString, differenceString) + \
                "Therefore, they will be merged in the output grammar.")