from collections import defaultdict
from sets import Set as set

from gmcs.linglib import lexicon
from gmcs.linglib.lexbase import (MorphotacticNode, PositionClass,
                                  LexicalRuleType, LexicalType,
                                  ALL_LEX_TYPES,
                                  LEXICAL_CATEGORIES,
                                  LEXICAL_SUPERTYPES)
from gmcs.lib import Hierarchy
from gmcs.utils import get_name

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
# lst : lexical supertype (generic lts, like noun, verb, iverb, etc.)
# mn : morphotactic node (a pc, lrt, lt, or lst)

########################
### MODULE VARIABLES ###
########################

_mns = {}

########################
### HELPER FUNCTIONS ###
########################

def all_position_classes(choices):
  """ Yield each position class defined in the choices file. """
  for lt in ALL_LEX_TYPES:
    for pc in choices[lt + '-pc']:
      yield pc

def intermediate_typename(pcs):
  return disjunctive_typename(pcs) + '-rule-dtr'

def disjunctive_typename(mns):
  return '-or-'.join(sorted([mn.name for mn in mns]))

def flag_name(flag_tuple):
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
  inp_map = defaultdict(list)
  for pc in pch.nodes.values():
    i_s = tuple(sorted(pc.valid_inputs(), key=lambda x: x.tdl_order))
    if len(i_s) > 0:
      inp_map[i_s] += [pc]
  return inp_map

##########################
### MAIN LOGIC METHODS ###
##########################

def customize_inflection(choices, add_methods, mylang, irules, lrules):
  """
  Process the information in the given choices file and add the rules
  and types necessary to model the inflectional system into the irules,
  lrules, and primary grammar files.
  """
  # first call other libraries' add_lexrules methods to see if they
  # have anything to add to the choices file before we begin
  for method in add_methods:
    method(choices)
  # now create the hierarchy
  pch = customize_lexical_rules(choices)
  # write_rules currently returns a list of items needing feature
  # customization. Hopefully we can find a better solution
  return write_rules(pch, mylang, irules, lrules)

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
  #create_lexical_rule_types(choices)
  interpret_constraints(choices)
  convert_obligatoriness_to_req(choices)
  create_flags()
  return pch

### POSITION CLASSES AND LEXICAL RULE TYPES ###

def position_class_hierarchy(choices):
  """
  Create and return the data structures to hold the information
  regarding position classes and lexical types.
  """
  pch = Hierarchy()

  # Create PositionClasses for lexical types so they can take flags
  for lex_cat in LEXICAL_CATEGORIES:
    if lex_cat not in choices: continue
    lth = lexicon.lexical_type_hierarchy(choices, lex_cat)
    _mns[lth.key] = lth
    _mns.update(lth.nodes)
    pch.add_node(lth)

  # We can't set parents until we have created all MN objects.
  pc_inputs = {}
  # Now create the actual position classes
  for i, pc in enumerate(all_position_classes(choices)):
    if len(pc.get('inputs', '')) > 0:
      pc_inputs[pc.full_key] = set(pc.get('inputs').split(', '))
    cur_pc = pch.add_node(PositionClass(pc.full_key, get_name(pc),
                                        order=pc.get('order')))
    cur_pc.tdl_order = i
    _mns[cur_pc.key] = cur_pc
    # If there's only one LRT, and the PC or the LRT are unnamed, merge them
    if len(pc.get('lrt')) == 1:
      lrt = pc['lrt'].get_first()
      if '' in (pc.get('name',''), lrt.get('name','')):
        name = pc.get('name') or lrt.get('name') or pc.full_key
        lrt['name'] = cur_pc.name = name
        cur_pc.identifier_suffix = 'lex-rule'
    # Fill the lexical rule types with the information we know
    lrt_parents = {}
    for j, lrt in enumerate(pc.get('lrt')):
      if 'supertypes' in lrt:
        lrt_parents[lrt.full_key] = set(lrt.get('supertypes').split(', '))
      # default name uses name of PC with _lrtX
      if 'name' not in lrt:
        lrt['name'] = cur_pc.name + lrt.full_key.replace(cur_pc.key, '', 1)
      cur_lrt = create_lexical_rule_type(lrt)
      # the ordering should only mess up if there are 100+ lrts
      cur_lrt.tdl_order = i + (0.01 * j)
      cur_pc.add_node(cur_lrt)
    for child in lrt_parents:
      for parent in lrt_parents[child]:
        cur_pc.relate_parent_child(_mns[parent], _mns[child])
    # With knowledge of the hierarchy, determine the appropriate
    # supertypes, then try to push common supertypes up to reduce
    # redundancy in TDL
    set_lexical_rule_supertypes(cur_pc)
    cur_pc.percolate_supertypes()
  # now assign pc inputs
  for pc in pc_inputs:
    for inp in pc_inputs[pc]:
      _mns[pc].relate(_mns[inp], 'parent')
  return pch

def create_lexical_rule_type(lrt):
  new_lrt = LexicalRuleType(lrt.full_key, get_name(lrt))
  for feat in lrt.get('feat'):
    new_lrt.features[feat['name']] = {'value': feat['value'],
                                      'head': feat.get('head')}
  new_lrt.lris = [lri.get('orth','') for lri in lrt.get('lri',[])]
  # if there exists a non-empty lri, give it an infl supertype
  if len(new_lrt.lris) > 0:
    if any([len(lri) > 0 for lri in new_lrt.lris]):
      new_lrt.supertypes.add('infl-lex-rule')
    else:
      new_lrt.supertypes.add('const-lex-rule')
  _mns[new_lrt.key] = new_lrt
  return new_lrt

def set_lexical_rule_supertypes(pc):
  # since we will later percolate up common supertypes, for now
  # just put them on all nodes with LRIs (mainly leaf nodes)
  nodes = [n for n in pc.nodes.values() if len(n.lris) > 0]
  for lrt in nodes:
    # NOTE: there is currently no check to ensure that
    # cont-change-only-lex-rule and add-only-no-ccont-rule won't
    # appear on the same LRT.
    if ('value', 'plus') in lrt.features.get('negation',{}).items():
      lrt.supertypes.add('cont-change-only-lex-rule')
    else:
      lrt.supertypes.add('add-only-no-ccont-rule')

### CONSTRAINTS ###

def interpret_constraints(choices):
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

def minimal_flag_set(mn, constraint_type):
  """
  For a given lexical rule, use its set of constraints to find the
  minimal set of flags necessary to model the constraints.
  """
  all_flag_groups = []
  cs = ordered_constraints(mn, constraint_type)
  accounted_for = dict([(c.key, False) for c in cs])
  for c in cs:
    flag_group = {}
    if accounted_for[c.key]: continue
    # first add disjunctive sets
    for ds in mn.disjunctive_flag_sets.values():
      if c in ds:
        flag_group.update(ds)
        for x in ds.keys():
          accounted_for[x] = True
    # nonseq are all nodes nonsequential with c (but may be with each other)
    nonseq = set([x for x in cs if not sequential(c, x)])
    # group only those items in nonseq that fulfill the following:
    # + are not preceded by an item that has not been accounted for
    # + are not accounted for themselves or are not followed by anything
    for x in nonseq:
      pre_x = set(x.input_span().values()).intersection(nonseq)
      if (any([not accounted_for[y.key] for y in pre_x])) \
         or (accounted_for[x.key] \
             and any([x.precedes(y) for y in nonseq])):
        continue
      flag_group[x.key] = x
      accounted_for[x.key] = True
    #flag_group = tuple(sorted(flag_group))
    if len(flag_group) != 0 and flag_group not in all_flag_groups:
      all_flag_groups += [flag_group]
  return all_flag_groups

def get_all_flags(out_or_in):
  flags = set()
  for mn in _mns.values():
    flags.update(set(mn.flags[out_or_in].keys()))
  return flags

######################
### OUTPUT METHODS ###
######################

def write_rules(pch, mylang, irules, lrules):
  all_flags = get_all_flags('out').union(get_all_flags('in'))
  write_inflected_avms(mylang, all_flags)
  mylang.set_section('lexrules')
  for pc in sorted(pch.nodes.values(), key=lambda x: x.tdl_order):
    # set the appropriate section
    mylang.set_section(get_section_from_pc(pc))
    # if it's a lexical type, just write flags and move on
    if pc.identifier_suffix == 'lex-super':
      write_pc_flags(mylang, pc, all_flags)
      continue
    # only lexical rules from this point
    write_supertypes(mylang, pc.identifier(), pc.supertypes)
    write_pc_flags(mylang, pc, all_flags)
    for lrt in sorted(pc.nodes.values(), key=lambda x: x.tdl_order):
      write_i_or_l_rules(irules, lrules, lrt, pc.order)
      # merged LRT/PCs have the same identifier, so don't write supertypes here
      if lrt.identifier() != pc.identifier():
        write_supertypes(mylang, lrt.identifier(), lrt.all_supertypes())
  write_daughter_types(mylang, pch)
  # features need to be written later
  return [(mn.key, mn.identifier(), mn.key.split('-')[0])
          for mn in _mns.values()
          if isinstance(mn, LexicalRuleType) and len(mn.features) > 0]

def get_section_from_pc(pc):
  if pc.identifier_suffix == 'lex-super':
    # get section for lexical type
    if 'noun' in pc.key:
      return 'nounlex'
    elif 'verb' in pc.key:
      return 'verblex'
    else:
      return 'otherlex'
  else:
    # get section for lexical rule
    if '-dir-inv' in pc.name:
      return 'dirinv'
    else:
      return 'lexrules'

def write_supertypes(mylang, identifier, supertypes=None):
  if supertypes is not None:
    mylang.add('''%(id)s := %(sts)s.''' %\
               {'id': identifier, 'sts': ' & '.join(sorted(supertypes))})

def write_daughter_types(mylang, pch):
  """
  Find the proper value for each position class's DTR, creating
  intermediate rule types when necessary.
  """
  inp_map = get_input_map(pch)
  for inp_set in inp_map:
    pcs = inp_map[inp_set]
    dtr_name = inp_set[0].identifier()
    # if there are multiple inputs, create an intermediate rule
    if len(inp_set) > 1:
      dtr_name = intermediate_typename(pcs)
      mylang.add(dtr_name + ' := avm.')
      # each input should inherit from the intermediate type
      for inp in inp_set:
        mylang.add(inp.identifier() + ' := ' + dtr_name + '.')
    # set the daughter value
    for pc in pcs:
      mylang.add('''%(id)s := [ DTR %(dtr)s ].''' %\
                 {'id':pc.identifier(), 'dtr': dtr_name})


def write_inflected_avms(mylang, all_flags):
  mylang.set_section('addenda')
  for f in all_flags:
    flag = flag_name(f)
    mylang.add('''inflected :+ [%(flag)s luk].''' % {'flag': flag})
    mylang.add('''infl-satisfied :+ [%(flag)s na-or-+].''' % {'flag': flag})

def write_pc_flags(mylang, pc, all_flags):
  """
  Go down the PC hierarchy and write input and output flags. If no
  output flags have been written, copy up all flags. Otherwise, copy
  up the flags that don't occur as output flags.
  """
  if len(all_flags) == 0: return
  write_flags(mylang, pc)
  to_copy = {}
  out_flags = set(pc.flags['out'].keys())
  for mn in pc.roots():
    to_copy[mn.key] = write_mn_flags(mylang, mn, out_flags, all_flags)
  # first write copy-ups for the root nodes
  copied_flags = write_copy_up_flags(mylang, to_copy, all_flags)
  # then, if any remain, copy up on the pc (if a lexrule)
  if pc.identifier_suffix != 'lex-super':
    to_copy = {pc.key: all_flags.difference(out_flags)}
    to_copy[pc.key].difference_update(copied_flags)
    write_copy_up_flags(mylang, to_copy, all_flags, force_write=True)
  else:
    # for lex-types, write initial flag values for all other flags
    initial_flags = set([f for f in get_all_flags('in').difference(out_flags)
                         if all(pc.precedes(mn) for mn in f)])
    for root in pc.roots():
      write_initial_flags(mylang, root,
                          initial_flags.difference(root.flags['out'].keys()))

def write_mn_flags(mylang, mn, output_flags, all_flags):
  write_flags(mylang, mn)
  to_copy = {}
  for sub_mn in mn.children().values():
    to_copy[sub_mn.key] = write_mn_flags(mylang, sub_mn,
                            output_flags.union(set(mn.flags['out'].keys())),
                            all_flags)
  copied_flags = write_copy_up_flags(mylang, to_copy, all_flags)
  return all_flags.difference(output_flags).difference(copied_flags)

def write_flags(mylang, mn):
  for flag in mn.flags['in']:
    mylang.add('''%(id)s := [ DTR.INFLECTED.%(flag)s %(val)s ].''' %\
               {'id': mn.identifier(), 'flag': flag_name(flag),
                'val': mn.flags['in'][flag]})
  for flag in mn.flags['out']:
    mylang.add('''%(id)s := [ INFLECTED.%(flag)s %(val)s ].''' %\
               {'id': mn.identifier(), 'flag': flag_name(flag),
                'val': mn.flags['out'][flag]})

def write_copy_up_flags(mylang, to_copy, all_flags, force_write=False):
  copied_flags = set()
  if len(to_copy) == 0: return copied_flags
  common_flags = reduce(set.intersection, to_copy.values())
  for mn_key in to_copy:
    mn = _mns[mn_key]
    # if all flags are common, none are copied here, and if the
    # difference contains all flags, just copy up the whole AVM.
    mn_copy_flags = to_copy[mn_key]
    if not force_write:
      mn_copy_flags.difference_update(common_flags)
    if mn_copy_flags == all_flags:
      mylang.add(mn.identifier() + ''' := [ INFLECTED #infl,
                                            DTR.INFLECTED #infl ].''')
    else:
      for flag in mn_copy_flags:
        mylang.add('''%(id)s := [ INFLECTED.%(flag)s #%(tag)s,
                                  DTR.INFLECTED.%(flag)s #%(tag)s ].''' %\
                   {'id': mn.identifier(), 'flag': flag_name(flag),
                    'tag': disjunctive_typename(flag).lower()})
    copied_flags.update(mn_copy_flags)
  return copied_flags

def write_initial_flags(mylang, mn, initial_flags):
  for flag in initial_flags:
    mylang.add('''%(id)s := [ INFLECTED.%(flag)s na-or--].''' %\
               {'id': mn.identifier(), 'flag': flag_name(flag),
                'tag': disjunctive_typename(flag).lower()})

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
      rule = '\n'.join(['-'.join([lrt.name, order + str(num[i])]) + ' :=',
                      r'%' + order + ' (* ' + lri + ')',
                      lrt.identifier()]) + '.'
      irules.add_literal(rule)
  else:
    # lexical rules
    for lri in lrt.lris:
      lrt_id = lrt.identifier()
      lrules.add(lrt_id.rsplit('-rule',1)[0] + ' := ' + lrt_id + '.')

##################
### VALIDATION ###
##################

def validate(choices, vr):
  index_feats = choices.index_features()
  for pc in all_position_classes(choices):
    basic_pc_validation(choices, pc, vr)
    cooccurrence_validation(pc, choices, vr)
    for lrt in pc.get('lrt', []):
      lrt_validation(lrt, vr, index_feats)
  cycle_validation(choices, vr)

def basic_pc_validation(choices, pc, vr):
  # Lexical rule types need order and inputs specified
  if not 'order' in pc:
    vr.err(pc.full_key + '_order',
           'You must specify an order for every position class you define.')
  if pc.get('inputs','') == '':
    vr.warn(pc.full_key + '_inputs',
            'A position class without any inputs is unusable unless you ' +\
            'define inputs during hand-development of the grammar.')
  else:
    # All inputs must be defined
    if any(inp not in choices and inp not in LEXICAL_SUPERTYPES
           for inp in pc.get('inputs','').split(', ')):
        vr.err(pc.full_key + '_inputs',
               'Every lexical type, lexical rule type, or position class ' +\
               'that serves as the input to a position class must be ' +\
               'defined somewhere in the questionnaire.')
  # Check for 0 or 1 LRTs, and warn appropriately
  if 'lrt' not in pc or len(pc.get('lrt', [])) == 0:
    vr.warn(pc.full_key + '_lrt', 'A position class without any defined ' +\
            'lexical rule types is unusable, though it can be later ' +\
            'defined by hand.')
  elif len(pc.get('lrt', [])) == 1:
    lrt = pc['lrt'].get_first()
    if lrt.get('name', '') == '':
      # if the lrt has no name, it will be merged with its position class.
      # make sure it has no constraints
      for c in lrt.get('require', []) + lrt.get('forbid', []):
        vr.err(c.full_key + '_others', 'Solitary lexical rule types with ' +\
               'no name will be merged with their position class, and ' +\
               'therefore cannot themselves take constraints.')

def lrt_validation(lrt, vr, index_feats):
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
    if lrt.full_key.startswith('verb-pc'):
      if 'head' not in feat:
        vr.err(feat.full_key + '_head',
               'You must choose where the feature is specified.')
      elif feat['head'] == 'verb' and feat.get('name','') in index_feats:
        vr.err(feat.full_key + '_head',
               'This feature is associated with nouns, ' +\
               'please select one of the NP-options.')

def cycle_validation(choices, vr):
  pch = position_class_hierarchy(choices)
  for pc in pch.nodes.values():
    cyclic_inps = set([i.key for i in pc.input_span().values()
                       if pc.precedes(i) and i.precedes(pc)])
    if len(cyclic_inps) > 0:
      vr.err(pc.key + '_inputs', 'The inputs of this position class might ' +\
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

