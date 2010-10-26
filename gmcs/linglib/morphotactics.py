from collections import defaultdict

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

def flag_name(flag):
  return flag.upper() + '-FLAG'

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

def valid_input(mn, pc):
  """
  Return True if the given morphotactic node can be an input for the
  given position class. This is the case if there exists an unblocked
  path from a lexical rule instance to the position class.
  """
  return any(input_path_exists(lrt, pc)
             for lrt in mn.descendants().values()
             if len(lrt.lris) > 0 or isinstance(lrt, LexicalType))

def input_path_exists(lrt, pc):
  """
  Return True if the input path is valid from lrt to pc. A path is
  valid (i.e. not blocked) if no ancestor of lrt requires a
  morphotactic node after lrt's position class and before pc.
  """
  ancestors = set([lrt, lrt.pc]).union(lrt.ancestors().values())
  blocking_mns = [mn for mn in ancestors
                  if len(mn.constraints['req-fwd']) > 0 \
                  and any(_mns[c].precedes(pc)
                          for c in mn.constraints['req-fwd'])]
  return len(blocking_mns) == 0

def get_input_map(pch):
  inp_map = defaultdict(list)
  for pc in pch.nodes.values():
    #TODO: filter impossible inputs here
    i_s = tuple(sorted(pc.input_span().values(), key=lambda x: x.key))
    if len(i_s) > 0:
      inp_map[i_s] += [pc]
  return inp_map

##########################
### MAIN LOGIC METHODS ###
##########################

def customize_inflection(choices, mylang, irules, lrules):
  """
  Process the information in the given choices file and add the rules
  and types necessary to model the inflectional system into the irules,
  lrules, and primary grammar files.
  """
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
  for pc in all_position_classes(choices):
    if len(pc.get('inputs', '')) > 0:
      pc_inputs[pc.full_key] = set(pc['inputs'].split(', '))
    cur_pc = pch.add_node(PositionClass(pc.full_key, get_name(pc),
                                        order=pc['order']))
    _mns[cur_pc.key] = cur_pc
    # Fill the lexical rule types with the information we know
    lrt_parents = {}
    for lrt in pc['lrt']:
      if 'supertypes' in lrt:
        lrt_parents[lrt.full_key] = set(lrt['supertypes'].split(', '))
      cur_pc.add_node(create_lexical_rule_type(lrt))
    for child in lrt_parents:
      for parent in lrt_parents[child]:
        cur_pc.relate_parent_child(_mns[parent], _mns[child])
    # if there is one unnamed LRT in a PC, merge the LRT with the PC
    if len(cur_pc.nodes) == 1 \
       and (cur_pc.nodes.values()[0].name == '' or cur_pc.name == ''):
      pc.identifier_suffix = 'lex-rule'
      pc.nodes.values()[0].name = pc.name
      pc.nodes.values()[0].parents = None
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
  for feat in lrt['feat']:
    new_lrt.features[feat['name']] = {'value': feat['value'],
                                      'head': feat.get('head', None)}
  new_lrt.lris = [lri.get('orth','') for lri in lrt['lri']]
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
    flag_name = disjunctive_typename(flag_group.values())
    # first apply the value to the LR making the constraint
    if flag_values[0][1] is not None:
      mn.flags['in'][flag_name] = flag_values[0][1]
    if flag_values[0][0] is not None:
      mn.flags['out'][flag_name] = flag_values[0][0]
    # now apply the flag values to all objects of the flag
    for other in flag_group.values():
      if flag_values[1][1] is not None:
        other.flags['in'][flag_name] = flag_values[1][1]
      if flag_values[1][0] is not None:
        other.flags['out'][flag_name] = flag_values[1][0]

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

def get_all_flags():
  flags = set()
  for mn in _mns.values():
    flags.update(set(mn.flags['out'].keys()))
    flags.update(set(mn.flags['in'].keys()))
  return flags

######################
### OUTPUT METHODS ###
######################

def write_rules(pch, mylang, irules, lrules):
  all_flags = get_all_flags()
  write_inflected_avms(mylang, all_flags)
  mylang.set_section('lexrules')
  write_daughter_types(mylang, pch)
  for pc in pch.nodes.values():
    # don't do anything for lex-type PCs
    if pc.identifier_suffix == 'lex-super': continue
    write_supertypes(mylang, pc.identifier(), pc.supertypes)
    write_pc_flags(mylang, pc, all_flags)
    for lrt in pc.nodes.values():
      write_i_or_l_rules(irules, lrules, lrt, pc.order)
      write_supertypes(mylang, lrt.identifier(), lrt.all_supertypes())
  # because we need to change sections, do lexical PCs last
  for pc in pch.nodes.values():
    if pc.identifier_suffix != 'lex-super': continue
    mylang.set_section(sec_from_lex(pc.key))
    write_pc_flags(mylang, pc, all_flags)
  # features need to be written later
  return [(mn.key, mn.identifier(), mn.key.split('-')[0])
          for mn in _mns.values()
          if isinstance(mn, LexicalRuleType) and len(mn.features) > 0]

def sec_from_lex(lextype):
  if 'noun' in lextype:
    return 'nounlex'
  elif 'verb' in lextype:
    return 'verblex'
  else:
    return 'otherlex'

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
  for mn in pc.roots():
    to_copy[mn.key] = write_mn_flags(mylang, mn,
                                     set(pc.flags['out'].keys()),
                                     all_flags)
  # first write copy-ups for the root nodes
  copied_flags = write_copy_up_flags(mylang, to_copy, all_flags)
  # then, if any remain, copy up on the pc (if a lexrule)
  if pc.identifier_suffix != 'lex-super':
    to_copy = {pc.key: all_flags.difference(set(pc.flags['out'].keys()))}
    to_copy[pc.key].difference_update(copied_flags)
    write_copy_up_flags(mylang, to_copy, all_flags, force_write=True)

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
  common_flags = set.intersection(*to_copy.values())
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
                   {'id': mn.identifier(), 'flag': flag, 'tag': flag.lower()})
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
    for inp in pc.get('inputs','').split(', '):
      if inp not in choices \
         and inp not in ALL_LEX_TYPES:
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
    vr.warn(lrt.full_key + '_name', 'If a position class has only one ' +\
            'defined lexical rule type, the lexical rule type will be ' +\
            'customized as a separate rule in the grammar if its name is ' +\
            'not blank, otherwise it will be merged with its position class.')

def lrt_validation(lrt, vr, index_feats):
  if 'supertypes' not in lrt:
    vr.err(lrt.full_key + '_supertypes',
           'You must select a supertype for every lexical rule type.')
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

