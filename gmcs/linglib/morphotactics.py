from collections import defaultdict

from gmcs.linglib import lexicon
from gmcs.utils import get_name
from gmcs.lib import Hierarchy, HierarchyNode

### Contents
# 1. Constants
# 2. Module Variables
# 3. Classes
# 4. Helper functions
# 5. Main logic functions
# 6. Output functions

## NOTES ON ABBREVIATIONS
# pc : position class
# lr : lexical rule
# lrt : lexical rule type
# lri : lexical rule instance
# lt : lexical type (e.g. noun1, verb1, etc.)
# lst : lexical supertype (e.g. noun, verb, iverb, etc.)
# mn : morphotactic node (a pc, lrt, lt, or lst)

################################
### CONSTANTS (in principle) ###
################################

ALL_LEX_TYPES = ['noun', 'verb', 'det', 'aux', 'adj']

########################
### MODULE VARIABLES ###
########################

_all_mns = {}
_pcs = {}
_all_types = {}

###############
### CLASSES ###
###############

class MorphotacticNode(HierarchyNode):
  def __init__(self, key, name=None, pc=None, parents=None, supertypes=None):
    HierarchyNode.__init__(self, key, parents=parents)
    self.name = name or ''
    self.pc = pc
    self.constraints = {'req-fwd':set(), 'req-bkwd':set(), 'forbid':set()}
    self.disjunctive_sets = set()
    self.flags = {'in':{},'out':{}}
    self.supertypes = supertypes or set()
    self.identifier_suffix = ''
    #self.input_lrt = None

  def relatives(self, relationship):
    # 'input' is a fake relation. It's just the parents of the position
    # class (useful when the morphotactic node is an LRT)
    if relationship == 'input':
      return self.pc.parents()
    else:
      return HierarchyNode.relatives(self, relationship)

  def identifier(self):
    return '-'.join([self.name, self.rule_type])

  def input_span(self):
    return PositionClass.input_span(self.pc, self.key)

class PositionClass(MorphotacticNode, Hierarchy):
  """
  A simple class for managing the properties of position classes.
  """
  def __init__(self, key, name, parents=None, order=None):
    MorphotacticNode.__init__(self, key, name, parents=parents)
    self.order = order      # string
    self.pc = self
    self.identifier_suffix = 'lex-rule-super'

  def __repr__(self):
    return 'PositionClass(' + self.identifier() + ')'

  def add_node(self, node):
    node.pc = self
    Hierarchy.add_node(self, node)

  def input_span(self):
    return Hierarchy.get_lineage(key=self.key, 'input')

  def percolate_supertypes(self):
    # roots are those nodes without parents
    roots = [n for n in self.nodes.values() if len(n.parents) == 0]
    root_sts = [r.percolate_supertypes() for r in roots]
    common_sts = set.intersection(*root_sts)
    # update the supertype sets
    self.supertypes.update(common_sts)
    for r in roots:
      r.supertypes.difference_update(common_sts)

class LexicalRuleType(MorphotacticNode):
  """
  A simple class for managing the properties of lexical rule types.
  """
  def __init__(self, key, name, pc=None, parents=None, supertypes=None):
    MorphotacticNode.__init__(self, key, name, pc, parents, supertypes)
    self.supertypes = supertypes or set()
    self.features = {}
    self.lris = {}
    self.identifier_suffix = 'lex-rule'

  def inputs(self):
    return self.pc.inputs

  def percolate_supertypes(self):
    # Only supertypes existing in all LRIs and subtypes can be percolated
    lri_sts = [lri.supertypes for lri in self.lris.values()]
    # Remember to do this recursively
    lrt_sts = [self.pc.nodes[c].percolate_supertypes()
               for c in self.children()]
    # func(*[list]) is the Pythonic way of making a list into function
    # parameters. so func(*[1,2]) => func(1, 2)
    common_sts = set.intersection(*(lri_sts + lrt_sts))
    # now update the supertype sets
    self.supertypes.update(common_sts)
    for lri in self.lris.values():
      lri.supertypes.difference_update(common_sts)
    for c in self.children():
      self.pc.nodes[c].supertypes.difference_update(common_sts)
    # for the recursive part, remember to return the updated set of supertypes
    return self.supertypes

class LexicalRuleInstance:
  def __init__(self, orthography, lrt, supertypes):
    self.orthography = orthography
    self.lrt = lrt
    self.supertypes = supertypes

########################
### HELPER FUNCTIONS ###
########################

def extended_input_span(mn):
  i_s = mn.input_span()
  return i_s.union(set(x for i in i_s
                         for x in _all_mns[i].pc.get_descendants(key=i)))

def generalized_input_span(mn):
  i_s = mn.input_span()
  extended = extended_input_span(mn)
  return extended.union(set(x for i in i_s
                              for x in _all_mns[i].get_ancestors(key=i)))

def all_position_classes(choices):
  """ Yield each position class defined in the choices file. """
  for lt in ALL_LEX_TYPES:
    for pc in choices[lt + '-pc']:
      yield pc

#def is_lexical_rule_type(typename):
#  """ Return true if the given type name references a slot. """
#  return typename.strip('0123456789').endswith('-slot')

def is_lexical_rule(s):
  """
  Return true if the current LexicalRuleType object is a lexical rule
  (that is, it is a lexical rule type or an intermediate rule, but not
  a lexical type.
  """
  return s.rule_type in ('lex-rule-super','lex-rule', 'rule-dtr')

def is_lexical_type(s):
  """
  Return true if the current LexicalRuleType object is a lexical type.
  """
  return s.rule_type == 'lex'

def disjunctive_typename(lexical_rule_types):
  return '-or-'.join([lr.name for lr in lexical_rule_types])

def flag_name(flag):
  return flag.upper() + '-FLAG'

#def create_input_span_dict():
#  """ Return a dictionary that maps each PC to its input span. """
#  input_spans = {}
#  for pc_key in all_position_classes(choices):
#    pc = _pcs[pc_key]
#    i_s = input_span(pc, input_spans)
#    pc.input_span['attested'] = i_s
#    pc.input_span['expanded'] = i_s.union(subtypes(i_s))
#    pc.input_span['generalized'] = i_s.union(supertypes(subtypes(i_s)))
#  return input_span_dict
#
#def input_span(pc, spans):
#  """
#  Return the input span for a given position class. Second parameter
#  is used for dynamic programming so we don't redundantly calculate
#  input spans for some position classes.
#  """
#  if pc.key in spans:
#    return spans[pc.key]
#  spans[pc] = pc.inputs.union(i_s for i in pc.inputs
#                                  for i_s in input_span(_pcs[i], spans))
#  return spans[pc.key]
#
#def subtypes(types):
#  return set(st for t in types for st in _all_types[t].subtypes)
#
#def supertypes(types):
#  return set(st for t in types for st in _all_types[t].supertypes)

#def __add_input_to_span(choices, pc, input_span):
#  """
#  Given a choices object, a pc, and a input_span dictionary, add all
#  attested inputs to the input_span dictionary.
#  """
#  if pc.full_key in input_span:
#    return
#  for inp_key in choices.split_multiselect_value(pc.full_key + '_input'):
#    input_span[pc.full_key].add(inp_key)
#    # find all preceding for that input, then add those as well
#    if is_lexical_rule_type(inp_key):
#      __add_input_to_span(choices, choices[inp_key], input_span)
#      input_span[pc.full_key].update(input_span[inp_key])

#def create_preceding_dict(input_span, choices):
#  """
#  Return a dictionary of all LRTs preceding a given LRT. The key is
#  the given LRT, and the values are sets of preceding LRTs. Note that
#  this is a superset of inputs, including expanded lexical supertypes
#  and generalized lexical types.
#  """
#  preceding_dict = {}
#  for key in input_span:
#    # need to create a new set for preceding_dict, otherwise updates
#    # to preceding_dict's items will be reflected in input_span
#    preceding_dict[key] = set([i for i in input_span[key]])
#    new_set = set()
#    for lrt in preceding_dict[key]:
#      # if the LRT is a lexical rule supertype, add its expansions
#      for x in lexicon.expand_lexical_supertype(lrt, choices):
#        new_set.add(x)
#      # otherwise add the lexical rule supertype
#      for x in lexicon.get_lexical_supertypes(lrt, choices):
#        new_set.add(x)
#    preceding_dict[key].update(new_set)
#  return preceding_dict

def add_lr_supertypes(ltypes, choices, subsumes=all):
  """
  If a set of lexical rule types spans the breadth of a supertype
  (either completely with subsumes=all, or at least one with
  subsumes=any), add that supertype to the list as well.
  """
  expansions = lexicon.get_lexical_supertype_expansions(choices)
  for st in sorted(expansions, key=lambda x: len(expansions[x]), reverse=True):
    if len(expansions[st]) > 0 and \
       subsumes([lt in ltypes for lt in expansions[st]]):
      ltypes.add(st)

def remove_subsumed_lrts(ltypes, choices, subsumes=all):
  """
  Given a set of slots, only return the subset that is not subsumed
  by some other slot. For instance, if the list is [verb, verb1,
  verb2, tverb, noun1], and verb1 is the only transitive verb defined
  and verb2 is one of two intransitive verbs, the result would be
  [verb, tverb, verb2, noun1]. If, instead, verb1 was the only
  transitive verb and verb2 was the only intransitive verb, the result
  would be [verb, noun1].
  """
  lrt_set = set()
  expansions = lexicon.get_lexical_supertype_expansions(choices)
  # go through each expansion by the number of items expanded
  for st in sorted(expansions, key=lambda x: len(expansions[x]), reverse=True):
    if st in ltypes and len(expansions[st]) > 0 and \
       subsumes([lt in ltypes for lt in expansions[st]]):
      lrt_set.add(st)
      for lt in expansions[st]:
        if lt in ltypes:
          ltypes.remove(lt)
  # add the remaining specific lexical types not included in a supertype
  lrt_set.update(ltypes)
  return lrt_set

def all_inputs(lr_key, lrs, choices):
  """
  Return all possible inputs for the given lr such that if the slot
  requires any slots after itself, none appear before the given lr.
  If it does, the slot can't be an input. For example, if we have
  A->B->C->D and B requires C, then all_inputs for D is [A,C].
  """
  lrts = [l for l in lrs[lr_key].input_span
          if not any([j in lrs[lr_key].input_span and l in j.input_span
                      for j in l.constraints['req-fwd']])]
  return [lrs[i] for i in remove_subsumed_lrts([l.key for l in lrts],
                                                choices)]

def basetypes(lr, choices):
  """
  Return the subset of ltypes that represent the smallest set of
  basetypes (that is, lexical types and supertypes; not slots). If a
  set of lexical types completely covers the span of a lexical
  supertype (as in lexicon.lexical_supertypes), only return the supertype.
  """
  covered_lex_types = [b.key for b in lr.input_span
                       if not is_lexical_rule_type(b.key) and b.key in choices]
  return remove_subsumed_lrts(covered_lex_types, choices)

def sequential(pc1, pc2):
  """
  Return True if the one of the LRTs appears on the other's input and
  if the two LRTs are different (i.e. not the same one).
  """
  return (lr1 in lr2.input_span or lr2 in lr1.input_span) \
         and lr1.key != lr2.key

def ordered_constraints(lr, constraint_type):
  """
  Return a list of constrained LRTs such that they are ordered
  according to their input order.
  """
  ordered = []
  for c in lr.constraints.get(constraint_type, []):
    loc = len(ordered)
    for i, o in enumerate(ordered):
      if c in o.input_span \
         or (not sequential(lr, o) and c.key < o.key):
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
  return any(input_path_exists(lri, pc)
             for lri in mn.descendants if type(lri) is LexicalRuleInstance)

def input_path_exists(lri, pc):
  """
  Return True if the input path is valid from lri to pc. A path is
  valid (i.e. not blocked) if no ancestor of lri requires a
  morphotactic node after lri's position class and before pc.
  """
  lrt = lri.lrt
  lri_ancestors = set([lrt, lrt.pc]).union(lrt.pc.get_ancestors(lrt))
  blocking_mns = [mn for mn in lri_ancestors
                  if any(c in pc.input_span['generalized']
                         for c in mn.constraints['req-fwd'])]
  return len(blocking_mns) == 0

##########################
### MAIN LOGIC METHODS ###
##########################

def customize_inflection(choices, mylang, irules, lrules):
  """
  Process the information in the given choices file and add the rules
  and types necessary to model the inflectional system into the irules,
  lrules, and primary grammar files.
  """
  pcs = customize_lexical_rules(choices)
  # write_rules currently returns a list of items needing feature
  # customization. Hopefully we can find a better solution
  return write_rules(pcs, mylang, irules, lrules)

def customize_lexical_rules(choices):
  """
  Interpret the slots in a Choices file into a set of lexical rules.
  """
  # When customizing and outputting lexical rules, there are some things
  # we get for free from the choices file (features, orthographies,
  # lexical rule hierarchies) and some things we must infer (DTRs for
  # rules (intermediate types), possible inputs for rules, flags).
  # We must do some things in a certain order:
  #  1. create containers for explicitly defined slots
  #  2. determine the constraints slots place on each other
  #  3. find the unique input for each slot (and create intermediate rules)
  #      (all_inputs() depends on forward-looking require constraints)
  #  4. determine and create flags based on constraints
  pch = position_class_hierarchy(choices)
  #create_lexical_rule_types(choices)
  interpret_constraints(pch, choices)
  convert_obligatoriness_to_req(lrs, choices)
  handle_inputs(lrs, choices)
  create_flags(lrs)
  return lrs

### POSITION CLASSES AND LEXICAL RULE TYPES ###

def position_class_hierarchy(choices):
  """
  Create and return the data structures to hold the information
  regarding position classes and lexical types.
  """
  pch = Hierarchy()

  # Create PositionClasses for lexical types so they can take flags
  for lex_cat in lexicon.LEXICAL_CATEGORIES:
    lth = lexicon.lexical_type_hierarchy(choices, lex_cat)
    _all_mns[lth.key] = lth
    _all_mns.update(lth.nodes)
    pch.add_node(lth)

  # Now create the actual position classes
  for pc in all_position_classes(choices):
    cur_pc = pch.add_node(PositionClass(pc.full_key, get_name(pc)),
                                        parents=pc['input'].split(', '),
                                        order=pc['order']))
    _all_mns[cur_pc.key] = cur_pc
    # Fill the lexical rule types with the information we know
    for lrt in pc['lrt']:
      cur_pc.add_node(create_lexical_rule_type(lrt))
    # Try to push common supertypes up to reduce redundancy in TDL
    cur_pc.percolate_supertypes()

  return pch

def create_lexical_rule_type(lrt):
  new_lrt = LexicalRuleType(lrt.full_key, get_name(lrt),
                            parents=lrt['supertypes'].split(', '))
  for feat in lrt['feat']:
    new_lrt.features[feat['name']] = {'value': feat['value'],
                                      'head': feat.get('head', None)}

    feat_vals = [(f['name'], f['value']) for f in morph.get('feat',[])]
    if ('negation', 'plus') in feat_vals:
      m.parents.add('cont-change-only-lex-rule')
    else:
      m.parents.add('add-only-no-ccont-rule')

  for lri in lrt['lri']:
    const_or_infl = 'const' if lri.get('orth','') == '' else 'infl'
    new_lri = LexicalRuleInstance(lri.get('orth',''), new_lrt,
                                  [const_or_infl + '-lex-rule'])
    new_lrt.lris[new_lri.key] = new_lri
  _all_mns[new_lrt.key] = new_lrt
  return new_lrt

#def ensure_lr_exists(lrs, key, name, rule_type=None):
#  """
#  If the given key does not exist in lrs, create a new LexicalRuleType
#  and insert it into lrs.
#  """
#  if rule_type is None:
#    if is_lexical_rule_type(key): rule_type = 'lex-rule-super'
#    else: rule_type = 'lex'
#  if key not in lrs:
#    lrs[key] = LexicalRuleType(name, key=key, rule_type=rule_type)

#def create_morpheme(morph, supertype, choices):
#  """
#  Interpret a morpheme from the Choices file into a Morpheme object.
#  """
#  name = get_name(morph)
#  m = Morpheme(name, morph.full_key, supertype, morph.get('orth',''))
#  for feature in morph.get('feat',[]):
#    m.features[feature['name']] = {'value':feature['value'],
#                                   'head':feature.get('head',None)}
#  if morph.get('orth','') == '':
#    m.parents.add('const-lex-rule')
#  else:
#    m.parents.add('infl-lex-rule')
#  feat_vals = [(f['name'], f['value']) for f in morph.get('feat',[])]
#  if ('negation', 'plus') in feat_vals:
#    m.parents.add('cont-change-only-lex-rule')
#  else:
#    m.parents.add('add-only-no-ccont-rule')
#
#  # add morpheme constraints here
#  return m

### CONSTRAINTS ###

def interpret_constraints(lrs, choices):
  for lr in lrs.values():
    # don't bother if the lr is not defined in choices
    if lr.key not in choices or \
       not isinstance(choices[lr.key], dict): continue
    for req in choices[lr.key].get('require', []):
      others = tuple([lrs[o] for o in req['other-slot'].split(', ')])
      lr.disjunctive_sets.add(others)
      if all([o in lr.preceding or o.key in lexicon.lexical_supertypes
              for o in others]):
        lr.constraints['req-bkwd'].update(others)
      elif all([lr in o.preceding for o in others]):
        lr.constraints['req-fwd'].update(others)
      # we're not covering the case where others appear before
      # and after the current slot.
      # the case where it is neither followed by or follows other
      # should be covered in a validation test
    for fbd in choices[lr.key].get('forbid',[]):
      other = lrs[fbd['other-slot']]
      # only forbid backwards. convert forwards forbids to backwards
      if other in lr.preceding:
        lr.constraints['forbid'].add(other)
      elif lr in other.preceding:
        other.constraints['forbid'].add(lr)

def convert_obligatoriness_to_req(lrs, choices):
  """
  For all slots marked as obligatory, add a "require" constraint for
  that slot on each of the slot's root inputs.
  """
  for lrt in choices.get_lexical_rule_types(all_lr_types):
    if lrt.get('obligatory','') == 'on':
      for bt_key in basetypes(lrs[lrt.full_key], choices):
        lrs[bt_key].constraints['req-fwd'].add(lrs[lrt.full_key])

### INPUTS ###

def handle_inputs(lrs, choices):
  inp_dict = create_input_dict(lrs, choices)
  for inp in inp_dict:
    # if there are than one input, we need an intermediate rule
    if len(inp) > 1:
      inp_keys = [i.key for i in inp]
      int_sts = remove_subsumed_lrts(inp_keys, choices)
      input_lrt = create_intermediate_rule(inp_dict[inp], int_sts, lrs)
    else:
      input_lrt = lrs[inp[0].key]
    for lr in inp_dict[inp]:
      lr.input_lrt = input_lrt

def create_input_dict(lrs, choices):
  """
  Return a dictionary with a tuple of rules (forming a set of inputs
  attested for at least one lexical rule type) as the key and a set
  of the lexical rule types with that set of inputs as the value.
  That is, input_dict[(inputs,)] = set([types with those inputs])
  """
  inps = defaultdict(set)
  for lr in lrs.values():
    # only lexical rule types have inputs
    if is_lexical_rule_type(lr.key):
      all_inp = tuple(sorted(all_inputs(lr.key, lrs, choices)))
      inps[all_inp].add(lr)
  return inps

def create_intermediate_rule(target_rules, inputs, lrs):
  intermediate = disjunctive_typename(target_rules)
  new_key = tuple(sorted(inputs))
  ensure_lr_exists(lrs, new_key, intermediate, 'rule-dtr')
  lrs[new_key].parents.add('avm')
  for i in inputs:
    lrs[i].parents.add(lrs[new_key].identifier())
  return lrs[new_key]

def percolate_parents(lr):
  if len(lr.morphs) == 0: return
  rts = [rt for rt in lr.morphs[0].parents]
  for rt in rts:
    if all([rt in m.parents for m in lr.morphs]):
      lr.parents.add(rt)
      for m in lr.morphs:
        m.parents.remove(rt)

### FLAGS ###

def create_flags(lrs):
  # these are values to be placed on flags:
  # tuple is ((SELF.MOTHER, SELF.DTR), (OTHER.MOTHER, OTHER.DTR))
  reqfwd  = (('-', None), ('+', None))
  reqbkwd = ((None, '+'), ('+', None))
  forbid  = ((None, 'na'), ('+', None))
  for lr_key in lrs:
    lr = lrs[lr_key]
    assign_flags(lr, lrs, reqfwd, minimal_flag_set(lr, 'req-fwd'))
    assign_flags(lr, lrs, reqbkwd, minimal_flag_set(lr, 'req-bkwd'))
    assign_flags(lr, lrs, forbid, minimal_flag_set(lr, 'forbid'))

def assign_flags(lr, lrs, values, flag_groups):
  for flag_group in flag_groups:
    flag_name = disjunctive_typename(flag_group)
    # first apply the value to the LR making the constraint
    if values[0][1]: lr.flags['in'][flag_name] = values[0][1]
    if values[0][0]: lr.flags['out'][flag_name] = values[0][0]
    # now apply the flag values to all objects of the flag
    for other in flag_group:
      if values[1][1]: other.flags['in'][flag_name] = values[1][1]
      if values[1][0]: other.flags['out'][flag_name] = values[1][0]

def minimal_flag_set(lr, constraint_type):
  """
  For a given lexical rule, use its set of constraints to find the
  minimal set of flags necessary to model the constraints.
  """
  all_flag_groups = []
  cs = ordered_constraints(lr, constraint_type)
  accounted_for = dict([(c, False) for c in cs])
  for c in cs:
    flag_group = set()
    if accounted_for[c]: continue
    # first add disjunctive sets
    for ds in lr.disjunctive_sets:
      if c in ds:
        flag_group.update(ds)
        for x in ds:
          accounted_for[x] = True
    # nonseq are all nodes nonsequential with c (but may be with each other)
    nonseq = set([x for x in cs if not sequential(c, x)])
    # group only those items in nonseq that fulfill the following:
    # + are not preceded by an item that has not been accounted for
    # + are not accounted for themselves or are not followed by anything
    for x in nonseq:
      pre_x = x.input_span.intersection(nonseq)
      if (any([not accounted_for[y] for y in pre_x])) or \
         (accounted_for[x] and any([x in y.input_span for y in nonseq])):
        continue
      flag_group.add(x)
      accounted_for[x] = True
    flag_group = tuple(sorted(flag_group))
    if len(flag_group) != 0 and flag_group not in all_flag_groups:
      all_flag_groups += [flag_group]
  return all_flag_groups

def get_all_flags(lrs):
  flags = set()
  for lr in lrs.values():
    flags.update(set(lr.flags['out'].keys()))
    flags.update(set(lr.flags['in'].keys()))
    for m in lr.morphs:
      flags.update(set(m.flags['out'].keys()))
      flags.update(set(m.flags['in'].keys()))
  return flags

######################
### OUTPUT METHODS ###
######################

def write_rules(lrs, mylang, irules, lrules):
  all_flags = get_all_flags(lrs)
  write_inflected_avms(all_flags, mylang)
  needing_cfv = []
  mylang.set_section('lexrules')
  for lr in filter(is_lexical_rule, lrs.values()):
    write_rule_parents(lr, sorted(lr.parents), mylang)
    write_rule_daughter(lr, lrs, mylang)
    write_flags(lr, mylang, all_flags)
    for m in lr.morphs:
      parents = [lrs[m.supertype].identifier()] + sorted(m.parents)
      write_rule_parents(m, parents, mylang)
      # a cleaner solution should replace customize_feature_values
      needing_cfv += [(m.key, m.identifier(),
                       lrs[m.supertype].key.split('-')[0])]
      # NOTE: don't write flags unless we allow morpheme constraints
      #write_flags(m, mylang, all_flags)
      # write l or i rule
      write_i_or_l_rule(m, lr.order, irules, lrules)
  for lt in filter(is_lexical_type, lrs.values()):
    mylang.set_section(sec_from_lex(lt.key))
    write_rule_parents(lt, sorted(lt.parents), mylang)
    write_flags(lt, mylang, all_flags)
  return needing_cfv

def sec_from_lex(lextype):
  if 'noun' in lextype:
    return 'nounlex'
  elif 'verb' in lextype:
    return 'verblex'
  else:
    return 'otherlex'

def write_inflected_avms(all_flags, mylang):
  mylang.set_section('addenda')
  for f in all_flags:
    flag = flag_name(f)
    mylang.add('''inflected :+ [%(flag)s luk].''' % {'flag': flag})
    mylang.add('''infl-satisfied :+ [%(flag)s na-or-+].''' % {'flag': flag})

def write_rule_parents(rule, parents, mylang):
  if len(parents) == 0: return
  mylang.add('''%(identifier)s := %(ruletypes)s.''' %\
             {'identifier': rule.identifier(),
              'ruletypes': ' & '.join(parents)})

def write_rule_daughter(rule, lrs, mylang):
  if rule.input_lrt is None: return
  mylang.add('''%(identifier)s := [ DTR %(dtr)s ].''' %\
             {'identifier': rule.identifier(),
              'dtr': rule.input_lrt.identifier()})

def write_flags(x, mylang, all_flags):
  """
  """
  # only putting flags on lextypes and lexical rule types (slots) for now
  # and within those two, lextypes can only specify output constraints
  if len(all_flags) == 0: return
  if x.rule_type in ('lex', 'lex-rule-super'):
    write_output_flags(x, mylang, all_flags)
  if x.rule_type == 'lex-rule-super':
    write_copy_up_flags(x, mylang, all_flags)
    write_input_flags(x, mylang)

def write_output_flags(x, mylang, all_flags):
  for flag in all_flags:
    if flag in x.flags['out']:
      mylang.add('''%(rule)s := [ INFLECTED.%(flag)s %(val)s ].''' %\
                 {'rule': x.identifier(), 'flag': flag_name(flag),
                  'val': x.flags['out'][flag]})

def write_copy_up_flags(x, mylang, all_flags):
  # if nothing is modified, carry up everything
  if len(x.flags['out']) == 0:
    mylang.add(x.identifier() + ' := [ INFLECTED #infl, ' +\
                                      'DTR.INFLECTED #infl ].')
  # otherwise copy up those that aren't modified in the output
  else:
    for flag in all_flags:
      if flag not in x.flags['out']:
        mylang.add('''%(rule)s := [ INFLECTED.%(flag)s #%(tag)s,
                                    DTR.INFLECTED.%(flag)s #%(tag)s ].''' %\
                   {'rule': x.identifier(), 'flag': flag_name(flag),
                    'tag': flag.lower()})

def write_input_flags(x, mylang):
  for flag in x.flags['in']:
    mylang.add('''%(rule)s := [ DTR.INFLECTED.%(flag)s %(val)s ].''' %\
               {'rule': x.identifier(), 'flag': flag_name(flag),
                'val': x.flags['in'][flag]})

def write_i_or_l_rule(m, order, irules, lrules):
  if len(m.orthography) == 0:
    mid = m.identifier()
    lrules.add(mid.rsplit('-rule',1)[0] + ' := ' + mid + '.')
  else:
    if order.lower() in ('prefix', 'before'):
      order = 'prefix'
    elif order.lower() in ('suffix', 'after'):
      order = 'suffix'
    rule = '\n'.join(['-'.join([m.name, order]) + ' :=',
                      r'%' + order + ' (* ' + m.orthography + ')',
                      m.identifier()]) + '.'
    irules.add_literal(rule)

##################
### VALIDATION ###
##################

def validate(choices, vr):
  index_feats = choices.index_features()
  cycle_validation(choices, vr)
  #lrs = create_lexical_rules(choices)
  for lrt in choices.get_lexical_rule_types(all_lr_types):
    basic_lrt_validation(choices, lrt, vr)
    cooccurrence_validation(lrt, choices, vr)
    for lr in lrt.get('morph', []):
      lr_validation(lr, vr, index_feats)

def basic_lrt_validation(choices, lrt, vr):
  # Lexical rule types need order and inputs specified
  if not 'order' in lrt:
    vr.err(lrt.full_key + '_order',
           'You must specify an order for every slot you define.')
  if not 'input' in lrt:
    vr.err(lrt.full_key + '_input1_type',
           'You must specify at least one input for every slot.')
  else:
    # All inputs must be defined
    for inp in lrt.get('input',[]):
      if inp['type'] not in choices and \
         inp['type'] not in lexicon.lexical_supertypes.keys():
        vr.err(inp.full_key + '_type',
               'Every lexical type or slot that serves as the input ' +\
               'of a slot must be defined somewhere in the questionnaire.')

def cycle_validation(choices, vr):
  inp_spans = create_input_span_dict(choices)
  cyclic_lrts = set([key for key in inp_spans if key in inp_spans[key]])
  cyclic_inps = [inp.full_key + '_type' for c in cyclic_lrts
                 for inp in choices[c + '_input']
                 if inp['type'] in cyclic_lrts]
  for i in cyclic_inps:
    vr.err(i, 'This input might cause a cycle. Please review the inputs ' +\
              'of this lexical rule.')

def cooccurrence_validation(lrt, choices, vr):
  # if A constrains B, A must precede or be preceded by B
  pass
  #  + forbidding something required
  #     (e.g. A > B > C, A req C, B forbids C, no other paths)
  #  + explicit reqs violating inputs
  #     (e.g. A > B > C, A > C, A reqs B)

def lr_validation(lr, vr, index_feats):
  # any features on an LR need a name and value (and head for verbs)
  for feat in lr.get('feat', []):
    if 'name' not in feat:
      vr.err(feat.full_key + '_name',
             'You must choose which feature you are specifying.')
    if 'value' not in feat:
      vr.err(feat.full_key + '_value',
             'You must choose a value for each feature you specify.')
    if lr.full_key.startswith('verb-slot'):
      if 'head' not in feat:
        vr.err(feat.full_key + '_head',
               'You must choose where the feature is specified.')
      elif feat['head'] == 'verb' and feat.get('name','') in index_feats:
        vr.err(feat.full_key + '_head',
               'This feature is associated with nouns, ' +\
               'please select one of the NP-options.')

