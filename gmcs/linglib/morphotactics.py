from collections import defaultdict

from gmcs.linglib import lexicon
from gmcs.utils import get_name

all_lr_types = ['noun', 'verb', 'det', 'aux', 'adj']

###############
### CLASSES ###
###############

class ConstraintBearingType:
  def __init__(self, rule_type):
    self.constraints = {'req-fwd':set(), 'req-bkwd':set(), 'forbid':set()}
    self.disjunctive_sets = set()
    self.flags = {'in':{},'out':{}}
    self.name = ''
    self.rule_type = rule_type

  def identifier(self):
    return '-'.join([self.name, self.rule_type])

class LexicalRuleType(ConstraintBearingType):
  """
  A simple class for keeping track of lexical rules and intermediates.
  """
  def __init__(self, name, key=None, rule_type=None):
    ConstraintBearingType.__init__(self, rule_type)
    self.name = name        # string
    self.key = key          # string
    self.parents = set()    # set of strings
    self.input_lrt = None   # set of LexicalRuleTypes
    self.order = None       # string
    self.morphs = []        # list of Morphemes
    self.input_span = None  # set of LexicalRuleTypes
    self.preceeding = None  # set of LexicalRuleTypes

class Morpheme(ConstraintBearingType):
  """
  This class holds information necessary for each morpheme, where a
  morpheme can be a lexical rule or lexical type.
  """
  def __init__(self, name, key, supertype, orthography=None):
    ConstraintBearingType.__init__(self, 'lex-rule')
    self.name = name
    self.key = key
    self.parents = set()
    self.supertype = supertype
    self.orthography = orthography or ''
    self.features = {}

########################
### HELPER FUNCTIONS ###
########################

def is_lexical_rule_type(typename):
  """
  Return true if the given type name references a slot.
  """
  return typename.strip('0123456789').endswith('-slot')

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

def get_lr_name(key, choices):
  """
  Return the appropriate name for a lexical rule, depending on if it
  is a lexical rule type, lexical type, or lexical supertype.
  """
  if is_lexical_rule_type(key):
    # just get slot name or key
    return get_name(choices[key])
  elif key in lexicon.lexical_supertypes:
    # lexical supertype-- pull out of lexical_supertypes, remove '-lex'
    return lexicon.lexical_supertypes[key].rsplit('-lex',1)[0]
  else:
    # defined lextype, name may or may not be defined
    name = get_name(choices[key])
    lex = lexicon.lexical_supertypes[key.strip('1234567890')]
    return '-'.join([name, lex.rsplit('-lex',1)[0]])

def disjunctive_typename(lexical_rule_types):
  return '-or-'.join([lr.name for lr in lexical_rule_types])

def flag_name(flag):
  return flag.upper() + '-FLAG'

def create_input_span_dict(choices):
  """
  Return a dictionary that maps each LRT to the set of attested inputs
  for it and its inputs.
  """
  input_span = defaultdict(set)
  for lrt in choices.get_lexical_rule_types(all_lr_types):
    __add_input_to_span(choices, lrt, input_span)
  return input_span

def __add_input_to_span(choices, slot, input_span):
  """
  Given a choices object, a slot, and a input_span dictionary, add all
  attested inputs to the input_span dictionary.
  """
  if slot.full_key in input_span:
    return
  for inp in slot['input']:
    inp_key = inp['type']
    input_span[slot.full_key].add(inp_key)
    # find all preceeding for that input, then add those as well
    if is_lexical_rule_type(inp_key):
      __add_input_to_span(choices, choices[inp_key], input_span)
      input_span[slot.full_key].update(input_span[inp_key])

def create_preceeding_dict(input_span, choices):
  """
  Return a dictionary of all LRTs preceeding a given LRT. The key is
  the given LRT, and the values are sets of preceeding LRTs. Note that
  this is a superset of inputs, including expanded lexical supertypes
  and generalized lexical types.
  """
  # create copy of input_span dict, then augment
  preceeding_dict = dict([i for i in input_span.items()])
  for key in preceeding_dict:
    new_set = set()
    for lt in preceeding_dict[key]:
      for x in lexicon.expand_lexical_supertype(lt, choices):
        new_set.add(x)
      for x in lexicon.get_lexical_supertypes(lt, choices):
        new_set.add(x)
    preceeding_dict[key] = new_set
  return preceeding_dict

def filter_redundant_lrts(ltypes, choices):
  """
  Given a set of slots, only return the subset that is not subsumed
  by some other slot. For instance, if the list is [verb, verb1,
  verb2, tverb, noun1], and verb1 is the only transitive verb defined
  and verb2 is one of two intransitive verbs, the result would be
  [verb, tverb, verb2, noun1]. If, instead, verb1 was the only
  transitive verb and verb2 was the only intransitive verb, the result
  would be [verb, noun1].
  """
  slots = set()
  expansions = dict([(st, lexicon.expand_lexical_supertype(st, choices))
                      for st in lexicon.lexical_supertypes])
  # go through each expansion by the number of items expanded
  for e in sorted(expansions, key=lambda x: len(expansions[x]), reverse=True):
    if len(expansions[e]) > 0 and all([lt in ltypes for lt in expansions[e]]):
      slots.add(e)
      for lt in expansions[e]: ltypes.remove(lt)
  # add the remaining specific lexical types not included in a supertype
  slots.update(ltypes)
  return slots

def all_inputs(lr_key, lrs, choices):
  """
  Return all possible inputs for the given lr such that if the slot
  requires any slots after itself, none appear before the given lr.
  If it does, the slot can't be an input. For example, if we have
  A->B->C->D and B requires C, then all_inputs for D is [A,C].
  """
  return [i for i in filter_redundant_lrts(lrs[lr_key].input_span, choices)
          if not any([j in lrs[lr_key].input_span and i in j.input_span
                      for j in lrs[i.key].constraints['req-fwd']])]

def basetypes(lr, choices):
  """
  Return the subset of ltypes that represent the smallest set of
  basetypes (that is, lexical types and supertypes; not slots). If a
  set of lexical types completely covers the span of a lexical
  supertype (as in lexicon.lexical_supertypes), only return the supertype.
  """
  covered_lex_types = [b for b in lr.input_span
                       if not is_lexical_rule_type(b.key) and b.key in choices]
  return filter_redundant_lrts(covered_lex_types, choices)

def sequential(lr1, lr2):
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

##########################
### MAIN LOGIC METHODS ###
##########################

def customize_inflection(choices, mylang, irules, lrules):
  """
  Process the information in the given choices file and add the rules
  and types necessary to model the inflectional system into the irules,
  lrules, and primary grammar files.
  """
  # Imagine the inflectional system as directed acyclic (for now) graph
  # with a root node for every lexical type (or perhaps supertype of
  # several lexical types). For every edge, the source node is the
  # input to the target node. Nodes may require other nodes to occur or
  # to have occurred, or to forbid other nodes from occurring
  lrs = customize_lexical_rules(choices)
  # write_rules currently returns a list of items needing feature
  # customization. Hopefully we can find a better solution
  return write_rules(lrs, mylang, irules, lrules)

def customize_lexical_rules(choices):
  """
  Interpret the slots in a Choices file into a set of lexical rules.
  """
  # We must do some things in a certain order:
  #  1. create containers for explicitly defined slots
  #  2. determine the constraints slots place on each other
  #  3. find the unique input for each slot (and create intermediate rules)
  #      (all_inputs() depends on forward-looking require constraints)
  #  4. determine and create flags based on constraints
  lrs = create_lexical_rules(choices)
  interpret_constraints(lrs, choices)
  convert_obligatoriness_to_req(lrs, choices)
  handle_inputs(lrs, choices)
  create_flags(lrs)
  return lrs

### SLOTS AND MORPHEMES ###

def create_lexical_rules(choices):
  lrs = {}
  # put a placeholder LexicalRuleType for lexical types
  # so they can take flags
  for lt_key in lexicon.used_lexical_supertypes(choices):
    ensure_lr_exists(lrs, lt_key, get_lr_name(lt_key, choices))
    for lextype in choices[lt_key]:
      key = lextype.full_key
      ensure_lr_exists(lrs, key, get_lr_name(key, choices))
  # pull info from choices and create LexicalRuleType objects
  for lrt in choices.get_lexical_rule_types(all_lr_types):
    ensure_lr_exists(lrs, lrt.full_key,
                     get_lr_name(lrt.full_key, choices), 'lex-rule-super')
    lr = lrs[lrt.full_key]
    lr.order = lrt['order']
    lr.morphs = [create_morpheme(m, lr.key, choices)
                 for m in lrt.get('morph',[])]
    percolate_parents(lr)
  # set input_span and preceeding lists for all
  input_span_dict = create_input_span_dict(choices)
  preceeding_dict = create_preceeding_dict(input_span_dict, choices)
  for lr in lrs.values():
    lr.input_span = set([lrs[x] for x in input_span_dict.get(lr.key,[])])
    lr.preceeding = set([lrs[x] for x in preceeding_dict.get(lr.key,[])])
  return lrs

def ensure_lr_exists(lrs, key, name, rule_type=None):
  """
  If the given key does not exist in lrs, create a new LexicalRuleType
  and insert it into lrs.
  """
  if rule_type is None:
    if is_lexical_rule_type(key): rule_type = 'lex-rule-super'
    else: rule_type = 'lex'
  if key not in lrs:
    lrs[key] = LexicalRuleType(name, key=key, rule_type=rule_type)

def create_morpheme(morph, supertype, choices):
  """
  Interpret a morpheme from the Choices file into a Morpheme object.
  """
  name = get_name(morph)
  m = Morpheme(name, morph.full_key, supertype, morph.get('orth',''))
  for feature in morph.get('feat',[]):
    m.features[feature['name']] = {'value':feature['value'],
                                   'head':feature.get('head',None)}
  if morph.get('orth','') == '':
    m.parents.add('const-lex-rule')
  else:
    m.parents.add('infl-lex-rule')
  feat_vals = [(f['name'], f['value']) for f in morph.get('feat',[])]
  if ('negation', 'plus') in feat_vals:
    m.parents.add('cont-change-only-lex-rule')
  else:
    m.parents.add('add-only-no-ccont-rule')

  # add morpheme constraints here
  return m

### CONSTRAINTS ###

def interpret_constraints(lrs, choices):
  for lr in lrs.values():
    # don't bother if the lr is not defined in choices
    if lr.key not in choices or \
       not isinstance(choices[lr.key], dict): continue
    for req in choices[lr.key].get('require', []):
      others = tuple([lrs[o] for o in req['other-slot'].split(', ')])
      lr.disjunctive_sets.add(others)
      if all([o in lr.preceeding or o.key in lexicon.lexical_supertypes
              for o in others]):
        lr.constraints['req-bkwd'].update(others)
      elif all([lr in o.preceeding for o in others]):
        lr.constraints['req-fwd'].update(others)
      # we're not covering the case where others appear before
      # and after the current slot.
      # the case where it is neither followed by or follows other
      # should be covered in a validation test
    for fbd in choices[lr.key].get('forbid',[]):
      other = lrs[fbd['other-slot']]
      # only forbid backwards. convert forwards forbids to backwards
      if other in lr.preceeding:
        lr.constraints['forbid'].add(other)
      elif lr in other.preceeding:
        other.constraints['forbid'].add(lr)

def convert_obligatoriness_to_req(lrs, choices):
  """
  For all slots marked as obligatory, add a "require" constraint for
  that slot on each of the slot's inputs.
  """
  for lrt in choices.get_lexical_rule_types(all_lr_types):
    if lrt.get('obligatory','') == 'on':
      for bt in basetypes(lrs[lrt.full_key], choices):
        lrs[bt.key].constraints['req-fwd'].add(lrs[lrt.full_key])

### INPUTS ###

def handle_inputs(lrs, choices):
  inp_dict = create_input_dict(lrs, choices)
  for inp in inp_dict:
    # if there are than one input, we need an intermediate rule
    if len(inp) > 1:
      input_lrt = create_intermediate_rule(inp_dict[inp], inp, lrs)
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
  new_key = tuple([i.key for i in inputs])
  ensure_lr_exists(lrs, new_key, intermediate, 'rule-dtr')
  lrs[new_key].parents.add('avm')
  for i in inputs:
    lrs[i.key].parents.add(lrs[new_key].identifier())
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
    # + are not preceeded by an item that has not been accounted for
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

def validate(choices):
  # consider validating the following
  #  + if A constrains B, A must preceed or be preceeded by B
  #  + forbidding something required
  #     (e.g. A > B > C, A req C, B forbids C, no other paths)
  #  + explicit reqs violating inputs
  #     (e.g. A > B > C, A > C, A reqs B)
  pass
