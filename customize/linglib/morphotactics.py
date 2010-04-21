from collections import defaultdict

all_slot_types = ['noun', 'verb', 'det', 'aux']

###############
### CLASSES ###
###############

class ConstraintBearingType:
  def __init__(self):
    self.input_constraints = {}
    self.output_constraints = {}
    self.input_flags = {}
    self.output_flags = {}

  def add_input_flag(self, flag, flag_set):
    # for input constraints, all in a flag set must have the same
    # value, which in practice means they are all na or +
    val = input_constraints[flag_set[0]]
    self.input_flags[flag] = val

class LexicalRuleSuperType(ConstraintBearingType):
  def __init__(self, name, parents=None):
    self.name = name
    self.parents = parents or []

class LexicalRule(LexicalRuleSuperType):
  """
  A simple class for keeping track of lexical rules and intermediates.
  """
  def __init__(self, name, slot_key):
    self.name = name
    self.rule_name = name + '-lex-rule'
    self.slot_key = slot_key
    self.input_type = None
    self.order = None
    self.morphs = []

class Morpheme(ConstraintBearingType):
  """
  This class holds information necessary for each morpheme, where a
  morpheme can be a lexical rule or lexical type.
  """
  def __init__(self, name):
    self.name = name
    self.rule_name = name + '-lex-rule'
    self.features = {}

########################
### HELPER FUNCTIONS ###
########################

def is_slot(typename):
  """
  Return true if the given type name references a slot.
  """
  return typename.strip('0123456789').endswith('-slot')

def slot_inputs(slot):
  """
  Return a sorted tuple of all inputs of a slot.
  """
  return tuple(sorted([inp['type'] for inp in slot['input']]))

def disjunctive_typename(types, choices):
  return '-or-'.join([choices[t]['name'] for t in types])

def create_preceeds_dict(choices):
  """
  Return a dictionary that maps each slot to a set of slots that can
  preceed it, according to their inputs.
  """
  preceeds = defaultdict(set)

  for slot in choices.get_slots(all_slot_types):
    __add_preceeding(choices, slot, preceeds)
  return preceeds

def __add_preceeding(choices, slot, preceeds):
  """
  Given a choices object, a slot, and a preceeds dictionary, add all
  preceeding slots to the preceeds dictionary.
  """
  if slot.full_key in preceeds:
    return
  for inp in slot['input']:
    inp_key = inp['type']
    # add the input, find all preceeding for that input,
    # then add those as well
    preceeds[slot.full_key].add(inp_key)
    if is_slot(inp_key):
      __add_preceeding(choices, choices[inp_key], preceeds)
      preceeds[slot.full_key].update(preceeds[inp_key])

def sequential(key1, key2, preceeds):
  return (key1 in preceeds[key2] or key2 in preceeds[key1]) and key1 != key2

def ordered_constraints(cs, preceeds):
  ordered = []
  for c in cs:
    loc = len(ordered)
    for i, o in enumerate(ordered):
      if c in preceeds[o] or (not sequential(c, o, preceeds) and c < o):
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
  # to have occurred, or to forbid other nodes from occurring.

  lrs = create_lexical_rules(choices)
  calculate_constraints(choices, lrs)
  write_rules(lrs, mylang, irules, lrules)

def create_lexical_rules(choices):
  lrs = {}
  #lrsts = {}
  for slot in choices.get_slots(all_slot_types):
    lr = LexicalRule(slot['name'], slot.full_key)
    lr.order = slot['order']
    # get single input for each slot
    inps = slot_inputs(slot)
    if len(inps) == 1:
      lr.input_type = inps[0]
    elif len(inps) > 1:
      lr_super = disjunctive_typename(inps, choices)
      lr.input_type = lr_super
      lrs[lr_super] = LexicalRuleSuperType(lr_super, inps)
      #lrsts[lr_super] = lrs[lr_super]
      #TODO: do something with lr_super
    # create morphemes
    lr.morphs = [create_morpheme(m) for m in slot['morph']]
    lrs[lr.slot_key] = lr

def create_morpheme(morph):
  """
  """
  m = Morpheme(morph['name'])
  for feature in morph['feat']:
    m.features[feature['name']] = {'value':feature['value'],
                                   'head':feature.get('head',None)}
  # add morpheme constraints here
  return m

##
## CONSTRAINTS
##  If the choices file has gone through validation and has been fixed
##  by the user, then we can assume that every slot that constrains
##  another slot either preceeds or is preceeded the other slot (that
##  is, slots that are not sequential with another can have no bearing
##  on the other's occurrence).

def calculate_constraints(choices, lrs):
  preceeds = create_preceeds_dict(choices)
  for slot in choices.get_slots(all_slot_types):
    lr = lrs[slot.full_key]
    # interpret overt constraints
    for c in slot['constraint']:
      other = c['other-slot']
      if c['type'] == 'require':
        # earlier requires later (A req B)
        if slot.full_key in preceeds[other]:
          lr.output_constraints[other] = '-'
          lrs[other].output_constraints[other] = '+'
        # later requires earlier (B req A)
        else:
          lr.input_constrains[other] = '+'
          lrs[other].output_constraints[other] = '+'
      elif c['type'] == 'forbid':
        # forbidding later rule
        if slot.full_key in preceeds(other):
          lrs[other].input_constraints[slot.full_key] = 'na'
        # forbidding previous rule
        else:
          lr.input_constraints[other] = 'na'
    # convert non-optionality into requires constraint
    bt_constraints = defaultdict(dict)
    if slot['opt'] != 'on':
      # get preceeding basetypes
      basetypes = [p for p in preceeds[slot.full_key] if not is_slot(p)]
      for bt in basetypes:
        bt_constraints[bt][slot.full_key] = '-'
        lrs[slot.full_key].output_constraints[slot.full_key] = '+'
  # now convert individual constraints to flags
  create_and_assign_flags(lrs, preceeds, choices)

def create_and_assign_flags(lrs, preceeds, choices):
  for lr in lrs:
    # add lr's input flags (and output flags for those affected)
    flag_sets = create_flags(lr.input_constraints, preceeds)
    for fs in flag_sets:
      flag = disjunctive_typename(fs, choices)
      lr.add_input_flag(flag, fs)
      for lr_key in fs:
        lrs[lr_key].add_output_flag(flag, fs)
    # now add lr's output flags (and input flags for those affected)
    flag_sets = create_flags(lr.output_constraints, preceeds)
    for fs in flag_sets:
      lr.add_output_flag(fs)
      for lr_key in fs:
        lrs[lr_key].add_input_flag(fs)

def create_flags(constraints, preceeds):
  all_flag_sets = []
  vals = defaultdict(list)
  # go through constraint dict by value, rather than constraint
  # (e.g. first all constraints with val +, then -, then na, etc.)
  for c in constraints:
    vals[constraints[c]] += [c]
  for val in vals:
    cs = ordered_constraints(vals[val], preceeds)
    accounted_for = dict([(c, False) for c in cs])
    for c in cs:
      if accounted_for[c]: continue
      # nonseq are all nodes nonsequential with c (but may be with each other)
      nonseq = set([x for x in cs if not sequential(c, x, preceeds)])
      # keep only those items in nonseq that are not preceeded by an
      # item that has not been accounted for and either are not
      # accounted for themselves or are not followed by anything
      flag_set = []
      for x in nonseq:
        pre_x = preceeds[x].intersection(nonseq)
        if (any([not accounted_for[y] for y in pre_x])) or \
           (accounted_for[x] and any([x in preceeds[y] for y in nonseq])):
          continue
        flag_set += [x]
        accounted_for[x] = True
      flag_set = tuple(sorted(flag_set))
      if len(flag_set) != 0 and flag_set not in all_flag_sets:
        all_flag_sets += [flag_set]
  return all_flag_sets

######################
### OUTPUT METHODS ###
######################

def write_rules(morphs, mylang, irules, lrules):
  for rule in rules:
    pass

def apply_constraints(morphs, all_flags, mylang):
  """
  On all morphs (rules or lexical types), add the appropriate values on
  the specified flags so the restriction is properly encoded. All
  non-modified flags should be carried up.
  """
  for m in morphs:
    if len(m.output_constraints) == 0:
      # nothing modified, so carry up everything
      mylang.add(m.rule_name + ' := [ INFLECTED #infl, ' +\
                                     'DTR.INFLECTED #infl ].')
    else:
      for flag in all_flags:
        if flag in m.output_constraints:
          # modify flags on output
          mylang.add('''%(rule)s := [ INFLECTED.%(attr)s %(val)s ].''' %\
                     {'rule': m.rule_name, 'attr': flag.upper(),
                      'val': m.output_constraints[flag]})
        else:
          # copy up those that aren't modified in the output
          mylang.add('''%(rule)s := [ INFLECTED.%(attr)s #%(tag)s,
                                      DTR.INFLECTED.%(attr)s #%(tag)s ].''' %\
                     {'rule': m.rule_name, 'attr': flag.upper(),
                      'tag': flag.lower()})
    # now add any input constraints
    for flag in m.input_constraints:
      mylang.add('''%(rule)s := [ DTR.INFLECTED.%(attr)s %(val)s ].''' %\
                 {'rule': m.rule_name, 'attr': flag.upper(),
                  'val': m.input_constraints[flag]})

# unused (delete later)

#def create_input_hierarchy(choices):
#  """
#  Go through all morphs and slots and find the single input they will
#  take, creating intermediate slots if necessary.
#  """
#  inp_hierarchy = defaultdict(list)
#  for slot in choices.get_slots(all_slot_types):
#    inputs = slot_inputs(slot)
#    # if validation did not fail we can assume at least one input
#    if len(inputs) > 1:
#      inp_hierarchy[slot.full_key] += [intermediate_node_type(inputs)]
#      # add int rule
#    elif len(inputs) == 1:
#      inp_hierarchy[slot.full_key] += [inputs[0]]
#    # else:
#    #   raise Exception
#  return inp_hierarchy

#def create_input_sets(choices):
#  """
#  Create the slot type hierarchy (including intermediate types) so each
#  morpheme can take a single type as input.
#  """
#  input_sets = defaultdict(set)
#  for slot in choices.get_slots(all_slot_types):
#    inputs = slot_inputs(slot)
#    input_sets[inputs].add(slot.full_key)
#  return input_sets

#def get_all_basetypes(choices):
#  """
#  Return a set of input types for slots that aren't other slots.
#  """
#  basetypes = set()
#  for slot in choices.get_slots(all_slot_types):
#    for inp in slot['input']:
#      input_type = inp['type']
#      if not is_slot(input_type):
#        basetypes.add(input_type)
#  return basetypes
#
#def create_basetype_dict(choices):
#  """
#  Given a choices file, create dictionary mapping each slot to its set
#  of possible basetypes.
#  """
#  basetypes = {}
#  for slot in choices.get_slots(all_slot_types):
#    try:
#      basetypes[slot.full_key] = find_basetypes_for_slot(choices, slot)
#    except RuntimeError as e:
#      # seems like an loop in the slot structure
#      # for now just throw it back
#      raise e
#  return basetypes
#
#def find_basetypes_for_slot(choices, slot):
#  """
#  Given a choices file and a slot, return the set of all lexical types
#  the slot can ultimately attach to.
#  """
#  basetypes = set()
#  for inp in slot.get('input', []):
#    input_type = inp.get('type')
#    if not is_slot(input_type):
#      basetypes.add(input_type)
#    else:
#      input_slot = choices[input_type]
#      basetypes.update(find_basetypes_for_slot(choices, input_slot))
#  return basetypes

#def create_restriction_dict(choices, basetypes):
#  """
#  Create and return a lookup dictionary mapping each slot to all other
#  slots or lexical types that place restrictions on the slot.
#  """
#  restrictions = defaultdict(defaultdict(set))
#  for slot in choices.get_slots(all_slot_types):
#    # convert non-optionality into a requires-relationship
#    # NOTE: this will become deprecated when we remove (or change)
#    #       optionality in the web form
#    key = slot.full_key
#    if not slot.get('opt'):
#      slot['opt'] = 'on'
#      restrictions['required_by'][key].add(basetypes[key])
#    else:
#      for const in slot.get('constraint',[]):
#        other = const['other-slot']
#        if const['type'] == 'require':
#          restrictions['required_by'][key].add(other)
#        elif const['type'] == 'forbid':
#          restrictions['forbidden_by'][key].add(other)
#  return restrictions

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
