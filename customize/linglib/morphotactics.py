from collections import defaultdict

from customize.linglib.lexicon import lextype_map

all_slot_types = ['noun', 'verb', 'det', 'aux']

###############
### CLASSES ###
###############

class ConstraintBearingType:
  def __init__(self):
    self.constraints = {'req-fwd':set(), 'req-bkwd':set(), 'forbid':set()}
    self.flags = {'in':{},'out':{}}

class Slot(ConstraintBearingType):
  """
  A simple class for keeping track of lexical rules and intermediates.
  """
  def __init__(self, name, slot_key=None):
    ConstraintBearingType.__init__(self)
    self.name = name
    self.slot_key = slot_key
    self.rule_types = set()
    self.all_inputs = set()
    self.input_type = None
    self.order = None
    self.morphs = []

class Morpheme(ConstraintBearingType):
  """
  This class holds information necessary for each morpheme, where a
  morpheme can be a lexical rule or lexical type.
  """
  def __init__(self, name, supertype, orthography=None):
    ConstraintBearingType.__init__(self)
    self.name = name
    self.rule_types = set([supertype])
    self.orthography = orthography or ''
    self.features = {}

########################
### HELPER FUNCTIONS ###
########################

def is_slot(typename):
  """
  Return true if the given type name references a slot.
  """
  return typename.strip('0123456789').endswith('-slot')

def disjunctive_typename(types, lrs):
  return '-or-'.join([lrs[t].name for t in types])

def intermediate_rule_name(dtrs, lrs):
  return disjunctive_typename(dtrs, lrs) + '-rule-dtr'

def rule_name(slot_or_morph):
  return slot_or_morph.name + '-lex-rule'

def lex_type_name(slot):
  return slot.name + '-lex'

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

def all_inputs(lr_key, lrs, preceeds):
  """
  Return all slots preceeding the given lr such that if the slot
  requires any slots after itself, none appear before the given lr.
  If it does, the slot can't be an input. For example, if we have
  A->B->C->D and B requires C, then all_inputs for D is [A,C].
  """
  return [i for i in preceeds[lr_key]
          if not any([j in preceeds[lr_key] and i in preceeds[j]
                      for j in lrs[i].constraints['req-fwd']])]

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
  # to have occurred, or to forbid other nodes from occurring
  lrs = customize_lexical_rules(choices)
  write_rules(lrs, mylang, irules, lrules)

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
  preceeds = create_preceeds_dict(choices)
  lrs = create_lexical_rules(choices, preceeds)
  interpret_constraints(lrs, choices, preceeds)
  convert_obligatoriness_to_req(lrs, choices, preceeds)
  inp_dict = create_input_dict(lrs, preceeds)
  for inp in inp_dict:
    # if there are than one input, we need an intermediate rule
    if len(inp) > 1:
      intermediate = intermediate_rule_name(inp_dict[inp], lrs)
      ensure_lr_exists(lrs, inp, intermediate)
      for i in inp:
        lrs[i].rule_types.add(intermediate)
    for rule in inp_dict[inp]:
      lrs[rule].input_type = inp
  create_flags(lrs, preceeds)
  return lrs

def create_lexical_rules(choices, preceeds):
  lrs = {}
  # pull info from choices and create Slot objects
  for slot in choices.get_slots(all_slot_types):
    ensure_lr_exists(lrs, slot.full_key, slot.get('name', slot.full_key))
    lr = lrs[slot.full_key]
    lr.order = slot['order']
    lr.morphs = [create_morpheme(m, rule_name(lr), choices)
                 for m in slot.get('morph',[])]
    # put a placeholder Slot for lexical types so they can take flags
    for inp in slot['input']:
      ensure_lr_exists(lrs, inp['type'],
                       choices[inp['type']].get('name', inp['type']))
  return lrs

def ensure_lr_exists(lrs, key, name):
  """
  If the given key does not exist in lrs, create a new Slot and insert
  it into lrs.
  """
  if key not in lrs:
    lrs[key] = Slot(name, slot_key=key)

def create_morpheme(morph, supertype, choices):
  """
  Interpret a morpheme from the Choices file into a Morpheme object.
  """
  m = Morpheme(morph['name'], supertype, morph.get('orth',''))
  for feature in morph.get('feat',[]):
    m.features[feature['name']] = {'value':feature['value'],
                                   'head':feature.get('head',None)}
  if morph.get('orth','') == '':
    m.rule_types.add('const-lex-rule')
  else:
    m.rule_types.add('infl-lex-rule')
  # add morpheme constraints here
  return m

def interpret_constraints(lrs, choices, preceeds):
  for slot in choices.get_slots(all_slot_types):
    slot_key = slot.full_key
    for c in slot.get('constraint', []):
      other = c['other-slot']
      if c['type'] == 'require' and other in preceeds[slot_key]:
        lrs[slot_key].constraints['req-bkwd'].add(other)
      elif c['type'] == 'require' and slot_key in preceeds[other]:
        lrs[slot_key].constraints['req-fwd'].add(other)
      # only forbid backwards. convert forward forbids to backwards
      elif c['type'] == 'forbid' and other in preceeds[slot_key]:
        lrs[slot_key].constraints['forbid'].add(other)
      elif c['type'] == 'forbid' and slot_key in preceeds[other]:
        lrs[other].constraints['forbid'].add(slot_key)

def convert_obligatoriness_to_req(lrs, choices, preceeds):
  for slot in choices.get_slots(all_slot_types):
    if slot.get('opt','') != 'on':
      basetypes = filter(is_slot, preceeds[slot.full_key])
      for bt in basetypes:
        lrs[bt].constraints['req-fwd'].add(slot.full_key)

def create_input_dict(lrs, preceeds):
  """
  """
  inps = defaultdict(set)
  for lr in lrs.values():
    key = lr.slot_key
    # only slots have inputs
    if is_slot(key):
      inps[tuple(sorted(all_inputs(lr.slot_key, lrs, preceeds)))].add(key)
  return inps

def create_flags(lrs, preceeds):
  # these are values to be placed on flags:
  # tuple is ((SELF.MOTHER, SELF.DTR), (OTHER.MOTHER, OTHER.DTR))
  reqfwd  = (('-', None), ('+', None))
  reqbkwd = ((None, '+'), ('+', None))
  forbid  = (('+', None), (None, 'na'))
  for lr in lrs:
    cs = lr.constraints
    assign_flags(lr, lrs, minimal_flag_set(cs['req-fwd'], preceeds), reqfwd)
    assign_flags(lr, lrs, minimal_flag_set(cs['req-bkwd'], preceeds), reqbkwd)
    assign_flags(lr, lrs, minimal_flag_set(cs['forbid'], preceeds), forbid)

def assign_flags(lr, lrs, flag_groups, values):
  for flag_group in flag_groups:
    flag_name = disjunctive_type_name(flag_group, lrs)
    # first apply the value to the LR making the constraint
    if values[0][1]: lr.flags['in'][flag_name] = values[0][1]
    if values[0][0]: lr.flags['out'][flag_name] = values[0][0]
    # now apply the flag values to all objects of the flag
    for other in flag_group:
      if values[1][1]: lrs[other].flags['in'][flag_name] = values[1][1]
      if values[1][0]: lrs[other].flags['out'][flag_name] = values[1][0]

def minimal_flag_set(constraints, preceeds):
  """
  For a given lexical rule, use its set of constraints to find the
  minimal set of flags necessary to model the constraints.
  """
  all_flag_groups = []
  cs = ordered_constraints(constraints, preceeds)
  accounted_for = dict([(c, False) for c in cs])
  for c in cs:
    if accounted_for[c]: continue
    # nonseq are all nodes nonsequential with c (but may be with each other)
    nonseq = set([x for x in cs if not sequential(c, x, preceeds)])
    # keep only those items in nonseq that are not preceeded by an
    # item that has not been accounted for and either are not
    # accounted for themselves or are not followed by anything
    flag_group = []
    for x in nonseq:
      pre_x = preceeds[x].intersection(nonseq)
      if (any([not accounted_for[y] for y in pre_x])) or \
         (accounted_for[x] and any([x in preceeds[y] for y in nonseq])):
        continue
      flag_group += [x]
      accounted_for[x] = True
    flag_group = tuple(sorted(flag_group))
    if len(flag_group) != 0 and flag_group not in all_flag_groups:
      all_flag_groups += [flag_group]
  return all_flag_groups

##
## RULE TYPES
##  There are rule types for morphemes as well as slots, and if any
##  morph rule type is the same for all morphs in a slot, it gets
##  moved up to the slot.

def get_rule_types(lr, choices):
  #if lr.name
  # first check if all morphs have the same rule type
  for rt in ('const-lex-rule', 'infl-lex-rule'):
    if len(lr.morphs) > 0 and all([rt in m.rule_types for m in lr.morphs]):
      lr.rule_types.add(rt)
      for m in lr.morphs:
        m.rule_types.remove(rt)
  # other types
  return lr.rule_types

def get_all_flags(lrs):
  flags = set()
  for lr in lrs:
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
  for lr in lrs:
    write_rule_types(lr, mylang)
    write_flags(lr, mylang, all_flags)
    for m in lr.morphs:
      write_rule_types(m, mylang)
      write_features(m, mylang)
      #write_flags(m, mylang)

def write_rule_types(lr, mylang):
  rule_types = get_rule_types(lr, choices)
  mylang.add('''%(rule)s := %(ruletypes)s.''' %\
             {'rule': rule_name(lr),
              'ruletypes': ' & '.join(sorted(rule_types))})

def write_flags(x, mylang, all_flags):
  """
  """
  if len(x.flags['out']) == 0:
    # nothing modified, so carry up everything
    mylang.add(rule_name(x) + ' := [ INFLECTED #infl, ' +\
                                    'DTR.INFLECTED #infl ].')
  else:
    for flag in all_flags:
      if flag in x.flags['out']:
        # modify flags on output
        mylang.add('''%(rule)s := [ INFLECTED.%(flag)s %(val)s ].''' %\
                   {'rule': rule_name(x), 'flag': flag.upper(),
                    'val': x.flags['out'][flag]})
      else:
        # copy up those that aren't modified in the output
        mylang.add('''%(rule)s := [ INFLECTED.%(attr)s #%(tag)s,
                                    DTR.INFLECTED.%(attr)s #%(tag)s ].''' %\
                   {'rule': rule_name(x), 'attr': flag.upper(),
                    'tag': flag.lower()})
  # now add any input constraints
  for flag in x.constraints['in']:
    mylang.add('''%(rule)s := [ DTR.INFLECTED.%(attr)s %(val)s ].''' %\
               {'rule': rule_name(x), 'attr': flag.upper(),
                'val': x.constraints['in'][flag]})

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
