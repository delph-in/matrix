from collections import defaultdict

all_slot_types = ['noun', 'verb', 'det', 'aux']

class Morpheme:
  """
  This class holds information necessary for each morpheme, where a
  morpheme can be a lexical rule or lexical type.
  """
  def __init__(self, name):
    self.name = name
    self.rule_name = name + '-lex-rule'
    self.inputs = set()
    self.features = {}
    self.output_constraints = {}
    self.input_constraints = {}
    self.preceeding_morphs = set()
    self.order = None

  def preceeds(self, other):
    return other.rule_name in self.preceeding_morphs

  def follows(self, other):
    return self.rule_name in other.preceeding_morphs

def is_slot(typename):
  """
  Return true if the given type name references a slot.
  """
  return typename.strip('0123456789').endswith('-slot')

def rule_name(slotname):
  """
  """

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

  #preceeds = create_ordering_dicts(choices)
  morphs = create_morpheme_dict(choices)
  # basetypes: {rule: [basetype]}
  basetypes = create_basetype_dict(choices)
  # restrictions: {'required_by':{rule:[rule]}, 'forbidden_by':{rule:[rule]}}
  restrictions = create_restriction_dict(choices, basetypes)
  # inter_types: {rule: parent}
  inter_types = create_intermediate_type(choices, basetypes, restrictions)

  flags = create_flag_dict(morphs)
  apply_constraints(morphs, all_flags, mylang)

  # given slots, their inputs, and their co-occurrence restrictions:
  #  + convert optionality to constraint on lex-type
  #  + create data struct with feature groups and the participants
  #  + collapse multiple groups where possible
  #  + construct feature AVM, features on rules, etc

def create_preceeds_dict(choices):
  """
  Return two dictionaries where the first one maps morphemes to a set
  of morphemes that proceed it, and the second maps morphemes to a set
  of morphemes that follow it.
  """
  input_set = set()

  for slot in choices.get_slots():
    try:


def create_morpheme_dict(choices):
  """
  Create the Morpheme objects and put them in a dictionary where the
  key is the full_key of the morpheme in the choices file.
  """
  preceeds = create_preceeds_dict(choices)
  morphs = {}
  for slot in choices.get_slots(all_slot_types):
    for morph in slot.get('morph',[]):
      m = Morpheme(morph['name'])
      m.order = slot['order']
      for feature in morph['feat']:
        m.features[feature['name']] = {'value':feature['value'],
                                       'head':feature.get('head',None)}
      m.inputs = set([inp['type'] for inp in slot['input']])
      m.preceeding_morphs = preceeds[morph['name']]
      morphs[morph.full_key] = m
  return morphs

def get_all_basetypes(choices):
  """
  Return a set of input types for slots that aren't other slots.
  """
  basetypes = set()
  for slot in choices.get_slots(all_slot_types):
    for inp in slot['input']:
      input_type = inp['type']
      if not is_slot(input_type):
        basetypes.add(input_type)
  return basetypes

def create_basetype_dict(choices):
  """
  Given a choices file, create dictionary mapping each slot to its set
  of possible basetypes.
  """
  basetypes = {}
  for slot in choices.get_slots(all_slot_types):
    try:
      basetypes[slot.full_key] = find_basetypes_for_slot(choices, slot)
    except RuntimeError as e:
      # seems like an loop in the slot structure
      # for now just throw it back
      raise e
  return basetypes

def find_basetypes_for_slot(choices, slot):
  """
  Given a choices file and a slot, return the set of all lexical types
  the slot can ultimately attach to.
  """
  basetypes = set()
  for inp in slot.get('input', []):
    input_type = inp.get('type')
    if not is_slot(input_type):
      basetypes.add(input_type)
    else:
      input_slot = choices[input_type]
      basetypes.update(find_basetypes_for_slot(choices, input_slot))
  return basetypes

def create_restriction_dict(choices, basetypes):
  """
  Create and return a lookup dictionary mapping each slot to all other
  slots or lexical types that place restrictions on the slot.
  """
  restrictions = defaultdict(defaultdict(set))
  for slot in choices.get_slots(all_slot_types):
    # convert non-optionality into a requires-relationship
    # NOTE: this will become deprecated when we remove (or change)
    #       optionality in the web form
    key = slot.full_key
    if not slot.get('opt'):
      slot['opt'] = 'on'
      restrictions['required_by'][key].add(basetypes[key])
    else:
      for const in slot.get('constraint',[]):
        other = const['other-slot']
        if const['type'] == 'require':
          restrictions['required_by'][key].add(other)
        elif const['type'] == 'forbid':
          restrictions['forbidden_by'][key].add(other)
  return restrictions

def create_flag_dict(morphs):
  pass

def percolate_constraints():
  pass

def write_rules(morphs, mylang, irules, lrules):
  pass

def apply_constraints(morphs, all_flags, mylang):
  """
  On all morphs (rules or lexical types), add the appropriate values on
  the specified flags so the restriction is properly encoded. All
  non-modified flags should be carried up.
  """
  for morph in morphs:
    rule_name = morph.name + '-lex-rule'
    if len(morph.output_constraints) == 0:
      # nothing modified, so carry up everything
      mylang.add(rule_name + ' := [ INFLECTED #infl, ' +\
                                   'DTR.INFLECTED #infl ].')
    else:
      for flag in all_flags:
        if flag in morph.output_constraints:
          # modify flags on output
          mylang.add('''%(rule)s := [ INFLECTED.%(attr)s %(val)s ].''' %\
                     {'rule': rule_name, 'attr': flag.upper(),
                      'val': morph.output_constraints[flag]})
        else:
          # copy up those that aren't modified in the output
          mylang.add('''%(rule)s := [ INFLECTED.%(attr)s #%(tag)s,
                                      DTR.INFLECTED.%(attr)s #%(tag)s ].''' %\
                     {'rule': rule_name, 'attr': flag.upper(),
                      'tag': flag.lower()})
    # now add any input constraints
    for flag in morph.input_constraints:
      mylang.add('''%(rule)s := [ DTR.INFLECTED.%(attr)s %(val)s ].''' %\
                 {'rule': rule_name, 'attr': flag.upper(),
                  'val': morph.input_constraints[flag]})
