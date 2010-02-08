from collections import defaultdict

all_slot_types = ['noun', 'verb', 'det', 'aux']

class Morpheme:
  """
  This class holds information necessary for each morpheme, where a
  morpheme can be a lexical rule or lexical type.
  """
  def __init__(self, rule_name):
    self.rule_name = rule_name
    self.output_constraints = {}
    self.input_constraints = {}

def find_basetypes_for_slot(choices, slot):
  """
  Given a choices file and a slot, return the set of all lexical types
  the slot can ultimately attach to.
  """
  basetypes = set()
  for inp in slot.get('input', []):
    input_type = inp.get('type')
    if not input_type.strip('0123456789').endswith('-slot'):
      basetypes.add(input_type)
    else:
      input_slot = choices[input_type]
      basetypes.update(find_basetypes_for_slot(choices, input_slot))
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

def create_restriction_dict(choices, basetypes):
  """
  Create and return a lookup dictionary mapping each slot to all other
  slots or lexical types that place restrictions on the slot.
  """
  restrictions = defaultdict(set)
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

def apply_restrictions(morphs, all_features, mylang):
  """
  On all morphs (rules or lexical types), add the appropriate values on
  the specified features so the restriction is properly encoded. All
  non-modified features should be carried up.
  """
  for morph in morphs:
    if len(morph.output_constraints) == 0:
      # nothing modified, so carry up everything
      mylang.add(morph.rule_name + ' := [ INFLECTED #infl, ' +\
                                         'DTR.INFLECTED #infl ].')
    else:
      for feat in all_features:
        if feat in morph.output_constraints:
          # modify features on output
          mylang.add('''%(rule)s := [ INFLECTED.%(attr)s %(val)s ].''' %\
                     {'rule': morph.rule_name, 'attr': feat.upper(),
                      'val': morph.output_constraints[feat]})
        else:
          # copy up those that aren't modified in the output
          mylang.add('''%(rule)s := [ INFLECTED.%(attr)s #%(tag)s,
                                      DTR.INFLECTED.%(attr)s #%(tag)s ].''' %\
                     {'rule': morph.rule_name, 'attr': feat.upper(),
                      'tag': feat.lower()})
    # now add any input constraints
    for feat in morph.input_constraints:
      mylang.add('''%(rule)s := [ DTR.INFLECTED.%(attr)s %(val)s ].''' %\
                 {'rule': morph.rule_name, 'attr': feat.upper(),
                  'val': morph.input_constraints[feat]})

def customize_inflection(choices, mylang, irules, lrules):
  """
  Process the information in the given choices file and add the rules
  and types necessary to model the inflectional system into the irules,
  lrules, and primary grammar files.
  """
  basetypes = create_basetype_dict(choices)
  restrictions = create_restriction_dict(choices, basetypes)

  # given slots, their inputs, and their co-occurence restrictions:
  #  + convert optionality to constraint on lex-type
  #  + create data struct with feature groups and the participants
  #  + collapse multiple groups where possible
  #  + construct feature AVM, features on rules, etc
