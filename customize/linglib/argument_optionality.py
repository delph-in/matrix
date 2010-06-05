from ..linglib import morphotactics

      # SS 2009-06-07 added check to see if a const rule which changes
      # the COMPS of the mother to OPT - is needed.  The code assumes
      # that a given slot will have the same co-occurrence
      # restrictions for all morphemes. i.e. if one morpheme is not
      # permitted to appear with an overt argument but is required
      # with a dropped argument, all the other morphemes in this slot
      # will have the same restrictions.  This is necessary because
      # the const rule that generated will change the value of the
      # COMPS of the mother OPT - for all items which are not marked
      # by one of morphemes in this slot.
      #
      # SS 2009-06-07 Now adding capability for when the marker is not
      # permitted with a dropped argument and is required (or
      # optional) overt argument.  This is done by increasing the
      # subrules count just like above.  The subrules created are
      # different.
def customize_arg_op(choices, mylang):
  '''
  '''
  for slot in choices.get_slots(morphotactics.all_slot_types):
    slot_key = slot.full_key
    last_morph_index = 0
    if len(slot.get('morph',[])) > 0:
      last_morph_index = int(slot['morph'].get_last().iter_num())
    for morph in slot.get('morph',[]):
      # only create a lexical rule if necessary
      if not (need_no_drop_rule('obj-mark', choices) or \
              need_no_drop_rule('subj-mark', choices)):
        continue
      overt = [f for f in morph.get('feat',[]) if f['name']=='overt-arg']
      dropped = [f for f in morph.get('feat',[]) if f['name']=='dropped-arg']
      # overt-arg morphs should be the index of the last morph + 1
      if overt:
        key = slot.full_key + '_morph' + str(last_morph_index + 1)
        name = morphotactics.get_slot_name(slot.full_key, choices) + '-no-drop'
        choices[key + '_name'] = name
        choices[key + '_feat1_name'] = 'OPT'
        choices[key + '_feat1_value'] = 'minus'
        choices[key + '_feat1_head'] = overt[0]['head']
      # dropped-arg morphs should be the index of the last morph + 2
      if dropped:
        key = slot.full_key + '_morph' + str(last_morph_index + 2)
        name = morphotactics.get_slot_name(slot.full_key, choices) + '-no-drop'
        choices[key + '_name'] = name
        choices[key + '_feat1_name'] = 'OPT'
        choices[key + '_feat1_value'] = 'plus'
        choices[key + '_feat1_head'] = dropped[0]['head']

def need_no_drop_rule(obj_subj, choices):
  '''
  Return True if the unordered values of the pair
  ((obj|subj)-mark-drop, (obj|subj)-mark-no-drop) is a valid pattern
  for needing a separate morpheme.
  '''
  patterns = (set(['req','not']), set(['req','opt']))
  if set([choices[obj_subj + '-drop'].split('-')[-1],
          choices[obj_subj + '-no-drop'].split('-')[-1]]) in patterns:
    return True
  return False

##################
### VALIDATION ###
##################

def validate(choices):
  # add validation tests specific to this module
  pass
