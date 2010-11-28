from gmcs.linglib import morphotactics
from gmcs.utils import get_name

# SS 2009-06-07 added check to see if a const rule which changes
# the COMPS of the mother to OPT - is needed.  The code assumes
# that a given position class will have the same co-occurrence
# restrictions for all lexical rule types. i.e. if one LRT is
# not permitted to appear with an overt argument but is required
# with a dropped argument, all the other LRTs in this PC will
# have the same restrictions.  This is necessary because the
# const rule that is generated will change the value of the
# COMPS of the mother OPT - for all items which are not marked
# by one of LRTs in this PC.
#
# SS 2009-06-07 Now adding capability for when the marker is not
# permitted with a dropped argument and is required (or
# optional) overt argument.  This is done by increasing the
# subrules count just like above.  The subrules created are
# different.

def add_lexrules(choices):
  '''
  '''
  for pc in morphotactics.all_position_classes(choices):
    pc_key = pc.full_key
    idx = pc['lrt'].next_iter_num() if 'lrt' in pc else 1
    for lrt in pc.get('lrt',[]):
      overt = [f for f in lrt.get('feat',[]) if f['name']=='overt-arg']
      dropped = [f for f in lrt.get('feat',[]) if f['name']=='dropped-arg']
      need_lex_rule = need_no_drop_rule('obj-mark', choices) or \
                      need_no_drop_rule('subj-mark', choices)
      # overt-arg morphs should be the index of the next available
      if overt:
        feat = overt[0]
        # convert overt-arg features to OPT
        if feat['value'] == 'not-permitted':
          feat['name'] = 'OPT'
          feat['value'] = 'plus'
        # only create a lexical rule if necessary
        if need_lex_rule:
          key = pc.full_key + '_lrt' + str(idx)
          name = get_name(pc) + '-no-drop'
          choices[key + '_name'] = name
          choices[key + '_feat1_name'] = 'OPT'
          choices[key + '_feat1_value'] = 'minus'
          choices[key + '_feat1_head'] = feat['head']
          choices[key + '_lri1_inflecting'] = 'no'
          choices[key + '_lri1_orth'] = ''
      # dropped-arg morphs should be the index of the next available + 1
      if dropped:
        feat = dropped[0]
        # convert dropped-arg features to OPT
        if feat['value'] == 'not-permitted':
          feat['name'] = 'OPT'
          feat['value'] = 'minus'
        # only create a lexical rule if necessary
        if need_lex_rule:
          key = pc.full_key + '_lrt' + str(idx + 1)
          name = get_name(pc) + '-drop'
          choices[key + '_name'] = name
          choices[key + '_feat1_name'] = 'OPT'
          choices[key + '_feat1_value'] = 'plus'
          choices[key + '_feat1_head'] = feat['head']
          choices[key + '_lri1_inflecting'] = 'no'
          choices[key + '_lri1_orth'] = ''

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
