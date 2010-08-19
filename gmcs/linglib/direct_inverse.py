import gmcs.linglib.morphotactics
from gmcs.linglib.lexicon import lexical_supertypes
from gmcs.utils import get_name
from gmcs.lib import Hierarchy

dirinv_geom = 'LOCAL.CAT.HEAD.DIRECTION'

########################
### HELPER FUNCTIONS ###
########################

def get_subj_comps_types(j, scale_size, direc, equal):
  hi_type = lo_type = 'dir-inv-'
  if equal == 'direct' and direc == 'dir':
    lo_type += ('scale' if j==1 else ('non-' + str(j-1)))
    hi_type += (('non-' + str(j-1)) if j==scale_size else str(j))
  else:
    hi_type += str(j)
    lo_type += 'non-' + str(j)

  if direc == 'dir':
    return (hi_type, lo_type)
  else:
    return (lo_type, hi_type)

##########################
### MAIN LOGIC METHODS ###
##########################

def customize_direct_inverse(choices, mylang, hierarchies):
  if not choices.has_dirinv():
      return
  write_dir_inv_types(choices, mylang, hierarchies)
  write_dir_inv_rule_supertypes(choices, mylang)
  features = choices.features()
  scale_size = len(choices['scale'])
  equal = choices.get('scale-equal')

  for lexprefix in ('noun', 'verb', 'det', 'aux', 'adj'):
    for lex in choices[lexprefix]:
      p = lex.full_key
      n = get_name(lex)
      if p in lexical_supertypes: # What does this do and when would it execute?
        continue
      l = lexprefix
      if l == 'det':
        l = 'determiner'

      # If the lexical type is a direct-inverse verb, later rules
      # should use its mandatory rules as input rather than the
      # lexical type.  Create those rules here and put their supertype
      # in the root_dict.
      if lexprefix == 'verb' and lex.get('valence').endswith('dirinv'):
        #direc_geom = [f[2] for f in features if f[0] == 'direction'][0]
        idx = 1
        if 'verb-slot' in choices:
          idx = choices['verb-slot'].next_iter_num()
        slot_key = 'verb-slot' + str(idx)
        choices[slot_key + '_name'] = n + '-dir-inv'
        choices[slot_key + '_input1_type'] = lex.full_key
        choices[slot_key + '_input1_type'] = lex.full_key
        # make the lexical type require the slot
        c_idx = 1
        if 'constraint' in lex:
          c_idx = lex['constraint'].next_iter_num()
        c_key = lex.full_key + '_constraint' + str(c_idx)
        choices[c_key + '_type'] = 'require'
        choices[c_key + '_other-slot'] = slot_key

        for i, direc in enumerate(['dir', 'inv']):
          for j in range(1, scale_size+1):
            morph_key = slot_key + '_morph' + str((scale_size*i)+j)
            if j == scale_size and not (equal == 'direct' and direc == 'dir'):
              break
            subj_type, comps_type = get_subj_comps_types(j, scale_size,
                                                         direc, equal)
            choices[morph_key + '_name'] = '-'.join([n,direc,str(j)])
            choices[morph_key + '_feat1_name'] = 'direction'
            choices[morph_key + '_feat1_value'] = direc
            choices[morph_key + '_feat2_name'] = 'dirinv-type'
            choices[morph_key + '_feat2_head'] = 'subj'
            choices[morph_key + '_feat2_value'] = subj_type
            choices[morph_key + '_feat3_name'] = 'dirinv-type'
            choices[morph_key + '_feat3_head'] = 'obj'
            choices[morph_key + '_feat3_value'] = comps_type

def write_dir_inv_types(choices, mylang, hierarchies):
  mylang.add('verb :+ [ DIRECTION direction ].', section='addenda')
  hier = Hierarchy('direction')
  hier.add('dir', 'direction')
  hier.add('inv', 'direction')
  hier.save(mylang)

  if choices.has_SCARGS():
    mylang.add('word-or-lexrule :+ [ SC-ARGS list ].', section='addenda')
    mylang.add('lex-rule :+ [ SC-ARGS #1, DTR.SC-ARGS #1 ].', section='addenda')

  cases = choices.cases()
  features = choices.features()

  # Figure out which features are involved in the hierarchy
  names = []  # feature names
  for scale in choices.get('scale',[]):
    for feat in scale.get('feat', []):
      names.append(feat.get('name',''))

  # Now pass through the scale, creating the direct-inverse hierarchy
  # pairwise
  mylang.set_section('dirinv')
  mylang.add_literal(';;; Direct-inverse scale')
  supertype = 'dir-inv-scale'
  mylang.add(supertype + ' := canonical-synsem.')

  scale_len = len(choices.get('scale',''))

  for i in range(scale_len - 1):
    values = {}  # for each feature, a set of values

    # get the features on the first scale entry in this range
    for feat in choices.get('scale')[i].get('feat', []):
      name = feat.get('name','')
      if name not in values:
        values[name] = set()
      values[name].add(feat.get('value'))

    # create the left type in the pair
    type = 'dir-inv-' + str(i+1)

    mylang.add(type + ' := ' + supertype + '.')

    for n in values:
      vset = values[n]

      if n == 'case':
        new_vset = set()
        for v in vset:
          new_vset.add(canon_to_abbr(v, cases))
        vset = new_vset

      geom = ''
      for f in features:
        if f[0] == n:
          geom = f[2]

      value = hierarchies[n].get_type_covering(vset)
      if value != n:  # don't bother if it doesn't constrain anything
        mylang.add(type + ' := [ ' + geom + ' ' + value + ' ].')

    # rest of the scale
    values = {}
    for scale in choices.get('scale')[i+1:]:
      for feat in scale.get('feat', []):
        name = feat.get('name','')
        if name not in values:
          values[name] = set()
        values[name].add(feat.get('value',''))

    # create the right type in the pair
    type = 'dir-inv-non-' + str(i+1)

    mylang.add(type + ' := ' + supertype + '.')

    for n in values:
      vset = values[n]

      if n == 'case':
        new_vset = set()
        for v in vset:
          new_vset.add(canon_to_abbr(v, cases))
        vset = new_vset

      geom = ''
      for f in features:
        if f[0] == n:
          geom = f[2]

      value = hierarchies[n].get_type_covering(vset)
      if value != n:  # don't bother if it doesn't constrain anything
        mylang.add(type + ' := [ ' + geom + ' ' + value + ' ].')

    supertype = type

def write_dir_inv_rule_supertypes(choices, mylang):
  mylang.set_section('dirinv')
  mylang.add_literal(';;; Direct-inverse lexical rules')
  mylang.add('dir-lex-rule := add-only-no-ccont-rule & ' + \
             '[ SYNSEM.' + dirinv_geom + ' dir ].')
  mylang.add('inv-lex-rule := add-only-no-ccont-rule & ' + \
             '[ SYNSEM.' + dirinv_geom + ' inv ].')
  if choices.has_SCARGS():
    mylang.add('dir-lex-rule := \
                   [ SC-ARGS < #1, #2 >, \
                     SYNSEM.LOCAL.CAT.VAL [ SUBJ < #1 >, \
                                            COMPS < #2 > ] ].')
    mylang.add('inv-lex-rule := \
                   [ SC-ARGS < #1, #2 >, \
                     SYNSEM.LOCAL.CAT.VAL [ SUBJ < #2 >, \
                                            COMPS < #1 > ] ].')
