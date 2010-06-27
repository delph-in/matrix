import gmcs.linglib.morphotactics
from gmcs.linglib.lexicon import lexical_supertypes
from gmcs.utils import get_name

########################
### HELPER FUNCTIONS ###
########################

##########################
### MAIN LOGIC METHODS ###
##########################

def customize_direct_inverse(choices, mylang):
  features = choices.features()
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
        key = 'verb-slot' + str(idx)
        choices[key + '_name'] = n + '-dir-inv'
        choices[key + '_input1_type'] = lex.full_key
        choices[key + '_input1_type'] = lex.full_key
        # make the lexical type require the slot
        c_idx = 1
        if 'constraint' in lex:
          c_idx = lex['constraint'].next_iter_num()
        c_key = lex.full_key + '_constraint' + str(c_idx)
        choices[c_key + '_type'] = 'require'
        choices[c_key + '_other-slot'] = key
        #rule_type = n + '-dir-inv-lex-rule'
        #input_type = n + '-verb-lex'
        #mylang.set_section('dirinv')
        #mylang.add_literal(';;; Direct-inverse lexical rules')
        #mylang.add(
        #  rule_type + ' := const-lex-rule & add-only-no-ccont-rule & ' + \
        #  '[ DTR ' + input_type + ' ].')
        #mylang.add(input_type + ' := [ INFLECTED - ].', section='verblex')

        for i, direc in enumerate(['dir', 'inv']):
          morph_key = key + '_morph' + str(i+1)
          choices[morph_key + '_name'] = n + '-' + direc
          choices[morph_key + '_feat1_name'] = 'direction'
          choices[morph_key + '_feat1_value'] = direc
          choices[morph_key + '_feat1_head'] = 'verb'
          #direc_type = n + '-' + direc + '-lex-rule'
          #mylang.add(direc_type + ' := ' + rule_type + ' &' + \
          #           '[ SYNSEM.' + direc_geom + ' ' + direc + ' ].')

          #if choices.has_SCARGS():
          #  if direc == 'dir':
          #    mylang.add(direc_type + ' := \
          #                 [ SC-ARGS < #1, #2 >, \
          #                   SYNSEM.LOCAL.CAT.VAL [ SUBJ < #1 >, \
          #                                          COMPS < #2 > ] ].')
          #  else:
          #    mylang.add(direc_type + ' := \
          #                 [ SC-ARGS < #1, #2 >, \
          #                   SYNSEM.LOCAL.CAT.VAL [ SUBJ < #2 >, \
          #                                          COMPS < #1 > ] ].')

        #choices[key + '_feat1_name'] = 'dir-inv'
        #choices[key + '_feat1_value'] = 'minus'
        #choices[key + '_feat1_head'] = overt[0]['head']
        #  size = len(choices['scale'])
        #  i = 1
        #  equal = choices.get('scale-equal')

        #  while i <= size:
        #    if i == size and not (equal == 'direct' and direc == 'dir'):
        #      break

        #    rule_type = direc_type + '-' + str(i)

        #    if equal == 'direct' and direc == 'dir':
        #      if i == 1:
        #        hi_type = 'dir-inv-1'
        #        lo_type = 'dir-inv-scale'
        #      elif i == size:
        #        hi_type = lo_type = 'dir-inv-non-' + str(i-1)
        #      else:
        #        hi_type = 'dir-inv-' + str(i)
        #        lo_type = 'dir-inv-non-' + str(i-1)
        #    else:
        #      hi_type = 'dir-inv-' + str(i)
        #      lo_type = 'dir-inv-non-' + str(i)

        #    if direc == 'dir':
        #      subj_type = hi_type
        #      comps_type = lo_type
        #    else:
        #      subj_type = lo_type
        #      comps_type = hi_type

        #    mylang.add(
        #      rule_type + ' := ' + direc_type + ' &' + \
        #      '[ SYNSEM.LOCAL.CAT.VAL [ SUBJ < ' + subj_type + ' >,' + \
        #      '                         COMPS < ' + comps_type + ' > ] ].')
        #    lrules.add(
        #      n + '-' + direc + '-' + str(i+1) + ' := ' + rule_type + '.')

        #    i += 1

      #  lexical_supertypes[p] = n + '-dir-inv-lex-rule'
      #else:
      #  lexical_supertypes[p] = n + '-' + l + '-lex'
