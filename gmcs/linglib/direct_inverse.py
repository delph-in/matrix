import gmcs.linglib.morphotactics
from gmcs.linglib.lexicon import lextype_map
from gmcs.utils import get_name

########################
### HELPER FUNCTIONS ###
########################

##########################
### MAIN LOGIC METHODS ###
##########################

def customize_direct_inverse(choices, mylang, irules, lrules):
  features = choices.features()
  for lexprefix in ('noun', 'verb', 'det', 'aux', 'adj'):
    for lex in choices[lexprefix]:
      p = lex.full_key
      n = get_name(lex)
      if p in lextype_map: # What does this do and when would it execute?
        continue
      l = lexprefix
      if l == 'det':
        l = 'determiner'

      # If the lexical type is a direct-inverse verb, later rules
      # should use its mandatory rules as input rather than the
      # lexical type.  Create those rules here and put their supertype
      # in the root_dict.
      if lexprefix == 'verb' and lex.get('valence').endswith('dirinv'):
        direc_geom = [f[2] for f in features if f[0] == 'direction'][0]

        rule_type = n + '-dir-inv-lex-rule'
        input_type = n + '-verb-lex'
        mylang.set_section('dirinv')
        mylang.add_literal(';;; Direct-inverse lexical rules')
        mylang.add(
          rule_type + ' := const-lex-rule & add-only-no-ccont-rule & ' + \
          '[ DTR ' + input_type + ' ].')
        mylang.add(input_type + ' := [ INFLECTED - ].', section='verblex')

        for direc in ['dir', 'inv']:
          direc_type = n + '-' + direc + '-lex-rule'
          mylang.add(direc_type + ' := ' + rule_type + ' &' + \
                     '[ SYNSEM.' + direc_geom + ' ' + direc + ' ].')
          if choices.has_SCARGS():
            if direc == 'dir':
              mylang.add(direc_type + ' := \
                           [ SC-ARGS < #1, #2 >, \
                             SYNSEM.LOCAL.CAT.VAL [ SUBJ < #1 >, \
                                                    COMPS < #2 > ] ].')
            else:
              mylang.add(direc_type + ' := \
                           [ SC-ARGS < #1, #2 >, \
                             SYNSEM.LOCAL.CAT.VAL [ SUBJ < #2 >, \
                                                    COMPS < #1 > ] ].')

          size = len(choices['scale'])
          i = 1
          equal = choices.get('scale-equal')

          while i <= size:
            if i == size and not (equal == 'direct' and direc == 'dir'):
              break

            rule_type = direc_type + '-' + str(i)

            if equal == 'direct' and direc == 'dir':
              if i == 1:
                hi_type = 'dir-inv-1'
                lo_type = 'dir-inv-scale'
              elif i == size:
                hi_type = lo_type = 'dir-inv-non-' + str(i-1)
              else:
                hi_type = 'dir-inv-' + str(i)
                lo_type = 'dir-inv-non-' + str(i-1)
            else:
              hi_type = 'dir-inv-' + str(i)
              lo_type = 'dir-inv-non-' + str(i)

            if direc == 'dir':
              subj_type = hi_type
              comps_type = lo_type
            else:
              subj_type = lo_type
              comps_type = hi_type

            mylang.add(
              rule_type + ' := ' + direc_type + ' &' + \
              '[ SYNSEM.LOCAL.CAT.VAL [ SUBJ < ' + subj_type + ' >,' + \
              '                         COMPS < ' + comps_type + ' > ] ].')
            lrules.add(
              n + '-' + direc + '-' + str(i+1) + ' := ' + rule_type + '.')

            i += 1

        root_dict[p] = n + '-dir-inv-lex-rule'
      else:
        root_dict[p] = n + '-' + l + '-lex'
