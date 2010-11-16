from gmcs.utils import TDLencode
from gmcs.lib import TDLHierarchy
from gmcs.utils import get_name

######################################################################
# customize_case()
#   Create the type definitions associated with the user's choices
#   about case.

# Given the canonical (i.e. choices variable) name of a case, return
# its abbreviation from the list of cases, which should be created by
# calling ChoicesFile.cases().  If there is no abbreviation, return
# the name.
def canon_to_abbr(name, cases):
  for c in cases:
    if c[0] == name:
      return c[2]
  return name

# Given the name of a case, return its abbreviation from the list of
# cases, which should be created by calling ChoicesFile.cases().  If
# there is no abbreviation, return the name.
def name_to_abbr(name, cases):
  for c in cases:
    if c[1] == name:
      return c[2]
  return name

def init_case_hierarchy(ch, hierarchies):
  cm = ch.get('case-marking')
  cases = ch.cases()

  hier = TDLHierarchy('case')

  # For most case patterns, just make a flat hierarchy.  For fluid-s,
  # split-n and split-v, however, a more articulated hierarchy is required.
  if cm in ['nom-acc', 'erg-abs', 'tripartite', 'split-s', 'focus']:
    for c in cases:
      hier.add(c[2], 'case', c[1])
  elif cm in ['fluid-s']:
    abbr = canon_to_abbr('a+o', cases)
    for c in cases:
      if c[0] in ['a', 'o']:
        hier.add(c[2], abbr, c[1])
      else:
        hier.add(c[2], 'case', c[1])
  elif cm in ['split-n', 'split-v']:
    nom_a = canon_to_abbr('nom', cases)
    acc_a = canon_to_abbr('acc', cases)
    erg_a = canon_to_abbr('erg', cases)
    abs_a = canon_to_abbr('abs', cases)
    if cm == 'split-v':
      for c in cases:
        hier.add(c[2], 'case', c[1])
    else:  # 'split-n':
      hier.add('a', 'case', 'transitive agent')
      hier.add('s', 'case', 'intransitive subject')
      hier.add('o', 'case', 'transitive patient')
      for c in cases:
        if c[2] == erg_a:
          hier.add(c[2], 'a', c[1])
        elif c[2] == nom_a:
          hier.add(c[2], 'a', c[1])
          hier.add(c[2], 's', c[1])
        elif c[2] == abs_a:
          hier.add(c[2], 's', c[1])
          hier.add(c[2], 'o', c[1])
        elif c[2] == acc_a:
          hier.add(c[2], 'o', c[1])
        else:
          hier.add(c[2], 'case', c[1])

  if not hier.is_empty():
    hierarchies[hier.name] = hier


# customize_case_type()
#   Create a type for case

def customize_case_type(mylang, hierarchies):
  if 'case' in hierarchies:
    hierarchies['case'].save(mylang)

# customize_case_adpositions()
#   Create the appropriate types for case-marking adpositions

def customize_case_adpositions(mylang, lexicon, ch):
  cases = ch.cases()
  features = ch.features()
  to_cfv = []

  if ch.has_adp_case():
    comment = \
      ';;; Case-marking adpositions\n' + \
      ';;; Case marking adpositions are constrained not to\n' + \
      ';;; be modifiers.'
    mylang.add_literal(comment)

    mylang.add('+np :+ [ CASE case ].', section='addenda')

    typedef = \
      'case-marking-adp-lex := basic-one-arg & raise-sem-lex-item & \
          [ SYNSEM.LOCAL.CAT [ HEAD adp & [ CASE #case, MOD < > ], \
                               VAL [ SPR < >, \
                                     SUBJ < >, \
                                     COMPS < #comps >, \
                                     SPEC < > ]], \
            ARG-ST < #comps & [ LOCAL.CAT [ HEAD noun & [ CASE #case ], \
                                            VAL.SPR < > ]] > ].'
    mylang.add(typedef)

    if ch.has_mixed_case():
      mylang.add('+np :+ [ CASE-MARKED bool ].', section='addenda')
      mylang.add(
        'case-marking-adp-lex := \
         [ ARG-ST < [ LOCAL.CAT.HEAD.CASE-MARKED - ] > ].')

    # Lexical entries
    lexicon.add_literal(';;; Case-marking adpositions')

    for adp in ch.get('adp',[]):
      orth = adp.get('orth','')

      # figure out the abbreviation for the case this adp marks
      cn = ''
      abbr = ''
      for feat in adp.get('feat', []):
        if feat['name'] == 'case':
          cn = feat['value']
          break

      abbr = name_to_abbr(cn, cases)

      adp_type = TDLencode(abbr + '-marker')
      typedef = \
        adp_type + ' := case-marking-adp-lex & \
                        [ STEM < "' + orth + '" > ].'
      lexicon.add(typedef)

      to_cfv += [(adp.full_key, adp_type, 'adp')]

  return to_cfv

def customize_case(mylang, ch, hierarchies):
  customize_case_type(mylang, hierarchies)
  if ch.has_mixed_case():
    add_lexrules_for_mixed_case(ch)

def add_lexrules_for_mixed_case(ch):
  for pc in ch['noun-pc']:
    if 'case' in [feat['name'] for lrt in pc['lrt'] for feat in lrt['feat']]:
      for c in ch.cases():
        if ch.has_adp_case(c[0]):
          idx = ch[pc.full_key + '_lrt'].next_iter_num()
          lrt_key = pc.full_key + '_lrt' + str(idx)
          ch[lrt_key + '_name'] = get_name(pc) + '-synth-' + c[0]
          ch[lrt_key + '_feat1_name'] = 'case'
          ch[lrt_key + '_feat1_value'] = c[0]
          ch[lrt_key + '_lri1_orth'] = ''

def interpret_verb_valence(valence):
  '''
  Return the canonical valence name (e.g. iverb, tverb) given the
  valence for a verb as defined in a choices file.
  '''
  if valence == 'trans' or '-' in valence:
    return 'tverb'
  else:
    return 'iverb'
