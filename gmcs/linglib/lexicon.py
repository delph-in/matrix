from gmcs.linglib import case

# lexical_supertypes is a dictionary mapping the choices file
# encodings to the actual lex-type identifiers.
lexical_supertypes = {'noun':'noun-lex',
                      'verb':'verb-lex',
                      'iverb':'intransitive-verb-lex',
                      'tverb':'transitive-verb-lex',
                      'mverb':'main-verb-lex',
                      'det':'determiner-lex',
                      'aux':'aux-lex',
                      'adj':'adjective-lex'}

def expand_lexical_supertype(st_key, choices):
  '''
  Given a generic lexical supertype, like those defined in
  lexical_supertypes, return a list of all defined lexical types that
  fit in that pattern. For example, 'tverb' may return verbs with
  valence marked as nom-acc.
  '''
  i_t = ['iverb','tverb']
  m = ['mverb']
  if st_key not in lexical_supertypes: return []
  if st_key == 'mverb' or (st_key == 'verb' and choices['has-aux'] == 'no'):
    return [v.full_key for v in choices['verb']] + i_t
  elif st_key == 'verb' and choices['has-aux'] == 'yes':
    return [v.full_key for v in choices['verb'] + choices['aux']] + i_t + m
  elif st_key in ('iverb','tverb'):
    return [v.full_key for v in choices['verb']
            if case.interpret_verb_valence(v['valence']) == st_key]
  else:
    return [x.full_key for x in choices.get(st_key,[])]

def used_lexical_supertypes(choices):
  '''
  Return a list of lexical supertypes (as defined in
  lexical_supertypes) that will actually be used in the grammar.
  '''
  used = set()
  for x in ['noun','aux','adj','det']:
    if x in choices:
      used.add(x)
  if 'verb' in choices:
    used.add('verb')
    used.update([case.interpret_verb_valence(v['valence'])
                 for v in choices['verb']])
    if choices['has-aux'] == 'yes':
      used.add('mverb')
  return used

def get_lexical_supertypes(lrt_key, choices):
  '''
  Return the list of appropriate lexical types for the given rule
  type. There may be more than one for verbs ([tverb, mverb, verb]) or
  auxiliaries ([aux, verb]).
  '''
  lexical_category = lrt_key.rstrip('0123456789')
  # first check if we are already dealing with a generic type
  if lexical_category == lrt_key:
    if lrt_key in ('iverb','tverb'):
      if choices['has-aux'] == 'yes': return ['mverb','verb']
      else: return ['verb']
    elif lrt_key == 'aux': return ['verb']
    else: return []
  # otherwise we have a lexical type (e.g. noun1, verb2, etc)
  elif lexical_category == 'verb':
    verb_type = case.interpret_verb_valence(choices[lrt_key]['valence'])
    return [verb_type] + get_lexical_supertypes(verb_type, choices)
  elif lexical_category == 'aux':
    return ['verb']
  elif lexical_category in ('noun', 'det', 'aux'):
    return [lexical_category]
  return []
