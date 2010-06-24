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

def expand_lexical_supertype(st, choices):
  '''
  Given a generic lexical supertype, like those defined in
  lexical_supertypes, return a list of all defined lexical types that
  fit in that pattern. For example, 'tverb' may return verbs with
  valence marked as nom-acc.
  '''
  i_t = ['iverb','tverb']
  m = ['mverb']
  if st not in lexical_supertypes: return []
  if st == 'mverb' or (st == 'verb' and choices['has-aux'] == 'no'):
    return [v.full_key for v in choices['verb']] + i_t
  elif st == 'verb' and choices['has-aux'] == 'yes':
    return [v.full_key for v in choices['verb'] + choices['aux']] + i_t + m
  elif st in ('iverb','tverb'):
    return [v.full_key for v in choices['verb']
            if case.interpret_verb_valence(v['valence']) == st]
  else:
    return [x.full_key for x in choices[st]]

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
