
def interpret_verb_valence(valence):
  '''
  Return the canonical valence name (e.g. iverb, tverb) given the
  valence for a verb as defined in a choices file.
  '''
  if valence == 'trans' or '-' in valence:
    return 'tverb'
  else:
    return 'iverb'
