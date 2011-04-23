from gmcs.linglib import case
from gmcs.linglib import lexical_items
from gmcs.utils import get_name
from gmcs.linglib.lexbase import LexicalType, PositionClass
from gmcs.linglib.lexbase import ALL_LEX_TYPES
from gmcs.linglib.lexbase import LEXICAL_CATEGORIES
from gmcs.linglib.lexbase import LEXICAL_SUPERTYPES

def lexical_type_hierarchy(choices, lexical_supertype):
  if lexical_supertype not in LEXICAL_CATEGORIES:
    return None
  lth = PositionClass(key=lexical_supertype + '-pc',
                      name=get_lt_name(lexical_supertype, choices),
                      identifier_suffix='lex-super')
  lth.add_node(LexicalType(lexical_supertype,
                           get_lt_name(lexical_supertype, choices)))
  lts_to_add = [lexical_supertype]
  if lexical_supertype == 'verb':
    if choices['has-aux'] == 'yes':
      lth.add_node(LexicalType('aux', get_lt_name('aux', choices),
                               parents={'verb':lth.nodes['verb']}))
      lth.add_node(LexicalType('mverb', get_lt_name('mverb', choices),
                               parents={'verb':lth.nodes['verb']}))
      lts_to_add += ['aux']
    st = get_lexical_supertype('iverb', choices)
    lth.add_node(LexicalType('iverb', get_lt_name('iverb', choices),
                             parents={st:lth.nodes[st]}))
    st = get_lexical_supertype('tverb', choices)
    lth.add_node(LexicalType('tverb', get_lt_name('tverb', choices),
                             parents={st:lth.nodes[st]}))
    if choices.get('ditransitives') == 'yes':
      st = get_lexical_supertype('dverb', choices)
      lth.add_node(LexicalType('dverb', get_lt_name('dverb', choices),
                               parents={st:lth.nodes[st]}))
  for lst in lts_to_add:
    for lt in choices[lst]:
      st = get_lexical_supertype(lt.full_key, choices)
      lth.add_node(LexicalType(lt.full_key, get_lt_name(lt.full_key, choices),
                               parents={st:lth.nodes[st]}))
      # If we're dealing with a verb add nodes for all lexical entries
      # because bistems can give rise to flags that need to appear on
      # all verbs.
      if lexical_supertype == 'verb':
        bistems = choices[lt.full_key]['bistem']
        stems = choices[lt.full_key]['stem']
        if stems:
          stems.extend(bistems)
        for stem in stems:
          lth.add_node(LexicalType(stem.full_key, stem['name'], 
                                   parents={lt.full_key:lth.nodes[lt.full_key]}, entry=True))
          
  return lth

def get_lexical_supertype(lt_key, choices):
  lexical_category = lt_key.rstrip('0123456789')
  if lexical_category in ('iverb','tverb','dverb') and choices['has-aux'] == 'yes':
    return 'mverb'
  elif lexical_category in ('aux','mverb','iverb','tverb','dverb'):
    return 'verb'
  elif lexical_category == 'verb':
    return case.interpret_verb_valence(choices[lt_key]['valence'])
  elif lexical_category in ('noun', 'det', 'aux'):
    return lexical_category
  return None


def expand_lexical_supertype(st_key, choices):
  """
  Given a generic lexical supertype, like those defined in
  lexical_supertypes, return a list of all defined lexical types that
  fit in that pattern. For example, 'tverb' may return verbs with
  valence marked as nom-acc.
  """
  i_t = ['iverb','tverb','dverb']
  m = ['mverb']
  if st_key not in LEXICAL_SUPERTYPES: return []
  if (st_key == 'mverb' and choices['has-aux'] == 'yes') or \
     (st_key == 'verb' and choices['has-aux'] == 'no'):
    return [v.full_key for v in choices['verb']] + i_t
  elif st_key == 'verb' and choices['has-aux'] == 'yes':
    return [v.full_key for v in choices['verb'] + choices['aux']] + i_t + m
  elif st_key in ('iverb','tverb','dverb'):
    return [v.full_key for v in choices['verb']
            if case.interpret_verb_valence(v['valence']) == st_key]
  else:
    return [x.full_key for x in choices.get(st_key,[])]

def get_lexical_supertype_expansions(choices):
  """
  Return a dictionary mapping each supertype to its possible
  expansions as lexical types defined in the choices file.
  """
  return dict([(st, expand_lexical_supertype(st, choices))
               for st in LEXICAL_SUPERTYPES])

def used_lexical_supertypes(choices):
  """
  Return a list of lexical supertypes (as defined in
  lexical_supertypes) that will actually be used in the grammar.
  """
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
  """
  Return the list of appropriate lexical types for the given rule
  type. There may be more than one for verbs ([tverb, mverb, verb]) or
  auxiliaries ([aux, verb]).
  """
  lexical_category = lrt_key.rstrip('0123456789')
  # first check if we are already dealing with a generic type
  if lexical_category == lrt_key:
    if lrt_key in ('iverb','tverb','dverb'):
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

def get_lt_name(key, choices):
  if key in LEXICAL_SUPERTYPES:
    # lexical supertype-- pull out of lexical_supertypes, remove '-lex'
    return LEXICAL_SUPERTYPES[key].rsplit('-lex',1)[0]
  else:
    # defined lextype, name may or may not be defined
    name = get_name(choices[key])
    lex_st = LEXICAL_SUPERTYPES[key.strip('1234567890')]
    return '-'.join([name, lex_st.rsplit('-lex',1)[0]])


