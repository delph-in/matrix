from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy

######################################################################
# Clausal Modifiers
#   Create the type definitions associated with the user's choices
#   about clasual modification.

######################################################################

def add_free_subordinator_to_lexicon(lexicon, freemorph):
  """
  Add free subordinators to the lexicon
  """
  lexicon.add(tn + ' := subord-lex &\\')
  for word in orth.get('freemorph'):
    orth = word.get('orth')
    orthstr = orth_encode(orth)
    pred = word.get('pred')
    predstr = pred_encode(pred)
    lexicon.add('[ STEM < "' + orthstr + '" >,\
                        SYNSEM.LKEYS.KEYREL.PRED "' + predstr + '",\
                        CFORM "' + cmsnum + '" ].')

def customize_clausalmods(mylang, ch, lexicon, rules, irules):
  """
  The main clausal modifier customization routine
  """
  mylang.set_section('clausalmods')

  for cms in ch.get('cms'):
    cmsnum = str(cms.iter_num())

    pos = cs.get('position')
    subord = cs.get('subordinator')
    subpos = cs.get('subposition')
    freemorph = cs.get('freemorph')


    tn = TDLencode(orth)
    if (len(ch.get('cms')) > 1):
      tn += cmsnum

    if subord == 'free':
      add_free_subordinator_to_lexicon(lexicon, freemorph)