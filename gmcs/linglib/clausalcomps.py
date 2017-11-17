from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy

from gmcs.linglib import features

######################################################################
# Clausal Complamants
#   Create the type definitions associated with the user's choices
#   about clasual complements.

######################################################################


def customize_clausalcomps(mylang,ch,lexicon,rules,irules):
    if not 'comps' in ch:
        return None

    add_complementizers_to_lexicon(lexicon,ch)
    add_verb_types(mylang,lexicon)

def add_complementizers_to_lexicon(lexicon,ch):
    lexicon.add_literal(';;; Complementizers')
    for comp_strategy in ch['comps']:
        for complementizer in comp_strategy['complementizer']:
            orth = complementizer['orth']
            typedef = 'comp := complementizer-lex-item & \
                          [ STEM < "' + orth + '" > ].'

            lexicon.add(typedef)

def add_verb_types(mylang,lexicon):
    pass
