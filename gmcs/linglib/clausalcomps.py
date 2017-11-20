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
    add_ctp_to_lexicon(mylang,lexicon)
    add_complementizer_type_to_grammar(mylang,ch,rules)

def add_complementizer_type_to_grammar(mylang,ch,rules):
    mylang.set_section('complex')
    for cs in ch.get('comps'):
        id = cs.full_key
        typename = id + '-lex-item'
        mylang.add(typename + ' := raise-sem-lex-item & basic-one-arg &\
      [ SYNSEM.LOCAL.CAT [ HEAD comp &\
                                [ MOD < > ],\
                           VAL [ SPR < >,\
                                 SUBJ < >,\
                                 COMPS < #comps > ] ],\
        ARG-ST < #comps & \
                 [ LOCAL.CAT [ HEAD verb, MC -,\
                               VAL [ SUBJ < >,\
                                     COMPS < > ] ] ] > ].',section='complex')
        # merge feature information in
        for f in cs['feat']:
            if f['name'] == 'form':
                mylang.add(typename + ' := raise-sem-lex-item & basic-one-arg & [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.FORM ' + f['value'] + ' ].',merge=True)

def add_complementizers_to_lexicon(lexicon,ch):
    lexicon.add_literal(';;; Complementizers')
    for comp_strategy in ch['comps']:
        for complementizer in comp_strategy['complementizer']:
            orth = complementizer['orth']
            typedef = 'comp := complementizer-lex-item & \
                          [ STEM < "' + orth + '" > ].'

            lexicon.add(typedef)

def add_ctp_to_lexicon(mylang,lexicon):
    pass
