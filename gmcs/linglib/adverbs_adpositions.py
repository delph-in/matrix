'''
This is not a fully fledged library.
Below is what was added along with the Constituent (Wh-) Questions library, mostly for testing purposes.
olzama@uw.edu
'''

from gmcs.utils import get_name,TDLencode, orth_encode

from gmcs import constants

# Constants

HEAD_ADJ = '''s-head-adj-phrase := head-adj-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT.VAL [ SPR < >, SUBJ < >, COMPS < > ],
                     NON-LOCAL.SLASH 0-dlist ],
    NON-HEAD-DTR.SYNSEM [ NON-LOCAL.QUE 0-dlist,
    					  LOCAL.CAT.VAL  [ SPR < >, SUBJ < >, COMPS < > ] ] ].
 '''

def customize_adv_adp(ch, mylang, rules):
    if len(ch.get('qadv',[])) > 0 or len (ch.get('normadp',[])) > 0:
        mylang.add_literal(';;; Head Adjunct rules')
        mylang.add_literal('; For adjuncts attaching on the S-level:')
        mylang.add(HEAD_ADJ)
        rules.add('head-adj := s-head-adj-phrase.')