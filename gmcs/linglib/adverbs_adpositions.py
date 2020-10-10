'''
This is not a fully fledged library.
Below is what was added along with the Constituent (Wh-) Questions library, mostly for testing purposes.
olzama@uw.edu
'''

from gmcs.utils import get_name, TDLencode, orth_encode

from gmcs import constants

# Constants

HEAD_ADJ = '''my-head-adj-phrase := head-adj-int-phrase &
 [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +nvr, VAL [ SUBJ clist, COMPS clist ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +jrp, VAL.COMPS < > ] ].
 '''

ADJ_HEAD = '''my-adj-head-phrase := adj-head-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD +nvr ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +jrp, VAL.COMPS < > ] ].
 '''


def customize_adv_adp(ch, mylang, rules):
    # need to handle also adjectives here
    if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
        mylang.add_literal(';;; Head Adjunct rules', section='phrases')
        mylang.add_literal(
            '; For intersective adjuncts with underspecified attachment locations:', section='phrases')
        mylang.add(HEAD_ADJ, section='phrases')
        mylang.add(ADJ_HEAD, section='phrases')
        mylang.add('bare-np-phrase := [ SYNSEM.LIGHT - ].')
        from gmcs.linglib.wh_ques import MTRX_FR_OPT, NONE_OBLIG
        if not ch.get(MTRX_FR_OPT) == NONE_OBLIG and not ch.get('word-order') == 'free':
            rules.add('head-adj := my-head-adj-phrase.')
            rules.add('adj-head := my-adj-head-phrase.')
