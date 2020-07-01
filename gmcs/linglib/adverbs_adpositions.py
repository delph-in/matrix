'''
This is not a fully fledged library.
Below is what was added along with the Constituent (Wh-) Questions library, mostly for testing purposes.
olzama@uw.edu
'''

from gmcs.utils import get_name,TDLencode, orth_encode

from gmcs import constants

# Constants

HEAD_ADJ = '''headv-adj-phrase := head-adj-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD +nvr ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +jrp, VAL.COMPS < > ] ].
 '''

ADJ_HEAD = '''adj-headv-phrase := adj-head-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD +nvr ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +jrp, VAL.COMPS < > ] ].
 '''

HEADN_ADJ = '''headn-adj-phrase := head-adj-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD noun ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +jrp, VAL.COMPS < > ] ].
 '''

ADJ_HEADN = '''adj-headn-phrase := adj-head-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD noun ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +jrp, VAL.COMPS < > ] ].
 '''


def customize_adv_adp(ch, mylang, rules):
    if len(ch.get('adv',[])) > 0 or len (ch.get('normadp',[])) > 0: #need to handle also adjectives here
        mylang.add_literal(';;; Head Adjunct rules',section='phrases')
        mylang.add_literal('; For intersective adjuncts with underspecified attachment locations:',section='phrases')
        #mylang.add(HEAD_ADJ, section='addenda')
        #mylang.add(ADJ_HEAD, section='addenda')
        mylang.add(HEAD_ADJ,section='phrases')
        mylang.add(ADJ_HEAD,section='phrases')
        mylang.add(HEADN_ADJ,section='phrases')
        mylang.add(ADJ_HEADN,section='phrases')
        mylang.add('bare-np-phrase := [ SYNSEM.LIGHT - ].')
        #rules.add('head-adj := head-adj-int-phrase.')
        #rules.add('adj-head := adj-head-int-phrase.')
        rules.add('headv-adj := headv-adj-phrase.')
        rules.add('adj-headv := adj-headv-phrase.')
        # rules.add('headn-adj := headn-adj-phrase.')
        # rules.add('adj-headn := adj-headn-phrase.')
        # if ch.get('q-part') == 'on' and ch.get('q-part-order') == 'second':
        #     mylang.add(HEADWH_ADJ, section='phrases')
        #     rules.add('headwh-adj := headwh-adj-phrase.')
