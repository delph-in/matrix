'''
This is not a fully fledged library.
Below is what was added along with the Constituent (Wh-) Questions library, mostly for testing purposes.
olzama@uw.edu
'''

from gmcs.utils import get_name,TDLencode, orth_encode

from gmcs import constants

# Constants

HEADJ_ADJ = '''headj-adj-phrase := head-adj-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD noun ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD adj, VAL.COMPS < > ] ].
 '''

ADJ_HEADJ = '''adj-headj-phrase := adj-head-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD noun ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD adj, VAL.COMPS < > ] ].
 '''


HEADV_ADJ = '''headv-adj-phrase := head-adj-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD verb ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +rp, VAL.COMPS < > ] ].
 '''

ADJ_HEADV = '''adj-headv-phrase := adj-head-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD verb ] ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +rp, VAL.COMPS < > ] ].
 '''

HEADN_ADJ = '''headn-adj-phrase := head-adj-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT.HEAD noun, LIGHT + ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +rp, VAL.COMPS < > ] ].
 '''

ADJ_HEADN = '''adj-headn-phrase := adj-head-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT.HEAD noun, LIGHT + ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +rp, VAL.COMPS < > ] ].
 '''

ADJ_HEADADV = '''adj-headadv-phrase := adj-head-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT.HEAD adv, LIGHT + ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].
 '''

HEADADV_ADJ = '''headadv-adj-phrase := head-adj-int-phrase &
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT.HEAD adv, LIGHT + ],
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].
 '''



def customize_adv_adp(ch, mylang, rules):
    if len(ch.get('adv',[])) > 0 or len (ch.get('normadp',[])) > 0:
        mylang.add_literal(';;; Head Adjunct rules',section='phrases')
        mylang.add_literal('; For intersective adjuncts with underspecified attachment locations:',section='phrases')
        mylang.add(HEADV_ADJ,section='phrases')
        mylang.add(ADJ_HEADV,section='phrases')
        mylang.add(HEADN_ADJ,section='phrases')
        mylang.add(ADJ_HEADN,section='phrases')
        mylang.add('bare-np-phrase := [ SYNSEM.LIGHT - ].')
        rules.add('headv-adj := headv-adj-phrase.')
        rules.add('adj-headv := adj-headv-phrase.')
        rules.add('headn-adj := headn-adj-phrase.')
        rules.add('adj-headn := adj-headn-phrase.')
        # If the word order is free, may need to constrain one of the head complement rules.
        if ch.get('q-part') == 'on' and ch.get('q-part-order') == 'second':
            mylang.add(HEADADV_ADJ, section='phrases')
            rules.add('headadv-adj := headadv-adj-phrase.')
