'''
This is not a fully fledged library.
Below is what was added along with the Constituent (Wh-) Questions library, mostly for testing purposes.
olzama@uw.edu
'''

from gmcs.utils import get_name, TDLencode, orth_encode

from gmcs import constants

# Constants


def customize_adv_adp(ch, mylang, rules):
    # need to handle also adjectives here
    if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
        mylang.add_literal(';;; Head Adjunct rules', section='phrases')
        mylang.add_literal(
            '; For intersective adjuncts with underspecified attachment locations:', section='phrases')
        mylang.add('bare-np-phrase := [ SYNSEM.LIGHT - ].')
        rules.add('head-adj := head-adj-int-phrase.')
        rules.add('adj-head := adj-head-int-phrase.')
       
