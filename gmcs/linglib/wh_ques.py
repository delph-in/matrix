'''
Module to support the Constituent (Wh-) Questions library.
email olzama@uw.edu with both constituent and polar questions about the library.
'''

from gmcs.utils import get_name,TDLencode, orth_encode

from gmcs import constants


'''
CONSTANTS
'''

WH_Q_PHR = ''' wh-ques-phrase := basic-head-filler-phrase & interrogative-clause &
		  head-final &
   [ SYNSEM.LOCAL.CAT [ MC bool,
			VAL #val,
			HEAD verb & [ FORM finite ] ],
     HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na,
				 VAL #val & [ SUBJ < >,
					      COMPS < > ] ],
     NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE <! ref-ind !> ].'''


EX_COMP = ''' extracted-comp-phrase := basic-extracted-comp-phrase &
  [ SYNSEM.LOCAL.CAT.HEAD verb,
    HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [  SUBJ cons, COMPS 0-1-list  ]].'''

EX_SUBJ = ''' extracted-subj-phrase := basic-extracted-subj-phrase &
  [ SYNSEM.LOCAL.CAT.HEAD verb,
    HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].'''

FRONTING = 'wh-q-pos'

FRONT_TYPE = 'wh-q-pos-type'


def customize_wh_ques(mylang,ch):
    mylang.add_literal(';;; Wh-question-related phrasal types')
    if ch.get(FRONTING) == constants.ON and ch.get(FRONT_TYPE) == constants.SINGLE:
        mylang.add_literal('''; Do not allow extracting "And Kim"''')
        mylang.add('''basic-head-filler-phrase :+
   [ ARGS < [ SYNSEM.LOCAL.COORD - ], [ SYNSEM.LOCAL.COORD - ] > ].''')
        mylang.add_literal('; Single extraction supertype')
        mylang.add(WH_Q_PHR)
        mylang.add_literal('; Complement extraction')
        mylang.add(EX_COMP)
        mylang.add_literal('; Subject extraction')
        mylang.add(EX_SUBJ)