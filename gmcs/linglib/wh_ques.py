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
			HEAD verb],
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

EX_ADJ = '''extracted-adv-adp-adj-phrase := basic-extracted-adj-phrase &
  [ SYNSEM [ LOCAL.CAT [ POSTHEAD #ph,
                         MC #mc ],
	     NON-LOCAL.SLASH 1-dlist &
		   <! [ CAT [ HEAD +rp & [ MOD < [ LOCAL intersective-mod &
                                                   [ CAT [ HEAD #head,
                                                           VAL #val,
                                                           POSTHEAD #ph,
                                                           MC #mc ],
                                                     CONT.HOOK #hook,
                                                     CTXT #ctxt ] ] > ],
                              VAL [ SUBJ olist,
                                    COMPS olist,
                                    SPR olist ] ] ] !> ],
    HEAD-DTR.SYNSEM canonical-synsem &
	   [ LOCAL local &
		   [ CAT [ HEAD verb & #head,
                           VAL #val & [ SUBJ < > ],
			   POSTHEAD #ph,
                           MC #mc ],
                     CONT.HOOK #hook,
                     CTXT #ctxt ],
             NON-LOCAL.SLASH 0-dlist,
	     MODIFIED notmod ],
    C-CONT [ HOOK #hook,
	     HCONS <! !>,
	     ICONS <! !> ] ].'''



MTX_SG_FRONT = 'matrix-front-opt'
SG_OBLIG = 'single-oblig'


def customize_wh_ques(mylang,ch,rules):
    mylang.add_literal(';;; Wh-question-related phrasal types')
    if ch.get(MTX_SG_FRONT) == SG_OBLIG:
        mylang.add_literal('''; Do not allow extracting "And Kim"''')
        mylang.add('''basic-head-filler-phrase :+
   [ ARGS < [ SYNSEM.LOCAL.COORD - ], [ SYNSEM.LOCAL.COORD - ] > ].''')
        mylang.add_literal('; Single extraction phrase')
        mylang.add(WH_Q_PHR)
        rules.add('wh-ques := wh-ques-phrase.')
        if ch.get('form-fin-nf') == 'on':
            mylang.add('wh-ques-phrase := [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ].')
        mylang.add_literal('; Complement extraction')
        mylang.add(EX_COMP)
        rules.add('ex-comp := extracted-comp-phrase.')
        mylang.add_literal('; Subject extraction')
        mylang.add(EX_SUBJ)
        rules.add('ex-subj := extracted-subj-phrase.')
        mylang.add_literal('; Adjunct extraction')
        mylang.add(EX_ADJ)
        rules.add('ex-adj := extracted-adv-adp-adj-phrase.')
