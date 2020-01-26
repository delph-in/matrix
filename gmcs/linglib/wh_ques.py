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
   [ SYNSEM [ LOCAL.CAT [ MC bool,
			VAL #val,
			HEAD verb ],
			 NON-LOCAL.QUE #que],
     HEAD-DTR.SYNSEM [ LOCAL.CAT [ MC na,
				 VAL #val & [ SUBJ < >,
					      COMPS < > ] ],
					    NON-LOCAL.QUE #que ],
     NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < ref-ind > ].'''


EX_COMP = ''' extracted-comp-phrase := basic-extracted-comp-phrase &
  [ SYNSEM.LOCAL.CAT.HEAD verb,
    HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [  SUBJ cons, COMPS 0-1-list  ]].'''

EX_SUBJ = ''' extracted-subj-phrase := basic-extracted-subj-phrase &
  [ SYNSEM.LOCAL.CAT.HEAD verb,
    HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].'''

EX_ADJ = '''extracted-adv-adp-adj-phrase := basic-extracted-adj-phrase &
  [ SYNSEM [ LOCAL.CAT [ POSTHEAD #ph,
                         MC #mc ],
	     NON-LOCAL [ QUE #que, SLASH append-list &
		   [ LIST < [ CAT [ HEAD +rp & [ MOD < [ LOCAL intersective-mod &
                                                   [ CAT [ HEAD #head,
                                                           VAL #val,
                                                           POSTHEAD #ph,
                                                           MC #mc ],
                                                     CONT.HOOK #hook,
                                                     CTXT #ctxt ] ] > ],
                              VAL [ SUBJ olist,
                                    COMPS olist,
                                    SPR olist ] ] ] > ] ] ],
    HEAD-DTR.SYNSEM canonical-synsem &
	   [ LOCAL local &
		   [ CAT [ HEAD verb & #head,
                           VAL #val & [ SUBJ < > ],
			   POSTHEAD #ph,
                           MC #mc ],
                     CONT.HOOK #hook,
                     CTXT #ctxt ],
             NON-LOCAL [ SLASH 0-alist, QUE #que ],
	     MODIFIED notmod ],
    C-CONT [ HOOK #hook,
         RELS.LIST < >,
	     HCONS.LIST < >,
	     ICONS.LIST < > ] ].'''


IN_SITU_PHRASE = '''insitu-int-cl := interrogative-clause & head-only &
  [ SYNSEM [ LOCAL.CAT [ MC +, VAL #val,
       MC bool ],
       NON-LOCAL non-local-none ],
    C-CONT [ RELS.LIST < >,
       HCONS.LIST < > ],
    HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD verb,
          VAL #val & 
            [ SUBJ < >,
              COMPS < > ] ],
          NON-LOCAL [ SLASH.LIST < >,
          REL.LIST < >,
          QUE.LIST < ref-ind > ] ] ].'''

MTX_FRONT = 'front-matrix'
MTX_FRONT_OPT = 'matrix-front-opt'
SG_OBLIG = 'single-oblig'
SINGLE = 'single'

def customize_wh_ques(mylang,ch,rules):
    if (not ch.get(MTX_FRONT)) or ch.get(MTX_FRONT_OPT) == SG_OBLIG:
        mylang.add('''clause :+ [ SYNSEM.NON-LOCAL.QUE.LIST < > ]. ''')
        if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
            mylang.add('''my-head-adj-phrase := [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < > ]. ''')
    mylang.add_literal(';;; Wh-question-related phrasal types')
    if ch.get(MTX_FRONT) in [SINGLE, SG_OBLIG]:
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

    # If the fronting isn't obligatory or if only one question phrase
    # is obligatorily fronted, need also in-situ rules:
    if ch.get(MTX_FRONT) == SINGLE and not ch.get(MTX_FRONT_OPT) == SG_OBLIG:
        mylang.add_literal('; In-situ interrogative clause.')
        mylang.add(IN_SITU_PHRASE)
        rules.add('in-situ-ques := insitu-int-cl.')

        # For non-free word orders, need to rule out structural ambiguity:
        if ch.get('word-order') == 'svo':
            mylang.add('subj-head-phrase := [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < > ].')
