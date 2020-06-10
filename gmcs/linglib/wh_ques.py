'''
Module to support the Constituent (Wh-) Questions library.
email olzama@uw.edu with both constituent and polar questions about the library.
'''

from gmcs.utils import get_name,TDLencode, orth_encode

from gmcs import constants,globals


'''
CONSTANTS
'''

WH_Q_PHR_NO_OR_SG_OBLIG_MULTI = ''' wh-ques-phrase := 
   [ SYNSEM.NON-LOCAL.QUE #que,
     HEAD-DTR.SYNSEM.NON-LOCAL.QUE #que ].'''

WH_Q_PHR_SG_OR_OBLIG_FRONT = ''' wh-ques-phrase := 
   [ SYNSEM.NON-LOCAL.QUE.LIST < > ].'''

WH_Q_PHR = ''' wh-ques-phrase := basic-head-filler-phrase & interrogative-clause &
		  head-final &
   [ SYNSEM [ LOCAL.CAT [ WH.BOOL +, MC bool,
			VAL #val,
			HEAD verb ] ],
     HEAD-DTR.SYNSEM [ LOCAL.CAT [ MC na-or-+,
				 VAL #val & [ SUBJ < >,
					      COMPS < > ] ] ],
     NON-HEAD-DTR.SYNSEM [ NON-LOCAL.QUE.LIST < ref-ind >,
                           LOCAL.CONT.HOOK.ICONS-KEY focus ] ].'''


EX_COMP = ''' extracted-comp-phrase := basic-extracted-comp-phrase &
  [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < [], ... >  ].'''

EX_SUBJ = ''' extracted-subj-phrase := basic-extracted-subj-phrase &
  [ SYNSEM.LOCAL.CAT.HEAD verb,
    HEAD-DTR.SYNSEM [ LOCAL.CAT.VAL.SUBJ.FIRST.LOCAL #slash & local,
                      NON-LOCAL.SLASH.LIST < #slash, ... > ] ].'''

EX_SUBJ_MULTI = '''extracted-subj-phrase := basic-extracted-arg-phrase & head-compositional &
  [ SYNSEM [ LOCAL.CAT.VAL [ SUBJ < >,
                           SPR < > ] ],
    HEAD-DTR.SYNSEM [ L-QUE -,
                      LOCAL.CAT [ VAL [ SUBJ < gap &
                                             [ LOCAL local &
                                               [ CONT.HOOK.INDEX ref-ind ] ] > ], MC na ] ],
    C-CONT [ RELS.LIST < >,
             HCONS.LIST < >,
             ICONS.LIST < > ] ].'''

EX_ADJ = '''extracted-adv-adp-adj-phrase := basic-extracted-adj-phrase &
  [ SYNSEM [ LOCAL.CAT [ WH #wh,
                         POSTHEAD #ph,
                         MC #mc ],
	     NON-LOCAL [ QUE #que, YNQ #ynq, SLASH append-list &
		   [ LIST < [ CAT [ HEAD +rp & [ MOD < [ LOCAL intersective-mod &
                                                   [ CAT [ HEAD #head,
                                                           VAL #val,
                                                           POSTHEAD #ph,
                                                           MC #mc ],
                                                     CONT.HOOK #hook,
                                                     CTXT #ctxt ] ] . #slash > ],
                              VAL [ SUBJ olist,
                                    COMPS olist,
                                    SPR olist ] ] ] > ] ] ],
    HEAD-DTR.SYNSEM canonical-synsem &
	   [ LOCAL local &
		   [ CAT [ WH #wh,
		           HEAD verb & #head,
                           VAL #val & [ SUBJ < > ],
			   POSTHEAD #ph,
                           MC #mc ],
                     CONT.HOOK #hook,
                     CTXT #ctxt ],
             NON-LOCAL [ QUE #que, SLASH.LIST #slash, YNQ #ynq ],
	     MODIFIED notmod ],
    C-CONT [ HOOK #hook,
         RELS.LIST < >,
	     HCONS.LIST < >,
	     ICONS.LIST < > ] ].'''


IN_SITU_PHRASE = '''insitu-int-cl := interrogative-clause & head-only &
  [ SYNSEM [ MODIFIED hasmod,
             LOCAL.CAT [ VAL #val,
       MC bool ],
       NON-LOCAL [ SLASH.LIST < >, QUE.LIST < >, REL.LIST < > ] ],
    C-CONT [ RELS.LIST < >,
       HCONS.LIST < > ],
    HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD verb,
          VAL #val & 
            [ SUBJ < >,
              COMPS < > ] ],
          NON-LOCAL [ SLASH.LIST < >,
          REL.LIST < >,
          QUE.LIST < ref-ind, ... > ] ] ].'''

EX_DET_PHRASE = '''extracted-det-phrase := basic-extracted-arg-phrase & head-compositional &
[ SYNSEM [ LOCAL.CAT [ VAL [ SUBJ < >, COMPS < >, SPR < > ] ] ],
  HEAD-DTR.SYNSEM [ MODIFIED notmod,
                    LOCAL.CAT.VAL.SPR <  gap & [ LOCAL #local & local &
                                                [ CAT.HEAD det,
                                                  CONT.HOOK #hook ] ] >,
                   L-PERIPH -,
                   NON-LOCAL.SLASH.LIST < #local > ],
    C-CONT [ RELS.LIST < >,
             HCONS.LIST < >,
             ICONS.LIST < >,
             HOOK #hook ] ].'''

BASIC_FILLER_SG = '''basic-filler-phrase :+ [ SYNSEM.NON-LOCAL.SLASH.LIST < >,
                                           ARGS < [ SYNSEM [ LOCAL #slash,
                                                             NON-LOCAL.SLASH.LIST < > ] ], 
                                                    [SYNSEM.NON-LOCAL.SLASH.LIST < #slash >] >]. '''

FIRST_FILLER = '''1st-head-filler-phrase := basic-filler-phrase & head-compositional &
  [  SYNSEM [ NON-LOCAL [ SLASH.LIST #slash, REL #rel, QUE 0-alist, YNQ 0-alist ] ],
     ARGS < [ SYNSEM.LOCAL #local & [ CAT.HEAD +nrpd ] ],
	   [ SYNSEM.NON-LOCAL [ SLASH.LIST < #local . #slash >,
				                  REL #rel & 0-alist ] ] > ].'''

SEC_FILLER = '''2nd-head-filler-phrase := binary-phrase & phrasal & head-compositional &
  [ SYNSEM.NON-LOCAL.SLASH.LIST < #firstarg . #otherargs >,
    ARGS < [ SYNSEM.LOCAL #local ],
     [ SYNSEM.NON-LOCAL [ SLASH.LIST [ FIRST #firstarg, REST < #local . #otherargs > ],
          REL 0-alist ] ] > ].'''


MTX_FRONT = 'front-matrix'
MTX_FRONT_OPT = 'matrix-front-opt'
SG_OBLIG = 'single-oblig'
SINGLE = 'single'
MULTI = 'multi'
NO_MULTI = 'no-multi'
ALL_OBLIG = 'all-oblig'


def customize_wh_ques(mylang,ch,rules,roots):
    if (not ch.get(MTX_FRONT)) and ch.get('wh-q-infl') != 'on':
        # If there are no wh-questions, need to put the default
        # constraints to establish the semantic links between
        # the filler and the gap and the extracted subject and the verb:
        mylang.add(BASIC_FILLER_SG,section='phrases')
        mylang.add(EX_SUBJ,section='phrases')
        mylang.add('''clause :+ [ SYNSEM.NON-LOCAL.QUE.LIST < > ]. ''')
    else:
        if ch.get('person') == '1-2-3':
            mylang.add('wh-pronoun-noun-lex := [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.PER 3rd ].')

    if (not ch.get(MTX_FRONT)) or ch.get(MTX_FRONT) == 'single':
        if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
            if ch.get(NO_MULTI) == 'on':
                mylang.add('''head-adj-int-phrase :+ [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 0-alist ].''',section='addenda')
            else:
                mylang.add('''head-adj-int-phrase :+ [ HEAD-DTR.SYNSEM [ L-QUE -,
                                LOCAL.CAT.VAL [ SPR < >, SUBJ clist, COMPS clist ] ] ].''',section='addenda')
            mylang.add('''adj-head-int-phrase :+ [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 0-alist ].''',section='addenda')

    if (not ch.get(MTX_FRONT) and ch.get('wh-q-infl') != 'on') or ch.get(NO_MULTI) == 'on':
        mylang.add('''clause :+ [ SYNSEM.NON-LOCAL.QUE.LIST < > ]. ''')
        if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
            mylang.add('''head-adj-int-phrase :+ [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < > ]. ''',section='addenda')

    mylang.add_literal(';;; Wh-question-related phrasal types',section='phrases')

    if ch.get(MTX_FRONT) in [SINGLE, MULTI]:
        mylang.add_literal('''; Do not allow extracting "And Kim"''')
        mylang.add('''basic-head-filler-phrase :+
   [ ARGS < [ SYNSEM.LOCAL.COORD - ], [ SYNSEM.LOCAL.COORD - ] > ].''')
        mylang.add(WH_Q_PHR,section='phrases')
        #if ch.get('form-fin-nf') == 'on':
        #    mylang.add('wh-ques-phrase := [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ].')
        mylang.add_literal('; Complement extraction',section='phrases')
        mylang.add(EX_COMP)
        rules.add('ex-comp := extracted-comp-phrase.')
        mylang.add_literal('; Adjunct extraction',section='phrases')
        mylang.add(EX_ADJ)
        rules.add('ex-adj := extracted-adv-adp-adj-phrase.')

    if ch.get(MTX_FRONT) in [SINGLE]:
        # With single fronting, can restrict SLASH to one element at most
        mylang.add(BASIC_FILLER_SG,section='phrases')
        mylang.add_literal('; Subject extraction')
        mylang.add(EX_SUBJ)
        rules.add('ex-subj := extracted-subj-phrase.')
        rules.add('wh-ques := wh-ques-phrase.')
        mylang.add('extracted-adv-adp-adj-phrase := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH.LIST < > ].')
        #mylang.add(SG_EX_SUBJ)
        # Pass up QUE from the HEAD-DTR in this case:
        mylang.add(WH_Q_PHR_SG_OR_OBLIG_FRONT)

    if ch.get(MTX_FRONT) in [MULTI]:
        mylang.add(EX_SUBJ_MULTI,section='phrases')
        rules.add('ex-subj := extracted-subj-phrase.')
        mylang.add('wh-ques-phrase := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH.LIST < [], ... > ].')
        mylang.add(FIRST_FILLER)
        # prevent adjunct extraction, as it will be done out of head-subj
        if ch.get(MTX_FRONT_OPT) == 'none-oblig':
            mylang.add('1st-head-filler-phrase := [ SYNSEM.MODIFIED hasmod ].')
            #if len(ch.get('adv', [])) > 0 or len(ch.get('normadp', [])) > 0:
            #    mylang.add('headv-adj-phrase := [ HEAD-DTR.SYNSEM.MODIFIED notmod ].')
        #mylang.add(SEC_FILLER)
        mylang.add('wh-1st-ques-phrase := 1st-head-filler-phrase & wh-ques-phrase.')
        #mylang.add('wh-2nd-ques-phrase := 2nd-head-filler-phrase & wh-ques-phrase.')
        rules.add('wh1-ques := wh-1st-ques-phrase.')
        #rules.add('wh2-ques := wh-2nd-ques-phrase.')
        if ch.get(MTX_FRONT_OPT) == 'all-oblig':
            mylang.add(WH_Q_PHR_NO_OR_SG_OBLIG_MULTI) # Pass up QUE from HEAD-DTR
        # Rule out structural ambiguity for sentences like "Who sleeps where?"
        if ch.get('word-order') in ['svo', 'sov', 'osv' ]:
            mylang.add('''head-adj-int-phrase :+ [ HEAD-DTR.SYNSEM [ L-QUE -,
             LOCAL.CAT.VAL [ SUBJ clist, COMPS clist ] ] ].''',section='addenda')
        if ch.get('word-order') == 'free':
            mylang.add('''adj-headv-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ clist ].''', merge=True)
            mylang.add('''headv-adj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ clist ].''', merge=True)
        # The below does not make sense?
        if ch.get('word-order') in ['ovs', 'vos', 'vso']:
            mylang.add('''adj-head-int-phrase := [ HEAD-DTR.SYNSEM.L-QUE - ].''',section='addenda')



    # If the fronting isn't obligatory or if only one question phrase
    # is obligatorily fronted, need also in-situ rules:
    if (ch.get(MTX_FRONT) == SINGLE and not ch.get(MTX_FRONT_OPT) == SG_OBLIG) \
            or ch.get(MTX_FRONT) == 'in-situ' \
            or (ch.get(MTX_FRONT) == MULTI and not ch.get(MTX_FRONT_OPT) == ALL_OBLIG) \
            or ch.get('wh-q-infl') == 'on':
        mylang.add_literal('; In-situ interrogative clause.',section='phrases')
        mylang.add(IN_SITU_PHRASE)
        rules.add('in-situ-ques := insitu-int-cl.')
        if not ch.get(MTX_FRONT) == 'in-situ':
            mylang.add('insitu-int-cl := [ SYNSEM.NON-LOCAL.YNQ.LIST < > ].')
            if ch.get('embed-insitu') == 'on':
                mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.WH.BOOL + ].')
            else:
                mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.WH.BOOL - ].')
        if ch.get(MTX_FRONT) in [SINGLE,MULTI]:
            mylang.add('insitu-int-cl := [ SYNSEM.L-QUE - ].')
        if (ch.get(MTX_FRONT) == SINGLE and not ch.get(MTX_FRONT_OPT) == SG_OBLIG):
            mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.MC + ].')
        elif ch.get(MTX_FRONT) == 'multi' and ch.get(MTX_FRONT_OPT) == SG_OBLIG:
            mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.MC - ].')
        # For non-free word orders, need to rule out structural ambiguity:
        if ch.get('word-order') in ['svo', 'sov'] \
                and not (ch.get(MTX_FRONT) == 'in-situ' or ch.get('wh-q-infl') == 'on'):
            mylang.add('subj-head-phrase := [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE.LIST < > ].')

    # Obligatory pied piping of both nouns and adpositions is the default.
    # If there is no pied piping or it is optional, additional extraction rules are needed.
    if ch.get(MTX_FRONT) in [SINGLE, MULTI] and len(ch.get('det', [])) > 0:
        if (not ch.get('pied-pip') == 'on' or (ch.get('pied-pip')=='on'
                                               and not (ch.get('oblig-pied-pip-noun')== 'on'))):
            mylang.add_literal('; If there is no obligatory pied-piping, determiners '
                               'can be extracted separately:',section='phrasal')
            mylang.add(EX_DET_PHRASE,section='phrases')
            if ch.get('case'):
                mylang.add('''extracted-det-phrase :=
                [ SYNSEM.LOCAL.CAT.HEAD.CASE #case,
                HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CAT.HEAD.CASE #case ].''')
            if ch.has_png():
                mylang.add('''extracted-det-phrase :=
                [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png,
                HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.LOCAL.CAT.VAL.SPEC.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png ].''')
            rules.add('ex-det := extracted-det-phrase.')
        if ch.get('pied-pip-adp') == 'on' and not ch.get('oblig-pied-pip-adp') == 'on':
            mylang.add('extracted-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD +vp ].')
            if ch.get('word-order') in ['vos', 'vso', 'ovs','v-initial']:
                mylang.add('head-subj-phrase := [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.SLASH.LIST < > ].')
        else:
            mylang.add('extracted-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb ].')

        if ch.get('pied-pip-adp') == 'on' and \
                ch.get('oblig-pied-pip-noun') != 'on' \
                and ch.get('oblig-pied-pip-adp') == 'on':
            mylang.add('norm-adposition-lex := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.NON-LOCAL.SLASH.LIST < > ].')

    if ch.get(MTX_FRONT) in [SINGLE, MULTI]:
        # Free probably shouldn't belong here? check
        if ch.get('word-order') in ['vos','svo','sov','free']:
            if ch.get('pied-pip-adp') != 'on' or ch.get('oblig-pied-pip-adp') == 'on':
                mylang.add('extracted-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ cons ].',merge=True)
            mylang.add('extracted-subj-phrase := [ SYNSEM.LOCAL.CAT.VAL.COMPS  < >,'
                                                 ' HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].',merge=True)
        elif ch.get('word-order') in ['vso','osv','ovs']:
            mylang.add('extracted-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].',merge=True)
            mylang.add('extracted-subj-phrase := [ SYNSEM.LOCAL.CAT.VAL.COMPS #comps,'
                                                 ' HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS #comps ].',merge=True)

    if ch.get('q-part') == 'on':
        if ch.get(MTX_FRONT) == 'in-situ':
            mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.WH.BOOL + ].')
            # Perhaps this special case below can be dealt with differently?
            # Like with the WH feature?
            if len(ch.get('q-particle')) == 1:
               qpart = ch.get('q-particle')[1] # This is 1 and not 0 because the Choices len method is overriden; see Choices.py
               if qpart['wh'] == 'oblig':
                   mylang.add('insitu-int-cl := [ SYNSEM.LOCAL.CAT.MC - ].')

            if ch.get('q-part-order') == 'second':
                mylang.add('insitu-int-cl := [ SYNSEM.NON-LOCAL.YNQ #ynq,'
                           'HEAD-DTR.SYNSEM.NON-LOCAL.YNQ #ynq ].')
            else:
                mylang.add('insitu-int-cl := [ SYNSEM.NON-LOCAL.YNQ.LIST < > ].')