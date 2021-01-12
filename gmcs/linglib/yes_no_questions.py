from gmcs.linglib import lexbase

######################################################################
# customize_yesno_questions()
#   Create the type definitions associated with the user's choices
#   about matrix yes/no questions.


INT_CL = '''int-cl := head-only & interrogative-clause &
  [ SYNSEM [ LOCAL.CAT [ HEAD +vc, VAL #val,
                                         MC bool ],
                    NON-LOCAL non-local-none ],
    HEAD-DTR.SYNSEM [ LOCAL.CAT [ MC na-or--,
                                  VAL #val & [ SUBJ < >, COMPS < >, SPR < >, SPEC < > ] ],
                                  NON-LOCAL [ YNQ.LIST < *top* >, SLASH.LIST < >, QUE.LIST < > ] ] ].'''

DECL_CL = '''decl-cl := head-only & declarative-clause & same-ynq-unary-phrase &
  [ SYNSEM.LOCAL.CAT [ VAL #val,
                                         MC bool ],
    HEAD-DTR.SYNSEM [ LOCAL.CAT [ MC na,
                                                           VAL #val ],
                                      NON-LOCAL.YNQ.LIST < > ]].'''

MC_NA = '''mc-na-headed-phrase := headed-phrase &
  [ SYNSEM.LOCAL.CAT.MC na,
    HEAD-DTR.SYNSEM.LOCAL.CAT.MC na ].'''

SAME_PERIPH = '''same-periph-unary-phrase :=
                       unary-phrase & [ SYNSEM [ L-PERIPH #periph ],
                       ARGS < [ SYNSEM [ L-PERIPH #periph ] ] > ].'''

SAME_YNQ = '''same-ynq-unary-phrase :=
                       unary-phrase & [ SYNSEM [ NON-LOCAL.YNQ #ynq ],
                       ARGS < [ SYNSEM [ NON-LOCAL.YNQ #ynq ] ] > ].'''


def customize_yesno_questions(mylang, ch, rules, lrules, hierarchies, roots):

    qinvverb = ch.get('q-inv-verb')

    if ch.get('q-inv') or (ch.get('q-part') and ch.get('q-part-order') != 'second'):
        mylang.add(
            'basic-head-comp-phrase :+ [ SYNSEM [ LOCAL.CAT.HC-LIGHT #light, LIGHT #light ] ].', section='addenda')
    else:
        mylang.add(
            'basic-head-comp-phrase :+ [ SYNSEM.LIGHT - ].', section='addenda')

    if ch.get('q-inv'):
        comment = \
            'For the analysis of inverted yes-no questions, we add the feature INV.'
        mylang.add('verb :+ [ INV bool ].', comment, section='addenda')

        comment = \
            'All verbs start off as not inverted.'
        mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].',
                   comment, section='verblex')
        if any(ch.get('cop')):
            mylang.add('cop-lex := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].', comment)

        comment = \
            'Rule for inverted subject verb order in questions.\n' + \
            'The incompatible SUBJ values on SYNSEM and DTR are\n' + \
            'what keeps this one from spinning.'

        # ERB 2006-10-05 Adding in semantics here.  This rule constrains MESG to ques.
        # ERB 2007-01-21 Removing semantics here: Need to allow inversion to not express questions.  Instead, the result of this is MC na, and there is a separate non-branching rule which introduces question semantics.  Following the ERG in this.
        # ERB 2010-04-15 Adding [AUX +] on DTR, too.
        typedef = '''
    subj-v-inv-lrule := cat-change-only-lex-rule &
			same-hc-light-lex-rule &
			same-posthead-lex-rule &
                        constant-lex-rule &
      [ INFLECTED #infl,
        SYNSEM [ LOCAL.CAT [ HEAD verb & [ INV + ],
                             VAL [ COMPS < #subj . #comps >,
                                     SUBJ < >,
                                     SPR #spr,
                                     SPEC #spec ],
                             MC na ],
                 LKEYS #lkeys ],
        DTR [ INFLECTED #infl,
              SYNSEM [ LOCAL.CAT [ HEAD verb,
                                 VAL [ SUBJ < #subj >,
                                       COMPS #comps,
                                       SPR #spr,
                                       SPEC #spec ]],
                     LKEYS #lkeys ]]].'''
        mylang.add(typedef, comment, section='lexrules')

        lrules.add('inv-lr := subj-v-inv-lrule.')

        # ERB 2010-04-15 Cleaning up treatent of constraints on AUX,
        # which were still very old-school. Need to both constrain DTR
        # and copy the value up.  Only checking qinvverb if we know
        # we have auxiliaries.

        if ch.get('has-aux') == 'yes':
            mylang.add('''
                 subj-v-inv-lrule :=
                    [ SYNSEM.LOCAL.CAT.HEAD.FORM #form,
                      DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].''')

            if qinvverb == 'aux':
                mylang.add(
                    'subj-v-inv-lrule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

            if qinvverb == 'main':
                mylang.add(
                    'subj-v-inv-lrule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].')

        # ERB 2010-04-15 If object drop is enabled (i.e., if the
        # head-opt-comp rule is instantiated) then we need to prevent
        # the inverted subject from being dropped.  This is true even if
        # subject drop is generally allowed, since subj-verb inversion
        # is not apparent if the subject is dropped.  Assuming for now
        # that this rule would not be used to model inflection that requires
        # subj-v inversion but allows subject drop.

        if ch.get('obj-drop'):
            mylang.add(
                'subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT - ].')

        # ERB 2010-04-15 If we have a finite/non-finite disctintion,
        # the FORM value needs to be copied up.  FIXME: More generally,
        # any verby head features should be copied by this rule.
        # See notes on FORM and q-part below.

        if 'form' in hierarchies:
            mylang.add('''
                 subj-v-inv-lrule :=
                    [ SYNSEM.LOCAL.CAT.HEAD.FORM #form,
                      DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].''')

        # ERB 2007-01-21 Then we need the non-branching construction which
        # corrects to MC + and adds SF ques.

        comment = \
            'This rule takes [MC na] inverted phrases and licenses' + \
            'them as main clauses with question semantics.\n'

        typedef = '''
    int-cl := interrogative-clause & head-only &
    [ SYNSEM [ MODIFIED hasmod, 
               LOCAL.CAT [ HEAD [ INV + ],
                         VAL #val,
                         MC + ],
               NON-LOCAL non-local-none ],
      HEAD-DTR.SYNSEM [ LOCAL.CAT [ MC na,
                                  VAL #val &
                                       [SUBJ < >,
                                       COMPS < >] ],
                        NON-LOCAL non-local-none ],
      C-CONT.HOOK.INDEX.SF ques ].'''
        mylang.add(typedef, comment, section='phrases')
        # OZ 2020-07-03 This is to suppress ambiguity in "Which house do the cats sleep in?"
        if ch.get('has-aux') == 'yes':
            mylang.add('int-cl := [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].')
        rules.add('int := int-cl.')

    if ch.get('q-part'):
        # Clause-initial and clause-final particles are analyzed as complementizers:
        if not ch.get('q-part-order') == 'second':
            comment = \
                'We treat question particles as complementizers.\n' + \
                'Here is the lexical type for complementizers.'
            typedef = lexbase.COMPLEMENTIZER
            mylang.add(typedef, comment, section='complex')

            comment = 'Subtype for question particles. Constrains SF to ques.'
            typedef = '''
            qpart-lex-item := complementizer-lex-item &
             [ SYNSEM.LOCAL [ CONT.HOOK.INDEX.SF ques ] ].'''
            mylang.add(typedef, comment, section='complex')
            supertype = 'qpart-lex-item'

            # ERB 2010-04-15 If we have a finite/non-finite distinction in the
            # language, the qpart should insist on attaching to finite clauses
            # only.  An alternative would be to have it raise the FORM value, but
            # I don't see any evidence for that just now. Using presence of 'form'
            # in hierarchies to detect this situation. This could break if someone
            # named their own feature "form", but the name-space validation should
            # catch that.  This works because customize_form() is called before
            # customize_yesno_questions. This is an example of cross-library
            # interaction.
            if 'form' in hierarchies:
                mylang.add(
                    'qpart-lex-item := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.FORM finite ].')

        # OZ: Second position  clitics are treated as modifiers:
        elif ch.get('q-part-order') == 'second':
            # Matrix addenda:
            mylang.add(
                '''non-local :+ [ YNQ append-list ].''', section='addenda')
            mylang.add(
                '''non-local-none :+ [ YNQ.LIST < > ].''', section='addenda')
            mylang.add(
                '''basic-filler-phrase :+ [ SYNSEM.NON-LOCAL.YNQ.LIST < > ].''', section='addenda')
            mylang.add('''basic-extracted-adj-phrase :+ 
            [ SYNSEM.NON-LOCAL.YNQ #ynq,
              HEAD-DTR.SYNSEM.NON-LOCAL.YNQ #ynq ].''', section='addenda')

            mylang.add('''coord-phrase :+ 
            [ SYNSEM.NON-LOCAL.YNQ.APPEND < #ynq1, #ynq2 >,
              LCOORD-DTR.SYNSEM.NON-LOCAL.YNQ #ynq1,
              RCOORD-DTR.SYNSEM.NON-LOCAL.YNQ #ynq2 ].''', section='addenda')
            mylang.add('''basic-binary-phrase :+ [ SYNSEM [ L-PERIPH #periph,
                                  NON-LOCAL.YNQ.APPEND < #ynq1,
                                    #ynq2 > ],
             ARGS < [ SYNSEM [ L-PERIPH #periph,
                               NON-LOCAL.YNQ #ynq1 ] ],
                    [ SYNSEM [ L-PERIPH -,
                               NON-LOCAL.YNQ #ynq2 ] ] > ].''', section='addenda')
            mylang.add('basic-head-mod-phrase-simple :+ '
                       '[ HEAD-DTR.SYNSEM.L-PERIPH #periph, '
                       'NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.MOD < [ L-PERIPH #periph ] > ].', section='addenda')
            mylang.add(SAME_PERIPH, section='phrases')
            mylang.add(SAME_YNQ, section='phrases')
            mylang.add(
                'bare-np-phrase := same-periph-unary-phrase & same-ynq-unary-phrase & [ SYNSEM.LIGHT - ].')
            comment = 'Second position question particles are treated as modifiers.'
            typedef = lexbase.QUES_CLITIC
            mylang.add(typedef, comment, section='complex')
            supertype = 'ques-clitic-lex'
            # Constrain all other words to be YNQ-empty:
            mylang.add(
                '''zero-arg-nonynq := basic-zero-arg & [ SYNSEM.NON-LOCAL.YNQ.LIST < > ].''', section='complex')
            mylang.add(
                '''zero-arg-ynq := zero-arg-nonrel & zero-arg-nonque & zero-arg-nonslash.''', section='complex')
            mylang.add('''norm-zero-arg :+ zero-arg-nonynq.''',
                       section='addenda')
            mylang.add('''zero-arg-rel :+ zero-arg-nonynq.''',
                       section='addenda')
            mylang.add('''zero-arg-que :+ zero-arg-nonynq.''',
                       section='addenda')
            mylang.add('''zero-arg-slash :+ zero-arg-nonynq.''',
                       section='addenda')
            mylang.add(
                'non-ynq-word := word-or-lexrule & [ SYNSEM.NON-LOCAL.YNQ.LIST < > ].')
            mylang.add('non-local-none-lex-item :+ non-ynq-word.',
                       section='addenda')
            mylang.add('intersective-mod-lex :+ non-ynq-word.',
                       section='addenda')
            roots.add('root := [ SYNSEM.NON-LOCAL.YNQ.LIST < > ].')
            mylang.add(INT_CL, section='phrases')
            mylang.add(DECL_CL, section='phrases')
            rules.add('intrg-phrase := int-cl.')
            #rules.add('decl-cl := decl-cl.')
            mylang.add(MC_NA, section='phrases')
            #mylang.add('binary-headed-phrase :+ mc-na-headed-phrase.',section='addenda')

        # Add subtypes for each question particle.
        for qpart in ch.get('q-particle'):
            typename = qpart.full_key + '-lex'
            typedef = typename + ' := ' + supertype + '.'
            mylang.add(typedef, section='complex')
            if qpart['main'] == 'on' and qpart['embed'] != 'on':
                mylang.add(typename + ' := [ SYNSEM.LOCAL.CAT [ MC #mc,'
                                      'VAL.COMPS.FIRST.LOCAL.CAT.MC #mc & + ] ].')
            elif qpart['embed'] == 'on' and qpart['main'] != 'on':
                mylang.add(typename + ' := [ SYNSEM.LOCAL.CAT [ MC #mc '
                                      'VAL.COMPS.FIRST.LOCAL.CAT.MC #mc & -  ] ].')
            if qpart['wh'] == 'imp':
                if ch.get('q-part-order') != 'second':
                    mylang.add(typename + ':= [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST '
                                          '[ LOCAL.CAT.WH.BOOL - ] ].')
                else:
                    mylang.add(typename + ':= [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST '
                                          '[ LOCAL.CAT.WH.BOOL - ] ].')

            # This constraint is weird because what it does is make
            # the particle impossible in polar questions, not obligatory
            # in consituent questions. It should either be removed or put back
            # if such a choice ("impossible in polar questions") is added to the
            # questionnaire.
            # Removing it currently breaks test wh24:
            # https://github.com/delph-in/matrix/issues/541
            # elif qpart['wh'] == 'oblig':
            #     if ch.get('q-part-order') != 'second':
            #         mylang.add(typename + ':= [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST '
            #                               '[ LOCAL.CAT.WH.BOOL +  ] ].')
