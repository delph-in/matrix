from gmcs.linglib import lexbase

######################################################################
# customize_yesno_questions()
#   Create the type definitions associated with the user's choices
#   about matrix yes/no questions.

def customize_yesno_questions(mylang, ch, rules, lrules, hierarchies):

    qinvverb = ch.get('q-inv-verb')
    qpartposthead = ch.get('q-part-order')
    qpartform = ch.get('q-part-orth')

    if ch.get('q-inv'):
        comment = \
            'For the analysis of inverted yes-no questions, we add the feature INV.'
        mylang.add('verb :+ [ INV bool ].', comment, section='addenda')

        comment = \
            'All verbs start off as not inverted.'
        mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].',
                   comment, section='verblex')


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
      [ SYNSEM [ LOCAL.CAT [ HEAD verb & [ INV + ],
                             VAL [ COMPS < #subj . #comps >,
                                     SUBJ < >,
                                     SPR #spr,
                                     SPEC #spec ],
                             MC na ],
                 LKEYS #lkeys ],
        DTR.SYNSEM [ LOCAL.CAT [ HEAD verb,
                                 VAL [ SUBJ < #subj >,
                                       COMPS #comps,
                                       SPR #spr,
                                       SPEC #spec ]],
                     LKEYS #lkeys ]].'''
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
                mylang.add('subj-v-inv-lrule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

            if qinvverb == 'main':
                mylang.add('subj-v-inv-lrule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].')



        # ERB 2010-04-15 If object drop is enabled (i.e., if the
        # head-opt-comp rule is instantiated) then we need to prevent
        # the inverted subject from being dropped.  This is true even if
        # subject drop is generally allowed, since subj-verb inversion
        # is not apparent if the subject is dropped.  Assuming for now
        # that this rule would not be used to model inflection that requires
        # subj-v inversion but allows subject drop.

        if ch.get('obj-drop'):
            mylang.add('subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT - ].')


        # ERB 2010-04-15 If we have a finite/non-finite disctintion,
        # the FORM value needs to be copied up.  FIXME: More generally,
        # any verby head features should be copied by this rule.
        # See notes on FORM and qpart below.

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
    [ SYNSEM.LOCAL.CAT [ HEAD.INV +,
                         VAL #val,
                         MC + ],
      HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na,
                                  VAL #val &
                                       [SUBJ < >,
                                       COMPS < >]],
      C-CONT.HOOK.INDEX.SF ques ].'''
        mylang.add(typedef, comment, section='phrases')

        rules.add('int := int-cl.')

    # ERB 2006-10-05 Moving away from the modifier analysis of question particles
    # which I think doesn't handle the facts well.  These look more like complementizers
    # to me.

    if ch.get('q-part'):
        comment = \
            'We treat question particles as complementizers.\n' + \
            'Here is the lexical type for complementizers.'
        typedef = lexbase.COMPLEMENTIZER
        mylang.add(typedef, comment, section='complex')

        comment = 'Subtype for question particles. Constrains SF to ques.'
        typedef = '''
      qpart-lex-item := complementizer-lex-item &
         [ SYNSEM.LOCAL [ CONT.HOOK.INDEX.SF ques,
                          CAT.VAL.COMPS.FIRST.LOCAL.CAT.MC + ] ].'''
        mylang.add(typedef, comment, section='complex')

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
            mylang.add('qpart-lex-item := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.FORM finite ].')




