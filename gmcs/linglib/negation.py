from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

######################################################################
# customize_sentential_negation()
#   Create the type definitions associated with the user's choices
#   about sentential negation.

def customize_sentential_negation(mylang, ch, lexicon, rules, lrules, hierarchies):
    # JDC 2012-06-01 Nowadays this function is something like
    # the entry point for the sentential negation library.
    # So it makes sense to put some notes here.

    # The first point is that the customization feature
    # 'negation=plus' is mapped to a series of different
    # lexical rule types based on the overall negation type
    # selected.
    #
    # This is done so that users can enjoy the simplicity of
    # selecting a negation strategy and then marking the
    # relevant rules 'negation=plus' without worrying
    # too much about the details.

    # However, because these lexical rules have different
    # properties, in this file we map negation=plus to
    # the following values so that features.py and
    # morphotactics.py can define the rules correctly.

    # negation values on lexical rules:
    #   'a' = standard simple negation,
    #         * cont-change-only-lex-rule
    #   'b' = lex rule with negation semantics,
    #         also requires NEGFORM on its complement
    #         this should only appear on auxiliaries

    ############################
    #'c' is deprecated, it can be defined on the morphotactics
    # page without further help from us
    #   'c' = form changing lex rule,
    #         changes form to negform and
    #         does not add semantics
    ############################

    #   'd' = comps-changing lexical rule,
    #         no semantics, adds negative comp to
    #         verb's comps list
    #   'e' = infl-lex-rule which sets NEG-SAT to -
    #   'f' = const-lex-rule which add neg1 to comps
    #   'g' = non-inflecting lex rule adds empty neg-comp to verb
    #         intended to be in the same pc as rule 'f' (the two
    #         rules should be exclusive)
    #   'h' = comps adding neg rule which also sets NEG-SAT -


    if ch.get('neg-head-feature') == 'on':
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')

    exp = ch.get('neg-exp') # exponence of sentential negation: simple, bipartite

    if exp in [ '1', '0' ]:
        # we have a simple negation strategy
        if ch.get('infl-neg') == 'on':
            # two things to do:
            #  * add supertype to appropriate lri (for morphotactic.py to work)
            #  * change value of negation=plus to negation=a (for feature.py to work)
            for vpc in ch['verb-pc']:
                for lrt in vpc['lrt']:
                    for f in lrt['feat']:
                        if 'negation' in f['name'] and f['value']=='plus':
                            lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                          ['cont-change-only-lex-rule'])
                            f['value'] = 'a'
        elif ch.get('neg-aux') == 'on':
            pass
            # nothing to do, neg-aux is like any other aux,
            # should be handled appropriately by word-order library
        elif ch.get('adv-neg') == 'on':
            # call create_neg_adv with proper parameters!
            # create_neg_adv_lex_item(mylang, ch, lexicon, rules, 1)
            customize_adv_neg(mylang, ch, lexicon, rules)
        elif ch.get('comp-neg') == 'on':
            # call create_neg_comp with proper parameters!
            customize_comp_neg(mylang, ch, lexicon, rules, lrules)
    elif exp == '2':
        bnegtype = ch.get('bineg-type')
        if bnegtype == 'infl-infl':
            customize_infl_infl_neg(ch)
        elif bnegtype == 'infl-head':
            customize_infl_head_neg()
        elif bnegtype == 'infl-comp':
            customize_infl_comp_neg(mylang,ch,lexicon)
        elif bnegtype == 'infl-mod':
            customize_infl_mod_neg(mylang,ch,lexicon,rules)
        elif bnegtype == 'head-head':
            customize_head_head_neg()
        elif bnegtype == 'head-comp':
            customize_head_comp_neg(mylang,ch,lexicon,hierarchies)
        elif bnegtype == 'head-mod':
            customize_head_mod_neg(mylang,ch,lexicon,rules)
        elif bnegtype == 'comp-comp':
            customize_comp_comp_neg(mylang,ch,lexicon)
        elif bnegtype == 'comp-mod':
            customize_comp_mod_neg(mylang,ch,lexicon,rules)
        elif bnegtype == 'mod-mod':
            customize_mod_mod_neg(mylang,ch,lexicon,rules)
#  if ch.get('neg-exp') == '2':
#    # see if we need to make any neg advs
#    if ch.get('neg1-type')[0] == 'f' or \
#      ch.get('neg2-type')[0] == 'f':
#      create_neg_adv_lex_item(mylang, ch, lexicon, rules, 2)

def customize_adv_neg(mylang, ch, lexicon, rules):
    # first add lexical type for negative adverb
    mylang.set_section('otherlex')
    mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD.MOD < [ LOCAL.CAT.HEAD verb ] > ]].''',
               'Type for negative adverbs.')

    mylang.add_comment('neg-adv-lex',
                       '''This adverb should go through a specialized phrase structure rule
                          included with this grammar.''')

    # now parameterize as pre/posthead, no value here means both orders
    # should work
    if ch.get('neg-order') == 'before':
        mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
    elif ch.get('neg-order') == 'after':
        mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')

    # constrain type of constituent modified by neg-adv
    if ch.get('neg-mod') == 's':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod') == 'vp':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod') == 'v':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')

    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
  lexical Vs for V-level attachment of negative adverbs.''')

    # add spelling for neg-adverb
    if(ch.get('neg-adv-orth')):
        orth = ch.get('neg-adv-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

    rules.add('adj-head-scop := adj-head-scop-phrase.')
    rules.add('head-adj-scop := head-adj-scop-phrase.',
              'Rule instances for head-modifier structures. Corresponding types\n' +
              'are defined in matrix.tdl.  The matrix customization script did\n' +
              'not need to add any further constraints, so no corresponding types\n' +
              'appear in ' + ch.get('language').lower() + '.tdl')
    # KPH 2-1-18 The following appears to be an artifact from before no-mod-lex was
    # added as a supertype to the basic lecical types for noun verb, etc, so I have
    # commented it out from now. 
    # mylang.add('+nvcdmo :+ [ MOD < > ].',
    #           'This grammar includes head-modifier rules.  To keep\n' +
    #           'out extraneous parses, constrain the value of MOD on\n' +
    #           'various subtypes of head.  This may need to be loosened later.\n' +
    #           'This constraint says that only adverbs, adjectives,\n' +
    #           'and adpositions can be modifiers.',
    #           section='addenda')
    # that's all folks!

def customize_comp_neg(mylang, ch, lexicon, rules, lrules):
    # the neg-comp analyses require the neg-head-feature choice
    # simulate it here if it's not on
    if(ch.get('neg-head-feature')!='on'):
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')

    # first add lexical type
    mylang.set_section('otherlex')
    mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ NEGATED +, 
                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ] ].''',
               '''Type for negative selected comps.
                  This type uses the MOD list to get scopal semantics.
                  Constrain head-modifier rules to be [NEGATED -] if you don't
                  want this type to act as a modifer.''')

    # okay, now add the spelling and lexical instance
    if(ch.get('comp-neg-orth')):
        orth = ch.get('comp-neg-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

    # we need the lexical rule that adds these types to the comps lists
    # of the verbs that selecte them
    if ch.get('comp-neg-order') == 'before':
        mylang.add('''neg-comp-add-lex-rule := const-val-change-only-lex-rule &
               [ SYNSEM.LOCAL [ CAT.VAL [  SUBJ #subj,
                                           COMPS < canonical-synsem & 
                                                 [ LOCAL.CAT.HEAD [ NEGATED +,
                                MOD < [ LOCAL.CONT.HOOK #hook ] > ] ] 
                                                   . #comps > ] ],
                 DTR.SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj,
                                              COMPS #comps ],
                                        CONT.HOOK #hook ] ].
               ''')

    elif ch.get('comp-neg-order') == 'after':
        mylang.add('''neg-comp-add-lex-rule := const-val-change-only-lex-rule &
               [ SYNSEM.LOCAL.CAT.VAL [  SUBJ #subj,
                                         COMPS < #comps , canonical-synsem & 
                                             [ LOCAL.CAT.HEAD [ NEGATED +,
                                             MOD < [ LOCAL.CONT.HOOK #hook ] > ] ] > ],
                 DTR.SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj,
                                              COMPS.FIRST #comps ],
                                    CONT.HOOK #hook ] ].
               ''')

    lrules.add('neg-lex-rule := neg-comp-add-lex-rule.')
    # deal with type of selecting verb: auxiliary verb or any finite verb
    if(ch.get('comp-neg-head')=='aux'):
        mylang.add('neg-comp-add-lex-rule := [ DTR aux-lex ].')
    elif(ch.get('comp-neg-head')=='v'):
        mylang.add('''neg-comp-add-lex-rule := [ DTR verb-lex &
                [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ] ].''')

        # all done!



# bipartite negation types
def customize_infl_infl_neg(ch):
    # mostly handled in feature processing
    # and custo system
    # need to change the value of negation=plus
    # and neg2=plus to let
    # feature processing know what kind of lex rules these are

    for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
            for f in lrt['feat']:
                if 'negation' in f['name'] and f['value']=='plus':
                    #          lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +\
                    #                                        ['add-only-rule'])
                    lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                  [#'same-agr-lex-rule',
                                                      #                                         'same-ctxt-lex-rule',
                                                      'val-and-cont-change-lex-rule',
                                                      #                                         'same-head-lex-rule',
                                                      #                                         'same-val-lex-rule',
                                                      #                                         'same-hc-light-lex-rule',
                                                      #                                         'same-posthead-lex-rule',
                                                      #                                         'same-mc-lex-rule'
                                                  ])
                    f['value'] = 'b'
# TODO: set up validation to require that the user has properly
#       defined a neg2 
#        if 'neg2' in f['name'] and f['value']=='plus':
#          lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +\
#                                        ['head-change-only-rule']) 
#          f['name'] = 'negation'
#          f['value'] = 'c'
def customize_infl_head_neg():
    pass
    # completely handled by customization system and
    # existing infrastructure.

    # * neg-aux is a typical aux,
    # * the required FORM value is added
    #   to the questionnaire page automatically,
    # * user selects that the inflector is FORM negform
    #   and makes the position class required to enforce
    #   complementary distribution of FORM

def customize_infl_comp_neg(mylang,ch,lexicon):
    # neg affix on verb requires neg-adv with neg semantics
    # neg-adv is a selected complement

    # this analysis requires the neg-head-feature, if it's not on
    # simulate it here
    if(ch.get('neg-head-feature')!='on'):
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')


    # add type for neg-adv
    mylang.set_section('otherlex')
    mylang.add('''neg-comp-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            SPEC < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ NEGATED +, 
                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ] ].''',
               '''Type for negative selected comps.
                  This type uses the MOD list to get scopal semantics.
                  Constrain head-modifier rules to be [NEGATED -] if you don't
                  want this type to act as a modifer.''')

    # add lexical instance
    if(ch.get('comp-neg-orth')):
        orth = ch.get('comp-neg-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg-comp-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

    # inflecting lexical rule must add neg-adv to comps list,
    for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
            for f in lrt['feat']:
                if 'negation' in f['name'] and f['value']=='plus':
                    lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                  ['val-change-only-lex-rule'])
                    f['value']='d'

def customize_infl_mod_neg(mylang,ch,lexicon,rules):
    # neg affix on verb requires neg-adv with neg semantics
    # neg-adv is a modifier

    # this analysis requires the neg-head-feature, if it's not on
    # simulate it here
    if(ch.get('neg-head-feature')!='on'):
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')

    # add neg-sat to SYNSEM
    mylang.set_section('addenda')
    mylang.add('''synsem :+ [ NEG-SAT luk ].''')

    # verbs must start out NEG-SAT na-or-+
    mylang.add('''basic-verb-lex :+ [ SYNSEM.NEG-SAT na-or-+ ].''')

    # decorate psrs to pass up NEG-SAT value
    mylang.add('''basic-head-comp-phrase :+ [ SYNSEM.NEG-SAT #ns,
                                            HEAD-DTR.SYNSEM.NEG-SAT #ns ].''')
    mylang.add('''basic-head-subj-phrase :+ [ SYNSEM.NEG-SAT #ns,
                                            HEAD-DTR.SYNSEM.NEG-SAT #ns ].''')

    # ammend root condition
    mylang.add('''clause :+ [ SYNSEM.NEG-SAT na-or-+ ].''')

    # inflecting lexical rule must add NEG-SAT - to verb,
    for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
            for f in lrt['feat']:
                if 'negation' in f['name'] and f['value']=='plus':
                    f['value']='e'

    # add type for neg-adv
    mylang.set_section('otherlex')
    mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ MOD < [ LOCAL.CAT.HEAD verb ] >,
                                             NEGATED + ]]].''',
               'Type for negative adverbs.')

    mylang.add_comment('neg-adv-lex',
                       '''This adverb should go through a specialized phrase structure rule
                          included with this grammar.''')

    # now parameterize as pre/posthead, no value here means both orders
    # should work, also add specialized modifier rules
    if ch.get('neg-mod-order-infl-mod-neg') == 'before':
        mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
        mylang.add('''neg-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT +,
                   HEAD-DTR.SYNSEM.NEG-SAT -,
                   NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-adj-head-scop := neg-adj-head-scop-phrase.')
    elif ch.get('neg-mod-order-infl-mod-neg') == 'after':
        mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
        mylang.add('''neg-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT +,
                 HEAD-DTR.SYNSEM.NEG-SAT -,
                 NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-head-adj-scop := neg-head-adj-scop-phrase.')
    else:
        mylang.add('''neg-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT +,
                   HEAD-DTR.SYNSEM.NEG-SAT -,
                   NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-adj-head-scop := neg-adj-head-scop-phrase.')
        mylang.add('''neg-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT +,
                 HEAD-DTR.SYNSEM.NEG-SAT -,
                 NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-head-adj-scop := neg-head-adj-scop-phrase.')


    # constrain type of constituent modified by neg-adv
    if ch.get('neg-mod-infl-mod-neg') == 's':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod-infl-mod-neg') == 'vp':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod-infl-mod-neg') == 'v':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')

    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
  lexical Vs for V-level attachment of negative adverbs.''')

    # add spelling for neg-adverb
    if(ch.get('neg-mod-orth')):
        orth = ch.get('neg-mod-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')


    mylang.add('+vcdmo :+ [ MOD < > ].',
               'This grammar includes head-modifier rules.  To keep\n' +
               'out extraneous parses, constrain the value of MOD on\n' +
               'various subtypes of head.  This may need to be loosened later.\n' +
               'This constraint says that only adverbs, adjectives,\n' +
               'and adpositions can be modifiers.',
               section='addenda')



def customize_head_head_neg():
    pass

def customize_head_comp_neg(mylang,ch,lexicon, hierarchies):
    # bipartite neg with negauxiliary which has a 'dummy' negcomp
    # add type for neg-adv

    # this analysis requires the neg-head-feature, if it's not on
    # simulate it here
    if(ch.get('neg-head-feature')!='on'):
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')

    # negauxes with modified comps lists are
    # generated by auxiliaries.py

    #TODO deal with v head

    # need to add type and instance for semantically empty neg-comp
    mylang.set_section('otherlex')
    mylang.add('''neg-comp-lex := norm-zero-arg &
                 [ SYNSEM.LOCAL [ CAT [ HEAD adv & [ NEGATED + ],
                                        VAL [ SUBJ < >,
                                              COMPS < > ] ],
                                  CONT [ RELS <! !>,
                                         HCONS <! !> ]]].''',
               '''Type for negative selected comps.
                  This type uses the MOD list to get scopal semantics.
                  Constrain head-modifier rules to be [NEGATED -] if you don't
                  want this type to act as a modifer.''')

    # add lexical instance
    if(ch.get('comp-neg-orth')):
        orth = ch.get('comp-neg-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg-comp-lex &\
                [ STEM < \"'+ orthstr +'\" > ].')



def customize_head_mod_neg(mylang, ch, lexicon,rules):
    # basically just need to add NEG-SAT feature dependencies

    # this analysis uses the neg-head-feature
    if(ch.get('neg-head-feature')!='on'):
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')

    # add neg-sat to SYNSEM
    mylang.set_section('addenda')
    mylang.add('''synsem :+ [ NEG-SAT luk ].''')

    # verbs must start out NEG-SAT na-or-+
    mylang.add('''basic-verb-lex :+ [ SYNSEM.NEG-SAT na-or-+ ].''')

    # decorate psrs to pass up NEG-SAT value
    mylang.add('''basic-head-comp-phrase :+ [ SYNSEM.NEG-SAT #ns,
                                            HEAD-DTR.SYNSEM.NEG-SAT #ns ].''')
    mylang.add('''basic-head-subj-phrase :+ [ SYNSEM.NEG-SAT #ns,
                                            HEAD-DTR.SYNSEM.NEG-SAT #ns ].''')

    # ammend root condition
    mylang.add('''clause :+ [ SYNSEM.NEG-SAT na-or-+ ].''')

    # neg-aux lexically introduces SYNSEM.NEG-SAT -
    #

    # add type for neg-adv
    mylang.set_section('otherlex')
    mylang.add('''neg-adv-lex := norm-zero-arg &
                 [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD adv & [ MOD < [ LOCAL.CAT.HEAD verb ] >,
                                             NEGATED + ] ],
                                  CONT [ RELS <! !>,
                                         HCONS <! !> ] ] ].''',
               'Type for negative adverbs.')

    mylang.add_comment('neg-adv-lex',
                       '''This adverb should go through a specialized phrase structure rule
                          included with this grammar.''')

    # now parameterize as pre/posthead, no value here means both orders
    # should work, also add specialized modifier rules
    if ch.get('neg-mod-order-head-mod-neg') == 'before':
        mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
        mylang.add('''neg-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT +,
                   HEAD-DTR.SYNSEM.NEG-SAT -,
                   NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-adj-head-scop := neg-adj-head-scop-phrase.')
    elif ch.get('neg-mod-order-head-mod-neg') == 'after':
        mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
        mylang.add('''neg-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT +,
                 HEAD-DTR.SYNSEM.NEG-SAT -,
                 NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-head-adj-scop := neg-head-adj-scop-phrase.')
    else:
        mylang.add('''neg-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT +,
                   HEAD-DTR.SYNSEM.NEG-SAT -,
                   NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-adj-head-scop := neg-adj-head-scop-phrase.')
        mylang.add('''neg-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT +,
                 HEAD-DTR.SYNSEM.NEG-SAT -,
                 NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-head-adj-scop := neg-head-adj-scop-phrase.')


    # constrain type of constituent modified by neg-adv
    if ch.get('neg-mod-head-mod-neg') == 's':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod-head-mod-neg') == 'vp':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod-head-mod-neg') == 'v':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')

    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
  lexical Vs for V-level attachment of negative adverbs.''')

    # add spelling for neg-adverb
    if(ch.get('neg-mod-orth')):
        orth = ch.get('neg-mod-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orthstr +'\" > ].')


    mylang.add('+vcdmo :+ [ MOD < > ].',
               'This grammar includes head-modifier rules.  To keep\n' +
               'out extraneous parses, constrain the value of MOD on\n' +
               'various subtypes of head.  This may need to be loosened later.\n' +
               'This constraint says that only adverbs, adjectives,\n' +
               'and adpositions can be modifiers.',
               section='addenda')

def customize_comp_comp_neg(mylang,ch,lexicon):
    # bipartite negation type with two negative complements,
    # presumably one is selected by an aux, the other by a v
    #

    # this analysis uses the neg-head-feature
    if(ch.get('neg-head-feature')!='on'):
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')

    # verbs need to be NEGATED - 'underlyingly'
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.NEGATED na-or-- ].')

    # add neg1 complement
    mylang.set_section('otherlex')
    mylang.add('''neg1-comp-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            SPEC < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ NEGATED +, 
                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ] ].''',
               '''Type for negative selected comps.
                  This type uses the MOD list to get scopal semantics.
                  Constrain head-modifier rules to be [NEGATED -] if you don't
                  want this type to act as a modifer.''')

    # add neg2 complement
    mylang.add('''neg2-comp-lex := norm-zero-arg &
                 [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >,
                                            SPEC < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ NEGATED +, 
                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ],
                                  CONT [ HCONS <! !>,
                                         RELS <! !> ]  ] ].''',
               '''Type for negative selected comps.
                  This type uses the MOD list to get scopal semantics.
                  Constrain head-modifier rules to be [NEGATED -] if you don't
                  want this type to act as a modifer.''')

    # add lexical instances
    if(ch.get('comp-neg1-orth')):
        orth = ch.get('comp-neg1-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg1-comp-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

    if(ch.get('comp-neg2-orth')):
        orth = ch.get('comp-neg2-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg2-comp-lex &\
                [ STEM < \"'+ orthstr +'\" > ].')

    # non-inflecting lexical rule must add neg-adv to comps list,
    next_n = ch['verb-pc'].next_iter_num() if 'verb-pc' in ch else 1
    ch['verb-pc%d_name' % next_n] = 'neg1'
    nvpc = ch['verb-pc'].get_last()
    nvpc['order'] = 'suffix'
    nvpc['inputs'] = 'aux'
    nvpc['lrt1_feat1_name'] = 'negation'
    nvpc['lrt1_feat1_value'] = 'f'
    nvpc['lrt1_lri1_inflecting'] = 'no'
    nvpc['lrt1_supertypes'] = 'val-change-only-lex-rule'

    # other non-inflecting lex rule adds other neg comp to other verbs
    # can go in same pc as above (the two rules should be exclusive)
    ch['verb-pc%d_name' % (next_n + 1)] = 'neg2'
    nvpc = ch['verb-pc'].get_last()
    nvpc['order'] = 'suffix'
    nvpc['inputs'] = 'verb'
    nvpc['lrt1_feat1_name'] = 'negation'
    nvpc['lrt1_feat1_value'] = 'g'
    nvpc['lrt1_lri1_inflecting'] = 'no'
    nvpc['lrt1_supertypes'] = 'cat-change-only-lex-rule, same-hc-light-lex-rule, same-posthead-lex-rule, same-mc-lex-rule'

    # also need auxes to underlyingly select for [ NEGATED - ] types
    mylang.add('''aux-lex := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.NEGATED na-or-- ].''')

def customize_comp_mod_neg(mylang,ch,lexicon,rules):
    # here we create a negation strategy with one selected complement,
    # which presumably carries the negative force

    # this analysis uses the neg-head-feature
    if(ch.get('neg-head-feature')!='on'):
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')

    # add neg-sat to SYNSEM
    mylang.set_section('addenda')
    mylang.add('''synsem :+ [ NEG-SAT luk ].''')

    # verbs must start out NEG-SAT na-or-+
    mylang.add('''basic-verb-lex :+ [ SYNSEM.NEG-SAT na-or-+ ].''')

    # decorate psrs to pass up NEG-SAT value
    mylang.add('''basic-head-comp-phrase :+ [ SYNSEM.NEG-SAT #ns,
                                            HEAD-DTR.SYNSEM.NEG-SAT #ns ].''')
    mylang.add('''basic-head-subj-phrase :+ [ SYNSEM.NEG-SAT #ns,
                                            HEAD-DTR.SYNSEM.NEG-SAT #ns ].''')

    # ammend root condition
    mylang.add('''clause :+ [ SYNSEM.NEG-SAT na-or-+ ].''')


    # first introduce the lexical rule that introduces the negative complement
    # also needs to set NEG-SAT - on its target

    #TODO: parameterize this rule according to neg-comp before/after
    #      current system only supports before and ignores the choice!
    attach=''
    if ch.get('comp-neg-head-comp-mod') == 'v':
        attach='verb'
    else: attach='aux'

    next_n = ch['verb-pc'].next_iter_num() if 'verb-pc' in ch else 1
    ch['verb-pc%d_name' % next_n] = 'neg1'
    nvpc = ch['verb-pc'].get_last()
    nvpc['order'] = 'suffix'
    nvpc['inputs'] = attach
    nvpc['lrt1_feat1_name'] = 'negation'
    nvpc['lrt1_feat1_value'] = 'h'
    nvpc['lrt1_lri1_inflecting'] = 'no'
    nvpc['lrt1_supertypes'] = 'val-change-only-lex-rule'

    # create lexical type for neg-comp
    mylang.set_section('otherlex')
    mylang.add('''neg1-comp-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            SPEC < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ NEGATED +, 
                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ] ].''',
               '''Type for negative selected comps.
                  This type uses the MOD list to get scopal semantics.
                  Constrain head-modifier rules to be [NEGATED -] if you don't
                  want this type to act as a modifer.''')

    # create lexical instance for neg1
    if(ch.get('comp-neg-orth')):
        orth = ch.get('comp-neg-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg1-comp-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

    # add lexical type for negadv "neg2"
    mylang.set_section('otherlex')
    mylang.add('''neg-adv-lex := norm-zero-arg &
                 [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD adv & [ MOD < [ LOCAL.CAT.HEAD verb ] >,
                                             NEGATED + ] ],
                                  CONT [ RELS <! !>,
                                         HCONS <! !> ] ] ].''',
               'Type for negative adverbs.')

    mylang.add_comment('neg-adv-lex',
                       '''This adverb should go through a specialized phrase structure rule
                          included with this grammar.''')

    # now parameterize as pre/posthead, no value here means both orders
    # should work, also add specialized modifier rules
    if ch.get('neg-mod-order-comp-mod-neg') == 'before':
        mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
        mylang.add('''neg-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT +,
                   HEAD-DTR.SYNSEM.NEG-SAT -,
                   NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-adj-head-scop := neg-adj-head-scop-phrase.')
    elif ch.get('neg-mod-order-comp-mod-neg') == 'after':
        mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
        mylang.add('''neg-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT +,
                 HEAD-DTR.SYNSEM.NEG-SAT -,
                 NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-head-adj-scop := neg-head-adj-scop-phrase.')
    else:
        mylang.add('''neg-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT +,
                   HEAD-DTR.SYNSEM.NEG-SAT -,
                   NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-adj-head-scop := neg-adj-head-scop-phrase.')
        mylang.add('''neg-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT +,
                 HEAD-DTR.SYNSEM.NEG-SAT -,
                 NON-HEAD-DTR neg-adv-lex ].''')
        rules.add('neg-head-adj-scop := neg-head-adj-scop-phrase.')


    # constrain type of constituent modified by neg-adv
    if ch.get('neg-mod-comp-mod-neg') == 's':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod-comp-mod-neg') == 'vp':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod-comp-mod-neg') == 'v':
        mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')

    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
  lexical Vs for V-level attachment of negative adverbs.''')

    # add spelling for neg-adverb
    if(ch.get('neg-mod-orth')):
        orth = ch.get('neg-mod-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orthstr +'\" > ].')


    mylang.add('+vcdmo :+ [ MOD < > ].',
               'This grammar includes head-modifier rules.  To keep\n' +
               'out extraneous parses, constrain the value of MOD on\n' +
               'various subtypes of head.  This may need to be loosened later.\n' +
               'This constraint says that only adverbs, adjectives,\n' +
               'and adpositions can be modifiers.',
               section='addenda')

def customize_mod_mod_neg(mylang,ch,lexicon,rules):
    # need to create two modifiers, one sets neg-sat to minus, the other to
    # back to plus.

    # add neg-sat and decorate psrs
    # this analysis uses the neg-head-feature
    if(ch.get('neg-head-feature')!='on'):
        mylang.add('head :+ [ NEGATED luk ].', section='addenda')

    # add neg-sat to SYNSEM
    mylang.set_section('addenda')
    mylang.add('''synsem :+ [ NEG-SAT luk ].''')

    # verbs must start out NEG-SAT na-or-+
    mylang.add('''basic-verb-lex :+ [ SYNSEM.NEG-SAT na-or-+ ].''')

    # decorate psrs to pass up NEG-SAT value
    mylang.add('''basic-head-comp-phrase :+ [ SYNSEM.NEG-SAT #ns,
                                            HEAD-DTR.SYNSEM.NEG-SAT #ns ].''')
    mylang.add('''basic-head-subj-phrase :+ [ SYNSEM.NEG-SAT #ns,
                                            HEAD-DTR.SYNSEM.NEG-SAT #ns ].''')

    # ammend root condition
    mylang.add('''clause :+ [ SYNSEM.NEG-SAT na-or-+ ].''')

    # add neg1 mod (sets neg-sat to -, via its psr)
    mylang.set_section('otherlex')
    mylang.add('''neg1-adv-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            SPEC < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ NEGATED +, 
                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ] ].''',
               '''This type uses the MOD list to get scopal semantics.
                  Constrain head-modifier rules to be [NEGATED -] if you don't
                  want this type to act as a modifer.''')

    # add neg2 mod (sets neg-sat back to +, via its psr)
    mylang.add('''neg2-adv-lex := norm-zero-arg &
                [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >,
                                             SPEC < >,
                                             COMPS < >,
                                             SUBJ < > ],
                                       HEAD adv & [ NEGATED +,
                                                    MOD < [ LOCAL.CAT.HEAD verb ] > ] ],
                                 CONT [ RELS <! !>,
                                        HCONS <! !> ] ] ].''')

    # create lexical instance for neg1
    if(ch.get('neg1-mod-orth')):
        orth = ch.get('neg1-mod-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg1-adv-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

    # create lexical instance for neg2
    if(ch.get('neg2-mod-orth')):
        orth = ch.get('neg2-mod-orth')
        orthstr = orth_encode(orth)
        lexicon.add(TDLencode(orth) + ' := neg2-adv-lex &\
                [ STEM < \"'+ orthstr +'\" > ].')

    # now parameterize as pre/posthead, no value here means both orders
    # should work, also add specialized modifier rules
    if ch.get('neg1-mod-order') == 'before':
        mylang.add('neg1-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
        mylang.add('''neg1-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT -,
                   HEAD-DTR.SYNSEM.NEG-SAT na-or-+,
                   NON-HEAD-DTR neg1-adv-lex ].''')
        rules.add('neg1-adj-head-scop := neg1-adj-head-scop-phrase.')
    elif ch.get('neg1-mod-order') == 'after':
        mylang.add('neg1-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
        mylang.add('''neg1-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT -,
                 HEAD-DTR.SYNSEM.NEG-SAT na-or-+,
                 NON-HEAD-DTR neg1-adv-lex ].''')
        rules.add('neg1-head-adj-scop := neg1-head-adj-scop-phrase.')
    else:
        mylang.add('''neg1-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT -,
                   HEAD-DTR.SYNSEM.NEG-SAT na-or-+,
                   NON-HEAD-DTR neg1-adv-lex ].''')
        rules.add('neg1-adj-head-scop := neg1-adj-head-scop-phrase.')
        mylang.add('''neg1-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT -,
                 HEAD-DTR.SYNSEM.NEG-SAT na-or-+,
                 NON-HEAD-DTR neg1-adv-lex ].''')
        rules.add('neg1-head-adj-scop := neg1-head-adj-scop-phrase.')


    # constrain type of constituent modified by neg-adv
    if ch.get('neg1-mod') == 's':
        mylang.add('''neg1-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod1') == 'vp':
        mylang.add('''neg1-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
                                                                                   COMPS null ]].''')
    elif ch.get('neg1-mod') == 'v':
        mylang.add('''neg1-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')

        mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
  lexical Vs for V-level attachment of negative adverbs.''')

    # now parameterize as pre/posthead, no value here means both orders
    # should work, also add specialized modifier rules
    if ch.get('neg2-mod-order') == 'before':
        mylang.add('neg2-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
        mylang.add('''neg2-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT +,
                   HEAD-DTR.SYNSEM.NEG-SAT -,
                   NON-HEAD-DTR neg2-adv-lex ].''')
        rules.add('neg2-adj-head-scop := neg2-adj-head-scop-phrase.')
    elif ch.get('neg2-mod-order') == 'after':
        mylang.add('neg2-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
        mylang.add('''neg2-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT +,
                 HEAD-DTR.SYNSEM.NEG-SAT -,
                 NON-HEAD-DTR neg2-adv-lex ].''')
        rules.add('neg2-head-adj-scop := neg2-head-adj-scop-phrase.')
    else:
        mylang.add('''neg2-adj-head-scop-phrase := adj-head-scop-phrase &
                 [ SYNSEM.NEG-SAT +,
                   HEAD-DTR.SYNSEM.NEG-SAT -,
                   NON-HEAD-DTR neg2-adv-lex ].''')
        rules.add('neg2-adj-head-scop := neg2-adj-head-scop-phrase.')
        mylang.add('''neg2-head-adj-scop-phrase := head-adj-scop-phrase &
               [ SYNSEM.NEG-SAT +,
                 HEAD-DTR.SYNSEM.NEG-SAT -,
                 NON-HEAD-DTR neg2-adv-lex ].''')
        rules.add('neg2-head-adj-scop := neg2-head-adj-scop-phrase.')


    # constrain type of constituent modified by neg-adv
    if ch.get('neg2-mod') == 's':
        mylang.add('''neg2-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
                                                                                   COMPS null ]].''')
    elif ch.get('neg-mod2') == 'vp':
        mylang.add('''neg2-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
                                                                                   COMPS null ]].''')
    elif ch.get('neg2-mod') == 'v':
        mylang.add('''neg2-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')

        mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
  lexical Vs for V-level attachment of negative adverbs.''')



##################
### VALIDATION ###
##################

def validate(ch, vr):
    if ch.get('infl-neg', default=False):
        for aux in ch.get('aux', []):
            for cf in aux.get('compfeature', []):
                if cf.get('name') == 'negation':
                    mess = 'When inflectional negation is selected ' + \
                           '[negation +] should only be found on bound morphemes.'
                    vr.err(cf.full_key + '_name', mess)
            for f in aux.get('feat', []):
                if f.get('name') == 'negation':
                    mess = 'When inflectional negation is selected ' + \
                           '[negation +] should only be found on bound morphemes.'
                    vr.err(f.full_key + '_name', mess)
        if ch.get('neg-exp') == "1":
            found=False
            for vpc in ch.get('verb-pc'):
                for lrt in vpc.get('lrt'):
                    for f in lrt.get('feat', []):
                        if f.get('name') == 'negation':
                            found=True
            if not found:
                mess = 'You have selected an inflectional negation strategy,' + \
                       'but no lexical rules are marked with [negation plus]'
                vr.warn('verb-pc' , mess)
        for verb in ch.get('verb', []):
            for cf in verb.get('compfeature', []):
                if cf.get('name') == 'negation':
                    mess = 'When inflectional negation is selected ' + \
                           '[negation +] should only be found on bound morphemes.'
                    vr.err(cf.full_key + '_name', mess)
            for f in verb.get('feat', []):
                if f.get('name') == 'negation':
                    mess = 'When inflectional negation is selected ' + \
                           '[negation +] should only be found on bound morphemes.'
                    vr.err(f.full_key + '_name', mess)

    neginfltype = ch.get('neg-infl-type')
    #  negseladv = ch.get('neg-sel-adv')

    if ch.get('neg-aux', default=False):
        has_neg_aux = False
        for aux in ch.get('aux', []):
            if aux.get('name') == 'neg':
                has_neg_aux = True
                break
        if has_neg_aux == False:
            vr.warn('neg-aux',
                    'You\'ve selected neg-aux but there is no corresponding ' + \
                    'type in the lexicon.')
            # ERB 2009-01-23 Commenting out the following because infl-neg is
            # now handled with customize_inflection.  We should eventually give
            # a warning if infl-neg is selected but no lexical rules actually
            # use it.  I think it would make sense for that warning to go
            # on the negation page.

            # If affix is indicated, must select prefix/suffix and
            # main/aux/either and give form.
        #   if (ch.get('infl-neg') == 'on'):
        #     if (not ch.get('neg-infl-type')):
        #       mess = 'If sentential negation is expressed through affixation, you must specify what the affix attaches to.'
        #       vr.err('neg-infl-type', mess)
        #     if (not ch.get('neg-aff')):
        #       mess = 'If sentential negation is expressed through affixation, you must specify whether its a prefix or a suffix'
        #       vr.err('neg-aff', mess)
        #     if (not ch.get('neg-aff-orth')):
        #       mess = 'If sentential negation is expressed through affixation, you must specify the form of the affix'
        #       vr.err('neg-aff-orth', mess)
        #     # If aux is selected then has-aux = 'yes' must be chosen in word order section
        #     if ((neginfltype == 'aux' or neginfltype == 'aux-main') and ch.get('has-aux') != 'yes'):
        #         mess = 'You have not indicated on the word order page that your language has auxiliaries.'
        #         vr.err('neg-infl-type', mess)

    # If adverb is indicated, must lexical entry, what it modifies, and
    # ind/selected modifier -- now only applicable under single negation
    # bipartite negs need to validate this for themeselves
    if (ch.get('adv-neg') == 'on') and (ch.get('neg-exp') == '1'):
        #    if (not ch.get('neg-adv')):
        #      mess = 'If sentential negation is expressed through an adverb, you must specify whether the adverb is a selected complement or an independent modifier.'
        #      vr.err('neg-adv', mess)
        #    if (ch.get('neg-adv') == 'ind-adv'):
        if (not ch.get('neg-mod')):
            mess = 'If sentential negaton is expressed through an adverb, ' + \
                   'you must specify what type of constituent the adverb modifies.'
            vr.err('neg-mod', mess)
        if (not ch.get('neg-order')):
            mess = 'If sentential negaton is expressed through an adverb, ' + \
                   'you must specify what side of its host the adverb attaches to.'
            vr.err('neg-order', mess)
        if (not ch.get('neg-adv-orth')):
            mess = 'If sentential negation is expressed through an adverb, ' + \
                   'you must specify the form of the adverb.'
            vr.err('neg-adv-orth', mess)

    if ch.get('comp-neg') == 'on':
        if ch.get('comp-neg-head') == 'aux' and ch.get('has-aux') != 'yes':
            mess = 'You have not indicated on the word order page ' + \
                   'that your language has auxiliaries.'
            vr.err('comp-neg-head', mess)

    if ch.get('neg-exp') == '2':
        if ch.get('bineg-type') == 'infl-infl' and ch.get('neg1b-neg2b') == 'on':
            # loose check that both lexical rules are present
            found1=False
            found2=False
            for vpc in ch.get('verb-pc'):
                for lrt in vpc.get('lrt'):
                    for f in lrt.get('feat', []):
                        if f.get('name') == 'negation':
                            found1=True
                        elif f.get('name') == 'form':
                            found2=True
            if not found1:
                mess = 'You have indicated that your language marks negation ' + \
                       'with two bound morphs.  The one which carries '  + \
                       'semantics should be marked [negation plus] (none ' + \
                       'were found.)'
                vr.warn('verb-pc',mess)
            if not found2:
                mess = 'You have indicated that your language marks negation ' + \
                       'with two bound morphs.  The semantically empty one should ' + \
                       'specify a FORM (no such rules were found).'
                vr.warn('verb-pc',mess)
        if ch.get('bineg-type') == 'infl-head':
            found=False
            for vpc in ch.get('verb-pc'):
                for lrt in vpc.get('lrt'):
                    for f in lrt.get('feat', []):
                        if f.get('name') == 'form' and f.get('value')=='negform':
                            found=True
            if not found:
                mess = 'You have indicated that your language marks negation ' + \
                       'with a negative auxiliary and an inflectional marker. ' + \
                       'At least one inflectional rule type should be specified ' + \
                       'for FORM negform.'
                vr.warn('verb-pc',mess)

                # If aux is selected then has-aux = 'yes' must be chosen in word
                # order section
            #    if ((negseladv == 'aux' or negseladv == 'main-aux') and
            #        ch.get('has-aux') != 'yes'):
            #       mess = 'You have not indicated on the word order page ' +\
            #             'that your language has auxiliaries.'
            #     vr.err('neg-sel-adv', mess)

            # ERB 2009-01-23 Currently not possible to say how they combine.

#   # If both strategies are checked, then they must say how they combine:
#   if ((ch.get('infl-neg') == 'on') and (ch.get('adv-neg') == 'on')):
#     if (not ch.get('multi-neg')):
#       mess = 'If you have selected both affix and adverb realizations of sentential negation, you must specify how they interact.'
#       vr.err('multi-neg', mess)
